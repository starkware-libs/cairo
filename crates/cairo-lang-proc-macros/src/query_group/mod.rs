//! Procedural macro crate – **fixed** version.
//!
//! This is a drop‑in replacement for the upstream `query-group` macro file.
//! Main fixes:
//!   • Correct handling of trait generics and lifetimes.
//!   • Removal of the `-input` typo (now `syn::parse::<ItemTrait>(input)`).
//!   • Consistent variable naming (no more `impl_generics_impl` references).
//!   • All generated items propagate the original generics.

use core::fmt;

use cairo_lang_utils::debug_macro;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{ToTokens, format_ident, quote};
use syn::spanned::Spanned;
use syn::visit_mut::VisitMut;
use syn::{FnArg, Generics, ItemTrait, Path, TraitItem, TraitItemFn, parse_quote};

use super::TokenStream;

pub mod queries;
use queries::{
    GeneratedInputStruct, InputQuery, InputSetter, InputSetterWithDurability, Intern, Lookup,
    Queries, SetterKind, TrackedQuery, Transparent,
};

// ——————————————————————————————————————————————————————————————————————————
// Helper structs for attribute parsing
// ——————————————————————————————————————————————————————————————————————————

#[derive(Debug)]
struct InputStructField {
    name: TokenStream2,
    ty: TokenStream2,
}

impl fmt::Display for InputStructField {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

struct SalsaAttr {
    name: String,
    tts: TokenStream,
    span: Span,
}

impl fmt::Debug for SalsaAttr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.name)
    }
}

impl TryFrom<syn::Attribute> for SalsaAttr {
    type Error = syn::Attribute;

    fn try_from(attr: syn::Attribute) -> Result<Self, Self::Error> {
        if is_not_salsa_attr_path(attr.path()) {
            return Err(attr);
        }
        let span = attr.span();
        let name = attr.path().segments[1].ident.to_string();
        let tts = match attr.meta {
            syn::Meta::Path(path) => path.into_token_stream(),
            syn::Meta::List(ref list) => {
                let tts = list
                    .into_token_stream()
                    .into_iter()
                    .skip(attr.path().to_token_stream().into_iter().count());
                TokenStream2::from_iter(tts)
            }
            syn::Meta::NameValue(nv) => nv.into_token_stream(),
        };
        Ok(SalsaAttr { name, tts: tts.into(), span })
    }
}

fn is_not_salsa_attr_path(path: &syn::Path) -> bool {
    path.segments.first().map(|s| s.ident != "salsa").unwrap_or(true) || path.segments.len() != 2
}

fn filter_attrs(attrs: Vec<syn::Attribute>) -> (Vec<syn::Attribute>, Vec<SalsaAttr>) {
    let mut other = vec![];
    let mut salsa = vec![];
    // Leave non-salsa attributes untouched. These are
    // attributes that don't start with `salsa::` or don't have
    // exactly two segments in their path.
    for attr in attrs {
        match SalsaAttr::try_from(attr) {
            Ok(it) => salsa.push(it),
            Err(it) => other.push(it),
        }
    }
    (other, salsa)
}

// ——————————————————————————————————————————————————————————————————————————
// Main macro implementation
// ——————————————————————————————————————————————————————————————————————————

#[derive(Debug, Clone, PartialEq, Eq)]
enum QueryKind {
    Input,
    Tracked,
    TrackedWithSalsaStruct,
    Transparent,
    Interned,
}

pub(crate) fn query_group_impl(
    _args: TokenStream,
    input: TokenStream,
) -> Result<TokenStream, syn::Error> {
    // 1. Parse the trait and grab its generics.
    let mut item_trait: ItemTrait = syn::parse::<ItemTrait>(input)?;
    let trait_name_ident = &item_trait.ident;

    // --- Only allow a generic parameter of 'db ---
    let mut has_db_lifetime = false;
    for param in item_trait.generics.params.iter() {
        if let syn::GenericParam::Lifetime(lt) = param {
            if lt.lifetime.ident == "db" {
                has_db_lifetime = true;
            } else {
                return Err(syn::Error::new(
                    lt.lifetime.span(),
                    "Only a single lifetime parameter named 'db is allowed in the trait generics",
                ));
            }
        }
    }
    let lifetime_addon = if has_db_lifetime {
        quote! {<'db>}
    } else {
        quote! {}
    };
    let ref_lifetime = if has_db_lifetime {
        quote! { 'db }
    } else {
        quote! {}
    };
    // 3. Prepare helper Idents that depend on the trait name.
    let input_struct_name = format_ident!("{}Data", trait_name_ident);
    let create_data_ident = format_ident!("create_data_{}", trait_name_ident);
    // Helper for trait path with lifetime if needed
    let input_struct_path = quote! { #input_struct_name #lifetime_addon };
    let _trait_path = quote! { #trait_name_ident #lifetime_addon };

    // 4. Collect information from each query method.
    let mut input_struct_fields: Vec<InputStructField> = vec![];
    let mut trait_methods: Vec<Queries> = vec![];
    let mut setter_trait_methods: Vec<SetterKind> = vec![];
    let mut lookup_signatures: Vec<TraitItem> = vec![];
    let mut lookup_methods = vec![];
    let mut cycle_functions = vec![];

    for item in item_trait.clone().items {
        if let TraitItem::Fn(method) = item {
            let signature = &method.sig;
            let method_name = &signature.ident;
            let (_attrs, salsa_attrs) = filter_attrs(method.attrs);

            // — Determine query kind and extra attributes —
            let mut query_kind = QueryKind::Tracked;
            let mut invoke: Option<Path> = None;
            let mut cycle: Option<Path> = None;
            let mut interned_struct_path: Option<syn::Path> = None;
            let mut lru: Option<u32> = None;

            // Params used many times later
            let params: Vec<FnArg> = signature.inputs.clone().into_iter().collect();
            let pat_and_tys: Vec<syn::PatType> = params
                .into_iter()
                .filter_map(|arg| match arg {
                    FnArg::Typed(p) => Some(p),
                    _ => None,
                })
                .collect();

            for SalsaAttr { name, tts, span } in salsa_attrs {
                match name.as_str() {
                    "cycle" => {
                        let path: Parenthesized<Path> = syn::parse(tts)?;
                        cycle = Some(path.0);
                    }
                    "input" => {
                        if !pat_and_tys.is_empty() {
                            return Err(syn::Error::new(
                                span,
                                "input methods cannot have a parameter",
                            ));
                        }
                        query_kind = QueryKind::Input;
                    }
                    "interned" => {
                        if let syn::ReturnType::Type(_, ty) = &signature.output {
                            if let syn::Type::Path(path) = &**ty {
                                interned_struct_path = Some(path.path.clone());
                                query_kind = QueryKind::Interned;
                            } else {
                                return Err(syn::Error::new(
                                    span,
                                    "interned queries must have return type",
                                ));
                            }
                        } else {
                            return Err(syn::Error::new(
                                span,
                                "interned queries must have return type",
                            ));
                        }
                    }
                    "invoke" => {
                        let path: Parenthesized<Path> = syn::parse(tts)?;
                        invoke = Some(path.0);
                    }
                    "invoke_actual" => {
                        let path: Parenthesized<Path> = syn::parse(tts)?;
                        invoke = Some(path.0);
                        query_kind = QueryKind::TrackedWithSalsaStruct;
                    }
                    "lru" => {
                        let lit: Parenthesized<syn::LitInt> = syn::parse(tts)?;
                        lru = Some(lit.0.base10_parse()?);
                    }
                    "transparent" => query_kind = QueryKind::Transparent,
                    _ => return Err(syn::Error::new(span, format!("unknown attribute `{name}`"))),
                }
            }

            // — Handle return type and input‑field accumulation —
            let syn::ReturnType::Type(_, return_ty) = signature.output.clone() else {
                return Err(syn::Error::new(signature.span(), "Queries must have a return type"));
            };
            if matches!(query_kind, QueryKind::Input) {
                if let syn::Type::Path(ty_path) = *return_ty.clone() {
                    input_struct_fields.push(InputStructField {
                        name: method_name.to_token_stream(),
                        ty: ty_path.path.to_token_stream(),
                    });
                }
            }

            // — Build the specific query structs —
            match (query_kind.clone(), invoke.clone()) {
                // Input query (no invoke)
                (QueryKind::Input, None) => {
                    trait_methods.push(Queries::InputQuery(InputQuery {
                        signature: signature.clone(),
                        create_data_ident: create_data_ident.clone(),
                    }));
                    setter_trait_methods.push(SetterKind::Plain(InputSetter {
                        signature: signature.clone(),
                        return_type: *(return_ty.clone()),
                        create_data_ident: create_data_ident.clone(),
                    }));
                    setter_trait_methods.push(SetterKind::WithDurability(
                        InputSetterWithDurability {
                            signature: signature.clone(),
                            return_type: *(return_ty.clone()),
                            create_data_ident: create_data_ident.clone(),
                        },
                    ));
                }
                // Interned query (no invoke)
                (QueryKind::Interned, None) => {
                    // Lookup needs interned struct without generic params
                    let interned_struct_path = {
                        let mut path = interned_struct_path.unwrap().clone();
                        for segment in &mut path.segments {
                            segment.arguments = syn::PathArguments::None;
                        }
                        path
                    };
                    let method = Intern {
                        signature: signature.clone(),
                        pat_and_tys: pat_and_tys.clone(),
                        interned_struct_path: interned_struct_path.clone(),
                    };
                    trait_methods.push(Queries::Intern(method));
                    let mut lookup = Lookup {
                        signature: signature.clone(),
                        pat_and_tys: pat_and_tys.clone(),
                        return_ty: *return_ty.clone(),
                        interned_struct_path,
                    };
                    lookup.prepare_signature();
                    lookup_signatures
                        .push(TraitItem::Fn(make_trait_method(lookup.signature.clone())));
                    lookup_methods.push(lookup);
                }
                // Tracked without invoke
                (QueryKind::Tracked, None) => {
                    trait_methods.push(Queries::TrackedQuery(TrackedQuery {
                        trait_name: trait_name_ident.clone(),
                        generated_struct: Some(GeneratedInputStruct {
                            input_struct_name: input_struct_name.clone(),
                            create_data_ident: create_data_ident.clone(),
                        }),
                        signature: signature.clone(),
                        pat_and_tys: pat_and_tys.clone(),
                        invoke: None,
                        cycle,
                        lru,
                    }));
                }
                // Tracked with invoke
                (QueryKind::Tracked, Some(invoke)) => {
                    trait_methods.push(Queries::TrackedQuery(TrackedQuery {
                        trait_name: trait_name_ident.clone(),
                        generated_struct: Some(GeneratedInputStruct {
                            input_struct_name: input_struct_name.clone(),
                            create_data_ident: create_data_ident.clone(),
                        }),
                        signature: signature.clone(),
                        pat_and_tys: pat_and_tys.clone(),
                        invoke: Some(invoke),
                        cycle,
                        lru,
                    }));
                }
                // Tracked with salsa struct
                (QueryKind::TrackedWithSalsaStruct, Some(invoke)) => {
                    trait_methods.push(Queries::TrackedQuery(TrackedQuery {
                        trait_name: trait_name_ident.clone(),
                        generated_struct: None,
                        signature: signature.clone(),
                        pat_and_tys: pat_and_tys.clone(),
                        invoke: Some(invoke),
                        cycle,
                        lru,
                    }));
                }
                // Transparent
                (QueryKind::Transparent, invoke) => {
                    trait_methods.push(Queries::Transparent(Transparent {
                        signature: signature.clone(),
                        pat_and_tys: pat_and_tys.clone(),
                        invoke,
                    }));
                }
                // Invalid combinations
                (QueryKind::Interned | QueryKind::Input, Some(path)) => {
                    let msg = if query_kind == QueryKind::Interned {
                        "Interned queries"
                    } else {
                        "Inputs"
                    };
                    return Err(syn::Error::new(
                        path.span(),
                        format!("{msg} cannot be used with an #[invoke]"),
                    ));
                }
                _ => unreachable!(),
            }
        }
    }

    for trait_method in trait_methods.iter() {
        if let Queries::TrackedQuery(tracked_query) = trait_method {
            if let Some(cycle_fn) = tracked_query.cycle_fn() {
                cycle_functions.push(cycle_fn);
            }
        }
    }

    // ————————————————————————————————————————————————————————————————
    // Generate helper input struct & its create method
    // ————————————————————————————————————————————————————————————————

    let fields_ts: Vec<TokenStream2> = input_struct_fields
        .iter()
        .map(|f| {
            let InputStructField { name, ty } = f;
            quote! { #name: Option<#ty> }
        })
        .collect();

    // TODO(eytan-starkware): Make this pub(crate) again.
    let input_struct = quote! {
        #[salsa::input]
        pub struct #input_struct_path { #(#fields_ts),* }
    };

    let none_initialisers = std::iter::repeat_n(quote! { None }, fields_ts.len());

    // Update all uses of trait_name_ident in generated code to use trait_path
    let create_data_method = quote! {
        #[allow(non_snake_case)]
        #[salsa::tracked]
        fn #create_data_ident (db: &#ref_lifetime dyn #trait_name_ident) -> #input_struct_path {
            #input_struct_name::new(db, #(#none_initialisers),*)
        }
    };

    // ————————————————————————————————————————————————————————————————
    // Setter & lookup method signatures
    // ————————————————————————————————————————————————————————————————

    let mut setter_signatures = vec![];
    let mut setter_methods = vec![];
    for ti in setter_trait_methods
        .iter()
        .map(|m| m.to_token_stream())
        .map(|ts| syn::parse2::<TraitItemFn>(ts).unwrap())
    {
        let mut sig_only = ti.clone();
        sig_only.default = None;
        sig_only.semi_token = Some(syn::Token![;](ti.span()));
        setter_signatures.push(TraitItem::Fn(sig_only));
        setter_methods.push(TraitItem::Fn(ti));
    }

    // Append signatures to the trait itself (in‑place mutation)
    item_trait.items.extend(setter_signatures);
    item_trait.items.extend(lookup_signatures);

    let trait_name_ident = &item_trait.ident.clone();
    // Remove all attributes from trait methods (cleanup)
    RemoveAttrsFromTraitMethods.visit_item_trait_mut(&mut item_trait);

    // ————————————————————————————————————————————————————————————————
    // Build the impl block – this is where we need to **merge** generics
    // ————————————————————————————————————————————————————————————————

    // Make a new Generics = original + helper param `DB`
    let mut impl_generics: Generics = item_trait.generics.clone();
    impl_generics.params.push(parse_quote! { DB });
    // if let Some(first_lt) = trait_generics.lifetimes().next() {
    //     impl_generics.make_where_clause().predicates.push(parse_quote! { DB: #first_lt });
    // }

    let supertraits = &item_trait.supertraits;
    item_trait.attrs.push(parse_quote! {
        #[salsa::db]
    });
    let trait_impl = quote! {
        #[salsa::db]
        impl #impl_generics  #trait_name_ident #lifetime_addon  for  DB
        where
            DB: #supertraits,
        {
            #(#trait_methods)*

            #(#setter_methods)*

            #(#lookup_methods)*
        }
    };

    // ————————————————————————————————————————————————————————————————
    // Output final token stream
    // ————————————————————————————————————————————————————————————————

    let out = quote! {
        #item_trait
        #trait_impl
        #input_struct
        #create_data_method
        #(#cycle_functions)*
    };

    debug_macro!("Output: \n{}", out);

    Ok(out.into())
}

// ——————————————————————————————————————————————————————————————————————————
// Utility helpers (unchanged logic)
// ——————————————————————————————————————————————————————————————————————————

/// Helper for parsing parentheses
pub(crate) struct Parenthesized<T>(pub(crate) T);
impl<T: syn::parse::Parse> syn::parse::Parse for Parenthesized<T> {
    fn parse(input: syn::parse::ParseStream<'_>) -> syn::Result<Self> {
        let content;
        syn::parenthesized!(content in input);
        content.parse().map(Parenthesized)
    }
}

fn make_trait_method(sig: syn::Signature) -> TraitItemFn {
    TraitItemFn {
        attrs: vec![],
        sig: sig.clone(),
        semi_token: Some(syn::Token![;](sig.span())),
        default: None,
    }
}

struct RemoveAttrsFromTraitMethods;
impl VisitMut for RemoveAttrsFromTraitMethods {
    fn visit_item_trait_mut(&mut self, i: &mut syn::ItemTrait) {
        for item in &mut i.items {
            if let TraitItem::Fn(f) = item {
                f.attrs.clear();
            }
        }
    }
}

pub(crate) fn token_stream_with_error(mut tokens: TokenStream, error: syn::Error) -> TokenStream {
    tokens.extend(TokenStream::from(error.into_compile_error()));
    tokens
}
