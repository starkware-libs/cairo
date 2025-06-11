//! Procedural macros.
use proc_macro::TokenStream;
use quote::{
    __private::{Span, TokenStream as TokenStream2},
    quote,
};
use syn::{DeriveInput, Generics, parse_macro_input, parse_quote};

/// Derives a [`cairo_lang_debug::DebugWithDb`] implementation for structs and enums.
pub fn derive_debug_with_db(input: TokenStream) -> TokenStream {
    // Parse the input tokens into a syntax tree.
    let input = parse_macro_input!(input as DeriveInput);

    // ── 1. Extract the db-type from `#[debug_db(..)]` ─────────────────────────────
    let attribute = input
        .attrs
        .iter()
        .find(|a| a.path().is_ident("debug_db"))
        .expect("debug_db attribute required for deriving DebugWithDb.");
    let db_ty: syn::Type = attribute.parse_args().expect("Invalid debug_db attribute!");
    let db = quote!((#db_ty)); // keep parentheses as in the original macro

    let name = &input.ident;
    let generics = &input.generics; // keep the user’s lifetimes / params / where-clause

    // ── 2. Generate the body per kind ────────────────────────────────────────────
    let body = match &input.data {
        syn::Data::Struct(s) => emit_struct_debug(name, generics, db.clone(), s),
        syn::Data::Enum(e) => emit_enum_debug(name, generics, db.clone(), e),
        syn::Data::Union(_) => panic!("Unions are not supported"),
    };

    quote! { #[allow(unused_parens)] #body }.into()
}

// ───────────────────────── helpers ──────────────────────────────────────────────

/// impl DebugWithDb for a *struct*
fn emit_struct_debug(
    name: &syn::Ident,
    generics: &Generics,
    db: TokenStream2,
    structure: &syn::DataStruct,
) -> TokenStream2 {
    let (pat, prints) = emit_fields_debug(db.clone(), name.to_string(), &structure.fields);

    // a) impl-side generics  = original + 'a + T
    let mut impl_generics = generics.clone();
    impl_generics.params.push(parse_quote! { 'a });
    impl_generics.params.push(parse_quote! { T: ?Sized + cairo_lang_utils::Upcast<#db> });
    let (impl_g, _, where_g) = impl_generics.split_for_impl();

    // b) type-side generics = *original only*
    let (_, ty_g, _) = generics.split_for_impl();

    let crt = debug_crate();
    quote! {
        impl #impl_g #crt::debug::DebugWithDb<T> for #name #ty_g #where_g {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>, other_db: &T) -> std::fmt::Result {
                use #crt::debug::DebugWithDb;
                use #crt::debug::helper::Fallback;

                let #name #pat = self;
                let db: &#db = other_db.upcast();
                #prints
            }
        }
    }
}

/// impl DebugWithDb for an *enum* – identical generics handling
fn emit_enum_debug(
    name: &syn::Ident,
    generics: &Generics,
    db: TokenStream2,
    enm: &syn::DataEnum,
) -> TokenStream2 {
    let mut arms = quote! {};
    for v in &enm.variants {
        let v_ident = &v.ident;
        let (pat, prints) = emit_fields_debug(db.clone(), v_ident.to_string(), &v.fields);
        arms = quote! {
            #arms
            #name :: #v_ident #pat => { #prints }
        };
    }

    let mut impl_generics = generics.clone();
    impl_generics.params.push(parse_quote! { 'a });
    impl_generics.params.push(parse_quote! { T: ?Sized + cairo_lang_utils::Upcast #db });
    let (impl_g, _, where_g) = impl_generics.split_for_impl();
    let (_, ty_g, _) = generics.split_for_impl();

    let crt = debug_crate();
    quote! {
        impl #impl_g #crt::debug::DebugWithDb<T> for #name #ty_g #where_g {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>, other_db: &T) -> std::fmt::Result {
                use #crt::debug::DebugWithDb;
                use #crt::debug::helper::Fallback;

                let db: &#db = other_db.upcast();
                match self {
                    #arms
                }
            }
        }
    }
}

/// Helper function for emit_struct_debug and emit_enum_debug. Both struct, and a variant use
/// a type called [syn::Fields].
/// This function builds and returns an unpacking pattern and code for `fmt()` on these fields.
fn emit_fields_debug(
    db: TokenStream2,
    name: String,
    fields: &syn::Fields,
) -> (TokenStream2, TokenStream2) {
    let crt = debug_crate();
    let mut pattern = quote! {};
    let mut field_prints = quote! {};
    for (i, field) in fields.iter().enumerate() {
        let has_hide_attr = field.attrs.iter().any(|a| {
            a.path().segments.len() == 1 && a.path().segments[0].ident == "hide_field_debug_with_db"
        });

        let ty = &field.ty;

        if has_hide_attr {
            if let Some(field_ident) = &field.ident {
                let ignore_name = syn::Ident::new(&format!("_{field_ident}"), Span::call_site());
                pattern = quote! { #pattern #field_ident: #ignore_name, };
            } else {
                panic!("Hiding unnamed fields is not implemented.");
            }
        } else {
            let field_ident = field
                .ident
                .clone()
                .unwrap_or_else(|| syn::Ident::new(&format!("v{i}"), Span::call_site()));
            pattern = quote! { #pattern #field_ident, };
            let func_call = quote! {
                &#crt::debug::helper::HelperDebug::<#ty, #db>::helper_debug(#field_ident, db)
            };
            if let Some(field_ident) = &field.ident {
                field_prints = quote! {
                    #field_prints
                    .field(stringify!(#field_ident), #func_call)
                }
            } else {
                field_prints = quote! {
                    #field_prints
                    .field(#func_call)
                }
            }
        }
    }
    match fields {
        syn::Fields::Named(_) => {
            pattern = quote! { { #pattern } };
            field_prints = quote! { f.debug_struct(#name) #field_prints .finish() };
        }
        syn::Fields::Unnamed(_) => {
            pattern = quote! { ( #pattern ) };
            field_prints = quote! { f.debug_tuple(#name) #field_prints .finish() };
        }
        syn::Fields::Unit => {
            pattern = quote! {};
            field_prints = quote! { f.debug_tuple(#name).finish() };
        }
    };
    (pattern, field_prints)
}

/// Returns the identifier of the debug crate. This is needed, since inside the debug
/// crate, it needs to be referred to as `crate` and no `debug`.
fn debug_crate() -> syn::Ident {
    let crate_name = std::env::var("CARGO_PKG_NAME").unwrap();
    let res = match crate_name.as_str() {
        "cairo-lang-debug" | "cairo_lang_debug" => "crate",
        _ => "cairo_lang_debug",
    };
    syn::Ident::new(res, Span::call_site())
}
