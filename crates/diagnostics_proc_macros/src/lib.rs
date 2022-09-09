use proc_macro::TokenStream;
use quote::__private::{Span, TokenStream as TokenStream2};
use quote::quote;
use syn::token::Mut;
use syn::{
    parse_macro_input, AngleBracketedGenericArguments, DeriveInput, FnArg, Pat, PatType, Path,
    TypePath, TypeReference,
};

/// Macro for defining functions that return WithDiagnostics<T>.
#[proc_macro_attribute]
pub fn with_diagnostics(_attr: TokenStream, item: TokenStream) -> TokenStream {
    // Parse token stream as a function AST.
    let syn::ItemFn { attrs, vis, sig, block } = syn::parse(item).unwrap();
    let function_ident = sig.ident;
    let mut params = sig.inputs.into_iter();

    // Extract the first parameter.
    let first_parameter = params.next();

    // Extract Diagnostics type.
    // TODO(spapini): Extract to a function.
    let diagnostics_ref_ty = match first_parameter.unwrap() {
        FnArg::Typed(PatType { pat, ty, .. }) => match &*pat {
            Pat::Ident(ident) => {
                assert_eq!(ident.ident, "diagnostics");
                ty
            }
            _ => panic!("Argument pattern is not a simple ident."),
        },
        FnArg::Receiver(_) => panic!("Argument is a receiver."),
    };
    let diagnostics_ty = match &*diagnostics_ref_ty {
        syn::Type::Reference(TypeReference { mutability: Some(Mut { .. }), elem, .. }) => elem,
        _ => panic!("Expected a reference as a first parameter."),
    };
    let segments = match &**diagnostics_ty {
        syn::Type::Path(TypePath { path: Path { segments, .. }, .. }) => segments,
        _ => panic!("Expected a path as the type for the first arg."),
    };
    let segment = segments.last().unwrap();
    assert_eq!(segment.ident, "Diagnostics");
    let generic_args = match &segment.arguments {
        syn::PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }) => args,
        _ => panic!("Expected angle brackets, e.g. Diagnostics<T>."),
    };
    let entry_ty = match generic_args.first().unwrap() {
        syn::GenericArgument::Type(ty) => ty,
        _ => panic!("Expected a generic argument for diagnostics."),
    };
    // TODO(spapini): Assert that the first arg is `&mut diagnostics: Diagnostics`.

    // Extract the rest of the arguments into args_syntax.
    let mut args_syntax = quote! {};
    for arg in params {
        match arg {
            FnArg::Typed(PatType { pat, ty, .. }) => match &*pat {
                Pat::Ident(ident) => args_syntax = quote! {#args_syntax #ident: #ty,},
                _ => panic!("Argument pattern is not a simple ident."),
            },
            FnArg::Receiver(_) => panic!("Argument is a receiver."),
        }
    }

    // Extract body and return type.
    let ret_ty = match sig.output {
        syn::ReturnType::Default => panic!("No return type."),
        syn::ReturnType::Type(_, ty) => ty,
    };

    // Emit a wrapper function.
    quote! {
        #(#attrs)* #vis fn #function_ident(#args_syntax) -> WithDiagnostics<#ret_ty, #entry_ty> {
            let mut diagnostics = Diagnostics::new();

            let mut f = |diagnostics: &mut Diagnostics<#entry_ty>| -> #ret_ty {
                #block
            };
            let value = f(&mut diagnostics);
            WithDiagnostics { value, diagnostics }
    }
    }
    .into()
}

/// Derives a [diagnostics::debug::DebugWithDb] implementation for structs and enums.
#[proc_macro_derive(DebugWithDb, attributes(debug_db, hide_field_debug_with_db))]
pub fn derive_debug_with_db(input: TokenStream) -> TokenStream {
    // Parse the input tokens into a syntax tree.
    let input = parse_macro_input!(input as DeriveInput);
    let attribute = input
        .attrs
        .iter()
        .find(|a| a.path.segments.len() == 1 && a.path.segments[0].ident == "debug_db")
        .expect("debug_db attribute required for deriving DebugWithDB.");
    let db: syn::Expr = syn::parse2(attribute.tokens.clone()).expect("Invalid debug_db attribute!");
    let db = if let syn::Expr::Paren(db) = db { db } else { panic!("Expected parenthesis") };
    let db = quote! {(dyn #db + 'static)};
    let name = input.ident;
    match input.data {
        syn::Data::Struct(strct) => emit_struct_debug(name, db, strct),
        syn::Data::Enum(enm) => emit_enum_debug(name, db, enm),
        syn::Data::Union(_) => panic!("Unions are not supported"),
    }
    .into()
}

/// Emits a DebugWithDb implementation for a struct.
fn emit_struct_debug(name: syn::Ident, db: TokenStream2, strct: syn::DataStruct) -> TokenStream2 {
    let (pattern, field_prints) = emit_fields_debug(db.clone(), name.to_string(), strct.fields);
    let crt = debug_crate();
    quote! {
        impl #crt::debug::DebugWithDb<#db> for #name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &#db) -> std::fmt::Result {
                use #crt::debug::DebugWithDb;
                use #crt::debug::helper::Fallback;
                let #name #pattern = self;
                #field_prints
            }
        }
    }
}

/// Emits a DebugWithDb implementation for an enum.
fn emit_enum_debug(name: syn::Ident, db: TokenStream2, enm: syn::DataEnum) -> TokenStream2 {
    let mut variant_prints = quote! {};
    for variant in enm.variants {
        let variant_name = variant.ident;
        let (pattern, field_prints) =
            emit_fields_debug(db.clone(), variant_name.to_string(), variant.fields);
        variant_prints = quote! {
            #variant_prints
            #name :: #variant_name #pattern => {
                #field_prints
            }
        }
    }
    let crt = debug_crate();
    quote! {
        impl #crt::debug::DebugWithDb<#db> for #name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &#db) -> std::fmt::Result {
                use #crt::debug::DebugWithDb;
                use #crt::debug::helper::Fallback;
                match self {
                    #variant_prints
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
    fields: syn::Fields,
) -> (TokenStream2, TokenStream2) {
    let crt = debug_crate();
    let mut pattern = quote! {};
    let mut field_prints = quote! {};
    for (i, field) in fields.iter().enumerate() {
        let has_hide_attr = field.attrs.iter().any(|a| {
            a.path.segments.len() == 1 && a.path.segments[0].ident == "hide_field_debug_with_db"
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
    let res = if crate_name == "debug" { "crate" } else { "debug" };
    syn::Ident::new(res, Span::call_site())
}
