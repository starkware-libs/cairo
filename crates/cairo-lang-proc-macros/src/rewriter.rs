//! Procedural macros.
use proc_macro::TokenStream;
use quote::__private::{Span, TokenStream as TokenStream2};
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

/// Derives a SemanticObject implementation for structs and enums.
pub fn derive_semantic_object(input: TokenStream) -> TokenStream {
    // Parse the input tokens into a syntax tree.
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    // TODO(yuval/shahar): extract the lifetime here and use it instead of `'a` below.
    let body = match input.data {
        syn::Data::Struct(structure) => emit_struct_semantic_object(name, structure),
        syn::Data::Enum(enm) => emit_enum_semantic_object(name, enm),
        syn::Data::Union(_) => panic!("Unions are not supported"),
    };
    quote! {
        #[allow(unused_parens)]
        #body
    }
    .into()
}

/// Emits a SemanticObject implementation for a struct.
fn emit_struct_semantic_object(name: syn::Ident, structure: syn::DataStruct) -> TokenStream2 {
    let (deps, pattern, field_rewrites) = emit_fields_semantic_object(structure.fields);
    let crt = semantic_crate();
    quote! {
        impl<T:#deps, Error> #crt::SemanticObject<T, Error> for #name {
            fn default_rewrite(self, rewriter: &mut T) -> Result<Self, Error> {
                let #name #pattern = self;
                Ok(#name #field_rewrites)
            }
        }
    }
}

/// Emits a SemanticObject implementation for an enum.
fn emit_enum_semantic_object(name: syn::Ident, enm: syn::DataEnum) -> TokenStream2 {
    let mut variant_rewrites = quote! {};
    let mut deps = quote! {};
    for variant in enm.variants {
        let variant_name = variant.ident;
        let (current_deps, pattern, field_rewrites) = emit_fields_semantic_object(variant.fields);
        deps = quote! {#deps #current_deps};
        variant_rewrites = quote! {
            #variant_rewrites
            #name :: #variant_name #pattern => {
                #name :: #variant_name #field_rewrites
            }
        }
    }
    let crt = semantic_crate();
    quote! {
        impl<T:#deps, Error> #crt::SemanticObject<T, Error> for #name {
            fn default_rewrite(self, rewriter: &mut T) -> Result<Self, Error> {
                Ok(match self {
                    #variant_rewrites
                })
            }
        }
    }
}

/// Helper function for emit_struct_semantic_object and emit_enum_semantic_object. Both struct, and
/// a variant use a type called [syn::Fields].
/// This function builds and returns an unpacking pattern and code for `fmt()` on these fields.
fn emit_fields_semantic_object(fields: syn::Fields) -> (TokenStream2, TokenStream2, TokenStream2) {
    let mut pattern = quote! {};
    let mut field_rewrites = quote! {};
    let mut deps = quote! {};
    for (i, field) in fields.iter().enumerate() {
        let field_ident = field
            .ident
            .clone()
            .unwrap_or_else(|| syn::Ident::new(&format!("v{i}"), Span::call_site()));
        pattern = quote! { #pattern #field_ident, };

        let has_dont_rewrite_attr = field
            .attrs
            .iter()
            .any(|a| a.path().segments.len() == 1 && a.path().segments[0].ident == "dont_rewrite");
        let rewrite_expr = if has_dont_rewrite_attr {
            quote! { #field_ident }
        } else {
            emit_expr_for_ty(&mut deps, quote! {#field_ident}, &field.ty)
        };
        if let Some(field_ident) = &field.ident {
            field_rewrites = quote! {
                #field_rewrites
                #field_ident: #rewrite_expr,
            }
        } else {
            field_rewrites = quote! {
                #field_rewrites
                #rewrite_expr,
            }
        }
    }
    match fields {
        syn::Fields::Named(_) => {
            pattern = quote! { { #pattern } };
            field_rewrites = quote! { { #field_rewrites } };
        }
        syn::Fields::Unnamed(_) => {
            pattern = quote! { ( #pattern ) };
            field_rewrites = quote! { ( #field_rewrites ) };
        }
        syn::Fields::Unit => {
            pattern = quote! {};
            field_rewrites = quote! {};
        }
    };
    (deps, pattern, field_rewrites)
}

fn emit_expr_for_ty(deps: &mut TokenStream2, item: TokenStream2, ty: &syn::Type) -> TokenStream2 {
    let crt = semantic_crate();
    *deps = quote! { #deps #crt::substitution::SemanticRewriter<#ty, Error> + };
    quote! {
        #crt::substitution::SemanticRewriter::<#ty, Error>::rewrite(rewriter, #item)?
    }
}

/// Returns the identifier of the semantic crate. This is needed, since inside the semantic
/// crate, it needs to be referred to as `crate` and no `semantic`.
fn semantic_crate() -> syn::Ident {
    let crate_name = std::env::var("CARGO_PKG_NAME").unwrap();
    let res = match crate_name.as_str() {
        "cairo-lang-semantic" | "cairo_lang_semantic" => "crate",
        _ => "cairo_lang_semantic",
    };
    syn::Ident::new(res, Span::call_site())
}
