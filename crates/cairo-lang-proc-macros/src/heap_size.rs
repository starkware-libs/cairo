//! Procedural macros for HeapSize derive.
use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::quote;
use syn::{DeriveInput, Generics, parse_macro_input};

/// Derives a HeapSize implementation for structs and enums.
pub fn derive_heap_size(input: TokenStream) -> TokenStream {
    // Parse the input tokens into a syntax tree.
    let input = parse_macro_input!(input as DeriveInput);

    let name = &input.ident;
    let generics = &input.generics;

    // Generate the body per kind
    let body = match &input.data {
        syn::Data::Struct(s) => emit_struct_heap_size(name, generics, s),
        syn::Data::Enum(e) => emit_enum_heap_size(name, generics, e),
        syn::Data::Union(_) => panic!("Unions are not supported"),
    };

    quote! { #body }.into()
}

// ───────────────────────── helpers ──────────────────────────────────────────────

/// impl HeapSize for a *struct*
fn emit_struct_heap_size(
    name: &syn::Ident,
    generics: &Generics,
    structure: &syn::DataStruct,
) -> TokenStream2 {
    let heap_size_calls = emit_fields_heap_size(&structure.fields);

    let (impl_g, ty_g, where_g) = generics.split_for_impl();

    let crt = heap_size_crate();

    quote! {
        impl #impl_g #crt::HeapSize for #name #ty_g #where_g {
            fn heap_size(&self) -> usize {
                #heap_size_calls
            }
        }
    }
}

/// impl HeapSize for an *enum*
fn emit_enum_heap_size(
    name: &syn::Ident,
    generics: &Generics,
    enm: &syn::DataEnum,
) -> TokenStream2 {
    let mut arms = quote! {};
    for v in enm.variants.iter() {
        let v_ident = &v.ident;
        let (pat, heap_size_calls) = emit_variant_heap_size(v_ident.to_string(), &v.fields);
        arms = quote! {
            #arms
            #name :: #v_ident #pat => { #heap_size_calls }
        };
    }

    let (impl_g, ty_g, where_g) = generics.split_for_impl();

    let crt = heap_size_crate();

    quote! {
        impl #impl_g #crt::HeapSize for #name #ty_g #where_g {
            fn heap_size(&self) -> usize {
                match self {
                    #arms
                }
            }
        }
    }
}

/// Helper function for emit_enum_heap_size. Both struct and enum variants use
/// fields, but enum variants need pattern matching.
fn emit_variant_heap_size(
    _variant_name: String,
    fields: &syn::Fields,
) -> (TokenStream2, TokenStream2) {
    let crt = heap_size_crate();
    let mut pattern = quote! {};
    let mut calls = quote! { 0 };

    for (i, field) in fields.iter().enumerate() {
        let ty = &field.ty;

        let should_skip = should_skip_field(ty);

        let field_ident = field
            .ident
            .clone()
            .unwrap_or_else(|| syn::Ident::new(&format!("v{i}"), Span::call_site()));

        pattern = quote! { #pattern #field_ident, };

        if !should_skip {
            // In enum patterns, the bindings are already references, so we don't need &
            calls = quote! {
                #calls + #crt::HeapSize::heap_size(#field_ident)
            };
        }
    }

    match fields {
        syn::Fields::Named(_) => {
            pattern = quote! { { #pattern } };
        }
        syn::Fields::Unnamed(_) => {
            pattern = quote! { ( #pattern ) };
        }
        syn::Fields::Unit => {
            pattern = quote! {};
        }
    };

    (pattern, calls)
}

fn should_skip_field(ty: &syn::Type) -> bool {
    match ty {
        syn::Type::Path(type_path) => {
            let segments = &type_path.path.segments;
            // Skip Arc<T> and salsa::Id.
            (segments.len() == 1 && segments[0].ident == "Arc")
                || (segments.len() == 2
                    && segments[0].ident == "salsa"
                    && segments[1].ident == "Id")
        }
        syn::Type::Reference(_) => true,
        _ => false,
    }
}

/// Helper function to generate heap_size calls for struct fields.
/// Skips Arc types, references, and salsa::Id (no heap allocation for these).
fn emit_fields_heap_size(fields: &syn::Fields) -> TokenStream2 {
    let crt = heap_size_crate();
    let mut calls = quote! { 0 };

    for (index, field) in fields.iter().enumerate() {
        let ty = &field.ty;

        let should_skip = should_skip_field(ty);

        if !should_skip {
            let field_ident = match &field.ident {
                Some(ident) => quote! { #ident },
                None => {
                    let index = syn::Index::from(index);
                    quote! { #index }
                }
            };

            calls = quote! {
                #calls + #crt::HeapSize::heap_size(&self.#field_ident)
            };
        }
    }

    calls
}

/// Returns the identifier of the crate that has HeapSize.
fn heap_size_crate() -> syn::Ident {
    syn::Ident::new("cairo_lang_utils", Span::call_site())
}
