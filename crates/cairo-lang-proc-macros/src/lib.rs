use proc_macro::TokenStream;
use quote::quote;
use syn::parse::Parser;
use syn::punctuated::Punctuated;
use syn::{ItemFn, Meta, Token, parse_macro_input};

mod debug;
mod heap_size;
mod rewriter;

#[proc_macro_derive(DebugWithDb, attributes(debug_db, hide_field_debug_with_db))]
pub fn derive_debug_with_db(input: TokenStream) -> TokenStream {
    debug::derive_debug_with_db(input)
}

#[proc_macro_derive(HeapSize)]
pub fn derive_heap_size(input: TokenStream) -> TokenStream {
    heap_size::derive_heap_size(input)
}

#[proc_macro_derive(SemanticObject, attributes(dont_rewrite))]
pub fn derive_semantic_object(input: TokenStream) -> TokenStream {
    rewriter::derive_semantic_object(input)
}

/// ──────────────────────────────────────────────────────────────────────────────
/// Test macro
/// ──────────────────────────────────────────────────────────────────────────────
/// A test attribute macro that initializes logging before running a test.
/// This replaces the `test_log::test` macro with Cairo's own logging initialization.
///
/// # Example
/// ```ignore
/// use cairo_lang_proc_macros::test;
///
/// #[test]
/// fn my_test() {
///     // Test code here
/// }
/// ```
#[proc_macro_attribute]
pub fn test(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let input_fn = parse_macro_input!(item as ItemFn);

    let _fn_name = &input_fn.sig.ident;
    let fn_vis = &input_fn.vis;
    let fn_sig = &input_fn.sig;
    let fn_block = &input_fn.block;
    let fn_attrs = &input_fn.attrs;

    // Generate the test function with logging initialization
    let output = quote! {
        #[::core::prelude::v1::test]
        #(#fn_attrs)*
        #fn_vis #fn_sig {
            // Initialize logging with ERROR level for tests
            cairo_lang_test_utils::logging::init_logging(cairo_lang_test_utils::logging::level::ERROR);

            // Run the original test body
            #fn_block
        }
    };

    TokenStream::from(output)
}

#[proc_macro_attribute]
pub fn interned(attr: TokenStream, item: TokenStream) -> TokenStream {
    let parser = Punctuated::<Meta, Token![,]>::parse_terminated;
    #[allow(unused_mut)]
    let mut args: Punctuated<Meta, Token![,]> =
        if attr.is_empty() { Punctuated::new() } else { parser.parse(attr).unwrap() };

    let has_heap_size = args
        .iter()
        .any(|meta| matches!(meta, Meta::NameValue(nv) if nv.path.is_ident("heap_size")));

    if !has_heap_size {
        let heap_size: Meta = syn::parse_quote!(heap_size = cairo_lang_utils::heap_size);
        args.push(heap_size);
    }

    let salsa_attr = quote! { #[salsa::interned(#args)] };
    let item: proc_macro2::TokenStream = item.into();
    let output = quote! {
        #salsa_attr
        #item
    };
    output.into()
}

#[proc_macro_attribute]
pub fn tracked(attr: TokenStream, item: TokenStream) -> TokenStream {
    let parser = Punctuated::<Meta, Token![,]>::parse_terminated;
    #[allow(unused_mut)]
    let mut args: Punctuated<Meta, Token![,]> =
        if attr.is_empty() { Punctuated::new() } else { parser.parse(attr).unwrap() };

    let has_heap_size = args
        .iter()
        .any(|meta| matches!(meta, Meta::NameValue(nv) if nv.path.is_ident("heap_size")));

    if !has_heap_size {
        let heap_size: Meta = syn::parse_quote!(heap_size = cairo_lang_utils::heap_size);
        args.push(heap_size);
    }

    let salsa_attr = quote! { #[salsa::tracked(#args)] };
    let item: proc_macro2::TokenStream = item.into();
    let output = quote! {
        #salsa_attr
        #item
    };
    output.into()
}
