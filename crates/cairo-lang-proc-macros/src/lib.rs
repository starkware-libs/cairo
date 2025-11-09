use proc_macro::TokenStream;
use quote::quote;
use syn::{ItemFn, parse_macro_input};

mod debug;
mod rewriter;

#[proc_macro_derive(DebugWithDb, attributes(debug_db, hide_field_debug_with_db))]
pub fn derive_debug_with_db(input: TokenStream) -> TokenStream {
    debug::derive_debug_with_db(input)
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
