use proc_macro::TokenStream;

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

// ──────────────────────────────────────────────────────────────────────────────
// Query group code - Copied from https://github.com/davidbarsky/db-ext-macro with small changes
// ──────────────────────────────────────────────────────────────────────────────

mod query_group;


// ——————————————————————————————————————————————————————————————————————————
// Entry point
// ——————————————————————————————————————————————————————————————————————————

#[proc_macro_attribute]
pub fn query_group(args: TokenStream, input: TokenStream) -> TokenStream {
    match query_group::query_group_impl(args, input.clone()) {
        Ok(tokens) => tokens.into(),
        Err(e) => query_group::token_stream_with_error(input, e),
    }
}