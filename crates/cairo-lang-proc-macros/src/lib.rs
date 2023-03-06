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
