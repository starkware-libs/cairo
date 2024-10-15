use rustc_hash::FxHashMap;

use super::client::{ProcMacroClient, ProcMacroResult};
use cairo_lang_macro::TokenStream;

#[salsa::query_group(ProcMacroCacheDatabase)]
pub trait ProcMacroCacheGroup {
    #[salsa::input]
    fn attribute_macro_resolution(
        &self,
    ) -> FxHashMap<(String, TokenStream, TokenStream), ProcMacroResult>;

    #[salsa::input]
    fn derive_macro_resolution(&self) -> FxHashMap<(Vec<String>, TokenStream), ProcMacroResult>;

    #[salsa::input]
    fn inline_macro_resolution(&self) -> FxHashMap<TokenStream, ProcMacroResult>;

    #[salsa::input]
    fn proc_macro_client(&self) -> ProcMacroClient;

    // not sure if this will bloat memory very fast
    // as it is hard to clear this cache
    // #[salsa::interned]
    // fn intern_token_stream(&self, field: TokenStream) -> TokenStreamId;

    /// Returns the expansion of macro defined by token streams.
    fn get_attribute_expansion(
        &self,
        name: String,
        args: TokenStream,
        token_stream: TokenStream,
    ) -> ProcMacroResult;
    /// Returns the expansion of macro defined by token streams.
    fn get_derive_expansion(
        &self,
        names: Vec<String>,
        token_stream: TokenStream,
    ) -> ProcMacroResult;
    /// Returns the expansion of macro defined by token streams.
    fn get_inline_macros_expansion(&self, token_stream: TokenStream) -> ProcMacroResult;
}

fn get_attribute_expansion(
    db: &dyn ProcMacroCacheGroup,
    name: String,
    args: TokenStream,
    token_stream: TokenStream,
) -> ProcMacroResult {
    let all_args = (name, args, token_stream);

    let result = db.attribute_macro_resolution().get(&all_args).cloned();

    match result {
        Some(item) => item,
        None => {
            let origin = all_args.2.clone();

            db.proc_macro_client().request_attribute(all_args.0, all_args.1, all_args.2);

            ProcMacroResult { token_stream: origin, diagnostics: Default::default() }
        }
    }
}

fn get_derive_expansion(
    db: &dyn ProcMacroCacheGroup,
    names: Vec<String>,
    token_stream: TokenStream,
) -> ProcMacroResult {
    let all_args = (names, token_stream);

    let result = db.derive_macro_resolution().get(&all_args).cloned();

    match result {
        Some(item) => item,
        None => {
            let origin = all_args.1.clone();

            db.proc_macro_client().request_derives(all_args.0, all_args.1);

            ProcMacroResult { token_stream: origin, diagnostics: Default::default() }
        }
    }
}

fn get_inline_macros_expansion(
    db: &dyn ProcMacroCacheGroup,
    token_stream: TokenStream,
) -> ProcMacroResult {
    let result = db.inline_macro_resolution().get(&token_stream).cloned();

    match result {
        Some(item) => item,
        None => {
            let origin = token_stream.clone();

            db.proc_macro_client().request_inline_macros(token_stream);

            ProcMacroResult { token_stream: origin, diagnostics: Default::default() }
        }
    }
}
