use std::collections::HashMap;
use std::sync::Arc;

use cairo_lang_macro::TokenStream;
use scarb_proc_macro_server_types::methods::ProcMacroResult;
use scarb_proc_macro_server_types::methods::expand::{
    ExpandAttributeParams, ExpandDeriveParams, ExpandInlineMacroParams,
};

use super::client::ClientStatus;

/// A set of queries that enable access to proc macro client from compiler plugins
/// `.generate_code()` methods.
#[salsa::query_group(ProcMacroDatabase)]
pub trait ProcMacroGroup {
    #[salsa::input]
    fn attribute_macro_resolution(&self) -> Arc<HashMap<ExpandAttributeParams, ProcMacroResult>>;
    #[salsa::input]
    fn derive_macro_resolution(&self) -> Arc<HashMap<ExpandDeriveParams, ProcMacroResult>>;
    #[salsa::input]
    fn inline_macro_resolution(&self) -> Arc<HashMap<ExpandInlineMacroParams, ProcMacroResult>>;

    #[salsa::input]
    fn proc_macro_client_status(&self) -> ClientStatus;

    /// Returns the expansion of attribute macro.
    fn get_attribute_expansion(&self, params: ExpandAttributeParams) -> ProcMacroResult;
    /// Returns the expansion of derive macros.
    fn get_derive_expansion(&self, params: ExpandDeriveParams) -> ProcMacroResult;
    /// Returns the expansion of inline macro.
    fn get_inline_macros_expansion(&self, params: ExpandInlineMacroParams) -> ProcMacroResult;
}

pub fn init_proc_macro_group(db: &mut dyn ProcMacroGroup) {
    db.set_attribute_macro_resolution(Default::default());
    db.set_derive_macro_resolution(Default::default());
    db.set_inline_macro_resolution(Default::default());
    db.set_proc_macro_client_status(Default::default());
}

fn get_attribute_expansion(
    db: &dyn ProcMacroGroup,
    params: ExpandAttributeParams,
) -> ProcMacroResult {
    db.attribute_macro_resolution().get(&params).cloned().unwrap_or_else(|| {
        let token_stream = params.item.clone();

        if let Some(client) = db.proc_macro_client_status().ready() {
            client.request_attribute(params);
        }

        ProcMacroResult { token_stream, diagnostics: Default::default() }
    })
}

fn get_derive_expansion(db: &dyn ProcMacroGroup, params: ExpandDeriveParams) -> ProcMacroResult {
    db.derive_macro_resolution().get(&params).cloned().unwrap_or_else(|| {
        let token_stream = params.item.clone();

        if let Some(client) = db.proc_macro_client_status().ready() {
            client.request_derives(params);
        }

        ProcMacroResult { token_stream, diagnostics: Default::default() }
    })
}

fn get_inline_macros_expansion(
    db: &dyn ProcMacroGroup,
    params: ExpandInlineMacroParams,
) -> ProcMacroResult {
    db.inline_macro_resolution().get(&params).cloned().unwrap_or_else(|| {
        // we can't return original node because it will make infinite recursive resolving.
        let token_stream = TokenStream::new("()".to_string());

        if let Some(client) = db.proc_macro_client_status().ready() {
            client.request_inline_macros(params);
        }

        ProcMacroResult { token_stream, diagnostics: Default::default() }
    })
}
