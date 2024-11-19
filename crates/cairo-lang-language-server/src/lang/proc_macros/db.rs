use std::sync::Arc;

use rustc_hash::FxHashMap;
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
    fn attribute_macro_resolution(&self) -> Arc<FxHashMap<ExpandAttributeParams, ProcMacroResult>>;
    #[salsa::input]
    fn derive_macro_resolution(&self) -> Arc<FxHashMap<ExpandDeriveParams, ProcMacroResult>>;
    #[salsa::input]
    fn inline_macro_resolution(&self) -> Arc<FxHashMap<ExpandInlineMacroParams, ProcMacroResult>>;

    #[salsa::input]
    fn proc_macro_client_status(&self) -> ClientStatus;
}

pub fn init_proc_macro_group(db: &mut dyn ProcMacroGroup) {
    db.set_attribute_macro_resolution(Default::default());
    db.set_derive_macro_resolution(Default::default());
    db.set_inline_macro_resolution(Default::default());
    db.set_proc_macro_client_status(Default::default());
}
