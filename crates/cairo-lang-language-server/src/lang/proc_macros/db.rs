use rustc_hash::FxHashMap;
use scarb_proc_macro_server_types::methods::ProcMacroResult;
use scarb_proc_macro_server_types::methods::expand::{
    ExpandAttributeParams, ExpandDeriveParams, ExpandInlineMacroParams,
};

use super::client::ClientStatus;

/// `salsa` interface for proc macros support in LS.
#[salsa::query_group(ProcMacroDatabase)]
pub trait ProcMacroGroup {
    /// Allows access to client from compiler plugins `.generate_code()` methods.
    #[salsa::input]
    fn attribute_macro_resolution(&self) -> FxHashMap<ExpandAttributeParams, ProcMacroResult>;
    #[salsa::input]
    fn derive_macro_resolution(&self) -> FxHashMap<ExpandDeriveParams, ProcMacroResult>;
    #[salsa::input]
    fn inline_macro_resolution(&self) -> FxHashMap<ExpandInlineMacroParams, ProcMacroResult>;

    #[salsa::input]
    fn proc_macro_client_status(&self) -> ClientStatus;
}

pub fn init_proc_macro_group(db: &mut dyn ProcMacroGroup) {
    db.set_attribute_macro_resolution(Default::default());
    db.set_derive_macro_resolution(Default::default());
    db.set_inline_macro_resolution(Default::default());
    db.set_proc_macro_client_status(Default::default());
}
