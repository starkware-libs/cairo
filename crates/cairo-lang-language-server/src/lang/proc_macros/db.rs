use super::client::ClientStatus;

/// `salsa` interface for proc macros support in LS.
#[salsa::query_group(ProcMacroDatabase)]
pub trait ProcMacroGroup {
    /// Allows access to client from compiler plugins `.generate_code()` methods.
    #[salsa::input]
    fn proc_macro_client_status(&self) -> ClientStatus;
}
