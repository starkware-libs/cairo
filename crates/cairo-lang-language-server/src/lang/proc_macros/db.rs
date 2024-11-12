use super::client::ClientStatus;

#[salsa::query_group(ProcMacroDatabase)]
pub trait ProcMacroGroup {
    #[salsa::input]
    fn proc_macro_client_status(&self) -> ClientStatus;
}
