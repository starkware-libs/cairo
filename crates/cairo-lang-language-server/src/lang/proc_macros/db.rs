use super::client::ClientStatus;

/// A set of queries that enable access to proc macro client from compiler plugins
/// `.generate_code()` methods.
#[salsa::query_group(ProcMacroDatabase)]
pub trait ProcMacroGroup {
    #[salsa::input]
    fn proc_macro_client_status(&self) -> ClientStatus;
}

pub fn init_proc_macro_group(db: &mut dyn ProcMacroGroup) {
    db.set_proc_macro_client_status(Default::default());
}
