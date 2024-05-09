//! This module contains definitions of custom methods (aka requests or notifications) for the
//! Language Server Protocol that are provided by the Cairo Language Server.

use tower_lsp::lsp_types::request::Request;

#[cfg(feature = "testing")]
pub mod test {
    use super::*;

    pub struct DebugProjects;
    impl Request for DebugProjects {
        type Params = ();
        type Result = String;
        const METHOD: &'static str = "test/debugProjects";
    }
}
