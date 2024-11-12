use cairo_lang_syntax::node::db::SyntaxGroup;

use crate::lang::db::AnalysisDatabase;

/// This function is necessary due to trait bounds that cannot be bypassed in any other way.
/// `generate_code()` takes db: [`&dyn SyntaxGroup`](`SyntaxGroup`),
/// but we need to use
/// [`ProcMacroGroup`](`crate::lang::proc_macros::db::ProcMacroGroup`). To do
/// this, we first convert the `db` reference to its original concrete type that implements both
/// traits [`AnalysisDatabase`]. After this,
/// [`ProcMacroGroup`](`crate::lang::proc_macros::db::ProcMacroGroup`) can be
/// accessed.
///
/// Safety: This function MUST only be invoked with an object that is of type
/// [AnalysisDatabase]. Using it with any other type leads to undefined behavior.
pub(super) unsafe fn unsafe_downcast_ref(db: &dyn SyntaxGroup) -> &AnalysisDatabase {
    // Replicated logic from `impl dyn Any downcast_ref_unchecked()`.
    // This approach works as long as `impl dyn Any downcast_ref_unchecked()` implementation is
    // unchanged and the caller can ensure that `db` is truly an instance of AnalysisDatabase.
    &*(db as *const dyn SyntaxGroup as *const AnalysisDatabase)
}

#[cfg(test)]
mod unsafe_downcast_ref_tests {
    use std::collections::HashMap;
    use std::sync::Arc;

    use cairo_lang_macro::TokenStream;
    use cairo_lang_syntax::node::db::SyntaxGroup;
    use scarb_proc_macro_server_types::methods::ProcMacroResult;
    use scarb_proc_macro_server_types::methods::expand::ExpandAttributeParams;

    use super::unsafe_downcast_ref;
    use crate::lang::db::AnalysisDatabase;
    use crate::lang::proc_macros::db::ProcMacroGroup;

    #[test]
    fn cast_succeed() {
        let mut db = AnalysisDatabase::new(&Default::default());

        let input = ExpandAttributeParams {
            attr: "asd".to_string(),
            args: TokenStream::new("asd".to_string()),
            item: TokenStream::new("asd".to_string()),
        };
        let output = ProcMacroResult {
            token_stream: TokenStream::new("asd".to_string()),
            diagnostics: Default::default(),
        };
        let macro_resolution: HashMap<_, _> = [(input, output)].into_iter().collect();
        let macro_resolution = Arc::new(macro_resolution);

        db.set_attribute_macro_resolution(macro_resolution.clone());

        let syntax: &dyn SyntaxGroup = &db;
        let analysis_db = unsafe { unsafe_downcast_ref(syntax) };

        assert_eq!(analysis_db.attribute_macro_resolution(), macro_resolution);
    }
}
