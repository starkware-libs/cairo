use crate::lang::db::AnalysisDatabase;
use cairo_lang_syntax::node::db::SyntaxGroup;

/// This function is necessary due to trait bounds that cannot be bypassed in any other way.
/// `generate_code()` takes db: [`&dyn SyntaxGroup`],
/// but we need to use [`ProcMacroCacheGroup`]. To do this, we first convert the `db` reference
/// to its original concrete type that implements both traits [`AnalysisDatabase`].
/// After this, [`ProcMacroCacheGroup`] can be accessed.
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
    use super::unsafe_downcast_ref;
    use crate::lang::{
        db::AnalysisDatabase,
        proc_macros::{cache_group::ProcMacroCacheGroup, client::ProcMacroResult},
    };
    use cairo_lang_macro::TokenStream;
    use cairo_lang_syntax::node::db::SyntaxGroup;
    use rustc_hash::FxHashMap;

    #[test]
    fn cast_succeed() {
        let mut db = AnalysisDatabase::new(&Default::default(), &Default::default(), None);

        let input = (
            "asd".to_string(),
            TokenStream::new("asd".to_string()),
            TokenStream::new("asd".to_string()),
        );
        let output = ProcMacroResult {
            token_stream: TokenStream::new("asd".to_string()),
            diagnostics: Default::default(),
        };
        let macro_resolution: FxHashMap<_, _> = [(input, output)].into_iter().collect();

        db.set_attribute_macro_resolution(macro_resolution.clone());

        let syntax: &dyn SyntaxGroup = &db;
        let analysis_db = unsafe { unsafe_downcast_ref(syntax) };

        assert_eq!(analysis_db.attribute_macro_resolution(), macro_resolution);
    }
}
