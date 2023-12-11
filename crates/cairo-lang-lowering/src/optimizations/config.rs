use std::sync::Arc;

use cairo_lang_semantic::corelib;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_utils::unordered_hash_set::UnorderedHashSet;

use crate::db::LoweringGroup;
use crate::ids::{FunctionId, FunctionLongId};

/// A configuration struct that controls the behavior of the optimization passes.
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct OptimizationConfig {
    /// A list of functions that can be moved during the reorder_statements optimization.
    pub moveable_functions: Vec<String>,
}

pub fn priv_movable_function_ids(db: &dyn LoweringGroup) -> Arc<UnorderedHashSet<FunctionId>> {
    let semantic_db: &dyn SemanticGroup = db.elongate();
    let libfunc_by_name = |name: &String| {
        db.intern_lowering_function(FunctionLongId::Semantic(corelib::get_core_function_id(
            semantic_db,
            name.into(),
            vec![],
        )))
    };

    Arc::new(db.optimization_config().moveable_functions.iter().map(libfunc_by_name).collect())
}
