use std::sync::Arc;

use cairo_lang_semantic::corelib;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_utils::Intern;
use cairo_lang_utils::unordered_hash_set::UnorderedHashSet;

use crate::db::LoweringGroup;
use crate::ids::{FunctionId, FunctionLongId};
use crate::utils::InliningStrategy;

/// A configuration struct that controls the behavior of the optimization passes.
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct OptimizationConfig {
    /// A list of functions that can be moved during the reorder_statements optimization.
    pub moveable_functions: Vec<String>,
    /// Determines whether inlining is disabled.
    pub inlining_strategy: InliningStrategy,
    /// Should const folding be skipped.
    pub skip_const_folding: bool,
}

impl OptimizationConfig {
    /// Sets the list of moveable functions.
    pub fn with_moveable_functions(mut self, moveable_functions: Vec<String>) -> Self {
        self.moveable_functions = moveable_functions;
        self
    }
    /// Sets the list of moveable functions to a minimal set, useful for testing.
    pub fn with_minimal_movable_functions(self) -> Self {
        self.with_moveable_functions(vec!["felt252_sub".into()])
    }
    /// Sets the `inlining_strategy` flag.
    pub fn with_inlining_strategy(mut self, inlining_strategy: InliningStrategy) -> Self {
        self.inlining_strategy = inlining_strategy;
        self
    }
    /// Sets the `skip_const_folding` flag.
    pub fn with_skip_const_folding(mut self, skip_const_folding: bool) -> Self {
        self.skip_const_folding = skip_const_folding;
        self
    }
}

impl Default for OptimizationConfig {
    fn default() -> Self {
        Self {
            moveable_functions: vec![],
            inlining_strategy: InliningStrategy::Default,
            skip_const_folding: false,
        }
    }
}

pub fn priv_movable_function_ids(db: &dyn LoweringGroup) -> Arc<UnorderedHashSet<FunctionId>> {
    let semantic_db: &dyn SemanticGroup = db.elongate();
    let libfunc_by_name = |name: &String| {
        let mut path_iter = name.split("::");

        let mut module = db.core_module();

        let mut next = path_iter.next();
        while let Some(path_item) = next {
            next = path_iter.next();
            if next.is_some() {
                module = corelib::get_submodule(semantic_db, module, path_item)
                    .unwrap_or_else(|| panic!("module not found: {}", path_item));
                continue;
            }

            return FunctionLongId::Semantic(corelib::get_function_id(
                semantic_db,
                module,
                path_item.into(),
                vec![],
            ))
            .intern(db);
        }

        panic!("Got empty string as movable_function");
    };

    Arc::new(db.optimization_config().moveable_functions.iter().map(libfunc_by_name).collect())
}
