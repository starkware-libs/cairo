use std::sync::Arc;

use cairo_lang_defs::ids::ExternFunctionId;
use cairo_lang_semantic::helper::ModuleHelper;
use cairo_lang_utils::unordered_hash_set::UnorderedHashSet;

use crate::db::LoweringGroup;
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

pub fn priv_movable_function_ids(
    db: &dyn LoweringGroup,
) -> Arc<UnorderedHashSet<ExternFunctionId>> {
    let libfunc_by_name = |name: &String| {
        let mut path_iter = name.split("::");

        let mut module = ModuleHelper::core(db);

        let mut next = path_iter.next();
        while let Some(path_item) = next {
            next = path_iter.next();
            if next.is_some() {
                module = module.submodule(path_item);
                continue;
            }

            return module.extern_function_id(path_item);
        }

        panic!("Got empty string as movable_function");
    };

    Arc::new(db.optimization_config().moveable_functions.iter().map(libfunc_by_name).collect())
}
