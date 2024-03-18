use std::sync::Arc;

use cairo_lang_semantic::corelib;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_utils::unordered_hash_set::UnorderedHashSet;

use crate::db::LoweringGroup;
use crate::ids::{FunctionId, FunctionLongId};

/// The default threshold for inlining small functions. Decided according to sample contracts
/// profiling.
// TODO(Gil): Expose this as a configuration in the project toml.
const DEFAULT_INLINE_SMALL_FUNCTIONS_THRESHOLD: usize = 24;

/// A configuration struct that controls the behavior of the optimization passes.
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct OptimizationConfig {
    /// A list of functions that can be moved during the reorder_statements optimization.
    pub moveable_functions: Vec<String>,
    /// The size of functions (in lowering statements) below which they are marked as
    /// `should_inline`.
    pub inline_small_functions_threshold: usize,
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
    /// Sets the threshold for inlining small functions.
    pub fn with_inline_small_functions_threshold(
        mut self,
        inline_small_functions_threshold: usize,
    ) -> Self {
        self.inline_small_functions_threshold = inline_small_functions_threshold;
        self
    }
}

impl Default for OptimizationConfig {
    fn default() -> Self {
        Self {
            moveable_functions: vec![],
            inline_small_functions_threshold: DEFAULT_INLINE_SMALL_FUNCTIONS_THRESHOLD,
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

            return db.intern_lowering_function(FunctionLongId::Semantic(
                corelib::get_function_id(semantic_db, module, path_item.into(), vec![]),
            ));
        }

        panic!("Got empty string as movable_function");
    };

    Arc::new(db.optimization_config().moveable_functions.iter().map(libfunc_by_name).collect())
}
