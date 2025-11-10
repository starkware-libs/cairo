use cairo_lang_defs::ids::ExternFunctionId;
use cairo_lang_semantic::helper::ModuleHelper;
use cairo_lang_utils::unordered_hash_set::UnorderedHashSet;
use itertools::chain;
use salsa::Database;

use crate::db::LoweringGroup;
use crate::utils::InliningStrategy;

/// A configuration that controls occurrences of optimizations and their behavior.
#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Optimizations {
    Disabled,
    Enabled(OptimizationConfig),
}

/// A configuration struct that controls the behavior of the optimization passes.
#[derive(Default, Debug, Eq, PartialEq, Clone)]
pub struct OptimizationConfig {
    /// A list of functions that can be moved during the reorder_statements optimization.
    pub(crate) moveable_functions: Vec<String>,
    /// Determines whether inlining is disabled.
    pub(crate) inlining_strategy: InliningStrategy,
    /// Should const folding be skipped.
    pub(crate) skip_const_folding: bool,
}

impl OptimizationConfig {
    pub fn with_skip_const_folding(mut self, skip_const_folding: bool) -> Self {
        self.skip_const_folding = skip_const_folding;
        self
    }
}

impl Optimizations {
    /// Returns enabled optimization with the list of moveable functions set to a default set and
    /// `inlining_strategy` set to the passed value.
    pub fn enabled_with_default_movable_functions(inlining_strategy: InliningStrategy) -> Self {
        Self::Enabled(OptimizationConfig {
            moveable_functions: default_moveable_functions(),
            inlining_strategy,
            skip_const_folding: false,
        })
    }

    /// Returns enabled optimization with the list of moveable functions set to a minimal set.
    /// Useful for testing.
    pub fn enabled_with_minimal_movable_functions() -> Self {
        Self::Enabled(OptimizationConfig {
            moveable_functions: vec!["felt252_sub".to_string()],
            inlining_strategy: Default::default(),
            skip_const_folding: false,
        })
    }

    /// A slice of function names that can be moved during the reorder_statements optimization.
    /// If `self` is [`Optimizations::Disabled`] returns an empty slice.
    pub fn moveable_functions(&self) -> &[String] {
        if let Self::Enabled(config) = self { &config.moveable_functions } else { &[] }
    }

    /// Inlining strategy that should be used.
    /// If `self` is [`Optimizations::Disabled`] returns [`InliningStrategy::Avoid`].
    pub fn inlining_strategy(&self) -> InliningStrategy {
        if let Self::Enabled(config) = self {
            config.inlining_strategy
        } else {
            InliningStrategy::Avoid
        }
    }

    /// Whether to skip const folding. If `self` is [`Optimizations::Disabled`] returns `true`.
    pub fn skip_const_folding(&self) -> bool {
        if let Self::Enabled(config) = self { config.skip_const_folding } else { true }
    }
}

#[salsa::tracked(returns(ref))]
pub fn priv_movable_function_ids<'db>(
    db: &'db dyn Database,
) -> UnorderedHashSet<ExternFunctionId<'db>> {
    db.optimizations()
        .moveable_functions()
        .iter()
        .map(|name: &String| {
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
        })
        .collect()
}

/// The default list of function names that can be moved during the reorder_statements optimization.
fn default_moveable_functions() -> Vec<String> {
    let mut moveable_functions: Vec<String> = chain!(
        ["bool_not_impl"],
        ["felt252_add", "felt252_sub", "felt252_mul", "felt252_div"],
        ["array::array_new", "array::array_append"],
        ["box::unbox", "box::box_forward_snapshot", "box::into_box"],
    )
    .map(|s| s.to_string())
    .collect();

    for ty in ["i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64"] {
        moveable_functions.push(format!("integer::{ty}_wide_mul"));
    }
    moveable_functions
}
