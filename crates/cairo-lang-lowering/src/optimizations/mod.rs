/// Macro for debug logging with "optimization" target.
#[allow(unused)]
macro_rules! debug {
    ($($arg:tt)*) => {
        tracing::debug!(target: "optimization", $($arg)*)
    };
}

/// Macro for trace logging with "optimization" target.
#[allow(unused)]
macro_rules! trace {
    ($($arg:tt)*) => {
        tracing::trace!(target: "optimization", $($arg)*)
    };
}

pub mod branch_inversion;
pub mod config;
pub mod const_folding;
pub mod cse;
pub mod dedup_blocks;
pub mod early_unsafe_panic;
pub mod gas_redeposit;
pub mod match_optimizer;
pub mod reboxing;
pub mod remappings;
pub mod reorder_statements;
pub mod return_optimization;
pub mod scrub_units;
pub mod split_structs;
pub mod strategy;
pub mod trim_unreachable;
pub mod validate;
pub mod var_renamer;
pub mod variable_forwarding;

// TODO(eytan-starkware): Move these cases into `variable_forwarding` test data and remove this
// module. The `cancel_ops` phase was removed, but its test cases are kept here, wired to
// `variable_forwarding`, until they are migrated.
#[cfg(test)]
#[path = "cancel_ops_test.rs"]
mod cancel_ops_test;
