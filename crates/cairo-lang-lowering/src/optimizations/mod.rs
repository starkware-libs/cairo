/// Macro for debug logging with "optimization" target.
#[allow(unused)]
macro_rules! debug {
    ($($arg:tt)*) => {
        tracing::debug!(target: "optimization", $($arg)*)
    };
}
#[allow(unused_imports)]
pub(crate) use debug;

/// Macro for trace logging with "optimization" target.
#[allow(unused)]
macro_rules! trace {
    ($($arg:tt)*) => {
        tracing::trace!(target: "optimization", $($arg)*)
    };
}
#[allow(unused_imports)]
pub(crate) use trace;

pub mod branch_inversion;
pub mod cancel_ops;
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
