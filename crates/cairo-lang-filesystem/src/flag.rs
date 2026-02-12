use cairo_lang_utils::Intern;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use salsa::{Database, Setter};
use serde::{Deserialize, Serialize};

use crate::db::files_group_input;
use crate::ids::{FlagId, FlagLongId};

/// A compilation flag.
#[derive(PartialEq, Eq, Debug, Copy, Clone, Serialize, Deserialize, Hash, salsa::Update)]
pub enum Flag {
    /// Whether to automatically add `withdraw_gas` calls in code cycles.
    /// Default is true - automatically add.
    ///
    /// Additionally controls addition of `redeposit_gas` which happens on default.
    AddWithdrawGas(bool),
    NumericMatchOptimizationMinArmsThreshold(usize),
    /// Whether to add panic backtrace handling to the generated code.
    ///
    /// Default is false - do not add, as it won't be used in production.
    PanicBacktrace(bool),
    /// Whether to use unsafe_panic in the generated code.
    ///
    /// Default is false as it makes panic unprovable.
    UnsafePanic(bool),
    /// Whether to use future_sierra in the generated code.
    ///
    /// Default is false.
    FutureSierra(bool),
}
impl Flag {
    pub const ADD_WITHDRAW_GAS: &'static str = "add_withdraw_gas";
    pub const NUMERIC_MATCH_OPTIMIZATION_MIN_ARMS_THRESHOLD: &'static str =
        "numeric_match_optimization_min_arms_threshold";
    pub const PANIC_BACKTRACE: &'static str = "panic_backtrace";
    pub const UNSAFE_PANIC: &'static str = "unsafe_panic";
    pub const FUTURE_SIERRA: &'static str = "future_sierra";
}

/// Extracts the value of a flag given the flag string and the expected variant.
/// Returns `None` if the flag is missing.
/// Panics if there is a variant mismatch.
macro_rules! extract_flag_value {
    ($db:ident, $flag:ident, $variant:ident) => {{
        let flag = FlagId::new($db, FlagLongId(Flag::$flag.into()));
        match $db.get_flag(flag) {
            None => None,
            Some(Flag::$variant(value)) => Some(value),
            Some(other) => panic!("Unexpected flag variant for `{}`: {other:?}", Flag::$flag),
        }
    }};
}

/// Returns the value of the `add_withdraw_gas` flag, or `true` if the flag is not set.
#[salsa::tracked]
fn flag_add_withdraw_gas(db: &dyn Database) -> bool {
    extract_flag_value!(db, ADD_WITHDRAW_GAS, AddWithdrawGas).unwrap_or(true)
}

/// Returns the value of the `numeric_match_optimization_min_arms_threshold` flag, or `None` if the
/// flag is not set.
#[salsa::tracked]
fn flag_numeric_match_optimization_min_arms_threshold(db: &dyn salsa::Database) -> Option<usize> {
    extract_flag_value!(
        db,
        NUMERIC_MATCH_OPTIMIZATION_MIN_ARMS_THRESHOLD,
        NumericMatchOptimizationMinArmsThreshold
    )
}

/// Returns the value of the `panic_backtrace` flag, or `false` if the flag is not set.
#[salsa::tracked]
fn flag_panic_backtrace(db: &dyn salsa::Database) -> bool {
    extract_flag_value!(db, PANIC_BACKTRACE, PanicBacktrace).unwrap_or_default()
}

/// Returns the value of the `unsafe_panic` flag, or `false` if the flag is not set.
#[salsa::tracked]
fn flag_unsafe_panic(db: &dyn salsa::Database) -> bool {
    extract_flag_value!(db, UNSAFE_PANIC, UnsafePanic).unwrap_or_default()
}

/// Returns the value of the `future_sierra` flag, or `false` if the flag is not set.
#[salsa::tracked]
fn flag_future_sierra(db: &dyn salsa::Database) -> bool {
    extract_flag_value!(db, FUTURE_SIERRA, FutureSierra).unwrap_or_default()
}

#[salsa::tracked(returns(ref))]
pub fn flags<'db>(db: &'db dyn Database) -> OrderedHashMap<FlagId<'db>, Flag> {
    let inp = files_group_input(db).flags(db).as_ref().expect("flags is not set");
    inp.iter().map(|(flag_id, flag)| (flag_id.clone().intern(db), *flag)).collect()
}

/// Returns a reference to the flag value.
#[salsa::tracked]
fn get_flag<'db>(db: &'db dyn Database, id: FlagId<'db>) -> Option<Flag> {
    db.flags().get(&id).copied()
}

pub trait FlagsGroup: Database {
    /// Interned version of `flags_input`.
    fn flags<'db>(&'db self) -> &'db OrderedHashMap<FlagId<'db>, Flag> {
        flags(self.as_dyn_database())
    }
    /// Query to get a compilation flag by its ID.
    fn get_flag<'db>(&'db self, id: FlagId<'db>) -> Option<Flag> {
        get_flag(self.as_dyn_database(), id)
    }

    /// Sets the given flag value. None value removes the flag.
    fn set_flag(&mut self, flag: FlagLongId, value: Option<Flag>) {
        let db_ref = self.as_dyn_database();
        let mut flags = files_group_input(db_ref).flags(db_ref).clone().unwrap();
        match value {
            Some(value) => flags.insert(flag, value),
            None => flags.swap_remove(&flag),
        };
        files_group_input(db_ref).set_flags(self).to(Some(flags));
    }
    /// Returns the value of the `add_withdraw_gas` flag.
    fn flag_add_withdraw_gas(&self) -> bool {
        flag_add_withdraw_gas(self.as_dyn_database())
    }
    /// Returns the value of the `numeric_match_optimization_min_arms_threshold` flag.
    fn flag_numeric_match_optimization_min_arms_threshold(&self) -> Option<usize> {
        flag_numeric_match_optimization_min_arms_threshold(self.as_dyn_database())
    }
    /// Returns the value of the `panic_backtrace` flag.
    fn flag_panic_backtrace(&self) -> bool {
        flag_panic_backtrace(self.as_dyn_database())
    }
    /// Returns the value of the `unsafe_panic` flag.
    fn flag_unsafe_panic(&self) -> bool {
        flag_unsafe_panic(self.as_dyn_database())
    }
    /// Returns the value of the `future_sierra` flag.
    fn flag_future_sierra(&self) -> bool {
        flag_future_sierra(self.as_dyn_database())
    }
}
impl<T: Database + ?Sized> FlagsGroup for T {}
