use std::fmt::Display;

use cairo_lang_sierra::extensions::gas::{CostTokenMap, CostTokenType};
use cairo_lang_utils::collection_arithmetics::SubCollection;
use thiserror::Error;

#[derive(Error, Debug, Eq, PartialEq)]
pub enum GasWalletError {
    #[error(
        "Ran out of gas ({token_type:?}) in the wallet, requested {cost:?} causing state {state}."
    )]
    OutOfGas { state: GasWallet, cost: CostTokenMap<i64>, token_type: CostTokenType },
}

/// Environment tracking the amount of gas available in a statement's context.
#[derive(Clone, Debug)]
pub enum GasWallet {
    /// A known value.
    Value(CostTokenMap<i64>),
    /// If gas tracking is disabled, this value should be used for all the statements.
    Disabled,
}
impl GasWallet {
    /// Updates the value in the wallet by subtracting `cost`. Can be both positive (for most
    /// libfuncs) and negative (for gas acquisition libfuncs).
    pub fn update(self, cost: CostTokenMap<i64>) -> Result<Self, GasWalletError> {
        match self {
            Self::Value(existing) => {
                let updated = existing.sub_collection(cost.iter().map(|(k, v)| (*k, *v)));
                if let Some(token_type) = updated.iter().find_map(|(k, v)| (*v < 0).then_some(*k)) {
                    let state = GasWallet::Value(updated);
                    return Err(GasWalletError::OutOfGas { state, cost, token_type });
                }
                Ok(GasWallet::Value(updated))
            }
            Self::Disabled => Ok(Self::Disabled),
        }
    }
}

impl Display for GasWallet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Value(value) => write!(f, "GasWallet::Value({value:?})"),
            Self::Disabled => write!(f, "GasWallet::Disabled"),
        }
    }
}
impl Eq for GasWallet {}
impl PartialEq for GasWallet {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Value(a), Self::Value(b)) => a.eq_unordered(b),
            (Self::Disabled, Self::Disabled) => true,
            _ => false,
        }
    }
}
