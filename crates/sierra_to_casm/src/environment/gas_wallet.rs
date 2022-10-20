use std::fmt::Display;

use thiserror::Error;

#[derive(Error, Debug, Eq, PartialEq)]
pub enum GasWalletError {
    #[error("Ran out of gas in the wallet, requested {request} while state is {state}.")]
    OutOfGas { state: GasWallet, request: i64 },
}

/// Enviroment tracking the amount of gas available in a statement's context.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum GasWallet {
    /// A known value.
    Value(i64),
    /// If gas tracking is disabled, this value should be used for all the statements.
    Disabled,
}
impl GasWallet {
    /// Updates the value in the wallet by `request`. Can be both negative (for most libfuncs) and
    /// positive (for gas acquisition libfuncs).
    pub fn update(&self, request: i64) -> Result<Self, GasWalletError> {
        match &self {
            Self::Value(existing) => {
                if existing + request >= 0 {
                    Ok(GasWallet::Value(existing + request))
                } else {
                    Err(GasWalletError::OutOfGas { state: *self, request })
                }
            }
            Self::Disabled => Ok(Self::Disabled),
        }
    }
}

impl Display for GasWallet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Value(change) => write!(f, "GasWallet::Value({})", change),
            Self::Disabled => write!(f, "GasWallet::Disabled"),
        }
    }
}
