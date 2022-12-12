use std::fmt::Display;

use sierra::extensions::builtin_cost::CostTokenType;
use thiserror::Error;
use utils::collection_arithmetics::add_maps;
use utils::ordered_hash_map::OrderedHashMap;

#[derive(Error, Debug, Eq, PartialEq)]
pub enum GasWalletError {
    #[error(
        "Ran out of gas ({token_type:?}) in the wallet, requested {request:?} while state is \
         {state}."
    )]
    OutOfGas {
        state: GasWallet,
        request: Box<OrderedHashMap<CostTokenType, i64>>,
        token_type: CostTokenType,
    },
}

/// Enviroment tracking the amount of gas available in a statement's context.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum GasWallet {
    /// A known value.
    Value(OrderedHashMap<CostTokenType, i64>),
    /// If gas tracking is disabled, this value should be used for all the statements.
    Disabled,
}
impl GasWallet {
    /// Updates the value in the wallet by `request`. Can be both negative (for most libfuncs) and
    /// positive (for gas acquisition libfuncs).
    pub fn update(
        &self,
        request: OrderedHashMap<CostTokenType, i64>,
    ) -> Result<Self, GasWalletError> {
        match &self {
            Self::Value(existing) => {
                let new_value = add_maps(existing.clone(), request.clone());
                for (token_type, val) in new_value.iter() {
                    if *val < 0 {
                        return Err(GasWalletError::OutOfGas {
                            state: self.clone(),
                            request: Box::new(request),
                            token_type: *token_type,
                        });
                    }
                }
                Ok(GasWallet::Value(new_value))
            }
            Self::Disabled => Ok(Self::Disabled),
        }
    }
}

impl Display for GasWallet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Value(value) => write!(f, "GasWallet::Value({:?})", value),
            Self::Disabled => write!(f, "GasWallet::Disabled"),
        }
    }
}
