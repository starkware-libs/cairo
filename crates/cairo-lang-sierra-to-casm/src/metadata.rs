use std::collections::hash_map::Entry;
use std::collections::HashSet;

use cairo_lang_sierra::extensions::builtin_cost::CostTokenType;
use cairo_lang_sierra::program::Program;
use cairo_lang_sierra_ap_change::ap_change_info::ApChangeInfo;
use cairo_lang_sierra_ap_change::{calc_ap_changes, ApChangeError};
use cairo_lang_sierra_gas::gas_info::GasInfo;
use cairo_lang_sierra_gas::{calc_gas_info, CostError};
use thiserror::Error;

/// Metadata provided with a Sierra program to simplify the compilation to casm.
pub struct Metadata {
    /// AP changes information for Sierra user functions.
    pub ap_change_info: ApChangeInfo,
    /// Gas information for validating Sierra code and taking the apporiate amount of gas.
    pub gas_info: GasInfo,
}

/// Error for metadata calculations.
#[derive(Debug, Error, Eq, PartialEq)]
pub enum MetadataError {
    #[error(transparent)]
    ApChangeError(#[from] ApChangeError),
    #[error(transparent)]
    CostError(#[from] CostError),
}

/// Calculates the metadata for a Sierra program.
pub fn calc_metadata(program: &Program) -> Result<Metadata, MetadataError> {
    let token_gas_info = calc_gas_info(program, &HashSet::from([CostTokenType::Pedersen]))?;
    let ap_change_info = calc_ap_changes(program)?;
    let mut gas_info = calc_gas_info(program, &HashSet::from([CostTokenType::Step]))?;
    gas_info.variable_values.extend(token_gas_info.variable_values.into_iter());
    for (func_id, costs) in token_gas_info.function_costs.into_iter() {
        match gas_info.function_costs.entry(func_id) {
            Entry::Occupied(mut e) => e.get_mut().extend(costs.into_iter()),
            Entry::Vacant(e) => {
                e.insert(costs);
            }
        };
    }
    Ok(Metadata { ap_change_info, gas_info })
}
