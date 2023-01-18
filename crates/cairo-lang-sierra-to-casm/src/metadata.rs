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
    let gas_info = calc_gas_info(program)?;
    let ap_change_info = calc_ap_changes(program, |idx, token_type| {
        gas_info.variable_values[(idx, token_type)] as usize
    })?;
    Ok(Metadata { ap_change_info, gas_info })
}
