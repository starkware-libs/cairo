use cairo_lang_sierra::extensions::builtin_cost::CostTokenType;
use cairo_lang_sierra::ids::FunctionId;
use cairo_lang_sierra::program::Program;
use cairo_lang_sierra_ap_change::ap_change_info::ApChangeInfo;
use cairo_lang_sierra_ap_change::{calc_ap_changes, ApChangeError};
use cairo_lang_sierra_gas::gas_info::GasInfo;
use cairo_lang_sierra_gas::{calc_gas_postcost_info, calc_gas_precost_info, CostError};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use thiserror::Error;

/// Metadata provided with a Sierra program to simplify the compilation to casm.
pub struct Metadata {
    /// AP changes information for Sierra user functions.
    pub ap_change_info: ApChangeInfo,
    /// Gas information for validating Sierra code and taking the appropriate amount of gas.
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

/// Configuration for metadata computation.
#[derive(Default)]
pub struct MetadataComputationConfig {
    pub function_set_costs: OrderedHashMap<FunctionId, OrderedHashMap<CostTokenType, i32>>,
}

/// Calculates the metadata for a Sierra program.
pub fn calc_metadata(
    program: &Program,
    config: MetadataComputationConfig,
) -> Result<Metadata, MetadataError> {
    let pre_function_set_costs = config
        .function_set_costs
        .iter()
        .map(|(func, costs)| {
            (
                func.clone(),
                CostTokenType::iter_precost()
                    .filter_map(|token| costs.get(token).map(|v| (*token, *v)))
                    .collect(),
            )
        })
        .collect();
    let pre_gas_info = calc_gas_precost_info(program, pre_function_set_costs)?;

    let ap_change_info = calc_ap_changes(program, |idx, token_type| {
        pre_gas_info.variable_values[(idx, token_type)] as usize
    })?;

    let post_function_set_costs = config
        .function_set_costs
        .iter()
        .map(|(func, costs)| {
            (
                func.clone(),
                [CostTokenType::Const]
                    .iter()
                    .filter_map(|token| costs.get(token).map(|v| (*token, *v)))
                    .collect(),
            )
        })
        .collect();
    let post_gas_info =
        calc_gas_postcost_info(program, post_function_set_costs, &pre_gas_info, |idx| {
            ap_change_info.variable_values.get(&idx).copied().unwrap_or_default()
        })?;

    Ok(Metadata { ap_change_info, gas_info: pre_gas_info.combine(post_gas_info) })
}
