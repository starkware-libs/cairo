use cairo_lang_sierra::extensions::gas::CostTokenType;
use cairo_lang_sierra::ids::FunctionId;
use cairo_lang_sierra::program::Program;
use cairo_lang_sierra_ap_change::ap_change_info::ApChangeInfo;
use cairo_lang_sierra_ap_change::compute::calc_ap_changes as linear_calc_ap_changes;
use cairo_lang_sierra_ap_change::{calc_ap_changes, ApChangeError};
use cairo_lang_sierra_gas::gas_info::GasInfo;
use cairo_lang_sierra_gas::{
    calc_gas_postcost_info, calc_gas_precost_info, compute_postcost_info, compute_precost_info,
    CostError,
};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use thiserror::Error;

#[derive(Default)]
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
#[derive(Clone)]
pub struct MetadataComputationConfig {
    /// Functions to enforce costs for, as well as the costs to enforce.
    pub function_set_costs: OrderedHashMap<FunctionId, OrderedHashMap<CostTokenType, i32>>,
    /// If true, uses a linear-time algorithm for calculating the gas, instead of solving
    /// equations.
    pub linear_gas_solver: bool,
    /// If true, uses a linear-time algorithm for calculating ap changes, instead of solving
    /// equations.
    pub linear_ap_change_solver: bool,
}

impl Default for MetadataComputationConfig {
    fn default() -> Self {
        Self {
            function_set_costs: Default::default(),
            linear_gas_solver: true,
            linear_ap_change_solver: true,
        }
    }
}

/// Calculates the metadata for a Sierra program, with ap change info only.
pub fn calc_metadata_ap_change_only(program: &Program) -> Result<Metadata, MetadataError> {
    Ok(Metadata {
        ap_change_info: calc_ap_changes(program, |_, _| 0)?,
        gas_info: GasInfo {
            variable_values: Default::default(),
            function_costs: Default::default(),
        },
    })
}

/// Calculates the metadata for a Sierra program.
///
/// `no_eq_solver` uses a linear-time algorithm for calculating the gas, instead of solving
/// equations.
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
    let pre_gas_info_new = compute_precost_info(program)?;
    let pre_gas_info_old = calc_gas_precost_info(program, pre_function_set_costs)?;
    pre_gas_info_old.assert_eq_functions(&pre_gas_info_new);
    let pre_gas_info = if config.linear_gas_solver {
        pre_gas_info_new
    } else {
        pre_gas_info_old.assert_eq_variables(&pre_gas_info_new);
        pre_gas_info_old
    };

    let ap_change_info =
        if config.linear_ap_change_solver { linear_calc_ap_changes } else { calc_ap_changes }(
            program,
            |idx, token_type| pre_gas_info.variable_values[&(idx, token_type)] as usize,
        )?;

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
    let mut post_gas_info =
        calc_gas_postcost_info(program, post_function_set_costs, &pre_gas_info, |idx| {
            ap_change_info.variable_values.get(&idx).copied().unwrap_or_default()
        })?;

    if config.linear_gas_solver {
        let enforced_function_costs: OrderedHashMap<FunctionId, i32> = config
            .function_set_costs
            .iter()
            .map(|(func, costs)| (func.clone(), costs[&CostTokenType::Const]))
            .collect();
        let post_gas_info2 = compute_postcost_info(
            program,
            &|idx| ap_change_info.variable_values.get(idx).copied().unwrap_or_default(),
            &pre_gas_info,
            &enforced_function_costs,
        )?;

        post_gas_info.assert_eq_functions(&post_gas_info2);

        // Replace post_gas_info with the result of the non-equation-based algorithm.
        post_gas_info = post_gas_info2;
    }

    Ok(Metadata { ap_change_info, gas_info: pre_gas_info.combine(post_gas_info) })
}
