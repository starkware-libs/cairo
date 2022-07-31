use super::*;

struct GetGasExtension {}

impl ExtensionImplementation for GetGasExtension {
    fn simulate(
        &self,
        tmpl_args: &[TemplateArg],
        mut inputs: Vec<Vec<i64>>,
    ) -> Result<(Vec<Vec<i64>>, usize), Error> {
        let gas = gas_value_arg(tmpl_args)?;
        validate_mem_sizes(&inputs, [1])?;
        if inputs[0][0] >= gas {
            Ok((vec![vec![inputs[0][0] - gas]], 0))
        } else {
            Ok((vec![inputs.remove(0)], 1))
        }
    }
}

struct RefundGasExtension {}

impl NonBranchExtensionImplementation for RefundGasExtension {
    fn simulate(
        &self,
        tmpl_args: &[TemplateArg],
        inputs: Vec<Vec<i64>>,
    ) -> Result<Vec<Vec<i64>>, Error> {
        validate_mem_sizes(&inputs, [1])?;
        Ok(vec![vec![inputs[0][0] + gas_value_arg(tmpl_args)?]])
    }
}

fn gas_value_arg(tmpl_args: &[TemplateArg]) -> Result<i64, Error> {
    let gas = single_value_arg(tmpl_args)?;
    if gas <= 0 { Err(Error::UnsupportedTypeArg) } else { Ok(gas) }
}

pub(super) fn extensions() -> [(String, ExtensionBox); 2] {
    [
        ("get_gas".to_string(), Box::new(GetGasExtension {})),
        ("refund_gas".to_string(), wrap_non_branch(Box::new(RefundGasExtension {}))),
    ]
}
