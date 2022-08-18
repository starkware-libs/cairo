// Module providing the gas related extensions.
use super::unpack_inputs;
use crate::define_extension_hierarchy;
use crate::extensions::{
    ConcreteExtension, GenericExtension, InputError, NamedExtension, NonBranchConcreteExtension,
    SpecializationError,
};
use crate::mem_cell::MemCell;
use crate::program::GenericArg;

define_extension_hierarchy! {
    pub enum GasExtension {
        GetGas(GetGasGeneric),
        RefundGas(RefundGasGeneric)
    }, GasConcrete
}

/// Helper for extracting a single positive value from template arguments.
fn as_single_positive_value(args: &[GenericArg]) -> Result<i64, SpecializationError> {
    match args {
        [GenericArg::Value(count)] if *count > 0 => Ok(*count),
        _ => Err(SpecializationError::UnsupportedGenericArg),
    }
}

/// Extension for getting gas branch.
#[derive(Default)]
pub struct GetGasGeneric {}
impl NamedExtension for GetGasGeneric {
    type Concrete = GetGasConcrete;
    const NAME: &'static str = "get_gas";
    fn specialize(&self, args: &[GenericArg]) -> Result<Self::Concrete, SpecializationError> {
        Ok(GetGasConcrete { count: as_single_positive_value(args)? })
    }
}

pub struct GetGasConcrete {
    count: i64,
}
impl ConcreteExtension for GetGasConcrete {
    fn simulate(
        &self,
        inputs: Vec<Vec<MemCell>>,
    ) -> Result<(Vec<Vec<MemCell>>, usize), InputError> {
        let [MemCell { value: gas_counter }] = unpack_inputs::<1>(inputs)?;
        if gas_counter >= self.count {
            // Have enough gas - return reduced counter and jump to success branch.
            Ok((vec![vec![(gas_counter - self.count).into()]], 0))
        } else {
            // Don't have enough gas - return the same counter and jump to failure branch.
            Ok((vec![vec![gas_counter.into()]], 1))
        }
    }
}

/// Extension for returning unused gas.
#[derive(Default)]
pub struct RefundGasGeneric {}
impl NamedExtension for RefundGasGeneric {
    type Concrete = RefundGasConcrete;
    const NAME: &'static str = "refund_gas";
    fn specialize(&self, args: &[GenericArg]) -> Result<Self::Concrete, SpecializationError> {
        Ok(RefundGasConcrete { count: as_single_positive_value(args)? })
    }
}

pub struct RefundGasConcrete {
    count: i64,
}
impl NonBranchConcreteExtension for RefundGasConcrete {
    fn non_branch_simulate(
        &self,
        inputs: Vec<Vec<MemCell>>,
    ) -> Result<Vec<Vec<MemCell>>, InputError> {
        let [MemCell { value: gas_counter }] = unpack_inputs::<1>(inputs)?;
        Ok(vec![vec![(gas_counter + self.count).into()]])
    }
}
