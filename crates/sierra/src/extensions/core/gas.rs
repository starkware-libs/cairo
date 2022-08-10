// Module providing the gas related extensions.
use super::unpack_inputs;
use crate::extensions::{
    ConcreteExtension, ConcreteExtensionBox, GenericExtension, GenericExtensionBox, InputError,
    NonBranchConcreteExtension, SpecializationError,
};
use crate::ids::GenericExtensionId;
use crate::mem_cell::MemCell;
use crate::program::GenericArg;

/// Helper for extracting a single positive value from template arguments.
fn as_single_positive_value(args: &[GenericArg]) -> Result<i64, SpecializationError> {
    match args {
        [GenericArg::Value(count)] if *count > 0 => Ok(*count),
        _ => Err(SpecializationError::UnsupportedGenericArg),
    }
}

/// Extension for getting gas branch.
struct GetGasGeneric {}
impl GenericExtension for GetGasGeneric {
    fn specialize(&self, args: &[GenericArg]) -> Result<ConcreteExtensionBox, SpecializationError> {
        Ok(Box::new(GetGasConcrete { count: as_single_positive_value(args)? }))
    }
}

struct GetGasConcrete {
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
struct RefundGasGeneric {}
impl GenericExtension for RefundGasGeneric {
    fn specialize(&self, args: &[GenericArg]) -> Result<ConcreteExtensionBox, SpecializationError> {
        Ok(Box::new(RefundGasConcrete { count: as_single_positive_value(args)? }))
    }
}

struct RefundGasConcrete {
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

pub(super) fn extensions() -> [(GenericExtensionId, GenericExtensionBox); 2] {
    [
        ("get_gas".into(), Box::new(GetGasGeneric {})),
        ("refund_gas".into(), Box::new(RefundGasGeneric {})),
    ]
}
