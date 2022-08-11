// Module providing the gas related extensions.
use crate::extensions::{
    ConcreteExtension, ConcreteExtensionBox, GenericExtensionBox, GenericTypeBox,
    NoGenericArgsGenericType, NoRegistryRequiredGenericExtension, SpecializationError,
};
use crate::program::{GenericArg, GenericExtensionId, GenericTypeId};

/// Helper for extracting a single positive value from template arguments.
fn as_single_positive_value(args: &[GenericArg]) -> Result<i64, SpecializationError> {
    match args {
        [GenericArg::Value(count)] if *count > 0 => Ok(*count),
        _ => Err(SpecializationError::UnsupportedGenericArg),
    }
}

/// Extension for getting gas branch.
struct GetGasGeneric {}
impl NoRegistryRequiredGenericExtension for GetGasGeneric {
    fn specialize(&self, args: &[GenericArg]) -> Result<ConcreteExtensionBox, SpecializationError> {
        Ok(Box::new(GetGasConcrete { _count: as_single_positive_value(args)? }))
    }
}

struct GetGasConcrete {
    _count: i64,
}
impl ConcreteExtension for GetGasConcrete {}

/// Extension for returning unused gas.
struct RefundGasGeneric {}
impl NoRegistryRequiredGenericExtension for RefundGasGeneric {
    fn specialize(&self, args: &[GenericArg]) -> Result<ConcreteExtensionBox, SpecializationError> {
        Ok(Box::new(RefundGasConcrete { _count: as_single_positive_value(args)? }))
    }
}

struct RefundGasConcrete {
    _count: i64,
}
impl ConcreteExtension for RefundGasConcrete {}

pub(super) fn extensions() -> [(GenericExtensionId, GenericExtensionBox); 2] {
    [
        ("get_gas".into(), Box::new(GetGasGeneric {})),
        ("refund_gas".into(), Box::new(RefundGasGeneric {})),
    ]
}

pub(super) fn types() -> [(GenericTypeId, GenericTypeBox); 1] {
    [("GasBuiltin".into(), Box::new(NoGenericArgsGenericType::<1> {}))]
}
