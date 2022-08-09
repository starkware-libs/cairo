// Module providing the gas related extensions.
use crate::extensions::{
    ConcreteExtension, ConcreteExtensionBox, GenericExtension, GenericExtensionBox,
    SpecializationError,
};
use crate::program::{GenericArg, GenericExtensionId};

/// Helper for extracting a single positive value from template arguments.
fn as_single_positive_value(args: &[GenericArg]) -> Result<i64, SpecializationError> {
    match args {
        [GenericArg::Value(count)] if *count > 0 => Ok(*count),
        _ => Err(SpecializationError::UnsupportedTemplateArg),
    }
}

/// Extension for getting gas branch.
struct GetGasGeneric {}
impl GenericExtension for GetGasGeneric {
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
impl GenericExtension for RefundGasGeneric {
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
