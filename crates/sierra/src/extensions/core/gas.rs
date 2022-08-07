// Module providing the gas related extensions.
use crate::extensions::{
    ConcreteExtension, ConcreteExtensionBox, Extension, ExtensionBox, SpecializationError,
};
use crate::program::{ExtensionId, TemplateArg};

/// Helper for extracting a single positive value from template arguments.
fn as_single_positive_value(args: &[TemplateArg]) -> Result<i64, SpecializationError> {
    match args {
        [TemplateArg::Value(count)] if *count > 0 => Ok(*count),
        _ => Err(SpecializationError::UnsupportedTemplateArg),
    }
}

/// Extension for getting gas branch.
struct GetGasExtension {}
impl Extension for GetGasExtension {
    fn specialize(
        &self,
        args: &[TemplateArg],
    ) -> Result<ConcreteExtensionBox, SpecializationError> {
        Ok(Box::new(GetGasConcrete { _count: as_single_positive_value(args)? }))
    }
}

struct GetGasConcrete {
    _count: i64,
}
impl ConcreteExtension for GetGasConcrete {}

/// Extension for returning unused gas.
struct RefundGasExtension {}
impl Extension for RefundGasExtension {
    fn specialize(
        &self,
        args: &[TemplateArg],
    ) -> Result<ConcreteExtensionBox, SpecializationError> {
        Ok(Box::new(RefundGasConcrete { _count: as_single_positive_value(args)? }))
    }
}

struct RefundGasConcrete {
    _count: i64,
}
impl ConcreteExtension for RefundGasConcrete {}

pub(super) fn extensions() -> [(ExtensionId, ExtensionBox); 2] {
    [
        ("get_gas".into(), Box::new(GetGasExtension {})),
        ("refund_gas".into(), Box::new(RefundGasExtension {})),
    ]
}
