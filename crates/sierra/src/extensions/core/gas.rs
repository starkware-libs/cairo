// Module provinding the gas related extensions.
use crate::extensions::{
    ConcreteExtension, ConcreteExtensionBox, Extension, ExtensionBox, SpecializationError,
};
use crate::program::{ExtensionId, TemplateArg};

/// Helper for extracting count and testing from the template arguments.
fn get_count(args: &[TemplateArg]) -> Result<i64, SpecializationError> {
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
        Ok(Box::new(GetGas { _count: get_count(args)? }))
    }
}

struct GetGas {
    _count: i64,
}
impl ConcreteExtension for GetGas {}

/// Extension for returning unused gas.
struct RefundGasExtension {}
impl Extension for RefundGasExtension {
    fn specialize(
        &self,
        args: &[TemplateArg],
    ) -> Result<ConcreteExtensionBox, SpecializationError> {
        Ok(Box::new(RefundGas { _count: get_count(args)? }))
    }
}

struct RefundGas {
    _count: i64,
}
impl ConcreteExtension for RefundGas {}

pub(super) fn extensions() -> [(ExtensionId, ExtensionBox); 2] {
    [
        (ExtensionId::Name("get_gas".into()), Box::new(GetGasExtension {})),
        (ExtensionId::Name("refund_gas".into()), Box::new(RefundGasExtension {})),
    ]
}
