use crate::extensions::{
    ConcreteExtension, ConcreteExtensionBox, Extension, ExtensionBox, SpecializationError,
};
use crate::program::{ExtensionId, TemplateArg};

fn get_count(tmpl_args: &[TemplateArg]) -> Result<i64, SpecializationError> {
    match tmpl_args {
        [TemplateArg::Value(count)] if *count > 0 => Ok(*count),
        _ => Err(SpecializationError::UnsupportedTemplateArg),
    }
}

struct GetGasExtension {}
impl Extension for GetGasExtension {
    fn specialize(
        &self,
        tmpl_args: &[TemplateArg],
    ) -> Result<ConcreteExtensionBox, SpecializationError> {
        Ok(Box::new(GetGas { _count: get_count(tmpl_args)? }))
    }
}
struct GetGas {
    _count: i64,
}
impl ConcreteExtension for GetGas {}

struct RefundGasExtension {}
impl Extension for RefundGasExtension {
    fn specialize(
        &self,
        tmpl_args: &[TemplateArg],
    ) -> Result<ConcreteExtensionBox, SpecializationError> {
        Ok(Box::new(RefundGas { _count: get_count(tmpl_args)? }))
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
