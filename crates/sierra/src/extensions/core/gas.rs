// Module providing the gas related extensions.
use crate::extensions::{ConcreteExtension, GenericExtension, SpecializationError};
use crate::ids::GenericExtensionId;
use crate::program::GenericArg;
use crate::super_extension;

super_extension! {
    pub enum GasExtension {
        GetGas(GetGasGeneric),
        RefundGas(RefundGasGeneric)
    }, GasConcrete
}

/// Helper for extracting a single positive value from template arguments.
fn as_single_positive_value(args: &[GenericArg]) -> Result<i64, SpecializationError> {
    match args {
        [GenericArg::Value(count)] if *count > 0 => Ok(*count),
        _ => Err(SpecializationError::UnsupportedTemplateArg),
    }
}

/// Extension for getting gas branch.
pub struct GetGasGeneric {}
impl GenericExtension for GetGasGeneric {
    type Concrete = GetGasConcrete;
    fn id() -> Option<GenericExtensionId> {
        Some("get_gas".into())
    }
    fn new() -> Option<Self> {
        Some(Self {})
    }
    fn specialize(&self, args: &[GenericArg]) -> Result<Self::Concrete, SpecializationError> {
        Ok(GetGasConcrete { _count: as_single_positive_value(args)? })
    }
}

pub struct GetGasConcrete {
    _count: i64,
}
impl ConcreteExtension for GetGasConcrete {}

/// Extension for returning unused gas.
pub struct RefundGasGeneric {}
impl GenericExtension for RefundGasGeneric {
    type Concrete = RefundGasConcrete;
    fn id() -> Option<GenericExtensionId> {
        Some("refund_gas".into())
    }
    fn new() -> Option<Self> {
        Some(Self {})
    }
    fn specialize(&self, args: &[GenericArg]) -> Result<Self::Concrete, SpecializationError> {
        Ok(RefundGasConcrete { _count: as_single_positive_value(args)? })
    }
}

pub struct RefundGasConcrete {
    _count: i64,
}
impl ConcreteExtension for RefundGasConcrete {}
