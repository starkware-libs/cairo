// Module providing the gas related extensions.
use crate::define_extension_hierarchy;
use crate::extensions::{
    ConcreteExtension, NoGenericArgsNamedType, NoRegistryRequiredNamedExtension,
    SpecializationError,
};
use crate::program::GenericArg;

#[derive(Default)]
pub struct GasBuiltinType {}
impl NoGenericArgsNamedType for GasBuiltinType {
    const NAME: &'static str = "GasBuiltin";
    const SIZE: usize = 1;
}

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
impl NoRegistryRequiredNamedExtension for GetGasGeneric {
    type Concrete = GetGasConcrete;
    const NAME: &'static str = "get_gas";
    fn specialize(&self, args: &[GenericArg]) -> Result<Self::Concrete, SpecializationError> {
        Ok(GetGasConcrete { count: as_single_positive_value(args)? })
    }
}

pub struct GetGasConcrete {
    pub count: i64,
}
impl ConcreteExtension for GetGasConcrete {}

/// Extension for returning unused gas.
#[derive(Default)]
pub struct RefundGasGeneric {}
impl NoRegistryRequiredNamedExtension for RefundGasGeneric {
    type Concrete = RefundGasConcrete;
    const NAME: &'static str = "refund_gas";
    fn specialize(&self, args: &[GenericArg]) -> Result<Self::Concrete, SpecializationError> {
        Ok(RefundGasConcrete { count: as_single_positive_value(args)? })
    }
}

pub struct RefundGasConcrete {
    pub count: i64,
}
impl ConcreteExtension for RefundGasConcrete {}
