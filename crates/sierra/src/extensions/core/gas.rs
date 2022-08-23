// Module providing the gas related extensions.
use super::mem::DeferredType;
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::SpecializationContext;
use crate::extensions::{
    ConcreteLibFunc, ConcreteType, GenericLibFunc, NamedLibFunc, NamedType,
    NoGenericArgsGenericType, NonBranchConcreteLibFunc, SpecializationError,
};
use crate::ids::ConcreteTypeId;
use crate::program::GenericArg;

/// Type for gas actions.
#[derive(Default)]
pub struct GasBuiltinType {}
impl NoGenericArgsGenericType for GasBuiltinType {
    type Concrete = GasBuiltinConcreteType;
    const NAME: &'static str = "GasBuiltin";
}
#[derive(Default)]
pub struct GasBuiltinConcreteType {}
impl ConcreteType for GasBuiltinConcreteType {}

define_libfunc_hierarchy! {
    pub enum GasLibFunc {
        GetGas(GetGasLibFunc),
        RefundGas(RefundGasLibFunc),
    }, GasConcreteLibFunc
}

/// Helper for extracting a single positive value from template arguments.
fn as_single_positive_value(args: &[GenericArg]) -> Result<i64, SpecializationError> {
    match args {
        [GenericArg::Value(count)] if *count > 0 => Ok(*count),
        _ => Err(SpecializationError::UnsupportedGenericArg),
    }
}

fn get_gas_types(
    context: &SpecializationContext<'_>,
) -> Result<(ConcreteTypeId, ConcreteTypeId), SpecializationError> {
    let gas_builtin_type = context.get_concrete_type(GasBuiltinType::id(), &[])?;
    Ok((
        gas_builtin_type.clone(),
        context.get_wrapped_concrete_type(DeferredType::id(), gas_builtin_type)?,
    ))
}

/// LibFunc for getting gas branch.
#[derive(Default)]
pub struct GetGasLibFunc {}
impl NamedLibFunc for GetGasLibFunc {
    type Concrete = GetGasConcreteLibFunc;
    const NAME: &'static str = "get_gas";
    fn specialize(
        &self,
        context: SpecializationContext<'_>,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        let (gas_builtin_type, deferred_gas_builtin_type) = get_gas_types(&context)?;
        Ok(GetGasConcreteLibFunc {
            count: as_single_positive_value(args)?,
            gas_builtin_type,
            deferred_gas_builtin_type,
        })
    }
}

pub struct GetGasConcreteLibFunc {
    pub count: i64,
    pub gas_builtin_type: ConcreteTypeId,
    pub deferred_gas_builtin_type: ConcreteTypeId,
}
impl ConcreteLibFunc for GetGasConcreteLibFunc {
    fn input_types(&self) -> Vec<ConcreteTypeId> {
        vec![self.gas_builtin_type.clone()]
    }
    fn output_types(&self) -> Vec<Vec<ConcreteTypeId>> {
        vec![
            // success=
            vec![self.deferred_gas_builtin_type.clone()],
            // failure=
            vec![self.gas_builtin_type.clone()],
        ]
    }
    fn fallthrough(&self) -> Option<usize> {
        Some(1)
    }
}

/// LibFunc for returning unused gas.
#[derive(Default)]
pub struct RefundGasLibFunc {}
impl NamedLibFunc for RefundGasLibFunc {
    type Concrete = RefundGasConcreteLibFunc;
    const NAME: &'static str = "refund_gas";
    fn specialize(
        &self,
        context: SpecializationContext<'_>,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        let (gas_builtin_type, deferred_gas_builtin_type) = get_gas_types(&context)?;
        Ok(RefundGasConcreteLibFunc {
            count: as_single_positive_value(args)?,
            gas_builtin_type,
            deferred_gas_builtin_type,
        })
    }
}

pub struct RefundGasConcreteLibFunc {
    pub count: i64,
    pub gas_builtin_type: ConcreteTypeId,
    pub deferred_gas_builtin_type: ConcreteTypeId,
}
impl NonBranchConcreteLibFunc for RefundGasConcreteLibFunc {
    fn input_types(&self) -> Vec<ConcreteTypeId> {
        vec![self.gas_builtin_type.clone()]
    }
    fn output_types(&self) -> Vec<ConcreteTypeId> {
        vec![self.deferred_gas_builtin_type.clone()]
    }
}
