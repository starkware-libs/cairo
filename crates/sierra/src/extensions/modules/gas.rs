// Module providing the gas related extensions.
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{LibFuncSignature, SpecializationContext};
use crate::extensions::{
    ConcreteType, NamedLibFunc, NamedType, NoGenericArgsGenericType, SignatureBasedConcreteLibFunc,
    SpecializationError,
};
use crate::ids::{GenericLibFuncId, GenericTypeId, SymbolId};
use crate::program::GenericArg;

/// Type for gas actions.
#[derive(Default)]
pub struct GasBuiltinType {}
impl NoGenericArgsGenericType for GasBuiltinType {
    type Concrete = GasBuiltinConcreteType;
    const ID: GenericTypeId = GenericTypeId::new_inline("GasBuiltin");
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

/// Helper for extracting a single symbol from template arguments.
fn as_constant(args: &[GenericArg]) -> Result<SymbolId, SpecializationError> {
    match args {
        [GenericArg::Symbol(id)] => Ok(id.clone()),
        _ => Err(SpecializationError::UnsupportedGenericArg),
    }
}

/// LibFunc for getting gas branch.
#[derive(Default)]
pub struct GetGasLibFunc {}
impl NamedLibFunc for GetGasLibFunc {
    type Concrete = GetGasConcreteLibFunc;
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("get_gas");
    fn specialize(
        &self,
        context: SpecializationContext<'_>,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        let gas_builtin_type = context.get_concrete_type(GasBuiltinType::id(), &[])?;
        Ok(GetGasConcreteLibFunc {
            count: as_constant(args)?,
            signature: LibFuncSignature {
                input_types: vec![gas_builtin_type.clone()],
                output_types: vec![
                    // success=
                    vec![gas_builtin_type.clone()],
                    // failure=
                    vec![gas_builtin_type],
                ],
                fallthrough: Some(1),
            },
        })
    }
}

pub struct GetGasConcreteLibFunc {
    pub count: SymbolId,
    pub signature: LibFuncSignature,
}
impl SignatureBasedConcreteLibFunc for GetGasConcreteLibFunc {
    fn signature(&self) -> &LibFuncSignature {
        &self.signature
    }
}

/// LibFunc for returning unused gas.
#[derive(Default)]
pub struct RefundGasLibFunc {}
impl NamedLibFunc for RefundGasLibFunc {
    type Concrete = RefundGasConcreteLibFunc;
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("refund_gas");
    fn specialize(
        &self,
        context: SpecializationContext<'_>,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        let gas_builtin_type = context.get_concrete_type(GasBuiltinType::id(), &[])?;
        Ok(RefundGasConcreteLibFunc {
            count: as_constant(args)?,
            signature: LibFuncSignature::new_non_branch(
                vec![gas_builtin_type.clone()],
                vec![gas_builtin_type],
            ),
        })
    }
}

pub struct RefundGasConcreteLibFunc {
    pub count: SymbolId,
    pub signature: LibFuncSignature,
}
impl SignatureBasedConcreteLibFunc for RefundGasConcreteLibFunc {
    fn signature(&self) -> &LibFuncSignature {
        &self.signature
    }
}
