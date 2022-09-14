// Module providing the gas related extensions.
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{BranchReferenceInfo, LibFuncSignature, SpecializationContext};
use crate::extensions::{
    ConcreteType, NamedLibFunc, NamedType, NoGenericArgsGenericType, OutputVarReferenceInfo,
    SignatureBasedConcreteLibFunc, SpecializationError,
};
use crate::ids::{GenericLibFuncId, GenericTypeId};
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

/// Helper for extracting a single positive value from template arguments.
fn as_single_positive_value(args: &[GenericArg]) -> Result<i64, SpecializationError> {
    match args {
        [GenericArg::Value(count)] if *count > 0 => Ok(*count),
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
            count: as_single_positive_value(args)?,
            signature: LibFuncSignature {
                input_types: vec![gas_builtin_type.clone()],
                output_types: vec![
                    // Success:
                    vec![gas_builtin_type.clone()],
                    // Failure:
                    vec![gas_builtin_type],
                ],
                fallthrough: Some(1),
                output_ref_info: vec![
                    // Success:
                    BranchReferenceInfo(vec![OutputVarReferenceInfo::Deferred]),
                    // Failure:
                    BranchReferenceInfo(vec![OutputVarReferenceInfo::SameAsParam { param_idx: 0 }]),
                ],
            },
        })
    }
}

pub struct GetGasConcreteLibFunc {
    pub count: i64,
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
            count: as_single_positive_value(args)?,
            signature: LibFuncSignature::new_non_branch(
                vec![gas_builtin_type.clone()],
                vec![gas_builtin_type],
                vec![OutputVarReferenceInfo::Deferred],
            ),
        })
    }
}

pub struct RefundGasConcreteLibFunc {
    pub count: i64,
    pub signature: LibFuncSignature,
}
impl SignatureBasedConcreteLibFunc for RefundGasConcreteLibFunc {
    fn signature(&self) -> &LibFuncSignature {
        &self.signature
    }
}
