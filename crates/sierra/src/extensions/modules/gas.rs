// Module providing the gas related extensions.
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    BranchReferenceInfo, LibFuncSignature, SignatureOnlyConcreteLibFunc,
    SignatureSpecializationContext, SpecializationContext,
};
use crate::extensions::{
    ConcreteType, NamedType, NoGenericArgsGenericLibFunc, NoGenericArgsGenericType,
    OutputVarReferenceInfo, SpecializationError,
};
use crate::ids::{GenericLibFuncId, GenericTypeId};

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

/// LibFunc for getting gas branch.
#[derive(Default)]
pub struct GetGasLibFunc {}
impl NoGenericArgsGenericLibFunc for GetGasLibFunc {
    type Concrete = SignatureOnlyConcreteLibFunc;
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("get_gas");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibFuncSignature, SpecializationError> {
        let gas_builtin_type = context.get_concrete_type(GasBuiltinType::id(), &[])?;
        Ok(LibFuncSignature {
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
        })
    }

    fn specialize(
        &self,
        context: SpecializationContext<'_>,
    ) -> Result<Self::Concrete, SpecializationError> {
        Ok(SignatureOnlyConcreteLibFunc { signature: self.specialize_signature(&context)? })
    }
}

/// LibFunc for returning unused gas.
#[derive(Default)]
pub struct RefundGasLibFunc {}
impl NoGenericArgsGenericLibFunc for RefundGasLibFunc {
    type Concrete = SignatureOnlyConcreteLibFunc;
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("refund_gas");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibFuncSignature, SpecializationError> {
        let gas_builtin_type = context.get_concrete_type(GasBuiltinType::id(), &[])?;
        Ok(LibFuncSignature::new_non_branch(
            vec![gas_builtin_type.clone()],
            vec![gas_builtin_type],
            vec![OutputVarReferenceInfo::Deferred],
        ))
    }

    fn specialize(
        &self,
        context: SpecializationContext<'_>,
    ) -> Result<Self::Concrete, SpecializationError> {
        Ok(SignatureOnlyConcreteLibFunc { signature: self.specialize_signature(&context)? })
    }
}
