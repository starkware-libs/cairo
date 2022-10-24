// Module providing the gas related extensions.
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    BranchSignature, DeferredOutputKind, LibFuncSignature, OutputVarInfo, ParamSignature,
    SierraApChange, SignatureOnlyConcreteLibFunc, SignatureSpecializationContext,
    SpecializationContext,
};
use crate::extensions::types::{InfoOnlyConcreteType, TypeInfo};
use crate::extensions::{
    NamedType, NoGenericArgsGenericLibFunc, NoGenericArgsGenericType, OutputVarReferenceInfo,
    SpecializationError,
};
use crate::ids::{GenericLibFuncId, GenericTypeId};

/// Type for gas actions.
#[derive(Default)]
pub struct GasBuiltinType {}
impl NoGenericArgsGenericType for GasBuiltinType {
    type Concrete = InfoOnlyConcreteType;
    const ID: GenericTypeId = GenericTypeId::new_inline("GasBuiltin");

    fn specialize(&self) -> Self::Concrete {
        InfoOnlyConcreteType {
            info: TypeInfo {
                long_id: Self::concrete_type_long_id(&[]),
                storable: true,
                droppable: false,
                duplicatable: false,
            },
        }
    }
}

define_libfunc_hierarchy! {
    pub enum GasLibFunc {
        BurnGas(BurnGasLibFunc),
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
            param_signatures: vec![ParamSignature::new(gas_builtin_type.clone())],
            branch_signatures: vec![
                // Success:
                BranchSignature {
                    vars: vec![OutputVarInfo {
                        ty: gas_builtin_type.clone(),
                        ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
                    }],
                    ap_change: SierraApChange::NotImplemented,
                },
                // Failure:
                BranchSignature {
                    vars: vec![OutputVarInfo {
                        ty: gas_builtin_type,
                        ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 0 },
                    }],
                    ap_change: SierraApChange::NotImplemented,
                },
            ],
            fallthrough: Some(1),
        })
    }

    fn specialize(
        &self,
        context: &dyn SpecializationContext,
    ) -> Result<Self::Concrete, SpecializationError> {
        Ok(SignatureOnlyConcreteLibFunc { signature: self.specialize_signature(context.upcast())? })
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
            vec![OutputVarInfo {
                ty: gas_builtin_type,
                ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
            }],
            SierraApChange::NotImplemented,
        ))
    }

    fn specialize(
        &self,
        context: &dyn SpecializationContext,
    ) -> Result<Self::Concrete, SpecializationError> {
        Ok(SignatureOnlyConcreteLibFunc { signature: self.specialize_signature(context.upcast())? })
    }
}

/// LibFunc for burning gas.
/// Used to equalize gas costs across merging paths without using the gas builtin.
/// If the gas builtin is available, it is typically better to use refund_gas.
#[derive(Default)]
pub struct BurnGasLibFunc {}
impl NoGenericArgsGenericLibFunc for BurnGasLibFunc {
    type Concrete = SignatureOnlyConcreteLibFunc;
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("burn_gas");

    fn specialize_signature(
        &self,
        _context: &dyn SignatureSpecializationContext,
    ) -> Result<LibFuncSignature, SpecializationError> {
        Ok(LibFuncSignature::new_non_branch(vec![], vec![], SierraApChange::Known))
    }

    fn specialize(
        &self,
        context: &dyn SpecializationContext,
    ) -> Result<Self::Concrete, SpecializationError> {
        Ok(SignatureOnlyConcreteLibFunc { signature: self.specialize_signature(context.upcast())? })
    }
}
