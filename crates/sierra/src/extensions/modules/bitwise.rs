use super::uint128::Uint128Type;
use crate::extensions::lib_func::{
    DeferredOutputKind, LibFuncSignature, OutputVarInfo, ParamSignature, SierraApChange,
    SignatureSpecializationContext,
};
use crate::extensions::types::{InfoOnlyConcreteType, TypeInfo};
use crate::extensions::{
    NamedType, NoGenericArgsGenericLibFunc, NoGenericArgsGenericType, OutputVarReferenceInfo,
    SpecializationError,
};
use crate::ids::{GenericLibFuncId, GenericTypeId};

/// Type representing the Bitwise builtin.
#[derive(Default)]
pub struct BitwiseType {}
impl NoGenericArgsGenericType for BitwiseType {
    type Concrete = InfoOnlyConcreteType;
    const ID: GenericTypeId = GenericTypeId::new_inline("Bitwise");

    fn specialize(&self) -> Self::Concrete {
        InfoOnlyConcreteType {
            info: TypeInfo {
                long_id: Self::concrete_type_long_id(&[]),
                storable: true,
                droppable: false,
                duplicatable: false,
                size: 1,
            },
        }
    }
}

/// LibFunc for computing the Bitwise (and,or,xor) of two u128s.
/// Returns 3 u128s (and the updated builtin pointer).
#[derive(Default)]
pub struct BitwiseLibFunc {}
impl NoGenericArgsGenericLibFunc for BitwiseLibFunc {
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("bitwise");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibFuncSignature, SpecializationError> {
        let bitwise_ty = context.get_concrete_type(BitwiseType::id(), &[])?;
        let u128_ty = context.get_concrete_type(Uint128Type::id(), &[])?;
        Ok(LibFuncSignature::new_non_branch_ex(
            vec![
                ParamSignature {
                    ty: bitwise_ty.clone(),
                    allow_deferred: false,
                    allow_add_const: true,
                    allow_const: false,
                },
                ParamSignature::new(u128_ty.clone()),
                ParamSignature::new(u128_ty.clone()),
            ],
            vec![
                OutputVarInfo {
                    ty: bitwise_ty,
                    ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::AddConst {
                        param_idx: 0,
                    }),
                },
                OutputVarInfo {
                    ty: u128_ty.clone(),
                    ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
                },
                OutputVarInfo {
                    ty: u128_ty.clone(),
                    ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
                },
                OutputVarInfo {
                    ty: u128_ty,
                    ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
                },
            ],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}
