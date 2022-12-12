use super::felt::FeltType;
use crate::define_libfunc_hierarchy;
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

/// Type representing the Pedersen hash builtin.
#[derive(Default)]
pub struct PedersenType {}
impl NoGenericArgsGenericType for PedersenType {
    type Concrete = InfoOnlyConcreteType;
    const ID: GenericTypeId = GenericTypeId::new_inline("Pedersen");

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

define_libfunc_hierarchy! {
    pub enum PedersenLibFunc {
        Hash(PedersenHashLibFunc),
    }, PedersenConcreteLibFunc
}

/// LibFunc for computing the Pedersen hash of two felts.
/// Returns a felt (and the updated builtin pointer).
#[derive(Default)]
pub struct PedersenHashLibFunc {}
impl NoGenericArgsGenericLibFunc for PedersenHashLibFunc {
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("pedersen");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibFuncSignature, SpecializationError> {
        let pedersen_ty = context.get_concrete_type(PedersenType::id(), &[])?;
        let felt_ty = context.get_concrete_type(FeltType::id(), &[])?;
        Ok(LibFuncSignature::new_non_branch_ex(
            vec![
                ParamSignature {
                    ty: pedersen_ty.clone(),
                    allow_deferred: false,
                    allow_add_const: true,
                    allow_const: false,
                },
                ParamSignature::new(felt_ty.clone()),
                ParamSignature::new(felt_ty.clone()),
            ],
            vec![
                OutputVarInfo {
                    ty: pedersen_ty,
                    ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::AddConst {
                        param_idx: 0,
                    }),
                },
                OutputVarInfo {
                    ty: felt_ty,
                    ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
                },
            ],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}
