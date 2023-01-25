use super::felt::FeltType;
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    DeferredOutputKind, LibfuncSignature, OutputVarInfo, ParamSignature, SierraApChange,
    SignatureSpecializationContext,
};
use crate::extensions::{
    NamedType, NoGenericArgsGenericLibfunc, NoGenericArgsGenericType, OutputVarReferenceInfo,
    SpecializationError,
};
use crate::ids::GenericTypeId;

/// Type representing the Pedersen hash builtin.
#[derive(Default)]
pub struct PedersenType {}
impl NoGenericArgsGenericType for PedersenType {
    const ID: GenericTypeId = GenericTypeId::new_inline("Pedersen");
    const STORABLE: bool = true;
    const DUPLICATABLE: bool = false;
    const DROPPABLE: bool = false;
    const SIZE: i16 = 1;
}

define_libfunc_hierarchy! {
    pub enum PedersenLibfunc {
        Hash(PedersenHashLibfunc),
    }, PedersenConcreteLibfunc
}

/// Libfunc for computing the Pedersen hash of two felts.
/// Returns a felt (and the updated builtin pointer).
#[derive(Default)]
pub struct PedersenHashLibfunc {}
impl NoGenericArgsGenericLibfunc for PedersenHashLibfunc {
    const STR_ID: &'static str = "pedersen";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let pedersen_ty = context.get_concrete_type(PedersenType::id(), &[])?;
        let felt_ty = context.get_concrete_type(FeltType::id(), &[])?;
        Ok(LibfuncSignature::new_non_branch_ex(
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
