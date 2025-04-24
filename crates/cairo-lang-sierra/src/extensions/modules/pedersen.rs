use super::felt252::Felt252Type;
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
    const ZERO_SIZED: bool = false;
}

define_libfunc_hierarchy! {
    pub enum PedersenLibfunc {
        PedersenHash(PedersenHashLibfunc),
    }, PedersenConcreteLibfunc
}

/// Libfunc for computing the Pedersen hash of two felt252s.
/// Returns a felt252 (and the updated builtin pointer).
#[derive(Default)]
pub struct PedersenHashLibfunc {}
impl NoGenericArgsGenericLibfunc for PedersenHashLibfunc {
    const STR_ID: &'static str = "pedersen";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let pedersen_ty = context.get_concrete_type(PedersenType::id(), &[])?;
        let felt252_ty = context.get_concrete_type(Felt252Type::id(), &[])?;
        let felt252_param = ParamSignature::new(felt252_ty.clone());
        Ok(LibfuncSignature::new_non_branch_ex(
            vec![
                ParamSignature::new(pedersen_ty.clone()).with_allow_add_const(),
                felt252_param.clone(),
                felt252_param,
            ],
            vec![
                OutputVarInfo::new_builtin(pedersen_ty, 0),
                OutputVarInfo {
                    ty: felt252_ty,
                    ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
                },
            ],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}
