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

/// Type representing the Poseidon hash builtin.
#[derive(Default)]
pub struct PoseidonType {}
impl NoGenericArgsGenericType for PoseidonType {
    const ID: GenericTypeId = GenericTypeId::new_inline("Poseidon");
    const STORABLE: bool = true;
    const DUPLICATABLE: bool = false;
    const DROPPABLE: bool = false;
    const SIZE: i16 = 1;
}

define_libfunc_hierarchy! {
    pub enum PoseidonLibfunc {
        HadesPermutation(HadesPermutationLibfunc),
    }, PoseidonConcreteLibfunc
}

/// Libfunc for computing the Poseidon permutation over 3 felt252s.
/// Returns the 3 element state after the permutation (and the updated builtin pointer).
#[derive(Default)]
pub struct HadesPermutationLibfunc {}
impl NoGenericArgsGenericLibfunc for HadesPermutationLibfunc {
    const STR_ID: &'static str = "hades_permutation";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let poseidon_ty = context.get_concrete_type(PoseidonType::id(), &[])?;
        let felt252_ty = context.get_concrete_type(Felt252Type::id(), &[])?;
        Ok(LibfuncSignature::new_non_branch_ex(
            vec![
                ParamSignature::new(poseidon_ty.clone()).with_allow_add_const(),
                ParamSignature::new(felt252_ty.clone()),
                ParamSignature::new(felt252_ty.clone()),
                ParamSignature::new(felt252_ty.clone()),
            ],
            vec![
                OutputVarInfo {
                    ty: poseidon_ty,
                    ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::AddConst {
                        param_idx: 0,
                    }),
                },
                OutputVarInfo {
                    ty: felt252_ty.clone(),
                    ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
                },
                OutputVarInfo {
                    ty: felt252_ty.clone(),
                    ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
                },
                OutputVarInfo {
                    ty: felt252_ty,
                    ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
                },
            ],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}
