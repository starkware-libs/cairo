use super::int::unsigned128::Uint128Type;
use crate::extensions::lib_func::{
    DeferredOutputKind, LibfuncSignature, OutputVarInfo, ParamSignature, SierraApChange,
    SignatureSpecializationContext,
};
use crate::extensions::{
    NamedType, NoGenericArgsGenericLibfunc, NoGenericArgsGenericType, OutputVarReferenceInfo,
    SpecializationError,
};
use crate::ids::GenericTypeId;

/// Type representing the Bitwise builtin.
#[derive(Default)]
pub struct BitwiseType {}
impl NoGenericArgsGenericType for BitwiseType {
    const ID: GenericTypeId = GenericTypeId::new_inline("Bitwise");
    const STORABLE: bool = true;
    const DUPLICATABLE: bool = false;
    const DROPPABLE: bool = false;
    const SIZE: i16 = 1;
}

/// Libfunc for computing the Bitwise (and,or,xor) of two u128s.
/// Returns 3 u128s (and the updated builtin pointer).
#[derive(Default)]
pub struct BitwiseLibfunc {}
impl NoGenericArgsGenericLibfunc for BitwiseLibfunc {
    const STR_ID: &'static str = "bitwise";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let bitwise_ty = context.get_concrete_type(BitwiseType::id(), &[])?;
        let u128_ty = context.get_concrete_type(Uint128Type::id(), &[])?;
        let deffered_u128_output_info = OutputVarInfo {
            ty: u128_ty.clone(),
            ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
        };
        Ok(LibfuncSignature::new_non_branch_ex(
            vec![
                ParamSignature::new(bitwise_ty.clone()).with_allow_add_const(),
                ParamSignature::new(u128_ty.clone()),
                ParamSignature::new(u128_ty),
            ],
            vec![
                OutputVarInfo {
                    ty: bitwise_ty,
                    ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::AddConst {
                        param_idx: 0,
                    }),
                },
                deffered_u128_output_info.clone(),
                deffered_u128_output_info.clone(),
                deffered_u128_output_info,
            ],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}
