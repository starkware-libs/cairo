use super::boxing::box_ty;
use super::int::unsigned::Uint32Type;
use super::utils::fixed_size_array_ty;
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    LibfuncSignature, OutputVarInfo, SierraApChange, SignatureSpecializationContext,
};
use crate::extensions::{
    NamedType, NoGenericArgsGenericLibfunc, NoGenericArgsGenericType, OutputVarReferenceInfo,
    SpecializationError,
};
use crate::ids::GenericTypeId;

/// Type for the state of the Blake2s.
#[derive(Default)]
pub struct Blake2sState {}
impl NoGenericArgsGenericType for Blake2sState {
    const ID: GenericTypeId = GenericTypeId::new_inline("Blake2sState");
    const STORABLE: bool = true;
    const DUPLICATABLE: bool = true;
    const DROPPABLE: bool = true;
    const ZERO_SIZED: bool = false;
}

define_libfunc_hierarchy! {
    pub enum BlakeLibfunc {
        Blake2sCompress(Blake2sCompressLibFunc),
        Blake2sFinalize(Blake2sFinalizeLibFunc),
    }, BlakeConcreteLibfunc
}

/// Libfunc for the Blake2s compress function.
#[derive(Default)]
pub struct Blake2sCompressLibFunc {}
impl NoGenericArgsGenericLibfunc for Blake2sCompressLibFunc {
    const STR_ID: &'static str = "blake2s_compress";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let u32_ty = context.get_concrete_type(Uint32Type::id(), &[])?;
        let state = box_ty(context, fixed_size_array_ty(context, u32_ty.clone(), 8)?)?;
        let u32_ty = context.get_concrete_type(Uint32Type::id(), &[])?;
        let msg_ty = box_ty(context, fixed_size_array_ty(context, u32_ty.clone(), 16)?)?;
        Ok(LibfuncSignature::new_non_branch(
            vec![state.clone(), u32_ty, msg_ty],
            vec![OutputVarInfo {
                ty: state,
                ref_info: OutputVarReferenceInfo::NewTempVar { idx: 0 },
            }],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}

/// Libfunc for the Blake2s finalize function.
#[derive(Default)]
pub struct Blake2sFinalizeLibFunc {}
impl NoGenericArgsGenericLibfunc for Blake2sFinalizeLibFunc {
    const STR_ID: &'static str = "blake2s_finalize";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let u32_ty = context.get_concrete_type(Uint32Type::id(), &[])?;
        let state = box_ty(context, fixed_size_array_ty(context, u32_ty.clone(), 8)?)?;
        let u32_ty = context.get_concrete_type(Uint32Type::id(), &[])?;
        let msg_ty = box_ty(context, fixed_size_array_ty(context, u32_ty.clone(), 16)?)?;
        Ok(LibfuncSignature::new_non_branch(
            vec![state.clone(), u32_ty, msg_ty],
            vec![OutputVarInfo {
                ty: state,
                ref_info: OutputVarReferenceInfo::NewTempVar { idx: 0 },
            }],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}
