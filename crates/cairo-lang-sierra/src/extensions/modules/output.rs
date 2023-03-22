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

/// Type representing the Output builtin.
#[derive(Default)]
pub struct OutputBuiltinType {}
impl NoGenericArgsGenericType for OutputBuiltinType {
    const ID: GenericTypeId = GenericTypeId::new_inline("OutputBuiltin");
    const STORABLE: bool = true;
    const DUPLICATABLE: bool = false;
    const DROPPABLE: bool = false;
    const SIZE: i16 = 1;
}

define_libfunc_hierarchy! {
    pub enum OutputLibfunc {
        OutputFelt(OutputFeltLibfunc),
    }, OutputConcreteLibfunc
}

/// Libfunc for outputing a felt252.
/// Returns the updated builtin pointer.
#[derive(Default)]
pub struct OutputFeltLibfunc {}
impl NoGenericArgsGenericLibfunc for OutputFeltLibfunc {
    const STR_ID: &'static str = "output_felt252";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let output_builtin_ty = context.get_concrete_type(OutputBuiltinType::id(), &[])?;
        let felt252_ty = context.get_concrete_type(Felt252Type::id(), &[])?;
        Ok(LibfuncSignature::new_non_branch_ex(
            vec![
                ParamSignature {
                    ty: output_builtin_ty.clone(),
                    allow_deferred: false,
                    allow_add_const: true,
                    allow_const: false,
                },
                ParamSignature::new(felt252_ty),
            ],
            vec![OutputVarInfo {
                ty: output_builtin_ty,
                ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::AddConst {
                    param_idx: 0,
                }),
            }],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}
