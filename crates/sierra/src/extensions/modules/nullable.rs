use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    DeferredOutputKind, LibFuncSignature, OutputVarInfo, SierraApChange,
    SignatureOnlyGenericLibFunc, SignatureSpecializationContext,
};
use crate::extensions::type_specialization_context::TypeSpecializationContext;
use crate::extensions::types::TypeInfo;
use crate::extensions::{
    args_as_single_type, ConcreteType, NamedType, OutputVarReferenceInfo, SpecializationError,
};
use crate::ids::{ConcreteTypeId, GenericLibFuncId, GenericTypeId};
use crate::program::GenericArg;

/// A type that holds a possibly-null pointer to an object.
///
/// It behaves exactly like `Option<Box<T>>`, except that it only uses 1 memory cell (rather than 2
/// in `Option<Box<T>>`) - the value is 0 if and only if there is no object.
///
/// This type uses the fact that Casm pointers can never be zero.
#[derive(Default)]
pub struct NullableType {}
impl NamedType for NullableType {
    type Concrete = NullableConcreteType;
    const ID: GenericTypeId = GenericTypeId::new_inline("Nullable");

    fn specialize(
        &self,
        context: &dyn TypeSpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        let ty = args_as_single_type(args)?;
        Ok(NullableConcreteType { info: context.get_type_info(ty.clone())?, ty })
    }
}

pub struct NullableConcreteType {
    pub info: TypeInfo,
    pub ty: ConcreteTypeId,
}
impl ConcreteType for NullableConcreteType {
    fn info(&self) -> &TypeInfo {
        &self.info
    }
}

define_libfunc_hierarchy! {
    pub enum NullableLibFunc {
        Null(NullLibFunc),
    }, NullableConcreteLibFunc
}

/// LibFunc for creating a null object of type Nullable<T>.
#[derive(Default)]
pub struct NullLibFunc {}
impl SignatureOnlyGenericLibFunc for NullLibFunc {
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("null");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibFuncSignature, SpecializationError> {
        let ty = args_as_single_type(args)?;
        Ok(LibFuncSignature::new_non_branch(
            vec![],
            vec![OutputVarInfo {
                ty: context.get_wrapped_concrete_type(NullableType::id(), ty)?,
                ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
            }],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}
