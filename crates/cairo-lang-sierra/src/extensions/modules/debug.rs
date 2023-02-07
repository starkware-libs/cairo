use super::array::ArrayType;
use super::felt::FeltType;
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    LibfuncSignature, SierraApChange, SignatureSpecializationContext,
};
use crate::extensions::{NamedType, NoGenericArgsGenericLibfunc, SpecializationError};

define_libfunc_hierarchy! {
    pub enum DebugLibfunc {
        Print(PrintLibfunc),
    }, DebugConcreteLibfunc
}

/// Libfunc for debug printing.
#[derive(Default)]
pub struct PrintLibfunc {}
impl NoGenericArgsGenericLibfunc for PrintLibfunc {
    const STR_ID: &'static str = "print";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        // TODO(spapini): We should get a StringView, which is something like
        // (Span<StringLimb>, len), or something like that.
        let felt_ty = context.get_concrete_type(FeltType::id(), &[])?;
        let arr_type = context.get_wrapped_concrete_type(ArrayType::id(), felt_ty)?;
        Ok(LibfuncSignature::new_non_branch(
            vec![arr_type],
            vec![],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}
