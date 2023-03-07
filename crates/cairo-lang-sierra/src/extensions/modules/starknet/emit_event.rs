use super::syscalls::SyscallGenericLibfunc;
use crate::extensions::array::ArrayType;
use crate::extensions::felt::FeltType;
use crate::extensions::lib_func::SignatureSpecializationContext;
use crate::extensions::{NamedType, SpecializationError};

/// Libfunc for an emit event system call.
#[derive(Default)]
pub struct EmitEventLibfunc {}
impl SyscallGenericLibfunc for EmitEventLibfunc {
    const STR_ID: &'static str = "emit_event_syscall";

    fn input_tys(
        context: &dyn SignatureSpecializationContext,
    ) -> Result<Vec<crate::ids::ConcreteTypeId>, SpecializationError> {
        let arr_ty = context.get_wrapped_concrete_type(
            ArrayType::id(),
            context.get_concrete_type(FeltType::id(), &[])?,
        )?;
        Ok(vec![
            // keys
            arr_ty.clone(),
            // data
            arr_ty,
        ])
    }

    fn success_output_tys(
        _context: &dyn SignatureSpecializationContext,
    ) -> Result<Vec<crate::ids::ConcreteTypeId>, SpecializationError> {
        Ok(vec![])
    }
}
