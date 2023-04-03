use super::felt252_span_ty;
use super::syscalls::SyscallGenericLibfunc;
use crate::extensions::lib_func::SignatureSpecializationContext;
use crate::extensions::SpecializationError;

/// Libfunc for an emit event system call.
#[derive(Default)]
pub struct EmitEventLibfunc {}
impl SyscallGenericLibfunc for EmitEventLibfunc {
    const STR_ID: &'static str = "emit_event_syscall";

    fn input_tys(
        context: &dyn SignatureSpecializationContext,
    ) -> Result<Vec<crate::ids::ConcreteTypeId>, SpecializationError> {
        let span_ty = felt252_span_ty(context)?;
        Ok(vec![
            // keys
            span_ty.clone(),
            // data
            span_ty,
        ])
    }

    fn success_output_tys(
        _context: &dyn SignatureSpecializationContext,
    ) -> Result<Vec<crate::ids::ConcreteTypeId>, SpecializationError> {
        Ok(vec![])
    }
}
