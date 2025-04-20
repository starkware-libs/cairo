use super::consts::SignatureAndConstConcreteLibfunc;
use crate::extensions::lib_func::{
    LibfuncSignature, SierraApChange, SignatureSpecializationContext,
};
use crate::extensions::{NamedLibfunc, SpecializationError, args_as_single_value};
use crate::program::GenericArg;

/// Libfunc for causing a trace hint in the hint processor.
#[derive(Default)]
pub struct TraceLibfunc {}
impl NamedLibfunc for TraceLibfunc {
    const STR_ID: &'static str = "trace";

    type Concrete = SignatureAndConstConcreteLibfunc;

    fn specialize_signature(
        &self,
        _context: &dyn SignatureSpecializationContext,
        generic_args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        let _flag = args_as_single_value(generic_args)?;
        Ok(LibfuncSignature::new_non_branch_ex(
            vec![],
            vec![],
            SierraApChange::Known { new_vars_only: false },
        ))
    }

    fn specialize(
        &self,
        context: &dyn crate::extensions::lib_func::SpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        let flag = args_as_single_value(args)?;
        Ok(Self::Concrete { signature: self.specialize_signature(context, args)?, c: flag })
    }
}
