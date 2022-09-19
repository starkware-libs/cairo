use sierra::extensions::lib_func::SignatureSpecializationContext;
use sierra::extensions::SpecializationError;
use sierra::program::ConcreteTypeLongId;

use crate::db::SierraGenGroup;

pub struct SierraSignatureSpecializationContext<'a>(pub &'a dyn SierraGenGroup);

impl SignatureSpecializationContext for SierraSignatureSpecializationContext<'_> {
    fn get_concrete_type(
        &self,
        id: sierra::ids::GenericTypeId,
        generic_args: &[sierra::program::GenericArg],
    ) -> Result<sierra::ids::ConcreteTypeId, sierra::extensions::SpecializationError> {
        Ok(self.0.intern_concrete_type(ConcreteTypeLongId {
            generic_id: id,
            generic_args: generic_args.to_vec(),
        }))
    }

    fn get_function_signature(
        &self,
        function_id: &sierra::ids::FunctionId,
    ) -> Result<sierra::program::FunctionSignature, sierra::extensions::SpecializationError> {
        let signature_as_arc = self
            .0
            .get_function_signature(function_id.clone())
            .ok_or_else(|| SpecializationError::MissingFunction(function_id.clone()))?;
        Ok((*signature_as_arc).clone())
    }
}
