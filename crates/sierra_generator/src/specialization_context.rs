use sierra::extensions::lib_func::SignatureSpecializationContext;
use sierra::program::ConcreteTypeLongId;

use crate::db::SierraGenGroup;

/// A wrapper over the [SierraGenGroup] salsa database, that provides the
/// [SignatureSpecializationContext] functionality.
/// In particular, it can be used when calling
/// [specialize_signature_by_id](sierra::extensions::lib_func::GenericLibFuncEx::specialize_signature_by_id).
pub struct SierraSignatureSpecializationContext<'a>(pub &'a dyn SierraGenGroup);

impl SignatureSpecializationContext for SierraSignatureSpecializationContext<'_> {
    fn get_concrete_type(
        &self,
        id: sierra::ids::GenericTypeId,
        generic_args: &[sierra::program::GenericArg],
    ) -> Option<sierra::ids::ConcreteTypeId> {
        Some(self.0.intern_concrete_type(ConcreteTypeLongId {
            generic_id: id,
            generic_args: generic_args.to_vec(),
        }))
    }

    fn get_function_signature(
        &self,
        function_id: &sierra::ids::FunctionId,
    ) -> Option<sierra::program::FunctionSignature> {
        self.0.get_function_signature(function_id.clone()).map(|signature| (*signature).clone())
    }
}
