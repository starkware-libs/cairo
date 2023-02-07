use cairo_lang_diagnostics::ToOption;
use cairo_lang_sierra::extensions::lib_func::{SierraApChange, SignatureSpecializationContext};
use cairo_lang_sierra::extensions::type_specialization_context::TypeSpecializationContext;
use cairo_lang_sierra::program::ConcreteTypeLongId;

use crate::db::SierraGenGroup;

/// A wrapper over the [SierraGenGroup] salsa database, that provides the
/// [SignatureSpecializationContext] functionality.
/// In particular, it can be used when calling
/// [specialize_signature_by_id](cairo_lang_sierra::extensions::lib_func::GenericLibfuncEx::specialize_signature_by_id).
pub struct SierraSignatureSpecializationContext<'a>(pub &'a dyn SierraGenGroup);

impl TypeSpecializationContext for SierraSignatureSpecializationContext<'_> {
    fn try_get_type_info(
        &self,
        id: cairo_lang_sierra::ids::ConcreteTypeId,
    ) -> Option<cairo_lang_sierra::extensions::types::TypeInfo> {
        self.0.get_type_info(id).map(|info| (*info).clone()).to_option()
    }
}
impl SignatureSpecializationContext for SierraSignatureSpecializationContext<'_> {
    fn try_get_concrete_type(
        &self,
        id: cairo_lang_sierra::ids::GenericTypeId,
        generic_args: &[cairo_lang_sierra::program::GenericArg],
    ) -> Option<cairo_lang_sierra::ids::ConcreteTypeId> {
        Some(self.0.intern_concrete_type(ConcreteTypeLongId {
            generic_id: id,
            generic_args: generic_args.to_vec(),
        }))
    }

    fn try_get_function_signature(
        &self,
        function_id: &cairo_lang_sierra::ids::FunctionId,
    ) -> Option<cairo_lang_sierra::program::FunctionSignature> {
        self.0
            .get_function_signature(function_id.clone())
            .map(|signature| (*signature).clone())
            .to_option()
    }

    fn as_type_specialization_context(&self) -> &dyn TypeSpecializationContext {
        self
    }

    fn try_get_function_ap_change(
        &self,
        function_id: &cairo_lang_sierra::ids::FunctionId,
    ) -> Option<SierraApChange> {
        let concrete_function = self
            .0
            .lookup_intern_function(self.0.lookup_intern_sierra_function(function_id.clone()))
            .function;
        let function = concrete_function.get_body(self.0.upcast()).expect(
            "Internal compiler error: get_function_ap_change() should only be used for user \
             defined functions.",
        );
        self.0.get_ap_change(function).to_option()
    }
}
