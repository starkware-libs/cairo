use std::sync::Arc;

use defs::ids::{FreeFunctionId, ModuleId};
use diagnostics::{Diagnostics, WithDiagnostics};
use diagnostics_proc_macros::with_diagnostics;
use semantic::db::{AsSemanticGroup, SemanticGroup};

use crate::diagnostic::Diagnostic;
use crate::pre_sierra;
use crate::program_generator::generate_program_code;

#[salsa::query_group(SierraGenDatabase)]
pub trait SierraGenGroup: SemanticGroup + AsSemanticGroup {
    #[salsa::interned]
    fn intern_label_id(&self, id: pre_sierra::LabelLongId) -> pre_sierra::LabelId;

    #[salsa::interned]
    fn intern_concrete_lib_func(
        &self,
        id: sierra::program::ConcreteLibFuncLongId,
    ) -> sierra::ids::ConcreteLibFuncId;

    #[salsa::interned]
    fn intern_concrete_type(
        &self,
        id: sierra::program::ConcreteTypeLongId,
    ) -> sierra::ids::ConcreteTypeId;

    /// Creates a Sierra function id for a function id of the semantic model.
    // TODO(lior): Can we have the short and long ids in the same place? Currently, the short
    //   id is defined in sierra and the long id is defined in semantic.
    #[salsa::interned]
    fn intern_sierra_function(&self, id: semantic::FunctionId) -> sierra::ids::FunctionId;

    /// Returns the matching sierra concrete type id for a given semantic type id.
    #[salsa::invoke(crate::types::get_concrete_type_id)]
    fn get_concrete_type_id(
        &self,
        type_id: semantic::TypeId,
    ) -> Option<sierra::ids::ConcreteTypeId>;

    /// Generates and returns the Sierra code (as [pre_sierra::Function]) for a given function.
    #[salsa::invoke(crate::function_generator::get_function_code)]
    fn get_function_code(
        &self,
        function_id: FreeFunctionId,
    ) -> WithDiagnostics<Option<Arc<pre_sierra::Function>>, Diagnostic>;

    /// Generates and returns the [sierra::program::Program] object for the given module.
    fn get_program_code(
        &self,
        module_id: ModuleId,
    ) -> WithDiagnostics<Option<Arc<sierra::program::Program>>, Diagnostic>;
}

#[with_diagnostics]
fn get_program_code(
    diagnostics: &mut Diagnostics<Diagnostic>,
    db: &dyn SierraGenGroup,
    module_id: ModuleId,
) -> Option<Arc<sierra::program::Program>> {
    let module_items = db.module_items(module_id)?;
    let program: sierra::program::Program = generate_program_code(diagnostics, db, &module_items)?;
    Some(Arc::new(program))
}
