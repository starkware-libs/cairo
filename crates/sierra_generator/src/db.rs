use std::sync::Arc;

use defs::ids::FreeFunctionId;
use diagnostics::{Diagnostics, WithDiagnostics};
use diagnostics_proc_macros::with_diagnostics;
use parser::parser::ParserDiagnostic;
use semantic::db::SemanticGroup;

use crate::function_generator::generate_function_code;
use crate::pre_sierra;

#[salsa::query_group(SierraGenDatabase)]
pub trait SierraGenGroup: SemanticGroup {
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
    fn intern_function(&self, id: semantic::ConcreteFunctionId) -> sierra::ids::FunctionId;

    /// Generates and returns the Sierra code (as [pre_sierra::Function]) for a given function.
    fn get_function_code(
        &self,
        function_id: FreeFunctionId,
    ) -> WithDiagnostics<Option<Arc<pre_sierra::Function>>, ParserDiagnostic>;
}

#[with_diagnostics]
fn get_function_code(
    diagnostics: &mut Diagnostics<ParserDiagnostic>,
    db: &dyn SierraGenGroup,
    function_id: FreeFunctionId,
) -> Option<Arc<pre_sierra::Function>> {
    let function_semantic: semantic::FreeFunction =
        db.free_function_semantic(function_id).unwrap(diagnostics)?;
    Some(Arc::new(generate_function_code(db, function_id, function_semantic)))
}
