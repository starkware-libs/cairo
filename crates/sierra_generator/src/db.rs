use std::sync::Arc;

use db_utils::Upcast;
use defs::ids::{FreeFunctionId, ModuleId};
use diagnostics::Diagnostics;
use semantic::db::SemanticGroup;

use crate::program_generator::{self};
use crate::{function_generator, pre_sierra, SierraGeneratorDiagnostic};

#[salsa::query_group(SierraGenDatabase)]
pub trait SierraGenGroup: SemanticGroup + Upcast<dyn SemanticGroup> {
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

    /// Returns the [sierra::program::FunctionSignature] object for the given function id.
    fn get_function_signature(
        &self,
        function_id: sierra::ids::FunctionId,
    ) -> Option<Arc<sierra::program::FunctionSignature>>;

    /// Private query to compute Sierra data about a free function.
    #[salsa::invoke(function_generator::priv_free_function_sierra_data)]
    fn priv_free_function_sierra_data(
        &self,
        function_id: FreeFunctionId,
    ) -> function_generator::SierraFreeFunctionData;
    /// Returns the Sierra diagnostics of a free function.
    #[salsa::invoke(function_generator::free_function_sierra_diagnostics)]
    fn free_function_sierra_diagnostics(
        &self,
        function_id: FreeFunctionId,
    ) -> Diagnostics<SierraGeneratorDiagnostic>;
    /// Returns the Sierra code (as [pre_sierra::Function]) for a given free function.
    #[salsa::invoke(function_generator::free_function_sierra)]
    fn free_function_sierra(
        &self,
        function_id: FreeFunctionId,
    ) -> Option<Arc<pre_sierra::Function>>;

    // TODO(spapini): A program is made of a crate, not a module.
    /// Returns the [sierra::program::Program] object for the given module.
    #[salsa::invoke(program_generator::module_sierra_program)]
    fn module_sierra_program(&self, module_id: ModuleId) -> Option<Arc<sierra::program::Program>>;
    /// Returns the Sierra diagnostics of a module.
    #[salsa::invoke(program_generator::module_sierra_diagnostics)]
    fn module_sierra_diagnostics(
        &self,
        module_id: ModuleId,
    ) -> Diagnostics<SierraGeneratorDiagnostic>;
}

fn get_function_signature(
    db: &dyn SierraGenGroup,
    function_id: sierra::ids::FunctionId,
) -> Option<Arc<sierra::program::FunctionSignature>> {
    let semantic_function_id = db.lookup_intern_sierra_function(function_id);
    let signature = db.concrete_function_signature(semantic_function_id)?;
    let mut param_types = Vec::new();
    for param in signature.params {
        param_types.push(db.get_concrete_type_id(param.ty)?);
    }
    let ret_types = vec![db.get_concrete_type_id(signature.return_type)?];

    Some(Arc::new(sierra::program::FunctionSignature { param_types, ret_types }))
}
