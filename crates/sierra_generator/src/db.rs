use std::sync::Arc;

use defs::ids::{FreeFunctionId, LanguageElementId, ModuleId};
use diagnostics::{Diagnostics, WithDiagnostics};
use diagnostics_proc_macros::with_diagnostics;
use semantic::db::SemanticGroup;
use sierra::program::ConcreteTypeLongId;

use crate::function_generator::generate_function_code;
use crate::pre_sierra;
use crate::program_generator::generate_program_code;

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
    fn intern_sierra_function(&self, id: semantic::FunctionId) -> sierra::ids::FunctionId;

    /// Returns the matching sierra concrete type id for a given semantic type id.
    fn get_concrete_type_id(
        &self,
        type_id: semantic::TypeId,
    ) -> WithDiagnostics<Option<sierra::ids::ConcreteTypeId>, semantic::Diagnostic>;

    /// Generates and returns the Sierra code (as [pre_sierra::Function]) for a given function.
    fn get_function_code(
        &self,
        function_id: FreeFunctionId,
    ) -> WithDiagnostics<Option<Arc<pre_sierra::Function>>, semantic::Diagnostic>;

    /// Generates and returns the [sierra::program::Program] object for the given module.
    fn get_program_code(
        &self,
        module_id: ModuleId,
    ) -> WithDiagnostics<Option<Arc<sierra::program::Program>>, semantic::Diagnostic>;
}

#[with_diagnostics]
fn get_concrete_type_id(
    diagnostics: &mut Diagnostics<semantic::Diagnostic>,
    db: &dyn SierraGenGroup,
    type_id: semantic::TypeId,
) -> Option<sierra::ids::ConcreteTypeId> {
    match db.lookup_intern_type(type_id) {
        semantic::TypeLongId::Concrete(ty) => {
            let mut generic_args = vec![];
            for arg in &ty.generic_args {
                match arg {
                    semantic::GenericArgumentId::Type(ty) => {
                        generic_args.push(sierra::program::GenericArg::Type(
                            db.get_concrete_type_id(*ty).propagte(diagnostics)?,
                        ));
                    }
                }
            }
            match ty.generic_type {
                defs::ids::GenericTypeId::Struct(_) => {
                    todo!("Add support for struct types when they are supported in Sierra.")
                }
                defs::ids::GenericTypeId::Extern(extrn) => {
                    Some(db.intern_concrete_type(ConcreteTypeLongId {
                        generic_id: sierra::ids::GenericTypeId::from_string(
                            extrn.name(db.as_defs_group()),
                        ),
                        args: generic_args,
                    }))
                }
            }
        }
        semantic::TypeLongId::Tuple(_) => {
            todo!("Add support for tuple types when they are supported in Sierra.")
        }
        semantic::TypeLongId::Missing => None,
    }
}

#[with_diagnostics]
fn get_function_code(
    diagnostics: &mut Diagnostics<semantic::Diagnostic>,
    db: &dyn SierraGenGroup,
    function_id: FreeFunctionId,
) -> Option<Arc<pre_sierra::Function>> {
    let function_semantic: semantic::FreeFunction =
        db.free_function_semantic(function_id).propagte(diagnostics)?;
    Some(Arc::new(generate_function_code(db, function_id, function_semantic)))
}

#[with_diagnostics]
fn get_program_code(
    diagnostics: &mut Diagnostics<semantic::Diagnostic>,
    db: &dyn SierraGenGroup,
    module_id: ModuleId,
) -> Option<Arc<sierra::program::Program>> {
    let module_items = db.module_items(module_id).propagte(diagnostics)?;
    let program: sierra::program::Program = generate_program_code(diagnostics, db, &module_items)?;
    Some(Arc::new(program))
}
