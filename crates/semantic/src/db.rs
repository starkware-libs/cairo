use defs::db::{AsDefsGroup, DefsGroup};
use defs::ids::{FreeFunctionId, GenericFunctionId, ModuleId, ModuleItemId, StructId};
use diagnostics::{Diagnostics, WithDiagnostics};
use filesystem::db::AsFilesGroup;
use parser::db::ParserGroup;

use crate::{corelib, expr, items, semantic, FunctionId, FunctionLongId, SemanticDiagnostic};

// Salsa database interface.
// All queries starting with priv_ are for internal use only by this crate.
// They appear in the public API because of salsa limitations.
#[salsa::query_group(SemanticDatabase)]
pub trait SemanticGroup: DefsGroup + AsDefsGroup + ParserGroup + AsFilesGroup {
    #[salsa::interned]
    fn intern_function(&self, id: FunctionLongId) -> FunctionId;
    #[salsa::interned]
    fn intern_type(&self, id: semantic::TypeLongId) -> semantic::TypeId;
    #[salsa::interned]
    fn intern_expr(&self, expr: semantic::Expr) -> semantic::ExprId;
    #[salsa::interned]
    fn intern_statement(&self, statement: semantic::Statement) -> semantic::StatementId;

    // Queries to compute the semantic model for definitions.
    #[salsa::invoke(items::strct::struct_semantic)]
    fn struct_semantic(&self, item: StructId) -> semantic::Struct;
    #[salsa::invoke(expr::expr_semantic)]
    fn expr_semantic(&self, item: semantic::ExprId) -> semantic::Expr;
    #[salsa::invoke(expr::statement_semantic)]
    fn statement_semantic(&self, item: semantic::StatementId) -> semantic::Statement;

    /// Should only be used internally.
    /// Computes semantic data about a signature of a generic function.
    #[salsa::invoke(items::functions::priv_generic_function_signature_data)]
    fn priv_generic_function_signature_data(
        &self,
        function_id: GenericFunctionId,
    ) -> WithDiagnostics<Option<items::functions::GenericFunctionData>, SemanticDiagnostic>;

    /// Returns the semantic signature of a function given the function_id.
    #[salsa::invoke(items::functions::generic_function_signature_semantic)]
    fn generic_function_signature_semantic(
        &self,
        function_id: GenericFunctionId,
    ) -> Option<semantic::Signature>;

    /// Returns the semantic function given the function_id.
    #[salsa::invoke(items::free_function::priv_free_function_semantic)]
    fn priv_free_function_semantic(
        &self,
        function_id: FreeFunctionId,
    ) -> WithDiagnostics<Option<semantic::FreeFunction>, SemanticDiagnostic>;
    /// Returns the semantic function given the function_id.
    #[salsa::invoke(items::free_function::free_function_semantic)]
    fn free_function_semantic(&self, function_id: FreeFunctionId)
    -> Option<semantic::FreeFunction>;

    // Aggregates module level semantic diagnostics.
    // TODO(spapini): use Arcs to Vec of Arcs of diagnostics.
    fn module_semantic_diagnostics(
        &self,
        module_id: ModuleId,
    ) -> Option<Diagnostics<SemanticDiagnostic>>;

    // Corelib.
    #[salsa::invoke(corelib::core_module)]
    fn core_module(&self) -> ModuleId;
    #[salsa::invoke(corelib::core_felt_ty)]
    fn core_felt_ty(&self) -> semantic::TypeId;
}

pub trait AsSemanticGroup {
    fn as_semantic_group(&self) -> &(dyn SemanticGroup + 'static);
}

impl AsSemanticGroup for dyn SemanticGroup {
    fn as_semantic_group(&self) -> &(dyn SemanticGroup + 'static) {
        self
    }
}

// ----------------------- Queries -----------------------

// TODO(spapini): Implement this more efficiently, with Arcs where needed, and not clones.
#[allow(clippy::single_match)]
fn module_semantic_diagnostics(
    db: &dyn SemanticGroup,
    module_id: ModuleId,
) -> Option<Diagnostics<SemanticDiagnostic>> {
    let mut diagnostics = Diagnostics::new();
    for (_name, item) in db.module_items(module_id)?.items.iter() {
        match item {
            // Add signature diagnostics.
            ModuleItemId::FreeFunction(free_function) => {
                diagnostics.0.extend(
                    db.priv_generic_function_signature_data(GenericFunctionId::Free(
                        *free_function,
                    ))
                    .get_diagnostics()
                    .0
                    .clone(),
                );
                // Add body diagnostics.
                diagnostics.0.extend(
                    db.priv_free_function_semantic(*free_function).get_diagnostics().0.clone(),
                );
            }
            ModuleItemId::Submodule(_) => {}
            ModuleItemId::Use(_) => {}
            ModuleItemId::Struct(_) => {}
            ModuleItemId::ExternType(_) => {}
            ModuleItemId::ExternFunction(_) => {}
        }
    }
    Some(diagnostics)
}
