use db_utils::Upcast;
use defs::db::DefsGroup;
use defs::ids::{
    ExternFunctionId, FreeFunctionId, GenericFunctionId, ModuleId, ModuleItemId, StructId,
};
use diagnostics::Diagnostics;
use filesystem::db::{AsFilesGroupMut, FilesGroup};
use parser::db::ParserGroup;
use smol_str::SmolStr;
use utils::ordered_hash_map::OrderedHashMap;

use crate::{corelib, expr, items, semantic, types, FunctionId, SemanticDiagnostic};

// Salsa database interface.
// All queries starting with priv_ are for internal use only by this crate.
// They appear in the public API because of salsa limitations.
#[salsa::query_group(SemanticDatabase)]
pub trait SemanticGroup:
    DefsGroup + Upcast<dyn DefsGroup> + ParserGroup + Upcast<dyn FilesGroup> + AsFilesGroupMut
{
    #[salsa::interned]
    fn intern_function(&self, id: items::functions::FunctionLongId) -> semantic::FunctionId;
    #[salsa::interned]
    fn intern_type(&self, id: types::TypeLongId) -> semantic::TypeId;
    #[salsa::interned]
    fn intern_expr(&self, expr: semantic::Expr) -> semantic::ExprId;
    #[salsa::interned]
    fn intern_statement(&self, statement: semantic::Statement) -> semantic::StatementId;

    // Struct.
    // =======
    /// Private query to compute data about a struct.
    #[salsa::invoke(items::strct::priv_struct_semantic_data)]
    fn priv_struct_semantic_data(&self, struct_id: StructId) -> Option<items::strct::StructData>;
    /// Returns the semantic diagnostics of a struct.
    #[salsa::invoke(items::strct::struct_semantic_diagnostics)]
    fn struct_semantic_diagnostics(&self, struct_id: StructId) -> Diagnostics<SemanticDiagnostic>;
    /// Returns the members of a struct.
    #[salsa::invoke(items::strct::struct_members)]
    fn struct_members(
        &self,
        struct_id: StructId,
    ) -> Option<OrderedHashMap<SmolStr, semantic::Member>>;

    // Free function.
    // ==============
    /// Private query to compute data about a free function declaration - its signature excluding
    /// its body.
    #[salsa::invoke(items::free_function::priv_free_function_declaration_data)]
    fn priv_free_function_declaration_data(
        &self,
        function_id: FreeFunctionId,
    ) -> Option<items::free_function::FreeFunctionDeclarationData>;
    /// Returns the semantic diagnostics of a function declaration - its signature excluding its
    /// body.
    #[salsa::invoke(items::free_function::free_function_declaration_diagnostics)]
    fn free_function_declaration_diagnostics(
        &self,
        free_function_id: FreeFunctionId,
    ) -> Diagnostics<SemanticDiagnostic>;
    /// Returns the signature of a free function.
    #[salsa::invoke(items::free_function::free_function_signature)]
    fn free_function_signature(
        &self,
        free_function_id: FreeFunctionId,
    ) -> Option<semantic::Signature>;

    /// Private query to compute data about a free function definition - its body.
    #[salsa::invoke(items::free_function::priv_free_function_definition_data)]
    fn priv_free_function_definition_data(
        &self,
        free_function_id: FreeFunctionId,
    ) -> Option<items::free_function::FreeFunctionDefinitionData>;
    /// Returns the semantic diagnostics of a function definition - its body.
    #[salsa::invoke(items::free_function::free_function_definition_diagnostics)]
    fn free_function_definition_diagnostics(
        &self,
        free_function_id: FreeFunctionId,
    ) -> Diagnostics<SemanticDiagnostic>;
    /// Returns the body of a free function.
    #[salsa::invoke(items::free_function::free_function_body)]
    fn free_function_body(&self, free_function_id: FreeFunctionId) -> Option<semantic::ExprId>;

    // Extern function.
    // ================
    /// Private query to compute data about an extern function declaration. An extern function has
    /// no body, and thus only has a declaration.
    #[salsa::invoke(items::extern_function::priv_extern_function_declaration_data)]
    fn priv_extern_function_declaration_data(
        &self,
        function_id: ExternFunctionId,
    ) -> Option<items::extern_function::ExternFunctionDeclarationData>;
    /// Returns the semantic diagnostics of an extern function declaration. An extern function has
    /// no body, and thus only has a declaration.
    #[salsa::invoke(items::extern_function::extern_function_declaration_diagnostics)]
    fn extern_function_declaration_diagnostics(
        &self,
        extern_function_id: ExternFunctionId,
    ) -> Diagnostics<SemanticDiagnostic>;
    /// Returns the signature of an extern function.
    #[salsa::invoke(items::extern_function::extern_function_declaration_signature)]
    fn extern_function_declaration_signature(
        &self,
        extern_function_id: ExternFunctionId,
    ) -> Option<semantic::Signature>;

    // Generic function.
    /// Returns the signature of a generic function. This include free functions, extern functions,
    /// etc...
    #[salsa::invoke(items::functions::generic_function_signature)]
    fn generic_function_signature(
        &self,
        generic_function: GenericFunctionId,
    ) -> Option<semantic::Signature>;

    /// Returns the signature of a concrete function. This include free functions, extern functions,
    /// etc...
    #[salsa::invoke(items::functions::concrete_function_signature)]
    fn concrete_function_signature(&self, function_id: FunctionId) -> Option<semantic::Signature>;

    // Expression.
    // ===========
    #[salsa::invoke(expr::expr_semantic)]
    fn expr_semantic(&self, item: semantic::ExprId) -> semantic::Expr;
    #[salsa::invoke(expr::statement_semantic)]
    fn statement_semantic(&self, item: semantic::StatementId) -> semantic::Statement;

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

// TODO(spapini): Implement this more efficiently, with Arcs where needed, and not clones.
#[allow(clippy::single_match)]
fn module_semantic_diagnostics(
    db: &dyn SemanticGroup,
    module_id: ModuleId,
) -> Option<Diagnostics<SemanticDiagnostic>> {
    let mut diagnostics = Diagnostics::default();
    for (_name, item) in db.module_items(module_id)?.items.iter() {
        match item {
            // Add signature diagnostics.
            ModuleItemId::FreeFunction(free_function) => {
                diagnostics
                    .0
                    .extend(db.free_function_declaration_diagnostics(*free_function).0.clone());
                diagnostics
                    .0
                    .extend(db.free_function_definition_diagnostics(*free_function).0.clone());
            }
            ModuleItemId::Struct(struct_id) => {
                diagnostics.0.extend(db.struct_semantic_diagnostics(*struct_id).0.clone());
            }
            ModuleItemId::Submodule(_) => {}
            ModuleItemId::Use(_) => {}
            ModuleItemId::ExternType(_) => {}
            ModuleItemId::ExternFunction(_) => {}
        }
    }
    Some(diagnostics)
}
