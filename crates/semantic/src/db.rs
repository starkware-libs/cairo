use db_utils::Upcast;
use defs::db::DefsGroup;
use defs::ids::{
    EnumId, ExternFunctionId, ExternTypeId, FreeFunctionId, GenericFunctionId, GenericParamId,
    GenericTypeId, ModuleId, ModuleItemId, StructId, UseId,
};
use diagnostics::{Diagnostics, DiagnosticsBuilder};
use filesystem::db::{AsFilesGroupMut, FilesGroup};
use filesystem::ids::FileId;
use parser::db::ParserGroup;
use smol_str::SmolStr;
use utils::ordered_hash_map::OrderedHashMap;

use crate::resolve_path::ResolvedGenericItem;
use crate::{corelib, items, semantic, types, FunctionId, SemanticDiagnostic};

// Salsa database interface.
// All queries starting with priv_ are for internal use only by this crate.
// They appear in the public API because of salsa limitations.
// We differentiate between the declaration and the definition of each item:
// Declarations and definitions must not depend on other definitions, only other declarations.
// This prevents cycles where there shouldn't be any.
#[salsa::query_group(SemanticDatabase)]
pub trait SemanticGroup:
    DefsGroup + Upcast<dyn DefsGroup> + ParserGroup + Upcast<dyn FilesGroup> + AsFilesGroupMut
{
    #[salsa::interned]
    fn intern_function(&self, id: items::functions::FunctionLongId) -> semantic::FunctionId;
    #[salsa::interned]
    fn intern_concrete_struct(&self, id: types::ConcreteStructLongId) -> types::ConcreteStructId;
    #[salsa::interned]
    fn intern_concrete_enum(&self, id: types::ConcreteEnumLongId) -> types::ConcreteEnumId;
    #[salsa::interned]
    fn intern_concrete_extern_type(
        &self,
        id: types::ConcreteExternTypeLongId,
    ) -> types::ConcreteExternTypeId;
    #[salsa::interned]
    fn intern_type(&self, id: types::TypeLongId) -> semantic::TypeId;

    // Use.
    // ====
    /// Private query to compute data about a use.
    #[salsa::invoke(items::us::priv_use_semantic_data)]
    #[salsa::cycle(items::us::priv_use_semantic_data_cycle)]
    fn priv_use_semantic_data(&self, use_id: UseId) -> Option<items::us::UseData>;
    /// Returns the semantic diagnostics of a use.
    #[salsa::invoke(items::us::use_semantic_diagnostics)]
    fn use_semantic_diagnostics(&self, use_id: UseId) -> Diagnostics<SemanticDiagnostic>;
    /// Returns the semantic diagnostics of a use.
    #[salsa::invoke(items::us::use_resolved_item)]
    fn use_resolved_item(&self, use_id: UseId) -> Option<ResolvedGenericItem>;

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

    // Enum.
    // =======
    /// Private query to compute data about an enum.
    #[salsa::invoke(items::enm::priv_enum_semantic_data)]
    fn priv_enum_semantic_data(&self, enum_id: EnumId) -> Option<items::enm::EnumData>;
    /// Returns the semantic diagnostics of an enum.
    #[salsa::invoke(items::enm::enum_semantic_diagnostics)]
    fn enum_semantic_diagnostics(&self, enum_id: EnumId) -> Diagnostics<SemanticDiagnostic>;
    /// Returns the members of an enum.
    #[salsa::invoke(items::enm::enum_variants)]
    fn enum_variants(&self, enum_id: EnumId) -> Option<OrderedHashMap<SmolStr, semantic::Variant>>;

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
    /// Returns the signature of a free function declaration.
    #[salsa::invoke(items::free_function::free_function_declaration_signature)]
    fn free_function_declaration_signature(
        &self,
        free_function_id: FreeFunctionId,
    ) -> Option<semantic::Signature>;
    /// Returns the generic params of a free function declaration.
    #[salsa::invoke(items::free_function::free_function_declaration_generic_params)]
    fn free_function_declaration_generic_params(
        &self,
        free_function_id: FreeFunctionId,
    ) -> Option<Vec<GenericParamId>>;

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
    /// Returns the body of a free function definition.
    #[salsa::invoke(items::free_function::free_function_definition_body)]
    fn free_function_definition_body(
        &self,
        free_function_id: FreeFunctionId,
    ) -> Option<semantic::ExprId>;

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
    /// Returns the generic params of an extern function.
    #[salsa::invoke(items::extern_function::extern_function_declaration_generic_params)]
    fn extern_function_declaration_generic_params(
        &self,
        extern_function_id: ExternFunctionId,
    ) -> Option<Vec<GenericParamId>>;

    // Extern type.
    // ============
    /// Private query to compute data about an extern type declaration. An extern type has
    /// no body, and thus only has a declaration.
    #[salsa::invoke(items::extern_type::priv_extern_type_declaration_data)]
    fn priv_extern_type_declaration_data(
        &self,
        type_id: ExternTypeId,
    ) -> Option<items::extern_type::ExternTypeDeclarationData>;
    /// Returns the semantic diagnostics of an extern type declaration. An extern type has
    /// no body, and thus only has a declaration.
    #[salsa::invoke(items::extern_type::extern_type_declaration_diagnostics)]
    fn extern_type_declaration_diagnostics(
        &self,
        extern_type_id: ExternTypeId,
    ) -> Diagnostics<SemanticDiagnostic>;
    /// Returns the generic params of an extern type.
    #[salsa::invoke(items::extern_type::extern_type_declaration_generic_params)]
    fn extern_type_declaration_generic_params(
        &self,
        extern_type_id: ExternTypeId,
    ) -> Option<Vec<GenericParamId>>;

    // Generic function.
    // =================
    /// Returns the signature of a generic function. This include free functions, extern functions,
    /// etc...
    #[salsa::invoke(items::functions::generic_function_signature)]
    fn generic_function_signature(
        &self,
        generic_function: GenericFunctionId,
    ) -> Option<semantic::Signature>;

    /// Returns the signature of a generic function. This include free functions, extern functions,
    /// etc...
    #[salsa::invoke(items::functions::generic_function_generic_params)]
    fn generic_function_generic_params(
        &self,
        generic_function: GenericFunctionId,
    ) -> Option<Vec<GenericParamId>>;

    /// Returns the signature of a concrete function. This include free functions, extern functions,
    /// etc...
    #[salsa::invoke(items::functions::concrete_function_signature)]
    fn concrete_function_signature(&self, function_id: FunctionId) -> Option<semantic::Signature>;

    // Generic type.
    // =================
    /// Returns the generic_type of a generic function. This include free types, extern
    /// types, etc...
    #[salsa::invoke(types::generic_type_generic_params)]
    fn generic_type_generic_params(
        &self,
        generic_type: GenericTypeId,
    ) -> Option<Vec<GenericParamId>>;

    // Expression.
    // ===========
    #[salsa::invoke(items::free_function::expr_semantic)]
    fn expr_semantic(
        &self,
        free_function_id: FreeFunctionId,
        id: semantic::ExprId,
    ) -> semantic::Expr;
    #[salsa::invoke(items::free_function::statement_semantic)]
    fn statement_semantic(
        &self,
        free_function_id: FreeFunctionId,
        id: semantic::StatementId,
    ) -> semantic::Statement;

    // Aggregates module level semantic diagnostics.
    fn module_semantic_diagnostics(
        &self,
        module_id: ModuleId,
    ) -> Option<Diagnostics<SemanticDiagnostic>>;

    // Aggregates file level semantic diagnostics.
    fn file_semantic_diagnostics(&self, file_id: FileId)
    -> Option<Diagnostics<SemanticDiagnostic>>;

    // Corelib.
    #[salsa::invoke(corelib::core_module)]
    fn core_module(&self) -> ModuleId;
    #[salsa::invoke(corelib::core_felt_ty)]
    fn core_felt_ty(&self) -> semantic::TypeId;
}

fn module_semantic_diagnostics(
    db: &dyn SemanticGroup,
    module_id: ModuleId,
) -> Option<Diagnostics<SemanticDiagnostic>> {
    let mut diagnostics = DiagnosticsBuilder::default();
    for (_name, item) in db.module_items(module_id)?.items.iter() {
        match item {
            // Add signature diagnostics.
            ModuleItemId::Use(use_id) => {
                diagnostics.extend(db.use_semantic_diagnostics(*use_id));
            }
            ModuleItemId::FreeFunction(free_function) => {
                diagnostics.extend(db.free_function_declaration_diagnostics(*free_function));
                diagnostics.extend(db.free_function_definition_diagnostics(*free_function));
            }
            ModuleItemId::Struct(struct_id) => {
                diagnostics.extend(db.struct_semantic_diagnostics(*struct_id));
            }
            ModuleItemId::Enum(enum_id) => {
                diagnostics.extend(db.enum_semantic_diagnostics(*enum_id));
            }
            ModuleItemId::Submodule(_) => {}
            ModuleItemId::ExternType(_) => {}
            ModuleItemId::ExternFunction(_) => {}
        }
    }
    Some(diagnostics.build())
}

fn file_semantic_diagnostics(
    db: &dyn SemanticGroup,
    file_id: FileId,
) -> Option<Diagnostics<SemanticDiagnostic>> {
    let mut diagnostics = DiagnosticsBuilder::default();
    for module_id in db.file_modules(file_id)? {
        if let Some(module_diagnostics) = db.module_semantic_diagnostics(module_id) {
            diagnostics.extend(module_diagnostics)
        }
    }
    Some(diagnostics.build())
}
