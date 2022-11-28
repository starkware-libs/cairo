use std::sync::Arc;

use db_utils::Upcast;
use defs::db::DefsGroup;
use defs::diagnostic_utils::StableLocation;
use defs::ids::{
    EnumId, ExternFunctionId, ExternTypeId, FreeFunctionId, GenericFunctionId, GenericParamId,
    GenericTypeId, ImplFunctionId, ImplId, LanguageElementId, LookupItemId, ModuleId, ModuleItemId,
    StructId, TraitFunctionId, TraitId, UseId, VariantId,
};
use diagnostics::{Diagnostics, DiagnosticsBuilder};
use filesystem::db::{AsFilesGroupMut, FilesGroup};
use filesystem::ids::FileId;
use parser::db::ParserGroup;
use smol_str::SmolStr;
use syntax::node::ast;
use utils::ordered_hash_map::OrderedHashMap;

use crate::diagnostic::SemanticDiagnosticKind;
use crate::items::attribute::Attribute;
use crate::items::imp::{ConcreteImplId, ImplLookupContext};
use crate::items::trt::ConcreteTraitId;
use crate::resolve_path::{ResolvedConcreteItem, ResolvedGenericItem, ResolvedLookback};
use crate::{
    corelib, items, semantic, types, FreeFunctionDefinition, FunctionId, SemanticDiagnostic, TypeId,
};

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
    fn intern_concrete_trait(
        &self,
        id: items::trt::ConcreteTraitLongId,
    ) -> items::trt::ConcreteTraitId;
    #[salsa::interned]
    fn intern_concrete_impl(
        &self,
        id: items::imp::ConcreteImplLongId,
    ) -> items::imp::ConcreteImplId;
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
    #[salsa::invoke(items::us::use_resolved_lookback)]
    fn use_resolved_lookback(&self, use_id: UseId) -> Option<Arc<ResolvedLookback>>;

    // Struct.
    // =======
    /// Private query to compute data about a struct.
    #[salsa::invoke(items::strct::priv_struct_semantic_data)]
    fn priv_struct_semantic_data(&self, struct_id: StructId) -> Option<items::strct::StructData>;
    /// Returns the semantic diagnostics of a struct.
    #[salsa::invoke(items::strct::struct_semantic_diagnostics)]
    fn struct_semantic_diagnostics(&self, struct_id: StructId) -> Diagnostics<SemanticDiagnostic>;
    /// Returns the generic parameters of an enum.
    #[salsa::invoke(items::strct::struct_generic_params)]
    fn struct_generic_params(&self, struct_id: StructId) -> Option<Vec<GenericParamId>>;
    /// Returns the members of a struct.
    #[salsa::invoke(items::strct::struct_members)]
    fn struct_members(
        &self,
        struct_id: StructId,
    ) -> Option<OrderedHashMap<SmolStr, semantic::Member>>;
    /// Returns the attributes of a struct.
    #[salsa::invoke(items::strct::struct_attributes)]
    fn struct_attributes(&self, struct_id: StructId) -> Option<Vec<Attribute>>;
    /// Returns the resolution lookback of a struct.
    #[salsa::invoke(items::strct::struct_resolved_lookback)]
    fn struct_resolved_lookback(&self, strct_id: StructId) -> Option<Arc<ResolvedLookback>>;

    // Enum.
    // =======
    /// Private query to compute data about an enum.
    #[salsa::invoke(items::enm::priv_enum_semantic_data)]
    fn priv_enum_semantic_data(&self, enum_id: EnumId) -> Option<items::enm::EnumData>;
    /// Returns the semantic diagnostics of an enum.
    #[salsa::invoke(items::enm::enum_semantic_diagnostics)]
    fn enum_semantic_diagnostics(&self, enum_id: EnumId) -> Diagnostics<SemanticDiagnostic>;
    /// Returns the generic parameters of an enum.
    #[salsa::invoke(items::enm::enum_generic_params)]
    fn enum_generic_params(&self, enum_id: EnumId) -> Option<Vec<GenericParamId>>;
    /// Returns the members of an enum.
    #[salsa::invoke(items::enm::enum_variants)]
    fn enum_variants(&self, enum_id: EnumId) -> Option<OrderedHashMap<SmolStr, VariantId>>;
    /// Returns the semantic model of a variant.
    #[salsa::invoke(items::enm::variant_semantic)]
    fn variant_semantic(&self, enum_id: EnumId, variant_id: VariantId)
    -> Option<semantic::Variant>;
    /// Returns the resolution lookback of an enum.
    #[salsa::invoke(items::enm::enum_resolved_lookback)]
    fn enum_resolved_lookback(&self, enum_id: EnumId) -> Option<Arc<ResolvedLookback>>;

    // Trait.
    // =======
    /// Private query to compute data about a trait.
    #[salsa::invoke(items::trt::priv_trait_semantic_data)]
    fn priv_trait_semantic_data(&self, trait_id: TraitId) -> Option<items::trt::TraitData>;
    /// Returns the semantic diagnostics of a trait.
    #[salsa::invoke(items::trt::trait_semantic_diagnostics)]
    fn trait_semantic_diagnostics(&self, trait_id: TraitId) -> Diagnostics<SemanticDiagnostic>;
    /// Returns the generic parameters of a trait.
    #[salsa::invoke(items::trt::trait_generic_params)]
    fn trait_generic_params(&self, trait_id: TraitId) -> Option<Vec<GenericParamId>>;
    /// Returns the attributes of a trait.
    #[salsa::invoke(items::trt::trait_attributes)]
    fn trait_attributes(&self, trait_id: TraitId) -> Option<Vec<Attribute>>;
    /// Returns the functions of a trait.
    #[salsa::invoke(items::trt::trait_functions)]
    fn trait_functions(
        &self,
        trait_id: TraitId,
    ) -> Option<OrderedHashMap<SmolStr, TraitFunctionId>>;

    // Trait function.
    // ================
    /// Private query to compute data about a trait function.
    #[salsa::invoke(items::trt::priv_trait_function_data)]
    fn priv_trait_function_data(
        &self,
        function_id: TraitFunctionId,
    ) -> Option<items::trt::TraitFunctionData>;
    /// Returns the semantic diagnostics of a trait function.
    #[salsa::invoke(items::trt::trait_function_diagnostics)]
    fn trait_function_diagnostics(
        &self,
        trait_function_id: TraitFunctionId,
    ) -> Diagnostics<SemanticDiagnostic>;
    /// Returns the signature of a trait function.
    #[salsa::invoke(items::trt::trait_function_signature)]
    fn trait_function_signature(
        &self,
        trait_function_id: TraitFunctionId,
    ) -> Option<semantic::Signature>;
    /// Returns the generic params of a trait function.
    #[salsa::invoke(items::trt::trait_function_generic_params)]
    fn trait_function_generic_params(
        &self,
        trait_function_id: TraitFunctionId,
    ) -> Option<Vec<GenericParamId>>;
    /// Returns the resolution lookback of a trait function.
    #[salsa::invoke(items::trt::trait_function_resolved_lookback)]
    fn trait_function_resolved_lookback(
        &self,
        trait_function_id: TraitFunctionId,
    ) -> Option<Arc<ResolvedLookback>>;

    // Impl.
    // =======
    /// Private query to compute declaration data about an impl.
    #[salsa::invoke(items::imp::priv_impl_declaration_data)]
    fn priv_impl_declaration_data(
        &self,
        impl_id: ImplId,
    ) -> Option<items::imp::ImplDeclarationData>;
    /// Returns the semantic declaration diagnostics of an impl.
    #[salsa::invoke(items::imp::impl_semantic_declaration_diagnostics)]
    fn impl_semantic_declaration_diagnostics(
        &self,
        impl_id: ImplId,
    ) -> Diagnostics<SemanticDiagnostic>;
    /// Returns the generic parameters of an impl.
    #[salsa::invoke(items::imp::impl_generic_params)]
    fn impl_generic_params(&self, impl_id: ImplId) -> Option<Vec<GenericParamId>>;
    /// Returns the resolution lookback of an impl.
    #[salsa::invoke(items::imp::impl_resolved_lookback)]
    fn impl_resolved_lookback(&self, impl_id: ImplId) -> Option<Arc<ResolvedLookback>>;
    /// Returns the concrete trait that is implemented by the impl
    #[salsa::invoke(items::imp::impl_trait)]
    fn impl_trait(&self, impl_id: ImplId) -> Option<ConcreteTraitId>;
    /// Private query to compute data about an impl.
    #[salsa::invoke(items::imp::priv_impl_definition_data)]
    fn priv_impl_definition_data(&self, impl_id: ImplId) -> Option<items::imp::ImplDefinitionData>;
    /// Returns the semantic definition diagnostics of an impl.
    #[salsa::invoke(items::imp::impl_semantic_definition_diagnostics)]
    fn impl_semantic_definition_diagnostics(
        &self,
        impl_id: ImplId,
    ) -> Diagnostics<SemanticDiagnostic>;
    /// Find implementation for a concrete trait in a module.
    #[salsa::invoke(items::imp::find_impls_at_module)]
    fn find_impls_at_module(
        &self,
        module_id: ModuleId,
        concrete_trait_id: ConcreteTraitId,
    ) -> Option<Vec<ConcreteImplId>>;
    /// Returns the functions in the impl.
    #[salsa::invoke(items::imp::impl_functions)]
    fn impl_functions(&self, impl_id: ImplId) -> Option<Vec<ImplFunctionId>>;

    // impl function.
    // ================
    /// Returns the signature of a impl function.
    #[salsa::invoke(items::imp::impl_function_signature)]
    fn impl_function_signature(
        &self,
        impl_function_id: ImplFunctionId,
    ) -> Option<semantic::Signature>;
    /// Returns the generic params of a impl function.
    #[salsa::invoke(items::imp::impl_function_generic_params)]
    fn impl_function_generic_params(
        &self,
        impl_function_id: ImplFunctionId,
    ) -> Option<Vec<GenericParamId>>;
    /// Returns the semantic diagnostics of a impl function declaration -
    /// its signature excluding its body.
    #[salsa::invoke(items::imp::impl_function_declaration_diagnostics)]
    fn impl_function_declaration_diagnostics(
        &self,
        impl_function_id: ImplFunctionId,
    ) -> Diagnostics<SemanticDiagnostic>;
    /// Returns the resolution lookback of an impl function.
    #[salsa::invoke(items::imp::impl_function_resolved_lookback)]
    fn impl_function_resolved_lookback(
        &self,
        impl_function_id: ImplFunctionId,
    ) -> Option<Arc<ResolvedLookback>>;
    /// Private query to compute data about a impl function declaration.
    #[salsa::invoke(items::imp::priv_impl_function_declaration_data)]
    fn priv_impl_function_declaration_data(
        &self,
        impl_function_id: ImplFunctionId,
    ) -> Option<items::imp::ImplFunctionDeclarationData>;

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
    /// Returns the attributes of a free function declaration.
    #[salsa::invoke(items::free_function::free_function_declaration_attributes)]
    fn free_function_declaration_attributes(
        &self,
        free_function_id: FreeFunctionId,
    ) -> Option<Vec<Attribute>>;
    /// Returns the explicit implicits of a signature of a free function declaration.
    #[salsa::invoke(items::free_function::free_function_declaration_implicits)]
    fn free_function_declaration_implicits(
        &self,
        free_function_id: FreeFunctionId,
    ) -> Option<Vec<TypeId>>;
    /// Returns the generic params of a free function declaration.
    #[salsa::invoke(items::free_function::free_function_declaration_generic_params)]
    fn free_function_declaration_generic_params(
        &self,
        free_function_id: FreeFunctionId,
    ) -> Option<Vec<GenericParamId>>;
    /// Returns the resolution lookback of a free function.
    #[salsa::invoke(items::free_function::free_function_declaration_resolved_lookback)]
    fn free_function_declaration_resolved_lookback(
        &self,
        free_function_id: FreeFunctionId,
    ) -> Option<Arc<ResolvedLookback>>;

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
    /// Returns the direct callees of a free function definition. The items in the vector are
    /// unique.
    #[salsa::invoke(items::free_function::free_function_definition_direct_callees)]
    fn free_function_definition_direct_callees(
        &self,
        free_function_id: FreeFunctionId,
    ) -> Option<Vec<FunctionId>>;
    /// Returns the free function direct callees of a free function definition (i.e. excluding
    /// libfunc callees). The items in the vector are unique.
    #[salsa::invoke(items::free_function::free_function_definition_direct_free_function_callees)]
    fn free_function_definition_direct_free_function_callees(
        &self,
        free_function_id: FreeFunctionId,
    ) -> Option<Vec<FreeFunctionId>>;
    /// Returns the definition of a free function.
    #[salsa::invoke(items::free_function::free_function_definition)]
    fn free_function_definition(
        &self,
        free_function_id: FreeFunctionId,
    ) -> Option<Arc<FreeFunctionDefinition>>;
    /// Returns the resolution lookback of a free function.
    #[salsa::invoke(items::free_function::free_function_definition_resolved_lookback)]
    fn free_function_definition_resolved_lookback(
        &self,
        free_function_id: FreeFunctionId,
    ) -> Option<Arc<ResolvedLookback>>;

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
    /// Returns the explicit implicits of an extern function declaration.
    #[salsa::invoke(items::extern_function::extern_function_declaration_implicits)]
    fn extern_function_declaration_implicits(
        &self,
        extern_function_id: ExternFunctionId,
    ) -> Option<Vec<TypeId>>;
    /// Returns the resolution lookback of an extern function.
    #[salsa::invoke(items::extern_function::extern_function_declaration_resolved_lookback)]
    fn extern_function_declaration_resolved_lookback(
        &self,
        extern_function_id: ExternFunctionId,
    ) -> Option<Arc<ResolvedLookback>>;

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

    // Concrete function.
    // =================
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

    // Concrete type.
    // ==============
    /// Returns the generic_type of a generic function. This include free types, extern
    /// types, etc...
    #[salsa::invoke(types::type_info)]
    fn type_info(
        &self,
        lookup_context: ImplLookupContext,
        ty: types::TypeId,
    ) -> Option<types::TypeInfo>;

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

    // Lookups.
    // ========
    fn lookup_resolved_generic_item_by_ptr(
        &self,
        id: LookupItemId,
        ptr: ast::TerminalIdentifierPtr,
    ) -> Option<ResolvedGenericItem>;
    fn lookup_resolved_concrete_item_by_ptr(
        &self,
        id: LookupItemId,
        ptr: ast::TerminalIdentifierPtr,
    ) -> Option<ResolvedConcreteItem>;

    // Diagnostics.
    // ============
    /// Aggregates module level semantic diagnostics.
    fn module_semantic_diagnostics(
        &self,
        module_id: ModuleId,
    ) -> Option<Diagnostics<SemanticDiagnostic>>;

    /// Aggregates file level semantic diagnostics.
    fn file_semantic_diagnostics(&self, file_id: FileId)
    -> Option<Diagnostics<SemanticDiagnostic>>;

    // Corelib.
    // ========
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
            ModuleItemId::Trait(trait_id) => {
                diagnostics.extend(db.trait_semantic_diagnostics(*trait_id));
            }
            ModuleItemId::Impl(impl_id) => {
                diagnostics.extend(db.impl_semantic_declaration_diagnostics(*impl_id));
                diagnostics.extend(db.impl_semantic_definition_diagnostics(*impl_id));
            }
            ModuleItemId::Submodule(id) => {
                if let Some(file_id) = db.module_main_file(ModuleId::Submodule(*id)) {
                    if db.file_content(file_id).is_none() {
                        // Note that the error location is in the parent module, not the submodule.
                        diagnostics.add(SemanticDiagnostic {
                            stable_location: StableLocation::new(
                                id.module_file(db.upcast()),
                                id.stable_ptr(db.upcast()).untyped(),
                            ),
                            kind: SemanticDiagnosticKind::FileNotFound,
                        });
                    }
                }
            }
            ModuleItemId::ExternType(_) => {}
            ModuleItemId::ExternFunction(extern_function) => {
                diagnostics.extend(db.extern_function_declaration_diagnostics(*extern_function));
            }
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

pub fn lookup_resolved_generic_item_by_ptr(
    db: &dyn SemanticGroup,
    id: LookupItemId,
    ptr: ast::TerminalIdentifierPtr,
) -> Option<ResolvedGenericItem> {
    get_resolver_lookbacks(id, db)
        .into_iter()
        .find_map(|resolver_lookback| resolver_lookback.generic.get(&ptr).cloned())
}

pub fn lookup_resolved_concrete_item_by_ptr(
    db: &dyn SemanticGroup,
    id: LookupItemId,
    ptr: ast::TerminalIdentifierPtr,
) -> Option<ResolvedConcreteItem> {
    get_resolver_lookbacks(id, db)
        .into_iter()
        .find_map(|resolver_lookback| resolver_lookback.concrete.get(&ptr).cloned())
}

fn get_resolver_lookbacks(id: LookupItemId, db: &dyn SemanticGroup) -> Vec<Arc<ResolvedLookback>> {
    match id {
        LookupItemId::ModuleItem(module_item) => match module_item {
            ModuleItemId::Submodule(_) => vec![],
            ModuleItemId::Use(id) => vec![db.use_resolved_lookback(id)],
            ModuleItemId::FreeFunction(id) => vec![
                db.free_function_declaration_resolved_lookback(id),
                db.free_function_definition_resolved_lookback(id),
            ],
            ModuleItemId::Struct(id) => vec![db.struct_resolved_lookback(id)],
            ModuleItemId::Enum(id) => vec![db.enum_resolved_lookback(id)],
            ModuleItemId::Trait(_) => vec![],
            ModuleItemId::Impl(id) => vec![db.impl_resolved_lookback(id)],
            ModuleItemId::ExternType(_) => vec![],
            ModuleItemId::ExternFunction(id) => {
                vec![db.extern_function_declaration_resolved_lookback(id)]
            }
        },
        LookupItemId::ImplFunction(_) => vec![],
    }
    .into_iter()
    .flatten()
    .collect()
}
