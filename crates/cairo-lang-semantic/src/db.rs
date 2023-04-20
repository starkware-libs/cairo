use std::sync::Arc;

use cairo_lang_defs::db::{DefsGroup, GeneratedFileInfo};
use cairo_lang_defs::diagnostic_utils::StableLocation;
use cairo_lang_defs::ids::{
    ConstantId, EnumId, ExternFunctionId, ExternTypeId, FreeFunctionId, FunctionTitleId,
    FunctionWithBodyId, GenericParamId, GenericTypeId, ImplAliasId, ImplDefId, ImplFunctionId,
    LanguageElementId, LookupItemId, ModuleId, ModuleItemId, StructId, TraitFunctionId, TraitId,
    TypeAliasId, UseId, VariantId,
};
use cairo_lang_defs::plugin::MacroPlugin;
use cairo_lang_diagnostics::{Diagnostics, DiagnosticsBuilder, Maybe};
use cairo_lang_filesystem::db::{AsFilesGroupMut, FilesGroup};
use cairo_lang_filesystem::ids::{CrateId, FileId, FileLongId};
use cairo_lang_parser::db::ParserGroup;
use cairo_lang_syntax::attribute::structured::Attribute;
use cairo_lang_syntax::node::ast;
use cairo_lang_syntax::node::stable_ptr::SyntaxStablePtr;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::Upcast;
use smol_str::SmolStr;

use crate::diagnostic::SemanticDiagnosticKind;
use crate::items::constant::Constant;
use crate::items::function_with_body::FunctionBody;
use crate::items::functions::InlineConfiguration;
use crate::items::generics::GenericParam;
use crate::items::imp::{ImplId, ImplLookupContext, UninferredImpl};
use crate::items::module::ModuleSemanticData;
use crate::items::trt::{ConcreteTraitGenericFunctionId, ConcreteTraitId};
use crate::plugin::{DynPluginAuxData, SemanticPlugin};
use crate::resolve::scope::Scope;
use crate::resolve::{ResolvedConcreteItem, ResolvedGenericItem, ResolverData};
use crate::{
    corelib, items, literals, semantic, types, FunctionId, Parameter, SemanticDiagnostic, TypeId,
};

/// Helper trait to make sure we can always get a `dyn SemanticGroup + 'static` from a
/// SemanticGroup.
pub trait Elongate {
    fn elongate(&self) -> &(dyn SemanticGroup + 'static);
}

// Salsa database interface.
// All queries starting with priv_ are for internal use only by this crate.
// They appear in the public API because of salsa limitations.
// We differentiate between the declaration and the definition of each item:
// Declarations and definitions must not depend on other definitions, only other declarations.
// This prevents cycles where there shouldn't be any.
#[salsa::query_group(SemanticDatabase)]
pub trait SemanticGroup:
    DefsGroup
    + Upcast<dyn DefsGroup>
    + ParserGroup
    + Upcast<dyn FilesGroup>
    + AsFilesGroupMut
    + Elongate
{
    #[salsa::interned]
    fn intern_function(&self, id: items::functions::FunctionLongId) -> semantic::FunctionId;
    #[salsa::interned]
    fn intern_concrete_function_with_body(
        &self,
        id: items::functions::ConcreteFunctionWithBody,
    ) -> semantic::ConcreteFunctionWithBodyId;
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
    fn intern_concrete_trait_function(
        &self,
        id: items::trt::ConcreteTraitGenericFunctionLongId,
    ) -> items::trt::ConcreteTraitGenericFunctionId;
    #[salsa::interned]
    fn intern_concrete_impl(
        &self,
        id: items::imp::ConcreteImplLongId,
    ) -> items::imp::ConcreteImplId;
    #[salsa::interned]
    fn intern_type(&self, id: types::TypeLongId) -> semantic::TypeId;
    #[salsa::interned]
    fn intern_literal(&self, id: literals::LiteralLongId) -> literals::LiteralId;

    // Const.
    // ====
    /// Private query to compute data about a constant definition.
    #[salsa::invoke(items::constant::priv_constant_semantic_data)]
    fn priv_constant_semantic_data(
        &self,
        const_id: ConstantId,
    ) -> Maybe<items::constant::ConstantData>;
    /// Returns the semantic diagnostics of a constant definition.
    #[salsa::invoke(items::constant::constant_semantic_diagnostics)]
    fn constant_semantic_diagnostics(
        &self,
        const_id: ConstantId,
    ) -> Diagnostics<SemanticDiagnostic>;
    /// Returns the semantic data of a constant definition.
    #[salsa::invoke(items::constant::constant_semantic_data)]
    fn constant_semantic_data(&self, use_id: ConstantId) -> Maybe<Constant>;
    #[salsa::invoke(items::constant::constant_resolver_data)]
    fn constant_resolver_data(&self, use_id: ConstantId) -> Maybe<Arc<ResolverData>>;

    // Use.
    // ====
    /// Private query to compute data about a use.
    #[salsa::invoke(items::us::priv_use_semantic_data)]
    #[salsa::cycle(items::us::priv_use_semantic_data_cycle)]
    fn priv_use_semantic_data(&self, use_id: UseId) -> Maybe<items::us::UseData>;
    /// Returns the semantic diagnostics of a use.
    #[salsa::invoke(items::us::use_semantic_diagnostics)]
    fn use_semantic_diagnostics(&self, use_id: UseId) -> Diagnostics<SemanticDiagnostic>;
    #[salsa::invoke(items::us::use_resolver_data)]
    fn use_resolver_data(&self, use_id: UseId) -> Maybe<Arc<ResolverData>>;

    // Module.
    // ====

    /// Private query to compute data about the module.
    #[salsa::invoke(items::module::priv_module_semantic_data)]
    fn priv_module_semantic_data(&self, module_id: ModuleId) -> Maybe<Arc<ModuleSemanticData>>;

    /// Returns the scope of a module. See [Scope].
    #[salsa::invoke(items::module::module_scope)]
    fn module_scope(&self, module_id: ModuleId) -> Maybe<Arc<Scope>>;

    /// Returns [Maybe::Err] if the module was not properly resolved.
    /// Returns [Maybe::Ok(Option::None)] if the item does not exist.
    #[salsa::invoke(items::module::module_item_by_name)]
    fn module_item_by_name(
        &self,
        module_id: ModuleId,
        name: SmolStr,
    ) -> Maybe<Option<ModuleItemId>>;

    /// Returns the attributes of a module.
    #[salsa::invoke(items::module::module_attributes)]
    fn module_attributes(&self, module_id: ModuleId) -> Maybe<Vec<Attribute>>;

    // Struct.
    // =======
    /// Private query to compute data about a struct declaration.
    #[salsa::invoke(items::structure::priv_struct_declaration_data)]
    fn priv_struct_declaration_data(
        &self,
        struct_id: StructId,
    ) -> Maybe<items::structure::StructDeclarationData>;
    /// Returns the declaration diagnostics of a struct.
    #[salsa::invoke(items::structure::struct_declaration_diagnostics)]
    fn struct_declaration_diagnostics(
        &self,
        struct_id: StructId,
    ) -> Diagnostics<SemanticDiagnostic>;
    /// Returns the attributes of a struct.
    #[salsa::invoke(items::structure::struct_attributes)]
    fn struct_attributes(&self, struct_id: StructId) -> Maybe<Vec<Attribute>>;
    /// Returns the generic parameters of an enum.
    #[salsa::invoke(items::structure::struct_generic_params)]
    fn struct_generic_params(&self, struct_id: StructId) -> Maybe<Vec<GenericParam>>;
    /// Returns the resolution resolved_items of a struct declaration.
    #[salsa::invoke(items::structure::struct_declaration_resolver_data)]
    fn struct_declaration_resolver_data(&self, structure_id: StructId) -> Maybe<Arc<ResolverData>>;

    /// Private query to compute data about a struct definition.
    #[salsa::invoke(items::structure::priv_struct_definition_data)]
    fn priv_struct_definition_data(
        &self,
        struct_id: StructId,
    ) -> Maybe<items::structure::StructDefinitionData>;
    /// Returns the semantic diagnostics of a struct definition.
    #[salsa::invoke(items::structure::struct_definition_diagnostics)]
    fn struct_definition_diagnostics(&self, struct_id: StructId)
    -> Diagnostics<SemanticDiagnostic>;
    /// Returns the members of a struct.
    #[salsa::invoke(items::structure::struct_members)]
    fn struct_members(
        &self,
        struct_id: StructId,
    ) -> Maybe<OrderedHashMap<SmolStr, semantic::Member>>;
    /// Returns the resolution resolved_items of a struct definition.
    #[salsa::invoke(items::structure::struct_definition_resolver_data)]
    fn struct_definition_resolver_data(&self, structure_id: StructId) -> Maybe<Arc<ResolverData>>;

    // Enum.
    // =======
    /// Private query to compute data about an enum declaration.
    #[salsa::invoke(items::enm::priv_enum_declaration_data)]
    fn priv_enum_declaration_data(&self, enum_id: EnumId)
    -> Maybe<items::enm::EnumDeclarationData>;
    /// Returns the diagnostics of an enum declaration.
    #[salsa::invoke(items::enm::enum_declaration_diagnostics)]
    fn enum_declaration_diagnostics(&self, enum_id: EnumId) -> Diagnostics<SemanticDiagnostic>;
    /// Returns the generic parameters of an enum.
    #[salsa::invoke(items::enm::enum_generic_params)]
    fn enum_generic_params(&self, enum_id: EnumId) -> Maybe<Vec<GenericParam>>;
    /// Returns the resolution resolved_items of an enum declaration.
    #[salsa::invoke(items::enm::enum_declaration_resolver_data)]
    fn enum_declaration_resolver_data(&self, enum_id: EnumId) -> Maybe<Arc<ResolverData>>;

    /// Private query to compute data about an enum definition.
    #[salsa::invoke(items::enm::priv_enum_definition_data)]
    fn priv_enum_definition_data(&self, enum_id: EnumId) -> Maybe<items::enm::EnumDefinitionData>;
    /// Returns the definition diagnostics of an enum definition.
    #[salsa::invoke(items::enm::enum_definition_diagnostics)]
    fn enum_definition_diagnostics(&self, enum_id: EnumId) -> Diagnostics<SemanticDiagnostic>;
    /// Returns the members of an enum.
    #[salsa::invoke(items::enm::enum_variants)]
    fn enum_variants(&self, enum_id: EnumId) -> Maybe<OrderedHashMap<SmolStr, VariantId>>;
    /// Returns the semantic model of a variant.
    #[salsa::invoke(items::enm::variant_semantic)]
    fn variant_semantic(&self, enum_id: EnumId, variant_id: VariantId) -> Maybe<semantic::Variant>;
    /// Returns the resolution resolved_items of an enum definition.
    #[salsa::invoke(items::enm::enum_definition_resolver_data)]
    fn enum_definition_resolver_data(&self, enum_id: EnumId) -> Maybe<Arc<ResolverData>>;

    // Type Alias.
    // ====
    /// Private query to compute data about a type alias.
    #[salsa::invoke(items::type_alias::priv_type_alias_semantic_data)]
    #[salsa::cycle(items::type_alias::priv_type_alias_semantic_data_cycle)]
    fn priv_type_alias_semantic_data(
        &self,
        type_alias_id: TypeAliasId,
    ) -> Maybe<items::type_alias::TypeAliasData>;
    /// Returns the semantic diagnostics of a type alias.
    #[salsa::invoke(items::type_alias::type_alias_semantic_diagnostics)]
    fn type_alias_semantic_diagnostics(
        &self,
        type_alias_id: TypeAliasId,
    ) -> Diagnostics<SemanticDiagnostic>;
    /// Returns the resolved type of a type alias.
    #[salsa::invoke(items::type_alias::type_alias_resolved_type)]
    fn type_alias_resolved_type(&self, type_alias_id: TypeAliasId) -> Maybe<TypeId>;
    /// Returns the generic parameters of a type alias.
    #[salsa::invoke(items::type_alias::type_alias_generic_params)]
    fn type_alias_generic_params(&self, enum_id: TypeAliasId) -> Maybe<Vec<GenericParam>>;
    /// Returns the resolution resolved_items of a type alias.
    #[salsa::invoke(items::type_alias::type_alias_resolver_data)]
    fn type_alias_resolver_data(&self, type_alias_id: TypeAliasId) -> Maybe<Arc<ResolverData>>;

    // Impl Alias.
    // ====
    /// Private query to compute data about a type alias.
    #[salsa::invoke(items::impl_alias::priv_impl_alias_semantic_data)]
    #[salsa::cycle(items::impl_alias::priv_impl_alias_semantic_data_cycle)]
    fn priv_impl_alias_semantic_data(
        &self,
        impl_alias_id: ImplAliasId,
    ) -> Maybe<items::impl_alias::ImplAliasData>;
    /// Returns the semantic diagnostics of a type alias.
    #[salsa::invoke(items::impl_alias::impl_alias_semantic_diagnostics)]
    fn impl_alias_semantic_diagnostics(
        &self,
        impl_alias_id: ImplAliasId,
    ) -> Diagnostics<SemanticDiagnostic>;
    /// Returns the resolved type of a type alias.
    #[salsa::invoke(items::impl_alias::impl_alias_resolved_impl)]
    fn impl_alias_resolved_impl(&self, impl_alias_id: ImplAliasId) -> Maybe<ImplId>;
    /// Returns the generic parameters of a type alias.
    #[salsa::invoke(items::impl_alias::impl_alias_generic_params)]
    fn impl_alias_generic_params(&self, enum_id: ImplAliasId) -> Maybe<Vec<GenericParam>>;
    /// Returns the resolution resolved_items of a type alias.
    #[salsa::invoke(items::impl_alias::impl_alias_resolver_data)]
    fn impl_alias_resolver_data(&self, impl_alias_id: ImplAliasId) -> Maybe<Arc<ResolverData>>;

    // Trait.
    // =======
    /// Private query to compute data about a trait.
    #[salsa::invoke(items::trt::priv_trait_semantic_data)]
    fn priv_trait_semantic_data(&self, trait_id: TraitId) -> Maybe<items::trt::TraitData>;
    /// Returns the semantic diagnostics of a trait.
    #[salsa::invoke(items::trt::trait_semantic_diagnostics)]
    fn trait_semantic_diagnostics(&self, trait_id: TraitId) -> Diagnostics<SemanticDiagnostic>;
    /// Returns the generic parameters of a trait.
    #[salsa::invoke(items::trt::trait_generic_params)]
    fn trait_generic_params(&self, trait_id: TraitId) -> Maybe<Vec<GenericParam>>;
    /// Returns the attributes of a trait.
    #[salsa::invoke(items::trt::trait_attributes)]
    fn trait_attributes(&self, trait_id: TraitId) -> Maybe<Vec<Attribute>>;
    /// Returns the functions of a trait.
    #[salsa::invoke(items::trt::trait_functions)]
    fn trait_functions(&self, trait_id: TraitId)
    -> Maybe<OrderedHashMap<SmolStr, TraitFunctionId>>;
    /// Returns the function with the given name of the given trait, if exists.
    #[salsa::invoke(items::trt::trait_function_by_name)]
    fn trait_function_by_name(
        &self,
        trait_id: TraitId,
        name: SmolStr,
    ) -> Maybe<Option<TraitFunctionId>>;

    // Trait function.
    // ================
    /// Private query to compute data about a trait function.
    #[salsa::invoke(items::trt::priv_trait_function_data)]
    fn priv_trait_function_data(
        &self,
        function_id: TraitFunctionId,
    ) -> Maybe<items::trt::TraitFunctionData>;
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
    ) -> Maybe<semantic::Signature>;
    /// Returns the attributes of a trait function.
    #[salsa::invoke(items::trt::trait_function_attributes)]
    fn trait_function_attributes(
        &self,
        trait_function_id: TraitFunctionId,
    ) -> Maybe<Vec<Attribute>>;
    /// Returns the generic params of a trait function.
    #[salsa::invoke(items::trt::trait_function_generic_params)]
    fn trait_function_generic_params(
        &self,
        trait_function_id: TraitFunctionId,
    ) -> Maybe<Vec<GenericParam>>;
    /// Returns the resolution resolved_items of a trait function.
    #[salsa::invoke(items::trt::trait_function_resolver_data)]
    fn trait_function_resolver_data(
        &self,
        trait_function_id: TraitFunctionId,
    ) -> Maybe<Arc<ResolverData>>;
    /// Returns the generic params of a concrete trait function.
    #[salsa::invoke(items::trt::concrete_trait_function_generic_params)]
    fn concrete_trait_function_generic_params(
        &self,
        concrete_trait_function_id: ConcreteTraitGenericFunctionId,
    ) -> Maybe<Vec<GenericParam>>;
    /// Returns the signature of a concrete trait function.
    #[salsa::invoke(items::trt::concrete_trait_function_signature)]
    fn concrete_trait_function_signature(
        &self,
        concrete_trait_function_id: ConcreteTraitGenericFunctionId,
    ) -> Maybe<semantic::Signature>;

    // Impl.
    // =======
    /// Private query to compute declaration data about an impl.
    #[salsa::invoke(items::imp::priv_impl_declaration_data)]
    #[salsa::cycle(items::imp::priv_impl_declaration_data_cycle)]
    fn priv_impl_declaration_data(
        &self,
        impl_def_id: ImplDefId,
    ) -> Maybe<items::imp::ImplDeclarationData>;
    /// Returns the semantic declaration diagnostics of an impl.
    #[salsa::invoke(items::imp::impl_semantic_declaration_diagnostics)]
    fn impl_semantic_declaration_diagnostics(
        &self,
        impl_def_id: ImplDefId,
    ) -> Diagnostics<SemanticDiagnostic>;
    /// Returns the generic parameters of an impl.
    #[salsa::invoke(items::imp::impl_def_generic_params)]
    fn impl_def_generic_params(&self, impl_def_id: ImplDefId) -> Maybe<Vec<GenericParam>>;
    /// Returns the resolution resolved_items of an impl.
    #[salsa::invoke(items::imp::impl_def_resolver_data)]
    fn impl_def_resolver_data(&self, impl_def_id: ImplDefId) -> Maybe<Arc<ResolverData>>;
    /// Returns the concrete trait that is implemented by the impl.
    #[salsa::invoke(items::imp::impl_def_concrete_trait)]
    fn impl_def_concrete_trait(&self, impl_def_id: ImplDefId) -> Maybe<ConcreteTraitId>;
    /// Returns the concrete trait that is implemented by the concrete impl.
    #[salsa::invoke(items::imp::impl_concrete_trait)]
    fn impl_concrete_trait(&self, impl_id: ImplId) -> Maybe<ConcreteTraitId>;
    /// Private query to compute data about an impl.
    #[salsa::invoke(items::imp::priv_impl_definition_data)]
    fn priv_impl_definition_data(
        &self,
        impl_def_id: ImplDefId,
    ) -> Maybe<items::imp::ImplDefinitionData>;
    /// Returns the semantic definition diagnostics of an impl.
    #[salsa::invoke(items::imp::impl_semantic_definition_diagnostics)]
    fn impl_semantic_definition_diagnostics(
        &self,
        impl_def_id: ImplDefId,
    ) -> Diagnostics<SemanticDiagnostic>;
    /// Returns the functions in the impl.
    #[salsa::invoke(items::imp::impl_functions)]
    fn impl_functions(
        &self,
        impl_def_id: ImplDefId,
    ) -> Maybe<OrderedHashMap<SmolStr, ImplFunctionId>>;
    /// Returns the impl function that matches the given trait function, if exists.
    /// Note that a function that doesn't exist in the impl doesn't necessarily indicate an error,
    /// as, e.g., a trait function that has a default implementation doesn't have to be
    /// implemented in the impl.
    #[salsa::invoke(items::imp::impl_function_by_trait_function)]
    fn impl_function_by_trait_function(
        &self,
        impl_def_id: ImplDefId,
        trait_function_id: TraitFunctionId,
    ) -> Maybe<Option<ImplFunctionId>>;
    /// Returns candidate [ImplDefId]s for a specific trait lookup constraint.
    #[salsa::invoke(items::imp::module_impl_ids_for_trait_info)]
    fn module_impl_ids_for_trait_info(
        &self,
        module_id: ModuleId,
        trait_lookup_constraint: items::imp::TraitFilter,
    ) -> Maybe<Vec<UninferredImpl>>;

    // Impl function.
    // ================
    /// Returns the signature of an impl function.
    #[salsa::invoke(items::imp::impl_function_signature)]
    fn impl_function_signature(
        &self,
        impl_function_id: ImplFunctionId,
    ) -> Maybe<semantic::Signature>;
    /// Returns the explicit implicits of a signature of an impl function.
    #[salsa::invoke(items::imp::impl_function_declaration_implicits)]
    fn impl_function_declaration_implicits(
        &self,
        impl_function_id: ImplFunctionId,
    ) -> Maybe<Vec<TypeId>>;
    /// Returns the generic params of an impl function.
    #[salsa::invoke(items::imp::impl_function_generic_params)]
    fn impl_function_generic_params(
        &self,
        impl_function_id: ImplFunctionId,
    ) -> Maybe<Vec<GenericParam>>;
    /// Returns the semantic diagnostics of an impl function's declaration (signature).
    #[salsa::invoke(items::imp::impl_function_declaration_diagnostics)]
    fn impl_function_declaration_diagnostics(
        &self,
        impl_function_id: ImplFunctionId,
    ) -> Diagnostics<SemanticDiagnostic>;
    /// Returns the resolution resolved_items of an impl function's declaration.
    #[salsa::invoke(items::imp::impl_function_resolver_data)]
    fn impl_function_resolver_data(
        &self,
        impl_function_id: ImplFunctionId,
    ) -> Maybe<Arc<ResolverData>>;
    /// Returns the inline configuration of an impl function's declaration.
    #[salsa::invoke(items::imp::impl_function_declaration_inline_config)]
    fn impl_function_declaration_inline_config(
        &self,
        impl_function_id: ImplFunctionId,
    ) -> Maybe<InlineConfiguration>;
    /// Returns the trait function of an impl function.
    #[salsa::invoke(items::imp::impl_function_trait_function)]
    fn impl_function_trait_function(
        &self,
        impl_function_id: ImplFunctionId,
    ) -> Maybe<TraitFunctionId>;
    /// Private query to compute data about an impl function declaration.
    #[salsa::invoke(items::imp::priv_impl_function_declaration_data)]
    fn priv_impl_function_declaration_data(
        &self,
        impl_function_id: ImplFunctionId,
    ) -> Maybe<items::imp::ImplFunctionDeclarationData>;

    /// Returns the semantic diagnostics of an impl function definition (declaration + body).
    #[salsa::invoke(items::imp::impl_function_body_diagnostics)]
    fn impl_function_body_diagnostics(
        &self,
        impl_function_id: ImplFunctionId,
    ) -> Diagnostics<SemanticDiagnostic>;
    /// Returns the definition of an impl function.
    #[salsa::invoke(items::imp::impl_function_body)]
    fn impl_function_body(&self, impl_function_id: ImplFunctionId) -> Maybe<Arc<FunctionBody>>;
    /// Returns the resolution resolved_items of an impl function's definition.
    #[salsa::invoke(items::imp::impl_function_body_resolver_data)]
    fn impl_function_body_resolver_data(
        &self,
        impl_function_id: ImplFunctionId,
    ) -> Maybe<Arc<ResolverData>>;
    /// Private query to compute data about an impl function definition (declaration + body)
    #[salsa::invoke(items::imp::priv_impl_function_body_data)]
    fn priv_impl_function_body_data(
        &self,
        impl_function_id: ImplFunctionId,
    ) -> Maybe<items::function_with_body::FunctionBodyData>;

    // Free function.
    // ==============
    /// Returns the semantic diagnostics of a free function's declaration (signature).
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
    ) -> Maybe<semantic::Signature>;
    /// Returns the explicit implicits of a signature of a free function.
    #[salsa::invoke(items::free_function::free_function_declaration_implicits)]
    fn free_function_declaration_implicits(
        &self,
        free_function_id: FreeFunctionId,
    ) -> Maybe<Vec<TypeId>>;
    /// Returns the generic params of a free function.
    #[salsa::invoke(items::free_function::free_function_generic_params)]
    fn free_function_generic_params(
        &self,
        free_function_id: FreeFunctionId,
    ) -> Maybe<Vec<GenericParam>>;
    /// Returns the resolution resolved_items of a free function's declaration.
    #[salsa::invoke(items::free_function::free_function_declaration_resolver_data)]
    fn free_function_declaration_resolver_data(
        &self,
        free_function_id: FreeFunctionId,
    ) -> Maybe<Arc<ResolverData>>;
    /// Returns the inline configuration of a free function's declaration.
    #[salsa::invoke(items::free_function::free_function_declaration_inline_config)]
    fn free_function_declaration_inline_config(
        &self,
        free_function_id: FreeFunctionId,
    ) -> Maybe<InlineConfiguration>;
    /// Private query to compute data about a free function declaration - its signature excluding
    /// its body.
    #[salsa::invoke(items::free_function::priv_free_function_declaration_data)]
    fn priv_free_function_declaration_data(
        &self,
        function_id: FreeFunctionId,
    ) -> Maybe<items::functions::FunctionDeclarationData>;

    /// Returns the semantic diagnostics of a free function's body.
    #[salsa::invoke(items::free_function::free_function_body_diagnostics)]
    fn free_function_body_diagnostics(
        &self,
        free_function_id: FreeFunctionId,
    ) -> Diagnostics<SemanticDiagnostic>;
    /// Returns the resolution resolved_items of a free function's body.
    #[salsa::invoke(items::free_function::free_function_body_resolver_data)]
    fn free_function_body_resolver_data(
        &self,
        free_function_id: FreeFunctionId,
    ) -> Maybe<Arc<ResolverData>>;
    /// Private query to compute data about a free function's body.
    #[salsa::invoke(items::free_function::priv_free_function_body_data)]
    fn priv_free_function_body_data(
        &self,
        free_function_id: FreeFunctionId,
    ) -> Maybe<items::function_with_body::FunctionBodyData>;

    // Function with body.
    // ===================
    /// Returns the semantic diagnostics of a declaration (signature) of a function with a body.
    #[salsa::invoke(items::function_with_body::function_declaration_diagnostics)]
    fn function_declaration_diagnostics(
        &self,
        function_id: FunctionWithBodyId,
    ) -> Diagnostics<SemanticDiagnostic>;
    /// Returns the inline configuration of a declaration (signature) of a function with a body.
    #[salsa::invoke(items::function_with_body::function_declaration_inline_config)]
    fn function_declaration_inline_config(
        &self,
        function_id: FunctionWithBodyId,
    ) -> Maybe<InlineConfiguration>;
    /// Returns the signature of a function with a body.
    #[salsa::invoke(items::function_with_body::function_with_body_signature)]
    fn function_with_body_signature(
        &self,
        function_id: FunctionWithBodyId,
    ) -> Maybe<semantic::Signature>;
    /// Returns all the available generic params inside a function body.
    #[salsa::invoke(items::function_with_body::function_with_body_generic_params)]
    fn function_with_body_generic_params(
        &self,
        function_id: FunctionWithBodyId,
    ) -> Maybe<Vec<GenericParam>>;
    /// Returns the attributes of a function with a body.
    #[salsa::invoke(items::function_with_body::function_with_body_attributes)]
    fn function_with_body_attributes(
        &self,
        function_id: FunctionWithBodyId,
    ) -> Maybe<Vec<Attribute>>;

    /// Returns the semantic diagnostics of a body of a function (with a body).
    #[salsa::invoke(items::function_with_body::function_body_diagnostics)]
    fn function_body_diagnostics(
        &self,
        function_id: FunctionWithBodyId,
    ) -> Diagnostics<SemanticDiagnostic>;
    /// Returns the body expr of a function (with a body).
    #[salsa::invoke(items::function_with_body::function_body_expr)]
    fn function_body_expr(&self, function_id: FunctionWithBodyId) -> Maybe<semantic::ExprId>;
    /// Returns the body of a function (with a body).
    #[salsa::invoke(items::function_with_body::function_body)]
    fn function_body(&self, function_id: FunctionWithBodyId) -> Maybe<Arc<FunctionBody>>;

    // Extern function.
    // ================
    /// Private query to compute data about an extern function declaration. An extern function has
    /// no body, and thus only has a declaration.
    #[salsa::invoke(items::extern_function::priv_extern_function_declaration_data)]
    fn priv_extern_function_declaration_data(
        &self,
        function_id: ExternFunctionId,
    ) -> Maybe<items::functions::FunctionDeclarationData>;
    /// Returns the inline configuration of an extern function's declaration.
    #[salsa::invoke(items::extern_function::extern_function_declaration_inline_config)]
    fn extern_function_declaration_inline_config(
        &self,
        extern_function_id: ExternFunctionId,
    ) -> Maybe<InlineConfiguration>;
    /// Returns the semantic diagnostics of an extern function declaration. An extern function has
    /// no body, and thus only has a declaration.
    #[salsa::invoke(items::extern_function::extern_function_declaration_diagnostics)]
    fn extern_function_declaration_diagnostics(
        &self,
        extern_function_id: ExternFunctionId,
    ) -> Diagnostics<SemanticDiagnostic>;
    /// Returns the signature of an extern function.
    #[salsa::invoke(items::extern_function::extern_function_signature)]
    fn extern_function_signature(
        &self,
        extern_function_id: ExternFunctionId,
    ) -> Maybe<semantic::Signature>;
    /// Returns the generic params of an extern function.
    #[salsa::invoke(items::extern_function::extern_function_declaration_generic_params)]
    fn extern_function_declaration_generic_params(
        &self,
        extern_function_id: ExternFunctionId,
    ) -> Maybe<Vec<GenericParam>>;
    /// Returns the explicit implicits of an extern function declaration.
    #[salsa::invoke(items::extern_function::extern_function_declaration_implicits)]
    fn extern_function_declaration_implicits(
        &self,
        extern_function_id: ExternFunctionId,
    ) -> Maybe<Vec<TypeId>>;
    /// Returns the ref parameters of an extern function declaration.
    #[salsa::invoke(items::extern_function::extern_function_declaration_refs)]
    fn extern_function_declaration_refs(
        &self,
        extern_function_id: ExternFunctionId,
    ) -> Maybe<Vec<Parameter>>;
    /// Returns the resolution resolved_items of an extern function.
    #[salsa::invoke(items::extern_function::extern_function_declaration_resolver_data)]
    fn extern_function_declaration_resolver_data(
        &self,
        extern_function_id: ExternFunctionId,
    ) -> Maybe<Arc<ResolverData>>;

    // Extern type.
    // ============
    /// Private query to compute data about an extern type declaration. An extern type has
    /// no body, and thus only has a declaration.
    #[salsa::invoke(items::extern_type::priv_extern_type_declaration_data)]
    fn priv_extern_type_declaration_data(
        &self,
        type_id: ExternTypeId,
    ) -> Maybe<items::extern_type::ExternTypeDeclarationData>;
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
    ) -> Maybe<Vec<GenericParam>>;

    // Function Signature.
    // =================
    /// Returns the signature of the given FunctionTitleId. This include free functions, extern
    /// functions, etc...
    #[salsa::invoke(items::functions::function_title_signature)]
    fn function_title_signature(
        &self,
        function_title_id: FunctionTitleId,
    ) -> Maybe<semantic::Signature>;

    /// Returns the generic parameters of the given FunctionTitleId. This include free
    /// functions, extern functions, etc...
    #[salsa::invoke(items::functions::function_title_generic_params)]
    fn function_title_generic_params(
        &self,
        function_title_id: FunctionTitleId,
    ) -> Maybe<Vec<GenericParam>>;

    // Concrete function.
    // =================
    /// Returns the signature of a concrete function. This include free functions, extern functions,
    /// etc...
    #[salsa::invoke(items::functions::concrete_function_signature)]
    fn concrete_function_signature(&self, function_id: FunctionId) -> Maybe<semantic::Signature>;

    // Generic type.
    // =============
    /// Returns the generic params of a generic type.
    #[salsa::invoke(types::generic_type_generic_params)]
    fn generic_type_generic_params(&self, generic_type: GenericTypeId) -> Maybe<Vec<GenericParam>>;

    // Generic param.
    // ==============
    #[salsa::invoke(items::generics::generic_param_semantic)]
    fn generic_param_semantic(&self, generic_param: GenericParamId) -> Maybe<GenericParam>;

    // Concrete type.
    // ==============
    /// Returns the generic_type of a generic function. This include free types, extern
    /// types, etc...
    #[salsa::invoke(types::type_info)]
    fn type_info(
        &self,
        lookup_context: ImplLookupContext,
        ty: types::TypeId,
    ) -> Maybe<types::TypeInfo>;

    // Expression.
    // ===========
    /// Assumes function and expression are present.
    #[salsa::invoke(items::function_with_body::expr_semantic)]
    fn expr_semantic(
        &self,
        function_id: FunctionWithBodyId,
        id: semantic::ExprId,
    ) -> semantic::Expr;
    /// Assumes function and statement are valid.
    #[salsa::invoke(items::function_with_body::statement_semantic)]
    fn statement_semantic(
        &self,
        function_id: FunctionWithBodyId,
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
    ) -> Maybe<Diagnostics<SemanticDiagnostic>>;

    /// Aggregates file level semantic diagnostics.
    fn file_semantic_diagnostics(&self, file_id: FileId) -> Maybe<Diagnostics<SemanticDiagnostic>>;

    // Corelib.
    // ========
    #[salsa::invoke(corelib::core_crate)]
    fn core_crate(&self) -> CrateId;
    #[salsa::invoke(corelib::core_module)]
    fn core_module(&self) -> ModuleId;
    #[salsa::invoke(corelib::core_felt252_ty)]
    fn core_felt252_ty(&self) -> semantic::TypeId;

    // Plugins.
    // ========
    #[salsa::input]
    fn semantic_plugins(&self) -> Vec<Arc<dyn SemanticPlugin>>;
}

impl<T: Upcast<dyn SemanticGroup + 'static>> Elongate for T {
    fn elongate(&self) -> &(dyn SemanticGroup + 'static) {
        self.upcast()
    }
}

/// Initializes a database with DefsGroup.
pub fn init_semantic_group(db: &mut (dyn SemanticGroup + 'static)) {
    // Initialize inputs.
    db.set_semantic_plugins(Vec::new());
}

pub trait SemanticGroupEx<'a>: Upcast<dyn SemanticGroup + 'a> {
    fn get_macro_plugins(&self) -> Vec<Arc<dyn MacroPlugin>> {
        self.upcast()
            .semantic_plugins()
            .into_iter()
            .map(|plugin| plugin.as_dyn_macro_plugin())
            .collect()
    }
}
impl<'a, T: Upcast<dyn SemanticGroup + 'a> + ?Sized> SemanticGroupEx<'a> for T {}

fn module_semantic_diagnostics(
    db: &dyn SemanticGroup,
    module_id: ModuleId,
) -> Maybe<Diagnostics<SemanticDiagnostic>> {
    let mut diagnostics = DiagnosticsBuilder::default();
    for (module_file_id, plugin_diag) in db.module_plugin_diagnostics(module_id)? {
        diagnostics.add(SemanticDiagnostic::new(
            StableLocation::new(module_file_id, plugin_diag.stable_ptr),
            SemanticDiagnosticKind::PluginDiagnostic(plugin_diag),
        ));
    }

    diagnostics.extend(db.priv_module_semantic_data(module_id)?.diagnostics.clone());

    for item in db.module_items(module_id)?.iter() {
        match item {
            ModuleItemId::Constant(const_id) => {
                diagnostics.extend(db.constant_semantic_diagnostics(*const_id));
            }
            // Add signature diagnostics.
            ModuleItemId::Use(use_id) => {
                diagnostics.extend(db.use_semantic_diagnostics(*use_id));
            }
            ModuleItemId::FreeFunction(free_function) => {
                diagnostics.extend(db.free_function_declaration_diagnostics(*free_function));
                diagnostics.extend(db.free_function_body_diagnostics(*free_function));
            }
            ModuleItemId::Struct(struct_id) => {
                diagnostics.extend(db.struct_declaration_diagnostics(*struct_id));
                diagnostics.extend(db.struct_definition_diagnostics(*struct_id));
            }
            ModuleItemId::Enum(enum_id) => {
                diagnostics.extend(db.enum_definition_diagnostics(*enum_id));
                diagnostics.extend(db.enum_declaration_diagnostics(*enum_id));
            }
            ModuleItemId::Trait(trait_id) => {
                diagnostics.extend(db.trait_semantic_diagnostics(*trait_id));
            }
            ModuleItemId::Impl(impl_def_id) => {
                diagnostics.extend(db.impl_semantic_declaration_diagnostics(*impl_def_id));
                diagnostics.extend(db.impl_semantic_definition_diagnostics(*impl_def_id));
            }
            ModuleItemId::Submodule(submodule_id) => {
                // Note that the parent module does not report the diagnostics of its submodules.
                if let Ok(file_id) = db.module_main_file(ModuleId::Submodule(*submodule_id)) {
                    if db.file_content(file_id).is_none() {
                        // Note that the error location is in the parent module, not the
                        // submodule.

                        let path = match db.lookup_intern_file(file_id) {
                            FileLongId::OnDisk(path) => path.display().to_string(),
                            FileLongId::Virtual(_) => panic!("Expected OnDisk file."),
                        };

                        let stable_location = StableLocation::new(
                            submodule_id.module_file_id(db.upcast()),
                            submodule_id.stable_ptr(db.upcast()).untyped(),
                        );
                        diagnostics.add(SemanticDiagnostic::new(
                            stable_location,
                            SemanticDiagnosticKind::ModuleFileNotFound { path },
                        ));
                    }
                }
            }
            ModuleItemId::ExternType(extern_type) => {
                diagnostics.extend(db.extern_type_declaration_diagnostics(*extern_type));
            }
            ModuleItemId::ExternFunction(extern_function) => {
                diagnostics.extend(db.extern_function_declaration_diagnostics(*extern_function));
            }
            ModuleItemId::TypeAlias(type_alias) => {
                diagnostics.extend(db.type_alias_semantic_diagnostics(*type_alias));
            }
            ModuleItemId::ImplAlias(type_alias) => {
                diagnostics.extend(db.impl_alias_semantic_diagnostics(*type_alias));
            }
        }
    }

    Ok(map_diagnostics(
        db.elongate(),
        module_id,
        &db.module_generated_file_infos(module_id)?,
        diagnostics.build(),
    )
    .1)
}

/// Transforms diagnostics that originate from plugin generated files. Uses the plugin's diagnostic
/// mapper.
fn map_diagnostics(
    db: &(dyn SemanticGroup + 'static),
    module_id: ModuleId,
    generated_file_info: &[Option<GeneratedFileInfo>],
    original_diagnostics: Diagnostics<SemanticDiagnostic>,
) -> (bool, Diagnostics<SemanticDiagnostic>) {
    let mut diagnostics = DiagnosticsBuilder::default();
    let mut has_change: bool = false;

    for tree in &original_diagnostics.0.subtrees {
        let (changed, new_diags) =
            map_diagnostics(db, module_id, generated_file_info, tree.clone());
        diagnostics.extend(new_diags);
        has_change |= changed;
    }

    for diag in &original_diagnostics.0.leaves {
        assert_eq!(diag.stable_location.module_file_id.0, module_id, "Unexpected module id.");
        let file_index = diag.stable_location.module_file_id.1;
        if let Some(file_info) = &generated_file_info[file_index.0] {
            let opt_diag = file_info
                .aux_data
                .0
                .as_any()
                .downcast_ref::<DynPluginAuxData>()
                .and_then(|mapper| mapper.map_diag(db.upcast(), diag));
            if let Some(plugin_diag) = opt_diag {
                // We don't have a real location, so we give a dummy location in the correct file.
                // SemanticDiagnostic struct knowns to give the proper span for
                // WrappedPluginDiagnostic.
                let stable_location = StableLocation::new(
                    file_info.origin,
                    db.intern_stable_ptr(SyntaxStablePtr::Root),
                );
                let kind = SemanticDiagnosticKind::WrappedPluginDiagnostic {
                    diagnostic: plugin_diag,
                    original_diag: Box::new(diag.clone()),
                };
                diagnostics.add(SemanticDiagnostic::new(stable_location, kind));
                has_change = true;
                continue;
            }
        }
        diagnostics.add(diag.clone());
    }

    if !has_change {
        return (false, original_diagnostics);
    }

    (has_change, diagnostics.build())
}

fn file_semantic_diagnostics(
    db: &dyn SemanticGroup,
    file_id: FileId,
) -> Maybe<Diagnostics<SemanticDiagnostic>> {
    let mut diagnostics = DiagnosticsBuilder::default();
    for module_id in db.file_modules(file_id)? {
        if let Ok(module_diagnostics) = db.module_semantic_diagnostics(module_id) {
            diagnostics.extend(module_diagnostics)
        }
    }
    Ok(diagnostics.build())
}

pub fn lookup_resolved_generic_item_by_ptr(
    db: &dyn SemanticGroup,
    id: LookupItemId,
    ptr: ast::TerminalIdentifierPtr,
) -> Option<ResolvedGenericItem> {
    get_resolver_datas(id, db)
        .into_iter()
        .find_map(|resolver_data| resolver_data.resolved_items.generic.get(&ptr).cloned())
}

pub fn lookup_resolved_concrete_item_by_ptr(
    db: &dyn SemanticGroup,
    id: LookupItemId,
    ptr: ast::TerminalIdentifierPtr,
) -> Option<ResolvedConcreteItem> {
    get_resolver_datas(id, db)
        .into_iter()
        .find_map(|resolver_data| resolver_data.resolved_items.concrete.get(&ptr).cloned())
}

fn get_resolver_datas(id: LookupItemId, db: &dyn SemanticGroup) -> Vec<Arc<ResolverData>> {
    match id {
        LookupItemId::ModuleItem(module_item) => match module_item {
            ModuleItemId::Constant(id) => vec![db.constant_resolver_data(id)],
            ModuleItemId::Submodule(_) => vec![],
            ModuleItemId::Use(id) => vec![db.use_resolver_data(id)],
            ModuleItemId::FreeFunction(id) => vec![
                db.free_function_declaration_resolver_data(id),
                db.free_function_body_resolver_data(id),
            ],
            ModuleItemId::Struct(id) => vec![
                db.struct_declaration_resolver_data(id),
                db.struct_definition_resolver_data(id),
            ],
            ModuleItemId::Enum(id) => {
                vec![db.enum_definition_resolver_data(id), db.enum_declaration_resolver_data(id)]
            }
            ModuleItemId::TypeAlias(id) => vec![db.type_alias_resolver_data(id)],
            ModuleItemId::ImplAlias(id) => vec![db.impl_alias_resolver_data(id)],
            ModuleItemId::Trait(_) => vec![],
            ModuleItemId::Impl(id) => vec![db.impl_def_resolver_data(id)],
            ModuleItemId::ExternType(_) => vec![],
            ModuleItemId::ExternFunction(id) => {
                vec![db.extern_function_declaration_resolver_data(id)]
            }
        },
        LookupItemId::ImplFunction(id) => vec![db.impl_function_resolver_data(id)],
    }
    .into_iter()
    .flatten()
    .collect()
}
