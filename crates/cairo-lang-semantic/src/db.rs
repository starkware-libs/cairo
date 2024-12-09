use std::collections::BTreeMap;
use std::sync::Arc;

use cairo_lang_defs::db::{DefsGroup, DefsGroupEx};
use cairo_lang_defs::diagnostic_utils::StableLocation;
use cairo_lang_defs::ids::{
    ConstantId, EnumId, ExternFunctionId, ExternTypeId, FreeFunctionId, FunctionTitleId,
    FunctionWithBodyId, GenericParamId, GenericTypeId, GlobalUseId, ImplAliasId, ImplConstantDefId,
    ImplDefId, ImplFunctionId, ImplImplDefId, ImplItemId, ImplTypeDefId,
    InlineMacroExprPluginLongId, LanguageElementId, LookupItemId, MacroPluginLongId, ModuleFileId,
    ModuleId, ModuleItemId, ModuleTypeAliasId, StructId, TraitConstantId, TraitFunctionId, TraitId,
    TraitImplId, TraitItemId, TraitTypeId, UseId, VariantId,
};
use cairo_lang_diagnostics::{Diagnostics, DiagnosticsBuilder, Maybe};
use cairo_lang_filesystem::db::{AsFilesGroupMut, FilesGroup};
use cairo_lang_filesystem::ids::{CrateId, FileId, FileLongId};
use cairo_lang_parser::db::ParserGroup;
use cairo_lang_syntax::attribute::structured::Attribute;
use cairo_lang_syntax::node::{TypedStablePtr, ast};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use cairo_lang_utils::{LookupIntern, Upcast, require};
use smol_str::SmolStr;

use crate::corelib::CoreInfo;
use crate::diagnostic::SemanticDiagnosticKind;
use crate::expr::inference::{self, ImplVar, ImplVarId};
use crate::ids::{AnalyzerPluginId, AnalyzerPluginLongId};
use crate::items::constant::{ConstCalcInfo, ConstValueId, Constant, ImplConstantId};
use crate::items::function_with_body::FunctionBody;
use crate::items::functions::{GenericFunctionId, ImplicitPrecedence, InlineConfiguration};
use crate::items::generics::{GenericParam, GenericParamData, GenericParamsData};
use crate::items::imp::{
    ImplId, ImplImplId, ImplItemInfo, ImplLookupContext, ImplicitImplImplData, UninferredImpl,
};
use crate::items::module::{ModuleItemInfo, ModuleSemanticData};
use crate::items::trt::{
    ConcreteTraitGenericFunctionId, ConcreteTraitId, TraitItemConstantData, TraitItemImplData,
    TraitItemInfo, TraitItemTypeData,
};
use crate::items::us::{ImportedModules, SemanticUseEx};
use crate::items::visibility::Visibility;
use crate::plugin::{AnalyzerPlugin, InternedPluginSuite, PluginSuite};
use crate::resolve::{ResolvedConcreteItem, ResolvedGenericItem, ResolverData};
use crate::substitution::GenericSubstitution;
use crate::types::{ImplTypeById, ImplTypeId, TypeSizeInformation};
use crate::{
    FunctionId, Parameter, SemanticDiagnostic, TypeId, corelib, items, lsp_helpers, semantic, types,
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
    + Upcast<dyn ParserGroup>
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
    fn intern_concrete_trait_type(
        &self,
        id: items::trt::ConcreteTraitTypeLongId,
    ) -> items::trt::ConcreteTraitTypeId;
    #[salsa::interned]
    fn intern_concrete_trait_constant(
        &self,
        id: items::trt::ConcreteTraitConstantLongId,
    ) -> items::trt::ConcreteTraitConstantId;
    #[salsa::interned]
    fn intern_concrete_impl(
        &self,
        id: items::imp::ConcreteImplLongId,
    ) -> items::imp::ConcreteImplId;
    #[salsa::interned]
    fn intern_concrete_trait_impl(
        &self,
        id: items::trt::ConcreteTraitImplLongId,
    ) -> items::trt::ConcreteTraitImplId;
    #[salsa::interned]
    fn intern_type(&self, id: types::TypeLongId) -> semantic::TypeId;
    #[salsa::interned]
    fn intern_const_value(&self, id: items::constant::ConstValue) -> items::constant::ConstValueId;
    #[salsa::interned]
    fn intern_impl(&self, id: items::imp::ImplLongId) -> items::imp::ImplId;
    #[salsa::interned]
    fn intern_impl_var(&self, id: ImplVar) -> ImplVarId;

    #[salsa::interned]
    fn intern_generated_impl(
        &self,
        id: items::imp::GeneratedImplLongId,
    ) -> items::imp::GeneratedImplId;

    #[salsa::interned]
    fn intern_uninferred_generated_impl(
        &self,
        id: items::imp::UninferredGeneratedImplLongId,
    ) -> items::imp::UninferredGeneratedImplId;

    // Const.
    // ====
    /// Private query to compute data about a constant definition.
    #[salsa::invoke(items::constant::priv_constant_semantic_data)]
    #[salsa::cycle(items::constant::priv_constant_semantic_data_cycle)]
    fn priv_constant_semantic_data(
        &self,
        const_id: ConstantId,
        in_cycle: bool,
    ) -> Maybe<items::constant::ConstantData>;
    /// Returns the semantic diagnostics of a constant definition.
    #[salsa::invoke(items::constant::constant_semantic_diagnostics)]
    fn constant_semantic_diagnostics(
        &self,
        const_id: ConstantId,
    ) -> Diagnostics<SemanticDiagnostic>;
    /// Returns the semantic data of a constant definition.
    #[salsa::invoke(items::constant::constant_semantic_data)]
    #[salsa::cycle(items::constant::constant_semantic_data_cycle)]
    fn constant_semantic_data(&self, use_id: ConstantId) -> Maybe<Constant>;
    #[salsa::invoke(items::constant::constant_resolver_data)]
    #[salsa::cycle(items::constant::constant_resolver_data_cycle)]
    fn constant_resolver_data(&self, use_id: ConstantId) -> Maybe<Arc<ResolverData>>;
    #[salsa::invoke(items::constant::constant_const_value)]
    #[salsa::cycle(items::constant::constant_const_value_cycle)]
    fn constant_const_value(&self, const_id: ConstantId) -> Maybe<ConstValueId>;
    #[salsa::invoke(items::constant::constant_const_type)]
    #[salsa::cycle(items::constant::constant_const_type_cycle)]
    fn constant_const_type(&self, const_id: ConstantId) -> Maybe<TypeId>;
    /// Returns information required for const calculations.
    #[salsa::invoke(items::constant::const_calc_info)]
    fn const_calc_info(&self) -> Arc<ConstCalcInfo>;

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
    #[salsa::cycle(items::us::use_resolver_data_cycle)]
    fn use_resolver_data(&self, use_id: UseId) -> Maybe<Arc<ResolverData>>;

    // Global Use.
    // ====
    /// Private query to compute data about a global use.
    #[salsa::invoke(items::us::priv_global_use_semantic_data)]
    #[salsa::cycle(items::us::priv_global_use_semantic_data_cycle)]
    fn priv_global_use_semantic_data(
        &self,
        global_use_id: GlobalUseId,
    ) -> Maybe<items::us::UseGlobalData>;
    /// Private query to compute the imported module, given a global use.
    #[salsa::invoke(items::us::priv_global_use_imported_module)]
    fn priv_global_use_imported_module(&self, global_use_id: GlobalUseId) -> Maybe<ModuleId>;
    /// Returns the semantic diagnostics of a global use.
    #[salsa::invoke(items::us::global_use_semantic_diagnostics)]
    fn global_use_semantic_diagnostics(
        &self,
        global_use_id: GlobalUseId,
    ) -> Diagnostics<SemanticDiagnostic>;
    /// Private query to compute the imported modules of a module, using global uses.
    #[salsa::invoke(items::us::priv_module_use_star_modules)]
    fn priv_module_use_star_modules(&self, module_id: ModuleId) -> Arc<ImportedModules>;

    // Module.
    // ====

    /// Private query to compute data about the module.
    #[salsa::invoke(items::module::priv_module_semantic_data)]
    fn priv_module_semantic_data(&self, module_id: ModuleId) -> Maybe<Arc<ModuleSemanticData>>;

    /// Returns [Maybe::Err] if the module was not properly resolved.
    /// Returns [Maybe::Ok(None)] if the item does not exist.
    #[salsa::invoke(items::module::module_item_by_name)]
    fn module_item_by_name(
        &self,
        module_id: ModuleId,
        name: SmolStr,
    ) -> Maybe<Option<ModuleItemId>>;

    /// Returns [Maybe::Err] if the module was not properly resolved.
    /// Returns [Maybe::Ok(None)] if the item does not exist.
    #[salsa::invoke(items::module::module_item_info_by_name)]
    fn module_item_info_by_name(
        &self,
        module_id: ModuleId,
        name: SmolStr,
    ) -> Maybe<Option<ModuleItemInfo>>;

    /// Returns all the items used within the module.
    #[salsa::invoke(items::module::module_all_used_items)]
    fn module_all_used_items(
        &self,
        module_id: ModuleId,
    ) -> Maybe<Arc<OrderedHashSet<LookupItemId>>>;

    /// Returns the attributes of a module.
    #[salsa::invoke(items::module::module_attributes)]
    fn module_attributes(&self, module_id: ModuleId) -> Maybe<Vec<Attribute>>;

    /// Finds all the trait ids usable in the module.
    #[salsa::invoke(items::module::module_usable_trait_ids)]
    fn module_usable_trait_ids(
        &self,
        module_id: ModuleId,
    ) -> Maybe<Arc<OrderedHashMap<TraitId, LookupItemId>>>;

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
    /// Returns the generic parameters data of an enum.
    #[salsa::invoke(items::structure::struct_generic_params_data)]
    fn struct_generic_params_data(&self, struct_id: StructId) -> Maybe<GenericParamsData>;
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
    ) -> Maybe<Arc<OrderedHashMap<SmolStr, semantic::Member>>>;
    /// Returns the resolution resolved_items of a struct definition.
    #[salsa::invoke(items::structure::struct_definition_resolver_data)]
    fn struct_definition_resolver_data(&self, structure_id: StructId) -> Maybe<Arc<ResolverData>>;
    /// Returns the concrete members of a struct.
    #[salsa::invoke(items::structure::concrete_struct_members)]
    fn concrete_struct_members(
        &self,
        concrete_struct_id: types::ConcreteStructId,
    ) -> Maybe<Arc<OrderedHashMap<SmolStr, semantic::Member>>>;

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
    /// Returns the generic parameters data of an enum.
    #[salsa::invoke(items::enm::enum_generic_params_data)]
    fn enum_generic_params_data(&self, enum_id: EnumId) -> Maybe<GenericParamsData>;
    /// Returns the attributes attached to an enum.
    #[salsa::invoke(items::enm::enum_attributes)]
    fn enum_attributes(&self, enum_id: EnumId) -> Maybe<Vec<Attribute>>;
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
    // ===========
    /// Returns the semantic diagnostics of a type alias.
    #[salsa::invoke(items::module_type_alias::module_type_alias_semantic_diagnostics)]
    fn module_type_alias_semantic_diagnostics(
        &self,
        module_type_alias_id: ModuleTypeAliasId,
    ) -> Diagnostics<SemanticDiagnostic>;
    /// Returns the resolved type of a type alias.
    #[salsa::invoke(items::module_type_alias::module_type_alias_resolved_type)]
    #[salsa::cycle(items::module_type_alias::module_type_alias_resolved_type_cycle)]
    fn module_type_alias_resolved_type(
        &self,
        module_type_alias_id: ModuleTypeAliasId,
    ) -> Maybe<TypeId>;
    /// Returns the generic parameters of a type alias.
    #[salsa::invoke(items::module_type_alias::module_type_alias_generic_params)]
    fn module_type_alias_generic_params(
        &self,
        enum_id: ModuleTypeAliasId,
    ) -> Maybe<Vec<GenericParam>>;
    /// Returns the resolution resolved_items of a type alias.
    #[salsa::invoke(items::module_type_alias::module_type_alias_resolver_data)]
    #[salsa::cycle(items::module_type_alias::module_type_alias_resolver_data_cycle)]
    fn module_type_alias_resolver_data(
        &self,
        module_type_alias_id: ModuleTypeAliasId,
    ) -> Maybe<Arc<ResolverData>>;
    /// Private query to compute the generic parameters data of a type alias.
    #[salsa::invoke(items::module_type_alias::priv_module_type_alias_generic_params_data)]
    fn priv_module_type_alias_generic_params_data(
        &self,
        enum_id: ModuleTypeAliasId,
    ) -> Maybe<GenericParamsData>;
    /// Private query to compute data about a type alias.
    #[salsa::invoke(items::module_type_alias::priv_module_type_alias_semantic_data)]
    #[salsa::cycle(items::module_type_alias::priv_module_type_alias_semantic_data_cycle)]
    fn priv_module_type_alias_semantic_data(
        &self,
        module_type_alias_id: ModuleTypeAliasId,
        in_cycle: bool,
    ) -> Maybe<items::module_type_alias::ModuleTypeAliasData>;

    // Impl Alias.
    // ====
    /// Returns the impl definition pointed to by the impl alias, or an error if it points to
    /// something else.
    #[salsa::invoke(items::impl_alias::impl_alias_impl_def)]
    #[salsa::cycle(items::impl_alias::impl_alias_impl_def_cycle)]
    fn impl_alias_impl_def(&self, impl_alias_id: ImplAliasId) -> Maybe<ImplDefId>;
    /// Private query to compute data about a type alias.
    #[salsa::invoke(items::impl_alias::priv_impl_alias_semantic_data)]
    #[salsa::cycle(items::impl_alias::priv_impl_alias_semantic_data_cycle)]
    fn priv_impl_alias_semantic_data(
        &self,
        impl_alias_id: ImplAliasId,
        in_cycle: bool,
    ) -> Maybe<items::impl_alias::ImplAliasData>;
    /// Returns the semantic diagnostics of a type alias.
    #[salsa::invoke(items::impl_alias::impl_alias_semantic_diagnostics)]
    fn impl_alias_semantic_diagnostics(
        &self,
        impl_alias_id: ImplAliasId,
    ) -> Diagnostics<SemanticDiagnostic>;
    /// Returns the resolved type of a type alias.
    #[salsa::invoke(items::impl_alias::impl_alias_resolved_impl)]
    #[salsa::cycle(items::impl_alias::impl_alias_resolved_impl_cycle)]
    fn impl_alias_resolved_impl(&self, impl_alias_id: ImplAliasId) -> Maybe<ImplId>;
    /// Returns the generic parameters of a type alias.
    #[salsa::invoke(items::impl_alias::impl_alias_generic_params)]
    fn impl_alias_generic_params(&self, impl_alias_id: ImplAliasId) -> Maybe<Vec<GenericParam>>;
    /// Returns the generic parameters data of a type alias.
    #[salsa::invoke(items::impl_alias::impl_alias_generic_params_data)]
    fn impl_alias_generic_params_data(
        &self,
        impl_alias_id: ImplAliasId,
    ) -> Maybe<GenericParamsData>;
    /// Returns the resolution resolved_items of a type alias.
    #[salsa::invoke(items::impl_alias::impl_alias_resolver_data)]
    #[salsa::cycle(items::impl_alias::impl_alias_resolver_data_cycle)]
    fn impl_alias_resolver_data(&self, impl_alias_id: ImplAliasId) -> Maybe<Arc<ResolverData>>;
    /// Returns the attributes attached to the impl alias.
    #[salsa::invoke(items::impl_alias::impl_alias_attributes)]
    fn impl_alias_attributes(&self, impl_def_id: ImplAliasId) -> Maybe<Vec<Attribute>>;

    // Trait.
    // =======
    /// Returns the semantic declaration diagnostics of a trait.
    #[salsa::invoke(items::trt::trait_semantic_declaration_diagnostics)]
    fn trait_semantic_declaration_diagnostics(
        &self,
        trait_id: TraitId,
    ) -> Diagnostics<SemanticDiagnostic>;
    /// Returns the generic parameters of a trait.
    #[salsa::invoke(items::trt::trait_generic_params)]
    #[salsa::cycle(items::trt::trait_generic_params_cycle)]
    fn trait_generic_params(&self, trait_id: TraitId) -> Maybe<Vec<GenericParam>>;
    /// Returns the generic parameters data of a trait.
    #[salsa::invoke(items::trt::trait_generic_params_data)]
    #[salsa::cycle(items::trt::trait_generic_params_data_cycle)]
    fn trait_generic_params_data(
        &self,
        trait_id: TraitId,
        in_cycle: bool,
    ) -> Maybe<GenericParamsData>;
    /// Returns the attributes of a trait.
    #[salsa::invoke(items::trt::trait_attributes)]
    fn trait_attributes(&self, trait_id: TraitId) -> Maybe<Vec<Attribute>>;
    /// Returns the resolution resolved_items of a trait.
    #[salsa::invoke(items::trt::trait_resolver_data)]
    fn trait_resolver_data(&self, trait_id: TraitId) -> Maybe<Arc<ResolverData>>;
    /// Private query to compute declaration data about a trait.
    #[salsa::invoke(items::trt::priv_trait_declaration_data)]
    fn priv_trait_declaration_data(
        &self,
        trait_id: TraitId,
    ) -> Maybe<items::trt::TraitDeclarationData>;

    /// Returns the semantic definition diagnostics of a trait.
    #[salsa::invoke(items::trt::trait_semantic_definition_diagnostics)]
    fn trait_semantic_definition_diagnostics(
        &self,
        trait_id: TraitId,
    ) -> Diagnostics<SemanticDiagnostic>;
    /// Returns the names of all the non default implemented items of a trait.
    #[salsa::invoke(items::trt::trait_required_item_names)]
    fn trait_required_item_names(&self, trait_id: TraitId) -> Maybe<OrderedHashSet<SmolStr>>;
    /// Returns the item of the trait, by the given `name`, if exists.
    #[salsa::invoke(items::trt::trait_item_by_name)]
    fn trait_item_by_name(&self, trait_id: TraitId, name: SmolStr) -> Maybe<Option<TraitItemId>>;
    /// Returns the metadata for a trait item, by the given `name`, if exists.
    #[salsa::invoke(items::trt::trait_item_info_by_name)]
    fn trait_item_info_by_name(
        &self,
        trait_id: TraitId,
        name: SmolStr,
    ) -> Maybe<Option<TraitItemInfo>>;
    /// Returns all the items used within the trait.
    #[salsa::invoke(items::trt::trait_all_used_items)]
    fn trait_all_used_items(&self, trait_id: TraitId) -> Maybe<Arc<OrderedHashSet<LookupItemId>>>;
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
    /// Returns the types of a trait.
    #[salsa::invoke(items::trt::trait_types)]
    fn trait_types(&self, trait_id: TraitId) -> Maybe<OrderedHashMap<SmolStr, TraitTypeId>>;
    /// Returns the item type with the given name of the given trait, if exists.
    #[salsa::invoke(items::trt::trait_type_by_name)]
    fn trait_type_by_name(&self, trait_id: TraitId, name: SmolStr) -> Maybe<Option<TraitTypeId>>;

    /// Returns the constants of a trait.
    #[salsa::invoke(items::trt::trait_constants)]
    fn trait_constants(&self, trait_id: TraitId)
    -> Maybe<OrderedHashMap<SmolStr, TraitConstantId>>;
    /// Returns the item constants with the given name of the given trait, if exists.
    #[salsa::invoke(items::trt::trait_constant_by_name)]
    fn trait_constant_by_name(
        &self,
        trait_id: TraitId,
        name: SmolStr,
    ) -> Maybe<Option<TraitConstantId>>;
    /// Returns the constants of a trait.
    #[salsa::invoke(items::trt::trait_impls)]
    fn trait_impls(&self, trait_id: TraitId) -> Maybe<OrderedHashMap<SmolStr, TraitImplId>>;
    /// Returns the item impls with the given name of the given trait, if exists.
    #[salsa::invoke(items::trt::trait_impl_by_name)]
    fn trait_impl_by_name(&self, trait_id: TraitId, name: SmolStr) -> Maybe<Option<TraitImplId>>;

    /// Private query to compute definition data about a trait.
    #[salsa::invoke(items::trt::priv_trait_definition_data)]
    fn priv_trait_definition_data(
        &self,
        trait_id: TraitId,
    ) -> Maybe<items::trt::TraitDefinitionData>;

    // Trait type.
    // ================
    /// Returns the semantic diagnostics of a trait type.
    #[salsa::invoke(items::trt::trait_type_diagnostics)]
    fn trait_type_diagnostics(&self, trait_type_id: TraitTypeId)
    -> Diagnostics<SemanticDiagnostic>;
    /// Returns the generic params of a trait type.
    #[salsa::invoke(items::trt::trait_type_generic_params)]
    fn trait_type_generic_params(&self, trait_type_id: TraitTypeId) -> Maybe<Vec<GenericParam>>;
    /// Returns the attributes of a trait type.
    #[salsa::invoke(items::trt::trait_type_attributes)]
    fn trait_type_attributes(&self, trait_type_id: TraitTypeId) -> Maybe<Vec<Attribute>>;
    /// Returns the resolution resolved_items of a trait type.
    #[salsa::invoke(items::trt::trait_type_resolver_data)]
    fn trait_type_resolver_data(&self, trait_type_id: TraitTypeId) -> Maybe<Arc<ResolverData>>;
    /// Private query to compute the generic params data of a trait type.
    #[salsa::invoke(items::trt::priv_trait_type_generic_params_data)]
    fn priv_trait_type_generic_params_data(
        &self,
        trait_type_id: TraitTypeId,
    ) -> Maybe<GenericParamsData>;
    /// Private query to compute data about a trait type.
    #[salsa::invoke(items::trt::priv_trait_type_data)]
    fn priv_trait_type_data(&self, type_id: TraitTypeId) -> Maybe<TraitItemTypeData>;

    // Trait constants.
    // ================
    /// Returns the semantic diagnostics of a trait type.
    #[salsa::invoke(items::trt::trait_constant_diagnostics)]
    fn trait_constant_diagnostics(
        &self,
        trait_constant: TraitConstantId,
    ) -> Diagnostics<SemanticDiagnostic>;
    /// Returns the attributes of a trait constants.
    #[salsa::invoke(items::trt::trait_constant_attributes)]
    fn trait_constant_attributes(&self, trait_constant: TraitConstantId) -> Maybe<Vec<Attribute>>;
    /// Returns the type of a trait constant.
    #[salsa::invoke(items::trt::trait_constant_type)]
    fn trait_constant_type(&self, trait_type_id: TraitConstantId) -> Maybe<TypeId>;
    /// Returns the resolution resolved_items of a trait constants.
    #[salsa::invoke(items::trt::trait_constant_resolver_data)]
    fn trait_constant_resolver_data(
        &self,
        trait_constant: TraitConstantId,
    ) -> Maybe<Arc<ResolverData>>;
    /// Private query to compute data about a trait constant.
    #[salsa::invoke(items::trt::priv_trait_constant_data)]
    fn priv_trait_constant_data(
        &self,
        trait_constant: TraitConstantId,
    ) -> Maybe<TraitItemConstantData>;
    /// Returns the type of a trait constant.
    #[salsa::invoke(items::trt::concrete_trait_constant_type)]
    fn concrete_trait_constant_type(
        &self,
        concrete_trait_constant_id: items::trt::ConcreteTraitConstantId,
    ) -> Maybe<TypeId>;

    // Trait impls.
    // ================
    /// Returns the semantic diagnostics of a trait impls.
    #[salsa::invoke(items::trt::trait_impl_diagnostics)]
    fn trait_impl_diagnostics(&self, trait_impl: TraitImplId) -> Diagnostics<SemanticDiagnostic>;
    /// Returns the attributes of a trait impls.
    #[salsa::invoke(items::trt::trait_impl_attributes)]
    fn trait_impl_attributes(&self, trait_impl: TraitImplId) -> Maybe<Vec<Attribute>>;
    /// Returns the concrete trait of a trait impl.
    #[salsa::invoke(items::trt::trait_impl_concrete_trait)]
    fn trait_impl_concrete_trait(&self, trait_impl_id: TraitImplId) -> Maybe<ConcreteTraitId>;
    /// Returns the resolution resolved_items of a trait impls.
    #[salsa::invoke(items::trt::trait_impl_resolver_data)]
    fn trait_impl_resolver_data(&self, trait_impl: TraitImplId) -> Maybe<Arc<ResolverData>>;
    /// Private query to compute data about a trait impl.
    #[salsa::invoke(items::trt::priv_trait_impl_data)]
    fn priv_trait_impl_data(&self, trait_impl: TraitImplId) -> Maybe<TraitItemImplData>;
    /// Returns the concrete trait of a concrete trait impl.
    #[salsa::invoke(items::trt::concrete_trait_impl_concrete_trait)]
    fn concrete_trait_impl_concrete_trait(
        &self,
        concrete_trait_impl_id: items::trt::ConcreteTraitImplId,
    ) -> Maybe<ConcreteTraitId>;

    // Trait function.
    // ================
    /// Returns the semantic diagnostics of a trait function.
    #[salsa::invoke(items::trt::trait_function_declaration_diagnostics)]
    fn trait_function_declaration_diagnostics(
        &self,
        trait_function_id: TraitFunctionId,
    ) -> Diagnostics<SemanticDiagnostic>;
    /// Returns the signature of a trait function.
    #[salsa::invoke(items::trt::trait_function_signature)]
    fn trait_function_signature(
        &self,
        trait_function_id: TraitFunctionId,
    ) -> Maybe<semantic::Signature>;
    /// Returns the generic params of a trait function.
    #[salsa::invoke(items::trt::trait_function_generic_params)]
    fn trait_function_generic_params(
        &self,
        trait_function_id: TraitFunctionId,
    ) -> Maybe<Vec<GenericParam>>;
    /// Returns the generic params data of a trait function.
    #[salsa::invoke(items::trt::priv_trait_function_generic_params_data)]
    fn priv_trait_function_generic_params_data(
        &self,
        trait_function_id: TraitFunctionId,
    ) -> Maybe<GenericParamsData>;
    /// Returns the attributes of a trait function.
    #[salsa::invoke(items::trt::trait_function_attributes)]
    fn trait_function_attributes(
        &self,
        trait_function_id: TraitFunctionId,
    ) -> Maybe<Vec<Attribute>>;
    /// Returns the resolution resolved_items of a trait function.
    #[salsa::invoke(items::trt::trait_function_resolver_data)]
    fn trait_function_resolver_data(
        &self,
        trait_function_id: TraitFunctionId,
    ) -> Maybe<Arc<ResolverData>>;
    /// Returns the inline configuration of a trait function's declaration.
    #[salsa::invoke(items::trt::trait_function_declaration_inline_config)]
    fn trait_function_declaration_inline_config(
        &self,
        trait_function_id: TraitFunctionId,
    ) -> Maybe<InlineConfiguration>;
    /// Returns the implicits precedence of a trait function.
    #[salsa::invoke(items::trt::trait_function_declaration_implicit_precedence)]
    fn trait_function_declaration_implicit_precedence(
        &self,
        trait_function_id: TraitFunctionId,
    ) -> Maybe<ImplicitPrecedence>;
    /// Returns the explicit implicits of a signature of a trait function.
    #[salsa::invoke(items::trt::trait_function_declaration_implicits)]
    fn trait_function_declaration_implicits(
        &self,
        trait_function_id: TraitFunctionId,
    ) -> Maybe<Vec<TypeId>>;
    /// Private query to compute data about a trait function declaration.
    #[salsa::invoke(items::trt::priv_trait_function_declaration_data)]
    fn priv_trait_function_declaration_data(
        &self,
        function_id: TraitFunctionId,
    ) -> Maybe<items::functions::FunctionDeclarationData>;

    /// Returns the semantic diagnostics of a trait function definition (declaration + body).
    #[salsa::invoke(items::trt::trait_function_body_diagnostics)]
    fn trait_function_body_diagnostics(
        &self,
        trait_function_id: TraitFunctionId,
    ) -> Diagnostics<SemanticDiagnostic>;
    /// Returns the body of a trait function, if any.
    #[salsa::invoke(items::trt::trait_function_body)]
    fn trait_function_body(
        &self,
        trait_function_id: TraitFunctionId,
    ) -> Maybe<Option<Arc<FunctionBody>>>;
    /// Private query to compute data about a trait function definition (declaration + body)
    #[salsa::invoke(items::trt::priv_trait_function_body_data)]
    fn priv_trait_function_body_data(
        &self,
        trait_function_id: TraitFunctionId,
    ) -> Maybe<Option<items::function_with_body::FunctionBodyData>>;

    // Concrete Trait function.
    // ========================
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

    // Trait filter.
    // ==============
    /// Returns candidate [ImplDefId]s for a specific trait lookup constraint.
    #[salsa::invoke(items::imp::module_impl_ids_for_trait_filter)]
    #[salsa::cycle(items::imp::module_impl_ids_for_trait_filter_cycle)]
    fn module_impl_ids_for_trait_filter(
        &self,
        module_id: ModuleId,
        trait_lookup_constraint: items::imp::TraitFilter,
    ) -> Maybe<Vec<UninferredImpl>>;
    #[salsa::invoke(items::imp::impl_impl_ids_for_trait_filter)]
    #[salsa::cycle(items::imp::impl_impl_ids_for_trait_filter_cycle)]
    fn impl_impl_ids_for_trait_filter(
        &self,
        impl_id: ImplId,
        trait_lookup_constraint: items::imp::TraitFilter,
    ) -> Maybe<Vec<UninferredImpl>>;
    // Returns the solution set for a canonical trait.
    #[salsa::invoke(inference::solver::canonic_trait_solutions)]
    #[salsa::cycle(inference::solver::canonic_trait_solutions_cycle)]
    fn canonic_trait_solutions(
        &self,
        canonical_trait: inference::canonic::CanonicalTrait,
        lookup_context: ImplLookupContext,
        impl_type_bounds: BTreeMap<ImplTypeById, TypeId>,
    ) -> Result<
        inference::solver::SolutionSet<inference::canonic::CanonicalImpl>,
        inference::InferenceError,
    >;

    // Impl.
    // =======
    /// Returns the semantic declaration diagnostics of an impl.
    #[salsa::invoke(items::imp::impl_semantic_declaration_diagnostics)]
    fn impl_semantic_declaration_diagnostics(
        &self,
        impl_def_id: ImplDefId,
    ) -> Diagnostics<SemanticDiagnostic>;
    /// Returns the generic parameters data of an impl.
    #[salsa::invoke(items::imp::impl_def_generic_params_data)]
    fn impl_def_generic_params_data(&self, impl_def_id: ImplDefId) -> Maybe<GenericParamsData>;
    /// Returns the generic parameters of an impl.
    #[salsa::invoke(items::imp::impl_def_generic_params)]
    fn impl_def_generic_params(&self, impl_def_id: ImplDefId) -> Maybe<Vec<GenericParam>>;
    /// Returns the resolution resolved_items of an impl.
    #[salsa::invoke(items::imp::impl_def_resolver_data)]
    #[salsa::cycle(items::imp::impl_def_resolver_data_cycle)]
    fn impl_def_resolver_data(&self, impl_def_id: ImplDefId) -> Maybe<Arc<ResolverData>>;
    /// Returns the concrete trait that is implemented by the impl.
    #[salsa::invoke(items::imp::impl_def_concrete_trait)]
    #[salsa::cycle(items::imp::impl_def_concrete_trait_cycle)]
    fn impl_def_concrete_trait(&self, impl_def_id: ImplDefId) -> Maybe<ConcreteTraitId>;
    /// Returns the substitution for generics for the impl.
    #[salsa::invoke(items::imp::impl_def_substitution)]
    fn impl_def_substitution(&self, impl_def_id: ImplDefId) -> Maybe<Arc<GenericSubstitution>>;
    /// Returns the attributes attached to the impl.
    #[salsa::invoke(items::imp::impl_def_attributes)]
    fn impl_def_attributes(&self, impl_def_id: ImplDefId) -> Maybe<Vec<Attribute>>;
    /// Returns the concrete trait that is implemented by the concrete impl.
    #[salsa::invoke(items::imp::impl_concrete_trait)]
    fn impl_concrete_trait(&self, impl_id: ImplId) -> Maybe<ConcreteTraitId>;
    /// Returns the trait that is implemented by the impl, or an error if the RHS of the `of` is not
    /// a trait.
    #[salsa::invoke(items::imp::impl_def_trait)]
    fn impl_def_trait(&self, impl_def_id: ImplDefId) -> Maybe<TraitId>;
    /// Private query to compute declaration data about an impl.
    #[salsa::invoke(items::imp::priv_impl_declaration_data)]
    #[salsa::cycle(items::imp::priv_impl_declaration_data_cycle)]
    fn priv_impl_declaration_data(
        &self,
        impl_def_id: ImplDefId,
    ) -> Maybe<items::imp::ImplDeclarationData>;

    /// Returns the semantic definition diagnostics of an impl.
    #[salsa::invoke(items::imp::impl_semantic_definition_diagnostics)]
    fn impl_semantic_definition_diagnostics(
        &self,
        impl_def_id: ImplDefId,
    ) -> Diagnostics<SemanticDiagnostic>;
    /// Returns the item of the impl, by the given `name`, if exists.
    #[salsa::invoke(items::imp::impl_item_by_name)]
    fn impl_item_by_name(&self, impl_def_id: ImplDefId, name: SmolStr)
    -> Maybe<Option<ImplItemId>>;
    /// Returns the metadata for an impl item, by the given `name`, if exists.
    #[salsa::invoke(items::imp::impl_item_info_by_name)]
    fn impl_item_info_by_name(
        &self,
        impl_def_id: ImplDefId,
        name: SmolStr,
    ) -> Maybe<Option<ImplItemInfo>>;
    /// Returns the trait impl of an implicit impl if `name` exists in trait and not in the impl.
    #[salsa::invoke(items::imp::impl_implicit_impl_by_name)]
    fn impl_implicit_impl_by_name(
        &self,
        impl_def_id: ImplDefId,
        name: SmolStr,
    ) -> Maybe<Option<TraitImplId>>;
    /// Returns all the items used within the impl.
    #[salsa::invoke(items::imp::impl_all_used_items)]
    fn impl_all_used_items(
        &self,
        impl_def_id: ImplDefId,
    ) -> Maybe<Arc<OrderedHashSet<LookupItemId>>>;
    /// Returns the type items in the impl.
    #[salsa::invoke(items::imp::impl_types)]
    fn impl_types(
        &self,
        impl_def_id: ImplDefId,
    ) -> Maybe<Arc<OrderedHashMap<ImplTypeDefId, ast::ItemTypeAlias>>>;
    /// Returns the ids of the type items in the impl.
    #[salsa::invoke(items::imp::impl_type_ids)]
    fn impl_type_ids(&self, impl_def_id: ImplDefId) -> Maybe<Arc<[ImplTypeDefId]>>;
    /// Returns the impl AST of the impl type that matches the given id, if exists.
    #[salsa::invoke(items::imp::impl_type_by_id)]
    fn impl_type_by_id(&self, impl_type_id: ImplTypeDefId) -> Maybe<Option<ast::ItemTypeAlias>>;
    /// Returns the impl type item that matches the given trait type item, if exists.
    #[salsa::invoke(items::imp::impl_type_by_trait_type)]
    fn impl_type_by_trait_type(
        &self,
        impl_def_id: ImplDefId,
        trait_type_id: TraitTypeId,
    ) -> Maybe<ImplTypeDefId>;

    /// Returns the constant items in the impl.
    #[salsa::invoke(items::imp::impl_constants)]
    fn impl_constants(
        &self,
        impl_def_id: ImplDefId,
    ) -> Maybe<Arc<OrderedHashMap<ImplConstantDefId, ast::ItemConstant>>>;

    /// Returns the impls items in the impl.
    #[salsa::invoke(items::imp::impl_impls)]
    fn impl_impls(
        &self,
        impl_def_id: ImplDefId,
    ) -> Maybe<Arc<OrderedHashMap<ImplImplDefId, ast::ItemImplAlias>>>;
    /// Returns the ids of the impl items in the impl.
    #[salsa::invoke(items::imp::impl_impl_ids)]
    fn impl_impl_ids(&self, impl_def_id: ImplDefId) -> Maybe<Arc<[ImplImplDefId]>>;
    /// Returns the impl AST of the impl impl that matches the given id, if exists.
    #[salsa::invoke(items::imp::impl_impl_by_id)]
    fn impl_impl_by_id(&self, impl_impl_id: ImplImplDefId) -> Maybe<Option<ast::ItemImplAlias>>;
    /// Returns the impl impl item that matches the given trait impl item, if exists.
    #[salsa::invoke(items::imp::impl_impl_by_trait_impl)]
    fn impl_impl_by_trait_impl(
        &self,
        impl_def_id: ImplDefId,
        trait_impl_id: TraitImplId,
    ) -> Maybe<ImplImplDefId>;
    /// Returns whether `trait_impl_id` is an implicit impl in `impl_def_id`.
    #[salsa::invoke(items::imp::is_implicit_impl_impl)]
    fn is_implicit_impl_impl(
        &self,
        impl_def_id: ImplDefId,
        trait_impl_id: TraitImplId,
    ) -> Maybe<bool>;

    /// Returns the impl constant item that matches the given trait constant item, if exists.
    #[salsa::invoke(items::imp::impl_constant_by_trait_constant)]
    fn impl_constant_by_trait_constant(
        &self,
        impl_def_id: ImplDefId,
        trait_constant_id: TraitConstantId,
    ) -> Maybe<ImplConstantDefId>;

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
    /// Private query to compute definition data about an impl.
    #[salsa::invoke(items::imp::priv_impl_definition_data)]
    fn priv_impl_definition_data(
        &self,
        impl_def_id: ImplDefId,
    ) -> Maybe<items::imp::ImplDefinitionData>;

    /// Private query to check if an impl is fully concrete.
    #[salsa::invoke(items::imp::priv_impl_is_fully_concrete)]
    fn priv_impl_is_fully_concrete(&self, impl_id: ImplId) -> bool;

    /// Private query to check if an impl contains no variables.
    #[salsa::invoke(items::imp::priv_impl_is_var_free)]
    fn priv_impl_is_var_free(&self, impl_id: ImplId) -> bool;

    // Impl type def.
    // ================
    /// Returns the semantic diagnostics of an impl item type.
    #[salsa::invoke(items::imp::impl_type_def_semantic_diagnostics)]
    fn impl_type_def_semantic_diagnostics(
        &self,
        impl_type_def_id: ImplTypeDefId,
    ) -> Diagnostics<SemanticDiagnostic>;
    /// Returns the resolved type of an impl item type.
    #[salsa::invoke(items::imp::impl_type_def_resolved_type)]
    #[salsa::cycle(items::imp::impl_type_def_resolved_type_cycle)]
    fn impl_type_def_resolved_type(&self, impl_type_def_id: ImplTypeDefId) -> Maybe<TypeId>;
    /// Returns the generic parameters of an impl item type.
    #[salsa::invoke(items::imp::impl_type_def_generic_params)]
    fn impl_type_def_generic_params(
        &self,
        impl_type_def_id: ImplTypeDefId,
    ) -> Maybe<Vec<GenericParam>>;
    /// Returns the attributes of an impl type.
    #[salsa::invoke(items::imp::impl_type_def_attributes)]
    fn impl_type_def_attributes(&self, impl_type_def_id: ImplTypeDefId) -> Maybe<Vec<Attribute>>;
    /// Returns the resolution resolved_items of an impl item type.
    #[salsa::invoke(items::imp::impl_type_def_resolver_data)]
    fn impl_type_def_resolver_data(
        &self,
        impl_type_def_id: ImplTypeDefId,
    ) -> Maybe<Arc<ResolverData>>;
    /// Returns the trait type of an impl type.
    #[salsa::invoke(items::imp::impl_type_def_trait_type)]
    fn impl_type_def_trait_type(&self, impl_type_def_id: ImplTypeDefId) -> Maybe<TraitTypeId>;

    /// Private query to compute data about an impl item type.
    #[salsa::invoke(items::imp::priv_impl_type_semantic_data)]
    #[salsa::cycle(items::imp::priv_impl_type_semantic_data_cycle)]
    fn priv_impl_type_semantic_data(
        &self,
        impl_type_def_id: ImplTypeDefId,
        in_cycle: bool,
    ) -> Maybe<items::imp::ImplItemTypeData>;
    /// Private query to compute data about the generic parameters of an impl item type.
    #[salsa::invoke(items::imp::priv_impl_type_def_generic_params_data)]
    fn priv_impl_type_def_generic_params_data(
        &self,
        impl_type_def_id: ImplTypeDefId,
    ) -> Maybe<GenericParamsData>;

    /// Returns the deref chain and diagnostics for a given type.
    #[salsa::invoke(items::imp::deref_chain)]
    #[salsa::cycle(items::imp::deref_chain_cycle)]
    fn deref_chain(&self, ty: TypeId, try_deref_mut: bool) -> Maybe<items::imp::DerefChain>;

    // Impl type.
    // ================
    /// Returns the implized impl type if the impl is concrete. Returns a TypeId that's not an impl
    /// type with a concrete impl.
    // TODO(Gil): Consider removing the cycle handling here if we will upgrade the salsa version.
    #[salsa::invoke(items::imp::impl_type_concrete_implized)]
    #[salsa::cycle(items::imp::impl_type_concrete_implized_cycle)]
    fn impl_type_concrete_implized(&self, impl_type_def_id: ImplTypeId) -> Maybe<TypeId>;

    // Impl constant def.
    // ================
    /// Returns the semantic diagnostics of an impl item constant.
    #[salsa::invoke(items::imp::impl_constant_def_semantic_diagnostics)]
    fn impl_constant_def_semantic_diagnostics(
        &self,
        impl_constant_def_id: ImplConstantDefId,
    ) -> Diagnostics<SemanticDiagnostic>;
    /// Returns the resolved constant value of an impl item constant.
    #[salsa::invoke(items::imp::impl_constant_def_value)]
    #[salsa::cycle(items::imp::impl_constant_def_value_cycle)]
    fn impl_constant_def_value(
        &self,
        impl_constant_def_id: ImplConstantDefId,
    ) -> Maybe<ConstValueId>;
    /// Returns the resolution resolved_items of an impl item constant.
    #[salsa::invoke(items::imp::impl_constant_def_resolver_data)]
    fn impl_constant_def_resolver_data(
        &self,
        impl_constant_def_id: ImplConstantDefId,
    ) -> Maybe<Arc<ResolverData>>;
    /// Returns the type of an impl item constant.
    #[salsa::invoke(items::imp::impl_constant_def_trait_constant)]
    fn impl_constant_def_trait_constant(
        &self,
        impl_constant_def_id: ImplConstantDefId,
    ) -> Maybe<TraitConstantId>;

    /// Private query to compute data about an impl item constant.
    #[salsa::invoke(items::imp::priv_impl_constant_semantic_data)]
    #[salsa::cycle(items::imp::priv_impl_constant_semantic_data_cycle)]
    fn priv_impl_constant_semantic_data(
        &self,
        impl_constant_def_id: ImplConstantDefId,
        in_cycle: bool,
    ) -> Maybe<items::imp::ImplItemConstantData>;

    // Impl constant.
    // ================
    /// Returns the given impl constant, implized by the given impl context.
    #[salsa::invoke(items::imp::impl_constant_implized_by_context)]
    #[salsa::cycle(items::imp::impl_constant_implized_by_context_cycle)]
    fn impl_constant_implized_by_context(
        &self,
        impl_constant_id: ImplConstantId,
        impl_def_id: ImplDefId,
    ) -> Maybe<ConstValueId>;
    /// Returns the implized impl constant value if the impl is concrete.
    // TODO(Gil): Consider removing the cycle handling here if we will upgrade the salsa version.
    #[salsa::invoke(items::imp::impl_constant_concrete_implized_value)]
    #[salsa::cycle(items::imp::impl_constant_concrete_implized_value_cycle)]
    fn impl_constant_concrete_implized_value(
        &self,
        impl_constant_id: ImplConstantId,
    ) -> Maybe<ConstValueId>;
    /// Returns the implized impl constant type if the impl is concrete.
    // TODO(Gil): Consider removing the cycle handling here if we will upgrade the salsa version.
    #[salsa::invoke(items::imp::impl_constant_concrete_implized_type)]
    #[salsa::cycle(items::imp::impl_constant_concrete_implized_type_cycle)]
    fn impl_constant_concrete_implized_type(
        &self,
        impl_constant_id: ImplConstantId,
    ) -> Maybe<TypeId>;

    // Impl impl def.
    // ================
    /// Returns the semantic diagnostics of an impl item impl.
    #[salsa::invoke(items::imp::impl_impl_def_semantic_diagnostics)]
    fn impl_impl_def_semantic_diagnostics(
        &self,
        impl_impl_def_id: ImplImplDefId,
    ) -> Diagnostics<SemanticDiagnostic>;
    /// Returns the resolution resolved_items of an impl item impl.
    #[salsa::invoke(items::imp::impl_impl_def_resolver_data)]
    fn impl_impl_def_resolver_data(
        &self,
        impl_impl_def_id: ImplImplDefId,
    ) -> Maybe<Arc<ResolverData>>;
    /// Returns the type of an impl item impl.
    #[salsa::invoke(items::imp::impl_impl_def_trait_impl)]
    fn impl_impl_def_trait_impl(&self, impl_impl_def_id: ImplImplDefId) -> Maybe<TraitImplId>;

    /// Returns the resolved impl of an impl item impl.
    #[salsa::invoke(items::imp::impl_impl_def_impl)]
    #[salsa::cycle(items::imp::impl_impl_def_impl_cycle)]
    fn impl_impl_def_impl(&self, impl_impl_def_id: ImplImplDefId, in_cycle: bool) -> Maybe<ImplId>;

    /// Private query to compute data about an impl item impl.
    #[salsa::invoke(items::imp::priv_impl_impl_semantic_data)]
    #[salsa::cycle(items::imp::priv_impl_impl_semantic_data_cycle)]
    fn priv_impl_impl_semantic_data(
        &self,
        impl_impl_def_id: ImplImplDefId,
        in_cycle: bool,
    ) -> Maybe<items::imp::ImplItemImplData>;

    /// Private query to compute data about the generic parameters of an impl item impl.
    #[salsa::invoke(items::imp::priv_impl_impl_def_generic_params_data)]
    fn priv_impl_impl_def_generic_params_data(
        &self,
        impl_impl_def_id: ImplImplDefId,
    ) -> Maybe<GenericParamsData>;

    /// Returns the semantic diagnostics of an implicit impl.
    #[salsa::invoke(items::imp::implicit_impl_impl_semantic_diagnostics)]
    fn implicit_impl_impl_semantic_diagnostics(
        &self,
        impl_def_id: ImplDefId,
        trait_impl_id: TraitImplId,
    ) -> Diagnostics<SemanticDiagnostic>;

    /// Returns the resolved impl of an implicit impl.
    #[salsa::invoke(items::imp::implicit_impl_impl_impl)]
    #[salsa::cycle(items::imp::implicit_impl_impl_impl_cycle)]
    fn implicit_impl_impl_impl(
        &self,
        impl_def_id: ImplDefId,
        trait_impl_id: TraitImplId,
        in_cycle: bool,
    ) -> Maybe<ImplId>;
    // Private query to compute data about an implicit impl.
    #[salsa::invoke(items::imp::priv_implicit_impl_impl_semantic_data)]
    #[salsa::cycle(items::imp::priv_implicit_impl_impl_semantic_data_cycle)]
    fn priv_implicit_impl_impl_semantic_data(
        &self,
        impl_def_id: ImplDefId,
        trait_impl_id: TraitImplId,
        in_cycle: bool,
    ) -> Maybe<ImplicitImplImplData>;

    // Impl impl.
    // ================
    /// Returns the implized impl impl if the impl is concrete.
    #[salsa::invoke(items::imp::impl_impl_implized_by_context)]
    #[salsa::cycle(items::imp::impl_impl_implized_by_context_cycle)]
    fn impl_impl_implized_by_context(
        &self,
        impl_impl_id: ImplImplId,
        impl_def_id: ImplDefId,
        in_cycle: bool,
    ) -> Maybe<ImplId>;
    /// Returns the implized impl impl value if the impl is concrete.
    #[salsa::invoke(items::imp::impl_impl_concrete_implized)]
    #[salsa::cycle(items::imp::impl_impl_concrete_implized_cycle)]
    fn impl_impl_concrete_implized(&self, impl_impl_id: ImplImplId) -> Maybe<ImplId>;
    /// Returns the concrete trait of an impl impl.
    #[salsa::invoke(items::imp::impl_impl_concrete_trait)]
    fn impl_impl_concrete_trait(&self, impl_impl_id: ImplImplId) -> Maybe<ConcreteTraitId>;

    // Impl function.
    // ================
    /// Returns the semantic diagnostics of an impl function's declaration (signature).
    #[salsa::invoke(items::imp::impl_function_declaration_diagnostics)]
    fn impl_function_declaration_diagnostics(
        &self,
        impl_function_id: ImplFunctionId,
    ) -> Diagnostics<SemanticDiagnostic>;
    /// Returns the signature of an impl function.
    #[salsa::invoke(items::imp::impl_function_signature)]
    fn impl_function_signature(
        &self,
        impl_function_id: ImplFunctionId,
    ) -> Maybe<semantic::Signature>;
    /// Returns the generic params of an impl function.
    #[salsa::invoke(items::imp::impl_function_generic_params)]
    fn impl_function_generic_params(
        &self,
        impl_function_id: ImplFunctionId,
    ) -> Maybe<Vec<GenericParam>>;
    /// Returns the generic params data of an impl function.
    #[salsa::invoke(items::imp::priv_impl_function_generic_params_data)]
    fn priv_impl_function_generic_params_data(
        &self,
        impl_function_id: ImplFunctionId,
    ) -> Maybe<GenericParamsData>;
    /// Returns the attributes of an impl function.
    #[salsa::invoke(items::imp::impl_function_attributes)]
    fn impl_function_attributes(&self, impl_function_id: ImplFunctionId) -> Maybe<Vec<Attribute>>;
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
    /// Returns the implicits precedence of an impl function.
    #[salsa::invoke(items::imp::impl_function_declaration_implicit_precedence)]
    fn impl_function_declaration_implicit_precedence(
        &self,
        impl_function_id: ImplFunctionId,
    ) -> Maybe<ImplicitPrecedence>;
    /// Returns the explicit implicits of a signature of an impl function.
    #[salsa::invoke(items::imp::impl_function_declaration_implicits)]
    fn impl_function_declaration_implicits(
        &self,
        impl_function_id: ImplFunctionId,
    ) -> Maybe<Vec<TypeId>>;
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

    // Implizations.
    // ==============
    /// Returns the impl type for the given trait type, by implization by the given impl context, if
    /// the impl matches the trait of the trait type.
    // TODO(Gil): Consider removing the cycle handling here if we will upgrade the salsa version.
    #[salsa::invoke(items::implization::trait_type_implized_by_context)]
    #[salsa::cycle(items::implization::trait_type_implized_by_context_cycle)]
    fn trait_type_implized_by_context(
        &self,
        trait_type_def_id: TraitTypeId,
        impl_def_id: ImplDefId,
    ) -> Maybe<TypeId>;

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
    /// Returns the implicits precedence of a free function.
    #[salsa::invoke(items::free_function::free_function_declaration_implicit_precedence)]
    fn free_function_declaration_implicit_precedence(
        &self,
        free_function_id: FreeFunctionId,
    ) -> Maybe<ImplicitPrecedence>;
    /// Returns the generic params of a free function.
    #[salsa::invoke(items::free_function::free_function_generic_params)]
    fn free_function_generic_params(
        &self,
        free_function_id: FreeFunctionId,
    ) -> Maybe<Vec<GenericParam>>;
    /// Returns the generic params data of a free function.
    #[salsa::invoke(items::free_function::free_function_generic_params_data)]
    fn free_function_generic_params_data(
        &self,
        free_function_id: FreeFunctionId,
    ) -> Maybe<GenericParamsData>;
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
    /// Returns the implicit order of a declaration (signature) of a function with a body.
    #[salsa::invoke(items::function_with_body::function_declaration_implicit_precedence)]
    fn function_declaration_implicit_precedence(
        &self,
        function_id: FunctionWithBodyId,
    ) -> Maybe<ImplicitPrecedence>;
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
    /// Returns the generic params data of an extern function.
    #[salsa::invoke(items::extern_function::extern_function_declaration_generic_params_data)]
    fn extern_function_declaration_generic_params_data(
        &self,
        extern_function_id: ExternFunctionId,
    ) -> Maybe<GenericParamsData>;
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
    /// Returns the generic params data of an extern type.
    #[salsa::invoke(items::extern_type::extern_type_declaration_generic_params_data)]
    fn extern_type_declaration_generic_params_data(
        &self,
        extern_type_id: ExternTypeId,
    ) -> Maybe<GenericParamsData>;

    /// Returns the attributes of an extern type.
    #[salsa::invoke(items::extern_type::extern_type_attributes)]
    fn extern_type_attributes(&self, extern_type_id: ExternTypeId) -> Maybe<Vec<Attribute>>;

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

    /// Returns a mapping of closure types to their associated parameter types for a concrete
    /// function.
    #[salsa::invoke(items::functions::concrete_function_closure_params)]
    fn concrete_function_closure_params(
        &self,
        function_id: FunctionId,
    ) -> Maybe<OrderedHashMap<semantic::TypeId, semantic::TypeId>>;

    /// Returns a mapping of closure types to their associated parameter types for a generic
    /// function.
    #[salsa::invoke(items::functions::get_closure_params)]
    fn get_closure_params(
        &self,
        generic_function_id: GenericFunctionId,
    ) -> Maybe<OrderedHashMap<TypeId, TypeId>>;

    // Generic type.
    // =============
    /// Returns the generic params of a generic type.
    #[salsa::invoke(types::generic_type_generic_params)]
    fn generic_type_generic_params(&self, generic_type: GenericTypeId) -> Maybe<Vec<GenericParam>>;

    // Generic param.
    // ==============
    /// Returns the semantic data of a generic param.
    #[salsa::invoke(items::generics::generic_param_semantic)]
    fn generic_param_semantic(&self, generic_param: GenericParamId) -> Maybe<GenericParam>;
    /// Returns the semantic diagnostics of a generic param.
    #[salsa::invoke(items::generics::generic_param_diagnostics)]
    fn generic_param_diagnostics(
        &self,
        generic_param: GenericParamId,
    ) -> Diagnostics<SemanticDiagnostic>;
    /// Returns the resolver data of a generic param.
    #[salsa::invoke(items::generics::generic_param_resolver_data)]
    fn generic_param_resolver_data(
        &self,
        generic_param: GenericParamId,
    ) -> Maybe<Arc<ResolverData>>;
    /// Returns the trait a generic param impl should implement.
    /// Panics if the generic param is not an impl generic param.
    #[salsa::invoke(items::generics::generic_impl_param_trait)]
    fn generic_impl_param_trait(&self, generic_param_id: GenericParamId) -> Maybe<TraitId>;
    /// Private query to compute data about a generic param.
    #[salsa::invoke(items::generics::priv_generic_param_data)]
    #[salsa::cycle(items::generics::priv_generic_param_data_cycle)]
    fn priv_generic_param_data(
        &self,
        generic_param: GenericParamId,
        in_cycle: bool,
    ) -> Maybe<GenericParamData>;

    /// Returns the type constraints intoduced by the generic params.
    #[salsa::invoke(items::generics::generic_params_type_constraints)]
    fn generic_params_type_constraints(
        &self,
        generic_params: Vec<GenericParamId>,
    ) -> Vec<(TypeId, TypeId)>;

    // Concrete type.
    // ==============
    /// Returns true if there is only one value for the given type and hence the values of the given
    /// type are all interchangeable.
    /// Examples include the unit type tuple of a unit type and empty structs.
    /// Always returns false for extern types.
    #[salsa::invoke(types::single_value_type)]
    fn single_value_type(&self, ty: types::TypeId) -> Maybe<bool>;

    /// Returns the type size information for the given type.
    #[salsa::invoke(types::type_size_info)]
    #[salsa::cycle(types::type_size_info_cycle)]
    fn type_size_info(&self, ty: types::TypeId) -> Maybe<TypeSizeInformation>;

    /// Returns the generic_type of a generic function. This include free types, extern
    /// types, etc...
    #[salsa::invoke(types::type_info)]
    fn type_info(
        &self,
        lookup_context: ImplLookupContext,
        ty: types::TypeId,
    ) -> Maybe<types::TypeInfo>;

    /// Private query to check if a type is fully concrete.
    #[salsa::invoke(types::priv_type_is_fully_concrete)]
    fn priv_type_is_fully_concrete(&self, ty: types::TypeId) -> bool;

    /// Private query to check if a type contains no variables.
    #[salsa::invoke(types::priv_type_is_var_free)]
    fn priv_type_is_var_free(&self, ty: types::TypeId) -> bool;

    /// Private query for a shorter unique name for types.
    #[salsa::invoke(types::priv_type_short_name)]
    fn priv_type_short_name(&self, ty: types::TypeId) -> String;

    // Expression.
    // ===========
    /// Assumes function and expression are present.
    #[salsa::invoke(items::function_with_body::expr_semantic)]
    fn expr_semantic(
        &self,
        function_id: FunctionWithBodyId,
        id: semantic::ExprId,
    ) -> semantic::Expr;
    /// Assumes function and pattern are present.
    #[salsa::invoke(items::function_with_body::pattern_semantic)]
    fn pattern_semantic(
        &self,
        function_id: FunctionWithBodyId,
        id: semantic::PatternId,
    ) -> semantic::Pattern;
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
    #[salsa::invoke(corelib::core_info)]
    fn core_info(&self) -> Arc<CoreInfo>;

    // Analyzer plugins.
    // ========

    #[salsa::input]
    fn default_analyzer_plugins(&self) -> Arc<[AnalyzerPluginId]>;

    #[salsa::input]
    fn analyzer_plugin_overrides(&self) -> Arc<OrderedHashMap<CrateId, Arc<[AnalyzerPluginId]>>>;

    #[salsa::interned]
    fn intern_analyzer_plugin(&self, plugin: AnalyzerPluginLongId) -> AnalyzerPluginId;

    /// Returns [`AnalyzerPluginId`]s of the plugins set for the crate with [`CrateId`].
    /// Returns
    /// [`SemanticGroupEx::set_override_crate_analyzer_plugins`] if it has been set,
    /// or the ([`SemanticGroup::default_analyzer_plugins`]) otherwise.
    fn crate_analyzer_plugins(&self, crate_id: CrateId) -> Arc<[AnalyzerPluginId]>;

    /// Returns the set of `allow` that were declared as by a plugin.
    /// An allow that is not in this set will be handled as an unknown allow.
    fn declared_allows(&self, crate_id: CrateId) -> Arc<OrderedHashSet<String>>;

    // Helpers for language server.
    // ============================
    /// Returns all methods in a module that match the given type filter.
    #[salsa::invoke(lsp_helpers::methods_in_module)]
    fn methods_in_module(
        &self,
        module_id: ModuleId,
        type_filter: lsp_helpers::TypeFilter,
    ) -> Arc<[TraitFunctionId]>;
    /// Returns all methods in a crate that match the given type filter.
    #[salsa::invoke(lsp_helpers::methods_in_crate)]
    fn methods_in_crate(
        &self,
        crate_id: CrateId,
        type_filter: lsp_helpers::TypeFilter,
    ) -> Arc<[TraitFunctionId]>;
    /// Returns all the traits visible from a module, alongside a visible use path to the trait.
    #[salsa::invoke(lsp_helpers::visible_traits_from_module)]
    fn visible_traits_from_module(
        &self,
        module_id: ModuleFileId,
    ) -> Option<Arc<OrderedHashMap<TraitId, String>>>;
    /// Returns all visible traits in a module, alongside a visible use path to the trait.
    /// `user_module_file_id` is the module from which the traits are should be visible. If
    /// `include_parent` is true, the parent module of `module_id` is also considered.
    #[salsa::invoke(lsp_helpers::visible_traits_in_module)]
    fn visible_traits_in_module(
        &self,
        module_id: ModuleId,
        user_module_file_id: ModuleFileId,
        include_parent: bool,
    ) -> Arc<[(TraitId, String)]>;
    /// Returns all visible traits in a crate, alongside a visible use path to the trait.
    /// `user_module_file_id` is the module from which the traits are should be visible.
    #[salsa::invoke(lsp_helpers::visible_traits_in_crate)]
    fn visible_traits_in_crate(
        &self,
        crate_id: CrateId,
        user_module_file_id: ModuleFileId,
    ) -> Arc<[(TraitId, String)]>;
}

/// Initializes the [`SemanticGroup`] database to a proper state.
pub fn init_semantic_group(db: &mut dyn SemanticGroup) {
    db.set_analyzer_plugin_overrides(Arc::new(OrderedHashMap::default()));
}

impl<T: Upcast<dyn SemanticGroup + 'static>> Elongate for T {
    fn elongate(&self) -> &(dyn SemanticGroup + 'static) {
        self.upcast()
    }
}

fn module_semantic_diagnostics(
    db: &dyn SemanticGroup,
    module_id: ModuleId,
) -> Maybe<Diagnostics<SemanticDiagnostic>> {
    let mut diagnostics = DiagnosticsBuilder::default();
    for (_module_file_id, plugin_diag) in db.module_plugin_diagnostics(module_id)?.iter().cloned() {
        diagnostics.add(SemanticDiagnostic::new(
            StableLocation::new(plugin_diag.stable_ptr),
            SemanticDiagnosticKind::PluginDiagnostic(plugin_diag),
        ));
    }
    let data = db.priv_module_semantic_data(module_id)?;
    diagnostics.extend(data.diagnostics.clone());
    // TODO(Gil): Aggregate diagnostics for subitems with semantic model (i.e. impl function, trait
    // functions and generic params) directly and not via the parent item.
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
                diagnostics.extend(db.trait_semantic_declaration_diagnostics(*trait_id));
                diagnostics.extend(db.trait_semantic_definition_diagnostics(*trait_id));
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

                        let path = match file_id.lookup_intern(db) {
                            FileLongId::OnDisk(path) => path.display().to_string(),
                            FileLongId::Virtual(_) | FileLongId::External(_) => {
                                panic!("Expected OnDisk file.")
                            }
                        };

                        let stable_location =
                            StableLocation::new(submodule_id.stable_ptr(db.upcast()).untyped());
                        diagnostics.add(SemanticDiagnostic::new(
                            stable_location,
                            SemanticDiagnosticKind::ModuleFileNotFound(path),
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
                diagnostics.extend(db.module_type_alias_semantic_diagnostics(*type_alias));
            }
            ModuleItemId::ImplAlias(type_alias) => {
                diagnostics.extend(db.impl_alias_semantic_diagnostics(*type_alias));
            }
        }
    }
    for global_use in db.module_global_uses(module_id)?.keys() {
        diagnostics.extend(db.global_use_semantic_diagnostics(*global_use));
    }
    add_unused_item_diagnostics(db, module_id, &data, &mut diagnostics);
    for analyzer_plugin_id in db.crate_analyzer_plugins(module_id.owning_crate(db.upcast())).iter()
    {
        let analyzer_plugin = db.lookup_intern_analyzer_plugin(*analyzer_plugin_id);

        for diag in analyzer_plugin.diagnostics(db, module_id) {
            diagnostics.add(SemanticDiagnostic::new(
                StableLocation::new(diag.stable_ptr),
                SemanticDiagnosticKind::PluginDiagnostic(diag),
            ));
        }
    }

    Ok(diagnostics.build())
}

fn crate_analyzer_plugins(db: &dyn SemanticGroup, crate_id: CrateId) -> Arc<[AnalyzerPluginId]> {
    db.analyzer_plugin_overrides()
        .get(&crate_id)
        .cloned()
        .unwrap_or_else(|| db.default_analyzer_plugins())
}

fn declared_allows(db: &dyn SemanticGroup, crate_id: CrateId) -> Arc<OrderedHashSet<String>> {
    Arc::new(OrderedHashSet::from_iter(
        db.crate_analyzer_plugins(crate_id)
            .iter()
            .flat_map(|plugin| db.lookup_intern_analyzer_plugin(*plugin).declared_allows()),
    ))
}

/// Adds diagnostics for unused items in a module.
///
/// Returns `None` if skipped attempt to add diagnostics.
fn add_unused_item_diagnostics(
    db: &dyn SemanticGroup,
    module_id: ModuleId,
    data: &ModuleSemanticData,
    diagnostics: &mut DiagnosticsBuilder<SemanticDiagnostic>,
) {
    let Ok(all_used_items) = db.module_all_used_items(module_id) else {
        return;
    };
    for info in data.items.values() {
        if info.visibility == Visibility::Public {
            continue;
        }
        if let ModuleItemId::Use(use_id) = info.item_id {
            add_unused_import_diagnostics(db, &all_used_items, use_id, diagnostics);
        };
    }
}

/// Adds diagnostics for unused imports.
fn add_unused_import_diagnostics(
    db: &dyn SemanticGroup,
    all_used_items: &OrderedHashSet<LookupItemId>,
    use_id: UseId,
    diagnostics: &mut DiagnosticsBuilder<SemanticDiagnostic>,
) {
    let _iife = (|| {
        let item = db.use_resolved_item(use_id).ok()?;
        // TODO(orizi): Properly handle usages of impls, and than add warnings on their usages as
        // well.
        require(!matches!(
            item,
            ResolvedGenericItem::Impl(_) | ResolvedGenericItem::GenericImplAlias(_)
        ))?;
        require(!all_used_items.contains(&LookupItemId::ModuleItem(ModuleItemId::Use(use_id))))?;
        let resolver_data = db.use_resolver_data(use_id).ok()?;
        require(!resolver_data.feature_config.allow_unused_imports)?;
        Some(diagnostics.add(SemanticDiagnostic::new(
            StableLocation::new(use_id.untyped_stable_ptr(db.upcast())),
            SemanticDiagnosticKind::UnusedImport(use_id),
        )))
    })();
}

fn file_semantic_diagnostics(
    db: &dyn SemanticGroup,
    file_id: FileId,
) -> Maybe<Diagnostics<SemanticDiagnostic>> {
    let mut diagnostics = DiagnosticsBuilder::default();
    for module_id in db.file_modules(file_id)?.iter().copied() {
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
    get_resolver_data_options(id, db)
        .into_iter()
        .find_map(|resolver_data| resolver_data.resolved_items.generic.get(&ptr).cloned())
}

pub fn lookup_resolved_concrete_item_by_ptr(
    db: &dyn SemanticGroup,
    id: LookupItemId,
    ptr: ast::TerminalIdentifierPtr,
) -> Option<ResolvedConcreteItem> {
    get_resolver_data_options(id, db)
        .into_iter()
        .find_map(|resolver_data| resolver_data.resolved_items.concrete.get(&ptr).cloned())
}

pub fn get_resolver_data_options(
    id: LookupItemId,
    db: &dyn SemanticGroup,
) -> Vec<Arc<ResolverData>> {
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
            ModuleItemId::TypeAlias(id) => vec![db.module_type_alias_resolver_data(id)],
            ModuleItemId::ImplAlias(id) => vec![db.impl_alias_resolver_data(id)],
            ModuleItemId::Trait(_) => vec![],
            ModuleItemId::Impl(id) => vec![db.impl_def_resolver_data(id)],
            ModuleItemId::ExternType(_) => vec![],
            ModuleItemId::ExternFunction(id) => {
                vec![db.extern_function_declaration_resolver_data(id)]
            }
        },
        LookupItemId::TraitItem(id) => match id {
            cairo_lang_defs::ids::TraitItemId::Function(id) => {
                let mut res = vec![db.trait_function_resolver_data(id)];
                if let Ok(Some(body)) = db.priv_trait_function_body_data(id) {
                    res.push(Ok(body.resolver_data));
                }
                res
            }
            cairo_lang_defs::ids::TraitItemId::Type(id) => vec![db.trait_type_resolver_data(id)],
            cairo_lang_defs::ids::TraitItemId::Constant(id) => {
                vec![db.trait_constant_resolver_data(id)]
            }
            cairo_lang_defs::ids::TraitItemId::Impl(id) => vec![db.trait_impl_resolver_data(id)],
        },
        LookupItemId::ImplItem(id) => match id {
            cairo_lang_defs::ids::ImplItemId::Function(id) => {
                vec![db.impl_function_resolver_data(id), db.impl_function_body_resolver_data(id)]
            }
            cairo_lang_defs::ids::ImplItemId::Type(id) => vec![db.impl_type_def_resolver_data(id)],
            cairo_lang_defs::ids::ImplItemId::Constant(id) => {
                vec![db.impl_constant_def_resolver_data(id)]
            }
            cairo_lang_defs::ids::ImplItemId::Impl(id) => vec![db.impl_impl_def_resolver_data(id)],
        },
    }
    .into_iter()
    .flatten()
    .collect()
}

pub trait SemanticGroupEx: SemanticGroup {
    /// Overrides the default analyzer plugins available for [`CrateId`] with `plugins`.
    ///
    /// *Note*: Sets the following Salsa input: `SemanticGroup::analyzer_plugin_overrides`.
    fn set_override_crate_analyzer_plugins(
        &mut self,
        crate_id: CrateId,
        plugins: Arc<[AnalyzerPluginId]>,
    ) {
        let mut overrides = self.analyzer_plugin_overrides().as_ref().clone();
        overrides.insert(crate_id, plugins);
        self.set_analyzer_plugin_overrides(Arc::new(overrides));
    }
}

impl<T: SemanticGroup + ?Sized> SemanticGroupEx for T {}

/// An extension trait for [`SemanticGroup`] to manage plugin setters.
pub trait PluginSuiteInput: SemanticGroup {
    /// Interns each plugin from the [`PluginSuite`] into the database.
    fn intern_plugin_suite(&mut self, suite: PluginSuite) -> InternedPluginSuite {
        let PluginSuite { plugins, inline_macro_plugins, analyzer_plugins } = suite;

        let macro_plugins = plugins
            .into_iter()
            .map(|plugin| self.intern_macro_plugin(MacroPluginLongId(plugin)))
            .collect::<Arc<[_]>>();

        let inline_macro_plugins = Arc::new(
            inline_macro_plugins
                .into_iter()
                .map(|(name, plugin)| {
                    (name, self.intern_inline_macro_plugin(InlineMacroExprPluginLongId(plugin)))
                })
                .collect::<OrderedHashMap<_, _>>(),
        );

        let analyzer_plugins = analyzer_plugins
            .into_iter()
            .map(|plugin| self.intern_analyzer_plugin(AnalyzerPluginLongId(plugin)))
            .collect::<Arc<[_]>>();

        InternedPluginSuite { macro_plugins, inline_macro_plugins, analyzer_plugins }
    }

    /// Sets macro, inline macro and analyzer plugins specified in the [`PluginSuite`] as default
    /// for all crates.
    ///
    /// *Note*: Sets the following Salsa inputs: [`DefsGroup::default_macro_plugins`],
    /// [`DefsGroup::default_inline_macro_plugins`], and
    /// [`SemanticGroup::default_analyzer_plugins`].
    fn set_default_plugins_from_suite(&mut self, suite: InternedPluginSuite) {
        let InternedPluginSuite { macro_plugins, inline_macro_plugins, analyzer_plugins } = suite;

        self.set_default_macro_plugins(macro_plugins);
        self.set_default_inline_macro_plugins(inline_macro_plugins);
        self.set_default_analyzer_plugins(analyzer_plugins);
    }

    /// Sets macro, inline macro and analyzer plugins present in the [`PluginSuite`] for a crate
    /// pointed to by the [`CrateId`], overriding the defaults for that crate.
    ///
    /// *Note*: Sets the following Salsa inputs: [`DefsGroup::macro_plugin_overrides`],
    /// [`DefsGroup::inline_macro_plugin_overrides`], and
    /// [`SemanticGroup::analyzer_plugin_overrides`].
    fn set_override_crate_plugins_from_suite(
        &mut self,
        crate_id: CrateId,
        suite: InternedPluginSuite,
    ) {
        let InternedPluginSuite { macro_plugins, inline_macro_plugins, analyzer_plugins } = suite;

        self.set_override_crate_macro_plugins(crate_id, macro_plugins);
        self.set_override_crate_inline_macro_plugins(crate_id, inline_macro_plugins);
        self.set_override_crate_analyzer_plugins(crate_id, analyzer_plugins);
    }
}

impl<T: SemanticGroup + ?Sized> PluginSuiteInput for T {}
