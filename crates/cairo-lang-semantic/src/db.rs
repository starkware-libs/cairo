use std::collections::{BTreeMap, BTreeSet};
use std::sync::Arc;

use cairo_lang_defs::db::{DefsGroup, DefsGroupEx, defs_group_input};
use cairo_lang_defs::diagnostic_utils::StableLocation;
use cairo_lang_defs::ids::{
    ConstantId, EnumId, ExternFunctionId, ExternTypeId, FreeFunctionId, FunctionTitleId,
    FunctionWithBodyId, GenericParamId, GenericTypeId, GlobalUseId, ImplAliasId, ImplConstantDefId,
    ImplDefId, ImplFunctionId, ImplImplDefId, ImplItemId, ImplTypeDefId, ImportableId,
    InlineMacroExprPluginId, InlineMacroExprPluginLongId, LanguageElementId, LookupItemId,
    MacroCallId, MacroDeclarationId, MacroPluginId, MacroPluginLongId, ModuleFileId, ModuleId,
    ModuleItemId, ModuleTypeAliasId, StructId, TraitConstantId, TraitFunctionId, TraitId,
    TraitImplId, TraitItemId, TraitTypeId, UseId, VariantId,
};
use cairo_lang_diagnostics::{Diagnostics, DiagnosticsBuilder, Maybe};
use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_filesystem::ids::{CrateId, CrateInput, FileId, FileLongId, StrRef, Tracked};
use cairo_lang_syntax::attribute::consts::{DEPRECATED_ATTR, UNUSED_IMPORTS, UNUSED_VARIABLES};
use cairo_lang_syntax::attribute::structured::Attribute;
use cairo_lang_syntax::node::{TypedStablePtr, ast};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use cairo_lang_utils::{Intern, Upcast, require};
use itertools::{Itertools, chain};
use salsa::{Database, Setter};

use crate::corelib::CoreInfo;
use crate::diagnostic::SemanticDiagnosticKind;
use crate::expr::inference::{self, InferenceError};
use crate::ids::{AnalyzerPluginId, AnalyzerPluginLongId};
use crate::items::constant::{ConstCalcInfo, ConstValueId, Constant, ImplConstantId};
use crate::items::function_with_body::FunctionBody;
use crate::items::functions::{GenericFunctionId, ImplicitPrecedence, InlineConfiguration};
use crate::items::generics::{GenericParam, GenericParamData, GenericParamsData};
use crate::items::imp::{
    GenericsHeadFilter, ImplId, ImplImplId, ImplItemInfo, ImplLookupContextId,
    ImplicitImplImplData, ModuleImpls, UninferredImplById,
};
use crate::items::macro_declaration::{MacroDeclarationData, MacroRuleData};
use crate::items::module::{ModuleItemInfo, ModuleSemanticData};
use crate::items::trt::{
    ConcreteTraitGenericFunctionId, ConcreteTraitId, TraitItemConstantData, TraitItemImplData,
    TraitItemInfo, TraitItemTypeData,
};
use crate::items::us::{ImportedModules, SemanticUseEx, UseData};
use crate::items::visibility::Visibility;
use crate::plugin::{AnalyzerPlugin, InternedPluginSuite, PluginSuite};
use crate::resolve::{ResolvedConcreteItem, ResolvedGenericItem, ResolverData};
use crate::substitution::GenericSubstitution;
use crate::types::{ImplTypeById, ImplTypeId, ShallowGenericArg, TypeSizeInformation};
use crate::{
    FunctionId, Parameter, SemanticDiagnostic, TypeId, corelib, items, lsp_helpers, semantic, types,
};

#[salsa::input]
pub struct SemanticGroupInput {
    #[returns(ref)]
    pub default_analyzer_plugins: Option<Vec<AnalyzerPluginLongId>>,
    #[returns(ref)]
    pub analyzer_plugin_overrides: Option<OrderedHashMap<CrateInput, Arc<[AnalyzerPluginLongId]>>>,
}

#[salsa::tracked(returns(ref))]
pub fn semantic_group_input(db: &dyn Database) -> SemanticGroupInput {
    SemanticGroupInput::new(db, None, None)
}

fn default_analyzer_plugins_input(db: &dyn Database) -> &[AnalyzerPluginLongId] {
    semantic_group_input(db).default_analyzer_plugins(db).as_ref().unwrap()
}

fn analyzer_plugin_overrides_input(
    db: &dyn Database,
) -> &OrderedHashMap<CrateInput, Arc<[AnalyzerPluginLongId]>> {
    semantic_group_input(db).analyzer_plugin_overrides(db).as_ref().unwrap()
}

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
#[cairo_lang_proc_macros::query_group]
pub trait SemanticGroup: Database + for<'db> Upcast<'db, dyn salsa::Database> + Elongate {
    // Const.
    // ====
    /// Private query to compute data about a constant definition.
    #[salsa::transparent]
    #[salsa::invoke(items::constant::priv_constant_semantic_data_tracked)]
    fn priv_constant_semantic_data<'db>(
        &'db self,
        _const_id: ConstantId<'db>,
        _in_cycle: bool,
    ) -> Maybe<items::constant::ConstantData<'db>>;
    /// Returns the semantic diagnostics of a constant definition.
    #[salsa::transparent]
    #[salsa::invoke(items::constant::constant_semantic_diagnostics_tracked)]
    fn constant_semantic_diagnostics<'db>(
        &'db self,
        const_id: ConstantId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;
    /// Returns the semantic data of a constant definition.
    #[salsa::transparent]
    #[salsa::invoke(items::constant::constant_semantic_data_tracked)]
    fn constant_semantic_data<'db>(&'db self, use_id: ConstantId<'db>) -> Maybe<Constant<'db>>;
    #[salsa::transparent]
    #[salsa::invoke(items::constant::constant_resolver_data_tracked)]
    fn constant_resolver_data<'db>(
        &'db self,
        use_id: ConstantId<'db>,
    ) -> Maybe<Arc<ResolverData<'db>>>;
    #[salsa::transparent]
    #[salsa::invoke(items::constant::constant_const_value_tracked)]
    fn constant_const_value<'db>(&'db self, const_id: ConstantId<'db>) -> Maybe<ConstValueId<'db>>;
    /// Returns information required for const calculations.
    #[salsa::transparent]
    #[salsa::invoke(items::constant::const_calc_info_tracked)]
    fn const_calc_info<'db>(&'db self) -> Arc<ConstCalcInfo<'db>>;

    // Use.
    // ====
    /// Private query to compute data about a use.
    #[salsa::transparent]
    #[salsa::invoke(items::us::priv_use_semantic_data_tracked)]
    fn priv_use_semantic_data<'db>(&'db self, use_id: UseId<'db>) -> Maybe<Arc<UseData<'db>>>;
    /// Returns the semantic diagnostics of a use.
    #[salsa::transparent]
    #[salsa::invoke(items::us::use_semantic_diagnostics_tracked)]
    fn use_semantic_diagnostics<'db>(
        &'db self,
        use_id: UseId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;
    #[salsa::transparent]
    #[salsa::invoke(items::us::use_resolver_data_tracked)]
    fn use_resolver_data<'db>(&'db self, use_id: UseId<'db>) -> Maybe<Arc<ResolverData<'db>>>;

    // Global Use.
    // ====
    /// Private query to compute data about a global use.
    #[salsa::transparent]
    #[salsa::invoke(items::us::priv_global_use_semantic_data_tracked)]
    fn priv_global_use_semantic_data<'db>(
        &'db self,
        global_use_id: GlobalUseId<'db>,
    ) -> Maybe<items::us::UseGlobalData<'db>>;
    /// Private query to compute the imported module, given a global use.
    #[salsa::transparent]
    #[salsa::invoke(items::us::priv_global_use_imported_module_tracked)]
    fn priv_global_use_imported_module<'db>(
        &'db self,
        global_use_id: GlobalUseId<'db>,
    ) -> Maybe<ModuleId<'db>>;
    /// Returns the semantic diagnostics of a global use.
    #[salsa::transparent]
    #[salsa::invoke(items::us::global_use_semantic_diagnostics_tracked)]
    fn global_use_semantic_diagnostics<'db>(
        &'db self,
        global_use_id: GlobalUseId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;
    /// Computes the imported modules of a module, using global uses and macro calls.
    #[salsa::transparent]
    #[salsa::invoke(items::us::module_imported_modules_tracked)]
    fn module_imported_modules<'db>(
        &'db self,
        module_id: ModuleId<'db>,
    ) -> Arc<ImportedModules<'db>>;

    // Module.
    // ====

    /// Private query to compute data about the module.
    #[salsa::transparent]
    #[salsa::invoke(items::module::priv_module_semantic_data_tracked)]
    fn priv_module_semantic_data<'db>(
        &'db self,
        module_id: ModuleId<'db>,
    ) -> Maybe<Arc<ModuleSemanticData<'db>>>;

    /// Returns [Maybe::Err] if the module was not properly resolved.
    /// Returns [Maybe::Ok(None)] if the item does not exist.
    #[salsa::transparent]
    #[salsa::invoke(items::module::module_item_by_name_tracked)]
    fn module_item_by_name<'db>(
        &'db self,
        module_id: ModuleId<'db>,
        name: StrRef<'db>,
    ) -> Maybe<Option<ModuleItemId<'db>>>;

    /// Returns [Maybe::Err] if the module was not properly resolved.
    /// Returns [Maybe::Ok(None)] if the item does not exist.
    #[salsa::transparent]
    #[salsa::invoke(items::module::module_item_info_by_name_tracked)]
    fn module_item_info_by_name<'db>(
        &'db self,
        module_id: ModuleId<'db>,
        name: StrRef<'db>,
    ) -> Maybe<Option<ModuleItemInfo<'db>>>;

    /// Returns all the items used within the module.
    #[salsa::transparent]
    #[salsa::invoke(items::module::module_all_used_uses_tracked)]
    fn module_all_used_uses<'db>(
        &'db self,
        module_id: ModuleId<'db>,
    ) -> Maybe<Arc<OrderedHashSet<UseId<'db>>>>;

    /// Returns the attributes of a module.
    #[salsa::transparent]
    #[salsa::invoke(items::module::module_attributes_tracked)]
    fn module_attributes<'db>(&'db self, module_id: ModuleId<'db>) -> Maybe<Vec<Attribute<'db>>>;

    /// Finds all the trait ids usable in the module.
    #[salsa::transparent]
    #[salsa::invoke(items::module::module_usable_trait_ids_tracked)]
    fn module_usable_trait_ids<'db>(
        &'db self,
        module_id: ModuleId<'db>,
    ) -> Maybe<Arc<OrderedHashMap<TraitId<'db>, LookupItemId<'db>>>>;

    // Struct.
    // =======
    /// Private query to compute data about a struct declaration.
    #[salsa::transparent]
    #[salsa::invoke(items::structure::priv_struct_declaration_data_tracked)]
    fn priv_struct_declaration_data<'db>(
        &'db self,
        struct_id: StructId<'db>,
    ) -> Maybe<items::structure::StructDeclarationData<'db>>;
    /// Returns the declaration diagnostics of a struct.
    #[salsa::transparent]
    #[salsa::invoke(items::structure::struct_declaration_diagnostics_tracked)]
    fn struct_declaration_diagnostics<'db>(
        &'db self,
        struct_id: StructId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;
    /// Returns the attributes of a struct.
    #[salsa::transparent]
    #[salsa::invoke(items::structure::struct_attributes_tracked)]
    fn struct_attributes<'db>(&'db self, struct_id: StructId<'db>) -> Maybe<Vec<Attribute<'db>>>;
    /// Returns the generic parameters of an enum.
    #[salsa::transparent]
    #[salsa::invoke(items::structure::struct_generic_params_tracked)]
    fn struct_generic_params<'db>(
        &'db self,
        struct_id: StructId<'db>,
    ) -> Maybe<Vec<GenericParam<'db>>>;
    /// Returns the generic parameters data of an enum.
    #[salsa::transparent]
    #[salsa::invoke(items::structure::struct_generic_params_data_tracked)]
    fn struct_generic_params_data<'db>(
        &'db self,
        struct_id: StructId<'db>,
    ) -> Maybe<GenericParamsData<'db>>;
    /// Returns the resolution resolved_items of a struct declaration.
    #[salsa::transparent]
    #[salsa::invoke(items::structure::struct_declaration_resolver_data_tracked)]
    fn struct_declaration_resolver_data<'db>(
        &'db self,
        structure_id: StructId<'db>,
    ) -> Maybe<Arc<ResolverData<'db>>>;

    /// Private query to compute data about a struct definition.
    #[salsa::transparent]
    #[salsa::invoke(items::structure::priv_struct_definition_data_tracked)]
    fn priv_struct_definition_data<'db>(
        &'db self,
        struct_id: StructId<'db>,
    ) -> Maybe<items::structure::StructDefinitionData<'db>>;
    /// Returns the semantic diagnostics of a struct definition.
    #[salsa::transparent]
    #[salsa::invoke(items::structure::struct_definition_diagnostics_tracked)]
    fn struct_definition_diagnostics<'db>(
        &'db self,
        struct_id: StructId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;
    /// Returns the members of a struct.
    #[salsa::transparent]
    #[salsa::invoke(items::structure::struct_members_tracked)]
    fn struct_members<'db>(
        &'db self,
        struct_id: StructId<'db>,
    ) -> Maybe<Arc<OrderedHashMap<StrRef<'db>, semantic::Member<'db>>>>;
    /// Returns the resolution resolved_items of a struct definition.
    #[salsa::transparent]
    #[salsa::invoke(items::structure::struct_definition_resolver_data_tracked)]
    fn struct_definition_resolver_data<'db>(
        &'db self,
        structure_id: StructId<'db>,
    ) -> Maybe<Arc<ResolverData<'db>>>;
    /// Returns the concrete members of a struct.
    #[salsa::transparent]
    #[salsa::invoke(items::structure::concrete_struct_members_tracked)]
    fn concrete_struct_members<'db>(
        &'db self,
        concrete_struct_id: types::ConcreteStructId<'db>,
    ) -> Maybe<Arc<OrderedHashMap<StrRef<'db>, semantic::Member<'db>>>>;

    // Enum.
    // =======
    /// Private query to compute data about an enum declaration.
    #[salsa::transparent]
    #[salsa::invoke(items::enm::priv_enum_declaration_data_tracked)]
    fn priv_enum_declaration_data<'db>(
        &'db self,
        enum_id: EnumId<'db>,
    ) -> Maybe<items::enm::EnumDeclarationData<'db>>;
    /// Returns the diagnostics of an enum declaration.
    #[salsa::transparent]
    #[salsa::invoke(items::enm::enum_declaration_diagnostics_tracked)]
    fn enum_declaration_diagnostics<'db>(
        &'db self,
        enum_id: EnumId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;
    /// Returns the generic parameters of an enum.
    #[salsa::transparent]
    #[salsa::invoke(items::enm::enum_generic_params_tracked)]
    fn enum_generic_params<'db>(&'db self, enum_id: EnumId<'db>) -> Maybe<Vec<GenericParam<'db>>>;
    /// Returns the generic parameters data of an enum.
    #[salsa::transparent]
    #[salsa::invoke(items::enm::enum_generic_params_data_tracked)]
    fn enum_generic_params_data<'db>(
        &'db self,
        enum_id: EnumId<'db>,
    ) -> Maybe<GenericParamsData<'db>>;
    /// Returns the attributes attached to an enum.
    #[salsa::transparent]
    #[salsa::invoke(items::enm::enum_attributes_tracked)]
    fn enum_attributes<'db>(&'db self, enum_id: EnumId<'db>) -> Maybe<Vec<Attribute<'db>>>;
    /// Returns the resolution resolved_items of an enum declaration.
    #[salsa::transparent]
    #[salsa::invoke(items::enm::enum_declaration_resolver_data_tracked)]
    fn enum_declaration_resolver_data<'db>(
        &'db self,
        enum_id: EnumId<'db>,
    ) -> Maybe<Arc<ResolverData<'db>>>;

    /// Private query to compute data about an enum definition.
    #[salsa::transparent]
    #[salsa::invoke(items::enm::priv_enum_definition_data_tracked)]
    fn priv_enum_definition_data<'db>(
        &'db self,
        enum_id: EnumId<'db>,
    ) -> Maybe<items::enm::EnumDefinitionData<'db>>;
    /// Returns the definition diagnostics of an enum definition.
    #[salsa::transparent]
    #[salsa::invoke(items::enm::enum_definition_diagnostics_tracked)]
    fn enum_definition_diagnostics<'db>(
        &'db self,
        enum_id: EnumId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;
    /// Returns the members of an enum.
    #[salsa::transparent]
    #[salsa::invoke(items::enm::enum_variants_tracked)]
    fn enum_variants<'db>(
        &'db self,
        enum_id: EnumId<'db>,
    ) -> Maybe<OrderedHashMap<StrRef<'db>, VariantId<'db>>>;
    /// Returns the semantic model of a variant.
    #[salsa::transparent]
    #[salsa::invoke(items::enm::variant_semantic_tracked)]
    fn variant_semantic<'db>(
        &'db self,
        enum_id: EnumId<'db>,
        variant_id: VariantId<'db>,
    ) -> Maybe<semantic::Variant<'db>>;
    /// Returns the resolution resolved_items of an enum definition.
    #[salsa::transparent]
    #[salsa::invoke(items::enm::enum_definition_resolver_data_tracked)]
    fn enum_definition_resolver_data<'db>(
        &'db self,
        enum_id: EnumId<'db>,
    ) -> Maybe<Arc<ResolverData<'db>>>;

    // Type Alias.
    // ===========
    /// Returns the semantic diagnostics of a type alias.
    #[salsa::transparent]
    #[salsa::invoke(items::module_type_alias::module_type_alias_semantic_diagnostics_tracked)]
    fn module_type_alias_semantic_diagnostics<'db>(
        &'db self,
        module_type_alias_id: ModuleTypeAliasId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;
    /// Returns the resolved type of a type alias.
    #[salsa::transparent]
    #[salsa::invoke(items::module_type_alias::module_type_alias_resolved_type_tracked)]
    fn module_type_alias_resolved_type<'db>(
        &'db self,
        module_type_alias_id: ModuleTypeAliasId<'db>,
    ) -> Maybe<TypeId<'db>>;
    /// Returns the generic parameters of a type alias.
    #[salsa::transparent]
    #[salsa::invoke(items::module_type_alias::module_type_alias_generic_params_tracked)]
    fn module_type_alias_generic_params<'db>(
        &'db self,
        enum_id: ModuleTypeAliasId<'db>,
    ) -> Maybe<Vec<GenericParam<'db>>>;
    /// Returns the resolution resolved_items of a type alias.
    #[salsa::transparent]
    #[salsa::invoke(items::module_type_alias::module_type_alias_resolver_data_tracked)]
    fn module_type_alias_resolver_data<'db>(
        &'db self,
        module_type_alias_id: ModuleTypeAliasId<'db>,
    ) -> Maybe<Arc<ResolverData<'db>>>;
    /// Private query to compute the generic parameters data of a type alias.
    #[salsa::transparent]
    #[salsa::invoke(items::module_type_alias::priv_module_type_alias_generic_params_data_tracked)]
    fn priv_module_type_alias_generic_params_data<'db>(
        &'db self,
        enum_id: ModuleTypeAliasId<'db>,
    ) -> Maybe<GenericParamsData<'db>>;
    /// Private query to compute data about a type alias.
    #[salsa::transparent]
    #[salsa::invoke(items::module_type_alias::priv_module_type_alias_semantic_data_tracked)]
    fn priv_module_type_alias_semantic_data<'db>(
        &'db self,
        module_type_alias_id: ModuleTypeAliasId<'db>,
        in_cycle: bool,
    ) -> Maybe<items::module_type_alias::ModuleTypeAliasData<'db>>;

    // Impl Alias.
    // ====
    /// Returns the impl definition pointed to by the impl alias, or an error if it points to
    /// something else.
    #[salsa::transparent]
    #[salsa::invoke(items::impl_alias::impl_alias_impl_def_tracked)]
    fn impl_alias_impl_def<'db>(
        &'db self,
        impl_alias_id: ImplAliasId<'db>,
    ) -> Maybe<ImplDefId<'db>>;
    /// Private query to compute data about a type alias.
    #[salsa::transparent]
    #[salsa::invoke(items::impl_alias::priv_impl_alias_semantic_data_tracked)]
    fn priv_impl_alias_semantic_data<'db>(
        &'db self,
        impl_alias_id: ImplAliasId<'db>,
        in_cycle: bool,
    ) -> Maybe<items::impl_alias::ImplAliasData<'db>>;
    /// Returns the semantic diagnostics of a type alias.
    #[salsa::transparent]
    #[salsa::invoke(items::impl_alias::impl_alias_semantic_diagnostics_tracked)]
    fn impl_alias_semantic_diagnostics<'db>(
        &'db self,
        impl_alias_id: ImplAliasId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;
    /// Returns the resolved type of a type alias.
    #[salsa::transparent]
    #[salsa::invoke(items::impl_alias::impl_alias_resolved_impl_tracked)]
    fn impl_alias_resolved_impl<'db>(
        &'db self,
        impl_alias_id: ImplAliasId<'db>,
    ) -> Maybe<ImplId<'db>>;
    /// Returns the generic parameters of a type alias.
    #[salsa::transparent]
    #[salsa::invoke(items::impl_alias::impl_alias_generic_params_tracked)]
    fn impl_alias_generic_params<'db>(
        &'db self,
        impl_alias_id: ImplAliasId<'db>,
    ) -> Maybe<Vec<GenericParam<'db>>>;
    /// Returns the generic parameters data of a type alias.
    #[salsa::transparent]
    #[salsa::invoke(items::impl_alias::impl_alias_generic_params_data_tracked)]
    fn impl_alias_generic_params_data<'db>(
        &'db self,
        impl_alias_id: ImplAliasId<'db>,
    ) -> Maybe<GenericParamsData<'db>>;
    /// Returns the resolution resolved_items of a type alias.
    #[salsa::transparent]
    #[salsa::invoke(items::impl_alias::impl_alias_resolver_data_tracked)]
    fn impl_alias_resolver_data<'db>(
        &'db self,
        impl_alias_id: ImplAliasId<'db>,
    ) -> Maybe<Arc<ResolverData<'db>>>;
    /// Returns the attributes attached to the impl alias.
    #[salsa::transparent]
    #[salsa::invoke(items::impl_alias::impl_alias_attributes_tracked)]
    fn impl_alias_attributes<'db>(
        &'db self,
        impl_def_id: ImplAliasId<'db>,
    ) -> Maybe<Vec<Attribute<'db>>>;

    // Trait.
    // =======
    /// Returns the semantic declaration diagnostics of a trait.
    #[salsa::transparent]
    #[salsa::invoke(items::trt::trait_semantic_declaration_diagnostics_tracked)]
    fn trait_semantic_declaration_diagnostics<'db>(
        &'db self,
        trait_id: TraitId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;
    /// Returns the generic parameters of a trait.
    #[salsa::transparent]
    #[salsa::invoke(items::trt::trait_generic_params_tracked)]
    fn trait_generic_params<'db>(
        &'db self,
        trait_id: TraitId<'db>,
    ) -> Maybe<Vec<GenericParam<'db>>>;
    /// Returns the generic parameters data of a trait.
    #[salsa::transparent]
    #[salsa::invoke(items::trt::trait_generic_params_data_tracked)]
    fn trait_generic_params_data<'db>(
        &'db self,
        trait_id: TraitId<'db>,
        in_cycle: bool,
    ) -> Maybe<GenericParamsData<'db>>;

    /// Returns the ids of the generic parameters of a trait.
    #[salsa::invoke(items::trt::trait_generic_params_ids_tracked)]
    fn trait_generic_params_ids<'db>(
        &'db self,
        trait_id: TraitId<'db>,
    ) -> Maybe<Vec<GenericParamId<'db>>>;

    /// Returns the attributes of a trait.
    #[salsa::transparent]
    #[salsa::invoke(items::trt::trait_attributes_tracked)]
    fn trait_attributes<'db>(&'db self, trait_id: TraitId<'db>) -> Maybe<Vec<Attribute<'db>>>;
    /// Returns the resolution resolved_items of a trait.
    #[salsa::transparent]
    #[salsa::invoke(items::trt::trait_resolver_data_tracked)]
    fn trait_resolver_data<'db>(&'db self, trait_id: TraitId<'db>)
    -> Maybe<Arc<ResolverData<'db>>>;
    /// Private query to compute declaration data about a trait.
    #[salsa::transparent]
    #[salsa::invoke(items::trt::priv_trait_declaration_data_tracked)]
    fn priv_trait_declaration_data<'db>(
        &'db self,
        trait_id: TraitId<'db>,
    ) -> Maybe<items::trt::TraitDeclarationData<'db>>;

    /// Returns the semantic definition diagnostics of a trait.
    #[salsa::transparent]
    #[salsa::invoke(items::trt::trait_semantic_definition_diagnostics_tracked)]
    fn trait_semantic_definition_diagnostics<'db>(
        &'db self,
        trait_id: TraitId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;
    /// Returns the names of all the non default implemented items of a trait.
    #[salsa::transparent]
    #[salsa::invoke(items::trt::trait_required_item_names_tracked)]
    fn trait_required_item_names<'db>(
        &'db self,
        trait_id: TraitId<'db>,
    ) -> Maybe<OrderedHashSet<StrRef<'db>>>;
    /// Returns the item of the trait, by the given `name`, if exists.
    #[salsa::transparent]
    #[salsa::invoke(items::trt::trait_item_by_name_tracked)]
    fn trait_item_by_name<'db>(
        &'db self,
        trait_id: TraitId<'db>,
        name: StrRef<'db>,
    ) -> Maybe<Option<TraitItemId<'db>>>;
    /// Returns the metadata for a trait item, by the given `name`, if exists.
    #[salsa::transparent]
    #[salsa::invoke(items::trt::trait_item_info_by_name_tracked)]
    fn trait_item_info_by_name<'db>(
        &'db self,
        trait_id: TraitId<'db>,
        name: StrRef<'db>,
    ) -> Maybe<Option<TraitItemInfo<'db>>>;
    /// Returns all the items used within the trait.
    #[salsa::transparent]
    #[salsa::invoke(items::trt::trait_all_used_uses_tracked)]
    fn trait_all_used_uses<'db>(
        &'db self,
        trait_id: TraitId<'db>,
    ) -> Maybe<Arc<OrderedHashSet<UseId<'db>>>>;
    /// Returns the functions of a trait.
    #[salsa::transparent]
    #[salsa::invoke(items::trt::trait_functions_tracked)]
    fn trait_functions<'db>(
        &'db self,
        trait_id: TraitId<'db>,
    ) -> Maybe<OrderedHashMap<StrRef<'db>, TraitFunctionId<'db>>>;
    /// Returns the function with the given name of the given trait, if exists.
    #[salsa::transparent]
    #[salsa::invoke(items::trt::trait_function_by_name_tracked)]
    fn trait_function_by_name<'db>(
        &'db self,
        trait_id: TraitId<'db>,
        name: StrRef<'db>,
    ) -> Maybe<Option<TraitFunctionId<'db>>>;
    /// Returns the types of a trait.
    #[salsa::transparent]
    #[salsa::invoke(items::trt::trait_types_tracked)]
    fn trait_types<'db>(
        &'db self,
        trait_id: TraitId<'db>,
    ) -> Maybe<OrderedHashMap<StrRef<'db>, TraitTypeId<'db>>>;
    /// Returns the item type with the given name of the given trait, if exists.
    #[salsa::transparent]
    #[salsa::invoke(items::trt::trait_type_by_name_tracked)]
    fn trait_type_by_name<'db>(
        &'db self,
        trait_id: TraitId<'db>,
        name: StrRef<'db>,
    ) -> Maybe<Option<TraitTypeId<'db>>>;

    /// Returns the constants of a trait.
    #[salsa::transparent]
    #[salsa::invoke(items::trt::trait_constants_tracked)]
    fn trait_constants<'db>(
        &'db self,
        trait_id: TraitId<'db>,
    ) -> Maybe<OrderedHashMap<StrRef<'db>, TraitConstantId<'db>>>;
    /// Returns the item constants with the given name of the given trait, if exists.
    #[salsa::transparent]
    #[salsa::invoke(items::trt::trait_constant_by_name_tracked)]
    fn trait_constant_by_name<'db>(
        &'db self,
        trait_id: TraitId<'db>,
        name: StrRef<'db>,
    ) -> Maybe<Option<TraitConstantId<'db>>>;
    /// Returns the constants of a trait.
    #[salsa::transparent]
    #[salsa::invoke(items::trt::trait_impls_tracked)]
    fn trait_impls<'db>(
        &'db self,
        trait_id: TraitId<'db>,
    ) -> Maybe<OrderedHashMap<StrRef<'db>, TraitImplId<'db>>>;
    /// Returns the item impls with the given name of the given trait, if exists.
    #[salsa::transparent]
    #[salsa::invoke(items::trt::trait_impl_by_name_tracked)]
    fn trait_impl_by_name<'db>(
        &'db self,
        trait_id: TraitId<'db>,
        name: StrRef<'db>,
    ) -> Maybe<Option<TraitImplId<'db>>>;

    /// Private query to compute definition data about a trait.
    #[salsa::transparent]
    #[salsa::invoke(items::trt::priv_trait_definition_data_tracked)]
    fn priv_trait_definition_data<'db>(
        &'db self,
        trait_id: TraitId<'db>,
    ) -> Maybe<items::trt::TraitDefinitionData<'db>>;

    // Trait type.
    // ================
    /// Returns the semantic diagnostics of a trait type.
    #[salsa::transparent]
    #[salsa::invoke(items::trt::trait_type_diagnostics_tracked)]
    fn trait_type_diagnostics<'db>(
        &'db self,
        trait_type_id: TraitTypeId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;
    /// Returns the generic params of a trait type.
    #[salsa::transparent]
    #[salsa::invoke(items::trt::trait_type_generic_params_tracked)]
    fn trait_type_generic_params<'db>(
        &'db self,
        trait_type_id: TraitTypeId<'db>,
    ) -> Maybe<Vec<GenericParam<'db>>>;
    /// Returns the attributes of a trait type.
    #[salsa::transparent]
    #[salsa::invoke(items::trt::trait_type_attributes_tracked)]
    fn trait_type_attributes<'db>(
        &'db self,
        trait_type_id: TraitTypeId<'db>,
    ) -> Maybe<Vec<Attribute<'db>>>;
    /// Returns the resolution resolved_items of a trait type.
    #[salsa::transparent]
    #[salsa::invoke(items::trt::trait_type_resolver_data_tracked)]
    fn trait_type_resolver_data<'db>(
        &'db self,
        trait_type_id: TraitTypeId<'db>,
    ) -> Maybe<Arc<ResolverData<'db>>>;
    /// Private query to compute the generic params data of a trait type.
    #[salsa::transparent]
    #[salsa::invoke(items::trt::priv_trait_type_generic_params_data_tracked)]
    fn priv_trait_type_generic_params_data<'db>(
        &'db self,
        trait_type_id: TraitTypeId<'db>,
    ) -> Maybe<GenericParamsData<'db>>;
    /// Private query to compute data about a trait type.
    #[salsa::transparent]
    #[salsa::invoke(items::trt::priv_trait_type_data_tracked)]
    fn priv_trait_type_data<'db>(
        &'db self,
        type_id: TraitTypeId<'db>,
    ) -> Maybe<TraitItemTypeData<'db>>;

    // Trait constants.
    // ================
    /// Returns the semantic diagnostics of a trait type.
    #[salsa::transparent]
    #[salsa::invoke(items::trt::trait_constant_diagnostics_tracked)]
    fn trait_constant_diagnostics<'db>(
        &'db self,
        trait_constant: TraitConstantId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;
    /// Returns the attributes of a trait constants.
    #[salsa::transparent]
    #[salsa::invoke(items::trt::trait_constant_attributes_tracked)]
    fn trait_constant_attributes<'db>(
        &'db self,
        trait_constant: TraitConstantId<'db>,
    ) -> Maybe<Vec<Attribute<'db>>>;
    /// Returns the type of a trait constant.
    #[salsa::transparent]
    #[salsa::invoke(items::trt::trait_constant_type_tracked)]
    fn trait_constant_type<'db>(
        &'db self,
        trait_type_id: TraitConstantId<'db>,
    ) -> Maybe<TypeId<'db>>;
    /// Returns the resolution resolved_items of a trait constants.
    #[salsa::transparent]
    #[salsa::invoke(items::trt::trait_constant_resolver_data_tracked)]
    fn trait_constant_resolver_data<'db>(
        &'db self,
        trait_constant: TraitConstantId<'db>,
    ) -> Maybe<Arc<ResolverData<'db>>>;
    /// Private query to compute data about a trait constant.
    #[salsa::transparent]
    #[salsa::invoke(items::trt::priv_trait_constant_data_tracked)]
    fn priv_trait_constant_data<'db>(
        &'db self,
        trait_constant: TraitConstantId<'db>,
    ) -> Maybe<TraitItemConstantData<'db>>;
    /// Returns the type of a trait constant.
    #[salsa::transparent]
    #[salsa::invoke(items::trt::concrete_trait_constant_type_tracked)]
    fn concrete_trait_constant_type<'db>(
        &'db self,
        concrete_trait_constant_id: items::trt::ConcreteTraitConstantId<'db>,
    ) -> Maybe<TypeId<'db>>;

    // Trait impls.
    // ================
    /// Returns the semantic diagnostics of a trait impls.
    #[salsa::transparent]
    #[salsa::invoke(items::trt::trait_impl_diagnostics_tracked)]
    fn trait_impl_diagnostics<'db>(
        &'db self,
        trait_impl: TraitImplId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;
    /// Returns the attributes of a trait impls.
    #[salsa::transparent]
    #[salsa::invoke(items::trt::trait_impl_attributes_tracked)]
    fn trait_impl_attributes<'db>(
        &'db self,
        trait_impl: TraitImplId<'db>,
    ) -> Maybe<Vec<Attribute<'db>>>;
    /// Returns the concrete trait of a trait impl.
    #[salsa::transparent]
    #[salsa::invoke(items::trt::trait_impl_concrete_trait_tracked)]
    fn trait_impl_concrete_trait<'db>(
        &'db self,
        trait_impl_id: TraitImplId<'db>,
    ) -> Maybe<ConcreteTraitId<'db>>;
    /// Returns the resolution resolved_items of a trait impls.
    #[salsa::transparent]
    #[salsa::invoke(items::trt::trait_impl_resolver_data_tracked)]
    fn trait_impl_resolver_data<'db>(
        &'db self,
        trait_impl: TraitImplId<'db>,
    ) -> Maybe<Arc<ResolverData<'db>>>;
    /// Private query to compute data about a trait impl.
    #[salsa::transparent]
    #[salsa::invoke(items::trt::priv_trait_impl_data_tracked)]
    fn priv_trait_impl_data<'db>(
        &'db self,
        trait_impl: TraitImplId<'db>,
    ) -> Maybe<TraitItemImplData<'db>>;
    /// Returns the concrete trait of a concrete trait impl.
    #[salsa::transparent]
    #[salsa::invoke(items::trt::concrete_trait_impl_concrete_trait_tracked)]
    fn concrete_trait_impl_concrete_trait<'db>(
        &'db self,
        concrete_trait_impl_id: items::trt::ConcreteTraitImplId<'db>,
    ) -> Maybe<ConcreteTraitId<'db>>;

    // Trait function.
    // ================
    /// Returns the semantic diagnostics of a trait function.
    #[salsa::transparent]
    #[salsa::invoke(items::trt::trait_function_declaration_diagnostics_tracked)]
    fn trait_function_declaration_diagnostics<'db>(
        &'db self,
        trait_function_id: TraitFunctionId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;
    /// Returns the signature of a trait function.
    #[salsa::transparent]
    #[salsa::invoke(items::trt::trait_function_signature_tracked)]
    fn trait_function_signature<'db>(
        &'db self,
        trait_function_id: TraitFunctionId<'db>,
    ) -> Maybe<semantic::Signature<'db>>;
    /// Returns the generic params of a trait function.
    #[salsa::transparent]
    #[salsa::invoke(items::trt::trait_function_generic_params_tracked)]
    fn trait_function_generic_params<'db>(
        &'db self,
        trait_function_id: TraitFunctionId<'db>,
    ) -> Maybe<Vec<GenericParam<'db>>>;
    /// Returns the generic params data of a trait function.
    #[salsa::transparent]
    #[salsa::invoke(items::trt::priv_trait_function_generic_params_data_tracked)]
    fn priv_trait_function_generic_params_data<'db>(
        &'db self,
        trait_function_id: TraitFunctionId<'db>,
    ) -> Maybe<GenericParamsData<'db>>;
    /// Returns the attributes of a trait function.
    #[salsa::transparent]
    #[salsa::invoke(items::trt::trait_function_attributes_tracked)]
    fn trait_function_attributes<'db>(
        &'db self,
        trait_function_id: TraitFunctionId<'db>,
    ) -> Maybe<Vec<Attribute<'db>>>;
    /// Returns the resolution resolved_items of a trait function.
    #[salsa::transparent]
    #[salsa::invoke(items::trt::trait_function_resolver_data_tracked)]
    fn trait_function_resolver_data<'db>(
        &'db self,
        trait_function_id: TraitFunctionId<'db>,
    ) -> Maybe<Arc<ResolverData<'db>>>;
    /// Returns the inline configuration of a trait function's declaration.
    #[salsa::transparent]
    #[salsa::invoke(items::trt::trait_function_declaration_inline_config_tracked)]
    fn trait_function_declaration_inline_config<'db>(
        &'db self,
        trait_function_id: TraitFunctionId<'db>,
    ) -> Maybe<InlineConfiguration<'db>>;
    /// Returns the implicits precedence of a trait function.
    #[salsa::transparent]
    #[salsa::invoke(items::trt::trait_function_declaration_implicit_precedence_tracked)]
    fn trait_function_declaration_implicit_precedence<'db>(
        &'db self,
        trait_function_id: TraitFunctionId<'db>,
    ) -> Maybe<ImplicitPrecedence<'db>>;
    /// Returns the explicit implicits of a signature of a trait function.
    #[salsa::transparent]
    #[salsa::invoke(items::trt::trait_function_declaration_implicits_tracked)]
    fn trait_function_declaration_implicits<'db>(
        &'db self,
        trait_function_id: TraitFunctionId<'db>,
    ) -> Maybe<Vec<TypeId<'db>>>;
    /// Private query to compute data about a trait function declaration.
    #[salsa::transparent]
    #[salsa::invoke(items::trt::priv_trait_function_declaration_data_tracked)]
    fn priv_trait_function_declaration_data<'db>(
        &'db self,
        function_id: TraitFunctionId<'db>,
    ) -> Maybe<items::functions::FunctionDeclarationData<'db>>;

    /// Returns the semantic diagnostics of a trait function definition (declaration + body).
    #[salsa::transparent]
    #[salsa::invoke(items::trt::trait_function_body_diagnostics_tracked)]
    fn trait_function_body_diagnostics<'db>(
        &'db self,
        trait_function_id: TraitFunctionId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;
    /// Returns the body of a trait function, if any.
    #[salsa::transparent]
    #[salsa::invoke(items::trt::trait_function_body_tracked)]
    fn trait_function_body<'db>(
        &'db self,
        trait_function_id: TraitFunctionId<'db>,
    ) -> Maybe<Option<Arc<FunctionBody<'db>>>>;
    /// Private query to compute data about a trait function definition (declaration + body)
    #[salsa::transparent]
    #[salsa::invoke(items::trt::priv_trait_function_body_data_tracked)]
    fn priv_trait_function_body_data<'db>(
        &'db self,
        trait_function_id: TraitFunctionId<'db>,
    ) -> Maybe<Option<items::function_with_body::FunctionBodyData<'db>>>;

    // Concrete Trait function.
    // ========================
    /// Returns the generic params of a concrete trait function.
    #[salsa::transparent]
    #[salsa::invoke(items::trt::concrete_trait_function_generic_params_tracked)]
    fn concrete_trait_function_generic_params<'db>(
        &'db self,
        concrete_trait_function_id: ConcreteTraitGenericFunctionId<'db>,
    ) -> Maybe<Vec<GenericParam<'db>>>;
    /// Returns the signature of a concrete trait function.
    #[salsa::transparent]
    #[salsa::invoke(items::trt::concrete_trait_function_signature_tracked)]
    fn concrete_trait_function_signature<'db>(
        &'db self,
        concrete_trait_function_id: ConcreteTraitGenericFunctionId<'db>,
    ) -> Maybe<semantic::Signature<'db>>;

    // Trait filter.
    // ==============
    // Returns the solution set for a canonical trait.
    #[salsa::transparent]
    #[salsa::invoke(inference::solver::canonic_trait_solutions_tracked)]
    fn canonic_trait_solutions<'db>(
        &'db self,
        canonical_trait: inference::canonic::CanonicalTrait<'db>,
        lookup_context: ImplLookupContextId<'db>,
        impl_type_bounds: BTreeMap<ImplTypeById<'db>, TypeId<'db>>,
    ) -> Result<
        inference::solver::SolutionSet<'db, inference::canonic::CanonicalImpl<'db>>,
        inference::InferenceError<'db>,
    >;

    // Impl.
    // =======
    /// Returns the semantic declaration diagnostics of an impl.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::impl_semantic_declaration_diagnostics_tracked)]
    fn impl_semantic_declaration_diagnostics<'db>(
        &'db self,
        impl_def_id: ImplDefId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;
    /// Returns the generic parameters data of an impl.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::impl_def_generic_params_data_tracked)]
    fn impl_def_generic_params_data<'db>(
        &'db self,
        impl_def_id: ImplDefId<'db>,
    ) -> Maybe<GenericParamsData<'db>>;
    /// Returns the generic parameters of an impl.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::impl_def_generic_params_tracked)]
    fn impl_def_generic_params<'db>(
        &'db self,
        impl_def_id: ImplDefId<'db>,
    ) -> Maybe<Vec<GenericParam<'db>>>;
    /// Returns the resolution resolved_items of an impl.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::impl_def_resolver_data_tracked)]
    fn impl_def_resolver_data<'db>(
        &'db self,
        impl_def_id: ImplDefId<'db>,
    ) -> Maybe<Arc<ResolverData<'db>>>;
    /// Returns the concrete trait that is implemented by the impl.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::impl_def_concrete_trait_tracked)]
    fn impl_def_concrete_trait<'db>(
        &'db self,
        impl_def_id: ImplDefId<'db>,
    ) -> Maybe<ConcreteTraitId<'db>>;
    /// Returns the substitution for generics for the impl.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::impl_def_substitution_tracked)]
    fn impl_def_substitution<'db>(
        &'db self,
        impl_def_id: ImplDefId<'db>,
    ) -> Maybe<Arc<GenericSubstitution<'db>>>;
    /// Returns the attributes attached to the impl.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::impl_def_attributes_tracked)]
    fn impl_def_attributes<'db>(
        &'db self,
        impl_def_id: ImplDefId<'db>,
    ) -> Maybe<Vec<Attribute<'db>>>;
    /// Returns the concrete trait that is implemented by the concrete impl.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::impl_concrete_trait_tracked)]
    fn impl_concrete_trait<'db>(&'db self, impl_id: ImplId<'db>) -> Maybe<ConcreteTraitId<'db>>;
    /// Returns the trait that is implemented by the impl, or an error if the RHS of the `of` is not
    /// a trait.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::impl_def_trait_tracked)]
    fn impl_def_trait<'db>(&'db self, impl_def_id: ImplDefId<'db>) -> Maybe<TraitId<'db>>;

    /// Returns the shallow trait generic arguments of an impl.
    #[salsa::invoke(items::imp::impl_def_shallow_trait_generic_args)]
    #[salsa::transparent]
    fn impl_def_shallow_trait_generic_args<'db>(
        &'db self,
        impl_def_id: ImplDefId<'db>,
    ) -> Maybe<&'db [(GenericParamId<'db>, ShallowGenericArg<'db>)]>;

    /// Returns the shallow trait generic arguments of an impl alias.
    #[salsa::invoke(items::imp::impl_alias_trait_generic_args)]
    #[salsa::transparent]
    fn impl_alias_trait_generic_args<'db>(
        &'db self,
        impl_def_id: ImplAliasId<'db>,
    ) -> Maybe<&'db [(GenericParamId<'db>, ShallowGenericArg<'db>)]>;

    /// Private query to compute declaration data about an impl.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::priv_impl_declaration_data_tracked)]
    fn priv_impl_declaration_data<'db>(
        &'db self,
        impl_def_id: ImplDefId<'db>,
    ) -> Maybe<items::imp::ImplDeclarationData<'db>>;

    /// Returns the semantic definition diagnostics of an impl.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::impl_semantic_definition_diagnostics_tracked)]
    fn impl_semantic_definition_diagnostics<'db>(
        &'db self,
        impl_def_id: ImplDefId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;
    /// Returns the item of the impl, by the given `name`, if exists.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::impl_item_by_name_tracked)]
    fn impl_item_by_name<'db>(
        &'db self,
        impl_def_id: ImplDefId<'db>,
        name: StrRef<'db>,
    ) -> Maybe<Option<ImplItemId<'db>>>;
    /// Returns the metadata for an impl item, by the given `name`, if exists.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::impl_item_info_by_name_tracked)]
    fn impl_item_info_by_name<'db>(
        &'db self,
        impl_def_id: ImplDefId<'db>,
        name: StrRef<'db>,
    ) -> Maybe<Option<ImplItemInfo<'db>>>;
    /// Returns the trait impl of an implicit impl if `name` exists in trait and not in the impl.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::impl_implicit_impl_by_name_tracked)]
    fn impl_implicit_impl_by_name<'db>(
        &'db self,
        impl_def_id: ImplDefId<'db>,
        name: StrRef<'db>,
    ) -> Maybe<Option<TraitImplId<'db>>>;
    /// Returns all the items used within the impl.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::impl_all_used_uses_tracked)]
    fn impl_all_used_uses<'db>(
        &'db self,
        impl_def_id: ImplDefId<'db>,
    ) -> Maybe<Arc<OrderedHashSet<UseId<'db>>>>;
    /// Returns the type items in the impl.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::impl_types_tracked)]
    fn impl_types<'db>(
        &'db self,
        impl_def_id: ImplDefId<'db>,
    ) -> Maybe<Arc<OrderedHashMap<ImplTypeDefId<'db>, ast::ItemTypeAlias<'db>>>>;
    /// Returns the ids of the type items in the impl.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::impl_type_ids_tracked)]
    fn impl_type_ids<'db>(
        &'db self,
        impl_def_id: ImplDefId<'db>,
    ) -> Maybe<Arc<Vec<ImplTypeDefId<'db>>>>;
    /// Returns the impl AST of the impl type that matches the given id, if exists.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::impl_type_by_id_tracked)]
    fn impl_type_by_id<'db>(
        &'db self,
        impl_type_id: ImplTypeDefId<'db>,
    ) -> Maybe<ast::ItemTypeAlias<'db>>;
    /// Returns the impl type item that matches the given trait type item, if exists.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::impl_type_by_trait_type_tracked)]
    fn impl_type_by_trait_type<'db>(
        &'db self,
        impl_def_id: ImplDefId<'db>,
        trait_type_id: TraitTypeId<'db>,
    ) -> Maybe<ImplTypeDefId<'db>>;

    /// Returns the constant items in the impl.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::impl_constants_tracked)]
    fn impl_constants<'db>(
        &'db self,
        impl_def_id: ImplDefId<'db>,
    ) -> Maybe<Arc<OrderedHashMap<ImplConstantDefId<'db>, ast::ItemConstant<'db>>>>;

    /// Returns the impls items in the impl.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::impl_impls_tracked)]
    fn impl_impls<'db>(
        &'db self,
        impl_def_id: ImplDefId<'db>,
    ) -> Maybe<Arc<OrderedHashMap<ImplImplDefId<'db>, ast::ItemImplAlias<'db>>>>;
    /// Returns the ids of the impl items in the impl.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::impl_impl_ids_tracked)]
    fn impl_impl_ids<'db>(
        &'db self,
        impl_def_id: ImplDefId<'db>,
    ) -> Maybe<Arc<Vec<ImplImplDefId<'db>>>>;
    /// Returns the impl AST of the impl impl that matches the given id, if exists.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::impl_impl_by_id_tracked)]
    fn impl_impl_by_id<'db>(
        &'db self,
        impl_impl_id: ImplImplDefId<'db>,
    ) -> Maybe<ast::ItemImplAlias<'db>>;
    /// Returns the impl impl item that matches the given trait impl item, if exists.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::impl_impl_by_trait_impl_tracked)]
    fn impl_impl_by_trait_impl<'db>(
        &'db self,
        impl_def_id: ImplDefId<'db>,
        trait_impl_id: TraitImplId<'db>,
    ) -> Maybe<ImplImplDefId<'db>>;
    /// Returns whether `trait_impl_id` is an implicit impl in `impl_def_id`.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::is_implicit_impl_impl_tracked)]
    fn is_implicit_impl_impl<'db>(
        &self,
        impl_def_id: ImplDefId<'db>,
        trait_impl_id: TraitImplId<'db>,
    ) -> Maybe<bool>;

    /// Returns the impl constant item that matches the given trait constant item, if exists.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::impl_constant_by_trait_constant_tracked)]
    fn impl_constant_by_trait_constant<'db>(
        &'db self,
        impl_def_id: ImplDefId<'db>,
        trait_constant_id: TraitConstantId<'db>,
    ) -> Maybe<ImplConstantDefId<'db>>;

    /// Returns the functions in the impl.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::impl_functions_tracked)]
    fn impl_functions<'db>(
        &'db self,
        impl_def_id: ImplDefId<'db>,
    ) -> Maybe<OrderedHashMap<StrRef<'db>, ImplFunctionId<'db>>>;
    /// Returns the impl function that matches the given trait function, if exists.
    /// Note that a function that doesn't exist in the impl doesn't necessarily indicate an error,
    /// as, e.g., a trait function that has a default implementation doesn't have to be
    /// implemented in the impl.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::impl_function_by_trait_function_tracked)]
    fn impl_function_by_trait_function<'db>(
        &'db self,
        impl_def_id: ImplDefId<'db>,
        trait_function_id: TraitFunctionId<'db>,
    ) -> Maybe<Option<ImplFunctionId<'db>>>;
    /// Private query to compute definition data about an impl.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::priv_impl_definition_data_tracked)]
    fn priv_impl_definition_data<'db>(
        &'db self,
        impl_def_id: ImplDefId<'db>,
    ) -> Maybe<items::imp::ImplDefinitionData<'db>>;

    /// Returns the uninferred impls in a module.
    #[salsa::invoke(items::imp::module_impl_ids_tracked)]
    fn module_impl_ids<'db>(
        &'db self,
        user_module: ModuleId<'db>,
        containing_module: ModuleId<'db>,
    ) -> Maybe<Arc<BTreeSet<UninferredImplById<'db>>>>;

    /// Private query to check if an impl is fully concrete.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::priv_impl_is_fully_concrete_tracked)]
    fn priv_impl_is_fully_concrete<'db>(&self, impl_id: ImplId<'db>) -> bool;

    /// Private query to check if an impl contains no variables.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::priv_impl_is_var_free_tracked)]
    fn priv_impl_is_var_free<'db>(&self, impl_id: ImplId<'db>) -> bool;

    // Impl type def.
    // ================
    /// Returns the semantic diagnostics of an impl item type.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::impl_type_def_semantic_diagnostics_tracked)]
    fn impl_type_def_semantic_diagnostics<'db>(
        &'db self,
        impl_type_def_id: ImplTypeDefId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;
    /// Returns the resolved type of an impl item type.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::impl_type_def_resolved_type_tracked)]
    fn impl_type_def_resolved_type<'db>(
        &'db self,
        impl_type_def_id: ImplTypeDefId<'db>,
    ) -> Maybe<TypeId<'db>>;
    /// Returns the generic parameters of an impl item type.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::impl_type_def_generic_params_tracked)]
    fn impl_type_def_generic_params<'db>(
        &'db self,
        impl_type_def_id: ImplTypeDefId<'db>,
    ) -> Maybe<Vec<GenericParam<'db>>>;
    /// Returns the attributes of an impl type.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::impl_type_def_attributes_tracked)]
    fn impl_type_def_attributes<'db>(
        &'db self,
        impl_type_def_id: ImplTypeDefId<'db>,
    ) -> Maybe<Vec<Attribute<'db>>>;
    /// Returns the resolution resolved_items of an impl item type.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::impl_type_def_resolver_data_tracked)]
    fn impl_type_def_resolver_data<'db>(
        &'db self,
        impl_type_def_id: ImplTypeDefId<'db>,
    ) -> Maybe<Arc<ResolverData<'db>>>;
    /// Returns the trait type of an impl type.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::impl_type_def_trait_type_tracked)]
    fn impl_type_def_trait_type<'db>(
        &'db self,
        impl_type_def_id: ImplTypeDefId<'db>,
    ) -> Maybe<TraitTypeId<'db>>;

    /// Private query to compute data about an impl item type.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::priv_impl_type_semantic_data_tracked)]
    fn priv_impl_type_semantic_data<'db>(
        &'db self,
        impl_type_def_id: ImplTypeDefId<'db>,
        in_cycle: bool,
    ) -> Maybe<items::imp::ImplItemTypeData<'db>>;
    /// Private query to compute data about the generic parameters of an impl item type.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::priv_impl_type_def_generic_params_data_tracked)]
    fn priv_impl_type_def_generic_params_data<'db>(
        &'db self,
        impl_type_def_id: ImplTypeDefId<'db>,
    ) -> Maybe<GenericParamsData<'db>>;

    /// Returns the deref chain and diagnostics for a given type.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::deref_chain_tracked)]
    fn deref_chain<'db>(
        &'db self,
        ty: TypeId<'db>,
        crate_id: CrateId<'db>,
        try_deref_mut: bool,
    ) -> Maybe<items::imp::DerefChain<'db>>;

    // Impl type.
    // ================
    /// Returns the implized impl type if the impl is concrete. Returns a TypeId that's not an impl
    /// type with a concrete impl.
    // TODO(Gil): Consider removing the cycle handling here if we will upgrade the salsa version.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::impl_type_concrete_implized_tracked)]
    fn impl_type_concrete_implized<'db>(
        &'db self,
        impl_type_def_id: ImplTypeId<'db>,
    ) -> Maybe<TypeId<'db>>;

    // Impl constant def.
    // ================
    /// Returns the semantic diagnostics of an impl item constant.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::impl_constant_def_semantic_diagnostics_tracked)]
    fn impl_constant_def_semantic_diagnostics<'db>(
        &'db self,
        impl_constant_def_id: ImplConstantDefId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;
    /// Returns the resolved constant value of an impl item constant.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::impl_constant_def_value_tracked)]
    fn impl_constant_def_value<'db>(
        &'db self,
        impl_constant_def_id: ImplConstantDefId<'db>,
    ) -> Maybe<ConstValueId<'db>>;
    /// Returns the resolution resolved_items of an impl item constant.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::impl_constant_def_resolver_data_tracked)]
    fn impl_constant_def_resolver_data<'db>(
        &'db self,
        impl_constant_def_id: ImplConstantDefId<'db>,
    ) -> Maybe<Arc<ResolverData<'db>>>;
    /// Returns the type of an impl item constant.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::impl_constant_def_trait_constant_tracked)]
    fn impl_constant_def_trait_constant<'db>(
        &'db self,
        impl_constant_def_id: ImplConstantDefId<'db>,
    ) -> Maybe<TraitConstantId<'db>>;

    /// Private query to compute data about an impl item constant.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::priv_impl_constant_semantic_data_tracked)]
    fn priv_impl_constant_semantic_data<'db>(
        &'db self,
        impl_constant_def_id: ImplConstantDefId<'db>,
        in_cycle: bool,
    ) -> Maybe<items::imp::ImplItemConstantData<'db>>;

    // Impl constant.
    // ================
    /// Returns the given impl constant, implized by the given impl context.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::impl_constant_implized_by_context_tracked)]
    fn impl_constant_implized_by_context<'db>(
        &'db self,
        impl_constant_id: ImplConstantId<'db>,
        impl_def_id: ImplDefId<'db>,
    ) -> Maybe<ConstValueId<'db>>;
    /// Returns the implized impl constant value if the impl is concrete.
    // TODO(Gil): Consider removing the cycle handling here if we will upgrade the salsa version.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::impl_constant_concrete_implized_value_tracked)]
    fn impl_constant_concrete_implized_value<'db>(
        &'db self,
        impl_constant_id: ImplConstantId<'db>,
    ) -> Maybe<ConstValueId<'db>>;
    /// Returns the implized impl constant type if the impl is concrete.
    // TODO(Gil): Consider removing the cycle handling here if we will upgrade the salsa version.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::impl_constant_concrete_implized_type_tracked)]
    fn impl_constant_concrete_implized_type<'db>(
        &'db self,
        impl_constant_id: ImplConstantId<'db>,
    ) -> Maybe<TypeId<'db>>;

    // Impl impl def.
    // ================
    /// Returns the semantic diagnostics of an impl item impl.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::impl_impl_def_semantic_diagnostics_tracked)]
    fn impl_impl_def_semantic_diagnostics<'db>(
        &'db self,
        impl_impl_def_id: ImplImplDefId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;
    /// Returns the resolution resolved_items of an impl item impl.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::impl_impl_def_resolver_data_tracked)]
    fn impl_impl_def_resolver_data<'db>(
        &'db self,
        impl_impl_def_id: ImplImplDefId<'db>,
    ) -> Maybe<Arc<ResolverData<'db>>>;
    /// Returns the type of an impl item impl.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::impl_impl_def_trait_impl_tracked)]
    fn impl_impl_def_trait_impl<'db>(
        &'db self,
        impl_impl_def_id: ImplImplDefId<'db>,
    ) -> Maybe<TraitImplId<'db>>;

    /// Returns the resolved impl of an impl item impl.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::impl_impl_def_impl_tracked)]
    fn impl_impl_def_impl<'db>(
        &'db self,
        impl_impl_def_id: ImplImplDefId<'db>,
        in_cycle: bool,
    ) -> Maybe<ImplId<'db>>;

    /// Private query to compute data about an impl item impl.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::priv_impl_impl_semantic_data_tracked)]
    fn priv_impl_impl_semantic_data<'db>(
        &'db self,
        impl_impl_def_id: ImplImplDefId<'db>,
        in_cycle: bool,
    ) -> Maybe<items::imp::ImplItemImplData<'db>>;

    /// Private query to compute data about the generic parameters of an impl item impl.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::priv_impl_impl_def_generic_params_data_tracked)]
    fn priv_impl_impl_def_generic_params_data<'db>(
        &'db self,
        impl_impl_def_id: ImplImplDefId<'db>,
    ) -> Maybe<GenericParamsData<'db>>;

    /// Returns the semantic diagnostics of an implicit impl.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::implicit_impl_impl_semantic_diagnostics_tracked)]
    fn implicit_impl_impl_semantic_diagnostics<'db>(
        &'db self,
        impl_def_id: ImplDefId<'db>,
        trait_impl_id: TraitImplId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;

    /// Returns the resolved impl of an implicit impl.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::implicit_impl_impl_impl_tracked)]
    fn implicit_impl_impl_impl<'db>(
        &'db self,
        impl_def_id: ImplDefId<'db>,
        trait_impl_id: TraitImplId<'db>,
        in_cycle: bool,
    ) -> Maybe<ImplId<'db>>;
    // Private query to compute data about an implicit impl.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::priv_implicit_impl_impl_semantic_data_tracked)]
    fn priv_implicit_impl_impl_semantic_data<'db>(
        &'db self,
        impl_def_id: ImplDefId<'db>,
        trait_impl_id: TraitImplId<'db>,
        in_cycle: bool,
    ) -> Maybe<ImplicitImplImplData<'db>>;

    // Impl impl.
    // ================
    /// Returns the implized impl impl if the impl is concrete.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::impl_impl_implized_by_context_tracked)]
    fn impl_impl_implized_by_context<'db>(
        &'db self,
        impl_impl_id: ImplImplId<'db>,
        impl_def_id: ImplDefId<'db>,
        in_cycle: bool,
    ) -> Maybe<ImplId<'db>>;
    /// Returns the implized impl impl value if the impl is concrete.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::impl_impl_concrete_implized_tracked)]
    fn impl_impl_concrete_implized<'db>(
        &'db self,
        impl_impl_id: ImplImplId<'db>,
    ) -> Maybe<ImplId<'db>>;
    /// Returns the concrete trait of an impl impl.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::impl_impl_concrete_trait_tracked)]
    fn impl_impl_concrete_trait<'db>(
        &'db self,
        impl_impl_id: ImplImplId<'db>,
    ) -> Maybe<ConcreteTraitId<'db>>;

    // Impl function.
    // ================
    /// Returns the semantic diagnostics of an impl function's declaration (signature).
    #[salsa::transparent]
    #[salsa::invoke(items::imp::impl_function_declaration_diagnostics_tracked)]
    fn impl_function_declaration_diagnostics<'db>(
        &'db self,
        impl_function_id: ImplFunctionId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;
    /// Returns the signature of an impl function.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::impl_function_signature_tracked)]
    fn impl_function_signature<'db>(
        &'db self,
        impl_function_id: ImplFunctionId<'db>,
    ) -> Maybe<semantic::Signature<'db>>;
    /// Returns the generic params of an impl function.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::impl_function_generic_params_tracked)]
    fn impl_function_generic_params<'db>(
        &'db self,
        impl_function_id: ImplFunctionId<'db>,
    ) -> Maybe<Vec<GenericParam<'db>>>;
    /// Returns the generic params data of an impl function.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::priv_impl_function_generic_params_data_tracked)]
    fn priv_impl_function_generic_params_data<'db>(
        &'db self,
        impl_function_id: ImplFunctionId<'db>,
    ) -> Maybe<GenericParamsData<'db>>;
    /// Returns the attributes of an impl function.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::impl_function_attributes_tracked)]
    fn impl_function_attributes<'db>(
        &'db self,
        impl_function_id: ImplFunctionId<'db>,
    ) -> Maybe<Vec<Attribute<'db>>>;
    /// Returns the resolution resolved_items of an impl function's declaration.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::impl_function_resolver_data_tracked)]
    fn impl_function_resolver_data<'db>(
        &'db self,
        impl_function_id: ImplFunctionId<'db>,
    ) -> Maybe<Arc<ResolverData<'db>>>;
    /// Returns the inline configuration of an impl function's declaration.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::impl_function_declaration_inline_config_tracked)]
    fn impl_function_declaration_inline_config<'db>(
        &'db self,
        impl_function_id: ImplFunctionId<'db>,
    ) -> Maybe<InlineConfiguration<'db>>;
    /// Returns the implicits precedence of an impl function.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::impl_function_declaration_implicit_precedence_tracked)]
    fn impl_function_declaration_implicit_precedence<'db>(
        &'db self,
        impl_function_id: ImplFunctionId<'db>,
    ) -> Maybe<ImplicitPrecedence<'db>>;
    /// Returns the explicit implicits of a signature of an impl function.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::impl_function_declaration_implicits_tracked)]
    fn impl_function_declaration_implicits<'db>(
        &'db self,
        impl_function_id: ImplFunctionId<'db>,
    ) -> Maybe<Vec<TypeId<'db>>>;
    /// Returns the trait function of an impl function.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::impl_function_trait_function_tracked)]
    fn impl_function_trait_function<'db>(
        &'db self,
        impl_function_id: ImplFunctionId<'db>,
    ) -> Maybe<TraitFunctionId<'db>>;
    /// Private query to compute data about an impl function declaration.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::priv_impl_function_declaration_data_tracked)]
    fn priv_impl_function_declaration_data<'db>(
        &'db self,
        impl_function_id: ImplFunctionId<'db>,
    ) -> Maybe<items::imp::ImplFunctionDeclarationData<'db>>;

    /// Returns the semantic diagnostics of an impl function definition (declaration + body).
    #[salsa::transparent]
    #[salsa::invoke(items::imp::impl_function_body_diagnostics_tracked)]
    fn impl_function_body_diagnostics<'db>(
        &'db self,
        impl_function_id: ImplFunctionId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;
    /// Returns the definition of an impl function.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::impl_function_body_tracked)]
    fn impl_function_body<'db>(
        &'db self,
        impl_function_id: ImplFunctionId<'db>,
    ) -> Maybe<Arc<FunctionBody<'db>>>;
    /// Returns the resolution resolved_items of an impl function's definition.
    #[salsa::transparent]
    #[salsa::invoke(items::imp::impl_function_body_resolver_data_tracked)]
    fn impl_function_body_resolver_data<'db>(
        &'db self,
        impl_function_id: ImplFunctionId<'db>,
    ) -> Maybe<Arc<ResolverData<'db>>>;
    /// Private query to compute data about an impl function definition (declaration + body)
    #[salsa::transparent]
    #[salsa::invoke(items::imp::priv_impl_function_body_data_tracked)]
    fn priv_impl_function_body_data<'db>(
        &'db self,
        impl_function_id: ImplFunctionId<'db>,
    ) -> Maybe<items::function_with_body::FunctionBodyData<'db>>;

    /// Returns the dependencies of a crate.
    #[salsa::invoke(items::imp::priv_crate_dependencies_tracked)]
    fn priv_crate_dependencies<'db>(
        &'db self,
        crate_id: CrateId<'db>,
    ) -> Arc<OrderedHashSet<CrateId<'db>>>;
    /// Returns the uninferred impls of a crate which are global.
    /// An impl is global if it is defined in the same module as the trait it implements or in the
    /// same module as one of its concrete traits' types.
    #[salsa::invoke(items::imp::crate_global_impls)]
    #[salsa::transparent]
    fn crate_global_impls<'db>(
        &'db self,
        crate_id: CrateId<'db>,
    ) -> Maybe<&'db UnorderedHashMap<TraitId<'db>, OrderedHashSet<UninferredImplById<'db>>>>;

    /// Returns the traits which impls of a trait directly depend on.
    #[salsa::invoke(items::imp::crate_traits_dependencies_tracked)]
    fn crate_traits_dependencies<'db>(
        &'db self,
        crate_id: CrateId<'db>,
    ) -> Arc<UnorderedHashMap<TraitId<'db>, OrderedHashSet<TraitId<'db>>>>;

    /// Returns the traits which are reachable from a trait.
    #[salsa::invoke(items::imp::reachable_trait_dependencies_tracked)]
    fn reachable_trait_dependencies<'db>(
        &'db self,
        trait_id: TraitId<'db>,
        crate_id: CrateId<'db>,
    ) -> OrderedHashSet<TraitId<'db>>;

    /// Returns the global and local impls of a module.
    #[salsa::invoke(items::imp::module_global_impls)]
    #[salsa::transparent]
    fn module_global_impls<'db>(
        &'db self,
        _tracked: Tracked,
        module_id: ModuleId<'db>,
    ) -> &'db Maybe<ModuleImpls<'db>>;

    /// Returns the candidates for a trait by its head.
    #[salsa::invoke(items::imp::trait_candidate_by_head)]
    #[salsa::transparent]
    fn trait_candidate_by_head<'db>(
        &'db self,
        crate_id: CrateId<'db>,
        trait_id: TraitId<'db>,
    ) -> &'db OrderedHashMap<GenericsHeadFilter<'db>, OrderedHashSet<UninferredImplById<'db>>>;

    // Implizations.
    // ==============
    /// Returns the impl type for the given trait type, by implization by the given impl context, if
    /// the impl matches the trait of the trait type.
    // TODO(Gil): Consider removing the cycle handling here if we will upgrade the salsa version.
    #[salsa::transparent]
    #[salsa::invoke(items::implization::trait_type_implized_by_context_tracked)]
    fn trait_type_implized_by_context<'db>(
        &'db self,
        trait_type_def_id: TraitTypeId<'db>,
        impl_def_id: ImplDefId<'db>,
    ) -> Maybe<TypeId<'db>>;

    // Free function.
    // ==============
    /// Returns the semantic diagnostics of a free function's declaration (signature).
    #[salsa::transparent]
    #[salsa::invoke(items::free_function::free_function_declaration_diagnostics_tracked)]
    fn free_function_declaration_diagnostics<'db>(
        &'db self,
        free_function_id: FreeFunctionId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;
    /// Returns the signature of a free function.
    #[salsa::transparent]
    #[salsa::invoke(items::free_function::free_function_signature_tracked)]
    fn free_function_signature<'db>(
        &'db self,
        free_function_id: FreeFunctionId<'db>,
    ) -> Maybe<semantic::Signature<'db>>;
    /// Returns the explicit implicits of a signature of a free function.
    #[salsa::transparent]
    #[salsa::invoke(items::free_function::free_function_declaration_implicits_tracked)]
    fn free_function_declaration_implicits<'db>(
        &'db self,
        free_function_id: FreeFunctionId<'db>,
    ) -> Maybe<Vec<TypeId<'db>>>;
    /// Returns the implicits precedence of a free function.
    #[salsa::transparent]
    #[salsa::invoke(items::free_function::free_function_declaration_implicit_precedence_tracked)]
    fn free_function_declaration_implicit_precedence<'db>(
        &'db self,
        free_function_id: FreeFunctionId<'db>,
    ) -> Maybe<ImplicitPrecedence<'db>>;
    /// Returns the generic params of a free function.
    #[salsa::transparent]
    #[salsa::invoke(items::free_function::free_function_generic_params_tracked)]
    fn free_function_generic_params<'db>(
        &'db self,
        free_function_id: FreeFunctionId<'db>,
    ) -> Maybe<Vec<GenericParam<'db>>>;
    /// Returns the generic params data of a free function.
    #[salsa::transparent]
    #[salsa::invoke(items::free_function::free_function_generic_params_data_tracked)]
    fn free_function_generic_params_data<'db>(
        &'db self,
        free_function_id: FreeFunctionId<'db>,
    ) -> Maybe<GenericParamsData<'db>>;
    /// Returns the resolution resolved_items of a free function's declaration.
    #[salsa::transparent]
    #[salsa::invoke(items::free_function::free_function_declaration_resolver_data_tracked)]
    fn free_function_declaration_resolver_data<'db>(
        &'db self,
        free_function_id: FreeFunctionId<'db>,
    ) -> Maybe<Arc<ResolverData<'db>>>;
    /// Returns the inline configuration of a free function's declaration.
    #[salsa::transparent]
    #[salsa::invoke(items::free_function::free_function_declaration_inline_config_tracked)]
    fn free_function_declaration_inline_config<'db>(
        &'db self,
        free_function_id: FreeFunctionId<'db>,
    ) -> Maybe<InlineConfiguration<'db>>;
    /// Private query to compute data about a free function declaration - its signature excluding
    /// its body.
    #[salsa::transparent]
    #[salsa::invoke(items::free_function::priv_free_function_declaration_data_tracked)]
    fn priv_free_function_declaration_data<'db>(
        &'db self,
        function_id: FreeFunctionId<'db>,
    ) -> Maybe<items::functions::FunctionDeclarationData<'db>>;

    /// Returns the semantic diagnostics of a free function's body.
    #[salsa::transparent]
    #[salsa::invoke(items::free_function::free_function_body_diagnostics_tracked)]
    fn free_function_body_diagnostics<'db>(
        &'db self,
        free_function_id: FreeFunctionId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;
    /// Returns the resolution resolved_items of a free function's body.
    #[salsa::transparent]
    #[salsa::invoke(items::free_function::free_function_body_resolver_data_tracked)]
    fn free_function_body_resolver_data<'db>(
        &'db self,
        free_function_id: FreeFunctionId<'db>,
    ) -> Maybe<Arc<ResolverData<'db>>>;
    /// Private query to compute data about a free function's body.
    #[salsa::transparent]
    #[salsa::invoke(items::free_function::priv_free_function_body_data_tracked)]
    fn priv_free_function_body_data<'db>(
        &'db self,
        free_function_id: FreeFunctionId<'db>,
    ) -> Maybe<items::function_with_body::FunctionBodyData<'db>>;

    // Function with body.
    // ===================
    /// Returns the semantic diagnostics of a declaration (signature) of a function with a body.
    #[salsa::transparent]
    #[salsa::invoke(items::function_with_body::function_declaration_diagnostics_tracked)]
    fn function_declaration_diagnostics<'db>(
        &'db self,
        function_id: FunctionWithBodyId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;
    /// Returns the inline configuration of a declaration (signature) of a function with a body.
    #[salsa::transparent]
    #[salsa::invoke(items::function_with_body::function_declaration_inline_config_tracked)]
    fn function_declaration_inline_config<'db>(
        &'db self,
        function_id: FunctionWithBodyId<'db>,
    ) -> Maybe<InlineConfiguration<'db>>;
    /// Returns the implicit order of a declaration (signature) of a function with a body.
    #[salsa::transparent]
    #[salsa::invoke(items::function_with_body::function_declaration_implicit_precedence_tracked)]
    fn function_declaration_implicit_precedence<'db>(
        &'db self,
        function_id: FunctionWithBodyId<'db>,
    ) -> Maybe<ImplicitPrecedence<'db>>;
    /// Returns the signature of a function with a body.
    #[salsa::transparent]
    #[salsa::invoke(items::function_with_body::function_with_body_signature_tracked)]
    fn function_with_body_signature<'db>(
        &'db self,
        function_id: FunctionWithBodyId<'db>,
    ) -> Maybe<semantic::Signature<'db>>;
    /// Returns all the available generic params inside a function body.
    #[salsa::transparent]
    #[salsa::invoke(items::function_with_body::function_with_body_generic_params_tracked)]
    fn function_with_body_generic_params<'db>(
        &'db self,
        function_id: FunctionWithBodyId<'db>,
    ) -> Maybe<Vec<GenericParam<'db>>>;
    /// Returns the attributes of a function with a body.
    #[salsa::transparent]
    #[salsa::invoke(items::function_with_body::function_with_body_attributes_tracked)]
    fn function_with_body_attributes<'db>(
        &'db self,
        function_id: FunctionWithBodyId<'db>,
    ) -> Maybe<Vec<Attribute<'db>>>;

    /// Returns the semantic diagnostics of a body of a function (with a body).
    #[salsa::transparent]
    #[salsa::invoke(items::function_with_body::function_body_diagnostics_tracked)]
    fn function_body_diagnostics<'db>(
        &'db self,
        function_id: FunctionWithBodyId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;
    /// Returns the body of a function (with a body).
    #[salsa::transparent]
    #[salsa::invoke(items::function_with_body::function_body_tracked)]
    fn function_body<'db>(
        &'db self,
        function_id: FunctionWithBodyId<'db>,
    ) -> Maybe<Arc<FunctionBody<'db>>>;

    // Extern function.
    // ================
    /// Private query to compute data about an extern function declaration. An extern function has
    /// no body, and thus only has a declaration.
    #[salsa::transparent]
    #[salsa::invoke(items::extern_function::priv_extern_function_declaration_data_tracked)]
    fn priv_extern_function_declaration_data<'db>(
        &'db self,
        function_id: ExternFunctionId<'db>,
    ) -> Maybe<items::functions::FunctionDeclarationData<'db>>;
    /// Returns the inline configuration of an extern function's declaration.
    #[salsa::transparent]
    #[salsa::invoke(items::extern_function::extern_function_declaration_inline_config_tracked)]
    fn extern_function_declaration_inline_config<'db>(
        &'db self,
        extern_function_id: ExternFunctionId<'db>,
    ) -> Maybe<InlineConfiguration<'db>>;
    /// Returns the semantic diagnostics of an extern function declaration. An extern function has
    /// no body, and thus only has a declaration.
    #[salsa::transparent]
    #[salsa::invoke(items::extern_function::extern_function_declaration_diagnostics_tracked)]
    fn extern_function_declaration_diagnostics<'db>(
        &'db self,
        extern_function_id: ExternFunctionId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;
    /// Returns the signature of an extern function.
    #[salsa::transparent]
    #[salsa::invoke(items::extern_function::extern_function_signature_tracked)]
    fn extern_function_signature<'db>(
        &'db self,
        extern_function_id: ExternFunctionId<'db>,
    ) -> Maybe<semantic::Signature<'db>>;
    /// Returns the generic params of an extern function.
    #[salsa::transparent]
    #[salsa::invoke(items::extern_function::extern_function_declaration_generic_params_tracked)]
    fn extern_function_declaration_generic_params<'db>(
        &'db self,
        extern_function_id: ExternFunctionId<'db>,
    ) -> Maybe<Vec<GenericParam<'db>>>;
    /// Returns the generic params data of an extern function.
    #[salsa::transparent]
    #[salsa::invoke(
        items::extern_function::extern_function_declaration_generic_params_data_tracked
    )]
    fn extern_function_declaration_generic_params_data<'db>(
        &'db self,
        extern_function_id: ExternFunctionId<'db>,
    ) -> Maybe<GenericParamsData<'db>>;
    /// Returns the explicit implicits of an extern function declaration.
    #[salsa::transparent]
    #[salsa::invoke(items::extern_function::extern_function_declaration_implicits_tracked)]
    fn extern_function_declaration_implicits<'db>(
        &'db self,
        extern_function_id: ExternFunctionId<'db>,
    ) -> Maybe<Vec<TypeId<'db>>>;
    /// Returns the ref parameters of an extern function declaration.
    #[salsa::transparent]
    #[salsa::invoke(items::extern_function::extern_function_declaration_refs_tracked)]
    fn extern_function_declaration_refs<'db>(
        &'db self,
        extern_function_id: ExternFunctionId<'db>,
    ) -> Maybe<Vec<Parameter<'db>>>;
    /// Returns the resolution resolved_items of an extern function.
    #[salsa::transparent]
    #[salsa::invoke(items::extern_function::extern_function_declaration_resolver_data_tracked)]
    fn extern_function_declaration_resolver_data<'db>(
        &'db self,
        extern_function_id: ExternFunctionId<'db>,
    ) -> Maybe<Arc<ResolverData<'db>>>;

    // Extern type.
    // ============
    /// Private query to compute data about an extern type declaration. An extern type has
    /// no body, and thus only has a declaration.
    #[salsa::transparent]
    #[salsa::invoke(items::extern_type::priv_extern_type_declaration_data_tracked)]
    fn priv_extern_type_declaration_data<'db>(
        &'db self,
        type_id: ExternTypeId<'db>,
    ) -> Maybe<items::extern_type::ExternTypeDeclarationData<'db>>;
    /// Returns the semantic diagnostics of an extern type declaration. An extern type has
    /// no body, and thus only has a declaration.
    #[salsa::transparent]
    #[salsa::invoke(items::extern_type::extern_type_declaration_diagnostics_tracked)]
    fn extern_type_declaration_diagnostics<'db>(
        &'db self,
        extern_type_id: ExternTypeId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;
    /// Returns the generic params of an extern type.
    #[salsa::transparent]
    #[salsa::invoke(items::extern_type::extern_type_declaration_generic_params_tracked)]
    fn extern_type_declaration_generic_params<'db>(
        &'db self,
        extern_type_id: ExternTypeId<'db>,
    ) -> Maybe<Vec<GenericParam<'db>>>;
    /// Returns the generic params data of an extern type.
    #[salsa::transparent]
    #[salsa::invoke(items::extern_type::extern_type_declaration_generic_params_data_tracked)]
    fn extern_type_declaration_generic_params_data<'db>(
        &'db self,
        extern_type_id: ExternTypeId<'db>,
    ) -> Maybe<GenericParamsData<'db>>;

    /// Returns the attributes of an extern type.
    #[salsa::transparent]
    #[salsa::invoke(items::extern_type::extern_type_attributes_tracked)]
    fn extern_type_attributes<'db>(
        &'db self,
        extern_type_id: ExternTypeId<'db>,
    ) -> Maybe<Vec<Attribute<'db>>>;

    // Function Signature.
    // =================
    /// Returns the signature of the given FunctionTitleId. This include free functions, extern
    /// functions, etc...
    #[salsa::transparent]
    #[salsa::invoke(items::functions::function_title_signature_tracked)]
    fn function_title_signature<'db>(
        &'db self,
        function_title_id: FunctionTitleId<'db>,
    ) -> Maybe<semantic::Signature<'db>>;

    /// Returns the generic parameters of the given FunctionTitleId. This include free
    /// functions, extern functions, etc...
    #[salsa::transparent]
    #[salsa::invoke(items::functions::function_title_generic_params_tracked)]
    fn function_title_generic_params<'db>(
        &'db self,
        function_title_id: FunctionTitleId<'db>,
    ) -> Maybe<Vec<GenericParam<'db>>>;

    // Concrete function.
    // =================
    /// Returns the signature of a concrete function. This include free functions, extern functions,
    /// etc...
    #[salsa::transparent]
    #[salsa::invoke(items::functions::concrete_function_signature_tracked)]
    fn concrete_function_signature<'db>(
        &'db self,
        function_id: FunctionId<'db>,
    ) -> Maybe<semantic::Signature<'db>>;

    /// Returns a mapping of closure types to their associated parameter types for a concrete
    /// function.
    #[salsa::transparent]
    #[salsa::invoke(items::functions::concrete_function_closure_params_tracked)]
    fn concrete_function_closure_params<'db>(
        &'db self,
        function_id: FunctionId<'db>,
    ) -> Maybe<OrderedHashMap<semantic::TypeId<'db>, semantic::TypeId<'db>>>;

    /// Returns a mapping of closure types to their associated parameter types for a generic
    /// function.
    #[salsa::transparent]
    #[salsa::invoke(items::functions::get_closure_params_tracked)]
    fn get_closure_params<'db>(
        &'db self,
        generic_function_id: GenericFunctionId<'db>,
    ) -> Maybe<OrderedHashMap<TypeId<'db>, TypeId<'db>>>;
    // Macro Declaration.
    // =================
    /// Private query to compute data about a macro declaration.
    #[salsa::transparent]
    #[salsa::invoke(items::macro_declaration::priv_macro_declaration_data_tracked)]
    fn priv_macro_declaration_data<'db>(
        &'db self,
        macro_id: MacroDeclarationId<'db>,
    ) -> Maybe<MacroDeclarationData<'db>>;
    /// Returns the semantic diagnostics of a macro declaration.
    #[salsa::transparent]
    #[salsa::invoke(items::macro_declaration::macro_declaration_diagnostics_tracked)]
    fn macro_declaration_diagnostics<'db>(
        &'db self,
        macro_id: MacroDeclarationId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;
    /// Returns the resolver data of a macro declaration.
    #[salsa::transparent]
    #[salsa::invoke(items::macro_declaration::macro_declaration_resolver_data_tracked)]
    fn macro_declaration_resolver_data<'db>(
        &'db self,
        macro_id: MacroDeclarationId<'db>,
    ) -> Maybe<Arc<ResolverData<'db>>>;
    /// Returns the attributes of a macro declaration.
    #[salsa::transparent]
    #[salsa::invoke(items::macro_declaration::macro_declaration_attributes_tracked)]
    fn macro_declaration_attributes<'db>(
        &'db self,
        macro_id: MacroDeclarationId<'db>,
    ) -> Maybe<Vec<Attribute<'db>>>;
    /// Returns the rules semantic data of a macro declaration.
    #[salsa::transparent]
    #[salsa::invoke(items::macro_declaration::macro_declaration_rules_tracked)]
    fn macro_declaration_rules<'db>(
        &'db self,
        macro_id: MacroDeclarationId<'db>,
    ) -> Maybe<Vec<MacroRuleData<'db>>>;
    // Macro call.
    // ================
    /// Returns the semantic data of a macro call.
    #[salsa::transparent]
    #[salsa::invoke(items::macro_call::priv_macro_call_data_tracked)]
    fn priv_macro_call_data<'db>(
        &'db self,
        macro_call_id: MacroCallId<'db>,
    ) -> Maybe<items::macro_call::MacroCallData<'db>>;
    /// Returns the expansion result of a macro call.
    #[salsa::transparent]
    #[salsa::invoke(items::macro_call::macro_call_module_id_tracked)]
    fn macro_call_module_id<'db>(
        &'db self,
        macro_call_id: MacroCallId<'db>,
    ) -> Maybe<ModuleId<'db>>;
    /// Returns the semantic diagnostics of a macro call.
    #[salsa::transparent]
    #[salsa::invoke(items::macro_call::macro_call_diagnostics_tracked)]
    fn macro_call_diagnostics<'db>(
        &'db self,
        macro_call_id: MacroCallId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;

    // Generic type.
    // =============
    /// Returns the generic params of a generic type.
    #[salsa::transparent]
    #[salsa::invoke(types::generic_type_generic_params_tracked)]
    fn generic_type_generic_params<'db>(
        &'db self,
        generic_type: GenericTypeId<'db>,
    ) -> Maybe<Vec<GenericParam<'db>>>;

    // Generic param.
    // ==============
    /// Returns the semantic data of a generic param.
    #[salsa::transparent]
    #[salsa::invoke(items::generics::generic_param_semantic_tracked)]
    fn generic_param_semantic<'db>(
        &'db self,
        generic_param: GenericParamId<'db>,
    ) -> Maybe<GenericParam<'db>>;
    /// Returns the semantic diagnostics of a generic param.
    #[salsa::transparent]
    #[salsa::invoke(items::generics::generic_param_diagnostics_tracked)]
    fn generic_param_diagnostics<'db>(
        &'db self,
        generic_param: GenericParamId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;
    /// Returns the resolver data of a generic param.
    #[salsa::transparent]
    #[salsa::invoke(items::generics::generic_param_resolver_data_tracked)]
    fn generic_param_resolver_data<'db>(
        &'db self,
        generic_param: GenericParamId<'db>,
    ) -> Maybe<Arc<ResolverData<'db>>>;
    /// Returns the trait a generic param impl should implement.
    /// Panics if the generic param is not an impl generic param.
    #[salsa::transparent]
    #[salsa::invoke(items::generics::generic_impl_param_trait_tracked)]
    fn generic_impl_param_trait<'db>(
        &'db self,
        generic_param_id: GenericParamId<'db>,
    ) -> Maybe<TraitId<'db>>;

    /// Returns the shallow generic args of a generic impl param.
    #[salsa::invoke(items::generics::generic_impl_param_shallow_trait_generic_args)]
    #[salsa::transparent]
    fn generic_impl_param_shallow_trait_generic_args<'db>(
        &'db self,
        generic_param: GenericParamId<'db>,
    ) -> Maybe<&'db [(GenericParamId<'db>, ShallowGenericArg<'db>)]>;
    /// Private query to compute data about a generic param.
    #[salsa::transparent]
    #[salsa::invoke(items::generics::priv_generic_param_data_tracked)]
    fn priv_generic_param_data<'db>(
        &'db self,
        generic_param: GenericParamId<'db>,
        in_cycle: bool,
    ) -> Maybe<GenericParamData<'db>>;

    /// Returns the type constraints introduced by the generic params.
    #[salsa::transparent]
    #[salsa::invoke(items::generics::generic_params_type_constraints_tracked)]
    fn generic_params_type_constraints<'db>(
        &'db self,
        generic_params: Vec<GenericParamId<'db>>,
    ) -> Vec<(TypeId<'db>, TypeId<'db>)>;

    // Concrete type.
    // ==============
    /// Returns true if there is only one value for the given type and hence the values of the given
    /// type are all interchangeable.
    /// Examples include the unit type tuple of a unit type and empty structs.
    /// Always returns false for extern types.
    #[salsa::transparent]
    #[salsa::invoke(types::single_value_type_tracked)]
    fn single_value_type<'db>(&self, ty: types::TypeId<'db>) -> Maybe<bool>;

    /// Returns the type size information for the given type.
    #[salsa::transparent]
    #[salsa::invoke(types::type_size_info_tracked)]
    fn type_size_info<'db>(&self, ty: types::TypeId<'db>) -> Maybe<TypeSizeInformation>;

    /// Returns the type info for a type in a context.
    #[salsa::transparent]
    #[salsa::invoke(types::type_info_tracked)]
    fn type_info<'db>(
        &'db self,
        lookup_context: ImplLookupContextId<'db>,
        ty: types::TypeId<'db>,
    ) -> types::TypeInfo<'db>;

    /// Returns the `Copy` impl for a type in general context.
    #[salsa::transparent]
    #[salsa::invoke(types::copyable_tracked)]
    fn copyable<'db>(&'db self, ty: types::TypeId<'db>)
    -> Result<ImplId<'db>, InferenceError<'db>>;

    /// Returns the `Drop` impl for a type in general context.
    #[salsa::transparent]
    #[salsa::invoke(types::droppable_tracked)]
    fn droppable<'db>(
        &'db self,
        ty: types::TypeId<'db>,
    ) -> Result<ImplId<'db>, InferenceError<'db>>;

    /// Private query to check if a type is fully concrete.
    #[salsa::transparent]
    #[salsa::invoke(types::priv_type_is_fully_concrete_tracked)]
    fn priv_type_is_fully_concrete<'db>(&self, ty: types::TypeId<'db>) -> bool;

    /// Private query to check if a type contains no variables.
    #[salsa::transparent]
    #[salsa::invoke(types::priv_type_is_var_free_tracked)]
    fn priv_type_is_var_free<'db>(&self, ty: types::TypeId<'db>) -> bool;

    /// Private query for a shorter unique name for types.
    #[salsa::transparent]
    #[salsa::invoke(types::priv_type_short_name_tracked)]
    fn priv_type_short_name<'db>(&self, ty: types::TypeId<'db>) -> String;

    // Expression.
    // ===========
    // Lookups.
    // ========
    fn lookup_resolved_generic_item_by_ptr<'db>(
        &'db self,
        id: LookupItemId<'db>,
        ptr: ast::TerminalIdentifierPtr<'db>,
    ) -> Option<ResolvedGenericItem<'db>>;
    fn lookup_resolved_concrete_item_by_ptr<'db>(
        &'db self,
        id: LookupItemId<'db>,
        ptr: ast::TerminalIdentifierPtr<'db>,
    ) -> Option<ResolvedConcreteItem<'db>>;

    // Diagnostics.
    // ============
    /// Aggregates module level semantic diagnostics.
    fn module_semantic_diagnostics<'db>(
        &'db self,
        module_id: ModuleId<'db>,
    ) -> Maybe<Diagnostics<'db, SemanticDiagnostic<'db>>>;

    /// Aggregates file level semantic diagnostics.
    fn file_semantic_diagnostics<'db>(
        &'db self,
        file_id: FileId<'db>,
    ) -> Maybe<Diagnostics<'db, SemanticDiagnostic<'db>>>;

    // Corelib.
    // ========
    #[salsa::transparent]
    #[salsa::invoke(corelib::core_crate_tracked)]
    fn core_crate<'db>(&'db self) -> CrateId<'db>;
    #[salsa::transparent]
    #[salsa::invoke(corelib::core_module_tracked)]
    fn core_module<'db>(&'db self) -> ModuleId<'db>;
    #[salsa::transparent]
    #[salsa::invoke(corelib::core_info_tracked)]
    fn core_info<'db>(&'db self) -> Arc<CoreInfo<'db>>;

    // Analyzer plugins.
    // ========

    #[salsa::transparent]
    fn default_analyzer_plugins_input(&self) -> &[AnalyzerPluginLongId];

    /// Interned version of `default_analyzer_plugins`.
    fn default_analyzer_plugins<'db>(&'db self) -> Arc<Vec<AnalyzerPluginId<'db>>>;

    #[salsa::transparent]
    fn analyzer_plugin_overrides_input(
        &self,
    ) -> &OrderedHashMap<CrateInput, Arc<[AnalyzerPluginLongId]>>;

    /// Interned version of `analyzer_plugin_overrides_input`.
    #[salsa::transparent]
    fn analyzer_plugin_overrides<'db>(
        &'db self,
    ) -> Arc<OrderedHashMap<CrateId<'db>, Arc<Vec<AnalyzerPluginId<'db>>>>>;

    /// Returns [`AnalyzerPluginId`]s of the plugins set for the crate with [`CrateId`].
    /// Returns
    /// [`SemanticGroupEx::set_override_crate_analyzer_plugins`] if it has been set,
    /// or the ([`SemanticGroup::default_analyzer_plugins`]) otherwise.
    #[salsa::transparent]
    fn crate_analyzer_plugins<'db>(
        &'db self,
        crate_id: CrateId<'db>,
    ) -> Arc<Vec<AnalyzerPluginId<'db>>>;

    /// Returns the set of `allow` that were declared as by a plugin.
    /// An allow that is not in this set will be handled as an unknown allow.
    #[salsa::transparent]
    fn declared_allows<'db>(&self, crate_id: CrateId<'db>) -> Arc<OrderedHashSet<String>>;

    // Helpers for language server.
    // ============================
    /// Returns all methods in a module that match the given type filter.
    #[salsa::transparent]
    #[salsa::invoke(lsp_helpers::methods_in_module_tracked)]
    fn methods_in_module<'db>(
        &'db self,
        module_id: ModuleId<'db>,
        type_filter: lsp_helpers::TypeFilter<'db>,
    ) -> Arc<Vec<TraitFunctionId<'db>>>;
    /// Returns all methods in a crate that match the given type filter.
    #[salsa::transparent]
    #[salsa::invoke(lsp_helpers::methods_in_crate_tracked)]
    fn methods_in_crate<'db>(
        &'db self,
        crate_id: CrateId<'db>,
        type_filter: lsp_helpers::TypeFilter<'db>,
    ) -> Arc<Vec<TraitFunctionId<'db>>>;
    /// Returns all the importables visible from a module, alongside a visible use path to the
    /// trait.
    #[salsa::transparent]
    #[salsa::invoke(lsp_helpers::visible_importables_from_module_tracked)]
    fn visible_importables_from_module<'db>(
        &'db self,
        module_id: ModuleFileId<'db>,
    ) -> Option<Arc<OrderedHashMap<ImportableId<'db>, String>>>;
    /// Returns all visible importables in a module, alongside a visible use path to the trait.
    /// `user_module_file_id` is the module from which the importables should be visible. If
    /// `include_parent` is true, the parent module of `module_id` is also considered.
    #[salsa::transparent]
    #[salsa::invoke(lsp_helpers::visible_importables_in_module_tracked)]
    fn visible_importables_in_module<'db>(
        &'db self,
        module_id: ModuleId<'db>,
        user_module_file_id: ModuleFileId<'db>,
        include_parent: bool,
    ) -> Arc<Vec<(ImportableId<'db>, String)>>;
    /// Returns all visible importables in a crate, alongside a visible use path to the trait.
    /// `user_module_file_id` is the module from which the importables should be visible.
    #[salsa::transparent]
    #[salsa::invoke(lsp_helpers::visible_importables_in_crate_tracked)]
    fn visible_importables_in_crate<'db>(
        &'db self,
        crate_id: CrateId<'db>,
        user_module_file_id: ModuleFileId<'db>,
    ) -> Arc<Vec<(ImportableId<'db>, String)>>;
    /// Returns all the traits visible from a module, alongside a visible use path to the trait.
    #[salsa::transparent]
    #[salsa::invoke(lsp_helpers::visible_traits_from_module_tracked)]
    fn visible_traits_from_module<'db>(
        &'db self,
        module_id: ModuleFileId<'db>,
    ) -> Option<Arc<OrderedHashMap<TraitId<'db>, String>>>;
}

/// Initializes the [`SemanticGroup`] database to a proper state.
pub fn init_semantic_group(db: &mut dyn SemanticGroup) {
    let db_ref = db.as_dyn_database_mut();
    semantic_group_input(db_ref)
        .set_analyzer_plugin_overrides(db_ref)
        .to(Some(OrderedHashMap::default()));
}

fn default_analyzer_plugins(db: &dyn SemanticGroup) -> Arc<Vec<AnalyzerPluginId<'_>>> {
    let inp = db.default_analyzer_plugins_input();
    Arc::new(inp.iter().map(|plugin| plugin.clone().intern(db)).collect_vec())
}

#[salsa::tracked]
fn analyzer_plugin_overrides(
    db: &dyn SemanticGroup,
) -> Arc<OrderedHashMap<CrateId<'_>, Arc<Vec<AnalyzerPluginId<'_>>>>> {
    let inp = db.analyzer_plugin_overrides_input();
    Arc::new(
        inp.iter()
            .map(|(crate_input, plugins)| {
                (
                    crate_input.clone().into_crate_long_id(db).intern(db),
                    Arc::new(plugins.iter().map(|plugin| plugin.clone().intern(db)).collect_vec()),
                )
            })
            .collect(),
    )
}

fn module_semantic_diagnostics<'db>(
    db: &'db dyn SemanticGroup,
    module_id: ModuleId<'db>,
) -> Maybe<Diagnostics<'db, SemanticDiagnostic<'db>>> {
    let mut diagnostics = DiagnosticsBuilder::default();
    for (_module_file_id, plugin_diag) in
        module_id.module_data(db)?.plugin_diagnostics(db).iter().cloned()
    {
        diagnostics.add(SemanticDiagnostic::new(
            match plugin_diag.inner_span {
                None => StableLocation::new(plugin_diag.stable_ptr),
                Some(inner_span) => {
                    StableLocation::with_inner_span(plugin_diag.stable_ptr, inner_span)
                }
            },
            SemanticDiagnosticKind::PluginDiagnostic(plugin_diag),
        ));
    }
    let data = db.priv_module_semantic_data(module_id)?;
    diagnostics.extend(data.diagnostics.clone());
    // TODO(Gil): Aggregate diagnostics for subitems with semantic model (i.e. impl function, trait
    // functions and generic params) directly and not via the parent item.
    for item in module_id.module_data(db)?.items(db).iter() {
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
                if let Ok(file_id) = db.module_main_file(ModuleId::Submodule(*submodule_id))
                    && db.file_content(file_id).is_none()
                {
                    // Note that the error location is in the parent module, not the
                    // submodule.

                    let path = match file_id.long(db) {
                        FileLongId::OnDisk(path) => path.display().to_string(),
                        FileLongId::Virtual(_) | FileLongId::External(_) => {
                            panic!("Expected OnDisk file.")
                        }
                    };

                    let stable_location =
                        StableLocation::new(submodule_id.stable_ptr(db).untyped());
                    diagnostics.add(SemanticDiagnostic::new(
                        stable_location,
                        SemanticDiagnosticKind::ModuleFileNotFound(path),
                    ));
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
            ModuleItemId::MacroDeclaration(macro_declaration) => {
                diagnostics.extend(db.macro_declaration_diagnostics(*macro_declaration));
            }
        }
    }
    for global_use in module_id.module_data(db)?.global_uses(db).keys() {
        diagnostics.extend(db.global_use_semantic_diagnostics(*global_use));
    }
    for macro_call in db.module_macro_calls_ids(module_id)?.iter() {
        diagnostics.extend(db.macro_call_diagnostics(*macro_call));
        if let Ok(macro_module_id) = db.macro_call_module_id(*macro_call)
            && let Ok(semantic_diags) = db.module_semantic_diagnostics(macro_module_id)
        {
            diagnostics.extend(semantic_diags);
        }
    }
    add_unused_item_diagnostics(db, module_id, &data, &mut diagnostics);
    for analyzer_plugin_id in db.crate_analyzer_plugins(module_id.owning_crate(db)).iter() {
        let analyzer_plugin = analyzer_plugin_id.long(db);

        for diag in analyzer_plugin.diagnostics(db, module_id) {
            diagnostics.add(SemanticDiagnostic::new(
                StableLocation::new(diag.stable_ptr),
                SemanticDiagnosticKind::PluginDiagnostic(diag),
            ));
        }
    }

    Ok(diagnostics.build())
}

#[salsa::tracked]
fn crate_analyzer_plugins<'db>(
    db: &'db dyn SemanticGroup,
    crate_id: CrateId<'db>,
) -> Arc<Vec<AnalyzerPluginId<'db>>> {
    db.analyzer_plugin_overrides()
        .get(&crate_id)
        .cloned()
        .unwrap_or_else(|| db.default_analyzer_plugins())
}

#[salsa::tracked]
fn declared_allows(db: &dyn SemanticGroup, crate_id: CrateId<'_>) -> Arc<OrderedHashSet<String>> {
    let base_lints = [DEPRECATED_ATTR, UNUSED_IMPORTS, UNUSED_VARIABLES];

    let crate_analyzer_plugins = db.crate_analyzer_plugins(crate_id);

    Arc::new(OrderedHashSet::from_iter(chain!(
        base_lints.map(|attr| attr.into()),
        crate_analyzer_plugins.iter().flat_map(|plugin| plugin.long(db).declared_allows())
    )))
}

/// Adds diagnostics for unused items in a module.
///
/// Returns `None` if skipped attempt to add diagnostics.
fn add_unused_item_diagnostics<'db>(
    db: &'db dyn SemanticGroup,
    module_id: ModuleId<'db>,
    data: &ModuleSemanticData<'db>,
    diagnostics: &mut DiagnosticsBuilder<'db, SemanticDiagnostic<'db>>,
) {
    let Ok(all_used_uses) = db.module_all_used_uses(module_id) else {
        return;
    };
    for info in data.items.values() {
        if info.visibility == Visibility::Public {
            continue;
        }
        if let ModuleItemId::Use(use_id) = info.item_id {
            add_unused_import_diagnostics(db, &all_used_uses, use_id, diagnostics);
        };
    }
}

/// Adds diagnostics for unused imports.
fn add_unused_import_diagnostics<'db>(
    db: &'db dyn SemanticGroup,
    all_used_uses: &OrderedHashSet<UseId<'db>>,
    use_id: UseId<'db>,
    diagnostics: &mut DiagnosticsBuilder<'db, SemanticDiagnostic<'db>>,
) {
    let _iife = (|| {
        let item = db.use_resolved_item(use_id).ok()?;
        // TODO(orizi): Properly handle usages of impls, and than add warnings on their usages as
        // well.
        require(!matches!(
            item,
            ResolvedGenericItem::Impl(_) | ResolvedGenericItem::GenericImplAlias(_)
        ))?;
        require(!all_used_uses.contains(&use_id))?;
        let resolver_data = db.use_resolver_data(use_id).ok()?;
        require(!resolver_data.feature_config.allowed_lints.contains(UNUSED_IMPORTS))?;
        Some(diagnostics.add(SemanticDiagnostic::new(
            StableLocation::new(use_id.untyped_stable_ptr(db)),
            SemanticDiagnosticKind::UnusedImport(use_id),
        )))
    })();
}

fn file_semantic_diagnostics<'db>(
    db: &'db dyn SemanticGroup,
    file_id: FileId<'db>,
) -> Maybe<Diagnostics<'db, SemanticDiagnostic<'db>>> {
    let mut diagnostics = DiagnosticsBuilder::default();
    for module_id in db.file_modules(file_id)?.iter().copied() {
        if let Ok(module_diagnostics) = db.module_semantic_diagnostics(module_id) {
            diagnostics.extend(module_diagnostics)
        }
    }
    Ok(diagnostics.build())
}

pub fn lookup_resolved_generic_item_by_ptr<'db>(
    db: &'db dyn SemanticGroup,
    id: LookupItemId<'db>,
    ptr: ast::TerminalIdentifierPtr<'db>,
) -> Option<ResolvedGenericItem<'db>> {
    get_resolver_data_options(id, db)
        .into_iter()
        .find_map(|resolver_data| resolver_data.resolved_items.generic.get(&ptr).cloned())
}

pub fn lookup_resolved_concrete_item_by_ptr<'db>(
    db: &'db dyn SemanticGroup,
    id: LookupItemId<'db>,
    ptr: ast::TerminalIdentifierPtr<'db>,
) -> Option<ResolvedConcreteItem<'db>> {
    get_resolver_data_options(id, db)
        .into_iter()
        .find_map(|resolver_data| resolver_data.resolved_items.concrete.get(&ptr).cloned())
}

pub fn get_resolver_data_options<'db>(
    id: LookupItemId<'db>,
    db: &'db dyn SemanticGroup,
) -> Vec<Arc<ResolverData<'db>>> {
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
            ModuleItemId::Trait(id) => vec![db.trait_resolver_data(id)],
            ModuleItemId::Impl(id) => vec![db.impl_def_resolver_data(id)],
            ModuleItemId::ExternType(_) => vec![],
            ModuleItemId::ExternFunction(id) => {
                vec![db.extern_function_declaration_resolver_data(id)]
            }
            ModuleItemId::MacroDeclaration(id) => vec![db.macro_declaration_resolver_data(id)],
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
        crate_id: CrateId<'_>,
        plugins: Arc<[AnalyzerPluginId<'_>]>,
    ) {
        let mut overrides = self.analyzer_plugin_overrides_input().clone();
        let plugins = plugins.iter().map(|plugin| plugin.long(self).clone()).collect_vec();
        overrides.insert(self.crate_input(crate_id).clone(), Arc::from(plugins));
        let db_ref = self.as_dyn_database_mut();
        semantic_group_input(db_ref).set_analyzer_plugin_overrides(db_ref).to(Some(overrides));
    }
}

impl dyn SemanticGroup {
    /// Returns the body expr of a function (with a body).
    pub fn function_body_expr<'db>(
        &'db self,
        function_id: FunctionWithBodyId<'db>,
    ) -> Maybe<semantic::ExprId> {
        items::function_with_body::function_body_expr(self, function_id)
    }

    /// Assumes function and expression are present.
    pub fn expr_semantic<'db>(
        &'db self,
        function_id: FunctionWithBodyId<'db>,
        id: semantic::ExprId,
    ) -> semantic::Expr<'db> {
        items::function_with_body::expr_semantic(self, function_id, id)
    }

    /// Assumes function and statement are valid.
    pub fn statement_semantic<'db>(
        &'db self,
        function_id: FunctionWithBodyId<'db>,
        id: semantic::StatementId,
    ) -> semantic::Statement<'db> {
        items::function_with_body::statement_semantic(self, function_id, id)
    }

    /// Assumes function and pattern are present.
    pub fn pattern_semantic<'db>(
        &'db self,
        function_id: FunctionWithBodyId<'db>,
        id: semantic::PatternId,
    ) -> semantic::Pattern<'db> {
        items::function_with_body::pattern_semantic(self, function_id, id)
    }
}

impl<T: SemanticGroup + ?Sized> SemanticGroupEx for T {}

/// An extension trait for [`SemanticGroup`] to manage plugin setters.
pub trait PluginSuiteInput: SemanticGroup {
    /// Interns each plugin from the [`PluginSuite`] into the database.
    fn intern_plugin_suite<'r>(&'r mut self, suite: PluginSuite) -> InternedPluginSuite<'r> {
        let PluginSuite { plugins, inline_macro_plugins, analyzer_plugins } = suite;

        let macro_plugins = plugins
            .into_iter()
            .map(|plugin| MacroPluginId::new(self, MacroPluginLongId(plugin)))
            .collect::<Arc<[_]>>();

        let inline_macro_plugins = Arc::new(
            inline_macro_plugins
                .into_iter()
                .map(|(name, plugin)| {
                    (name, InlineMacroExprPluginId::new(self, InlineMacroExprPluginLongId(plugin)))
                })
                .collect::<OrderedHashMap<_, _>>(),
        );

        let analyzer_plugins = analyzer_plugins
            .into_iter()
            .map(|plugin| AnalyzerPluginId::new(self, AnalyzerPluginLongId(plugin)))
            .collect::<Arc<[_]>>();

        InternedPluginSuite { macro_plugins, inline_macro_plugins, analyzer_plugins }
    }

    /// Sets macro, inline macro and analyzer plugins specified in the [`PluginSuite`] as default
    /// for all crates.
    ///
    /// *Note*: Sets the following Salsa inputs: [`DefsGroup::default_macro_plugins`],
    /// [`DefsGroup::default_inline_macro_plugins`], and
    /// [`SemanticGroup::default_analyzer_plugins`].
    fn set_default_plugins_from_suite(&mut self, suite: PluginSuite) {
        let PluginSuite { plugins, inline_macro_plugins, analyzer_plugins } = suite;
        // let interned = self.intern_plugin_suite(suite);

        let macro_plugins = plugins.into_iter().map(MacroPluginLongId).collect_vec();

        let inline_macro_plugins = inline_macro_plugins
            .into_iter()
            .map(|(name, plugin)| (name, InlineMacroExprPluginLongId(plugin)))
            .collect::<OrderedHashMap<_, _>>();

        let analyzer_plugins =
            analyzer_plugins.into_iter().map(AnalyzerPluginLongId).collect::<Vec<_>>();

        let db_ref = self.as_dyn_database_mut();
        defs_group_input(db_ref).set_default_macro_plugins(db_ref).to(Some(macro_plugins));
        defs_group_input(db_ref)
            .set_default_inline_macro_plugins(db_ref)
            .to(Some(inline_macro_plugins));
        semantic_group_input(db_ref)
            .set_default_analyzer_plugins(db_ref)
            .to(Some(analyzer_plugins));
    }

    /// Sets macro, inline macro and analyzer plugins present in the [`PluginSuite`] for a crate
    /// pointed to by the [`CrateId`], overriding the defaults for that crate.
    ///
    /// *Note*: Sets the following Salsa inputs: [`DefsGroup::macro_plugin_overrides`],
    /// [`DefsGroup::inline_macro_plugin_overrides`], and
    /// [`SemanticGroup::analyzer_plugin_overrides`].
    fn set_override_crate_plugins_from_suite(
        &mut self,
        crate_id: CrateId<'_>,
        suite: InternedPluginSuite<'_>,
    ) {
        let InternedPluginSuite { macro_plugins, inline_macro_plugins, analyzer_plugins } = suite;

        self.set_override_crate_macro_plugins(crate_id, Arc::new(macro_plugins.to_vec()));
        self.set_override_crate_inline_macro_plugins(crate_id, inline_macro_plugins);
        self.set_override_crate_analyzer_plugins(crate_id, analyzer_plugins);
    }
}

impl<T: SemanticGroup + ?Sized> PluginSuiteInput for T {}

/// Returns all ancestors (parents) of the given module, including the module itself, in order from
/// closest to farthest.
pub fn module_ancestors<'db>(
    db: &'db dyn Database,
    mut module_id: ModuleId<'db>,
) -> Vec<ModuleId<'db>> {
    let mut ancestors = Vec::new();
    ancestors.push(module_id); // Include the module itself first
    while let ModuleId::Submodule(submodule_id) = module_id {
        let parent = submodule_id.parent_module(db);
        ancestors.push(parent);
        module_id = parent;
    }
    ancestors
}

/// Returns all ancestors (parents) and all macro-generated modules within them.
pub fn module_fully_accessible_modules<'db>(
    db: &'db dyn SemanticGroup,
    module_id: ModuleId<'db>,
) -> OrderedHashSet<ModuleId<'db>> {
    let mut result: Vec<ModuleId<'db>> = module_ancestors(db, module_id);
    let mut index = 0;
    while let Some(curr) = result.get(index).copied() {
        index += 1;
        if let Ok(macro_call_ids) = db.module_macro_calls_ids(curr) {
            for macro_call_id in macro_call_ids.iter() {
                if let Ok(generated_module_id) = db.macro_call_module_id(*macro_call_id) {
                    result.push(generated_module_id);
                }
            }
        }
    }
    result.into_iter().collect()
}
