use std::collections::BTreeMap;
use std::sync::Arc;

use cairo_lang_defs::db::{DefsGroup, DefsGroupEx};
use cairo_lang_defs::diagnostic_utils::StableLocation;
use cairo_lang_defs::ids::{
    ConstantId, EnumId, ExternFunctionId, ExternTypeId, FreeFunctionId, FunctionTitleId,
    FunctionWithBodyId, GenericParamId, GenericTypeId, GlobalUseId, ImplAliasId, ImplConstantDefId,
    ImplDefId, ImplFunctionId, ImplImplDefId, ImplItemId, ImplTypeDefId, ImportableId,
    InlineMacroExprPluginLongId, LanguageElementId, LookupItemId, MacroCallId, MacroDeclarationId,
    MacroPluginLongId, ModuleFileId, ModuleId, ModuleItemId, ModuleTypeAliasId, StructId,
    TraitConstantId, TraitFunctionId, TraitId, TraitImplId, TraitItemId, TraitTypeId, UseId,
    VariantId,
};
use cairo_lang_diagnostics::{Diagnostics, DiagnosticsBuilder, Maybe};
use cairo_lang_filesystem::ids::{CrateId, CrateInput, FileId, FileLongId, StrRef};
use cairo_lang_parser::db::ParserGroup;
use cairo_lang_syntax::attribute::structured::Attribute;
use cairo_lang_syntax::node::{TypedStablePtr, ast};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use cairo_lang_utils::{Intern, Upcast, require};
use itertools::Itertools;

use crate::corelib::CoreInfo;
use crate::diagnostic::SemanticDiagnosticKind;
use crate::expr::inference::{self, ImplVar, ImplVarId, InferenceError};
use crate::ids::{AnalyzerPluginId, AnalyzerPluginLongId};
use crate::items::constant::{ConstCalcInfo, ConstValueId, Constant, ImplConstantId};
use crate::items::function_with_body::FunctionBody;
use crate::items::functions::{GenericFunctionId, ImplicitPrecedence, InlineConfiguration};
use crate::items::generics::{GenericParam, GenericParamData, GenericParamsData};
use crate::items::imp::{
    ImplId, ImplImplId, ImplItemInfo, ImplLookupContext, ImplicitImplImplData, UninferredImpl,
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
#[cairo_lang_proc_macros::query_group]
pub trait SemanticGroup:
    DefsGroup + for<'db> Upcast<'db, dyn DefsGroup> + for<'db> Upcast<'db, dyn ParserGroup> + Elongate
{
    #[salsa::interned]
    fn intern_function<'db>(
        &'db self,
        id: items::functions::FunctionLongId<'db>,
    ) -> semantic::FunctionId<'db>;
    #[salsa::interned]
    fn intern_concrete_function_with_body<'db>(
        &'db self,
        id: items::functions::ConcreteFunctionWithBody<'db>,
    ) -> semantic::ConcreteFunctionWithBodyId<'db>;
    #[salsa::interned]
    fn intern_concrete_struct<'db>(
        &'db self,
        id: types::ConcreteStructLongId<'db>,
    ) -> types::ConcreteStructId<'db>;
    #[salsa::interned]
    fn intern_concrete_enum<'db>(
        &'db self,
        id: types::ConcreteEnumLongId<'db>,
    ) -> types::ConcreteEnumId<'db>;
    #[salsa::interned]
    fn intern_concrete_extern_type<'db>(
        &'db self,
        id: types::ConcreteExternTypeLongId<'db>,
    ) -> types::ConcreteExternTypeId<'db>;
    #[salsa::interned]
    fn intern_concrete_trait<'db>(
        &'db self,
        id: items::trt::ConcreteTraitLongId<'db>,
    ) -> items::trt::ConcreteTraitId<'db>;
    #[salsa::interned]
    fn intern_concrete_trait_function<'db>(
        &'db self,
        id: items::trt::ConcreteTraitGenericFunctionLongId<'db>,
    ) -> items::trt::ConcreteTraitGenericFunctionId<'db>;
    #[salsa::interned]
    fn intern_concrete_trait_type<'db>(
        &'db self,
        id: items::trt::ConcreteTraitTypeLongId<'db>,
    ) -> items::trt::ConcreteTraitTypeId<'db>;
    #[salsa::interned]
    fn intern_concrete_trait_constant<'db>(
        &'db self,
        id: items::trt::ConcreteTraitConstantLongId<'db>,
    ) -> items::trt::ConcreteTraitConstantId<'db>;
    #[salsa::interned]
    fn intern_concrete_impl<'db>(
        &'db self,
        id: items::imp::ConcreteImplLongId<'db>,
    ) -> items::imp::ConcreteImplId<'db>;
    #[salsa::interned]
    fn intern_concrete_trait_impl<'db>(
        &'db self,
        id: items::trt::ConcreteTraitImplLongId<'db>,
    ) -> items::trt::ConcreteTraitImplId<'db>;
    #[salsa::interned]
    fn intern_type<'db>(&'db self, id: types::TypeLongId<'db>) -> semantic::TypeId<'db>;
    #[salsa::interned]
    fn intern_const_value<'db>(
        &'db self,
        id: items::constant::ConstValue<'db>,
    ) -> items::constant::ConstValueId<'db>;
    #[salsa::interned]
    fn intern_impl<'db>(&'db self, id: items::imp::ImplLongId<'db>) -> items::imp::ImplId<'db>;
    #[salsa::interned]
    fn intern_impl_var<'db>(&'db self, id: ImplVar<'db>) -> ImplVarId<'db>;

    #[salsa::interned]
    fn intern_generated_impl<'db>(
        &'db self,
        id: items::imp::GeneratedImplLongId<'db>,
    ) -> items::imp::GeneratedImplId<'db>;

    #[salsa::interned]
    fn intern_uninferred_generated_impl<'db>(
        &'db self,
        id: items::imp::UninferredGeneratedImplLongId<'db>,
    ) -> items::imp::UninferredGeneratedImplId<'db>;

    // Const.
    // ====
    /// Private query to compute data about a constant definition.
    #[salsa::invoke(items::constant::priv_constant_semantic_data)]
    #[salsa::cycle(items::constant::priv_constant_semantic_data_cycle)]
    fn priv_constant_semantic_data<'db>(
        &'db self,
        _const_id: ConstantId<'db>,
        _in_cycle: bool,
    ) -> Maybe<items::constant::ConstantData<'db>>;
    /// Returns the semantic diagnostics of a constant definition.
    #[salsa::invoke(items::constant::constant_semantic_diagnostics)]
    fn constant_semantic_diagnostics<'db>(
        &'db self,
        const_id: ConstantId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;
    /// Returns the semantic data of a constant definition.
    #[salsa::invoke(items::constant::constant_semantic_data)]
    #[salsa::cycle(items::constant::constant_semantic_data_cycle)]
    fn constant_semantic_data<'db>(&'db self, use_id: ConstantId<'db>) -> Maybe<Constant<'db>>;
    #[salsa::invoke(items::constant::constant_resolver_data)]
    #[salsa::cycle(items::constant::constant_resolver_data_cycle)]
    fn constant_resolver_data<'db>(
        &'db self,
        use_id: ConstantId<'db>,
    ) -> Maybe<Arc<ResolverData<'db>>>;
    #[salsa::invoke(items::constant::constant_const_value)]
    #[salsa::cycle(items::constant::constant_const_value_cycle)]
    fn constant_const_value<'db>(&'db self, const_id: ConstantId<'db>) -> Maybe<ConstValueId<'db>>;
    #[salsa::invoke(items::constant::constant_const_type)]
    #[salsa::cycle(items::constant::constant_const_type_cycle)]
    fn constant_const_type<'db>(&'db self, const_id: ConstantId<'db>) -> Maybe<TypeId<'db>>;
    /// Returns information required for const calculations.
    #[salsa::invoke(items::constant::const_calc_info)]
    fn const_calc_info<'db>(&'db self) -> Arc<ConstCalcInfo<'db>>;

    // Use.
    // ====
    /// Private query to compute data about a use.
    #[salsa::invoke(items::us::priv_use_semantic_data)]
    #[salsa::cycle(items::us::priv_use_semantic_data_cycle)]
    fn priv_use_semantic_data<'db>(&'db self, use_id: UseId<'db>) -> Maybe<Arc<UseData<'db>>>;
    /// Returns the semantic diagnostics of a use.
    #[salsa::invoke(items::us::use_semantic_diagnostics)]
    fn use_semantic_diagnostics<'db>(
        &'db self,
        use_id: UseId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;
    #[salsa::invoke(items::us::use_resolver_data)]
    #[salsa::cycle(items::us::use_resolver_data_cycle)]
    fn use_resolver_data<'db>(&'db self, use_id: UseId<'db>) -> Maybe<Arc<ResolverData<'db>>>;

    // Global Use.
    // ====
    /// Private query to compute data about a global use.
    #[salsa::invoke(items::us::priv_global_use_semantic_data)]
    #[salsa::cycle(items::us::priv_global_use_semantic_data_cycle)]
    fn priv_global_use_semantic_data<'db>(
        &'db self,
        global_use_id: GlobalUseId<'db>,
    ) -> Maybe<items::us::UseGlobalData<'db>>;
    /// Private query to compute the imported module, given a global use.
    #[salsa::invoke(items::us::priv_global_use_imported_module)]
    fn priv_global_use_imported_module<'db>(
        &'db self,
        global_use_id: GlobalUseId<'db>,
    ) -> Maybe<ModuleId<'db>>;
    /// Returns the semantic diagnostics of a global use.
    #[salsa::invoke(items::us::global_use_semantic_diagnostics)]
    fn global_use_semantic_diagnostics<'db>(
        &'db self,
        global_use_id: GlobalUseId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;
    /// Private query to compute the imported modules of a module, using global uses.
    #[salsa::invoke(items::us::priv_module_use_star_modules)]
    fn priv_module_use_star_modules<'db>(
        &'db self,
        module_id: ModuleId<'db>,
    ) -> Arc<ImportedModules<'db>>;

    // Module.
    // ====

    /// Private query to compute data about the module.
    #[salsa::invoke(items::module::priv_module_semantic_data)]
    fn priv_module_semantic_data<'db>(
        &'db self,
        module_id: ModuleId<'db>,
    ) -> Maybe<Arc<ModuleSemanticData<'db>>>;

    /// Returns [Maybe::Err] if the module was not properly resolved.
    /// Returns [Maybe::Ok(None)] if the item does not exist.
    #[salsa::invoke(items::module::module_item_by_name)]
    fn module_item_by_name<'db>(
        &'db self,
        module_id: ModuleId<'db>,
        name: StrRef<'db>,
    ) -> Maybe<Option<ModuleItemId<'db>>>;

    /// Returns [Maybe::Err] if the module was not properly resolved.
    /// Returns [Maybe::Ok(None)] if the item does not exist.
    #[salsa::invoke(items::module::module_item_info_by_name)]
    fn module_item_info_by_name<'db>(
        &'db self,
        module_id: ModuleId<'db>,
        name: StrRef<'db>,
    ) -> Maybe<Option<ModuleItemInfo<'db>>>;

    /// Returns all the items used within the module.
    #[salsa::invoke(items::module::module_all_used_uses)]
    fn module_all_used_uses<'db>(
        &'db self,
        module_id: ModuleId<'db>,
    ) -> Maybe<Arc<OrderedHashSet<UseId<'db>>>>;

    /// Returns the attributes of a module.
    #[salsa::invoke(items::module::module_attributes)]
    fn module_attributes<'db>(&'db self, module_id: ModuleId<'db>) -> Maybe<Vec<Attribute<'db>>>;

    /// Finds all the trait ids usable in the module.
    #[salsa::invoke(items::module::module_usable_trait_ids)]
    fn module_usable_trait_ids<'db>(
        &'db self,
        module_id: ModuleId<'db>,
    ) -> Maybe<Arc<OrderedHashMap<TraitId<'db>, LookupItemId<'db>>>>;

    // Struct.
    // =======
    /// Private query to compute data about a struct declaration.
    #[salsa::invoke(items::structure::priv_struct_declaration_data)]
    fn priv_struct_declaration_data<'db>(
        &'db self,
        struct_id: StructId<'db>,
    ) -> Maybe<items::structure::StructDeclarationData<'db>>;
    /// Returns the declaration diagnostics of a struct.
    #[salsa::invoke(items::structure::struct_declaration_diagnostics)]
    fn struct_declaration_diagnostics<'db>(
        &'db self,
        struct_id: StructId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;
    /// Returns the attributes of a struct.
    #[salsa::invoke(items::structure::struct_attributes)]
    fn struct_attributes<'db>(&'db self, struct_id: StructId<'db>) -> Maybe<Vec<Attribute<'db>>>;
    /// Returns the generic parameters of an enum.
    #[salsa::invoke(items::structure::struct_generic_params)]
    fn struct_generic_params<'db>(
        &'db self,
        struct_id: StructId<'db>,
    ) -> Maybe<Vec<GenericParam<'db>>>;
    /// Returns the generic parameters data of an enum.
    #[salsa::invoke(items::structure::struct_generic_params_data)]
    fn struct_generic_params_data<'db>(
        &'db self,
        struct_id: StructId<'db>,
    ) -> Maybe<GenericParamsData<'db>>;
    /// Returns the resolution resolved_items of a struct declaration.
    #[salsa::invoke(items::structure::struct_declaration_resolver_data)]
    fn struct_declaration_resolver_data<'db>(
        &'db self,
        structure_id: StructId<'db>,
    ) -> Maybe<Arc<ResolverData<'db>>>;

    /// Private query to compute data about a struct definition.
    #[salsa::invoke(items::structure::priv_struct_definition_data)]
    fn priv_struct_definition_data<'db>(
        &'db self,
        struct_id: StructId<'db>,
    ) -> Maybe<items::structure::StructDefinitionData<'db>>;
    /// Returns the semantic diagnostics of a struct definition.
    #[salsa::invoke(items::structure::struct_definition_diagnostics)]
    fn struct_definition_diagnostics<'db>(
        &'db self,
        struct_id: StructId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;
    /// Returns the members of a struct.
    #[salsa::invoke(items::structure::struct_members)]
    fn struct_members<'db>(
        &'db self,
        struct_id: StructId<'db>,
    ) -> Maybe<Arc<OrderedHashMap<StrRef<'db>, semantic::Member<'db>>>>;
    /// Returns the resolution resolved_items of a struct definition.
    #[salsa::invoke(items::structure::struct_definition_resolver_data)]
    fn struct_definition_resolver_data<'db>(
        &'db self,
        structure_id: StructId<'db>,
    ) -> Maybe<Arc<ResolverData<'db>>>;
    /// Returns the concrete members of a struct.
    #[salsa::invoke(items::structure::concrete_struct_members)]
    fn concrete_struct_members<'db>(
        &'db self,
        concrete_struct_id: types::ConcreteStructId<'db>,
    ) -> Maybe<Arc<OrderedHashMap<StrRef<'db>, semantic::Member<'db>>>>;

    // Enum.
    // =======
    /// Private query to compute data about an enum declaration.
    #[salsa::invoke(items::enm::priv_enum_declaration_data)]
    fn priv_enum_declaration_data<'db>(
        &'db self,
        enum_id: EnumId<'db>,
    ) -> Maybe<items::enm::EnumDeclarationData<'db>>;
    /// Returns the diagnostics of an enum declaration.
    #[salsa::invoke(items::enm::enum_declaration_diagnostics)]
    fn enum_declaration_diagnostics<'db>(
        &'db self,
        enum_id: EnumId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;
    /// Returns the generic parameters of an enum.
    #[salsa::invoke(items::enm::enum_generic_params)]
    fn enum_generic_params<'db>(&'db self, enum_id: EnumId<'db>) -> Maybe<Vec<GenericParam<'db>>>;
    /// Returns the generic parameters data of an enum.
    #[salsa::invoke(items::enm::enum_generic_params_data)]
    fn enum_generic_params_data<'db>(
        &'db self,
        enum_id: EnumId<'db>,
    ) -> Maybe<GenericParamsData<'db>>;
    /// Returns the attributes attached to an enum.
    #[salsa::invoke(items::enm::enum_attributes)]
    fn enum_attributes<'db>(&'db self, enum_id: EnumId<'db>) -> Maybe<Vec<Attribute<'db>>>;
    /// Returns the resolution resolved_items of an enum declaration.
    #[salsa::invoke(items::enm::enum_declaration_resolver_data)]
    fn enum_declaration_resolver_data<'db>(
        &'db self,
        enum_id: EnumId<'db>,
    ) -> Maybe<Arc<ResolverData<'db>>>;

    /// Private query to compute data about an enum definition.
    #[salsa::invoke(items::enm::priv_enum_definition_data)]
    fn priv_enum_definition_data<'db>(
        &'db self,
        enum_id: EnumId<'db>,
    ) -> Maybe<items::enm::EnumDefinitionData<'db>>;
    /// Returns the definition diagnostics of an enum definition.
    #[salsa::invoke(items::enm::enum_definition_diagnostics)]
    fn enum_definition_diagnostics<'db>(
        &'db self,
        enum_id: EnumId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;
    /// Returns the members of an enum.
    #[salsa::invoke(items::enm::enum_variants)]
    fn enum_variants<'db>(
        &'db self,
        enum_id: EnumId<'db>,
    ) -> Maybe<OrderedHashMap<StrRef<'db>, VariantId<'db>>>;
    /// Returns the semantic model of a variant.
    #[salsa::invoke(items::enm::variant_semantic)]
    fn variant_semantic<'db>(
        &'db self,
        enum_id: EnumId<'db>,
        variant_id: VariantId<'db>,
    ) -> Maybe<semantic::Variant<'db>>;
    /// Returns the resolution resolved_items of an enum definition.
    #[salsa::invoke(items::enm::enum_definition_resolver_data)]
    fn enum_definition_resolver_data<'db>(
        &'db self,
        enum_id: EnumId<'db>,
    ) -> Maybe<Arc<ResolverData<'db>>>;

    // Type Alias.
    // ===========
    /// Returns the semantic diagnostics of a type alias.
    #[salsa::invoke(items::module_type_alias::module_type_alias_semantic_diagnostics)]
    fn module_type_alias_semantic_diagnostics<'db>(
        &'db self,
        module_type_alias_id: ModuleTypeAliasId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;
    /// Returns the resolved type of a type alias.
    #[salsa::invoke(items::module_type_alias::module_type_alias_resolved_type)]
    #[salsa::cycle(items::module_type_alias::module_type_alias_resolved_type_cycle)]
    fn module_type_alias_resolved_type<'db>(
        &'db self,
        module_type_alias_id: ModuleTypeAliasId<'db>,
    ) -> Maybe<TypeId<'db>>;
    /// Returns the generic parameters of a type alias.
    #[salsa::invoke(items::module_type_alias::module_type_alias_generic_params)]
    fn module_type_alias_generic_params<'db>(
        &'db self,
        enum_id: ModuleTypeAliasId<'db>,
    ) -> Maybe<Vec<GenericParam<'db>>>;
    /// Returns the resolution resolved_items of a type alias.
    #[salsa::invoke(items::module_type_alias::module_type_alias_resolver_data)]
    #[salsa::cycle(items::module_type_alias::module_type_alias_resolver_data_cycle)]
    fn module_type_alias_resolver_data<'db>(
        &'db self,
        module_type_alias_id: ModuleTypeAliasId<'db>,
    ) -> Maybe<Arc<ResolverData<'db>>>;
    /// Private query to compute the generic parameters data of a type alias.
    #[salsa::invoke(items::module_type_alias::priv_module_type_alias_generic_params_data)]
    fn priv_module_type_alias_generic_params_data<'db>(
        &'db self,
        enum_id: ModuleTypeAliasId<'db>,
    ) -> Maybe<GenericParamsData<'db>>;
    /// Private query to compute data about a type alias.
    #[salsa::invoke(items::module_type_alias::priv_module_type_alias_semantic_data)]
    #[salsa::cycle(items::module_type_alias::priv_module_type_alias_semantic_data_cycle)]
    fn priv_module_type_alias_semantic_data<'db>(
        &'db self,
        module_type_alias_id: ModuleTypeAliasId<'db>,
        in_cycle: bool,
    ) -> Maybe<items::module_type_alias::ModuleTypeAliasData<'db>>;

    // Impl Alias.
    // ====
    /// Returns the impl definition pointed to by the impl alias, or an error if it points to
    /// something else.
    #[salsa::invoke(items::impl_alias::impl_alias_impl_def)]
    #[salsa::cycle(items::impl_alias::impl_alias_impl_def_cycle)]
    fn impl_alias_impl_def<'db>(
        &'db self,
        impl_alias_id: ImplAliasId<'db>,
    ) -> Maybe<ImplDefId<'db>>;
    /// Private query to compute data about a type alias.
    #[salsa::invoke(items::impl_alias::priv_impl_alias_semantic_data)]
    #[salsa::cycle(items::impl_alias::priv_impl_alias_semantic_data_cycle)]
    fn priv_impl_alias_semantic_data<'db>(
        &'db self,
        impl_alias_id: ImplAliasId<'db>,
        in_cycle: bool,
    ) -> Maybe<items::impl_alias::ImplAliasData<'db>>;
    /// Returns the semantic diagnostics of a type alias.
    #[salsa::invoke(items::impl_alias::impl_alias_semantic_diagnostics)]
    fn impl_alias_semantic_diagnostics<'db>(
        &'db self,
        impl_alias_id: ImplAliasId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;
    /// Returns the resolved type of a type alias.
    #[salsa::invoke(items::impl_alias::impl_alias_resolved_impl)]
    #[salsa::cycle(items::impl_alias::impl_alias_resolved_impl_cycle)]
    fn impl_alias_resolved_impl<'db>(
        &'db self,
        impl_alias_id: ImplAliasId<'db>,
    ) -> Maybe<ImplId<'db>>;
    /// Returns the generic parameters of a type alias.
    #[salsa::invoke(items::impl_alias::impl_alias_generic_params)]
    fn impl_alias_generic_params<'db>(
        &'db self,
        impl_alias_id: ImplAliasId<'db>,
    ) -> Maybe<Vec<GenericParam<'db>>>;
    /// Returns the generic parameters data of a type alias.
    #[salsa::invoke(items::impl_alias::impl_alias_generic_params_data)]
    fn impl_alias_generic_params_data<'db>(
        &'db self,
        impl_alias_id: ImplAliasId<'db>,
    ) -> Maybe<GenericParamsData<'db>>;
    /// Returns the resolution resolved_items of a type alias.
    #[salsa::invoke(items::impl_alias::impl_alias_resolver_data)]
    #[salsa::cycle(items::impl_alias::impl_alias_resolver_data_cycle)]
    fn impl_alias_resolver_data<'db>(
        &'db self,
        impl_alias_id: ImplAliasId<'db>,
    ) -> Maybe<Arc<ResolverData<'db>>>;
    /// Returns the attributes attached to the impl alias.
    #[salsa::invoke(items::impl_alias::impl_alias_attributes)]
    fn impl_alias_attributes<'db>(
        &'db self,
        impl_def_id: ImplAliasId<'db>,
    ) -> Maybe<Vec<Attribute<'db>>>;

    // Trait.
    // =======
    /// Returns the semantic declaration diagnostics of a trait.
    #[salsa::invoke(items::trt::trait_semantic_declaration_diagnostics)]
    fn trait_semantic_declaration_diagnostics<'db>(
        &'db self,
        trait_id: TraitId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;
    /// Returns the generic parameters of a trait.
    #[salsa::invoke(items::trt::trait_generic_params)]
    #[salsa::cycle(items::trt::trait_generic_params_cycle)]
    fn trait_generic_params<'db>(
        &'db self,
        trait_id: TraitId<'db>,
    ) -> Maybe<Vec<GenericParam<'db>>>;
    /// Returns the generic parameters data of a trait.
    #[salsa::invoke(items::trt::trait_generic_params_data)]
    #[salsa::cycle(items::trt::trait_generic_params_data_cycle)]
    fn trait_generic_params_data<'db>(
        &'db self,
        trait_id: TraitId<'db>,
        in_cycle: bool,
    ) -> Maybe<GenericParamsData<'db>>;
    /// Returns the attributes of a trait.
    #[salsa::invoke(items::trt::trait_attributes)]
    fn trait_attributes<'db>(&'db self, trait_id: TraitId<'db>) -> Maybe<Vec<Attribute<'db>>>;
    /// Returns the resolution resolved_items of a trait.
    #[salsa::invoke(items::trt::trait_resolver_data)]
    fn trait_resolver_data<'db>(&'db self, trait_id: TraitId<'db>)
    -> Maybe<Arc<ResolverData<'db>>>;
    /// Private query to compute declaration data about a trait.
    #[salsa::invoke(items::trt::priv_trait_declaration_data)]
    fn priv_trait_declaration_data<'db>(
        &'db self,
        trait_id: TraitId<'db>,
    ) -> Maybe<items::trt::TraitDeclarationData<'db>>;

    /// Returns the semantic definition diagnostics of a trait.
    #[salsa::invoke(items::trt::trait_semantic_definition_diagnostics)]
    fn trait_semantic_definition_diagnostics<'db>(
        &'db self,
        trait_id: TraitId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;
    /// Returns the names of all the non default implemented items of a trait.
    #[salsa::invoke(items::trt::trait_required_item_names)]
    fn trait_required_item_names<'db>(
        &'db self,
        trait_id: TraitId<'db>,
    ) -> Maybe<OrderedHashSet<StrRef<'db>>>;
    /// Returns the item of the trait, by the given `name`, if exists.
    #[salsa::invoke(items::trt::trait_item_by_name)]
    fn trait_item_by_name<'db>(
        &'db self,
        trait_id: TraitId<'db>,
        name: StrRef<'db>,
    ) -> Maybe<Option<TraitItemId<'db>>>;
    /// Returns the metadata for a trait item, by the given `name`, if exists.
    #[salsa::invoke(items::trt::trait_item_info_by_name)]
    fn trait_item_info_by_name<'db>(
        &'db self,
        trait_id: TraitId<'db>,
        name: StrRef<'db>,
    ) -> Maybe<Option<TraitItemInfo<'db>>>;
    /// Returns all the items used within the trait.
    #[salsa::invoke(items::trt::trait_all_used_uses)]
    fn trait_all_used_uses<'db>(
        &'db self,
        trait_id: TraitId<'db>,
    ) -> Maybe<Arc<OrderedHashSet<UseId<'db>>>>;
    /// Returns the functions of a trait.
    #[salsa::invoke(items::trt::trait_functions)]
    fn trait_functions<'db>(
        &'db self,
        trait_id: TraitId<'db>,
    ) -> Maybe<OrderedHashMap<StrRef<'db>, TraitFunctionId<'db>>>;
    /// Returns the function with the given name of the given trait, if exists.
    #[salsa::invoke(items::trt::trait_function_by_name)]
    fn trait_function_by_name<'db>(
        &'db self,
        trait_id: TraitId<'db>,
        name: StrRef<'db>,
    ) -> Maybe<Option<TraitFunctionId<'db>>>;
    /// Returns the types of a trait.
    #[salsa::invoke(items::trt::trait_types)]
    fn trait_types<'db>(
        &'db self,
        trait_id: TraitId<'db>,
    ) -> Maybe<OrderedHashMap<StrRef<'db>, TraitTypeId<'db>>>;
    /// Returns the item type with the given name of the given trait, if exists.
    #[salsa::invoke(items::trt::trait_type_by_name)]
    fn trait_type_by_name<'db>(
        &'db self,
        trait_id: TraitId<'db>,
        name: StrRef<'db>,
    ) -> Maybe<Option<TraitTypeId<'db>>>;

    /// Returns the constants of a trait.
    #[salsa::invoke(items::trt::trait_constants)]
    fn trait_constants<'db>(
        &'db self,
        trait_id: TraitId<'db>,
    ) -> Maybe<OrderedHashMap<StrRef<'db>, TraitConstantId<'db>>>;
    /// Returns the item constants with the given name of the given trait, if exists.
    #[salsa::invoke(items::trt::trait_constant_by_name)]
    fn trait_constant_by_name<'db>(
        &'db self,
        trait_id: TraitId<'db>,
        name: StrRef<'db>,
    ) -> Maybe<Option<TraitConstantId<'db>>>;
    /// Returns the constants of a trait.
    #[salsa::invoke(items::trt::trait_impls)]
    fn trait_impls<'db>(
        &'db self,
        trait_id: TraitId<'db>,
    ) -> Maybe<OrderedHashMap<StrRef<'db>, TraitImplId<'db>>>;
    /// Returns the item impls with the given name of the given trait, if exists.
    #[salsa::invoke(items::trt::trait_impl_by_name)]
    fn trait_impl_by_name<'db>(
        &'db self,
        trait_id: TraitId<'db>,
        name: StrRef<'db>,
    ) -> Maybe<Option<TraitImplId<'db>>>;

    /// Private query to compute definition data about a trait.
    #[salsa::invoke(items::trt::priv_trait_definition_data)]
    fn priv_trait_definition_data<'db>(
        &'db self,
        trait_id: TraitId<'db>,
    ) -> Maybe<items::trt::TraitDefinitionData<'db>>;

    // Trait type.
    // ================
    /// Returns the semantic diagnostics of a trait type.
    #[salsa::invoke(items::trt::trait_type_diagnostics)]
    fn trait_type_diagnostics<'db>(
        &'db self,
        trait_type_id: TraitTypeId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;
    /// Returns the generic params of a trait type.
    #[salsa::invoke(items::trt::trait_type_generic_params)]
    fn trait_type_generic_params<'db>(
        &'db self,
        trait_type_id: TraitTypeId<'db>,
    ) -> Maybe<Vec<GenericParam<'db>>>;
    /// Returns the attributes of a trait type.
    #[salsa::invoke(items::trt::trait_type_attributes)]
    fn trait_type_attributes<'db>(
        &'db self,
        trait_type_id: TraitTypeId<'db>,
    ) -> Maybe<Vec<Attribute<'db>>>;
    /// Returns the resolution resolved_items of a trait type.
    #[salsa::invoke(items::trt::trait_type_resolver_data)]
    fn trait_type_resolver_data<'db>(
        &'db self,
        trait_type_id: TraitTypeId<'db>,
    ) -> Maybe<Arc<ResolverData<'db>>>;
    /// Private query to compute the generic params data of a trait type.
    #[salsa::invoke(items::trt::priv_trait_type_generic_params_data)]
    fn priv_trait_type_generic_params_data<'db>(
        &'db self,
        trait_type_id: TraitTypeId<'db>,
    ) -> Maybe<GenericParamsData<'db>>;
    /// Private query to compute data about a trait type.
    #[salsa::invoke(items::trt::priv_trait_type_data)]
    fn priv_trait_type_data<'db>(
        &'db self,
        type_id: TraitTypeId<'db>,
    ) -> Maybe<TraitItemTypeData<'db>>;

    // Trait constants.
    // ================
    /// Returns the semantic diagnostics of a trait type.
    #[salsa::invoke(items::trt::trait_constant_diagnostics)]
    fn trait_constant_diagnostics<'db>(
        &'db self,
        trait_constant: TraitConstantId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;
    /// Returns the attributes of a trait constants.
    #[salsa::invoke(items::trt::trait_constant_attributes)]
    fn trait_constant_attributes<'db>(
        &'db self,
        trait_constant: TraitConstantId<'db>,
    ) -> Maybe<Vec<Attribute<'db>>>;
    /// Returns the type of a trait constant.
    #[salsa::invoke(items::trt::trait_constant_type)]
    fn trait_constant_type<'db>(
        &'db self,
        trait_type_id: TraitConstantId<'db>,
    ) -> Maybe<TypeId<'db>>;
    /// Returns the resolution resolved_items of a trait constants.
    #[salsa::invoke(items::trt::trait_constant_resolver_data)]
    fn trait_constant_resolver_data<'db>(
        &'db self,
        trait_constant: TraitConstantId<'db>,
    ) -> Maybe<Arc<ResolverData<'db>>>;
    /// Private query to compute data about a trait constant.
    #[salsa::invoke(items::trt::priv_trait_constant_data)]
    fn priv_trait_constant_data<'db>(
        &'db self,
        trait_constant: TraitConstantId<'db>,
    ) -> Maybe<TraitItemConstantData<'db>>;
    /// Returns the type of a trait constant.
    #[salsa::invoke(items::trt::concrete_trait_constant_type)]
    fn concrete_trait_constant_type<'db>(
        &'db self,
        concrete_trait_constant_id: items::trt::ConcreteTraitConstantId<'db>,
    ) -> Maybe<TypeId<'db>>;

    // Trait impls.
    // ================
    /// Returns the semantic diagnostics of a trait impls.
    #[salsa::invoke(items::trt::trait_impl_diagnostics)]
    fn trait_impl_diagnostics<'db>(
        &'db self,
        trait_impl: TraitImplId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;
    /// Returns the attributes of a trait impls.
    #[salsa::invoke(items::trt::trait_impl_attributes)]
    fn trait_impl_attributes<'db>(
        &'db self,
        trait_impl: TraitImplId<'db>,
    ) -> Maybe<Vec<Attribute<'db>>>;
    /// Returns the concrete trait of a trait impl.
    #[salsa::invoke(items::trt::trait_impl_concrete_trait)]
    fn trait_impl_concrete_trait<'db>(
        &'db self,
        trait_impl_id: TraitImplId<'db>,
    ) -> Maybe<ConcreteTraitId<'db>>;
    /// Returns the resolution resolved_items of a trait impls.
    #[salsa::invoke(items::trt::trait_impl_resolver_data)]
    fn trait_impl_resolver_data<'db>(
        &'db self,
        trait_impl: TraitImplId<'db>,
    ) -> Maybe<Arc<ResolverData<'db>>>;
    /// Private query to compute data about a trait impl.
    #[salsa::invoke(items::trt::priv_trait_impl_data)]
    fn priv_trait_impl_data<'db>(
        &'db self,
        trait_impl: TraitImplId<'db>,
    ) -> Maybe<TraitItemImplData<'db>>;
    /// Returns the concrete trait of a concrete trait impl.
    #[salsa::invoke(items::trt::concrete_trait_impl_concrete_trait)]
    fn concrete_trait_impl_concrete_trait<'db>(
        &'db self,
        concrete_trait_impl_id: items::trt::ConcreteTraitImplId<'db>,
    ) -> Maybe<ConcreteTraitId<'db>>;

    // Trait function.
    // ================
    /// Returns the semantic diagnostics of a trait function.
    #[salsa::invoke(items::trt::trait_function_declaration_diagnostics)]
    fn trait_function_declaration_diagnostics<'db>(
        &'db self,
        trait_function_id: TraitFunctionId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;
    /// Returns the signature of a trait function.
    #[salsa::invoke(items::trt::trait_function_signature)]
    fn trait_function_signature<'db>(
        &'db self,
        trait_function_id: TraitFunctionId<'db>,
    ) -> Maybe<semantic::Signature<'db>>;
    /// Returns the generic params of a trait function.
    #[salsa::invoke(items::trt::trait_function_generic_params)]
    fn trait_function_generic_params<'db>(
        &'db self,
        trait_function_id: TraitFunctionId<'db>,
    ) -> Maybe<Vec<GenericParam<'db>>>;
    /// Returns the generic params data of a trait function.
    #[salsa::invoke(items::trt::priv_trait_function_generic_params_data)]
    fn priv_trait_function_generic_params_data<'db>(
        &'db self,
        trait_function_id: TraitFunctionId<'db>,
    ) -> Maybe<GenericParamsData<'db>>;
    /// Returns the attributes of a trait function.
    #[salsa::invoke(items::trt::trait_function_attributes)]
    fn trait_function_attributes<'db>(
        &'db self,
        trait_function_id: TraitFunctionId<'db>,
    ) -> Maybe<Vec<Attribute<'db>>>;
    /// Returns the resolution resolved_items of a trait function.
    #[salsa::invoke(items::trt::trait_function_resolver_data)]
    fn trait_function_resolver_data<'db>(
        &'db self,
        trait_function_id: TraitFunctionId<'db>,
    ) -> Maybe<Arc<ResolverData<'db>>>;
    /// Returns the inline configuration of a trait function's declaration.
    #[salsa::invoke(items::trt::trait_function_declaration_inline_config)]
    fn trait_function_declaration_inline_config<'db>(
        &'db self,
        trait_function_id: TraitFunctionId<'db>,
    ) -> Maybe<InlineConfiguration<'db>>;
    /// Returns the implicits precedence of a trait function.
    #[salsa::invoke(items::trt::trait_function_declaration_implicit_precedence)]
    fn trait_function_declaration_implicit_precedence<'db>(
        &'db self,
        trait_function_id: TraitFunctionId<'db>,
    ) -> Maybe<ImplicitPrecedence<'db>>;
    /// Returns the explicit implicits of a signature of a trait function.
    #[salsa::invoke(items::trt::trait_function_declaration_implicits)]
    fn trait_function_declaration_implicits<'db>(
        &'db self,
        trait_function_id: TraitFunctionId<'db>,
    ) -> Maybe<Vec<TypeId<'db>>>;
    /// Private query to compute data about a trait function declaration.
    #[salsa::invoke(items::trt::priv_trait_function_declaration_data)]
    fn priv_trait_function_declaration_data<'db>(
        &'db self,
        function_id: TraitFunctionId<'db>,
    ) -> Maybe<items::functions::FunctionDeclarationData<'db>>;

    /// Returns the semantic diagnostics of a trait function definition (declaration + body).
    #[salsa::invoke(items::trt::trait_function_body_diagnostics)]
    fn trait_function_body_diagnostics<'db>(
        &'db self,
        trait_function_id: TraitFunctionId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;
    /// Returns the body of a trait function, if any.
    #[salsa::invoke(items::trt::trait_function_body)]
    fn trait_function_body<'db>(
        &'db self,
        trait_function_id: TraitFunctionId<'db>,
    ) -> Maybe<Option<Arc<FunctionBody<'db>>>>;
    /// Private query to compute data about a trait function definition (declaration + body)
    #[salsa::invoke(items::trt::priv_trait_function_body_data)]
    fn priv_trait_function_body_data<'db>(
        &'db self,
        trait_function_id: TraitFunctionId<'db>,
    ) -> Maybe<Option<items::function_with_body::FunctionBodyData<'db>>>;

    // Concrete Trait function.
    // ========================
    /// Returns the generic params of a concrete trait function.
    #[salsa::invoke(items::trt::concrete_trait_function_generic_params)]
    fn concrete_trait_function_generic_params<'db>(
        &'db self,
        concrete_trait_function_id: ConcreteTraitGenericFunctionId<'db>,
    ) -> Maybe<Vec<GenericParam<'db>>>;
    /// Returns the signature of a concrete trait function.
    #[salsa::invoke(items::trt::concrete_trait_function_signature)]
    fn concrete_trait_function_signature<'db>(
        &'db self,
        concrete_trait_function_id: ConcreteTraitGenericFunctionId<'db>,
    ) -> Maybe<semantic::Signature<'db>>;

    // Trait filter.
    // ==============
    /// Returns candidate [ImplDefId]s for a specific trait lookup constraint.
    #[salsa::invoke(items::imp::module_impl_ids_for_trait_filter)]
    #[salsa::cycle(items::imp::module_impl_ids_for_trait_filter_cycle)]
    fn module_impl_ids_for_trait_filter<'db>(
        &'db self,
        module_id: ModuleId<'db>,
        trait_lookup_constraint: items::imp::TraitFilter<'db>,
    ) -> Maybe<Vec<UninferredImpl<'db>>>;
    #[salsa::invoke(items::imp::impl_impl_ids_for_trait_filter)]
    #[salsa::cycle(items::imp::impl_impl_ids_for_trait_filter_cycle)]
    fn impl_impl_ids_for_trait_filter<'db>(
        &'db self,
        impl_id: ImplId<'db>,
        trait_lookup_constraint: items::imp::TraitFilter<'db>,
    ) -> Maybe<Vec<UninferredImpl<'db>>>;
    // Returns the solution set for a canonical trait.
    #[salsa::invoke(inference::solver::canonic_trait_solutions)]
    #[salsa::cycle(inference::solver::canonic_trait_solutions_cycle)]
    fn canonic_trait_solutions<'db>(
        &'db self,
        canonical_trait: inference::canonic::CanonicalTrait<'db>,
        lookup_context: ImplLookupContext<'db>,
        impl_type_bounds: BTreeMap<ImplTypeById<'db>, TypeId<'db>>,
    ) -> Result<
        inference::solver::SolutionSet<'db, inference::canonic::CanonicalImpl<'db>>,
        inference::InferenceError<'db>,
    >;

    // Impl.
    // =======
    /// Returns the semantic declaration diagnostics of an impl.
    #[salsa::invoke(items::imp::impl_semantic_declaration_diagnostics)]
    fn impl_semantic_declaration_diagnostics<'db>(
        &'db self,
        impl_def_id: ImplDefId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;
    /// Returns the generic parameters data of an impl.
    #[salsa::invoke(items::imp::impl_def_generic_params_data)]
    fn impl_def_generic_params_data<'db>(
        &'db self,
        impl_def_id: ImplDefId<'db>,
    ) -> Maybe<GenericParamsData<'db>>;
    /// Returns the generic parameters of an impl.
    #[salsa::invoke(items::imp::impl_def_generic_params)]
    fn impl_def_generic_params<'db>(
        &'db self,
        impl_def_id: ImplDefId<'db>,
    ) -> Maybe<Vec<GenericParam<'db>>>;
    /// Returns the resolution resolved_items of an impl.
    #[salsa::invoke(items::imp::impl_def_resolver_data)]
    #[salsa::cycle(items::imp::impl_def_resolver_data_cycle)]
    fn impl_def_resolver_data<'db>(
        &'db self,
        impl_def_id: ImplDefId<'db>,
    ) -> Maybe<Arc<ResolverData<'db>>>;
    /// Returns the concrete trait that is implemented by the impl.
    #[salsa::invoke(items::imp::impl_def_concrete_trait)]
    #[salsa::cycle(items::imp::impl_def_concrete_trait_cycle)]
    fn impl_def_concrete_trait<'db>(
        &'db self,
        impl_def_id: ImplDefId<'db>,
    ) -> Maybe<ConcreteTraitId<'db>>;
    /// Returns the substitution for generics for the impl.
    #[salsa::invoke(items::imp::impl_def_substitution)]
    fn impl_def_substitution<'db>(
        &'db self,
        impl_def_id: ImplDefId<'db>,
    ) -> Maybe<Arc<GenericSubstitution<'db>>>;
    /// Returns the attributes attached to the impl.
    #[salsa::invoke(items::imp::impl_def_attributes)]
    fn impl_def_attributes<'db>(
        &'db self,
        impl_def_id: ImplDefId<'db>,
    ) -> Maybe<Vec<Attribute<'db>>>;
    /// Returns the concrete trait that is implemented by the concrete impl.
    #[salsa::invoke(items::imp::impl_concrete_trait)]
    fn impl_concrete_trait<'db>(&'db self, impl_id: ImplId<'db>) -> Maybe<ConcreteTraitId<'db>>;
    /// Returns the trait that is implemented by the impl, or an error if the RHS of the `of` is not
    /// a trait.
    #[salsa::invoke(items::imp::impl_def_trait)]
    fn impl_def_trait<'db>(&'db self, impl_def_id: ImplDefId<'db>) -> Maybe<TraitId<'db>>;
    /// Private query to compute declaration data about an impl.
    #[salsa::invoke(items::imp::priv_impl_declaration_data)]
    #[salsa::cycle(items::imp::priv_impl_declaration_data_cycle)]
    fn priv_impl_declaration_data<'db>(
        &'db self,
        impl_def_id: ImplDefId<'db>,
    ) -> Maybe<items::imp::ImplDeclarationData<'db>>;

    /// Returns the semantic definition diagnostics of an impl.
    #[salsa::invoke(items::imp::impl_semantic_definition_diagnostics)]
    fn impl_semantic_definition_diagnostics<'db>(
        &'db self,
        impl_def_id: ImplDefId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;
    /// Returns the item of the impl, by the given `name`, if exists.
    #[salsa::invoke(items::imp::impl_item_by_name)]
    fn impl_item_by_name<'db>(
        &'db self,
        impl_def_id: ImplDefId<'db>,
        name: StrRef<'db>,
    ) -> Maybe<Option<ImplItemId<'db>>>;
    /// Returns the metadata for an impl item, by the given `name`, if exists.
    #[salsa::invoke(items::imp::impl_item_info_by_name)]
    fn impl_item_info_by_name<'db>(
        &'db self,
        impl_def_id: ImplDefId<'db>,
        name: StrRef<'db>,
    ) -> Maybe<Option<ImplItemInfo<'db>>>;
    /// Returns the trait impl of an implicit impl if `name` exists in trait and not in the impl.
    #[salsa::invoke(items::imp::impl_implicit_impl_by_name)]
    fn impl_implicit_impl_by_name<'db>(
        &'db self,
        impl_def_id: ImplDefId<'db>,
        name: StrRef<'db>,
    ) -> Maybe<Option<TraitImplId<'db>>>;
    /// Returns all the items used within the impl.
    #[salsa::invoke(items::imp::impl_all_used_uses)]
    fn impl_all_used_uses<'db>(
        &'db self,
        impl_def_id: ImplDefId<'db>,
    ) -> Maybe<Arc<OrderedHashSet<UseId<'db>>>>;
    /// Returns the type items in the impl.
    #[salsa::invoke(items::imp::impl_types)]
    fn impl_types<'db>(
        &'db self,
        impl_def_id: ImplDefId<'db>,
    ) -> Maybe<Arc<OrderedHashMap<ImplTypeDefId<'db>, ast::ItemTypeAlias<'db>>>>;
    /// Returns the ids of the type items in the impl.
    #[salsa::invoke(items::imp::impl_type_ids)]
    fn impl_type_ids<'db>(
        &'db self,
        impl_def_id: ImplDefId<'db>,
    ) -> Maybe<Arc<Vec<ImplTypeDefId<'db>>>>;
    /// Returns the impl AST of the impl type that matches the given id, if exists.
    #[salsa::invoke(items::imp::impl_type_by_id)]
    fn impl_type_by_id<'db>(
        &'db self,
        impl_type_id: ImplTypeDefId<'db>,
    ) -> Maybe<Option<ast::ItemTypeAlias<'db>>>;
    /// Returns the impl type item that matches the given trait type item, if exists.
    #[salsa::invoke(items::imp::impl_type_by_trait_type)]
    fn impl_type_by_trait_type<'db>(
        &'db self,
        impl_def_id: ImplDefId<'db>,
        trait_type_id: TraitTypeId<'db>,
    ) -> Maybe<ImplTypeDefId<'db>>;

    /// Returns the constant items in the impl.
    #[salsa::invoke(items::imp::impl_constants)]
    fn impl_constants<'db>(
        &'db self,
        impl_def_id: ImplDefId<'db>,
    ) -> Maybe<Arc<OrderedHashMap<ImplConstantDefId<'db>, ast::ItemConstant<'db>>>>;

    /// Returns the impls items in the impl.
    #[salsa::invoke(items::imp::impl_impls)]
    fn impl_impls<'db>(
        &'db self,
        impl_def_id: ImplDefId<'db>,
    ) -> Maybe<Arc<OrderedHashMap<ImplImplDefId<'db>, ast::ItemImplAlias<'db>>>>;
    /// Returns the ids of the impl items in the impl.
    #[salsa::invoke(items::imp::impl_impl_ids)]
    fn impl_impl_ids<'db>(
        &'db self,
        impl_def_id: ImplDefId<'db>,
    ) -> Maybe<Arc<Vec<ImplImplDefId<'db>>>>;
    /// Returns the impl AST of the impl impl that matches the given id, if exists.
    #[salsa::invoke(items::imp::impl_impl_by_id)]
    fn impl_impl_by_id<'db>(
        &'db self,
        impl_impl_id: ImplImplDefId<'db>,
    ) -> Maybe<ast::ItemImplAlias<'db>>;
    /// Returns the impl impl item that matches the given trait impl item, if exists.
    #[salsa::invoke(items::imp::impl_impl_by_trait_impl)]
    fn impl_impl_by_trait_impl<'db>(
        &'db self,
        impl_def_id: ImplDefId<'db>,
        trait_impl_id: TraitImplId<'db>,
    ) -> Maybe<ImplImplDefId<'db>>;
    /// Returns whether `trait_impl_id` is an implicit impl in `impl_def_id`.
    #[salsa::invoke(items::imp::is_implicit_impl_impl)]
    fn is_implicit_impl_impl<'db>(
        &self,
        impl_def_id: ImplDefId<'db>,
        trait_impl_id: TraitImplId<'db>,
    ) -> Maybe<bool>;

    /// Returns the impl constant item that matches the given trait constant item, if exists.
    #[salsa::invoke(items::imp::impl_constant_by_trait_constant)]
    fn impl_constant_by_trait_constant<'db>(
        &'db self,
        impl_def_id: ImplDefId<'db>,
        trait_constant_id: TraitConstantId<'db>,
    ) -> Maybe<ImplConstantDefId<'db>>;

    /// Returns the functions in the impl.
    #[salsa::invoke(items::imp::impl_functions)]
    fn impl_functions<'db>(
        &'db self,
        impl_def_id: ImplDefId<'db>,
    ) -> Maybe<OrderedHashMap<StrRef<'db>, ImplFunctionId<'db>>>;
    /// Returns the impl function that matches the given trait function, if exists.
    /// Note that a function that doesn't exist in the impl doesn't necessarily indicate an error,
    /// as, e.g., a trait function that has a default implementation doesn't have to be
    /// implemented in the impl.
    #[salsa::invoke(items::imp::impl_function_by_trait_function)]
    fn impl_function_by_trait_function<'db>(
        &'db self,
        impl_def_id: ImplDefId<'db>,
        trait_function_id: TraitFunctionId<'db>,
    ) -> Maybe<Option<ImplFunctionId<'db>>>;
    /// Private query to compute definition data about an impl.
    #[salsa::invoke(items::imp::priv_impl_definition_data)]
    fn priv_impl_definition_data<'db>(
        &'db self,
        impl_def_id: ImplDefId<'db>,
    ) -> Maybe<items::imp::ImplDefinitionData<'db>>;

    /// Private query to check if an impl is fully concrete.
    #[salsa::invoke(items::imp::priv_impl_is_fully_concrete)]
    fn priv_impl_is_fully_concrete<'db>(&self, impl_id: ImplId<'db>) -> bool;

    /// Private query to check if an impl contains no variables.
    #[salsa::invoke(items::imp::priv_impl_is_var_free)]
    fn priv_impl_is_var_free<'db>(&self, impl_id: ImplId<'db>) -> bool;

    // Impl type def.
    // ================
    /// Returns the semantic diagnostics of an impl item type.
    #[salsa::invoke(items::imp::impl_type_def_semantic_diagnostics)]
    fn impl_type_def_semantic_diagnostics<'db>(
        &'db self,
        impl_type_def_id: ImplTypeDefId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;
    /// Returns the resolved type of an impl item type.
    #[salsa::invoke(items::imp::impl_type_def_resolved_type)]
    #[salsa::cycle(items::imp::impl_type_def_resolved_type_cycle)]
    fn impl_type_def_resolved_type<'db>(
        &'db self,
        impl_type_def_id: ImplTypeDefId<'db>,
    ) -> Maybe<TypeId<'db>>;
    /// Returns the generic parameters of an impl item type.
    #[salsa::invoke(items::imp::impl_type_def_generic_params)]
    fn impl_type_def_generic_params<'db>(
        &'db self,
        impl_type_def_id: ImplTypeDefId<'db>,
    ) -> Maybe<Vec<GenericParam<'db>>>;
    /// Returns the attributes of an impl type.
    #[salsa::invoke(items::imp::impl_type_def_attributes)]
    fn impl_type_def_attributes<'db>(
        &'db self,
        impl_type_def_id: ImplTypeDefId<'db>,
    ) -> Maybe<Vec<Attribute<'db>>>;
    /// Returns the resolution resolved_items of an impl item type.
    #[salsa::invoke(items::imp::impl_type_def_resolver_data)]
    fn impl_type_def_resolver_data<'db>(
        &'db self,
        impl_type_def_id: ImplTypeDefId<'db>,
    ) -> Maybe<Arc<ResolverData<'db>>>;
    /// Returns the trait type of an impl type.
    #[salsa::invoke(items::imp::impl_type_def_trait_type)]
    fn impl_type_def_trait_type<'db>(
        &'db self,
        impl_type_def_id: ImplTypeDefId<'db>,
    ) -> Maybe<TraitTypeId<'db>>;

    /// Private query to compute data about an impl item type.
    #[salsa::invoke(items::imp::priv_impl_type_semantic_data)]
    #[salsa::cycle(items::imp::priv_impl_type_semantic_data_cycle)]
    fn priv_impl_type_semantic_data<'db>(
        &'db self,
        impl_type_def_id: ImplTypeDefId<'db>,
        in_cycle: bool,
    ) -> Maybe<items::imp::ImplItemTypeData<'db>>;
    /// Private query to compute data about the generic parameters of an impl item type.
    #[salsa::invoke(items::imp::priv_impl_type_def_generic_params_data)]
    fn priv_impl_type_def_generic_params_data<'db>(
        &'db self,
        impl_type_def_id: ImplTypeDefId<'db>,
    ) -> Maybe<GenericParamsData<'db>>;

    /// Returns the deref chain and diagnostics for a given type.
    #[salsa::invoke(items::imp::deref_chain)]
    #[salsa::cycle(items::imp::deref_chain_cycle)]
    fn deref_chain<'db>(
        &'db self,
        ty: TypeId<'db>,
        try_deref_mut: bool,
    ) -> Maybe<items::imp::DerefChain<'db>>;

    // Impl type.
    // ================
    /// Returns the implized impl type if the impl is concrete. Returns a TypeId that's not an impl
    /// type with a concrete impl.
    // TODO(Gil): Consider removing the cycle handling here if we will upgrade the salsa version.
    #[salsa::invoke(items::imp::impl_type_concrete_implized)]
    #[salsa::cycle(items::imp::impl_type_concrete_implized_cycle)]
    fn impl_type_concrete_implized<'db>(
        &'db self,
        impl_type_def_id: ImplTypeId<'db>,
    ) -> Maybe<TypeId<'db>>;

    // Impl constant def.
    // ================
    /// Returns the semantic diagnostics of an impl item constant.
    #[salsa::invoke(items::imp::impl_constant_def_semantic_diagnostics)]
    fn impl_constant_def_semantic_diagnostics<'db>(
        &'db self,
        impl_constant_def_id: ImplConstantDefId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;
    /// Returns the resolved constant value of an impl item constant.
    #[salsa::invoke(items::imp::impl_constant_def_value)]
    #[salsa::cycle(items::imp::impl_constant_def_value_cycle)]
    fn impl_constant_def_value<'db>(
        &'db self,
        impl_constant_def_id: ImplConstantDefId<'db>,
    ) -> Maybe<ConstValueId<'db>>;
    /// Returns the resolution resolved_items of an impl item constant.
    #[salsa::invoke(items::imp::impl_constant_def_resolver_data)]
    fn impl_constant_def_resolver_data<'db>(
        &'db self,
        impl_constant_def_id: ImplConstantDefId<'db>,
    ) -> Maybe<Arc<ResolverData<'db>>>;
    /// Returns the type of an impl item constant.
    #[salsa::invoke(items::imp::impl_constant_def_trait_constant)]
    fn impl_constant_def_trait_constant<'db>(
        &'db self,
        impl_constant_def_id: ImplConstantDefId<'db>,
    ) -> Maybe<TraitConstantId<'db>>;

    /// Private query to compute data about an impl item constant.
    #[salsa::invoke(items::imp::priv_impl_constant_semantic_data)]
    #[salsa::cycle(items::imp::priv_impl_constant_semantic_data_cycle)]
    fn priv_impl_constant_semantic_data<'db>(
        &'db self,
        impl_constant_def_id: ImplConstantDefId<'db>,
        in_cycle: bool,
    ) -> Maybe<items::imp::ImplItemConstantData<'db>>;

    // Impl constant.
    // ================
    /// Returns the given impl constant, implized by the given impl context.
    #[salsa::invoke(items::imp::impl_constant_implized_by_context)]
    #[salsa::cycle(items::imp::impl_constant_implized_by_context_cycle)]
    fn impl_constant_implized_by_context<'db>(
        &'db self,
        impl_constant_id: ImplConstantId<'db>,
        impl_def_id: ImplDefId<'db>,
    ) -> Maybe<ConstValueId<'db>>;
    /// Returns the implized impl constant value if the impl is concrete.
    // TODO(Gil): Consider removing the cycle handling here if we will upgrade the salsa version.
    #[salsa::invoke(items::imp::impl_constant_concrete_implized_value)]
    #[salsa::cycle(items::imp::impl_constant_concrete_implized_value_cycle)]
    fn impl_constant_concrete_implized_value<'db>(
        &'db self,
        impl_constant_id: ImplConstantId<'db>,
    ) -> Maybe<ConstValueId<'db>>;
    /// Returns the implized impl constant type if the impl is concrete.
    // TODO(Gil): Consider removing the cycle handling here if we will upgrade the salsa version.
    #[salsa::invoke(items::imp::impl_constant_concrete_implized_type)]
    #[salsa::cycle(items::imp::impl_constant_concrete_implized_type_cycle)]
    fn impl_constant_concrete_implized_type<'db>(
        &'db self,
        impl_constant_id: ImplConstantId<'db>,
    ) -> Maybe<TypeId<'db>>;

    // Impl impl def.
    // ================
    /// Returns the semantic diagnostics of an impl item impl.
    #[salsa::invoke(items::imp::impl_impl_def_semantic_diagnostics)]
    fn impl_impl_def_semantic_diagnostics<'db>(
        &'db self,
        impl_impl_def_id: ImplImplDefId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;
    /// Returns the resolution resolved_items of an impl item impl.
    #[salsa::invoke(items::imp::impl_impl_def_resolver_data)]
    fn impl_impl_def_resolver_data<'db>(
        &'db self,
        impl_impl_def_id: ImplImplDefId<'db>,
    ) -> Maybe<Arc<ResolverData<'db>>>;
    /// Returns the type of an impl item impl.
    #[salsa::invoke(items::imp::impl_impl_def_trait_impl)]
    fn impl_impl_def_trait_impl<'db>(
        &'db self,
        impl_impl_def_id: ImplImplDefId<'db>,
    ) -> Maybe<TraitImplId<'db>>;

    /// Returns the resolved impl of an impl item impl.
    #[salsa::invoke(items::imp::impl_impl_def_impl)]
    #[salsa::cycle(items::imp::impl_impl_def_impl_cycle)]
    fn impl_impl_def_impl<'db>(
        &'db self,
        impl_impl_def_id: ImplImplDefId<'db>,
        in_cycle: bool,
    ) -> Maybe<ImplId<'db>>;

    /// Private query to compute data about an impl item impl.
    #[salsa::invoke(items::imp::priv_impl_impl_semantic_data)]
    #[salsa::cycle(items::imp::priv_impl_impl_semantic_data_cycle)]
    fn priv_impl_impl_semantic_data<'db>(
        &'db self,
        impl_impl_def_id: ImplImplDefId<'db>,
        in_cycle: bool,
    ) -> Maybe<items::imp::ImplItemImplData<'db>>;

    /// Private query to compute data about the generic parameters of an impl item impl.
    #[salsa::invoke(items::imp::priv_impl_impl_def_generic_params_data)]
    fn priv_impl_impl_def_generic_params_data<'db>(
        &'db self,
        impl_impl_def_id: ImplImplDefId<'db>,
    ) -> Maybe<GenericParamsData<'db>>;

    /// Returns the semantic diagnostics of an implicit impl.
    #[salsa::invoke(items::imp::implicit_impl_impl_semantic_diagnostics)]
    fn implicit_impl_impl_semantic_diagnostics<'db>(
        &'db self,
        impl_def_id: ImplDefId<'db>,
        trait_impl_id: TraitImplId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;

    /// Returns the resolved impl of an implicit impl.
    #[salsa::invoke(items::imp::implicit_impl_impl_impl)]
    #[salsa::cycle(items::imp::implicit_impl_impl_impl_cycle)]
    fn implicit_impl_impl_impl<'db>(
        &'db self,
        impl_def_id: ImplDefId<'db>,
        trait_impl_id: TraitImplId<'db>,
        in_cycle: bool,
    ) -> Maybe<ImplId<'db>>;
    // Private query to compute data about an implicit impl.
    #[salsa::invoke(items::imp::priv_implicit_impl_impl_semantic_data)]
    #[salsa::cycle(items::imp::priv_implicit_impl_impl_semantic_data_cycle)]
    fn priv_implicit_impl_impl_semantic_data<'db>(
        &'db self,
        impl_def_id: ImplDefId<'db>,
        trait_impl_id: TraitImplId<'db>,
        in_cycle: bool,
    ) -> Maybe<ImplicitImplImplData<'db>>;

    // Impl impl.
    // ================
    /// Returns the implized impl impl if the impl is concrete.
    #[salsa::invoke(items::imp::impl_impl_implized_by_context)]
    #[salsa::cycle(items::imp::impl_impl_implized_by_context_cycle)]
    fn impl_impl_implized_by_context<'db>(
        &'db self,
        impl_impl_id: ImplImplId<'db>,
        impl_def_id: ImplDefId<'db>,
        in_cycle: bool,
    ) -> Maybe<ImplId<'db>>;
    /// Returns the implized impl impl value if the impl is concrete.
    #[salsa::invoke(items::imp::impl_impl_concrete_implized)]
    #[salsa::cycle(items::imp::impl_impl_concrete_implized_cycle)]
    fn impl_impl_concrete_implized<'db>(
        &'db self,
        impl_impl_id: ImplImplId<'db>,
    ) -> Maybe<ImplId<'db>>;
    /// Returns the concrete trait of an impl impl.
    #[salsa::invoke(items::imp::impl_impl_concrete_trait)]
    fn impl_impl_concrete_trait<'db>(
        &'db self,
        impl_impl_id: ImplImplId<'db>,
    ) -> Maybe<ConcreteTraitId<'db>>;

    // Impl function.
    // ================
    /// Returns the semantic diagnostics of an impl function's declaration (signature).
    #[salsa::invoke(items::imp::impl_function_declaration_diagnostics)]
    fn impl_function_declaration_diagnostics<'db>(
        &'db self,
        impl_function_id: ImplFunctionId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;
    /// Returns the signature of an impl function.
    #[salsa::invoke(items::imp::impl_function_signature)]
    fn impl_function_signature<'db>(
        &'db self,
        impl_function_id: ImplFunctionId<'db>,
    ) -> Maybe<semantic::Signature<'db>>;
    /// Returns the generic params of an impl function.
    #[salsa::invoke(items::imp::impl_function_generic_params)]
    fn impl_function_generic_params<'db>(
        &'db self,
        impl_function_id: ImplFunctionId<'db>,
    ) -> Maybe<Vec<GenericParam<'db>>>;
    /// Returns the generic params data of an impl function.
    #[salsa::invoke(items::imp::priv_impl_function_generic_params_data)]
    fn priv_impl_function_generic_params_data<'db>(
        &'db self,
        impl_function_id: ImplFunctionId<'db>,
    ) -> Maybe<GenericParamsData<'db>>;
    /// Returns the attributes of an impl function.
    #[salsa::invoke(items::imp::impl_function_attributes)]
    fn impl_function_attributes<'db>(
        &'db self,
        impl_function_id: ImplFunctionId<'db>,
    ) -> Maybe<Vec<Attribute<'db>>>;
    /// Returns the resolution resolved_items of an impl function's declaration.
    #[salsa::invoke(items::imp::impl_function_resolver_data)]
    fn impl_function_resolver_data<'db>(
        &'db self,
        impl_function_id: ImplFunctionId<'db>,
    ) -> Maybe<Arc<ResolverData<'db>>>;
    /// Returns the inline configuration of an impl function's declaration.
    #[salsa::invoke(items::imp::impl_function_declaration_inline_config)]
    fn impl_function_declaration_inline_config<'db>(
        &'db self,
        impl_function_id: ImplFunctionId<'db>,
    ) -> Maybe<InlineConfiguration<'db>>;
    /// Returns the implicits precedence of an impl function.
    #[salsa::invoke(items::imp::impl_function_declaration_implicit_precedence)]
    fn impl_function_declaration_implicit_precedence<'db>(
        &'db self,
        impl_function_id: ImplFunctionId<'db>,
    ) -> Maybe<ImplicitPrecedence<'db>>;
    /// Returns the explicit implicits of a signature of an impl function.
    #[salsa::invoke(items::imp::impl_function_declaration_implicits)]
    fn impl_function_declaration_implicits<'db>(
        &'db self,
        impl_function_id: ImplFunctionId<'db>,
    ) -> Maybe<Vec<TypeId<'db>>>;
    /// Returns the trait function of an impl function.
    #[salsa::invoke(items::imp::impl_function_trait_function)]
    fn impl_function_trait_function<'db>(
        &'db self,
        impl_function_id: ImplFunctionId<'db>,
    ) -> Maybe<TraitFunctionId<'db>>;
    /// Private query to compute data about an impl function declaration.
    #[salsa::invoke(items::imp::priv_impl_function_declaration_data)]
    fn priv_impl_function_declaration_data<'db>(
        &'db self,
        impl_function_id: ImplFunctionId<'db>,
    ) -> Maybe<items::imp::ImplFunctionDeclarationData<'db>>;

    /// Returns the semantic diagnostics of an impl function definition (declaration + body).
    #[salsa::invoke(items::imp::impl_function_body_diagnostics)]
    fn impl_function_body_diagnostics<'db>(
        &'db self,
        impl_function_id: ImplFunctionId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;
    /// Returns the definition of an impl function.
    #[salsa::invoke(items::imp::impl_function_body)]
    fn impl_function_body<'db>(
        &'db self,
        impl_function_id: ImplFunctionId<'db>,
    ) -> Maybe<Arc<FunctionBody<'db>>>;
    /// Returns the resolution resolved_items of an impl function's definition.
    #[salsa::invoke(items::imp::impl_function_body_resolver_data)]
    fn impl_function_body_resolver_data<'db>(
        &'db self,
        impl_function_id: ImplFunctionId<'db>,
    ) -> Maybe<Arc<ResolverData<'db>>>;
    /// Private query to compute data about an impl function definition (declaration + body)
    #[salsa::invoke(items::imp::priv_impl_function_body_data)]
    fn priv_impl_function_body_data<'db>(
        &'db self,
        impl_function_id: ImplFunctionId<'db>,
    ) -> Maybe<items::function_with_body::FunctionBodyData<'db>>;

    // Implizations.
    // ==============
    /// Returns the impl type for the given trait type, by implization by the given impl context, if
    /// the impl matches the trait of the trait type.
    // TODO(Gil): Consider removing the cycle handling here if we will upgrade the salsa version.
    #[salsa::invoke(items::implization::trait_type_implized_by_context)]
    #[salsa::cycle(items::implization::trait_type_implized_by_context_cycle)]
    fn trait_type_implized_by_context<'db>(
        &'db self,
        trait_type_def_id: TraitTypeId<'db>,
        impl_def_id: ImplDefId<'db>,
    ) -> Maybe<TypeId<'db>>;

    // Free function.
    // ==============
    /// Returns the semantic diagnostics of a free function's declaration (signature).
    #[salsa::invoke(items::free_function::free_function_declaration_diagnostics)]
    fn free_function_declaration_diagnostics<'db>(
        &'db self,
        free_function_id: FreeFunctionId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;
    /// Returns the signature of a free function.
    #[salsa::invoke(items::free_function::free_function_signature)]
    fn free_function_signature<'db>(
        &'db self,
        free_function_id: FreeFunctionId<'db>,
    ) -> Maybe<semantic::Signature<'db>>;
    /// Returns the explicit implicits of a signature of a free function.
    #[salsa::invoke(items::free_function::free_function_declaration_implicits)]
    fn free_function_declaration_implicits<'db>(
        &'db self,
        free_function_id: FreeFunctionId<'db>,
    ) -> Maybe<Vec<TypeId<'db>>>;
    /// Returns the implicits precedence of a free function.
    #[salsa::invoke(items::free_function::free_function_declaration_implicit_precedence)]
    fn free_function_declaration_implicit_precedence<'db>(
        &'db self,
        free_function_id: FreeFunctionId<'db>,
    ) -> Maybe<ImplicitPrecedence<'db>>;
    /// Returns the generic params of a free function.
    #[salsa::invoke(items::free_function::free_function_generic_params)]
    fn free_function_generic_params<'db>(
        &'db self,
        free_function_id: FreeFunctionId<'db>,
    ) -> Maybe<Vec<GenericParam<'db>>>;
    /// Returns the generic params data of a free function.
    #[salsa::invoke(items::free_function::free_function_generic_params_data)]
    fn free_function_generic_params_data<'db>(
        &'db self,
        free_function_id: FreeFunctionId<'db>,
    ) -> Maybe<GenericParamsData<'db>>;
    /// Returns the resolution resolved_items of a free function's declaration.
    #[salsa::invoke(items::free_function::free_function_declaration_resolver_data)]
    fn free_function_declaration_resolver_data<'db>(
        &'db self,
        free_function_id: FreeFunctionId<'db>,
    ) -> Maybe<Arc<ResolverData<'db>>>;
    /// Returns the inline configuration of a free function's declaration.
    #[salsa::invoke(items::free_function::free_function_declaration_inline_config)]
    fn free_function_declaration_inline_config<'db>(
        &'db self,
        free_function_id: FreeFunctionId<'db>,
    ) -> Maybe<InlineConfiguration<'db>>;
    /// Private query to compute data about a free function declaration - its signature excluding
    /// its body.
    #[salsa::invoke(items::free_function::priv_free_function_declaration_data)]
    fn priv_free_function_declaration_data<'db>(
        &'db self,
        function_id: FreeFunctionId<'db>,
    ) -> Maybe<items::functions::FunctionDeclarationData<'db>>;

    /// Returns the semantic diagnostics of a free function's body.
    #[salsa::invoke(items::free_function::free_function_body_diagnostics)]
    fn free_function_body_diagnostics<'db>(
        &'db self,
        free_function_id: FreeFunctionId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;
    /// Returns the resolution resolved_items of a free function's body.
    #[salsa::invoke(items::free_function::free_function_body_resolver_data)]
    fn free_function_body_resolver_data<'db>(
        &'db self,
        free_function_id: FreeFunctionId<'db>,
    ) -> Maybe<Arc<ResolverData<'db>>>;
    /// Private query to compute data about a free function's body.
    #[salsa::invoke(items::free_function::priv_free_function_body_data)]
    fn priv_free_function_body_data<'db>(
        &'db self,
        free_function_id: FreeFunctionId<'db>,
    ) -> Maybe<items::function_with_body::FunctionBodyData<'db>>;

    // Function with body.
    // ===================
    /// Returns the semantic diagnostics of a declaration (signature) of a function with a body.
    #[salsa::invoke(items::function_with_body::function_declaration_diagnostics)]
    fn function_declaration_diagnostics<'db>(
        &'db self,
        function_id: FunctionWithBodyId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;
    /// Returns the inline configuration of a declaration (signature) of a function with a body.
    #[salsa::invoke(items::function_with_body::function_declaration_inline_config)]
    fn function_declaration_inline_config<'db>(
        &'db self,
        function_id: FunctionWithBodyId<'db>,
    ) -> Maybe<InlineConfiguration<'db>>;
    /// Returns the implicit order of a declaration (signature) of a function with a body.
    #[salsa::invoke(items::function_with_body::function_declaration_implicit_precedence)]
    fn function_declaration_implicit_precedence<'db>(
        &'db self,
        function_id: FunctionWithBodyId<'db>,
    ) -> Maybe<ImplicitPrecedence<'db>>;
    /// Returns the signature of a function with a body.
    #[salsa::invoke(items::function_with_body::function_with_body_signature)]
    fn function_with_body_signature<'db>(
        &'db self,
        function_id: FunctionWithBodyId<'db>,
    ) -> Maybe<semantic::Signature<'db>>;
    /// Returns all the available generic params inside a function body.
    #[salsa::invoke(items::function_with_body::function_with_body_generic_params)]
    fn function_with_body_generic_params<'db>(
        &'db self,
        function_id: FunctionWithBodyId<'db>,
    ) -> Maybe<Vec<GenericParam<'db>>>;
    /// Returns the attributes of a function with a body.
    #[salsa::invoke(items::function_with_body::function_with_body_attributes)]
    fn function_with_body_attributes<'db>(
        &'db self,
        function_id: FunctionWithBodyId<'db>,
    ) -> Maybe<Vec<Attribute<'db>>>;

    /// Returns the semantic diagnostics of a body of a function (with a body).
    #[salsa::invoke(items::function_with_body::function_body_diagnostics)]
    fn function_body_diagnostics<'db>(
        &'db self,
        function_id: FunctionWithBodyId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;
    /// Returns the body of a function (with a body).
    #[salsa::invoke(items::function_with_body::function_body)]
    fn function_body<'db>(
        &'db self,
        function_id: FunctionWithBodyId<'db>,
    ) -> Maybe<Arc<FunctionBody<'db>>>;

    // Extern function.
    // ================
    /// Private query to compute data about an extern function declaration. An extern function has
    /// no body, and thus only has a declaration.
    #[salsa::invoke(items::extern_function::priv_extern_function_declaration_data)]
    fn priv_extern_function_declaration_data<'db>(
        &'db self,
        function_id: ExternFunctionId<'db>,
    ) -> Maybe<items::functions::FunctionDeclarationData<'db>>;
    /// Returns the inline configuration of an extern function's declaration.
    #[salsa::invoke(items::extern_function::extern_function_declaration_inline_config)]
    fn extern_function_declaration_inline_config<'db>(
        &'db self,
        extern_function_id: ExternFunctionId<'db>,
    ) -> Maybe<InlineConfiguration<'db>>;
    /// Returns the semantic diagnostics of an extern function declaration. An extern function has
    /// no body, and thus only has a declaration.
    #[salsa::invoke(items::extern_function::extern_function_declaration_diagnostics)]
    fn extern_function_declaration_diagnostics<'db>(
        &'db self,
        extern_function_id: ExternFunctionId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;
    /// Returns the signature of an extern function.
    #[salsa::invoke(items::extern_function::extern_function_signature)]
    fn extern_function_signature<'db>(
        &'db self,
        extern_function_id: ExternFunctionId<'db>,
    ) -> Maybe<semantic::Signature<'db>>;
    /// Returns the generic params of an extern function.
    #[salsa::invoke(items::extern_function::extern_function_declaration_generic_params)]
    fn extern_function_declaration_generic_params<'db>(
        &'db self,
        extern_function_id: ExternFunctionId<'db>,
    ) -> Maybe<Vec<GenericParam<'db>>>;
    /// Returns the generic params data of an extern function.
    #[salsa::invoke(items::extern_function::extern_function_declaration_generic_params_data)]
    fn extern_function_declaration_generic_params_data<'db>(
        &'db self,
        extern_function_id: ExternFunctionId<'db>,
    ) -> Maybe<GenericParamsData<'db>>;
    /// Returns the explicit implicits of an extern function declaration.
    #[salsa::invoke(items::extern_function::extern_function_declaration_implicits)]
    fn extern_function_declaration_implicits<'db>(
        &'db self,
        extern_function_id: ExternFunctionId<'db>,
    ) -> Maybe<Vec<TypeId<'db>>>;
    /// Returns the ref parameters of an extern function declaration.
    #[salsa::invoke(items::extern_function::extern_function_declaration_refs)]
    fn extern_function_declaration_refs<'db>(
        &'db self,
        extern_function_id: ExternFunctionId<'db>,
    ) -> Maybe<Vec<Parameter<'db>>>;
    /// Returns the resolution resolved_items of an extern function.
    #[salsa::invoke(items::extern_function::extern_function_declaration_resolver_data)]
    fn extern_function_declaration_resolver_data<'db>(
        &'db self,
        extern_function_id: ExternFunctionId<'db>,
    ) -> Maybe<Arc<ResolverData<'db>>>;

    // Extern type.
    // ============
    /// Private query to compute data about an extern type declaration. An extern type has
    /// no body, and thus only has a declaration.
    #[salsa::invoke(items::extern_type::priv_extern_type_declaration_data)]
    fn priv_extern_type_declaration_data<'db>(
        &'db self,
        type_id: ExternTypeId<'db>,
    ) -> Maybe<items::extern_type::ExternTypeDeclarationData<'db>>;
    /// Returns the semantic diagnostics of an extern type declaration. An extern type has
    /// no body, and thus only has a declaration.
    #[salsa::invoke(items::extern_type::extern_type_declaration_diagnostics)]
    fn extern_type_declaration_diagnostics<'db>(
        &'db self,
        extern_type_id: ExternTypeId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;
    /// Returns the generic params of an extern type.
    #[salsa::invoke(items::extern_type::extern_type_declaration_generic_params)]
    fn extern_type_declaration_generic_params<'db>(
        &'db self,
        extern_type_id: ExternTypeId<'db>,
    ) -> Maybe<Vec<GenericParam<'db>>>;
    /// Returns the generic params data of an extern type.
    #[salsa::invoke(items::extern_type::extern_type_declaration_generic_params_data)]
    fn extern_type_declaration_generic_params_data<'db>(
        &'db self,
        extern_type_id: ExternTypeId<'db>,
    ) -> Maybe<GenericParamsData<'db>>;

    /// Returns the attributes of an extern type.
    #[salsa::invoke(items::extern_type::extern_type_attributes)]
    fn extern_type_attributes<'db>(
        &'db self,
        extern_type_id: ExternTypeId<'db>,
    ) -> Maybe<Vec<Attribute<'db>>>;

    // Function Signature.
    // =================
    /// Returns the signature of the given FunctionTitleId. This include free functions, extern
    /// functions, etc...
    #[salsa::invoke(items::functions::function_title_signature)]
    fn function_title_signature<'db>(
        &'db self,
        function_title_id: FunctionTitleId<'db>,
    ) -> Maybe<semantic::Signature<'db>>;

    /// Returns the generic parameters of the given FunctionTitleId. This include free
    /// functions, extern functions, etc...
    #[salsa::invoke(items::functions::function_title_generic_params)]
    fn function_title_generic_params<'db>(
        &'db self,
        function_title_id: FunctionTitleId<'db>,
    ) -> Maybe<Vec<GenericParam<'db>>>;

    // Concrete function.
    // =================
    /// Returns the signature of a concrete function. This include free functions, extern functions,
    /// etc...
    #[salsa::invoke(items::functions::concrete_function_signature)]
    fn concrete_function_signature<'db>(
        &'db self,
        function_id: FunctionId<'db>,
    ) -> Maybe<semantic::Signature<'db>>;

    /// Returns a mapping of closure types to their associated parameter types for a concrete
    /// function.
    #[salsa::invoke(items::functions::concrete_function_closure_params)]
    fn concrete_function_closure_params<'db>(
        &'db self,
        function_id: FunctionId<'db>,
    ) -> Maybe<OrderedHashMap<semantic::TypeId<'db>, semantic::TypeId<'db>>>;

    /// Returns a mapping of closure types to their associated parameter types for a generic
    /// function.
    #[salsa::invoke(items::functions::get_closure_params)]
    fn get_closure_params<'db>(
        &'db self,
        generic_function_id: GenericFunctionId<'db>,
    ) -> Maybe<OrderedHashMap<TypeId<'db>, TypeId<'db>>>;
    // Macro Declaration.
    // =================
    /// Private query to compute data about a macro declaration.
    #[salsa::invoke(items::macro_declaration::priv_macro_declaration_data)]
    fn priv_macro_declaration_data<'db>(
        &'db self,
        macro_id: MacroDeclarationId<'db>,
    ) -> Maybe<MacroDeclarationData<'db>>;
    /// Returns the semantic diagnostics of a macro declaration.
    #[salsa::invoke(items::macro_declaration::macro_declaration_diagnostics)]
    fn macro_declaration_diagnostics<'db>(
        &'db self,
        macro_id: MacroDeclarationId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;
    /// Returns the resolver data of a macro declaration.
    #[salsa::invoke(items::macro_declaration::macro_declaration_resolver_data)]
    fn macro_declaration_resolver_data<'db>(
        &'db self,
        macro_id: MacroDeclarationId<'db>,
    ) -> Maybe<Arc<ResolverData<'db>>>;
    /// Returns the attributes of a macro declaration.
    #[salsa::invoke(items::macro_declaration::macro_declaration_attributes)]
    fn macro_declaration_attributes<'db>(
        &'db self,
        macro_id: MacroDeclarationId<'db>,
    ) -> Maybe<Vec<Attribute<'db>>>;
    /// Returns the rules semantic data of a macro declaration.
    #[salsa::invoke(items::macro_declaration::macro_declaration_rules)]
    fn macro_declaration_rules<'db>(
        &'db self,
        macro_id: MacroDeclarationId<'db>,
    ) -> Maybe<Vec<MacroRuleData<'db>>>;
    // Macro call.
    // ================
    /// Returns the semantic data of a macro call.
    #[salsa::invoke(items::macro_call::priv_macro_call_data)]
    fn priv_macro_call_data<'db>(
        &'db self,
        macro_call_id: MacroCallId<'db>,
    ) -> Maybe<items::macro_call::MacroCallData<'db>>;
    /// Returns the macro declaration id of a macro call.
    #[salsa::invoke(items::macro_call::macro_call_declaration_id)]
    fn macro_call_declaration_id<'db>(
        &'db self,
        macro_call_id: MacroCallId<'db>,
    ) -> Maybe<Option<MacroDeclarationId<'db>>>;
    /// Returns the semantic diagnostics of a macro call.
    #[salsa::invoke(items::macro_call::macro_call_diagnostics)]
    fn macro_call_diagnostics<'db>(
        &'db self,
        macro_call_id: MacroCallId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;

    // Generic type.
    // =============
    /// Returns the generic params of a generic type.
    #[salsa::invoke(types::generic_type_generic_params)]
    fn generic_type_generic_params<'db>(
        &'db self,
        generic_type: GenericTypeId<'db>,
    ) -> Maybe<Vec<GenericParam<'db>>>;

    // Generic param.
    // ==============
    /// Returns the semantic data of a generic param.
    #[salsa::invoke(items::generics::generic_param_semantic)]
    fn generic_param_semantic<'db>(
        &'db self,
        generic_param: GenericParamId<'db>,
    ) -> Maybe<GenericParam<'db>>;
    /// Returns the semantic diagnostics of a generic param.
    #[salsa::invoke(items::generics::generic_param_diagnostics)]
    fn generic_param_diagnostics<'db>(
        &'db self,
        generic_param: GenericParamId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>>;
    /// Returns the resolver data of a generic param.
    #[salsa::invoke(items::generics::generic_param_resolver_data)]
    fn generic_param_resolver_data<'db>(
        &'db self,
        generic_param: GenericParamId<'db>,
    ) -> Maybe<Arc<ResolverData<'db>>>;
    /// Returns the trait a generic param impl should implement.
    /// Panics if the generic param is not an impl generic param.
    #[salsa::invoke(items::generics::generic_impl_param_trait)]
    fn generic_impl_param_trait<'db>(
        &'db self,
        generic_param_id: GenericParamId<'db>,
    ) -> Maybe<TraitId<'db>>;
    /// Private query to compute data about a generic param.
    #[salsa::invoke(items::generics::priv_generic_param_data)]
    #[salsa::cycle(items::generics::priv_generic_param_data_cycle)]
    fn priv_generic_param_data<'db>(
        &'db self,
        generic_param: GenericParamId<'db>,
        in_cycle: bool,
    ) -> Maybe<GenericParamData<'db>>;

    /// Returns the type constraints intoduced by the generic params.
    #[salsa::invoke(items::generics::generic_params_type_constraints)]
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
    #[salsa::invoke(types::single_value_type)]
    fn single_value_type<'db>(&self, ty: types::TypeId<'db>) -> Maybe<bool>;

    /// Returns the type size information for the given type.
    #[salsa::invoke(types::type_size_info)]
    #[salsa::cycle(types::type_size_info_cycle)]
    fn type_size_info<'db>(&self, ty: types::TypeId<'db>) -> Maybe<TypeSizeInformation>;

    /// Returns the type info for a type in a context.
    #[salsa::invoke(types::type_info)]
    fn type_info<'db>(
        &'db self,
        lookup_context: ImplLookupContext<'db>,
        ty: types::TypeId<'db>,
    ) -> types::TypeInfo<'db>;

    /// Returns the `Copy` impl for a type in general context.
    #[salsa::invoke(types::copyable)]
    fn copyable<'db>(&'db self, ty: types::TypeId<'db>)
    -> Result<ImplId<'db>, InferenceError<'db>>;

    /// Returns the `Drop` impl for a type in general context.
    #[salsa::invoke(types::droppable)]
    fn droppable<'db>(
        &'db self,
        ty: types::TypeId<'db>,
    ) -> Result<ImplId<'db>, InferenceError<'db>>;

    /// Private query to check if a type is fully concrete.
    #[salsa::invoke(types::priv_type_is_fully_concrete)]
    fn priv_type_is_fully_concrete<'db>(&self, ty: types::TypeId<'db>) -> bool;

    /// Private query to check if a type contains no variables.
    #[salsa::invoke(types::priv_type_is_var_free)]
    fn priv_type_is_var_free<'db>(&self, ty: types::TypeId<'db>) -> bool;

    /// Private query for a shorter unique name for types.
    #[salsa::invoke(types::priv_type_short_name)]
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
    #[salsa::invoke(corelib::core_crate)]
    fn core_crate<'db>(&'db self) -> CrateId<'db>;
    #[salsa::invoke(corelib::core_module)]
    fn core_module<'db>(&'db self) -> ModuleId<'db>;
    #[salsa::invoke(corelib::core_info)]
    fn core_info<'db>(&'db self) -> Arc<CoreInfo<'db>>;

    // Analyzer plugins.
    // ========

    #[salsa::input]
    fn default_analyzer_plugins_input(&self) -> Arc<[AnalyzerPluginLongId]>;

    /// Interned version of `default_analyzer_plugins`.
    fn default_analyzer_plugins<'db>(&'db self) -> Arc<Vec<AnalyzerPluginId<'db>>>;

    #[salsa::input]
    fn analyzer_plugin_overrides_input(
        &self,
    ) -> Arc<OrderedHashMap<CrateInput, Arc<[AnalyzerPluginLongId]>>>;

    /// Interned version of `analyzer_plugin_overrides_input`.
    fn analyzer_plugin_overrides<'db>(
        &'db self,
    ) -> Arc<OrderedHashMap<CrateId<'db>, Arc<Vec<AnalyzerPluginId<'db>>>>>;

    #[salsa::interned]
    fn intern_analyzer_plugin<'db>(
        &'db self,
        plugin: AnalyzerPluginLongId,
    ) -> AnalyzerPluginId<'db>;

    /// Returns [`AnalyzerPluginId`]s of the plugins set for the crate with [`CrateId`].
    /// Returns
    /// [`SemanticGroupEx::set_override_crate_analyzer_plugins`] if it has been set,
    /// or the ([`SemanticGroup::default_analyzer_plugins`]) otherwise.
    fn crate_analyzer_plugins<'db>(
        &'db self,
        crate_id: CrateId<'db>,
    ) -> Arc<Vec<AnalyzerPluginId<'db>>>;

    /// Returns the set of `allow` that were declared as by a plugin.
    /// An allow that is not in this set will be handled as an unknown allow.
    fn declared_allows<'db>(&self, crate_id: CrateId<'db>) -> Arc<OrderedHashSet<String>>;

    // Helpers for language server.
    // ============================
    /// Returns all methods in a module that match the given type filter.
    #[salsa::invoke(lsp_helpers::methods_in_module)]
    fn methods_in_module<'db>(
        &'db self,
        module_id: ModuleId<'db>,
        type_filter: lsp_helpers::TypeFilter<'db>,
    ) -> Arc<Vec<TraitFunctionId<'db>>>;
    /// Returns all methods in a crate that match the given type filter.
    #[salsa::invoke(lsp_helpers::methods_in_crate)]
    fn methods_in_crate<'db>(
        &'db self,
        crate_id: CrateId<'db>,
        type_filter: lsp_helpers::TypeFilter<'db>,
    ) -> Arc<Vec<TraitFunctionId<'db>>>;
    /// Returns all the importables visible from a module, alongside a visible use path to the
    /// trait.
    #[salsa::invoke(lsp_helpers::visible_importables_from_module)]
    fn visible_importables_from_module<'db>(
        &'db self,
        module_id: ModuleFileId<'db>,
    ) -> Option<Arc<OrderedHashMap<ImportableId<'db>, String>>>;
    /// Returns all visible importables in a module, alongside a visible use path to the trait.
    /// `user_module_file_id` is the module from which the importables should be visible. If
    /// `include_parent` is true, the parent module of `module_id` is also considered.
    #[salsa::invoke(lsp_helpers::visible_importables_in_module)]
    fn visible_importables_in_module<'db>(
        &'db self,
        module_id: ModuleId<'db>,
        user_module_file_id: ModuleFileId<'db>,
        include_parent: bool,
    ) -> Arc<Vec<(ImportableId<'db>, String)>>;
    /// Returns all visible importables in a crate, alongside a visible use path to the trait.
    /// `user_module_file_id` is the module from which the importables should be visible.
    #[salsa::invoke(lsp_helpers::visible_importables_in_crate)]
    fn visible_importables_in_crate<'db>(
        &'db self,
        crate_id: CrateId<'db>,
        user_module_file_id: ModuleFileId<'db>,
    ) -> Arc<Vec<(ImportableId<'db>, String)>>;
    /// Returns all the traits visible from a module, alongside a visible use path to the trait.
    #[salsa::invoke(lsp_helpers::visible_traits_from_module)]
    fn visible_traits_from_module<'db>(
        &'db self,
        module_id: ModuleFileId<'db>,
    ) -> Option<Arc<OrderedHashMap<TraitId<'db>, String>>>;
}

/// Initializes the [`SemanticGroup`] database to a proper state.
pub fn init_semantic_group(db: &mut dyn SemanticGroup) {
    db.set_analyzer_plugin_overrides_input(Arc::new(OrderedHashMap::default()));
}

fn default_analyzer_plugins(db: &dyn SemanticGroup) -> Arc<Vec<AnalyzerPluginId<'_>>> {
    let inp = db.default_analyzer_plugins_input();
    Arc::new(inp.iter().map(|plugin| plugin.clone().intern(db)).collect_vec())
}

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
    for (_module_file_id, plugin_diag) in db.module_plugin_diagnostics(module_id)?.iter().cloned() {
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
    for global_use in db.module_global_uses(module_id)?.keys() {
        diagnostics.extend(db.global_use_semantic_diagnostics(*global_use));
    }
    for macro_call in db.module_macro_calls_ids(module_id)?.iter() {
        diagnostics.extend(db.macro_call_diagnostics(*macro_call));
    }
    add_unused_item_diagnostics(db, module_id, &data, &mut diagnostics);
    for analyzer_plugin_id in db.crate_analyzer_plugins(module_id.owning_crate(db)).iter() {
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

fn crate_analyzer_plugins<'db>(
    db: &'db dyn SemanticGroup,
    crate_id: CrateId<'db>,
) -> Arc<Vec<AnalyzerPluginId<'db>>> {
    db.analyzer_plugin_overrides()
        .get(&crate_id)
        .cloned()
        .unwrap_or_else(|| db.default_analyzer_plugins())
}

fn declared_allows(db: &dyn SemanticGroup, crate_id: CrateId<'_>) -> Arc<OrderedHashSet<String>> {
    Arc::new(OrderedHashSet::from_iter(
        db.crate_analyzer_plugins(crate_id)
            .iter()
            .flat_map(|plugin| db.lookup_intern_analyzer_plugin(*plugin).declared_allows()),
    ))
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
        require(!resolver_data.feature_config.allow_unused_imports)?;
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
        let mut overrides = self.analyzer_plugin_overrides_input().as_ref().clone();
        let plugins =
            plugins.iter().map(|plugin| self.lookup_intern_analyzer_plugin(*plugin)).collect_vec();
        overrides.insert(self.crate_input(crate_id), Arc::from(plugins));
        self.set_analyzer_plugin_overrides_input(Arc::new(overrides));
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
    fn set_default_plugins_from_suite(&mut self, suite: PluginSuite) {
        let PluginSuite { plugins, inline_macro_plugins, analyzer_plugins } = suite;
        // let interned = self.intern_plugin_suite(suite);

        let macro_plugins = plugins.into_iter().map(MacroPluginLongId).collect::<Arc<[_]>>();

        let inline_macro_plugins = Arc::new(
            inline_macro_plugins
                .into_iter()
                .map(|(name, plugin)| (name, InlineMacroExprPluginLongId(plugin)))
                .collect::<OrderedHashMap<_, _>>(),
        );

        let analyzer_plugins =
            analyzer_plugins.into_iter().map(AnalyzerPluginLongId).collect::<Arc<[_]>>();

        self.set_default_macro_plugins_input(macro_plugins);
        self.set_default_inline_macro_plugins_input(inline_macro_plugins);
        self.set_default_analyzer_plugins_input(analyzer_plugins);
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
