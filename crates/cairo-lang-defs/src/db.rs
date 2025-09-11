use std::collections::VecDeque;
use std::sync::Arc;

use cairo_lang_diagnostics::{
    DiagnosticNote, Maybe, MaybeAsRef, PluginFileDiagnosticNotes, ToMaybe, skip_diagnostic,
};
use cairo_lang_filesystem::db::{ExtAsVirtual, FilesGroup, files_group_input};
use cairo_lang_filesystem::ids::{
    CrateId, CrateInput, Directory, FileId, FileKind, FileLongId, Tracked, VirtualFile,
};
use cairo_lang_parser::db::ParserGroup;
use cairo_lang_syntax::attribute::consts::{
    ALLOW_ATTR, ALLOW_ATTR_ATTR, DEPRECATED_ATTR, FEATURE_ATTR, FMT_SKIP_ATTR,
    IMPLICIT_PRECEDENCE_ATTR, INLINE_ATTR, INTERNAL_ATTR, MUST_USE_ATTR, PHANTOM_ATTR,
    STARKNET_INTERFACE_ATTR, UNSTABLE_ATTR,
};
use cairo_lang_syntax::attribute::structured::AttributeStructurize;
use cairo_lang_syntax::node::ast::MaybeModuleBody;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_syntax::node::{Terminal, TypedStablePtr, TypedSyntaxNode, ast};
use cairo_lang_utils::Intern;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use itertools::{Itertools, chain};
use salsa::{Database, Setter};

use crate::cache::{DefCacheLoadingData, load_cached_crate_modules};
use crate::ids::*;
use crate::plugin::{DynGeneratedFileAuxData, MacroPlugin, MacroPluginMetadata, PluginDiagnostic};
use crate::plugin_utils::try_extract_unnamed_arg;

#[salsa::input]
pub struct DefsGroupInput {
    #[returns(ref)]
    pub default_macro_plugins: Option<Vec<MacroPluginLongId>>,
    #[returns(ref)]
    pub macro_plugin_overrides: Option<OrderedHashMap<CrateInput, Arc<[MacroPluginLongId]>>>,
    #[returns(ref)]
    pub default_inline_macro_plugins: Option<OrderedHashMap<String, InlineMacroExprPluginLongId>>,
    #[returns(ref)]
    pub inline_macro_plugin_overrides: Option<
        OrderedHashMap<CrateInput, Arc<OrderedHashMap<String, InlineMacroExprPluginLongId>>>,
    >,
}

/// Returns a reference to the inputs of [DefsGroup].
/// The reference is also used to set the inputs to new values.
#[salsa::tracked(returns(ref))]
pub fn defs_group_input(db: &dyn Database) -> DefsGroupInput {
    DefsGroupInput::new(db, None, None, None, None)
}

/// Salsa database interface.
/// See [`super::ids`] for further details.
pub trait DefsGroup: Database {
    fn default_macro_plugins_input(&self) -> &[MacroPluginLongId] {
        defs_group_input(self.as_dyn_database()).default_macro_plugins(self).as_ref().unwrap()
    }

    fn macro_plugin_overrides_input(
        &self,
    ) -> &OrderedHashMap<CrateInput, Arc<[MacroPluginLongId]>> {
        defs_group_input(self.as_dyn_database()).macro_plugin_overrides(self).as_ref().unwrap()
    }

    fn inline_macro_plugin_overrides_input(
        &self,
    ) -> &OrderedHashMap<CrateInput, Arc<OrderedHashMap<String, InlineMacroExprPluginLongId>>> {
        defs_group_input(self.as_dyn_database())
            .inline_macro_plugin_overrides(self)
            .as_ref()
            .unwrap()
    }

    fn default_inline_macro_plugins_input(
        &self,
    ) -> &OrderedHashMap<String, InlineMacroExprPluginLongId> {
        defs_group_input(self.as_dyn_database())
            .default_inline_macro_plugins(self)
            .as_ref()
            .unwrap()
    }

    // Plugins.
    // ========

    /// Interned version of `default_macro_plugins_input`.
    fn default_macro_plugins<'db>(&'db self) -> &'db [MacroPluginId<'db>] {
        default_macro_plugins(self.as_dyn_database())
    }

    /// Interned version of `macro_plugin_overrides_input`.
    fn macro_plugin_overrides<'db>(
        &'db self,
    ) -> &'db OrderedHashMap<CrateId<'db>, Vec<MacroPluginId<'db>>> {
        macro_plugin_overrides(self.as_dyn_database())
    }

    /// Returns [`MacroPluginId`]s of the plugins set for the crate with [`CrateId`].
    /// Provides an override if it has been set with
    /// [`DefsGroupEx::set_override_crate_macro_plugins`] or the default
    /// ([`DefsGroup::default_macro_plugins`]) otherwise.
    fn crate_macro_plugins<'db>(&'db self, crate_id: CrateId<'db>) -> &'db [MacroPluginId<'db>] {
        crate_macro_plugins(self.as_dyn_database(), crate_id)
    }

    /// Interned version of `default_inline_macro_plugins_input`.
    fn default_inline_macro_plugins<'db>(
        &'db self,
    ) -> &'db OrderedHashMap<String, InlineMacroExprPluginId<'db>> {
        default_inline_macro_plugins(self.as_dyn_database())
    }

    /// Interned version of `inline_macro_plugin_overrides_input`.
    fn inline_macro_plugin_overrides<'db>(
        &'db self,
    ) -> &'db OrderedHashMap<CrateId<'db>, OrderedHashMap<String, InlineMacroExprPluginId<'db>>>
    {
        inline_macro_plugin_overrides(self.as_dyn_database())
    }

    /// Returns [`InlineMacroExprPluginId`]s of the plugins set for the crate with [`CrateId`].
    /// Provides an override if it has been set with
    /// [`DefsGroupEx::set_override_crate_inline_macro_plugins`] or the default
    /// ([`DefsGroup::default_inline_macro_plugins`]) otherwise.
    fn crate_inline_macro_plugins<'db>(
        &'db self,
        crate_id: CrateId<'db>,
    ) -> &'db OrderedHashMap<String, InlineMacroExprPluginId<'db>> {
        crate_inline_macro_plugins(self.as_dyn_database(), crate_id)
    }

    /// Returns the set of attributes allowed anywhere.
    /// An attribute on any item that is not in this set will be handled as an unknown attribute.
    fn allowed_attributes<'db>(&'db self, crate_id: CrateId<'db>) -> &'db OrderedHashSet<String> {
        allowed_attributes(self.as_dyn_database(), crate_id)
    }

    /// Returns the set of attributes allowed on statements.
    /// An attribute on a statement that is not in this set will be handled as an unknown attribute.
    fn allowed_statement_attributes(&self) -> &OrderedHashSet<String> {
        allowed_statement_attributes(self.as_dyn_database())
    }

    /// Returns the set of `derive` that were declared as by a plugin.
    /// A derive that is not in this set will be handled as an unknown derive.
    fn declared_derives<'db>(&'db self, crate_id: CrateId<'db>) -> &'db OrderedHashSet<String> {
        declared_derives(self.as_dyn_database(), crate_id)
    }

    /// Returns the set of attributes that were declared as phantom type attributes by a plugin,
    /// i.e. a type marked with this attribute is considered a phantom type.
    fn declared_phantom_type_attributes<'db>(
        &'db self,
        crate_id: CrateId<'db>,
    ) -> &'db OrderedHashSet<String> {
        declared_phantom_type_attributes(self.as_dyn_database(), crate_id)
    }

    /// Checks whether the submodule is defined as inline.
    fn is_submodule_inline<'db>(&self, submodule_id: SubmoduleId<'db>) -> bool {
        is_submodule_inline(self.as_dyn_database(), submodule_id)
    }

    // Module to syntax.
    /// Gets the main file of the module.
    /// A module might have more virtual files generated by plugins.
    fn module_main_file<'db>(&'db self, module_id: ModuleId<'db>) -> Maybe<FileId<'db>> {
        module_main_file(self.as_dyn_database(), module_id)
    }
    /// Gets all the files of a module - main files and generated virtual files.
    fn module_files<'db>(&'db self, module_id: ModuleId<'db>) -> Maybe<&'db [FileId<'db>]> {
        module_files(self.as_dyn_database(), module_id)
    }
    /// Gets a file from a module and a FileIndex (i.e. ModuleFileId).
    fn module_file<'db>(&'db self, module_id: ModuleFileId<'db>) -> Maybe<FileId<'db>> {
        module_file(self.as_dyn_database(), module_id)
    }
    /// Gets the directory of a module.
    fn module_dir<'db>(&'db self, module_id: ModuleId<'db>) -> Maybe<&'db Directory<'db>> {
        module_dir(self.as_dyn_database(), module_id)
    }

    // File to module.
    fn crate_modules<'db>(&'db self, crate_id: CrateId<'db>) -> &'db [ModuleId<'db>] {
        crate_modules(self.as_dyn_database(), crate_id)
    }
    fn file_modules<'db>(&'db self, file_id: FileId<'db>) -> Maybe<&'db [ModuleId<'db>]> {
        file_modules(self.as_dyn_database(), file_id)
    }

    /// Returns the [ModuleData] of all modules in the crate's cache, and the loading data of the
    /// [DefsGroup] in the crate.
    fn cached_crate_modules<'db>(
        &'db self,
        crate_id: CrateId<'db>,
    ) -> Option<ModuleDataCacheAndLoadingData<'db>> {
        cached_crate_modules(self.as_dyn_database(), crate_id)
    }
    fn module_submodules_ids<'db>(
        &'db self,
        module_id: ModuleId<'db>,
    ) -> Maybe<&'db [SubmoduleId<'db>]> {
        module_submodules_ids(self.as_dyn_database(), module_id)
    }
    fn module_constants_ids<'db>(
        &'db self,
        module_id: ModuleId<'db>,
    ) -> Maybe<&'db [ConstantId<'db>]> {
        module_constants_ids(self.as_dyn_database(), module_id)
    }
    fn module_constant_by_id<'db>(
        &'db self,
        constant_data: ConstantId<'db>,
    ) -> Maybe<ast::ItemConstant<'db>> {
        module_constant_by_id(self.as_dyn_database(), constant_data)
    }
    fn module_free_functions_ids<'db>(
        &'db self,
        module_id: ModuleId<'db>,
    ) -> Maybe<&'db [FreeFunctionId<'db>]> {
        module_free_functions_ids(self.as_dyn_database(), module_id)
    }
    fn module_free_function_by_id<'db>(
        &'db self,
        free_function_id: FreeFunctionId<'db>,
    ) -> Maybe<ast::FunctionWithBody<'db>> {
        module_free_function_by_id(self.as_dyn_database(), free_function_id)
    }
    /// Returns the stable ptr of the name of a module item.
    fn module_item_name_stable_ptr<'db>(
        &'db self,
        module_id: ModuleId<'db>,
        item_id: ModuleItemId<'db>,
    ) -> Maybe<SyntaxStablePtrId<'db>> {
        module_item_name_stable_ptr(self.as_dyn_database(), module_id, item_id)
    }
    fn module_uses_ids<'db>(&'db self, module_id: ModuleId<'db>) -> Maybe<&'db [UseId<'db>]> {
        module_uses_ids(self.as_dyn_database(), module_id)
    }
    fn module_use_by_id<'db>(&'db self, use_id: UseId<'db>) -> Maybe<ast::UsePathLeaf<'db>> {
        module_use_by_id(self.as_dyn_database(), use_id)
    }
    fn module_global_use_by_id<'db>(
        &'db self,
        global_use_id: GlobalUseId<'db>,
    ) -> Maybe<ast::UsePathStar<'db>> {
        module_global_use_by_id(self.as_dyn_database(), global_use_id)
    }
    fn module_structs_ids<'db>(&'db self, module_id: ModuleId<'db>) -> Maybe<&'db [StructId<'db>]> {
        module_structs_ids(self.as_dyn_database(), module_id)
    }
    fn module_struct_by_id<'db>(
        &'db self,
        struct_id: StructId<'db>,
    ) -> Maybe<ast::ItemStruct<'db>> {
        module_struct_by_id(self.as_dyn_database(), struct_id)
    }
    fn module_enums_ids<'db>(&'db self, module_id: ModuleId<'db>) -> Maybe<&'db [EnumId<'db>]> {
        module_enums_ids(self.as_dyn_database(), module_id)
    }
    fn module_enum_by_id<'db>(&'db self, enum_id: EnumId<'db>) -> Maybe<ast::ItemEnum<'db>> {
        module_enum_by_id(self.as_dyn_database(), enum_id)
    }
    fn module_type_aliases_ids<'db>(
        &'db self,
        module_id: ModuleId<'db>,
    ) -> Maybe<&'db [ModuleTypeAliasId<'db>]> {
        module_type_aliases_ids(self.as_dyn_database(), module_id)
    }
    fn module_type_alias_by_id<'db>(
        &'db self,
        module_type_alias_id: ModuleTypeAliasId<'db>,
    ) -> Maybe<ast::ItemTypeAlias<'db>> {
        module_type_alias_by_id(self.as_dyn_database(), module_type_alias_id)
    }
    fn module_impl_aliases_ids<'db>(
        &'db self,
        module_id: ModuleId<'db>,
    ) -> Maybe<&'db [ImplAliasId<'db>]> {
        module_impl_aliases_ids(self.as_dyn_database(), module_id)
    }
    fn module_impl_alias_by_id<'db>(
        &'db self,
        impl_alias_id: ImplAliasId<'db>,
    ) -> Maybe<ast::ItemImplAlias<'db>> {
        module_impl_alias_by_id(self.as_dyn_database(), impl_alias_id)
    }
    fn module_traits_ids<'db>(&'db self, module_id: ModuleId<'db>) -> Maybe<&'db [TraitId<'db>]> {
        module_traits_ids(self.as_dyn_database(), module_id)
    }
    fn module_trait_by_id<'db>(&'db self, trait_id: TraitId<'db>) -> Maybe<ast::ItemTrait<'db>> {
        module_trait_by_id(self.as_dyn_database(), trait_id)
    }
    fn module_impls_ids<'db>(&'db self, module_id: ModuleId<'db>) -> Maybe<&'db [ImplDefId<'db>]> {
        module_impls_ids(self.as_dyn_database(), module_id)
    }
    fn module_impl_by_id<'db>(&'db self, impl_id: ImplDefId<'db>) -> Maybe<ast::ItemImpl<'db>> {
        module_impl_by_id(self.as_dyn_database(), impl_id)
    }
    fn module_extern_types_ids<'db>(
        &'db self,
        module_id: ModuleId<'db>,
    ) -> Maybe<&'db [ExternTypeId<'db>]> {
        module_extern_types_ids(self.as_dyn_database(), module_id)
    }
    fn module_extern_type_by_id<'db>(
        &'db self,
        extern_type_id: ExternTypeId<'db>,
    ) -> Maybe<ast::ItemExternType<'db>> {
        module_extern_type_by_id(self.as_dyn_database(), extern_type_id)
    }
    fn module_extern_functions_ids<'db>(
        &'db self,
        module_id: ModuleId<'db>,
    ) -> Maybe<&'db [ExternFunctionId<'db>]> {
        module_extern_functions_ids(self.as_dyn_database(), module_id)
    }
    fn module_extern_function_by_id<'db>(
        &'db self,
        extern_function_id: ExternFunctionId<'db>,
    ) -> Maybe<ast::ItemExternFunction<'db>> {
        module_extern_function_by_id(self.as_dyn_database(), extern_function_id)
    }

    /// Returns the IDs of the macro declarations in the module.
    fn module_macro_declarations_ids<'db>(
        &'db self,
        module_id: ModuleId<'db>,
    ) -> Maybe<&'db [MacroDeclarationId<'db>]> {
        module_macro_declarations_ids(self.as_dyn_database(), module_id)
    }
    /// Returns the macro declaration by its ID.
    fn module_macro_declaration_by_id<'db>(
        &'db self,
        macro_declaration_id: MacroDeclarationId<'db>,
    ) -> Maybe<ast::ItemMacroDeclaration<'db>> {
        module_macro_declaration_by_id(self.as_dyn_database(), macro_declaration_id)
    }

    /// Returns the IDs of the macro calls in the module.
    fn module_macro_calls_ids<'db>(
        &'db self,
        module_id: ModuleId<'db>,
    ) -> Maybe<&'db [MacroCallId<'db>]> {
        module_macro_calls_ids(self.as_dyn_database(), module_id)
    }
    /// Returns the macro call by its ID.
    fn module_macro_call_by_id<'db>(
        &'db self,
        macro_call_id: MacroCallId<'db>,
    ) -> Maybe<ast::ItemInlineMacro<'db>> {
        module_macro_call_by_id(self.as_dyn_database(), macro_call_id)
    }
    /// Returns the ancestors of a module.
    fn module_ancestors<'db>(
        &'db self,
        module_id: ModuleId<'db>,
    ) -> &'db OrderedHashSet<ModuleId<'db>> {
        module_ancestors_helper(self.as_dyn_database(), (), module_id)
    }
    /// Returns the module that the module is perceives as.
    /// Specifically if this is a macro call, it returns the module that the macro call was called
    /// from, including recursive calls.
    fn module_perceived_module<'db>(&'db self, module_id: ModuleId<'db>) -> ModuleId<'db> {
        module_perceived_module_helper(self.as_dyn_database(), (), module_id)
    }
}

impl<T: Database + ?Sized> DefsGroup for T {}

/// Initializes the [`DefsGroup`] database to a proper state.
pub fn init_defs_group(db: &mut dyn Database) {
    defs_group_input(db).set_macro_plugin_overrides(db).to(Some(OrderedHashMap::default()));
    defs_group_input(db).set_inline_macro_plugin_overrides(db).to(Some(OrderedHashMap::default()));
}

#[salsa::tracked(returns(ref))]
fn default_macro_plugins_helper<'db>(db: &'db dyn Database) -> Vec<MacroPluginId<'db>> {
    db.default_macro_plugins_input()
        .iter()
        .map(|plugin| MacroPluginId::new(db, plugin.clone()))
        .collect()
}

pub fn default_macro_plugins<'db>(db: &'db dyn Database) -> &'db [MacroPluginId<'db>] {
    default_macro_plugins_helper(db)
}

#[salsa::tracked(returns(ref))]
pub fn macro_plugin_overrides<'db>(
    db: &'db dyn Database,
) -> OrderedHashMap<CrateId<'db>, Vec<MacroPluginId<'db>>> {
    let inp = db.macro_plugin_overrides_input();
    inp.iter()
        .map(|(crate_id, plugins)| {
            (
                crate_id.clone().into_crate_long_id(db).intern(db),
                plugins.iter().map(|plugin| plugin.clone().intern(db)).collect(),
            )
        })
        .collect()
}

#[salsa::tracked(returns(ref))]
pub fn inline_macro_plugin_overrides<'db>(
    db: &'db dyn Database,
) -> OrderedHashMap<CrateId<'db>, OrderedHashMap<String, InlineMacroExprPluginId<'db>>> {
    let inp = db.inline_macro_plugin_overrides_input();
    inp.iter()
        .map(|(crate_id, plugins)| {
            (
                crate_id.clone().into_crate_long_id(db).intern(db),
                plugins
                    .iter()
                    .map(|(name, plugin)| (name.clone(), plugin.clone().intern(db)))
                    .collect(),
            )
        })
        .collect()
}

#[salsa::tracked(returns(ref))]
pub fn default_inline_macro_plugins<'db>(
    db: &'db dyn Database,
) -> OrderedHashMap<String, InlineMacroExprPluginId<'db>> {
    let inp = db.default_inline_macro_plugins_input();
    inp.iter().map(|(name, plugin)| (name.clone(), plugin.clone().intern(db))).collect()
}

fn crate_macro_plugins<'db>(
    db: &'db dyn Database,
    crate_id: CrateId<'db>,
) -> &'db [MacroPluginId<'db>] {
    macro_plugin_overrides(db).get(&crate_id).unwrap_or_else(|| default_macro_plugins_helper(db))
}

fn crate_inline_macro_plugins<'db>(
    db: &'db dyn Database,
    crate_id: CrateId<'db>,
) -> &'db OrderedHashMap<String, InlineMacroExprPluginId<'db>> {
    db.inline_macro_plugin_overrides()
        .get(&crate_id)
        .unwrap_or_else(|| db.default_inline_macro_plugins())
}

#[salsa::tracked(returns(ref))]
fn allowed_attributes<'db>(
    db: &'db dyn Database,
    crate_id: CrateId<'db>,
) -> OrderedHashSet<String> {
    let base_attrs = [
        INLINE_ATTR,
        MUST_USE_ATTR,
        UNSTABLE_ATTR,
        DEPRECATED_ATTR,
        INTERNAL_ATTR,
        ALLOW_ATTR,
        ALLOW_ATTR_ATTR,
        FEATURE_ATTR,
        PHANTOM_ATTR,
        IMPLICIT_PRECEDENCE_ATTR,
        FMT_SKIP_ATTR,
        // TODO(orizi): Remove this once `starknet` is removed from corelib.
        STARKNET_INTERFACE_ATTR,
    ];

    let crate_plugins = db.crate_macro_plugins(crate_id);

    OrderedHashSet::from_iter(chain!(
        base_attrs.map(|attr| attr.into()),
        crate_plugins.iter().flat_map(|plugin| plugin.long(db).declared_attributes())
    ))
}

// TODO(eytan-starkware): Untrack this
#[salsa::tracked(returns(ref))]
fn allowed_statement_attributes<'db>(_db: &'db dyn Database) -> OrderedHashSet<String> {
    let all_attributes = [FMT_SKIP_ATTR, ALLOW_ATTR, FEATURE_ATTR];
    OrderedHashSet::from_iter(all_attributes.map(|attr| attr.into()))
}

#[salsa::tracked(returns(ref))]
fn declared_derives<'db>(db: &'db dyn Database, crate_id: CrateId<'db>) -> OrderedHashSet<String> {
    OrderedHashSet::from_iter(
        db.crate_macro_plugins(crate_id)
            .iter()
            .flat_map(|plugin| plugin.long(db).declared_derives()),
    )
}

#[salsa::tracked(returns(ref))]
fn declared_phantom_type_attributes<'db>(
    db: &'db dyn Database,
    crate_id: CrateId<'db>,
) -> OrderedHashSet<String> {
    let crate_plugins = db.crate_macro_plugins(crate_id);

    OrderedHashSet::from_iter(chain!(
        [PHANTOM_ATTR.into()],
        crate_plugins.iter().flat_map(|plugin| plugin.long(db).phantom_type_attributes())
    ))
}

#[salsa::tracked]
fn is_submodule_inline<'db>(db: &'db dyn Database, submodule_id: SubmoduleId<'db>) -> bool {
    match submodule_id.stable_ptr(db).lookup(db).body(db) {
        MaybeModuleBody::Some(_) => true,
        MaybeModuleBody::None(_) => false,
    }
}

#[salsa::tracked]
fn module_main_file_helper<'db>(
    db: &'db dyn Database,
    _tracked: Tracked,
    module_id: ModuleId<'db>,
) -> Maybe<FileId<'db>> {
    Ok(match module_id {
        ModuleId::CrateRoot(crate_id) => {
            db.crate_config(crate_id).to_maybe()?.root.file(db, "lib.cairo")
        }
        ModuleId::Submodule(submodule_id) => {
            let parent = submodule_id.parent_module(db);
            if db.is_submodule_inline(submodule_id) {
                // This is an inline module, we return the file where the inline module was
                // defined. It can be either the file of the parent module
                // or a plugin-generated virtual file.
                db.module_file(submodule_id.module_file_id(db))?
            } else {
                let name = format!("{}.cairo", submodule_id.name(db));
                db.module_dir(parent)?.file(db, &name)
            }
        }
        // This is a macro-generated module, so the main file is the generated file.
        ModuleId::MacroCall { generated_file_id, .. } => generated_file_id,
    })
}

fn module_main_file<'db>(db: &'db dyn Database, module_id: ModuleId<'db>) -> Maybe<FileId<'db>> {
    module_main_file_helper(db, (), module_id)
}

fn module_files<'db>(db: &'db dyn Database, module_id: ModuleId<'db>) -> Maybe<&'db [FileId<'db>]> {
    Ok(module_id.module_data(db)?.files(db))
}

fn module_file<'db>(
    db: &'db dyn Database,
    module_file_id: ModuleFileId<'db>,
) -> Maybe<FileId<'db>> {
    Ok(db.module_files(module_file_id.0)?[module_file_id.1.0])
}

// TODO(eytan-starkware): This doesn't really need to be tracked.
#[salsa::tracked(returns(ref))]
fn module_dir_helper<'db>(
    db: &'db dyn Database,
    _tracked: Tracked,
    module_id: ModuleId<'db>,
) -> Maybe<Directory<'db>> {
    match module_id {
        ModuleId::CrateRoot(crate_id) => {
            db.crate_config(crate_id).to_maybe().map(|config| config.root.clone())
        }
        ModuleId::Submodule(submodule_id) => {
            let parent = submodule_id.parent_module(db);
            let name = submodule_id.name(db);
            Ok(db.module_dir(parent)?.subdir(name))
        }
        // This is a macro call, we return the directory for the file that contained the macro
        // call, as it is considered the location of the macro itself.
        ModuleId::MacroCall { id, .. } => db.module_dir(id.parent_module(db)).cloned(),
    }
}

fn module_dir<'db>(db: &'db dyn Database, module_id: ModuleId<'db>) -> Maybe<&'db Directory<'db>> {
    module_dir_helper(db, (), module_id).maybe_as_ref()
}

/// Appends all the modules under the given module, including nested modules.
fn collect_modules_under<'db>(
    db: &'db dyn Database,
    modules: &mut Vec<ModuleId<'db>>,
    module_id: ModuleId<'db>,
) {
    modules.push(module_id);
    if let Ok(submodule_ids) = db.module_submodules_ids(module_id) {
        for submodule_module_id in submodule_ids.iter().copied() {
            collect_modules_under(db, modules, ModuleId::Submodule(submodule_module_id));
        }
    }
}

/// Returns all the modules in the crate, including recursively.
#[salsa::tracked(returns(ref))]
fn crate_modules<'db>(db: &'db dyn Database, crate_id: CrateId<'db>) -> Vec<ModuleId<'db>> {
    let mut modules = Vec::new();
    collect_modules_under(db, &mut modules, ModuleId::CrateRoot(crate_id));
    modules
}

/// Returns a mapping from file IDs to the modules that contain them.
#[salsa::tracked(returns(ref))]
fn file_to_module_mapping<'db>(
    db: &'db dyn Database,
) -> OrderedHashMap<FileId<'db>, Vec<ModuleId<'db>>> {
    let mut mapping = OrderedHashMap::<FileId<'db>, Vec<ModuleId<'db>>>::default();
    for crate_id in db.crates() {
        for module_id in db.crate_modules(*crate_id).iter().copied() {
            if let Ok(files) = db.module_files(module_id) {
                for file_id in files.iter().copied() {
                    match mapping.get_mut(&file_id) {
                        Some(file_modules) => {
                            file_modules.push(module_id);
                        }
                        None => {
                            mapping.insert(file_id, vec![module_id]);
                        }
                    }
                }
            }
        }
    }
    mapping
}

fn file_modules<'db>(db: &'db dyn Database, file_id: FileId<'db>) -> Maybe<&'db [ModuleId<'db>]> {
    Ok(file_to_module_mapping(db).get(&file_id).to_maybe()?)
}

#[salsa::tracked]
pub struct ModuleData<'db> {
    /// The list of IDs of all items in the module. Each ID here is guaranteed to be a key in one
    /// of the specific-item-kind maps.
    #[returns(ref)]
    pub items: Vec<ModuleItemId<'db>>,

    // Specific-item-kind maps
    #[returns(ref)]
    pub constants: OrderedHashMap<ConstantId<'db>, ast::ItemConstant<'db>>,
    /// All the *direct* submodules of the given module - including those generated by
    /// macro plugins. To get all the submodules including nested modules, use
    /// [`collect_modules_under`].
    #[returns(ref)]
    pub submodules: OrderedHashMap<SubmoduleId<'db>, ast::ItemModule<'db>>,
    /// All the uses of the given module.
    #[returns(ref)]
    pub uses: OrderedHashMap<UseId<'db>, ast::UsePathLeaf<'db>>,
    #[returns(ref)]
    pub free_functions: OrderedHashMap<FreeFunctionId<'db>, ast::FunctionWithBody<'db>>,
    /// All the structs of the given module.
    #[returns(ref)]
    pub structs: OrderedHashMap<StructId<'db>, ast::ItemStruct<'db>>,
    /// All the enums of the given module.
    #[returns(ref)]
    pub enums: OrderedHashMap<EnumId<'db>, ast::ItemEnum<'db>>,
    /// All the type aliases of the given module.
    #[returns(ref)]
    pub type_aliases: OrderedHashMap<ModuleTypeAliasId<'db>, ast::ItemTypeAlias<'db>>,
    /// All the impl aliases of the given module.
    #[returns(ref)]
    pub impl_aliases: OrderedHashMap<ImplAliasId<'db>, ast::ItemImplAlias<'db>>,
    /// All the traits of the given module.
    #[returns(ref)]
    pub traits: OrderedHashMap<TraitId<'db>, ast::ItemTrait<'db>>,
    /// All the impls of the given module.
    #[returns(ref)]
    pub impls: OrderedHashMap<ImplDefId<'db>, ast::ItemImpl<'db>>,
    // This is needed to limitation of Hash on large tuples in combination with salsa.
    /// Additional metadata for the module.
    #[returns(ref)]
    pub metadata: ModuleDataMetadata<'db>,
}

impl<'db> ModuleData<'db> {
    /// All the extern types of the given module.
    pub fn extern_types(
        &self,
        db: &'db dyn Database,
    ) -> &'db OrderedHashMap<ExternTypeId<'db>, ast::ItemExternType<'db>> {
        self.metadata(db).extern_types(db)
    }
    /// All the extern functions of the given module.
    pub fn extern_functions(
        &self,
        db: &'db dyn Database,
    ) -> &'db OrderedHashMap<ExternFunctionId<'db>, ast::ItemExternFunction<'db>> {
        self.metadata(db).extern_functions(db)
    }
    /// All the macro declarations of the given module.
    pub fn macro_declarations(
        &self,
        db: &'db dyn Database,
    ) -> &'db OrderedHashMap<MacroDeclarationId<'db>, ast::ItemMacroDeclaration<'db>> {
        self.metadata(db).macro_declarations(db)
    }
    /// All the global uses of the given module.
    pub fn global_uses(
        &self,
        db: &'db dyn Database,
    ) -> &'db OrderedHashMap<GlobalUseId<'db>, ast::UsePathStar<'db>> {
        self.metadata(db).global_uses(db)
    }
    /// Calls to inline macros in the module (only those that were not handled by plugins).
    pub fn macro_calls(
        &self,
        db: &'db dyn Database,
    ) -> &'db OrderedHashMap<MacroCallId<'db>, ast::ItemInlineMacro<'db>> {
        self.metadata(db).macro_calls(db)
    }

    /// All the files of the given module.
    pub fn files(&self, db: &'db dyn Database) -> &'db Vec<FileId<'db>> {
        self.metadata(db).files(db)
    }

    /// Generation info for each file. Virtual files have Some. Other files have None.
    pub fn generated_file_aux_data(
        &self,
        db: &'db dyn Database,
    ) -> &'db Vec<Option<DynGeneratedFileAuxData>> {
        self.metadata(db).generated_file_aux_data(db)
    }

    pub fn plugin_diagnostics(
        &self,
        db: &'db dyn Database,
    ) -> &'db Vec<(ModuleFileId<'db>, PluginDiagnostic<'db>)> {
        self.metadata(db).plugin_diagnostics(db)
    }

    /// Diagnostic notes for diagnostics originating in the plugin generated files identified by
    /// [`FileId`].
    /// Diagnostic notes are added with `note: ` prefix at the end of diagnostic display.
    pub fn diagnostics_notes(&self, db: &'db dyn Database) -> &'db PluginFileDiagnosticNotes<'db> {
        self.metadata(db).diagnostics_notes(db)
    }
}

#[salsa::tracked]
pub struct ModuleDataMetadata<'db> {
    /// All the extern types of the given module.
    #[returns(ref)]
    pub extern_types: OrderedHashMap<ExternTypeId<'db>, ast::ItemExternType<'db>>,
    /// All the extern functions of the given module.
    #[returns(ref)]
    pub extern_functions: OrderedHashMap<ExternFunctionId<'db>, ast::ItemExternFunction<'db>>,
    /// All the macro declarations of the given module.
    #[returns(ref)]
    pub macro_declarations: OrderedHashMap<MacroDeclarationId<'db>, ast::ItemMacroDeclaration<'db>>,
    /// All the global uses of the given module.
    #[returns(ref)]
    pub global_uses: OrderedHashMap<GlobalUseId<'db>, ast::UsePathStar<'db>>,
    /// Calls to inline macros in the module (only those that were not handled by plugins).
    #[returns(ref)]
    pub macro_calls: OrderedHashMap<MacroCallId<'db>, ast::ItemInlineMacro<'db>>,

    #[returns(ref)]
    pub files: Vec<FileId<'db>>,
    /// Generation info for each file. Virtual files have Some. Other files have None.
    #[returns(ref)]
    pub generated_file_aux_data: Vec<Option<DynGeneratedFileAuxData>>,
    #[returns(ref)]
    pub plugin_diagnostics: Vec<(ModuleFileId<'db>, PluginDiagnostic<'db>)>,
    /// Diagnostic notes for diagnostics originating in the plugin generated files identified by
    /// [`FileId`].
    /// Diagnostic notes are added with `note: ` prefix at the end of diagnostic display.
    #[returns(ref)]
    pub diagnostics_notes: PluginFileDiagnosticNotes<'db>,
}

/// Information about generated files from running on a module file.
#[derive(Clone, Debug, Eq, PartialEq, salsa::Update)]
pub struct PrivModuleSubFiles<'db> {
    /// The files generated by plugins running on items.
    files: OrderedHashMap<FileId<'db>, VirtualFile<'db>>,
    /// The aux data per such file.
    aux_data: Vec<Option<DynGeneratedFileAuxData>>,
    /// The items not filtered out by plugins.
    items: Vec<ast::ModuleItem<'db>>,
    /// The diagnostics generated by the plugins.
    plugin_diagnostics: Vec<PluginDiagnostic<'db>>,
    /// Diagnostic notes for diagnostics originating in the plugin generated files identified by
    /// [`FileId`].
    /// Diagnostic notes are added with `note: ` prefix at the end of diagnostic display.
    diagnostics_notes: PluginFileDiagnosticNotes<'db>,
}

#[salsa::tracked]
fn priv_module_data_helper<'db>(
    db: &'db dyn Database,
    _tracked: Tracked,
    module_id: ModuleId<'db>,
) -> Maybe<ModuleData<'db>> {
    let crate_id = module_id.owning_crate(db);

    if let Some((map, _)) = db.cached_crate_modules(crate_id) {
        if let Some(module_data) = map.get(&module_id) {
            return Ok(*module_data);
        } else {
            panic!("module not found in cached modules_data {:?}", module_id.name(db));
        }
    };

    let module_file = db.module_main_file(module_id)?;
    let main_file_aux_data = if let ModuleId::Submodule(submodule_id) = module_id {
        let parent_module_data = submodule_id.module_data(db)?;
        let item_module_ast = &parent_module_data.submodules(db)[&submodule_id];
        if matches!(item_module_ast.body(db), MaybeModuleBody::Some(_)) {
            // TODO(spapini): Diagnostics in this module that get mapped to parent module
            // should lie in that modules ModuleData, or somehow collected by its
            // diagnostics collector function.

            // If this is an inline module, copy its generation file info from the parent
            // module, from the file where this submodule was defined.
            parent_module_data.generated_file_aux_data(db)[submodule_id.file_index(db).0].clone()
        } else {
            None
        }
    } else {
        None
    };
    let mut file_queue = VecDeque::new();
    file_queue.push_back(module_file);
    let mut constants = OrderedHashMap::default();
    let mut submodules = OrderedHashMap::default();
    let mut uses = OrderedHashMap::default();
    let mut free_functions = OrderedHashMap::default();
    let mut structs = OrderedHashMap::default();
    let mut enums = OrderedHashMap::default();
    let mut type_aliases = OrderedHashMap::default();
    let mut impl_aliases = OrderedHashMap::default();
    let mut traits = OrderedHashMap::default();
    let mut impls = OrderedHashMap::default();
    let mut extern_types = OrderedHashMap::default();
    let mut extern_functions = OrderedHashMap::default();
    let mut macro_declarations = OrderedHashMap::default();
    let mut macro_calls = OrderedHashMap::default();
    let mut global_uses = OrderedHashMap::default();
    let mut aux_data = Vec::new();
    let mut files = Vec::new();
    let mut plugin_diagnostics = Vec::new();
    let mut diagnostics_notes = OrderedHashMap::default();

    let mut items = vec![];
    aux_data.push(main_file_aux_data);
    while let Some(file_id) = file_queue.pop_front() {
        let file_index = FileIndex(files.len());
        let module_file_id = ModuleFileId(module_id, file_index);
        files.push(file_id);

        let priv_module_data = module_sub_files(db, module_id, file_id).maybe_as_ref()?;
        diagnostics_notes.extend(priv_module_data.diagnostics_notes.clone().into_iter());
        file_queue.extend(priv_module_data.files.keys().copied());
        for diag in &priv_module_data.plugin_diagnostics {
            plugin_diagnostics.push((module_file_id, diag.clone()));
        }
        aux_data.extend(priv_module_data.aux_data.iter().cloned());
        for item_ast in &priv_module_data.items {
            match item_ast.clone() {
                ast::ModuleItem::Constant(constant) => {
                    let item_id =
                        ConstantLongId(module_file_id, constant.stable_ptr(db)).intern(db);
                    constants.insert(item_id, constant);
                    items.push(ModuleItemId::Constant(item_id));
                }
                ast::ModuleItem::Module(module) => {
                    let item_id = SubmoduleLongId(module_file_id, module.stable_ptr(db)).intern(db);
                    submodules.insert(item_id, module);
                    items.push(ModuleItemId::Submodule(item_id));
                }
                ast::ModuleItem::Use(us) => {
                    for leaf in get_all_path_leaves(db, &us) {
                        let id = UseLongId(module_file_id, leaf.stable_ptr(db)).intern(db);
                        uses.insert(id, leaf);
                        items.push(ModuleItemId::Use(id));
                    }
                    for star in get_all_path_stars(db, &us) {
                        let id = GlobalUseLongId(module_file_id, star.stable_ptr(db)).intern(db);
                        global_uses.insert(id, star);
                    }
                }
                ast::ModuleItem::FreeFunction(function) => {
                    let item_id =
                        FreeFunctionLongId(module_file_id, function.stable_ptr(db)).intern(db);
                    free_functions.insert(item_id, function);
                    items.push(ModuleItemId::FreeFunction(item_id));
                }
                ast::ModuleItem::ExternFunction(extern_function) => {
                    let item_id =
                        ExternFunctionLongId(module_file_id, extern_function.stable_ptr(db))
                            .intern(db);
                    extern_functions.insert(item_id, extern_function);
                    items.push(ModuleItemId::ExternFunction(item_id));
                }
                ast::ModuleItem::ExternType(extern_type) => {
                    let item_id =
                        ExternTypeLongId(module_file_id, extern_type.stable_ptr(db)).intern(db);
                    extern_types.insert(item_id, extern_type);
                    items.push(ModuleItemId::ExternType(item_id));
                }
                ast::ModuleItem::Trait(trt) => {
                    let item_id = TraitLongId(module_file_id, trt.stable_ptr(db)).intern(db);
                    traits.insert(item_id, trt);
                    items.push(ModuleItemId::Trait(item_id));
                }
                ast::ModuleItem::Impl(imp) => {
                    let item_id = ImplDefLongId(module_file_id, imp.stable_ptr(db)).intern(db);
                    impls.insert(item_id, imp);
                    items.push(ModuleItemId::Impl(item_id));
                }
                ast::ModuleItem::Struct(structure) => {
                    let item_id = StructLongId(module_file_id, structure.stable_ptr(db)).intern(db);
                    structs.insert(item_id, structure);
                    items.push(ModuleItemId::Struct(item_id));
                }
                ast::ModuleItem::Enum(enm) => {
                    let item_id = EnumLongId(module_file_id, enm.stable_ptr(db)).intern(db);
                    enums.insert(item_id, enm);
                    items.push(ModuleItemId::Enum(item_id));
                }
                ast::ModuleItem::TypeAlias(type_alias) => {
                    let item_id =
                        ModuleTypeAliasLongId(module_file_id, type_alias.stable_ptr(db)).intern(db);
                    type_aliases.insert(item_id, type_alias);
                    items.push(ModuleItemId::TypeAlias(item_id));
                }
                ast::ModuleItem::ImplAlias(impl_alias) => {
                    let item_id =
                        ImplAliasLongId(module_file_id, impl_alias.stable_ptr(db)).intern(db);
                    impl_aliases.insert(item_id, impl_alias);
                    items.push(ModuleItemId::ImplAlias(item_id));
                }
                ast::ModuleItem::MacroDeclaration(macro_declaration) => {
                    let item_id =
                        MacroDeclarationLongId(module_file_id, macro_declaration.stable_ptr(db))
                            .intern(db);
                    macro_declarations.insert(item_id, macro_declaration);
                    items.push(ModuleItemId::MacroDeclaration(item_id));
                }
                ast::ModuleItem::InlineMacro(inline_macro_ast) => {
                    let item_id =
                        MacroCallLongId(module_file_id, inline_macro_ast.stable_ptr(db)).intern(db);
                    macro_calls.insert(item_id, inline_macro_ast.clone());
                }
                ast::ModuleItem::HeaderDoc(_) => {}
                ast::ModuleItem::Missing(_) => {}
            }
        }
    }
    let metadata = ModuleDataMetadata::new(
        db,
        extern_types,
        extern_functions,
        macro_declarations,
        global_uses,
        macro_calls,
        files,
        aux_data,
        plugin_diagnostics,
        diagnostics_notes,
    );
    let res = ModuleData::new(
        db,
        items,
        constants,
        submodules,
        uses,
        free_functions,
        structs,
        enums,
        type_aliases,
        impl_aliases,
        traits,
        impls,
        metadata,
    );
    Ok(res)
}

pub(crate) fn module_data<'db>(
    db: &'db dyn Database,
    module_id: ModuleId<'db>,
) -> Maybe<ModuleData<'db>> {
    priv_module_data_helper(db, (), module_id)
}

pub type ModuleDataCacheAndLoadingData<'db> =
    (Arc<OrderedHashMap<ModuleId<'db>, ModuleData<'db>>>, Arc<DefCacheLoadingData<'db>>);

fn cached_crate_modules<'db>(
    db: &'db dyn Database,
    crate_id: CrateId<'db>,
) -> Option<ModuleDataCacheAndLoadingData<'db>> {
    load_cached_crate_modules(db, crate_id)
}

pub fn init_external_files<T: DefsGroup>(db: &mut T) {
    let ext_as_virtual_impl: ExtAsVirtual =
        Arc::new(|db: &dyn Database, external_id: salsa::Id| ext_as_virtual_impl(db, external_id));
    files_group_input(db).set_ext_as_virtual_obj(db).to(Some(ext_as_virtual_impl));
}

/// Returns the `VirtualFile` matching the given external id.
pub fn ext_as_virtual_impl<'db>(
    db: &'db dyn Database,
    external_id: salsa::Id,
) -> &'db VirtualFile<'db> {
    let long_id = PluginGeneratedFileId::from_intern_id(external_id).long(db);
    let file_id = FileLongId::External(external_id).intern(db);
    let data =
        module_sub_files(db, long_id.module_id, long_id.stable_ptr.file_id(db)).as_ref().unwrap();
    &data.files[&file_id]
}

#[salsa::tracked(returns(ref))]
/// Returns the information about sub-files generated by the file in the module.
fn module_sub_files<'db>(
    db: &'db dyn Database,
    module_id: ModuleId<'db>,
    file_id: FileId<'db>,
) -> Maybe<PrivModuleSubFiles<'db>> {
    let module_main_file = db.module_main_file(module_id)?;
    let file_syntax = db.file_module_syntax(file_id)?;
    let item_asts = if module_main_file == file_id {
        if let ModuleId::Submodule(submodule_id) = module_id {
            let data = submodule_id.module_data(db)?;
            if let MaybeModuleBody::Some(body) = data.submodules(db)[&submodule_id].body(db) {
                Some(body.items(db))
            } else {
                None
            }
        } else {
            None
        }
    } else {
        None
    }
    .unwrap_or_else(|| file_syntax.items(db));

    let crate_id = module_id.owning_crate(db);

    let allowed_attributes = db.allowed_attributes(crate_id);
    // TODO(orizi): Actually extract the allowed features per module.
    let allowed_features = Default::default();

    let cfg_set = db
        .crate_config(crate_id)
        .and_then(|cfg| cfg.settings.cfg_set.as_ref())
        .unwrap_or(db.cfg_set());
    let edition = db
        .crate_config(module_id.owning_crate(db))
        .map(|cfg| cfg.settings.edition)
        .unwrap_or_default();
    let metadata = MacroPluginMetadata {
        cfg_set,
        declared_derives: db.declared_derives(crate_id),
        allowed_features: &allowed_features,
        edition,
    };

    let mut files = OrderedHashMap::<_, _>::default();
    let mut aux_data = Vec::new();
    let mut items = Vec::new();
    let mut plugin_diagnostics = Vec::new();
    let mut diagnostics_notes = OrderedHashMap::default();
    for item_ast in item_asts.elements(db) {
        let mut remove_original_item = false;
        // Iterate the plugins by their order. The first one to change something (either
        // generate new code, remove the original code, or both), breaks the loop. If more
        // plugins might have act on the item, they can do it on the generated code.
        for plugin_id in db.crate_macro_plugins(crate_id).iter() {
            let plugin = plugin_id.long(db);

            let result = plugin.generate_code(db, item_ast.clone(), &metadata);
            plugin_diagnostics.extend(result.diagnostics);
            if result.remove_original_item {
                remove_original_item = true;
            }

            if let Some(generated) = result.code {
                let generated_file_id = FileLongId::External(
                    PluginGeneratedFileLongId {
                        module_id,
                        stable_ptr: item_ast.stable_ptr(db).untyped(),
                        name: generated.name.clone(),
                    }
                    .intern(db)
                    .as_intern_id(),
                )
                .intern(db);
                if let Some(text) = generated.diagnostics_note {
                    diagnostics_notes
                        .insert(generated_file_id, DiagnosticNote { text, location: None });
                }
                files.insert(
                    generated_file_id,
                    VirtualFile {
                        parent: Some(file_id),
                        name: generated.name,
                        content: generated.content.into(),
                        code_mappings: generated.code_mappings.into(),
                        kind: FileKind::Module,
                        original_item_removed: remove_original_item,
                    },
                );
                aux_data.push(generated.aux_data);
            }
            if remove_original_item {
                break;
            }
        }
        if remove_original_item {
            // Don't add the original item to the module data.
            continue;
        }
        validate_attributes(db, allowed_attributes, &item_ast, &mut plugin_diagnostics);
        items.push(item_ast);
    }
    Ok(PrivModuleSubFiles { files, aux_data, items, plugin_diagnostics, diagnostics_notes })
}

/// Collects attributes allowed by `allow_attr` attribute.
fn collect_extra_allowed_attributes<'db>(
    db: &'db dyn Database,
    item: &impl QueryAttrs<'db>,
    plugin_diagnostics: &mut Vec<PluginDiagnostic<'db>>,
) -> OrderedHashSet<String> {
    let mut extra_allowed_attributes = OrderedHashSet::default();
    for attr in item.query_attr(db, ALLOW_ATTR_ATTR) {
        let args = attr.clone().structurize(db).args;
        if args.is_empty() {
            plugin_diagnostics.push(PluginDiagnostic::error(
                attr.stable_ptr(db),
                "Expected arguments.".to_string(),
            ));
            continue;
        }
        for arg in args {
            if let Some(ast::Expr::Path(path)) = try_extract_unnamed_arg(db, &arg.arg)
                && let Some([ast::PathSegment::Simple(segment)]) =
                    path.segments(db).elements(db).collect_array()
            {
                extra_allowed_attributes.insert(segment.ident(db).text(db).into());
                continue;
            }
            plugin_diagnostics.push(PluginDiagnostic::error(
                arg.arg.stable_ptr(db),
                "Expected simple identifier.".to_string(),
            ));
        }
    }
    extra_allowed_attributes
}

/// Validates that all attributes on the given item are in the allowed set or adds diagnostics.
pub fn validate_attributes_flat<'db>(
    db: &'db dyn Database,
    allowed_attributes: &OrderedHashSet<String>,
    extra_allowed_attributes: &OrderedHashSet<String>,
    item: &impl QueryAttrs<'db>,
    plugin_diagnostics: &mut Vec<PluginDiagnostic<'db>>,
) {
    let local_extra_attributes = collect_extra_allowed_attributes(db, item, plugin_diagnostics);
    for attr in item.attributes_elements(db) {
        let attr_text = attr.attr(db).as_syntax_node().get_text_without_trivia(db);
        if !(allowed_attributes.contains(attr_text)
            || extra_allowed_attributes.contains(attr_text)
            || local_extra_attributes.contains(attr_text))
        {
            plugin_diagnostics.push(PluginDiagnostic::error(
                attr.stable_ptr(db),
                "Unsupported attribute.".to_string(),
            ));
        }
    }
}

/// Validates that all attributes on all items in the given element list are in the allowed set or
/// adds diagnostics.
fn validate_attributes_element_list<'db, Item: QueryAttrs<'db> + TypedSyntaxNode<'db>>(
    db: &'db dyn Database,
    allowed_attributes: &OrderedHashSet<String>,
    extra_allowed_attributes: &OrderedHashSet<String>,
    items: impl Iterator<Item = Item>,
    plugin_diagnostics: &mut Vec<PluginDiagnostic<'db>>,
) {
    for item in items {
        validate_attributes_flat(
            db,
            allowed_attributes,
            extra_allowed_attributes,
            &item,
            plugin_diagnostics,
        );
    }
}

/// Validates that all attributes on an item and on items contained within it are in the allowed set
/// or adds diagnostics.
fn validate_attributes<'db>(
    db: &'db dyn Database,
    allowed_attributes: &OrderedHashSet<String>,
    item_ast: &ast::ModuleItem<'db>,
    plugin_diagnostics: &mut Vec<PluginDiagnostic<'db>>,
) {
    let extra_allowed_attributes =
        collect_extra_allowed_attributes(db, item_ast, plugin_diagnostics);
    validate_attributes_flat(
        db,
        allowed_attributes,
        &extra_allowed_attributes,
        item_ast,
        plugin_diagnostics,
    );

    match item_ast {
        ast::ModuleItem::Trait(item) => {
            if let ast::MaybeTraitBody::Some(body) = item.body(db) {
                validate_attributes_element_list(
                    db,
                    allowed_attributes,
                    &extra_allowed_attributes,
                    body.items(db).elements(db),
                    plugin_diagnostics,
                );
            }
        }
        ast::ModuleItem::Impl(item) => {
            if let ast::MaybeImplBody::Some(body) = item.body(db) {
                validate_attributes_element_list(
                    db,
                    allowed_attributes,
                    &extra_allowed_attributes,
                    body.items(db).elements(db),
                    plugin_diagnostics,
                );
            }
        }
        ast::ModuleItem::Struct(item) => {
            validate_attributes_element_list(
                db,
                allowed_attributes,
                &extra_allowed_attributes,
                item.members(db).elements(db),
                plugin_diagnostics,
            );
        }
        ast::ModuleItem::Enum(item) => {
            validate_attributes_element_list(
                db,
                allowed_attributes,
                &extra_allowed_attributes,
                item.variants(db).elements(db),
                plugin_diagnostics,
            );
        }
        _ => {}
    }
}

/// Returns all the path leaves under a given use item.
pub fn get_all_path_leaves<'db>(
    db: &'db dyn Database,
    use_item: &ast::ItemUse<'db>,
) -> Vec<ast::UsePathLeaf<'db>> {
    let mut res = vec![];
    let mut stack = vec![use_item.use_path(db)];
    while let Some(use_path) = stack.pop() {
        match use_path {
            ast::UsePath::Leaf(use_path) => res.push(use_path),
            ast::UsePath::Single(use_path) => stack.push(use_path.use_path(db)),
            ast::UsePath::Multi(use_path) => {
                stack.extend(use_path.use_paths(db).elements(db).rev())
            }
            ast::UsePath::Star(_) => {}
        }
    }
    res
}

/// Returns all the path stars under a given use item.
pub fn get_all_path_stars<'db>(
    db: &'db dyn Database,
    use_item: &ast::ItemUse<'db>,
) -> Vec<ast::UsePathStar<'db>> {
    let mut res = vec![];
    let mut stack = vec![use_item.use_path(db)];
    while let Some(use_path) = stack.pop() {
        match use_path {
            ast::UsePath::Leaf(_) => {}
            ast::UsePath::Single(use_path) => stack.push(use_path.use_path(db)),
            ast::UsePath::Multi(use_path) => {
                stack.extend(use_path.use_paths(db).elements(db).rev())
            }
            ast::UsePath::Star(use_path) => res.push(use_path),
        }
    }
    res
}

#[salsa::tracked(returns(ref))]
fn module_constants_ids_helper<'db>(
    db: &'db dyn Database,
    module_data: ModuleData<'db>,
) -> Vec<ConstantId<'db>> {
    module_data.constants(db).keys().copied().collect_vec()
}

pub fn module_constants_ids<'db>(
    db: &'db dyn Database,
    module_id: ModuleId<'db>,
) -> Maybe<&'db [ConstantId<'db>]> {
    Ok(module_constants_ids_helper(db, module_id.module_data(db)?))
}

pub fn module_constant_by_id<'db>(
    db: &'db dyn Database,
    constant_id: ConstantId<'db>,
) -> Maybe<ast::ItemConstant<'db>> {
    let module_constants = constant_id.module_data(db)?.constants(db);
    module_constants.get(&constant_id).cloned().ok_or_else(skip_diagnostic)
}

#[salsa::tracked(returns(ref))]
fn module_submodules_ids_helper<'db>(
    db: &'db dyn Database,
    module_data: ModuleData<'db>,
) -> Vec<SubmoduleId<'db>> {
    module_data.submodules(db).keys().copied().collect_vec()
}

fn module_submodules_ids<'db>(
    db: &'db dyn Database,
    module_id: ModuleId<'db>,
) -> Maybe<&'db [SubmoduleId<'db>]> {
    Ok(module_submodules_ids_helper(db, module_id.module_data(db)?))
}

pub fn module_submodule_by_id<'db>(
    db: &'db dyn Database,
    submodule_id: SubmoduleId<'db>,
) -> Maybe<ast::ItemModule<'db>> {
    let module_submodules = submodule_id.module_data(db)?.submodules(db);
    module_submodules.get(&submodule_id).cloned().ok_or_else(skip_diagnostic)
}

#[salsa::tracked(returns(ref))]
fn module_free_functions_ids_helper<'db>(
    db: &'db dyn Database,
    module_data: ModuleData<'db>,
) -> Vec<FreeFunctionId<'db>> {
    module_data.free_functions(db).keys().copied().collect_vec()
}

pub fn module_free_functions_ids<'db>(
    db: &'db dyn Database,
    module_id: ModuleId<'db>,
) -> Maybe<&'db [FreeFunctionId<'db>]> {
    Ok(module_free_functions_ids_helper(db, module_id.module_data(db)?))
}
pub fn module_free_function_by_id<'db>(
    db: &'db dyn Database,
    free_function_id: FreeFunctionId<'db>,
) -> Maybe<ast::FunctionWithBody<'db>> {
    let module_free_functions = free_function_id.module_data(db)?.free_functions(db);
    module_free_functions.get(&free_function_id).cloned().ok_or_else(skip_diagnostic)
}

#[salsa::tracked(returns(ref))]
fn module_uses_ids_helper<'db>(
    db: &'db dyn Database,
    module_data: ModuleData<'db>,
) -> Vec<UseId<'db>> {
    module_data.uses(db).keys().copied().collect_vec()
}
pub fn module_uses_ids<'db>(
    db: &'db dyn Database,
    module_id: ModuleId<'db>,
) -> Maybe<&'db [UseId<'db>]> {
    Ok(module_uses_ids_helper(db, module_id.module_data(db)?))
}
pub fn module_use_by_id<'db>(
    db: &'db dyn Database,
    use_id: UseId<'db>,
) -> Maybe<ast::UsePathLeaf<'db>> {
    let module_uses = use_id.module_data(db)?.uses(db);
    module_uses.get(&use_id).cloned().ok_or_else(skip_diagnostic)
}

/// Returns the `use *` of the given module, by its ID.
pub fn module_global_use_by_id<'db>(
    db: &'db dyn Database,
    global_use_id: GlobalUseId<'db>,
) -> Maybe<ast::UsePathStar<'db>> {
    let module_global_uses = global_use_id.module_data(db)?.global_uses(db);
    module_global_uses.get(&global_use_id).cloned().ok_or_else(skip_diagnostic)
}

#[salsa::tracked(returns(ref))]
fn module_structs_ids_helper<'db>(
    db: &'db dyn Database,
    module_data: ModuleData<'db>,
) -> Vec<StructId<'db>> {
    module_data.structs(db).keys().copied().collect_vec()
}

pub fn module_structs_ids<'db>(
    db: &'db dyn Database,
    module_id: ModuleId<'db>,
) -> Maybe<&'db [StructId<'db>]> {
    Ok(module_structs_ids_helper(db, module_id.module_data(db)?))
}

pub fn module_struct_by_id<'db>(
    db: &'db dyn Database,
    struct_id: StructId<'db>,
) -> Maybe<ast::ItemStruct<'db>> {
    let module_structs = struct_id.module_data(db)?.structs(db);
    module_structs.get(&struct_id).cloned().ok_or_else(skip_diagnostic)
}

#[salsa::tracked(returns(ref))]
fn module_enums_ids_helper<'db>(
    db: &'db dyn Database,
    module_data: ModuleData<'db>,
) -> Vec<EnumId<'db>> {
    module_data.enums(db).keys().copied().collect_vec()
}

pub fn module_enums_ids<'db>(
    db: &'db dyn Database,
    module_id: ModuleId<'db>,
) -> Maybe<&'db [EnumId<'db>]> {
    Ok(module_enums_ids_helper(db, module_id.module_data(db)?))
}

pub fn module_enum_by_id<'db>(
    db: &'db dyn Database,
    enum_id: EnumId<'db>,
) -> Maybe<ast::ItemEnum<'db>> {
    let module_enums = enum_id.module_data(db)?.enums(db);
    module_enums.get(&enum_id).cloned().ok_or_else(skip_diagnostic)
}

#[salsa::tracked(returns(ref))]
fn module_type_aliases_ids_helper<'db>(
    db: &'db dyn Database,
    module_data: ModuleData<'db>,
) -> Vec<ModuleTypeAliasId<'db>> {
    module_data.type_aliases(db).keys().copied().collect_vec()
}

pub fn module_type_aliases_ids<'db>(
    db: &'db dyn Database,
    module_id: ModuleId<'db>,
) -> Maybe<&'db [ModuleTypeAliasId<'db>]> {
    Ok(module_type_aliases_ids_helper(db, module_id.module_data(db)?))
}

pub fn module_type_alias_by_id<'db>(
    db: &'db dyn Database,
    module_type_alias_id: ModuleTypeAliasId<'db>,
) -> Maybe<ast::ItemTypeAlias<'db>> {
    let module_type_aliases = module_type_alias_id.module_data(db)?.type_aliases(db);
    module_type_aliases.get(&module_type_alias_id).cloned().ok_or_else(skip_diagnostic)
}

#[salsa::tracked(returns(ref))]
fn module_impl_aliases_ids_helper<'db>(
    db: &'db dyn Database,
    module_data: ModuleData<'db>,
) -> Vec<ImplAliasId<'db>> {
    module_data.impl_aliases(db).keys().copied().collect_vec()
}

pub fn module_impl_aliases_ids<'db>(
    db: &'db dyn Database,
    module_id: ModuleId<'db>,
) -> Maybe<&'db [ImplAliasId<'db>]> {
    Ok(module_impl_aliases_ids_helper(db, module_id.module_data(db)?))
}

pub fn module_impl_alias_by_id<'db>(
    db: &'db dyn Database,
    impl_alias_id: ImplAliasId<'db>,
) -> Maybe<ast::ItemImplAlias<'db>> {
    let module_impl_aliases = impl_alias_id.module_data(db)?.impl_aliases(db);
    module_impl_aliases.get(&impl_alias_id).cloned().ok_or_else(skip_diagnostic)
}

#[salsa::tracked(returns(ref))]
fn module_traits_ids_helper<'db>(
    db: &'db dyn Database,
    module_data: ModuleData<'db>,
) -> Vec<TraitId<'db>> {
    module_data.traits(db).keys().copied().collect_vec()
}

pub fn module_traits_ids<'db>(
    db: &'db dyn Database,
    module_id: ModuleId<'db>,
) -> Maybe<&'db [TraitId<'db>]> {
    Ok(module_traits_ids_helper(db, module_id.module_data(db)?))
}

pub fn module_trait_by_id<'db>(
    db: &'db dyn Database,
    trait_id: TraitId<'db>,
) -> Maybe<ast::ItemTrait<'db>> {
    let module_traits = trait_id.module_data(db)?.traits(db);
    module_traits.get(&trait_id).cloned().ok_or_else(skip_diagnostic)
}

#[salsa::tracked(returns(ref))]
fn module_impls_ids_helper<'db>(
    db: &'db dyn Database,
    module_data: ModuleData<'db>,
) -> Vec<ImplDefId<'db>> {
    module_data.impls(db).keys().copied().collect_vec()
}

pub fn module_impls_ids<'db>(
    db: &'db dyn Database,
    module_id: ModuleId<'db>,
) -> Maybe<&'db [ImplDefId<'db>]> {
    Ok(module_impls_ids_helper(db, module_id.module_data(db)?))
}
pub fn module_impl_by_id<'db>(
    db: &'db dyn Database,
    impl_def_id: ImplDefId<'db>,
) -> Maybe<ast::ItemImpl<'db>> {
    let module_impls = impl_def_id.module_data(db)?.impls(db);
    module_impls.get(&impl_def_id).cloned().ok_or_else(skip_diagnostic)
}

#[salsa::tracked(returns(ref))]
fn module_extern_types_ids_helper<'db>(
    db: &'db dyn Database,
    module_data: ModuleData<'db>,
) -> Vec<ExternTypeId<'db>> {
    module_data.extern_types(db).keys().copied().collect_vec()
}
pub fn module_extern_types_ids<'db>(
    db: &'db dyn Database,
    module_id: ModuleId<'db>,
) -> Maybe<&'db [ExternTypeId<'db>]> {
    Ok(module_extern_types_ids_helper(db, module_id.module_data(db)?))
}
pub fn module_extern_type_by_id<'db>(
    db: &'db dyn Database,
    extern_type_id: ExternTypeId<'db>,
) -> Maybe<ast::ItemExternType<'db>> {
    let module_extern_types = extern_type_id.module_data(db)?.extern_types(db);
    module_extern_types.get(&extern_type_id).cloned().ok_or_else(skip_diagnostic)
}

#[salsa::tracked(returns(ref))]
fn module_macro_declarations_ids_helper<'db>(
    db: &'db dyn Database,
    module_data: ModuleData<'db>,
) -> Vec<MacroDeclarationId<'db>> {
    module_data.macro_declarations(db).keys().copied().collect_vec()
}

/// Returns all the ids of the macro declarations of the given module.
pub fn module_macro_declarations_ids<'db>(
    db: &'db dyn Database,
    module_id: ModuleId<'db>,
) -> Maybe<&'db [MacroDeclarationId<'db>]> {
    Ok(module_macro_declarations_ids_helper(db, module_id.module_data(db)?))
}
/// Returns the macro declaration of the given id.
pub fn module_macro_declaration_by_id<'db>(
    db: &'db dyn Database,
    macro_declaration_id: MacroDeclarationId<'db>,
) -> Maybe<ast::ItemMacroDeclaration<'db>> {
    let module_macro_declarations = macro_declaration_id.module_data(db)?.macro_declarations(db);
    module_macro_declarations.get(&macro_declaration_id).cloned().ok_or_else(skip_diagnostic)
}

#[salsa::tracked(returns(ref))]
fn module_macro_calls_ids_helper<'db>(
    db: &'db dyn Database,
    module_data: ModuleData<'db>,
) -> Vec<MacroCallId<'db>> {
    module_data.macro_calls(db).keys().copied().collect_vec()
}

pub fn module_macro_calls_ids<'db>(
    db: &'db dyn Database,
    module_id: ModuleId<'db>,
) -> Maybe<&'db [MacroCallId<'db>]> {
    Ok(module_macro_calls_ids_helper(db, module_id.module_data(db)?))
}
/// Query implementation of [DefsGroup::module_macro_call_by_id].
fn module_macro_call_by_id<'db>(
    db: &'db dyn Database,
    macro_call_id: MacroCallId<'db>,
) -> Maybe<ast::ItemInlineMacro<'db>> {
    let module_macro_calls = macro_call_id.module_data(db)?.macro_calls(db);
    module_macro_calls.get(&macro_call_id).cloned().ok_or_else(skip_diagnostic)
}

#[salsa::tracked(returns(ref))]
fn module_extern_functions_ids_helper<'db>(
    db: &'db dyn Database,
    module_data: ModuleData<'db>,
) -> Vec<ExternFunctionId<'db>> {
    module_data.extern_functions(db).keys().copied().collect_vec()
}

pub fn module_extern_functions_ids<'db>(
    db: &'db dyn Database,
    module_id: ModuleId<'db>,
) -> Maybe<&'db [ExternFunctionId<'db>]> {
    Ok(module_extern_functions_ids_helper(db, module_id.module_data(db)?))
}
pub fn module_extern_function_by_id<'db>(
    db: &'db dyn Database,
    extern_function_id: ExternFunctionId<'db>,
) -> Maybe<ast::ItemExternFunction<'db>> {
    let module_extern_functions = extern_function_id.module_data(db)?.extern_functions(db);
    module_extern_functions.get(&extern_function_id).cloned().ok_or_else(skip_diagnostic)
}

#[salsa::tracked(returns(ref))]
fn module_ancestors_helper<'db>(
    db: &'db dyn Database,
    _tracked: Tracked,
    module_id: ModuleId<'db>,
) -> OrderedHashSet<ModuleId<'db>> {
    let mut current = module_id;
    let mut ancestors = OrderedHashSet::default();
    loop {
        match current {
            ModuleId::CrateRoot(_) => {
                ancestors.insert(current);
                return ancestors;
            }
            ModuleId::Submodule(submodule_id) => {
                ancestors.insert(current);
                current = submodule_id.parent_module(db);
            }
            ModuleId::MacroCall { id, .. } => {
                current = id.parent_module(db);
            }
        }
    }
}

#[salsa::tracked]
fn module_perceived_module_helper<'db>(
    db: &'db dyn Database,
    _tracked: Tracked,
    mut module_id: ModuleId<'db>,
) -> ModuleId<'db> {
    while let ModuleId::MacroCall { id, .. } = module_id {
        module_id = id.parent_module(db);
    }
    module_id
}

fn module_item_name_stable_ptr<'db>(
    db: &'db dyn Database,
    module_id: ModuleId<'db>,
    item_id: ModuleItemId<'db>,
) -> Maybe<SyntaxStablePtrId<'db>> {
    let data = module_id.module_data(db)?;
    Ok(match &item_id {
        ModuleItemId::Constant(id) => data.constants(db)[id].name(db).stable_ptr(db).untyped(),
        ModuleItemId::Submodule(id) => data.submodules(db)[id].name(db).stable_ptr(db).untyped(),
        ModuleItemId::Use(id) => data.uses(db)[id].name_stable_ptr(db),
        ModuleItemId::FreeFunction(id) => {
            data.free_functions(db)[id].declaration(db).name(db).stable_ptr(db).untyped()
        }
        ModuleItemId::Struct(id) => data.structs(db)[id].name(db).stable_ptr(db).untyped(),
        ModuleItemId::Enum(id) => data.enums(db)[id].name(db).stable_ptr(db).untyped(),
        ModuleItemId::TypeAlias(id) => data.type_aliases(db)[id].name(db).stable_ptr(db).untyped(),
        ModuleItemId::ImplAlias(id) => data.impl_aliases(db)[id].name(db).stable_ptr(db).untyped(),
        ModuleItemId::Trait(id) => data.traits(db)[id].name(db).stable_ptr(db).untyped(),
        ModuleItemId::Impl(id) => data.impls(db)[id].name(db).stable_ptr(db).untyped(),
        ModuleItemId::ExternType(id) => data.extern_types(db)[id].name(db).stable_ptr(db).untyped(),
        ModuleItemId::ExternFunction(id) => {
            data.extern_functions(db)[id].declaration(db).name(db).stable_ptr(db).untyped()
        }
        ModuleItemId::MacroDeclaration(id) => {
            data.macro_declarations(db)[id].name(db).stable_ptr(db).untyped()
        }
    })
}

pub trait DefsGroupEx: DefsGroup {
    /// Overrides the default macro plugins available for [`CrateId`] with `plugins`.
    ///
    /// *Note*: Sets the following Salsa input: `DefsGroup::macro_plugin_overrides`.
    fn set_override_crate_macro_plugins<'db>(
        &mut self,
        crate_id: CrateId<'db>,
        plugins: Arc<Vec<MacroPluginId<'db>>>,
    ) {
        let crate_input = self.crate_input(crate_id);
        let mut overrides = self.macro_plugin_overrides_input().clone();
        let plugins = plugins.iter().map(|plugin| plugin.long(self).clone()).collect();
        overrides.insert(crate_input.clone(), plugins);
        defs_group_input(self.as_dyn_database_mut())
            .set_macro_plugin_overrides(self)
            .to(Some(overrides));
    }

    /// Overrides the default inline macro plugins available for [`CrateId`] with `plugins`.
    ///
    /// *Note*: Sets the following Salsa input: `DefsGroup::inline_macro_plugin_overrides`.
    fn set_override_crate_inline_macro_plugins<'db>(
        &mut self,
        crate_id: CrateId<'db>,
        plugins: Arc<OrderedHashMap<String, InlineMacroExprPluginId<'db>>>,
    ) {
        let crate_input = self.crate_input(crate_id);
        let mut overrides = self.inline_macro_plugin_overrides_input().clone();
        let plugins = Arc::new(
            plugins
                .iter()
                .map(|(name, plugin)| (name.clone(), plugin.long(self).clone()))
                .collect(),
        );
        overrides.insert(crate_input.clone(), plugins);
        defs_group_input(self.as_dyn_database_mut())
            .set_inline_macro_plugin_overrides(self)
            .to(Some(overrides));
    }
}

impl<T: DefsGroup + ?Sized> DefsGroupEx for T {}
