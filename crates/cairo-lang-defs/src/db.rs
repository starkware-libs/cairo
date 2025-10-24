use std::collections::VecDeque;
use std::sync::Arc;

use cairo_lang_diagnostics::{
    DiagnosticNote, Maybe, MaybeAsRef, PluginFileDiagnosticNotes, ToMaybe, skip_diagnostic,
};
use cairo_lang_filesystem::db::{ExtAsVirtual, FilesGroup, files_group_input};
use cairo_lang_filesystem::ids::{
    CrateId, CrateInput, Directory, FileId, FileKind, FileLongId, SmolStrId, Tracked, VirtualFile,
};
use cairo_lang_parser::db::ParserGroup;
use cairo_lang_syntax::attribute::consts::{
    ALLOW_ATTR, ALLOW_ATTR_ATTR, DEPRECATED_ATTR, FEATURE_ATTR, FMT_SKIP_ATTR,
    IMPLICIT_PRECEDENCE_ATTR, INLINE_ATTR, INTERNAL_ATTR, MUST_USE_ATTR, PATH_ATTR, PHANTOM_ATTR,
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
    fn allowed_attributes<'db>(
        &'db self,
        crate_id: CrateId<'db>,
    ) -> &'db OrderedHashSet<SmolStrId<'db>> {
        allowed_attributes(self.as_dyn_database(), crate_id)
    }

    /// Returns the set of attributes allowed on statements.
    /// An attribute on a statement that is not in this set will be handled as an unknown attribute.
    fn allowed_statement_attributes<'db>(&'db self) -> &'db OrderedHashSet<SmolStrId<'db>> {
        allowed_statement_attributes(self.as_dyn_database())
    }

    /// Returns the set of `derive` that were declared as by a plugin.
    /// A derive that is not in this set will be handled as an unknown derive.
    fn declared_derives<'db>(
        &'db self,
        crate_id: CrateId<'db>,
    ) -> &'db OrderedHashSet<SmolStrId<'db>> {
        declared_derives(self.as_dyn_database(), crate_id)
    }

    /// Returns the set of attributes that were declared as phantom type attributes by a plugin,
    /// i.e. a type marked with this attribute is considered a phantom type.
    fn declared_phantom_type_attributes<'db>(
        &'db self,
        crate_id: CrateId<'db>,
    ) -> &'db OrderedHashSet<SmolStrId<'db>> {
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
) -> OrderedHashSet<SmolStrId<'db>> {
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
        PATH_ATTR,
        // TODO(orizi): Remove this once `starknet` is removed from corelib.
        STARKNET_INTERFACE_ATTR,
    ];

    let crate_plugins = db.crate_macro_plugins(crate_id);

    OrderedHashSet::from_iter(chain!(
        base_attrs.map(|attr| SmolStrId::from(db, attr)),
        crate_plugins.iter().flat_map(|plugin| plugin.long(db).declared_attributes(db))
    ))
}

// TODO(eytan-starkware): Untrack this
#[salsa::tracked(returns(ref))]
fn allowed_statement_attributes<'db>(db: &'db dyn Database) -> OrderedHashSet<SmolStrId<'db>> {
    let all_attributes = [FMT_SKIP_ATTR, ALLOW_ATTR, FEATURE_ATTR];
    OrderedHashSet::from_iter(all_attributes.map(|attr| SmolStrId::from(db, attr)))
}

#[salsa::tracked(returns(ref))]
fn declared_derives<'db>(
    db: &'db dyn Database,
    crate_id: CrateId<'db>,
) -> OrderedHashSet<SmolStrId<'db>> {
    OrderedHashSet::from_iter(
        db.crate_macro_plugins(crate_id)
            .iter()
            .flat_map(|plugin| plugin.long(db).declared_derives(db)),
    )
}

#[salsa::tracked(returns(ref))]
fn declared_phantom_type_attributes<'db>(
    db: &'db dyn Database,
    crate_id: CrateId<'db>,
) -> OrderedHashSet<SmolStrId<'db>> {
    let crate_plugins = db.crate_macro_plugins(crate_id);

    OrderedHashSet::from_iter(chain!(
        [SmolStrId::from(db, PHANTOM_ATTR)],
        crate_plugins.iter().flat_map(|plugin| plugin.long(db).phantom_type_attributes(db))
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
                submodule_id.stable_ptr(db).untyped().file_id(db)
            } else if let Some(path_override) = submodule_path_override(db, submodule_id) {
                path_override
            } else {
                db.module_dir(parent)?
                    .file(db, &format!("{}.cairo", submodule_id.name(db).long(db)))
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
            Ok(db.module_dir(parent)?.subdir(name.long(db)))
        }
        // This is a macro call, we return the directory for the file that contained the macro
        // call, as it is considered the location of the macro itself.
        ModuleId::MacroCall { id, .. } => db.module_dir(id.parent_module(db)).cloned(),
    }
}

/// Extracts an override path from a `#[path("...")]` attribute on the submodule declaration, if
/// any. The returned string can be a relative path (resolved against the parent module directory)
/// or an absolute path.
fn submodule_path_override<'db>(
    db: &'db dyn Database,
    submodule_id: SubmoduleId<'db>,
) -> Option<FileId<'db>> {
    // Get the parent module's AST for this submodule.
    let parent = submodule_id.parent_module(db);
    let parent_data = parent.module_data(db).ok()?;
    let item_module_ast = parent_data.submodules(db).get(&submodule_id)?.clone();

    let attr_ast = item_module_ast.find_attr(db, PATH_ATTR)?;
    let terminal = extract_path_arg(db, &attr_ast.arguments(db))?;
    let path = terminal.string_value(db)?;
    /// Find the module where the parents of this module is defined.
    fn find_base<'db>(db: &'db dyn Database, module_id: ModuleId<'db>) -> ModuleId<'db> {
        match module_id {
            ModuleId::CrateRoot(crate_id) => ModuleId::CrateRoot(crate_id),
            ModuleId::Submodule(submodule_id) => submodule_id.parent_module(db),
            ModuleId::MacroCall { id, .. } => find_base(db, id.parent_module(db)),
        }
    }
    let base = find_base(db, parent);
    Some(db.module_dir(base).ok()?.file(db, &path))
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

    // Splitting into parts to avoid overly large tuples in salsa, which fails to compile.
    /// Data for type items.
    #[returns(ref)]
    types_data: ModuleTypesData<'db>,
    /// Data for non-type named items.
    #[returns(ref)]
    named_items_data: ModuleNamedItemsData<'db>,
    /// Data for unnamed items.
    #[returns(ref)]
    unnamed_items_data: ModuleUnnamedItemsData<'db>,
    /// Data for files.
    #[returns(ref)]
    files_data: ModuleFilesData<'db>,
}

#[salsa::tracked]
pub struct ModuleTypesData<'db> {
    /// All the structs of the given module.
    #[returns(ref)]
    structs: OrderedHashMap<StructId<'db>, ast::ItemStruct<'db>>,
    /// All the enums of the given module.
    #[returns(ref)]
    enums: OrderedHashMap<EnumId<'db>, ast::ItemEnum<'db>>,
    /// All the type aliases of the given module.
    #[returns(ref)]
    type_aliases: OrderedHashMap<ModuleTypeAliasId<'db>, ast::ItemTypeAlias<'db>>,
    /// All the extern types of the given module.
    #[returns(ref)]
    extern_types: OrderedHashMap<ExternTypeId<'db>, ast::ItemExternType<'db>>,
}

#[salsa::tracked]
pub struct ModuleNamedItemsData<'db> {
    /// All the constants of the given module.
    #[returns(ref)]
    constants: OrderedHashMap<ConstantId<'db>, ast::ItemConstant<'db>>,
    /// All the *direct* submodules of the given module - including those generated by
    /// macro plugins. To get all the submodules including nested modules, use
    /// [`collect_modules_under`].
    #[returns(ref)]
    submodules: OrderedHashMap<SubmoduleId<'db>, ast::ItemModule<'db>>,
    /// All the uses of the given module.
    #[returns(ref)]
    uses: OrderedHashMap<UseId<'db>, ast::UsePathLeaf<'db>>,
    #[returns(ref)]
    free_functions: OrderedHashMap<FreeFunctionId<'db>, ast::FunctionWithBody<'db>>,
    /// All the impl aliases of the given module.
    #[returns(ref)]
    impl_aliases: OrderedHashMap<ImplAliasId<'db>, ast::ItemImplAlias<'db>>,
    /// All the traits of the given module.
    #[returns(ref)]
    traits: OrderedHashMap<TraitId<'db>, ast::ItemTrait<'db>>,
    /// All the impls of the given module.
    #[returns(ref)]
    impls: OrderedHashMap<ImplDefId<'db>, ast::ItemImpl<'db>>,
    /// All the extern functions of the given module.
    #[returns(ref)]
    extern_functions: OrderedHashMap<ExternFunctionId<'db>, ast::ItemExternFunction<'db>>,
    /// All the macro declarations of the given module.
    #[returns(ref)]
    macro_declarations: OrderedHashMap<MacroDeclarationId<'db>, ast::ItemMacroDeclaration<'db>>,
}
#[salsa::tracked]
pub struct ModuleUnnamedItemsData<'db> {
    /// All the global uses of the given module.
    #[returns(ref)]
    global_uses: OrderedHashMap<GlobalUseId<'db>, ast::UsePathStar<'db>>,
    /// Calls to inline macros in the module (only those that were not handled by plugins).
    #[returns(ref)]
    macro_calls: OrderedHashMap<MacroCallId<'db>, ast::ItemInlineMacro<'db>>,
}

#[salsa::tracked]
pub struct ModuleFilesData<'db> {
    #[returns(ref)]
    files: Vec<FileId<'db>>,
    /// Generation info for each file. Virtual files have Some. Other files have None.
    #[returns(ref)]
    generated_file_aux_data: OrderedHashMap<FileId<'db>, Option<DynGeneratedFileAuxData>>,
    #[returns(ref)]
    plugin_diagnostics: Vec<(ModuleId<'db>, PluginDiagnostic<'db>)>,
    /// Diagnostic notes for diagnostics originating in the plugin generated files identified by
    /// [`FileId`].
    /// Diagnostic notes are added with `note: ` prefix at the end of diagnostic display.
    #[returns(ref)]
    diagnostics_notes: PluginFileDiagnosticNotes<'db>,
}

impl<'db> ModuleData<'db> {
    /// All the constants of the given module.
    pub fn constants(
        &self,
        db: &'db dyn Database,
    ) -> &'db OrderedHashMap<ConstantId<'db>, ast::ItemConstant<'db>> {
        self.named_items_data(db).constants(db)
    }
    /// All the submodules of the given module.
    pub fn submodules(
        &self,
        db: &'db dyn Database,
    ) -> &'db OrderedHashMap<SubmoduleId<'db>, ast::ItemModule<'db>> {
        self.named_items_data(db).submodules(db)
    }
    /// All the uses of the given module.
    pub fn uses(
        &self,
        db: &'db dyn Database,
    ) -> &'db OrderedHashMap<UseId<'db>, ast::UsePathLeaf<'db>> {
        self.named_items_data(db).uses(db)
    }
    /// All the free functions of the given module.
    pub fn free_functions(
        &self,
        db: &'db dyn Database,
    ) -> &'db OrderedHashMap<FreeFunctionId<'db>, ast::FunctionWithBody<'db>> {
        self.named_items_data(db).free_functions(db)
    }
    /// All the structs of the given module.
    pub fn structs(
        &self,
        db: &'db dyn Database,
    ) -> &'db OrderedHashMap<StructId<'db>, ast::ItemStruct<'db>> {
        self.types_data(db).structs(db)
    }

    /// All the enums of the given module.
    pub fn enums(
        &self,
        db: &'db dyn Database,
    ) -> &'db OrderedHashMap<EnumId<'db>, ast::ItemEnum<'db>> {
        self.types_data(db).enums(db)
    }

    /// All the type aliases of the given module.
    pub fn type_aliases(
        &self,
        db: &'db dyn Database,
    ) -> &'db OrderedHashMap<ModuleTypeAliasId<'db>, ast::ItemTypeAlias<'db>> {
        self.types_data(db).type_aliases(db)
    }

    /// All the impl aliases of the given module.
    pub fn impl_aliases(
        &self,
        db: &'db dyn Database,
    ) -> &'db OrderedHashMap<ImplAliasId<'db>, ast::ItemImplAlias<'db>> {
        self.named_items_data(db).impl_aliases(db)
    }
    /// All the traits of the given module.
    pub fn traits(
        &self,
        db: &'db dyn Database,
    ) -> &'db OrderedHashMap<TraitId<'db>, ast::ItemTrait<'db>> {
        self.named_items_data(db).traits(db)
    }
    /// All the impls of the given module.
    pub fn impls(
        &self,
        db: &'db dyn Database,
    ) -> &'db OrderedHashMap<ImplDefId<'db>, ast::ItemImpl<'db>> {
        self.named_items_data(db).impls(db)
    }
    /// All the extern types of the given module.
    pub fn extern_types(
        &self,
        db: &'db dyn Database,
    ) -> &'db OrderedHashMap<ExternTypeId<'db>, ast::ItemExternType<'db>> {
        self.types_data(db).extern_types(db)
    }
    /// All the extern functions of the given module.
    pub fn extern_functions(
        &self,
        db: &'db dyn Database,
    ) -> &'db OrderedHashMap<ExternFunctionId<'db>, ast::ItemExternFunction<'db>> {
        self.named_items_data(db).extern_functions(db)
    }
    /// All the macro declarations of the given module.
    pub fn macro_declarations(
        &self,
        db: &'db dyn Database,
    ) -> &'db OrderedHashMap<MacroDeclarationId<'db>, ast::ItemMacroDeclaration<'db>> {
        self.named_items_data(db).macro_declarations(db)
    }
    /// All the global uses of the given module.
    pub fn global_uses(
        &self,
        db: &'db dyn Database,
    ) -> &'db OrderedHashMap<GlobalUseId<'db>, ast::UsePathStar<'db>> {
        self.unnamed_items_data(db).global_uses(db)
    }
    /// Calls to inline macros in the module (only those that were not handled by plugins).
    pub fn macro_calls(
        &self,
        db: &'db dyn Database,
    ) -> &'db OrderedHashMap<MacroCallId<'db>, ast::ItemInlineMacro<'db>> {
        self.unnamed_items_data(db).macro_calls(db)
    }

    /// All the files of the given module.
    pub fn files(&self, db: &'db dyn Database) -> &'db Vec<FileId<'db>> {
        self.files_data(db).files(db)
    }

    /// Generation info for each file. Virtual files have Some. Other files have None.
    pub fn generated_file_aux_data(
        &self,
        db: &'db dyn Database,
    ) -> &'db OrderedHashMap<FileId<'db>, Option<DynGeneratedFileAuxData>> {
        self.files_data(db).generated_file_aux_data(db)
    }

    pub fn plugin_diagnostics(
        &self,
        db: &'db dyn Database,
    ) -> &'db Vec<(ModuleId<'db>, PluginDiagnostic<'db>)> {
        self.files_data(db).plugin_diagnostics(db)
    }

    /// Diagnostic notes for diagnostics originating in the plugin generated files identified by
    /// [`FileId`].
    /// Diagnostic notes are added with `note: ` prefix at the end of diagnostic display.
    pub fn diagnostics_notes(&self, db: &'db dyn Database) -> &'db PluginFileDiagnosticNotes<'db> {
        self.files_data(db).diagnostics_notes(db)
    }
}

/// Information about generated files from running on a module file.
#[derive(Clone, Debug, Eq, PartialEq, salsa::Update)]
pub struct PrivModuleSubFiles<'db> {
    /// The files generated by plugins running on items.
    files: OrderedHashMap<FileId<'db>, VirtualFile<'db>>,
    /// The aux data per such file.
    aux_data: OrderedHashMap<FileId<'db>, Option<DynGeneratedFileAuxData>>,
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
            parent_module_data.generated_file_aux_data(db)
                [&item_module_ast.stable_ptr(db).untyped().file_id(db)]
                .clone()
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
    let mut aux_data = OrderedHashMap::default();
    let mut files = Vec::new();
    let mut plugin_diagnostics = Vec::new();
    let mut diagnostics_notes = OrderedHashMap::default();

    let mut items = vec![];
    aux_data.insert(module_file, main_file_aux_data);
    while let Some(file_id) = file_queue.pop_front() {
        files.push(file_id);

        let priv_module_data = module_sub_files(db, module_id, file_id).maybe_as_ref()?;
        diagnostics_notes.extend(priv_module_data.diagnostics_notes.clone().into_iter());
        file_queue.extend(priv_module_data.files.keys().copied());
        for diag in &priv_module_data.plugin_diagnostics {
            plugin_diagnostics.push((module_id, diag.clone()));
        }
        aux_data.extend(
            priv_module_data.aux_data.iter().map(|(file, aux_data)| (*file, aux_data.clone())),
        );
        for item_ast in &priv_module_data.items {
            match item_ast.clone() {
                ast::ModuleItem::Constant(constant) => {
                    let item_id = ConstantLongId(module_id, constant.stable_ptr(db)).intern(db);
                    constants.insert(item_id, constant);
                    items.push(ModuleItemId::Constant(item_id));
                }
                ast::ModuleItem::Module(module) => {
                    let item_id = SubmoduleLongId(module_id, module.stable_ptr(db)).intern(db);
                    submodules.insert(item_id, module);
                    items.push(ModuleItemId::Submodule(item_id));
                }
                ast::ModuleItem::Use(us) => {
                    for leaf in get_all_path_leaves(db, &us) {
                        let id = UseLongId(module_id, leaf.stable_ptr(db)).intern(db);
                        uses.insert(id, leaf);
                        items.push(ModuleItemId::Use(id));
                    }
                    for star in get_all_path_stars(db, &us) {
                        let id = GlobalUseLongId(module_id, star.stable_ptr(db)).intern(db);
                        global_uses.insert(id, star);
                    }
                }
                ast::ModuleItem::FreeFunction(function) => {
                    let item_id = FreeFunctionLongId(module_id, function.stable_ptr(db)).intern(db);
                    free_functions.insert(item_id, function);
                    items.push(ModuleItemId::FreeFunction(item_id));
                }
                ast::ModuleItem::ExternFunction(extern_function) => {
                    let item_id =
                        ExternFunctionLongId(module_id, extern_function.stable_ptr(db)).intern(db);
                    extern_functions.insert(item_id, extern_function);
                    items.push(ModuleItemId::ExternFunction(item_id));
                }
                ast::ModuleItem::ExternType(extern_type) => {
                    let item_id =
                        ExternTypeLongId(module_id, extern_type.stable_ptr(db)).intern(db);
                    extern_types.insert(item_id, extern_type);
                    items.push(ModuleItemId::ExternType(item_id));
                }
                ast::ModuleItem::Trait(trt) => {
                    let item_id = TraitLongId(module_id, trt.stable_ptr(db)).intern(db);
                    traits.insert(item_id, trt);
                    items.push(ModuleItemId::Trait(item_id));
                }
                ast::ModuleItem::Impl(imp) => {
                    let item_id = ImplDefLongId(module_id, imp.stable_ptr(db)).intern(db);
                    impls.insert(item_id, imp);
                    items.push(ModuleItemId::Impl(item_id));
                }
                ast::ModuleItem::Struct(structure) => {
                    let item_id = StructLongId(module_id, structure.stable_ptr(db)).intern(db);
                    structs.insert(item_id, structure);
                    items.push(ModuleItemId::Struct(item_id));
                }
                ast::ModuleItem::Enum(enm) => {
                    let item_id = EnumLongId(module_id, enm.stable_ptr(db)).intern(db);
                    enums.insert(item_id, enm);
                    items.push(ModuleItemId::Enum(item_id));
                }
                ast::ModuleItem::TypeAlias(type_alias) => {
                    let item_id =
                        ModuleTypeAliasLongId(module_id, type_alias.stable_ptr(db)).intern(db);
                    type_aliases.insert(item_id, type_alias);
                    items.push(ModuleItemId::TypeAlias(item_id));
                }
                ast::ModuleItem::ImplAlias(impl_alias) => {
                    let item_id = ImplAliasLongId(module_id, impl_alias.stable_ptr(db)).intern(db);
                    impl_aliases.insert(item_id, impl_alias);
                    items.push(ModuleItemId::ImplAlias(item_id));
                }
                ast::ModuleItem::MacroDeclaration(macro_declaration) => {
                    let item_id =
                        MacroDeclarationLongId(module_id, macro_declaration.stable_ptr(db))
                            .intern(db);
                    macro_declarations.insert(item_id, macro_declaration);
                    items.push(ModuleItemId::MacroDeclaration(item_id));
                }
                ast::ModuleItem::InlineMacro(inline_macro_ast) => {
                    let item_id =
                        MacroCallLongId(module_id, inline_macro_ast.stable_ptr(db)).intern(db);
                    macro_calls.insert(item_id, inline_macro_ast.clone());
                }
                ast::ModuleItem::HeaderDoc(_) => {}
                ast::ModuleItem::Missing(_) => {}
            }
        }
    }
    let types_data = ModuleTypesData::new(db, structs, enums, type_aliases, extern_types);
    let named_items_data = ModuleNamedItemsData::new(
        db,
        constants,
        submodules,
        uses,
        free_functions,
        impl_aliases,
        traits,
        impls,
        extern_functions,
        macro_declarations,
    );
    let unnamed_items_data = ModuleUnnamedItemsData::new(db, global_uses, macro_calls);
    let files_data =
        ModuleFilesData::new(db, files, aux_data, plugin_diagnostics, diagnostics_notes);
    Ok(ModuleData::new(db, items, types_data, named_items_data, unnamed_items_data, files_data))
}

pub(crate) fn module_data<'db>(
    db: &'db dyn Database,
    module_id: ModuleId<'db>,
) -> Maybe<ModuleData<'db>> {
    priv_module_data_helper(db, (), module_id)
}

pub type ModuleDataCacheAndLoadingData<'db> =
    (Arc<OrderedHashMap<ModuleId<'db>, ModuleData<'db>>>, Arc<DefCacheLoadingData<'db>>);

#[salsa::tracked]
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
    let mut aux_data = OrderedHashMap::default();
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
                        name: SmolStrId::from(db, generated.name),
                        content: SmolStrId::from(db, generated.content),
                        code_mappings: generated.code_mappings.into(),
                        kind: FileKind::Module,
                        original_item_removed: remove_original_item,
                    },
                );
                aux_data.insert(generated_file_id, generated.aux_data);
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
) -> OrderedHashSet<SmolStrId<'db>> {
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
                extra_allowed_attributes.insert(segment.ident(db).text(db));
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
pub fn validate_attributes_flat<'db, Item: QueryAttrs<'db> + TypedSyntaxNode<'db>>(
    db: &'db dyn Database,
    allowed_attributes: &OrderedHashSet<SmolStrId<'db>>,
    extra_allowed_attributes: &OrderedHashSet<SmolStrId<'db>>,
    item: &Item,
    plugin_diagnostics: &mut Vec<PluginDiagnostic<'db>>,
) {
    let local_extra_attributes = collect_extra_allowed_attributes(db, item, plugin_diagnostics);
    for attr in item.attributes_elements(db) {
        let attr_text = attr.attr(db).as_syntax_node().get_text_without_trivia(db);
        if !(allowed_attributes.contains(&attr_text)
            || extra_allowed_attributes.contains(&attr_text)
            || local_extra_attributes.contains(&attr_text))
        {
            plugin_diagnostics.push(PluginDiagnostic::error(
                attr.stable_ptr(db),
                "Unsupported attribute.".to_string(),
            ));
        }
    }

    // Additional semantic validation for `#[path("...")]` attribute.
    for attr in item.query_attr(db, PATH_ATTR) {
        let node = item.as_syntax_node();
        let Some(item_module) = ast::ItemModule::cast(db, node) else {
            plugin_diagnostics.push(PluginDiagnostic::error(
                attr.stable_ptr(db),
                "`#[path(..)]` is only allowed on module declarations.".to_string(),
            ));
            continue;
        };
        // Must be file-based (`mod name;`), not inline.
        if matches!(item_module.body(db), MaybeModuleBody::Some(_)) {
            plugin_diagnostics.push(PluginDiagnostic::error(
                attr.stable_ptr(db),
                "`#[path(..)]` requires a file-based module: use `mod name;` with a semicolon."
                    .to_string(),
            ));
            continue;
        }

        let args = attr.arguments(db);
        if extract_path_arg(db, &args).is_none() {
            plugin_diagnostics.push(PluginDiagnostic::error(
                args.stable_ptr(db),
                "`#[path(..)]` expects exactly one string literal argument.".to_string(),
            ));
        }
    }
}

/// Extracts the path argument from the given attribute arguments.
fn extract_path_arg<'db>(
    db: &'db dyn Database,
    args: &ast::OptionArgListParenthesized<'db>,
) -> Option<ast::TerminalString<'db>> {
    match args {
        ast::OptionArgListParenthesized::Empty(_) => None,
        ast::OptionArgListParenthesized::ArgListParenthesized(args) => {
            let [arg] = args.arguments(db).elements(db).collect_array()?;
            if let ast::Expr::String(path) = try_extract_unnamed_arg(db, &arg)? {
                Some(path)
            } else {
                None
            }
        }
    }
}

/// Validates that all attributes on all items in the given element list are in the allowed set or
/// adds diagnostics.
fn validate_attributes_element_list<'db, Item: QueryAttrs<'db> + TypedSyntaxNode<'db>>(
    db: &'db dyn Database,
    allowed_attributes: &OrderedHashSet<SmolStrId<'db>>,
    extra_allowed_attributes: &OrderedHashSet<SmolStrId<'db>>,
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
    allowed_attributes: &OrderedHashSet<SmolStrId<'db>>,
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
        defs_group_input(self.as_dyn_database())
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
        defs_group_input(self.as_dyn_database())
            .set_inline_macro_plugin_overrides(self)
            .to(Some(overrides));
    }
}

impl<T: DefsGroup + ?Sized> DefsGroupEx for T {}
