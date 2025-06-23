use std::hash::{Hash, Hasher};
use std::ops::{Deref, DerefMut};
use std::path::PathBuf;
use std::sync::Arc;

use cairo_lang_diagnostics::{DiagnosticLocation, DiagnosticNote, Maybe, Severity};
use cairo_lang_filesystem::flag::Flag;
use cairo_lang_filesystem::ids::{
    CodeMapping, CrateId, CrateLongId, FileId, FileKind, FileLongId, VirtualFile,
};
use cairo_lang_filesystem::span::{TextOffset, TextSpan, TextWidth};
use cairo_lang_syntax::node::ast::{
    FunctionWithBodyPtr, GenericParamPtr, ItemConstantPtr, ItemEnumPtr, ItemExternFunctionPtr,
    ItemExternTypePtr, ItemImplAliasPtr, ItemImplPtr, ItemMacroDeclarationPtr, ItemModulePtr,
    ItemStructPtr, ItemTraitPtr, ItemTypeAliasPtr, UsePathLeafPtr, UsePathStarPtr,
};
use cairo_lang_syntax::node::green::{GreenNode, GreenNodeDetails};
use cairo_lang_syntax::node::ids::{GreenId, SyntaxStablePtrId};
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::stable_ptr::SyntaxStablePtr;
use cairo_lang_syntax::node::{SyntaxNode, TypedSyntaxNode, ast};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::{Intern, LookupIntern};
use salsa::InternKey;
use serde::{Deserialize, Serialize};
use smol_str::SmolStr;

use crate::db::{DefsGroup, ModuleData, ModuleDataCacheAndLoadingData};
use crate::ids::{
    ConstantId, ConstantLongId, EnumId, EnumLongId, ExternFunctionId, ExternFunctionLongId,
    ExternTypeId, ExternTypeLongId, FileIndex, FreeFunctionId, FreeFunctionLongId, GenericParamId,
    GenericParamLongId, GlobalUseId, GlobalUseLongId, ImplAliasId, ImplAliasLongId, ImplDefId,
    ImplDefLongId, LanguageElementId, MacroDeclarationId, MacroDeclarationLongId, ModuleFileId,
    ModuleId, ModuleItemId, ModuleTypeAliasId, ModuleTypeAliasLongId, PluginGeneratedFileId,
    PluginGeneratedFileLongId, StructId, StructLongId, SubmoduleId, SubmoduleLongId, TraitId,
    TraitLongId, UseId, UseLongId,
};
use crate::plugin::{DynGeneratedFileAuxData, PluginDiagnostic};

/// Metadata for a cached crate.
#[derive(Serialize, Deserialize)]
pub struct CachedCrateMetadata {
    /// Hash of the settings the crate was compiles with.
    pub settings: Option<u64>,
    /// The version of the compiler that compiled the crate.
    pub compiler_version: String,
    /// The global flags the crate was compiled with.
    pub global_flags: OrderedHashMap<SmolStr, Flag>,
}

impl CachedCrateMetadata {
    /// Creates a new [CachedCrateMetadata] from the input crate with the current settings.
    pub fn new(crate_id: CrateId, db: &dyn DefsGroup) -> Self {
        let settings = db.crate_config(crate_id).map(|config| config.settings).map(|v| {
            let mut hasher = xxhash_rust::xxh3::Xxh3::default();
            v.hash(&mut hasher);
            hasher.finish()
        });
        let compiler_version = env!("CARGO_PKG_VERSION").to_string();
        let global_flags = db
            .flags()
            .iter()
            .map(|(flag_id, flag)| (flag_id.lookup_intern(db).0, (**flag).clone()))
            .collect();
        Self { settings, compiler_version, global_flags }
    }
}

/// Validates that the metadata of the cached crate is valid.
fn validate_metadata(crate_id: CrateId, metadata: &CachedCrateMetadata, db: &dyn DefsGroup) {
    let current_metadata = CachedCrateMetadata::new(crate_id, db);

    if current_metadata.compiler_version != metadata.compiler_version {
        panic!("Cached crate was compiled with a different compiler version.");
    }
    if current_metadata.settings != metadata.settings {
        panic!("Cached crate was compiled with different settings.");
    }

    if !current_metadata.global_flags.eq_unordered(&metadata.global_flags) {
        panic!("Cached crate was compiled with different global flags.");
    }
}

type DefCache = (CachedCrateMetadata, Vec<(ModuleIdCached, ModuleDataCached)>, DefCacheLookups);

/// Load the cached lowering of a crate if it has a cache file configuration.
pub fn load_cached_crate_modules(
    db: &dyn DefsGroup,
    crate_id: CrateId,
) -> Option<ModuleDataCacheAndLoadingData> {
    let blob_id = db.crate_config(crate_id)?.cache_file?;
    let Some(content) = db.blob_content(blob_id) else {
        return Default::default();
    };

    let size = usize::from_be_bytes(content[..8].try_into().unwrap());

    let content = &content[8..size + 8];

    let ((metadata, module_data, defs_lookups), _): (DefCache, _) =
        bincode::serde::borrow_decode_from_slice(content, bincode::config::standard())
            .unwrap_or_else(|e| {
                panic!(
                    "failed to deserialize modules cache for crate `{}`: {e}",
                    crate_id.name(db),
                )
            });

    validate_metadata(crate_id, &metadata, db);

    let mut ctx = DefCacheLoadingContext::new(db, defs_lookups, crate_id);
    Some((
        module_data
            .into_iter()
            .map(|(module_id, module_data)| {
                let module_id = module_id.embed(&mut ctx);

                let module_data = module_data.embed(&mut ctx);
                (module_id, module_data)
            })
            .collect::<OrderedHashMap<_, _>>()
            .into(),
        ctx.data.into(),
    ))
}

/// Cache the module_data of each module in the crate and returns the cache and the context.
pub fn generate_crate_def_cache(
    db: &dyn DefsGroup,
    crate_id: cairo_lang_filesystem::ids::CrateId,
    ctx: &mut DefCacheSavingContext<'_>,
) -> Maybe<Vec<(ModuleIdCached, ModuleDataCached)>> {
    let modules = db.crate_modules(crate_id);

    let cached: Vec<(ModuleIdCached, ModuleDataCached)> = modules
        .iter()
        .map(|id| {
            let module_data = db.priv_module_data(*id)?;
            Ok((ModuleIdCached::new(*id, ctx), ModuleDataCached::new(module_data, ctx)))
        })
        .collect::<Maybe<Vec<_>>>()?;

    Ok(cached)
}

/// Context for loading cache into the database.
pub struct DefCacheLoadingContext<'db> {
    /// The variable ids of the flat lowered that is currently being loaded.
    db: &'db dyn DefsGroup,

    /// data for loading the entire cache into the database.
    data: DefCacheLoadingData,
}

impl<'db> DefCacheLoadingContext<'db> {
    pub fn new(db: &'db dyn DefsGroup, lookups: DefCacheLookups, self_crate_id: CrateId) -> Self {
        let mut res = Self { db, data: DefCacheLoadingData::new(lookups, self_crate_id) };
        res.embed_lookups();
        res
    }
    fn embed_lookups(&mut self) {
        for id in 0..self.lookups.green_ids_lookup.len() {
            GreenIdCached(id).embed(self);
        }

        for id in 0..self.lookups.crate_ids_lookup.len() {
            CrateIdCached::Other(id).embed(self);
        }
        for id in 0..self.lookups.syntax_stable_ptr_ids_lookup.len() {
            SyntaxStablePtrIdCached(id).embed(self);
        }
        for id in 0..self.lookups.submodule_ids_lookup.len() {
            SubmoduleIdCached(id).embed(self);
        }
        for id in 0..self.lookups.constant_ids_lookup.len() {
            ConstantIdCached(id).embed(self);
        }
        for id in 0..self.lookups.use_ids_lookup.len() {
            UseIdCached(id).embed(self);
        }
        for id in 0..self.lookups.free_function_ids_lookup.len() {
            FreeFunctionIdCached(id).embed(self);
        }
        for id in 0..self.lookups.struct_ids_lookup.len() {
            StructIdCached(id).embed(self);
        }
        for id in 0..self.lookups.enum_ids_lookup.len() {
            EnumIdCached(id).embed(self);
        }
        for id in 0..self.lookups.type_alias_ids_lookup.len() {
            ModuleTypeAliasIdCached(id).embed(self);
        }
        for id in 0..self.lookups.impl_alias_ids_lookup.len() {
            ImplAliasIdCached(id).embed(self);
        }
        for id in 0..self.lookups.trait_ids_lookup.len() {
            TraitIdCached(id).embed(self);
        }
        for id in 0..self.lookups.impl_def_ids_lookup.len() {
            ImplDefIdCached(id).embed(self);
        }
        for id in 0..self.lookups.extern_type_ids_lookup.len() {
            ExternTypeIdCached(id).embed(self);
        }
        for id in 0..self.lookups.extern_function_ids_lookup.len() {
            ExternFunctionIdCached(id).embed(self);
        }
        for id in 0..self.lookups.global_use_ids_lookup.len() {
            GlobalUseIdCached(id).embed(self);
        }
        for id in 0..self.lookups.file_ids_lookup.len() {
            FileIdCached(id).embed(self);
        }
    }
}

impl Deref for DefCacheLoadingContext<'_> {
    type Target = DefCacheLoadingData;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}
impl DerefMut for DefCacheLoadingContext<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.data
    }
}

/// Data for loading cache into the database.
#[derive(PartialEq, Eq)]
pub struct DefCacheLoadingData {
    green_ids: OrderedHashMap<GreenIdCached, GreenId>,
    crate_ids: OrderedHashMap<CrateIdCached, CrateId>,
    syntax_stable_ptr_ids: OrderedHashMap<SyntaxStablePtrIdCached, SyntaxStablePtrId>,
    submodule_ids: OrderedHashMap<SubmoduleIdCached, SubmoduleId>,
    constant_ids: OrderedHashMap<ConstantIdCached, ConstantId>,
    use_ids: OrderedHashMap<UseIdCached, UseId>,
    free_function_ids: OrderedHashMap<FreeFunctionIdCached, FreeFunctionId>,
    struct_ids: OrderedHashMap<StructIdCached, StructId>,
    enum_ids: OrderedHashMap<EnumIdCached, EnumId>,
    type_alias_ids: OrderedHashMap<ModuleTypeAliasIdCached, ModuleTypeAliasId>,
    impl_alias_ids: OrderedHashMap<ImplAliasIdCached, ImplAliasId>,
    trait_ids: OrderedHashMap<TraitIdCached, TraitId>,
    impl_def_ids: OrderedHashMap<ImplDefIdCached, ImplDefId>,
    extern_type_ids: OrderedHashMap<ExternTypeIdCached, ExternTypeId>,
    extern_function_ids: OrderedHashMap<ExternFunctionIdCached, ExternFunctionId>,
    macro_declaration_ids: OrderedHashMap<MacroDeclarationIdCached, MacroDeclarationId>,
    global_use_ids: OrderedHashMap<GlobalUseIdCached, GlobalUseId>,

    file_ids: OrderedHashMap<FileIdCached, FileId>,
    self_crate_id: CrateId,
    lookups: DefCacheLookups,
}

impl DefCacheLoadingData {
    fn new(lookups: DefCacheLookups, self_crate_id: CrateId) -> Self {
        Self {
            green_ids: OrderedHashMap::default(),
            syntax_stable_ptr_ids: OrderedHashMap::default(),
            crate_ids: OrderedHashMap::default(),
            submodule_ids: OrderedHashMap::default(),
            constant_ids: OrderedHashMap::default(),
            use_ids: OrderedHashMap::default(),
            free_function_ids: OrderedHashMap::default(),
            struct_ids: OrderedHashMap::default(),
            enum_ids: OrderedHashMap::default(),
            type_alias_ids: OrderedHashMap::default(),
            impl_alias_ids: OrderedHashMap::default(),
            trait_ids: OrderedHashMap::default(),
            impl_def_ids: OrderedHashMap::default(),
            extern_type_ids: OrderedHashMap::default(),
            extern_function_ids: OrderedHashMap::default(),
            macro_declaration_ids: OrderedHashMap::default(),
            global_use_ids: OrderedHashMap::default(),

            file_ids: OrderedHashMap::default(),
            self_crate_id,
            lookups,
        }
    }
}

impl std::fmt::Debug for DefCacheLoadingData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("DefCacheLoadingData").finish()
    }
}

impl Deref for DefCacheLoadingData {
    type Target = DefCacheLookups;

    fn deref(&self) -> &Self::Target {
        &self.lookups
    }
}
impl DerefMut for DefCacheLoadingData {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.lookups
    }
}

/// Context for saving cache from the database.
pub struct DefCacheSavingContext<'db> {
    db: &'db dyn DefsGroup,
    data: DefCacheSavingData,
    self_crate_id: CrateId,
}
impl Deref for DefCacheSavingContext<'_> {
    type Target = DefCacheSavingData;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}
impl DerefMut for DefCacheSavingContext<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.data
    }
}
impl<'db> DefCacheSavingContext<'db> {
    pub fn new(db: &'db dyn DefsGroup, self_crate_id: CrateId) -> Self {
        Self { db, data: DefCacheSavingData::default(), self_crate_id }
    }
}

/// Data for saving cache from the database.
#[derive(Default)]
pub struct DefCacheSavingData {
    green_ids: OrderedHashMap<GreenId, GreenIdCached>,
    crate_ids: OrderedHashMap<CrateId, CrateIdCached>,
    submodule_ids: OrderedHashMap<SubmoduleId, SubmoduleIdCached>,
    constant_ids: OrderedHashMap<ConstantId, ConstantIdCached>,
    use_ids: OrderedHashMap<UseId, UseIdCached>,
    free_function_ids: OrderedHashMap<FreeFunctionId, FreeFunctionIdCached>,
    struct_ids: OrderedHashMap<StructId, StructIdCached>,
    enum_ids: OrderedHashMap<EnumId, EnumIdCached>,
    type_alias_ids: OrderedHashMap<ModuleTypeAliasId, ModuleTypeAliasIdCached>,
    impl_alias_ids: OrderedHashMap<ImplAliasId, ImplAliasIdCached>,
    trait_ids: OrderedHashMap<TraitId, TraitIdCached>,
    impl_def_ids: OrderedHashMap<ImplDefId, ImplDefIdCached>,
    extern_type_ids: OrderedHashMap<ExternTypeId, ExternTypeIdCached>,
    extern_function_ids: OrderedHashMap<ExternFunctionId, ExternFunctionIdCached>,
    global_use_ids: OrderedHashMap<GlobalUseId, GlobalUseIdCached>,
    macro_declaration_ids: OrderedHashMap<MacroDeclarationId, MacroDeclarationIdCached>,

    syntax_stable_ptr_ids: OrderedHashMap<SyntaxStablePtrId, SyntaxStablePtrIdCached>,
    file_ids: OrderedHashMap<FileId, FileIdCached>,

    pub lookups: DefCacheLookups,
}

impl Deref for DefCacheSavingData {
    type Target = DefCacheLookups;

    fn deref(&self) -> &Self::Target {
        &self.lookups
    }
}
impl DerefMut for DefCacheSavingData {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.lookups
    }
}

#[derive(Serialize, Deserialize)]
pub struct ModuleDataCached {
    items: Vec<ModuleItemIdCached>,

    constants: OrderedHashMap<ConstantIdCached, TypeSyntaxNodeCached<ast::ItemConstant>>,
    submodules: OrderedHashMap<SubmoduleIdCached, TypeSyntaxNodeCached<ast::ItemModule>>,
    uses: OrderedHashMap<UseIdCached, TypeSyntaxNodeCached<ast::UsePathLeaf>>,
    free_functions:
        OrderedHashMap<FreeFunctionIdCached, TypeSyntaxNodeCached<ast::FunctionWithBody>>,
    structs: OrderedHashMap<StructIdCached, TypeSyntaxNodeCached<ast::ItemStruct>>,
    enums: OrderedHashMap<EnumIdCached, TypeSyntaxNodeCached<ast::ItemEnum>>,
    type_aliases: OrderedHashMap<ModuleTypeAliasIdCached, TypeSyntaxNodeCached<ast::ItemTypeAlias>>,
    impl_aliases: OrderedHashMap<ImplAliasIdCached, TypeSyntaxNodeCached<ast::ItemImplAlias>>,
    traits: OrderedHashMap<TraitIdCached, TypeSyntaxNodeCached<ast::ItemTrait>>,
    impls: OrderedHashMap<ImplDefIdCached, TypeSyntaxNodeCached<ast::ItemImpl>>,
    extern_types: OrderedHashMap<ExternTypeIdCached, TypeSyntaxNodeCached<ast::ItemExternType>>,
    extern_functions:
        OrderedHashMap<ExternFunctionIdCached, TypeSyntaxNodeCached<ast::ItemExternFunction>>,
    macro_declarations:
        OrderedHashMap<MacroDeclarationIdCached, TypeSyntaxNodeCached<ast::ItemMacroDeclaration>>,
    global_uses: OrderedHashMap<GlobalUseIdCached, TypeSyntaxNodeCached<ast::UsePathStar>>,

    files: Vec<FileIdCached>,

    generated_file_aux_data: Vec<Option<DynGeneratedFileAuxData>>,
    plugin_diagnostics: Vec<(ModuleFileCached, PluginDiagnosticCached)>,
    diagnostics_notes: PluginFileDiagnosticNotesCached,
}
impl ModuleDataCached {
    fn new(module_data: ModuleData, ctx: &mut DefCacheSavingContext<'_>) -> Self {
        Self {
            items: module_data.items.iter().map(|id| ModuleItemIdCached::new(*id, ctx)).collect(),
            constants: module_data
                .constants
                .iter()
                .map(|(id, node)| {
                    (ConstantIdCached::new(*id, ctx), TypeSyntaxNodeCached::new(node.clone(), ctx))
                })
                .collect(),

            submodules: module_data
                .submodules
                .iter()
                .map(|(id, node)| {
                    (SubmoduleIdCached::new(*id, ctx), TypeSyntaxNodeCached::new(node.clone(), ctx))
                })
                .collect(),

            uses: module_data
                .uses
                .iter()
                .map(|(id, node)| {
                    (UseIdCached::new(*id, ctx), TypeSyntaxNodeCached::new(node.clone(), ctx))
                })
                .collect(),
            free_functions: module_data
                .free_functions
                .iter()
                .map(|(id, node)| {
                    (
                        FreeFunctionIdCached::new(*id, ctx),
                        TypeSyntaxNodeCached::new(node.clone(), ctx),
                    )
                })
                .collect(),
            structs: module_data
                .structs
                .iter()
                .map(|(id, node)| {
                    (StructIdCached::new(*id, ctx), TypeSyntaxNodeCached::new(node.clone(), ctx))
                })
                .collect(),
            enums: module_data
                .enums
                .iter()
                .map(|(id, node)| {
                    (EnumIdCached::new(*id, ctx), TypeSyntaxNodeCached::new(node.clone(), ctx))
                })
                .collect(),
            type_aliases: module_data
                .type_aliases
                .iter()
                .map(|(id, node)| {
                    (
                        ModuleTypeAliasIdCached::new(*id, ctx),
                        TypeSyntaxNodeCached::new(node.clone(), ctx),
                    )
                })
                .collect(),
            impl_aliases: module_data
                .impl_aliases
                .iter()
                .map(|(id, node)| {
                    (ImplAliasIdCached::new(*id, ctx), TypeSyntaxNodeCached::new(node.clone(), ctx))
                })
                .collect(),
            traits: module_data
                .traits
                .iter()
                .map(|(id, node)| {
                    (TraitIdCached::new(*id, ctx), TypeSyntaxNodeCached::new(node.clone(), ctx))
                })
                .collect(),
            impls: module_data
                .impls
                .iter()
                .map(|(id, node)| {
                    (ImplDefIdCached::new(*id, ctx), TypeSyntaxNodeCached::new(node.clone(), ctx))
                })
                .collect(),
            extern_types: module_data
                .extern_types
                .iter()
                .map(|(id, node)| {
                    (
                        ExternTypeIdCached::new(*id, ctx),
                        TypeSyntaxNodeCached::new(node.clone(), ctx),
                    )
                })
                .collect(),
            extern_functions: module_data
                .extern_functions
                .iter()
                .map(|(id, node)| {
                    (
                        ExternFunctionIdCached::new(*id, ctx),
                        TypeSyntaxNodeCached::new(node.clone(), ctx),
                    )
                })
                .collect(),

            macro_declarations: module_data
                .macro_declarations
                .iter()
                .map(|(id, node)| {
                    (
                        MacroDeclarationIdCached::new(*id, ctx),
                        TypeSyntaxNodeCached::new(node.clone(), ctx),
                    )
                })
                .collect(),
            global_uses: module_data
                .global_uses
                .iter()
                .map(|(id, node)| {
                    (GlobalUseIdCached::new(*id, ctx), TypeSyntaxNodeCached::new(node.clone(), ctx))
                })
                .collect(),
            files: module_data.files.iter().map(|id| FileIdCached::new(*id, ctx)).collect(),
            generated_file_aux_data: module_data.generated_file_aux_data.to_vec(),

            plugin_diagnostics: module_data
                .plugin_diagnostics
                .iter()
                .map(|(file_id, diagnostic)| {
                    (
                        ModuleFileCached::new(*file_id, ctx),
                        PluginDiagnosticCached::new(diagnostic, ctx),
                    )
                })
                .collect(),
            diagnostics_notes: module_data
                .diagnostics_notes
                .iter()
                .map(|(filed_id, note)| {
                    (
                        FileIdCached::new(*filed_id, ctx),
                        DiagnosticNoteCached::new(note.clone(), ctx),
                    )
                })
                .collect(),
        }
    }
    fn embed(self, ctx: &mut DefCacheLoadingContext<'_>) -> ModuleData {
        ModuleData {
            items: self.items.iter().map(|id| id.embed(ctx)).collect(),
            constants: Arc::new(
                self.constants.iter().map(|(id, node)| (id.embed(ctx), node.embed(ctx))).collect(),
            ),
            submodules: Arc::new(
                self.submodules.iter().map(|(id, node)| (id.embed(ctx), node.embed(ctx))).collect(),
            ),
            uses: Arc::new(
                self.uses.iter().map(|(id, node)| (id.embed(ctx), node.embed(ctx))).collect(),
            ),
            free_functions: Arc::new(
                self.free_functions
                    .iter()
                    .map(|(id, node)| (id.embed(ctx), node.embed(ctx)))
                    .collect(),
            ),
            structs: Arc::new(
                self.structs.iter().map(|(id, node)| (id.embed(ctx), node.embed(ctx))).collect(),
            ),
            enums: Arc::new(
                self.enums.iter().map(|(id, node)| (id.embed(ctx), node.embed(ctx))).collect(),
            ),
            type_aliases: Arc::new(
                self.type_aliases
                    .iter()
                    .map(|(id, node)| (id.embed(ctx), node.embed(ctx)))
                    .collect(),
            ),
            impl_aliases: Arc::new(
                self.impl_aliases
                    .iter()
                    .map(|(id, node)| (id.embed(ctx), node.embed(ctx)))
                    .collect(),
            ),
            traits: Arc::new(
                self.traits.iter().map(|(id, node)| (id.embed(ctx), node.embed(ctx))).collect(),
            ),
            impls: Arc::new(
                self.impls.iter().map(|(id, node)| (id.embed(ctx), node.embed(ctx))).collect(),
            ),
            extern_types: Arc::new(
                self.extern_types
                    .iter()
                    .map(|(id, node)| (id.embed(ctx), node.embed(ctx)))
                    .collect(),
            ),
            extern_functions: Arc::new(
                self.extern_functions
                    .iter()
                    .map(|(id, node)| (id.embed(ctx), node.embed(ctx)))
                    .collect(),
            ),
            macro_declarations: Arc::new(
                self.macro_declarations
                    .iter()
                    .map(|(id, node)| (id.embed(ctx), node.embed(ctx)))
                    .collect(),
            ),
            global_uses: Arc::new(
                self.global_uses
                    .iter()
                    .map(|(id, node)| (id.embed(ctx), node.embed(ctx)))
                    .collect(),
            ),
            files: self.files.iter().map(|id| id.embed(ctx)).collect(),

            plugin_diagnostics: self
                .plugin_diagnostics
                .into_iter()
                .map(|(file_id, diagnostic)| (file_id.embed(ctx), diagnostic.embed(ctx)))
                .collect(),

            diagnostics_notes: self
                .diagnostics_notes
                .into_iter()
                .map(|(file_id, note)| (file_id.embed(ctx), note.embed(ctx)))
                .collect(),

            generated_file_aux_data: self.generated_file_aux_data.into(),
        }
    }
}

/// Saved interned items for the cache.
#[derive(Serialize, Deserialize, Default, PartialEq, Eq)]
pub struct DefCacheLookups {
    green_ids_lookup: Vec<GreenNodeCached>,
    crate_ids_lookup: Vec<CrateCached>,
    syntax_stable_ptr_ids_lookup: Vec<SyntaxStablePtrCached>,
    submodule_ids_lookup: Vec<SubmoduleCached>,
    constant_ids_lookup: Vec<ConstantCached>,
    use_ids_lookup: Vec<UseCached>,
    free_function_ids_lookup: Vec<FreeFunctionCached>,
    struct_ids_lookup: Vec<StructCached>,
    enum_ids_lookup: Vec<EnumCached>,
    type_alias_ids_lookup: Vec<ModuleTypeAliasCached>,
    impl_alias_ids_lookup: Vec<ImplAliasCached>,
    trait_ids_lookup: Vec<TraitCached>,
    impl_def_ids_lookup: Vec<ImplDefCached>,
    extern_type_ids_lookup: Vec<ExternTypeCached>,
    extern_function_ids_lookup: Vec<ExternFunctionCached>,
    global_use_ids_lookup: Vec<GlobalUseCached>,
    macro_declaration_ids_lookup: Vec<MacroDeclarationCached>,

    file_ids_lookup: Vec<FileCached>,
}

#[derive(Serialize, Deserialize, Clone)]
struct TypeSyntaxNodeCached<T: TypedSyntaxNode> {
    syntax_node: SyntaxNodeCached,
    _marker: std::marker::PhantomData<T>,
}
impl<T: TypedSyntaxNode> TypeSyntaxNodeCached<T> {
    fn new(syntax_node: T, ctx: &mut DefCacheSavingContext<'_>) -> Self {
        Self {
            syntax_node: SyntaxNodeCached::new(syntax_node.as_syntax_node(), ctx),
            _marker: std::marker::PhantomData,
        }
    }
    fn embed(&self, ctx: &mut DefCacheLoadingContext<'_>) -> T {
        T::from_syntax_node(ctx.db, self.syntax_node.embed(ctx))
    }
}

#[derive(Serialize, Deserialize, Clone)]
pub struct GenericParamCached {
    language_element: LanguageElementCached,
}
impl GenericParamCached {
    pub fn new(generic_param_id: GenericParamId, ctx: &mut DefCacheSavingContext<'_>) -> Self {
        Self { language_element: LanguageElementCached::new(generic_param_id, ctx) }
    }

    pub fn get_embedded(
        self,
        data: &Arc<DefCacheLoadingData>,
        db: &dyn DefsGroup,
    ) -> GenericParamId {
        let (module_file_id, stable_ptr) = self.language_element.get_embedded(data);
        GenericParamLongId(module_file_id, GenericParamPtr(stable_ptr)).intern(db)
    }
}

#[derive(Serialize, Deserialize, Clone, Hash, Eq, PartialEq)]
struct ModuleFileCached {
    module: ModuleIdCached,
    file_index: usize,
}
impl ModuleFileCached {
    fn new(module_file_id: ModuleFileId, ctx: &mut DefCacheSavingContext<'_>) -> Self {
        Self { module: ModuleIdCached::new(module_file_id.0, ctx), file_index: module_file_id.1.0 }
    }
    fn embed(&self, ctx: &mut DefCacheLoadingContext<'_>) -> ModuleFileId {
        ModuleFileId(self.module.embed(ctx), FileIndex(self.file_index))
    }
    fn get_embedded(self, data: &Arc<DefCacheLoadingData>) -> ModuleFileId {
        ModuleFileId(self.module.get_embedded(data), FileIndex(self.file_index))
    }
}

#[derive(Serialize, Deserialize, Clone, Hash, Eq, PartialEq)]
pub enum ModuleIdCached {
    CrateRoot(CrateIdCached),
    Submodule(SubmoduleIdCached),
}
impl ModuleIdCached {
    fn new(module_id: ModuleId, ctx: &mut DefCacheSavingContext<'_>) -> Self {
        match module_id {
            ModuleId::CrateRoot(crate_id) => {
                ModuleIdCached::CrateRoot(CrateIdCached::new(crate_id, ctx))
            }
            ModuleId::Submodule(submodule_id) => {
                ModuleIdCached::Submodule(SubmoduleIdCached::new(submodule_id, ctx))
            }
        }
    }
    fn embed(&self, ctx: &mut DefCacheLoadingContext<'_>) -> ModuleId {
        match self {
            ModuleIdCached::CrateRoot(crate_id) => ModuleId::CrateRoot(crate_id.embed(ctx)),
            ModuleIdCached::Submodule(submodule_id) => ModuleId::Submodule(submodule_id.embed(ctx)),
        }
    }
    fn get_embedded(self, data: &Arc<DefCacheLoadingData>) -> ModuleId {
        match self {
            ModuleIdCached::CrateRoot(crate_id) => ModuleId::CrateRoot(crate_id.get_embedded(data)),
            ModuleIdCached::Submodule(submodule_id) => {
                ModuleId::Submodule(submodule_id.get_embedded(data))
            }
        }
    }
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq)]
enum CrateCached {
    Real { name: SmolStr, discriminator: Option<SmolStr> },
    Virtual { name: SmolStr, file_id: FileIdCached, settings: String },
}
impl CrateCached {
    fn new(crate_id: CrateLongId, _ctx: &mut DefCacheSavingContext<'_>) -> Self {
        match crate_id {
            CrateLongId::Real { name, discriminator } => CrateCached::Real { name, discriminator },
            CrateLongId::Virtual { name, file_id, settings, cache_file: _ } => {
                CrateCached::Virtual { name, file_id: FileIdCached::new(file_id, _ctx), settings }
            }
        }
    }
    fn embed(self, ctx: &mut DefCacheLoadingContext<'_>) -> CrateLongId {
        match self {
            CrateCached::Real { name, discriminator } => CrateLongId::Real { name, discriminator },
            CrateCached::Virtual { name, file_id, settings } => {
                CrateLongId::Virtual {
                    name,
                    file_id: file_id.embed(ctx),
                    settings,
                    cache_file: None, // todo  if two virtual crates are supported
                }
            }
        }
    }
}
#[derive(Serialize, Deserialize, Copy, Clone, PartialEq, Eq, Hash)]
pub enum CrateIdCached {
    SelfCrate,
    Other(usize),
}
impl CrateIdCached {
    fn new(crate_id: CrateId, ctx: &mut DefCacheSavingContext<'_>) -> Self {
        if crate_id == ctx.self_crate_id {
            return CrateIdCached::SelfCrate;
        }
        if let Some(id) = ctx.crate_ids.get(&crate_id) {
            return *id;
        }
        let crate_long_id = CrateCached::new(crate_id.lookup_intern(ctx.db), ctx);
        let id = CrateIdCached::Other(ctx.crate_ids_lookup.len());
        ctx.crate_ids_lookup.push(crate_long_id);
        ctx.crate_ids.insert(crate_id, id);
        id
    }
    fn embed(self, ctx: &mut DefCacheLoadingContext<'_>) -> CrateId {
        let CrateIdCached::Other(id) = self else {
            return ctx.self_crate_id;
        };

        if let Some(crate_id) = ctx.crate_ids.get(&self) {
            return *crate_id;
        }
        let crate_long_id = ctx.crate_ids_lookup[id].clone();
        let crate_id = crate_long_id.embed(ctx).intern(ctx.db);
        ctx.data.crate_ids.insert(self, crate_id);
        crate_id
    }
    fn get_embedded(self, data: &Arc<DefCacheLoadingData>) -> CrateId {
        let CrateIdCached::Other(_) = self else {
            return data.self_crate_id;
        };
        data.crate_ids[&self]
    }
}

#[derive(Serialize, Deserialize, Copy, Clone)]
enum ModuleItemIdCached {
    Constant(ConstantIdCached),
    Submodule(SubmoduleIdCached),
    Use(UseIdCached),
    FreeFunction(FreeFunctionIdCached),
    Struct(StructIdCached),
    Enum(EnumIdCached),
    TypeAlias(ModuleTypeAliasIdCached),
    ImplAlias(ImplAliasIdCached),
    Trait(TraitIdCached),
    Impl(ImplDefIdCached),
    ExternType(ExternTypeIdCached),
    ExternFunction(ExternFunctionIdCached),
    MacroDeclaration(MacroDeclarationIdCached),
}

impl ModuleItemIdCached {
    fn new(module_item_id: ModuleItemId, ctx: &mut DefCacheSavingContext<'_>) -> Self {
        match module_item_id {
            ModuleItemId::Constant(constant_id) => {
                ModuleItemIdCached::Constant(ConstantIdCached::new(constant_id, ctx))
            }
            ModuleItemId::Submodule(submodule_id) => {
                ModuleItemIdCached::Submodule(SubmoduleIdCached::new(submodule_id, ctx))
            }
            ModuleItemId::Use(use_id) => ModuleItemIdCached::Use(UseIdCached::new(use_id, ctx)),
            ModuleItemId::FreeFunction(free_function_id) => {
                ModuleItemIdCached::FreeFunction(FreeFunctionIdCached::new(free_function_id, ctx))
            }
            ModuleItemId::Struct(struct_id) => {
                ModuleItemIdCached::Struct(StructIdCached::new(struct_id, ctx))
            }
            ModuleItemId::Enum(enum_id) => {
                ModuleItemIdCached::Enum(EnumIdCached::new(enum_id, ctx))
            }
            ModuleItemId::TypeAlias(type_alias_id) => {
                ModuleItemIdCached::TypeAlias(ModuleTypeAliasIdCached::new(type_alias_id, ctx))
            }
            ModuleItemId::ImplAlias(impl_alias_id) => {
                ModuleItemIdCached::ImplAlias(ImplAliasIdCached::new(impl_alias_id, ctx))
            }
            ModuleItemId::Trait(trait_id) => {
                ModuleItemIdCached::Trait(TraitIdCached::new(trait_id, ctx))
            }
            ModuleItemId::Impl(impl_def_id) => {
                ModuleItemIdCached::Impl(ImplDefIdCached::new(impl_def_id, ctx))
            }
            ModuleItemId::ExternType(extern_type_id) => {
                ModuleItemIdCached::ExternType(ExternTypeIdCached::new(extern_type_id, ctx))
            }
            ModuleItemId::ExternFunction(extern_function_id) => ModuleItemIdCached::ExternFunction(
                ExternFunctionIdCached::new(extern_function_id, ctx),
            ),
            ModuleItemId::MacroDeclaration(macro_declaration_id) => {
                ModuleItemIdCached::MacroDeclaration(MacroDeclarationIdCached::new(
                    macro_declaration_id,
                    ctx,
                ))
            }
        }
    }
    fn embed(self, ctx: &mut DefCacheLoadingContext<'_>) -> ModuleItemId {
        match self {
            ModuleItemIdCached::Constant(constant_id) => {
                ModuleItemId::Constant(constant_id.embed(ctx))
            }
            ModuleItemIdCached::Submodule(submodule_id) => {
                ModuleItemId::Submodule(submodule_id.embed(ctx))
            }
            ModuleItemIdCached::Use(use_id) => ModuleItemId::Use(use_id.embed(ctx)),
            ModuleItemIdCached::FreeFunction(free_function_id) => {
                ModuleItemId::FreeFunction(free_function_id.embed(ctx))
            }
            ModuleItemIdCached::Struct(struct_id) => ModuleItemId::Struct(struct_id.embed(ctx)),
            ModuleItemIdCached::Enum(enum_id) => ModuleItemId::Enum(enum_id.embed(ctx)),
            ModuleItemIdCached::TypeAlias(type_alias_id) => {
                ModuleItemId::TypeAlias(type_alias_id.embed(ctx))
            }
            ModuleItemIdCached::ImplAlias(impl_alias_id) => {
                ModuleItemId::ImplAlias(impl_alias_id.embed(ctx))
            }
            ModuleItemIdCached::Trait(trait_id) => ModuleItemId::Trait(trait_id.embed(ctx)),
            ModuleItemIdCached::Impl(impl_def_id) => ModuleItemId::Impl(impl_def_id.embed(ctx)),
            ModuleItemIdCached::ExternType(extern_type_id) => {
                ModuleItemId::ExternType(extern_type_id.embed(ctx))
            }
            ModuleItemIdCached::ExternFunction(extern_function_id) => {
                ModuleItemId::ExternFunction(extern_function_id.embed(ctx))
            }
            ModuleItemIdCached::MacroDeclaration(macro_declaration_id) => {
                ModuleItemId::MacroDeclaration(macro_declaration_id.embed(ctx))
            }
        }
    }
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq)]
struct ConstantCached {
    language_element: LanguageElementCached,
}
impl ConstantCached {
    fn new(constant_id: ConstantLongId, ctx: &mut DefCacheSavingContext<'_>) -> Self {
        Self { language_element: LanguageElementCached::new(constant_id.intern(ctx.db), ctx) }
    }
    fn embed(self, ctx: &mut DefCacheLoadingContext<'_>) -> ConstantLongId {
        let (module_file_id, stable_ptr) = self.language_element.embed(ctx);

        ConstantLongId(module_file_id, ItemConstantPtr(stable_ptr))
    }
}

#[derive(Serialize, Deserialize, Copy, Clone, PartialEq, Eq, Hash)]
struct ConstantIdCached(usize);

impl ConstantIdCached {
    fn new(constant_id: ConstantId, ctx: &mut DefCacheSavingContext<'_>) -> Self {
        if let Some(id) = ctx.constant_ids.get(&constant_id) {
            return *id;
        }
        let constant = ConstantCached::new(constant_id.lookup_intern(ctx.db), ctx);
        let id = ConstantIdCached(ctx.constant_ids_lookup.len());
        ctx.constant_ids_lookup.push(constant);
        ctx.constant_ids.insert(constant_id, id);
        id
    }
    fn embed(self, ctx: &mut DefCacheLoadingContext<'_>) -> ConstantId {
        if let Some(constant_id) = ctx.constant_ids.get(&self) {
            return *constant_id;
        }
        let constant = ctx.constant_ids_lookup[self.0].clone();
        let constant = constant.embed(ctx).intern(ctx.db);
        ctx.constant_ids.insert(self, constant);
        constant
    }
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq)]
struct SubmoduleCached {
    language_element: LanguageElementCached,
}
impl SubmoduleCached {
    fn new(submodule_id: SubmoduleLongId, ctx: &mut DefCacheSavingContext<'_>) -> Self {
        Self { language_element: LanguageElementCached::new(submodule_id.intern(ctx.db), ctx) }
    }
    fn embed(self, ctx: &mut DefCacheLoadingContext<'_>) -> SubmoduleLongId {
        let (module_file_id, stable_ptr) = self.language_element.embed(ctx);

        SubmoduleLongId(module_file_id, ItemModulePtr(stable_ptr))
    }
}

#[derive(Serialize, Deserialize, Copy, Clone, PartialEq, Eq, Hash)]
pub struct SubmoduleIdCached(usize);

impl SubmoduleIdCached {
    fn new(submodule_id: SubmoduleId, ctx: &mut DefCacheSavingContext<'_>) -> Self {
        if let Some(id) = ctx.submodule_ids.get(&submodule_id) {
            return *id;
        }
        let submodule = SubmoduleCached::new(submodule_id.lookup_intern(ctx.db), ctx);
        let id = SubmoduleIdCached(ctx.submodule_ids_lookup.len());
        ctx.submodule_ids_lookup.push(submodule);
        ctx.submodule_ids.insert(submodule_id, id);
        id
    }
    fn embed(self, ctx: &mut DefCacheLoadingContext<'_>) -> SubmoduleId {
        if let Some(submodule_id) = ctx.submodule_ids.get(&self) {
            return *submodule_id;
        }
        let submodule = ctx.submodule_ids_lookup[self.0].clone();
        let submodule = submodule.embed(ctx).intern(ctx.db);
        ctx.submodule_ids.insert(self, submodule);
        submodule
    }
    fn get_embedded(self, data: &Arc<DefCacheLoadingData>) -> SubmoduleId {
        data.submodule_ids[&self]
    }
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq)]
struct UseCached {
    language_element: LanguageElementCached,
}
impl UseCached {
    fn new(use_id: UseLongId, ctx: &mut DefCacheSavingContext<'_>) -> Self {
        Self { language_element: LanguageElementCached::new(use_id.intern(ctx.db), ctx) }
    }
    fn embed(self, ctx: &mut DefCacheLoadingContext<'_>) -> UseLongId {
        let (module_file_id, stable_ptr) = self.language_element.embed(ctx);

        UseLongId(module_file_id, UsePathLeafPtr(stable_ptr))
    }
}

#[derive(Serialize, Deserialize, Copy, Clone, PartialEq, Eq, Hash)]
struct UseIdCached(usize);

impl UseIdCached {
    fn new(use_id: UseId, ctx: &mut DefCacheSavingContext<'_>) -> Self {
        if let Some(id) = ctx.use_ids.get(&use_id) {
            return *id;
        }
        let use_cache = UseCached::new(use_id.lookup_intern(ctx.db), ctx);
        let id = UseIdCached(ctx.use_ids_lookup.len());
        ctx.use_ids_lookup.push(use_cache);
        ctx.use_ids.insert(use_id, id);
        id
    }
    fn embed(self, ctx: &mut DefCacheLoadingContext<'_>) -> UseId {
        if let Some(use_id) = ctx.use_ids.get(&self) {
            return *use_id;
        }
        let use_cached = ctx.use_ids_lookup[self.0].clone();
        let use_cached = use_cached.embed(ctx).intern(ctx.db);
        ctx.use_ids.insert(self, use_cached);
        use_cached
    }
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq)]
struct FreeFunctionCached {
    language_element: LanguageElementCached,
}
impl FreeFunctionCached {
    fn new(free_function_id: FreeFunctionLongId, ctx: &mut DefCacheSavingContext<'_>) -> Self {
        Self { language_element: LanguageElementCached::new(free_function_id.intern(ctx.db), ctx) }
    }
    fn embed(self, ctx: &mut DefCacheLoadingContext<'_>) -> FreeFunctionLongId {
        let (module_file_id, stable_ptr) = self.language_element.embed(ctx);

        FreeFunctionLongId(module_file_id, FunctionWithBodyPtr(stable_ptr))
    }
}

#[derive(Serialize, Deserialize, Copy, Clone, PartialEq, Eq, Hash)]
struct FreeFunctionIdCached(usize);

impl FreeFunctionIdCached {
    fn new(free_function_id: FreeFunctionId, ctx: &mut DefCacheSavingContext<'_>) -> Self {
        if let Some(id) = ctx.free_function_ids.get(&free_function_id) {
            return *id;
        }
        let free_function = FreeFunctionCached::new(free_function_id.lookup_intern(ctx.db), ctx);
        let id = FreeFunctionIdCached(ctx.free_function_ids_lookup.len());
        ctx.free_function_ids_lookup.push(free_function);
        ctx.free_function_ids.insert(free_function_id, id);
        id
    }
    fn embed(self, ctx: &mut DefCacheLoadingContext<'_>) -> FreeFunctionId {
        if let Some(free_function_id) = ctx.free_function_ids.get(&self) {
            return *free_function_id;
        }
        let free_function = ctx.free_function_ids_lookup[self.0].clone();
        let free_function = free_function.embed(ctx).intern(ctx.db);
        ctx.free_function_ids.insert(self, free_function);
        free_function
    }
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq)]
struct StructCached {
    language_element: LanguageElementCached,
}
impl StructCached {
    fn new(struct_id: StructLongId, ctx: &mut DefCacheSavingContext<'_>) -> Self {
        Self { language_element: LanguageElementCached::new(struct_id.intern(ctx.db), ctx) }
    }
    fn embed(self, ctx: &mut DefCacheLoadingContext<'_>) -> StructLongId {
        let (module_file_id, stable_ptr) = self.language_element.embed(ctx);

        StructLongId(module_file_id, ItemStructPtr(stable_ptr))
    }
}

#[derive(Serialize, Deserialize, Copy, Clone, PartialEq, Eq, Hash)]
struct StructIdCached(usize);

impl StructIdCached {
    fn new(struct_id: StructId, ctx: &mut DefCacheSavingContext<'_>) -> Self {
        if let Some(id) = ctx.struct_ids.get(&struct_id) {
            return *id;
        }
        let struct_cached = StructCached::new(struct_id.lookup_intern(ctx.db), ctx);
        let id = StructIdCached(ctx.struct_ids_lookup.len());
        ctx.struct_ids_lookup.push(struct_cached);
        ctx.struct_ids.insert(struct_id, id);
        id
    }
    fn embed(self, ctx: &mut DefCacheLoadingContext<'_>) -> StructId {
        if let Some(struct_id) = ctx.struct_ids.get(&self) {
            return *struct_id;
        }
        let struct_cached = ctx.struct_ids_lookup[self.0].clone();
        let struct_cached = struct_cached.embed(ctx).intern(ctx.db);
        ctx.struct_ids.insert(self, struct_cached);
        struct_cached
    }
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq)]
struct EnumCached {
    language_element: LanguageElementCached,
}
impl EnumCached {
    fn new(enum_id: EnumLongId, ctx: &mut DefCacheSavingContext<'_>) -> Self {
        Self { language_element: LanguageElementCached::new(enum_id.intern(ctx.db), ctx) }
    }
    fn embed(self, ctx: &mut DefCacheLoadingContext<'_>) -> EnumLongId {
        let (module_file_id, stable_ptr) = self.language_element.embed(ctx);

        EnumLongId(module_file_id, ItemEnumPtr(stable_ptr))
    }
}

#[derive(Serialize, Deserialize, Copy, Clone, PartialEq, Eq, Hash)]
struct EnumIdCached(usize);

impl EnumIdCached {
    fn new(enum_id: EnumId, ctx: &mut DefCacheSavingContext<'_>) -> Self {
        if let Some(id) = ctx.enum_ids.get(&enum_id) {
            return *id;
        }
        let enum_cached = EnumCached::new(enum_id.lookup_intern(ctx.db), ctx);
        let id = EnumIdCached(ctx.enum_ids_lookup.len());
        ctx.enum_ids_lookup.push(enum_cached);
        ctx.enum_ids.insert(enum_id, id);
        id
    }
    fn embed(self, ctx: &mut DefCacheLoadingContext<'_>) -> EnumId {
        if let Some(enum_id) = ctx.enum_ids.get(&self) {
            return *enum_id;
        }
        let enum_cached = ctx.enum_ids_lookup[self.0].clone();
        let enum_cached = enum_cached.embed(ctx).intern(ctx.db);
        ctx.enum_ids.insert(self, enum_cached);
        enum_cached
    }
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq)]
struct ModuleTypeAliasCached {
    language_element: LanguageElementCached,
}
impl ModuleTypeAliasCached {
    fn new(type_alias_id: ModuleTypeAliasLongId, ctx: &mut DefCacheSavingContext<'_>) -> Self {
        Self { language_element: LanguageElementCached::new(type_alias_id.intern(ctx.db), ctx) }
    }
    fn embed(self, ctx: &mut DefCacheLoadingContext<'_>) -> ModuleTypeAliasLongId {
        let (module_file_id, stable_ptr) = self.language_element.embed(ctx);

        ModuleTypeAliasLongId(module_file_id, ItemTypeAliasPtr(stable_ptr))
    }
}

#[derive(Serialize, Deserialize, Copy, Clone, PartialEq, Eq, Hash)]
struct ModuleTypeAliasIdCached(usize);

impl ModuleTypeAliasIdCached {
    fn new(type_alias_id: ModuleTypeAliasId, ctx: &mut DefCacheSavingContext<'_>) -> Self {
        if let Some(id) = ctx.type_alias_ids.get(&type_alias_id) {
            return *id;
        }
        let type_alias = ModuleTypeAliasCached::new(type_alias_id.lookup_intern(ctx.db), ctx);
        let id = ModuleTypeAliasIdCached(ctx.type_alias_ids_lookup.len());
        ctx.type_alias_ids_lookup.push(type_alias);
        ctx.type_alias_ids.insert(type_alias_id, id);
        id
    }
    fn embed(self, ctx: &mut DefCacheLoadingContext<'_>) -> ModuleTypeAliasId {
        if let Some(type_alias_id) = ctx.type_alias_ids.get(&self) {
            return *type_alias_id;
        }
        let type_alias = ctx.type_alias_ids_lookup[self.0].clone();
        let type_alias = type_alias.embed(ctx).intern(ctx.db);
        ctx.type_alias_ids.insert(self, type_alias);
        type_alias
    }
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq)]
struct ImplAliasCached {
    language_element: LanguageElementCached,
}
impl ImplAliasCached {
    fn new(impl_alias_id: ImplAliasLongId, ctx: &mut DefCacheSavingContext<'_>) -> Self {
        Self { language_element: LanguageElementCached::new(impl_alias_id.intern(ctx.db), ctx) }
    }
    fn embed(self, ctx: &mut DefCacheLoadingContext<'_>) -> ImplAliasLongId {
        let (module_file_id, stable_ptr) = self.language_element.embed(ctx);

        ImplAliasLongId(module_file_id, ItemImplAliasPtr(stable_ptr))
    }
}

#[derive(Serialize, Deserialize, Copy, Clone, PartialEq, Eq, Hash)]
struct ImplAliasIdCached(usize);

impl ImplAliasIdCached {
    fn new(impl_alias_id: ImplAliasId, ctx: &mut DefCacheSavingContext<'_>) -> Self {
        if let Some(id) = ctx.impl_alias_ids.get(&impl_alias_id) {
            return *id;
        }
        let impl_alias = ImplAliasCached::new(impl_alias_id.lookup_intern(ctx.db), ctx);
        let id = ImplAliasIdCached(ctx.impl_alias_ids_lookup.len());
        ctx.impl_alias_ids_lookup.push(impl_alias);
        ctx.impl_alias_ids.insert(impl_alias_id, id);
        id
    }
    fn embed(self, ctx: &mut DefCacheLoadingContext<'_>) -> ImplAliasId {
        if let Some(impl_alias_id) = ctx.impl_alias_ids.get(&self) {
            return *impl_alias_id;
        }
        let impl_alias = ctx.impl_alias_ids_lookup[self.0].clone();
        let impl_alias = impl_alias.embed(ctx).intern(ctx.db);
        ctx.impl_alias_ids.insert(self, impl_alias);
        impl_alias
    }
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq)]
struct TraitCached {
    language_element: LanguageElementCached,
}
impl TraitCached {
    fn new(trait_id: TraitLongId, ctx: &mut DefCacheSavingContext<'_>) -> Self {
        Self { language_element: LanguageElementCached::new(trait_id.intern(ctx.db), ctx) }
    }
    fn embed(self, ctx: &mut DefCacheLoadingContext<'_>) -> TraitLongId {
        let (module_file_id, stable_ptr) = self.language_element.embed(ctx);

        TraitLongId(module_file_id, ItemTraitPtr(stable_ptr))
    }
}

#[derive(Serialize, Deserialize, Copy, Clone, PartialEq, Eq, Hash)]
struct TraitIdCached(usize);

impl TraitIdCached {
    fn new(trait_id: TraitId, ctx: &mut DefCacheSavingContext<'_>) -> Self {
        if let Some(id) = ctx.trait_ids.get(&trait_id) {
            return *id;
        }
        let trait_cached = TraitCached::new(trait_id.lookup_intern(ctx.db), ctx);
        let id = TraitIdCached(ctx.trait_ids_lookup.len());
        ctx.trait_ids_lookup.push(trait_cached);
        ctx.trait_ids.insert(trait_id, id);
        id
    }
    fn embed(self, ctx: &mut DefCacheLoadingContext<'_>) -> TraitId {
        if let Some(trait_id) = ctx.trait_ids.get(&self) {
            return *trait_id;
        }
        let trait_cached = ctx.trait_ids_lookup[self.0].clone();
        let trait_cached = trait_cached.embed(ctx).intern(ctx.db);
        ctx.trait_ids.insert(self, trait_cached);
        trait_cached
    }
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq)]
struct ImplDefCached {
    language_element: LanguageElementCached,
}
impl ImplDefCached {
    fn new(impl_def_id: ImplDefLongId, ctx: &mut DefCacheSavingContext<'_>) -> Self {
        Self { language_element: LanguageElementCached::new(impl_def_id.intern(ctx.db), ctx) }
    }
    fn embed(self, ctx: &mut DefCacheLoadingContext<'_>) -> ImplDefLongId {
        let (module_file_id, stable_ptr) = self.language_element.embed(ctx);

        ImplDefLongId(module_file_id, ItemImplPtr(stable_ptr))
    }
}

#[derive(Serialize, Deserialize, Copy, Clone, PartialEq, Eq, Hash)]
pub struct ImplDefIdCached(usize);

impl ImplDefIdCached {
    pub fn new(impl_def_id: ImplDefId, ctx: &mut DefCacheSavingContext<'_>) -> Self {
        if let Some(id) = ctx.impl_def_ids.get(&impl_def_id) {
            return *id;
        }
        let impl_def = ImplDefCached::new(impl_def_id.lookup_intern(ctx.db), ctx);
        let id = ImplDefIdCached(ctx.impl_def_ids_lookup.len());
        ctx.impl_def_ids_lookup.push(impl_def);
        ctx.impl_def_ids.insert(impl_def_id, id);
        id
    }

    fn embed(self, ctx: &mut DefCacheLoadingContext<'_>) -> ImplDefId {
        if let Some(impl_def_id) = ctx.impl_def_ids.get(&self) {
            return *impl_def_id;
        }
        let impl_def = ctx.impl_def_ids_lookup[self.0].clone();
        let impl_def = impl_def.embed(ctx).intern(ctx.db);
        ctx.impl_def_ids.insert(self, impl_def);
        impl_def
    }

    pub fn get_embedded(self, data: &Arc<DefCacheLoadingData>) -> ImplDefId {
        data.impl_def_ids[&self]
    }
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq)]
struct ExternTypeCached {
    language_element: LanguageElementCached,
}
impl ExternTypeCached {
    fn new(extern_type_id: ExternTypeLongId, ctx: &mut DefCacheSavingContext<'_>) -> Self {
        Self { language_element: LanguageElementCached::new(extern_type_id.intern(ctx.db), ctx) }
    }
    fn embed(self, ctx: &mut DefCacheLoadingContext<'_>) -> ExternTypeLongId {
        let (module_file_id, stable_ptr) = self.language_element.embed(ctx);

        ExternTypeLongId(module_file_id, ItemExternTypePtr(stable_ptr))
    }
}

#[derive(Serialize, Deserialize, Copy, Clone, PartialEq, Eq, Hash)]
struct ExternTypeIdCached(usize);

impl ExternTypeIdCached {
    fn new(extern_type_id: ExternTypeId, ctx: &mut DefCacheSavingContext<'_>) -> Self {
        if let Some(id) = ctx.extern_type_ids.get(&extern_type_id) {
            return *id;
        }
        let extern_type = ExternTypeCached::new(extern_type_id.lookup_intern(ctx.db), ctx);
        let id = ExternTypeIdCached(ctx.extern_type_ids_lookup.len());
        ctx.extern_type_ids_lookup.push(extern_type);
        ctx.extern_type_ids.insert(extern_type_id, id);
        id
    }
    fn embed(self, ctx: &mut DefCacheLoadingContext<'_>) -> ExternTypeId {
        if let Some(extern_type_id) = ctx.extern_type_ids.get(&self) {
            return *extern_type_id;
        }
        let extern_type = ctx.extern_type_ids_lookup[self.0].clone();
        let extern_type = extern_type.embed(ctx).intern(ctx.db);
        ctx.extern_type_ids.insert(self, extern_type);
        extern_type
    }
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq)]
struct ExternFunctionCached {
    language_element: LanguageElementCached,
}
impl ExternFunctionCached {
    fn new(extern_function_id: ExternFunctionLongId, ctx: &mut DefCacheSavingContext<'_>) -> Self {
        Self {
            language_element: LanguageElementCached::new(extern_function_id.intern(ctx.db), ctx),
        }
    }
    fn embed(self, ctx: &mut DefCacheLoadingContext<'_>) -> ExternFunctionLongId {
        let (module_file_id, stable_ptr) = self.language_element.embed(ctx);

        ExternFunctionLongId(module_file_id, ItemExternFunctionPtr(stable_ptr))
    }
}

#[derive(Serialize, Deserialize, Copy, Clone, PartialEq, Eq, Hash)]
struct ExternFunctionIdCached(usize);

impl ExternFunctionIdCached {
    fn new(extern_function_id: ExternFunctionId, ctx: &mut DefCacheSavingContext<'_>) -> Self {
        if let Some(id) = ctx.extern_function_ids.get(&extern_function_id) {
            return *id;
        }
        let extern_function =
            ExternFunctionCached::new(extern_function_id.lookup_intern(ctx.db), ctx);
        let id = ExternFunctionIdCached(ctx.extern_function_ids_lookup.len());
        ctx.extern_function_ids_lookup.push(extern_function);
        ctx.extern_function_ids.insert(extern_function_id, id);
        id
    }
    fn embed(self, ctx: &mut DefCacheLoadingContext<'_>) -> ExternFunctionId {
        if let Some(extern_function_id) = ctx.extern_function_ids.get(&self) {
            return *extern_function_id;
        }
        let extern_function = ctx.extern_function_ids_lookup[self.0].clone();
        let extern_function = extern_function.embed(ctx).intern(ctx.db);
        ctx.extern_function_ids.insert(self, extern_function);
        extern_function
    }
}
#[derive(Serialize, Deserialize, Clone, PartialEq, Eq)]
struct MacroDeclarationCached {
    language_element: LanguageElementCached,
}

impl MacroDeclarationCached {
    fn new(
        macro_declaration_id: MacroDeclarationLongId,
        ctx: &mut DefCacheSavingContext<'_>,
    ) -> Self {
        Self {
            language_element: LanguageElementCached::new(macro_declaration_id.intern(ctx.db), ctx),
        }
    }
    fn embed(self, ctx: &mut DefCacheLoadingContext<'_>) -> MacroDeclarationLongId {
        let (module_file_id, stable_ptr) = self.language_element.embed(ctx);

        MacroDeclarationLongId(module_file_id, ItemMacroDeclarationPtr(stable_ptr))
    }
}

#[derive(Serialize, Deserialize, Copy, Clone, PartialEq, Eq, Hash)]
struct MacroDeclarationIdCached(usize);

impl MacroDeclarationIdCached {
    fn new(macro_declaration_id: MacroDeclarationId, ctx: &mut DefCacheSavingContext<'_>) -> Self {
        if let Some(id) = ctx.macro_declaration_ids.get(&macro_declaration_id) {
            return *id;
        }
        let macro_declaration =
            MacroDeclarationCached::new(macro_declaration_id.lookup_intern(ctx.db), ctx);
        let id = MacroDeclarationIdCached(ctx.macro_declaration_ids_lookup.len());
        ctx.macro_declaration_ids_lookup.push(macro_declaration);
        ctx.macro_declaration_ids.insert(macro_declaration_id, id);
        id
    }
    fn embed(self, ctx: &mut DefCacheLoadingContext<'_>) -> MacroDeclarationId {
        if let Some(macro_declaration_id) = ctx.macro_declaration_ids.get(&self) {
            return *macro_declaration_id;
        }
        let macro_declaration = ctx.macro_declaration_ids_lookup[self.0].clone();
        let macro_declaration = macro_declaration.embed(ctx).intern(ctx.db);
        ctx.macro_declaration_ids.insert(self, macro_declaration);
        macro_declaration
    }
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq)]
struct GlobalUseCached {
    language_element: LanguageElementCached,
}
impl GlobalUseCached {
    fn new(global_use_id: GlobalUseLongId, ctx: &mut DefCacheSavingContext<'_>) -> Self {
        Self { language_element: LanguageElementCached::new(global_use_id.intern(ctx.db), ctx) }
    }
    fn embed(self, ctx: &mut DefCacheLoadingContext<'_>) -> GlobalUseLongId {
        let (module_file_id, stable_ptr) = self.language_element.embed(ctx);

        GlobalUseLongId(module_file_id, UsePathStarPtr(stable_ptr))
    }
}

#[derive(Serialize, Deserialize, Copy, Clone, PartialEq, Eq, Hash)]
struct GlobalUseIdCached(usize);

impl GlobalUseIdCached {
    fn new(global_use_id: GlobalUseId, ctx: &mut DefCacheSavingContext<'_>) -> Self {
        if let Some(id) = ctx.global_use_ids.get(&global_use_id) {
            return *id;
        }
        let global_use = GlobalUseCached::new(global_use_id.lookup_intern(ctx.db), ctx);
        let id = GlobalUseIdCached(ctx.global_use_ids_lookup.len());
        ctx.global_use_ids_lookup.push(global_use);
        ctx.global_use_ids.insert(global_use_id, id);
        id
    }
    fn embed(self, ctx: &mut DefCacheLoadingContext<'_>) -> GlobalUseId {
        if let Some(global_use_id) = ctx.global_use_ids.get(&self) {
            return *global_use_id;
        }
        let global_use = ctx.global_use_ids_lookup[self.0].clone();
        let global_use = global_use.embed(ctx).intern(ctx.db);
        ctx.global_use_ids.insert(self, global_use);
        global_use
    }
}

#[derive(Serialize, Deserialize, Clone, Hash, Eq, PartialEq)]
struct SyntaxNodeCached(Box<SyntaxNodeInnerCached>);

#[derive(Serialize, Deserialize, Clone, Hash, Eq, PartialEq)]
struct SyntaxNodeInnerCached {
    green: GreenIdCached,
    offset: TextOffset,
    parent: Option<SyntaxNodeCached>,
    stable_ptr: SyntaxStablePtrIdCached,
}

impl SyntaxNodeCached {
    fn new(syntax_node: SyntaxNode, ctx: &mut DefCacheSavingContext<'_>) -> Self {
        let db = ctx.db;
        let green = GreenIdCached::new(syntax_node.green_node(db).intern(db), ctx);
        let parent = syntax_node.parent(db).map(|it| Self::new(it, ctx));
        let stable_ptr = SyntaxStablePtrIdCached::new(syntax_node.stable_ptr(db), ctx);
        let offset = syntax_node.offset(db);
        let inner = SyntaxNodeInnerCached { green, offset, parent, stable_ptr };
        SyntaxNodeCached(Box::new(inner))
    }
    fn embed(&self, ctx: &mut DefCacheLoadingContext<'_>) -> SyntaxNode {
        let inner = self.0.as_ref();
        let green = inner.green.embed(ctx);
        let parent = inner.parent.as_ref().map(|it| it.embed(ctx));
        let stable_ptr = inner.stable_ptr.embed(ctx);
        let offset = inner.offset;
        SyntaxNode::new_with_inner(ctx.db, green, offset, parent, stable_ptr)
    }
}

#[derive(Serialize, Deserialize, Clone, Hash, Eq, PartialEq)]
pub struct LanguageElementCached {
    module_file_id: ModuleFileCached,
    stable_ptr: SyntaxStablePtrIdCached,
}
impl LanguageElementCached {
    pub fn new<T: LanguageElementId>(
        language_element: T,
        ctx: &mut DefCacheSavingContext<'_>,
    ) -> Self {
        Self {
            module_file_id: ModuleFileCached::new(language_element.module_file_id(ctx.db), ctx),
            stable_ptr: SyntaxStablePtrIdCached::new(
                language_element.untyped_stable_ptr(ctx.db),
                ctx,
            ),
        }
    }
    fn embed(self, ctx: &mut DefCacheLoadingContext<'_>) -> (ModuleFileId, SyntaxStablePtrId) {
        let module_file_id = self.module_file_id.embed(ctx);
        let stable_ptr = self.stable_ptr.embed(ctx);
        (module_file_id, stable_ptr)
    }
    pub fn get_embedded(
        self,
        data: &Arc<DefCacheLoadingData>,
    ) -> (ModuleFileId, SyntaxStablePtrId) {
        let module_file_id = self.module_file_id.get_embedded(data);
        let stable_ptr = self.stable_ptr.get_embedded(data);
        (module_file_id, stable_ptr)
    }
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq)]
enum SyntaxStablePtrCached {
    /// The root node of the tree.
    Root(FileIdCached, GreenIdCached),
    /// A child node.
    Child {
        /// The parent of the node.
        parent: SyntaxStablePtrIdCached,
        /// The SyntaxKind of the node.
        kind: SyntaxKind,
        /// A list of field values for this node, to index by.
        /// Which fields are used is determined by each SyntaxKind.
        /// For example, a function item might use the name of the function.
        key_fields: Vec<GreenIdCached>,
        /// Chronological index among all nodes with the same (parent, kind, key_fields).
        index: usize,
    },
}

impl SyntaxStablePtrCached {
    fn new(syntax_stable_ptr: SyntaxStablePtr, ctx: &mut DefCacheSavingContext<'_>) -> Self {
        match syntax_stable_ptr {
            SyntaxStablePtr::Root(root, green_id) => SyntaxStablePtrCached::Root(
                FileIdCached::new(root, ctx),
                GreenIdCached::new(green_id, ctx),
            ),
            SyntaxStablePtr::Child { parent, kind, key_fields, index } => {
                SyntaxStablePtrCached::Child {
                    parent: SyntaxStablePtrIdCached::new(parent, ctx),
                    kind,
                    key_fields: key_fields
                        .iter()
                        .map(|field| GreenIdCached::new(*field, ctx))
                        .collect(),
                    index,
                }
            }
        }
    }
    fn embed(self, ctx: &mut DefCacheLoadingContext<'_>) -> SyntaxStablePtr {
        match self {
            SyntaxStablePtrCached::Root(file, green_id) => {
                SyntaxStablePtr::Root(file.embed(ctx), green_id.embed(ctx))
            }
            SyntaxStablePtrCached::Child { parent, kind, key_fields, index } => {
                SyntaxStablePtr::Child {
                    parent: parent.embed(ctx),
                    kind,
                    key_fields: key_fields.into_iter().map(|field| field.embed(ctx)).collect(),
                    index,
                }
            }
        }
    }
}

#[derive(Serialize, Deserialize, Copy, Clone, PartialEq, Eq, Hash)]
pub struct SyntaxStablePtrIdCached(usize);
impl SyntaxStablePtrIdCached {
    pub fn new(
        syntax_stable_ptr_id: SyntaxStablePtrId,
        ctx: &mut DefCacheSavingContext<'_>,
    ) -> Self {
        if let Some(id) = ctx.syntax_stable_ptr_ids.get(&syntax_stable_ptr_id) {
            return *id;
        }
        let stable_ptr =
            SyntaxStablePtrCached::new(syntax_stable_ptr_id.lookup_intern(ctx.db), ctx);
        let id = SyntaxStablePtrIdCached(ctx.syntax_stable_ptr_ids_lookup.len());
        ctx.syntax_stable_ptr_ids_lookup.push(stable_ptr);
        ctx.syntax_stable_ptr_ids.insert(syntax_stable_ptr_id, id);
        id
    }
    fn embed(self, ctx: &mut DefCacheLoadingContext<'_>) -> SyntaxStablePtrId {
        if let Some(syntax_stable_ptr_id) = ctx.syntax_stable_ptr_ids.get(&self) {
            return *syntax_stable_ptr_id;
        }
        let stable_ptr = ctx.syntax_stable_ptr_ids_lookup[self.0].clone();
        let stable_ptr = stable_ptr.embed(ctx);
        let stable_ptr_id = stable_ptr.intern(ctx.db);
        ctx.syntax_stable_ptr_ids.insert(self, stable_ptr_id);
        stable_ptr_id
    }
    pub fn get_embedded(self, data: &Arc<DefCacheLoadingData>) -> SyntaxStablePtrId {
        data.syntax_stable_ptr_ids[&self]
    }
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq)]
enum GreenNodeDetailsCached {
    Token(SmolStr),
    Node { children: Vec<GreenIdCached>, width: TextWidth },
}

impl GreenNodeDetailsCached {
    fn new(
        green_node_details: &GreenNodeDetails,
        ctx: &mut DefCacheSavingContext<'_>,
    ) -> GreenNodeDetailsCached {
        match green_node_details {
            GreenNodeDetails::Token(token) => GreenNodeDetailsCached::Token(token.clone()),
            GreenNodeDetails::Node { children, width } => GreenNodeDetailsCached::Node {
                children: children.iter().map(|child| GreenIdCached::new(*child, ctx)).collect(),
                width: *width,
            },
        }
    }
    fn embed(&self, ctx: &mut DefCacheLoadingContext<'_>) -> GreenNodeDetails {
        match self {
            GreenNodeDetailsCached::Token(token) => GreenNodeDetails::Token(token.clone()),
            GreenNodeDetailsCached::Node { children, width } => GreenNodeDetails::Node {
                children: children.iter().map(|child| child.embed(ctx)).collect(),
                width: *width,
            },
        }
    }
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq)]
struct GreenNodeCached {
    kind: SyntaxKind,
    details: GreenNodeDetailsCached,
}
impl GreenNodeCached {
    fn new(green_node: &GreenNode, ctx: &mut DefCacheSavingContext<'_>) -> Self {
        Self {
            kind: green_node.kind,
            details: GreenNodeDetailsCached::new(&green_node.details, ctx),
        }
    }
    fn embed(&self, ctx: &mut DefCacheLoadingContext<'_>) -> GreenNode {
        GreenNode { kind: self.kind, details: self.details.embed(ctx) }
    }
}

#[derive(Serialize, Deserialize, Clone, Copy, Eq, Hash, PartialEq)]
struct GreenIdCached(usize);

impl GreenIdCached {
    fn new(green_id: GreenId, ctx: &mut DefCacheSavingContext<'_>) -> Self {
        if let Some(id) = ctx.green_ids.get(&green_id) {
            return *id;
        }
        let green_node = GreenNodeCached::new(green_id.lookup_intern(ctx.db).as_ref(), ctx);
        let id = GreenIdCached(ctx.green_ids_lookup.len());
        ctx.green_ids_lookup.push(green_node);
        ctx.green_ids.insert(green_id, id);
        id
    }
    fn embed(self, ctx: &mut DefCacheLoadingContext<'_>) -> GreenId {
        if let Some(green_id) = ctx.green_ids.get(&self) {
            return *green_id;
        }
        let green_node = ctx.green_ids_lookup[self.0].clone();
        let green_node = Arc::new(green_node.embed(ctx));
        let green_id = green_node.intern(ctx.db);
        ctx.green_ids.insert(self, green_id);
        green_id
    }
}
#[derive(Serialize, Deserialize, Clone, PartialEq, Eq)]
enum FileCached {
    OnDisk(PathBuf),
    Virtual(VirtualFileCached),
    External(PluginGeneratedFileCached),
}

impl FileCached {
    fn new(file: &FileLongId, ctx: &mut DefCacheSavingContext<'_>) -> Self {
        match file {
            FileLongId::OnDisk(path) => FileCached::OnDisk(path.clone()),
            FileLongId::Virtual(virtual_file) => {
                FileCached::Virtual(VirtualFileCached::new(virtual_file, ctx))
            }
            FileLongId::External(external_file) => {
                FileCached::External(PluginGeneratedFileCached::new(
                    PluginGeneratedFileId::from_intern_id(*external_file),
                    ctx,
                ))
            }
        }
    }
    fn embed(self, ctx: &mut DefCacheLoadingContext<'_>) -> FileLongId {
        match self {
            FileCached::OnDisk(path) => FileLongId::OnDisk(path),
            FileCached::Virtual(virtual_file) => FileLongId::Virtual(virtual_file.embed(ctx)),
            FileCached::External(external_file) => {
                FileLongId::External(external_file.embed(ctx).as_intern_id())
            }
        }
    }
}

#[derive(Serialize, Deserialize, Clone, Copy, Eq, Hash, PartialEq)]
struct FileIdCached(usize);
impl FileIdCached {
    fn new(file_id: FileId, ctx: &mut DefCacheSavingContext<'_>) -> Self {
        if let Some(id) = ctx.file_ids.get(&file_id) {
            return *id;
        }
        let file = FileCached::new(&file_id.lookup_intern(ctx.db), ctx);
        let id = FileIdCached(ctx.file_ids_lookup.len());
        ctx.file_ids_lookup.push(file);
        ctx.file_ids.insert(file_id, id);
        id
    }
    fn embed(self, ctx: &mut DefCacheLoadingContext<'_>) -> FileId {
        if let Some(file_id) = ctx.file_ids.get(&self) {
            return *file_id;
        }
        let file = ctx.file_ids_lookup[self.0].clone();
        let file = file.embed(ctx);
        let file_id = file.intern(ctx.db);
        ctx.file_ids.insert(self, file_id);
        file_id
    }
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq)]
struct VirtualFileCached {
    parent: Option<FileIdCached>,
    name: SmolStr,
    content: String,
    code_mappings: Vec<CodeMapping>,
    kind: FileKind,
    original_item_removed: bool,
}

impl VirtualFileCached {
    fn new(virtual_file: &VirtualFile, ctx: &mut DefCacheSavingContext<'_>) -> Self {
        Self {
            parent: virtual_file.parent.map(|parent| FileIdCached::new(parent, ctx)),
            name: virtual_file.name.clone(),
            content: String::from(&*(virtual_file.content)),
            code_mappings: virtual_file.code_mappings.to_vec(),
            kind: virtual_file.kind.clone(),
            original_item_removed: virtual_file.original_item_removed,
        }
    }
    fn embed(self, ctx: &mut DefCacheLoadingContext<'_>) -> VirtualFile {
        VirtualFile {
            parent: self.parent.map(|parent| parent.embed(ctx)),
            name: self.name,
            content: self.content.into(),
            code_mappings: self.code_mappings.into(),
            kind: self.kind,
            original_item_removed: self.original_item_removed,
        }
    }
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq)]
struct PluginGeneratedFileCached {
    module_id: ModuleIdCached,
    stable_ptr: SyntaxStablePtrIdCached,
    name: SmolStr,
}

impl PluginGeneratedFileCached {
    fn new(
        plugin_generated_file: PluginGeneratedFileId,
        ctx: &mut DefCacheSavingContext<'_>,
    ) -> Self {
        let long_id = plugin_generated_file.lookup_intern(ctx.db);
        Self {
            module_id: ModuleIdCached::new(long_id.module_id, ctx),
            stable_ptr: SyntaxStablePtrIdCached::new(long_id.stable_ptr, ctx),
            name: long_id.name,
        }
    }
    fn embed(self, ctx: &mut DefCacheLoadingContext<'_>) -> PluginGeneratedFileId {
        let module_id = self.module_id.embed(ctx);
        let stable_ptr = self.stable_ptr.embed(ctx);
        let long_id = PluginGeneratedFileLongId { module_id, stable_ptr, name: self.name };
        long_id.intern(ctx.db)
    }
}

#[derive(Serialize, Deserialize, Clone)]
enum SeverityCached {
    Error,
    Warning,
}

#[derive(Serialize, Deserialize, Clone)]
struct PluginDiagnosticCached {
    stable_ptr: SyntaxStablePtrIdCached,
    message: String,
    severity: SeverityCached,
    inner_span: Option<(TextWidth, TextWidth)>,
}

impl PluginDiagnosticCached {
    fn new(
        diagnostic: &PluginDiagnostic,
        ctx: &mut DefCacheSavingContext<'_>,
    ) -> PluginDiagnosticCached {
        PluginDiagnosticCached {
            stable_ptr: SyntaxStablePtrIdCached::new(diagnostic.stable_ptr, ctx),
            message: diagnostic.message.clone(),
            severity: match diagnostic.severity {
                Severity::Error => SeverityCached::Error,
                Severity::Warning => SeverityCached::Warning,
            },
            inner_span: diagnostic.inner_span,
        }
    }
    fn embed(self, ctx: &mut DefCacheLoadingContext<'_>) -> PluginDiagnostic {
        PluginDiagnostic {
            stable_ptr: self.stable_ptr.embed(ctx),
            message: self.message,
            severity: match self.severity {
                SeverityCached::Error => Severity::Error,
                SeverityCached::Warning => Severity::Warning,
            },
            inner_span: self.inner_span,
        }
    }
}

type PluginFileDiagnosticNotesCached = OrderedHashMap<FileIdCached, DiagnosticNoteCached>;

#[derive(Serialize, Deserialize, Clone)]
struct DiagnosticNoteCached {
    text: String,
    location: Option<DiagnosticLocationCached>,
}

impl DiagnosticNoteCached {
    fn new(note: DiagnosticNote, ctx: &mut DefCacheSavingContext<'_>) -> DiagnosticNoteCached {
        DiagnosticNoteCached {
            text: note.text.clone(),
            location: note.location.map(|location| DiagnosticLocationCached::new(&location, ctx)),
        }
    }
    fn embed(self, ctx: &mut DefCacheLoadingContext<'_>) -> DiagnosticNote {
        DiagnosticNote {
            text: self.text,
            location: self.location.map(|location| location.embed(ctx)),
        }
    }
}

#[derive(Serialize, Deserialize, Clone)]
struct DiagnosticLocationCached {
    file_id: FileIdCached,
    span: TextSpan,
}

impl DiagnosticLocationCached {
    fn new(
        location: &DiagnosticLocation,
        ctx: &mut DefCacheSavingContext<'_>,
    ) -> DiagnosticLocationCached {
        DiagnosticLocationCached {
            file_id: FileIdCached::new(location.file_id, ctx),
            span: location.span,
        }
    }
    fn embed(self, ctx: &mut DefCacheLoadingContext<'_>) -> DiagnosticLocation {
        DiagnosticLocation { file_id: self.file_id.embed(ctx), span: self.span }
    }
}
