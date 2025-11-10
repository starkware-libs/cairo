use std::hash::Hash;
use std::ops::{Deref, DerefMut};
use std::sync::Arc;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::cache::{
    CrateDefCache, DefCacheLoadingData, DefCacheSavingContext, GenericParamCached,
    GlobalUseIdCached, ImplAliasIdCached, ImplDefIdCached, LanguageElementCached, ModuleDataCached,
    ModuleIdCached, ModuleItemIdCached, SyntaxStablePtrIdCached,
};
use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::diagnostic_utils::StableLocation;
use cairo_lang_defs::ids::{
    EnumLongId, ExternFunctionLongId, ExternTypeLongId, FreeFunctionLongId, ImplFunctionLongId,
    LocalVarId, LocalVarLongId, MemberLongId, ParamId, ParamLongId, StatementConstLongId,
    StatementItemId, StatementUseLongId, StructLongId, TraitConstantId, TraitConstantLongId,
    TraitFunctionLongId, TraitImplId, TraitImplLongId, TraitLongId, TraitTypeId, TraitTypeLongId,
    VarId, VariantLongId,
};
use cairo_lang_diagnostics::{Diagnostics, Maybe};
use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_filesystem::ids::CrateId;
use cairo_lang_syntax::node::TypedStablePtr;
use cairo_lang_syntax::node::ast::{
    ExprPtr, FunctionWithBodyPtr, ItemConstantPtr, ItemEnumPtr, ItemExternFunctionPtr,
    ItemExternTypePtr, ItemStructPtr, ItemTraitPtr, MemberPtr, ParamPtr, TerminalIdentifierPtr,
    TraitItemConstantPtr, TraitItemFunctionPtr, TraitItemImplPtr, TraitItemTypePtr, UsePathLeafPtr,
    VariantPtr,
};
use cairo_lang_utils::Intern;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::smol_str::SmolStr;
use itertools::chain;
use num_bigint::BigInt;
use salsa::Database;
use serde::{Deserialize, Serialize};

use crate::db::ModuleSemanticDataCacheAndLoadingData;
use crate::items::constant::{ConstValue, ConstValueId, ImplConstantId};
use crate::items::feature_kind::FeatureKind;
use crate::items::functions::{
    ConcreteFunctionWithBody, GenericFunctionId, GenericFunctionWithBodyId, ImplFunctionBodyId,
    ImplGenericFunctionId, ImplGenericFunctionWithBodyId,
};
use crate::items::generics::{GenericParamConst, GenericParamImpl, GenericParamType};
use crate::items::imp::{
    GeneratedImplId, GeneratedImplItems, GeneratedImplLongId, ImplId, ImplImplId, ImplLongId,
    NegativeImplId, NegativeImplLongId,
};
use crate::items::impl_alias::ImplAliasSemantic;
use crate::items::macro_call::module_macro_modules;
use crate::items::module::{ModuleItemInfo, ModuleSemantic, ModuleSemanticData};
use crate::items::trt::ConcreteTraitGenericFunctionLongId;
use crate::items::visibility::Visibility;
use crate::types::{
    ClosureTypeLongId, ConcreteEnumLongId, ConcreteExternTypeLongId, ConcreteStructLongId,
    ImplTypeId,
};
use crate::{
    ConcreteEnumId, ConcreteExternTypeId, ConcreteFunction, ConcreteFunctionWithBodyId,
    ConcreteImplId, ConcreteImplLongId, ConcreteStructId, ConcreteTraitId, ConcreteTraitLongId,
    ConcreteTypeId, ConcreteVariant, ExprVar, ExprVarMemberPath, FunctionId, FunctionLongId,
    GenericArgumentId, GenericParam, MatchArmSelector, TypeId, TypeLongId, ValueSelectorArm,
};

type SemanticCache<'db> = (CrateSemanticCache, SemanticCacheLookups);

/// Load the cached semantic of a crate if it has a cache file configuration.
pub fn load_cached_crate_modules_semantic<'db>(
    db: &'db dyn Database,
    crate_id: CrateId<'db>,
) -> Option<ModuleSemanticDataCacheAndLoadingData<'db>> {
    let def_loading_data = db.cached_crate_modules(crate_id)?.1;

    let blob_id = db.crate_config(crate_id)?.cache_file?;
    let Some(content) = db.blob_content(blob_id) else {
        return Default::default();
    };

    let def_size = usize::from_be_bytes(content[..8].try_into().unwrap());

    let semantic_start = 8 + def_size;
    let semantic_size =
        usize::from_be_bytes(content[semantic_start..semantic_start + 8].try_into().unwrap());

    let content = &content[semantic_start + 8..semantic_start + 8 + semantic_size];

    let ((module_data, semantic_lookups), _): (SemanticCache<'_>, _) =
        bincode::serde::borrow_decode_from_slice(content, bincode::config::standard())
            .unwrap_or_else(|e| {
                panic!(
                    "failed to deserialize modules cache for crate `{}`: {e}",
                    crate_id.long(db).name().long(db),
                )
            });

    let mut ctx = SemanticCacheLoadingContext::new(db, semantic_lookups, def_loading_data);
    Some(ModuleSemanticDataCacheAndLoadingData {
        modules_semantic_data: module_data
            .modules
            .into_iter()
            .map(|(module_id, module_data)| {
                let module_id = module_id.get_embedded(&ctx.defs_loading_data);

                let module_data = module_data.embed(&mut ctx);
                (module_id, module_data)
            })
            .collect::<OrderedHashMap<_, _>>()
            .into(),
        impl_aliases_resolved_impls: module_data
            .impl_aliases
            .into_iter()
            .map(|(impl_alias_id, impl_id)| {
                let impl_alias_id = impl_alias_id.get_embedded(&ctx.defs_loading_data);
                let impl_id = impl_id.embed(&mut ctx);
                (impl_alias_id, impl_id)
            })
            .collect::<OrderedHashMap<_, _>>()
            .into(),
        loading_data: ctx.data.into(),
    })
}

/// Cache the module_data of each module in the crate and returns the cache and the context.
pub fn generate_crate_def_cache<'db>(
    db: &'db dyn Database,
    crate_id: cairo_lang_filesystem::ids::CrateId<'db>,
    ctx: &mut DefCacheSavingContext<'db>,
) -> Maybe<CrateDefCache<'db>> {
    let modules = db.crate_modules(crate_id);

    let mut modules_data = Vec::new();
    for module_id in modules.iter() {
        for module_id in chain!([module_id], module_macro_modules(db, true, *module_id)) {
            let module_data = module_id.module_data(db)?;
            modules_data.push((
                ModuleIdCached::new(*module_id, ctx),
                ModuleDataCached::new(db, module_data, ctx),
            ));
        }
    }
    Ok(CrateDefCache::new(modules_data))
}

/// Semantic items in the semantic cache.
#[derive(Serialize, Deserialize)]
pub struct CrateSemanticCache {
    /// Semantic data of the modules in the crate.
    modules: Vec<(ModuleIdCached, ModuleSemanticDataCached)>,
    /// Resolved implementations of the impl aliases in the crate.
    impl_aliases: Vec<(ImplAliasIdCached, ImplIdCached)>,
}

/// Generate semantic cache for a crate.
pub fn generate_crate_semantic_cache<'db>(
    crate_id: CrateId<'db>,
    ctx: &mut SemanticCacheSavingContext<'db>,
) -> Maybe<CrateSemanticCache> {
    let modules = ctx.db.crate_modules(crate_id);

    let mut modules_data = Vec::new();
    for module_id in modules.iter() {
        for module_id in chain!([module_id], module_macro_modules(ctx.db, true, *module_id)) {
            let module_data = ctx.db.priv_module_semantic_data(*module_id)?.clone();
            modules_data.push((
                ModuleIdCached::new(*module_id, &mut ctx.defs_ctx),
                ModuleSemanticDataCached::new(module_data, ctx),
            ));
        }
    }

    Ok(CrateSemanticCache {
        modules: modules_data,
        impl_aliases: modules
            .iter()
            .flat_map(|id| match ctx.db.module_impl_aliases_ids(*id) {
                Err(err) => vec![Err(err)],
                Ok(impl_aliases) => impl_aliases
                    .iter()
                    .map(|id| {
                        Ok((
                            ImplAliasIdCached::new(*id, &mut ctx.defs_ctx),
                            ImplIdCached::new(ctx.db.impl_alias_resolved_impl(*id)?, ctx),
                        ))
                    })
                    .collect::<Vec<_>>(),
            })
            .collect::<Maybe<Vec<_>>>()?,
    })
}

/// Context for loading cache into the database.
struct SemanticCacheLoadingContext<'db> {
    db: &'db dyn Database,
    data: SemanticCacheLoadingData<'db>,
}

impl<'db> Deref for SemanticCacheLoadingContext<'db> {
    type Target = SemanticCacheLoadingData<'db>;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}
impl<'db> DerefMut for SemanticCacheLoadingContext<'db> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.data
    }
}

impl<'db> SemanticCacheLoadingContext<'db> {
    pub fn new(
        db: &'db dyn Database,
        lookups: SemanticCacheLookups,
        defs_loading_data: Arc<DefCacheLoadingData<'db>>,
    ) -> Self {
        let mut res = Self { db, data: SemanticCacheLoadingData::new(lookups, defs_loading_data) };
        res.embed_lookups();
        res
    }
    fn embed_lookups(&mut self) {
        for id in 0..self.lookups.function_ids_lookup.len() {
            SemanticFunctionIdCached(id).embed(self);
        }
        for id in 0..self.lookups.type_ids_lookup.len() {
            TypeIdCached(id).embed(self);
        }
        for id in 0..self.lookups.impl_ids_lookup.len() {
            ImplIdCached(id).embed(self);
        }
        for id in 0..self.lookups.negative_impl_ids_lookup.len() {
            NegativeImplIdCached(id).embed(self);
        }
        for id in 0..self.lookups.const_value_ids_lookup.len() {
            ConstValueIdCached(id).embed(self);
        }
    }
}

/// Data for loading cache into the database.
#[derive(PartialEq, Eq, salsa::Update)]
pub struct SemanticCacheLoadingData<'db> {
    pub defs_loading_data: Arc<DefCacheLoadingData<'db>>,

    function_ids: OrderedHashMap<SemanticFunctionIdCached, FunctionId<'db>>,
    type_ids: OrderedHashMap<TypeIdCached, TypeId<'db>>,
    impl_ids: OrderedHashMap<ImplIdCached, ImplId<'db>>,
    negative_impl_ids: OrderedHashMap<NegativeImplIdCached, NegativeImplId<'db>>,
    const_value_ids: OrderedHashMap<ConstValueIdCached, ConstValueId<'db>>,
    lookups: SemanticCacheLookups,
}

impl<'db> SemanticCacheLoadingData<'db> {
    fn new(
        lookups: SemanticCacheLookups,
        defs_loading_data: Arc<DefCacheLoadingData<'db>>,
    ) -> Self {
        Self {
            defs_loading_data,
            function_ids: OrderedHashMap::default(),
            type_ids: OrderedHashMap::default(),
            impl_ids: OrderedHashMap::default(),
            negative_impl_ids: OrderedHashMap::default(),
            const_value_ids: OrderedHashMap::default(),
            lookups,
        }
    }
}

impl<'db> Deref for SemanticCacheLoadingData<'db> {
    type Target = SemanticCacheLookups;

    fn deref(&self) -> &Self::Target {
        &self.lookups
    }
}
impl<'db> DerefMut for SemanticCacheLoadingData<'db> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.lookups
    }
}

/// Context for saving cache from the database.
pub struct SemanticCacheSavingContext<'db> {
    pub db: &'db dyn Database,
    pub data: SemanticCacheSavingData<'db>,
    pub defs_ctx: DefCacheSavingContext<'db>,
}
impl<'db> Deref for SemanticCacheSavingContext<'db> {
    type Target = SemanticCacheSavingData<'db>;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}
impl<'db> DerefMut for SemanticCacheSavingContext<'db> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.data
    }
}

/// Data for saving cache from the database.
#[derive(Default)]
pub struct SemanticCacheSavingData<'db> {
    function_ids: OrderedHashMap<FunctionId<'db>, SemanticFunctionIdCached>,
    type_ids: OrderedHashMap<TypeId<'db>, TypeIdCached>,
    impl_ids: OrderedHashMap<ImplId<'db>, ImplIdCached>,
    negative_impl_ids: OrderedHashMap<NegativeImplId<'db>, NegativeImplIdCached>,
    const_value_ids: OrderedHashMap<ConstValueId<'db>, ConstValueIdCached>,
    pub lookups: SemanticCacheLookups,
}

impl<'db> Deref for SemanticCacheSavingData<'db> {
    type Target = SemanticCacheLookups;

    fn deref(&self) -> &Self::Target {
        &self.lookups
    }
}
impl<'db> DerefMut for SemanticCacheSavingData<'db> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.lookups
    }
}

/// Saved interned items for the cache.
#[derive(Serialize, Deserialize, Default, PartialEq, Eq, salsa::Update)]
pub struct SemanticCacheLookups {
    function_ids_lookup: Vec<SemanticFunctionCached>,
    type_ids_lookup: Vec<TypeCached>,
    impl_ids_lookup: Vec<ImplCached>,
    negative_impl_ids_lookup: Vec<NegativeImplCached>,
    const_value_ids_lookup: Vec<ConstValueCached>,
}

#[derive(Serialize, Deserialize)]
enum VisibilityCached {
    Public,
    PublicInCrate,
    Private,
}
impl VisibilityCached {
    fn new(visibility: Visibility) -> Self {
        match visibility {
            Visibility::Public => VisibilityCached::Public,
            Visibility::PublicInCrate => VisibilityCached::PublicInCrate,
            Visibility::Private => VisibilityCached::Private,
        }
    }
    fn embed(self) -> Visibility {
        match self {
            VisibilityCached::Public => Visibility::Public,
            VisibilityCached::PublicInCrate => Visibility::PublicInCrate,
            VisibilityCached::Private => Visibility::Private,
        }
    }
}

#[derive(Serialize, Deserialize)]
struct ModuleSemanticDataCached {
    items: OrderedHashMap<SmolStr, ModuleItemInfoCached>,
    global_uses: OrderedHashMap<GlobalUseIdCached, VisibilityCached>,
}

impl ModuleSemanticDataCached {
    fn new<'db>(
        module_semantic_data: ModuleSemanticData<'db>,
        ctx: &mut SemanticCacheSavingContext<'db>,
    ) -> Self {
        Self {
            items: module_semantic_data
                .items
                .into_iter()
                .map(|(item_id, item_info)| {
                    (item_id.long(ctx.db).clone(), ModuleItemInfoCached::new(item_info, ctx))
                })
                .collect(),
            global_uses: module_semantic_data
                .global_uses
                .into_iter()
                .map(|(global_use_id, visibility)| {
                    (
                        GlobalUseIdCached::new(global_use_id, &mut ctx.defs_ctx),
                        VisibilityCached::new(visibility),
                    )
                })
                .collect(),
        }
    }
    fn embed<'db>(self, ctx: &mut SemanticCacheLoadingContext<'db>) -> ModuleSemanticData<'db> {
        ModuleSemanticData {
            items: self
                .items
                .into_iter()
                .map(|(item_id, item_info)| (item_id.intern(ctx.db), item_info.embed(ctx)))
                .collect(),
            global_uses: self
                .global_uses
                .into_iter()
                .map(|(global_use_id, visibility)| {
                    (global_use_id.get_embedded(&ctx.defs_loading_data), visibility.embed())
                })
                .collect(),
            diagnostics: Diagnostics::default(),
        }
    }
}

#[derive(Serialize, Deserialize)]
struct ModuleItemInfoCached {
    item_id: ModuleItemIdCached,
    visibility: VisibilityCached,
    feature_kind: FeatureKindCached,
}

impl ModuleItemInfoCached {
    fn new<'db>(
        module_item_info: ModuleItemInfo<'db>,
        ctx: &mut SemanticCacheSavingContext<'db>,
    ) -> Self {
        Self {
            item_id: ModuleItemIdCached::new(module_item_info.item_id, &mut ctx.defs_ctx),
            visibility: VisibilityCached::new(module_item_info.visibility),
            feature_kind: FeatureKindCached::new(module_item_info.feature_kind, ctx),
        }
    }
    fn embed<'db>(self, ctx: &mut SemanticCacheLoadingContext<'db>) -> ModuleItemInfo<'db> {
        ModuleItemInfo {
            item_id: self.item_id.get_embedded(&ctx.defs_loading_data),
            visibility: self.visibility.embed(),
            feature_kind: self.feature_kind.embed(ctx),
        }
    }
}

/// The kind of a feature for an item.
#[derive(Serialize, Deserialize)]
pub enum FeatureKindCached {
    Stable,
    Unstable { feature: SmolStr, note: Option<SmolStr> },
    Deprecated { feature: SmolStr, note: Option<SmolStr> },
    Internal { feature: SmolStr, note: Option<SmolStr> },
}

impl FeatureKindCached {
    fn new<'db>(feature_kind: FeatureKind<'db>, ctx: &mut SemanticCacheSavingContext<'db>) -> Self {
        match feature_kind {
            FeatureKind::Stable => FeatureKindCached::Stable,
            FeatureKind::Unstable { feature, note } => FeatureKindCached::Unstable {
                feature: feature.long(ctx.db).clone(),
                note: note.map(|note| note.long(ctx.db).clone()),
            },
            FeatureKind::Deprecated { feature, note } => FeatureKindCached::Deprecated {
                feature: feature.long(ctx.db).clone(),
                note: note.map(|note| note.long(ctx.db).clone()),
            },
            FeatureKind::Internal { feature, note } => FeatureKindCached::Internal {
                feature: feature.long(ctx.db).clone(),
                note: note.map(|note| note.long(ctx.db).clone()),
            },
        }
    }
    fn embed<'db>(self, ctx: &mut SemanticCacheLoadingContext<'db>) -> FeatureKind<'db> {
        match self {
            FeatureKindCached::Stable => FeatureKind::Stable,
            FeatureKindCached::Unstable { feature, note } => FeatureKind::Unstable {
                feature: feature.intern(ctx.db),
                note: note.map(|note| note.intern(ctx.db)),
            },
            FeatureKindCached::Deprecated { feature, note } => FeatureKind::Deprecated {
                feature: feature.intern(ctx.db),
                note: note.map(|note| note.intern(ctx.db)),
            },
            FeatureKindCached::Internal { feature, note } => FeatureKind::Internal {
                feature: feature.intern(ctx.db),
                note: note.map(|note| note.intern(ctx.db)),
            },
        }
    }
}

#[derive(Serialize, Deserialize)]
pub enum ExprVarMemberPathCached {
    Var(ExprVarCached),
    Member {
        parent: Box<ExprVarMemberPathCached>,
        member_id: LanguageElementCached,
        concrete_struct_id: ConcreteStructCached,
        stable_ptr: SyntaxStablePtrIdCached,
        ty: TypeIdCached,
    },
}
impl ExprVarMemberPathCached {
    pub fn new<'db>(
        expr_var_member_path: ExprVarMemberPath<'db>,
        ctx: &mut SemanticCacheSavingContext<'db>,
    ) -> Self {
        match expr_var_member_path {
            ExprVarMemberPath::Var(var) => {
                ExprVarMemberPathCached::Var(ExprVarCached::new(var, ctx))
            }
            ExprVarMemberPath::Member { parent, member_id, concrete_struct_id, stable_ptr, ty } => {
                ExprVarMemberPathCached::Member {
                    parent: Box::new(ExprVarMemberPathCached::new(*parent, ctx)),
                    member_id: LanguageElementCached::new(member_id, &mut ctx.defs_ctx),
                    concrete_struct_id: ConcreteStructCached::new(concrete_struct_id, ctx),
                    stable_ptr: SyntaxStablePtrIdCached::new(
                        stable_ptr.untyped(),
                        &mut ctx.defs_ctx,
                    ),
                    ty: TypeIdCached::new(ty, ctx),
                }
            }
        }
    }
    pub fn get_embedded<'db>(
        self,
        data: &Arc<SemanticCacheLoadingData<'db>>,
        db: &'db dyn Database,
    ) -> ExprVarMemberPath<'db> {
        match self {
            ExprVarMemberPathCached::Var(var) => ExprVarMemberPath::Var(var.get_embedded(data, db)),
            ExprVarMemberPathCached::Member {
                parent,
                member_id,
                concrete_struct_id,
                stable_ptr,
                ty,
            } => {
                let parent = Box::new(parent.get_embedded(data, db));
                let (module_id, member_stable_ptr) =
                    member_id.get_embedded(&data.defs_loading_data);
                let member_id = MemberLongId(module_id, MemberPtr(member_stable_ptr)).intern(db);
                ExprVarMemberPath::Member {
                    parent,
                    member_id,
                    concrete_struct_id: concrete_struct_id.get_embedded(data, db),
                    stable_ptr: ExprPtr(stable_ptr.get_embedded(&data.defs_loading_data)),
                    ty: ty.get_embedded(data),
                }
            }
        }
    }
}

#[derive(Serialize, Deserialize)]
pub struct ExprVarCached {
    var: SemanticVarIdCached,
    ty: TypeIdCached,
    stable_ptr: SyntaxStablePtrIdCached,
}
impl ExprVarCached {
    fn new<'db>(expr_var: ExprVar<'db>, ctx: &mut SemanticCacheSavingContext<'db>) -> Self {
        Self {
            var: SemanticVarIdCached::new(expr_var.var, ctx),
            ty: TypeIdCached::new(expr_var.ty, ctx),
            stable_ptr: SyntaxStablePtrIdCached::new(
                expr_var.stable_ptr.untyped(),
                &mut ctx.defs_ctx,
            ),
        }
    }
    pub fn get_embedded<'db>(
        self,
        data: &Arc<SemanticCacheLoadingData<'db>>,
        db: &'db dyn Database,
    ) -> ExprVar<'db> {
        ExprVar {
            var: self.var.get_embedded(data, db),
            ty: self.ty.get_embedded(data),
            stable_ptr: ExprPtr(self.stable_ptr.get_embedded(&data.defs_loading_data)),
        }
    }
}

#[derive(Serialize, Deserialize)]
enum SemanticVarIdCached {
    Param(SemanticParamIdCached),
    Local(SemanticLocalVarIdCached),
    Item(SemanticStatementItemIdCached),
}
impl SemanticVarIdCached {
    fn new<'db>(var_id: VarId<'db>, ctx: &mut SemanticCacheSavingContext<'db>) -> Self {
        match var_id {
            VarId::Param(id) => SemanticVarIdCached::Param(SemanticParamIdCached::new(id, ctx)),
            VarId::Local(id) => SemanticVarIdCached::Local(SemanticLocalVarIdCached::new(id, ctx)),
            VarId::Item(id) => {
                SemanticVarIdCached::Item(SemanticStatementItemIdCached::new(id, ctx))
            }
        }
    }
    pub fn get_embedded<'db>(
        self,
        data: &Arc<SemanticCacheLoadingData<'db>>,
        db: &'db dyn Database,
    ) -> VarId<'db> {
        match self {
            SemanticVarIdCached::Param(id) => VarId::Param(id.get_embedded(data, db)),
            SemanticVarIdCached::Local(id) => VarId::Local(id.get_embedded(data, db)),
            SemanticVarIdCached::Item(id) => VarId::Item(id.get_embedded(data, db)),
        }
    }
}

#[derive(Serialize, Deserialize)]
struct SemanticParamIdCached {
    language_element: LanguageElementCached,
}
impl SemanticParamIdCached {
    fn new<'db>(param_id: ParamId<'db>, ctx: &mut SemanticCacheSavingContext<'db>) -> Self {
        Self { language_element: LanguageElementCached::new(param_id, &mut ctx.defs_ctx) }
    }
    pub fn get_embedded<'db>(
        self,
        data: &Arc<SemanticCacheLoadingData<'db>>,
        db: &'db dyn Database,
    ) -> ParamId<'db> {
        let (module_id, stable_ptr) = self.language_element.get_embedded(&data.defs_loading_data);
        ParamLongId(module_id, ParamPtr(stable_ptr)).intern(db)
    }
}

#[derive(Serialize, Deserialize)]
struct SemanticLocalVarIdCached {
    language_element: LanguageElementCached,
}
impl SemanticLocalVarIdCached {
    fn new<'db>(local_var_id: LocalVarId<'db>, ctx: &mut SemanticCacheSavingContext<'db>) -> Self {
        Self { language_element: LanguageElementCached::new(local_var_id, &mut ctx.defs_ctx) }
    }
    pub fn get_embedded<'db>(
        self,
        data: &Arc<SemanticCacheLoadingData<'db>>,
        db: &'db dyn Database,
    ) -> LocalVarId<'db> {
        let (module_id, stable_ptr) = self.language_element.get_embedded(&data.defs_loading_data);
        LocalVarLongId(module_id, TerminalIdentifierPtr(stable_ptr)).intern(db)
    }
}

#[derive(Serialize, Deserialize)]
enum SemanticStatementItemIdCached {
    Constant(LanguageElementCached),
    Use(LanguageElementCached),
}

impl SemanticStatementItemIdCached {
    fn new<'db>(item_id: StatementItemId<'db>, ctx: &mut SemanticCacheSavingContext<'db>) -> Self {
        match item_id {
            StatementItemId::Constant(id) => SemanticStatementItemIdCached::Constant(
                LanguageElementCached::new(id, &mut ctx.defs_ctx),
            ),
            StatementItemId::Use(id) => SemanticStatementItemIdCached::Use(
                LanguageElementCached::new(id, &mut ctx.defs_ctx),
            ),
        }
    }
    pub fn get_embedded<'db>(
        self,
        data: &Arc<SemanticCacheLoadingData<'db>>,
        db: &'db dyn Database,
    ) -> StatementItemId<'db> {
        match self {
            SemanticStatementItemIdCached::Constant(id) => {
                let (module_id, stable_ptr) = id.get_embedded(&data.defs_loading_data);
                StatementItemId::Constant(
                    StatementConstLongId(module_id, ItemConstantPtr(stable_ptr)).intern(db),
                )
            }
            SemanticStatementItemIdCached::Use(id) => {
                let (module_id, stable_ptr) = id.get_embedded(&data.defs_loading_data);
                StatementItemId::Use(
                    StatementUseLongId(module_id, UsePathLeafPtr(stable_ptr)).intern(db),
                )
            }
        }
    }
}

#[derive(Serialize, Deserialize)]
pub enum MatchArmSelectorCached {
    VariantId(ConcreteVariantCached),
    Value(usize),
}

impl MatchArmSelectorCached {
    pub fn new<'db>(
        match_arm_selector: MatchArmSelector<'db>,
        ctx: &mut SemanticCacheSavingContext<'db>,
    ) -> Self {
        match match_arm_selector {
            MatchArmSelector::VariantId(variant_id) => {
                MatchArmSelectorCached::VariantId(ConcreteVariantCached::new(variant_id, ctx))
            }
            MatchArmSelector::Value(value) => MatchArmSelectorCached::Value(value.value),
        }
    }
    pub fn get_embedded<'db>(
        self,
        data: &Arc<SemanticCacheLoadingData<'db>>,
        db: &'db dyn Database,
    ) -> MatchArmSelector<'db> {
        match self {
            MatchArmSelectorCached::VariantId(variant_id) => {
                MatchArmSelector::VariantId(variant_id.get_embedded(data, db))
            }
            MatchArmSelectorCached::Value(value) => {
                MatchArmSelector::Value(ValueSelectorArm { value })
            }
        }
    }
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq)]
enum ConstValueCached {
    Int(BigInt, TypeIdCached),
    Struct(Vec<ConstValueIdCached>, TypeIdCached),
    Enum(ConcreteVariantCached, ConstValueIdCached),
    NonZero(ConstValueIdCached),
    Generic(GenericParamCached),
    ImplConstant(ImplConstantCached),
}
impl ConstValueCached {
    fn new<'db>(const_value: ConstValue<'db>, ctx: &mut SemanticCacheSavingContext<'db>) -> Self {
        match const_value {
            ConstValue::Int(value, ty) => ConstValueCached::Int(value, TypeIdCached::new(ty, ctx)),
            ConstValue::Struct(values, ty) => ConstValueCached::Struct(
                values.into_iter().map(|v| ConstValueIdCached::new(v, ctx)).collect(),
                TypeIdCached::new(ty, ctx),
            ),
            ConstValue::Enum(variant, value) => ConstValueCached::Enum(
                ConcreteVariantCached::new(variant, ctx),
                ConstValueIdCached::new(value, ctx),
            ),
            ConstValue::NonZero(value) => {
                ConstValueCached::NonZero(ConstValueIdCached::new(value, ctx))
            }
            ConstValue::Generic(generic_param) => {
                ConstValueCached::Generic(GenericParamCached::new(generic_param, &mut ctx.defs_ctx))
            }
            ConstValue::ImplConstant(impl_constant_id) => {
                ConstValueCached::ImplConstant(ImplConstantCached::new(impl_constant_id, ctx))
            }
            ConstValue::Var(_, _) | ConstValue::Missing(_) => {
                unreachable!("Const {:#?} is not supported for caching", const_value.debug(ctx.db))
            }
        }
    }
    fn embed<'db>(self, ctx: &mut SemanticCacheLoadingContext<'db>) -> ConstValue<'db> {
        match self {
            ConstValueCached::Int(value, ty) => ConstValue::Int(value, ty.embed(ctx)),
            ConstValueCached::Struct(values, ty) => ConstValue::Struct(
                values.into_iter().map(|v| v.embed(ctx)).collect(),
                ty.embed(ctx),
            ),
            ConstValueCached::Enum(variant, value) => {
                ConstValue::Enum(variant.embed(ctx), value.embed(ctx))
            }
            ConstValueCached::NonZero(value) => ConstValue::NonZero(value.embed(ctx)),
            ConstValueCached::Generic(generic_param) => {
                ConstValue::Generic(generic_param.get_embedded(&ctx.defs_loading_data, ctx.db))
            }
            ConstValueCached::ImplConstant(impl_constant_id) => {
                ConstValue::ImplConstant(impl_constant_id.embed(ctx))
            }
        }
    }
    pub fn get_embedded<'db>(
        self,
        data: &Arc<SemanticCacheLoadingData<'db>>,
        db: &'db dyn Database,
    ) -> ConstValue<'db> {
        match self {
            ConstValueCached::Int(value, ty) => ConstValue::Int(value, ty.get_embedded(data)),
            ConstValueCached::Struct(values, ty) => ConstValue::Struct(
                values.into_iter().map(|v| v.get_embedded(data)).collect(),
                ty.get_embedded(data),
            ),
            ConstValueCached::Enum(variant, value) => {
                ConstValue::Enum(variant.get_embedded(data, db), value.get_embedded(data))
            }
            ConstValueCached::NonZero(value) => ConstValue::NonZero(value.get_embedded(data)),
            ConstValueCached::Generic(generic_param) => {
                ConstValue::Generic(generic_param.get_embedded(&data.defs_loading_data, db))
            }
            ConstValueCached::ImplConstant(impl_constant_id) => {
                ConstValue::ImplConstant(impl_constant_id.get_embedded(data, db))
            }
        }
    }
}

#[derive(Serialize, Deserialize, Copy, Clone, PartialEq, Eq, Hash, salsa::Update)]
pub struct ConstValueIdCached(usize);

impl ConstValueIdCached {
    pub fn new<'db>(
        const_value_id: ConstValueId<'db>,
        ctx: &mut SemanticCacheSavingContext<'db>,
    ) -> Self {
        if let Some(id) = ctx.const_value_ids.get(&const_value_id) {
            return *id;
        }
        let cached = ConstValueCached::new(const_value_id.long(ctx.db).clone(), ctx);
        let id = Self(ctx.const_value_ids_lookup.len());
        ctx.const_value_ids_lookup.push(cached);
        ctx.const_value_ids.insert(const_value_id, id);
        id
    }
    fn embed<'db>(self, ctx: &mut SemanticCacheLoadingContext<'db>) -> ConstValueId<'db> {
        if let Some(const_value_id) = ctx.const_value_ids.get(&self) {
            return *const_value_id;
        }

        let cached = ctx.const_value_ids_lookup[self.0].clone();
        let id = cached.embed(ctx).intern(ctx.db);
        ctx.const_value_ids.insert(self, id);
        id
    }
    pub fn get_embedded<'db>(self, data: &Arc<SemanticCacheLoadingData<'db>>) -> ConstValueId<'db> {
        data.const_value_ids[&self]
    }
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq)]
struct ImplConstantCached {
    impl_id: ImplIdCached,
    trait_constant: TraitConstantCached,
}
impl ImplConstantCached {
    fn new<'db>(
        impl_constant_id: ImplConstantId<'db>,
        ctx: &mut SemanticCacheSavingContext<'db>,
    ) -> Self {
        Self {
            impl_id: ImplIdCached::new(impl_constant_id.impl_id(), ctx),
            trait_constant: TraitConstantCached::new(impl_constant_id.trait_constant_id(), ctx),
        }
    }
    fn embed<'db>(self, ctx: &mut SemanticCacheLoadingContext<'db>) -> ImplConstantId<'db> {
        ImplConstantId::new(self.impl_id.embed(ctx), self.trait_constant.embed(ctx), ctx.db)
    }
    pub fn get_embedded<'db>(
        self,
        data: &Arc<SemanticCacheLoadingData<'db>>,
        db: &'db dyn Database,
    ) -> ImplConstantId<'db> {
        ImplConstantId::new(
            self.impl_id.get_embedded(data),
            self.trait_constant.get_embedded(data, db),
            db,
        )
    }
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq)]
struct TraitConstantCached {
    language_element: LanguageElementCached,
}
impl TraitConstantCached {
    fn new<'db>(
        trait_constant_id: TraitConstantId<'db>,
        ctx: &mut SemanticCacheSavingContext<'db>,
    ) -> Self {
        Self { language_element: LanguageElementCached::new(trait_constant_id, &mut ctx.defs_ctx) }
    }
    fn embed<'db>(self, ctx: &mut SemanticCacheLoadingContext<'db>) -> TraitConstantId<'db> {
        let (module_id, stable_ptr) = self.language_element.get_embedded(&ctx.defs_loading_data);
        TraitConstantLongId(module_id, TraitItemConstantPtr(stable_ptr)).intern(ctx.db)
    }
    pub fn get_embedded<'db>(
        self,
        data: &Arc<SemanticCacheLoadingData<'db>>,
        db: &'db dyn Database,
    ) -> TraitConstantId<'db> {
        let (module_id, stable_ptr) = self.language_element.get_embedded(&data.defs_loading_data);
        TraitConstantLongId(module_id, TraitItemConstantPtr(stable_ptr)).intern(db)
    }
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq)]
struct SemanticFunctionCached {
    generic_function: GenericFunctionCached,

    generic_args: Vec<GenericArgumentCached>,
}
impl SemanticFunctionCached {
    fn new<'db>(
        function_id: FunctionLongId<'db>,
        ctx: &mut SemanticCacheSavingContext<'db>,
    ) -> Self {
        let function = function_id.function;
        Self {
            generic_function: GenericFunctionCached::new(function.generic_function, ctx),
            generic_args: function
                .generic_args
                .into_iter()
                .map(|arg| GenericArgumentCached::new(arg, ctx))
                .collect(),
        }
    }
    fn embed<'db>(self, ctx: &mut SemanticCacheLoadingContext<'db>) -> FunctionLongId<'db> {
        FunctionLongId {
            function: ConcreteFunction {
                generic_function: self.generic_function.embed(ctx),
                generic_args: self.generic_args.into_iter().map(|arg| arg.embed(ctx)).collect(),
            },
        }
    }
}
#[derive(Serialize, Deserialize, Copy, Clone, PartialEq, Eq, Hash, salsa::Update)]
pub struct SemanticFunctionIdCached(usize);
impl SemanticFunctionIdCached {
    pub fn new<'db>(
        function_id: FunctionId<'db>,
        ctx: &mut SemanticCacheSavingContext<'db>,
    ) -> Self {
        if let Some(id) = ctx.function_ids.get(&function_id) {
            return *id;
        }
        let function = SemanticFunctionCached::new(function_id.long(ctx.db).clone(), ctx);
        let id = SemanticFunctionIdCached(ctx.function_ids_lookup.len());
        ctx.function_ids_lookup.push(function);
        ctx.function_ids.insert(function_id, id);
        id
    }
    fn embed<'db>(self, ctx: &mut SemanticCacheLoadingContext<'db>) -> FunctionId<'db> {
        if let Some(function_id) = ctx.function_ids.get(&self) {
            return *function_id;
        }

        let function = ctx.function_ids_lookup[self.0].clone();
        let function_id = function.embed(ctx).intern(ctx.db);
        ctx.function_ids.insert(self, function_id);
        function_id
    }
    pub fn get_embedded<'db>(self, data: &Arc<SemanticCacheLoadingData<'db>>) -> FunctionId<'db> {
        data.function_ids[&self]
    }
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq)]
enum GenericFunctionCached {
    Free(LanguageElementCached),
    Extern(LanguageElementCached),
    Impl(ImplIdCached, LanguageElementCached),
}
impl GenericFunctionCached {
    fn new<'db>(
        generic_function: GenericFunctionId<'db>,
        ctx: &mut SemanticCacheSavingContext<'db>,
    ) -> Self {
        match generic_function {
            GenericFunctionId::Free(id) => {
                GenericFunctionCached::Free(LanguageElementCached::new(id, &mut ctx.defs_ctx))
            }
            GenericFunctionId::Extern(id) => {
                GenericFunctionCached::Extern(LanguageElementCached::new(id, &mut ctx.defs_ctx))
            }
            GenericFunctionId::Impl(id) => GenericFunctionCached::Impl(
                ImplIdCached::new(id.impl_id, ctx),
                LanguageElementCached::new(id.function, &mut ctx.defs_ctx),
            ),
        }
    }
    fn embed<'db>(self, ctx: &mut SemanticCacheLoadingContext<'db>) -> GenericFunctionId<'db> {
        match self {
            GenericFunctionCached::Free(id) => {
                let (module_id, stable_ptr) = id.get_embedded(&ctx.defs_loading_data);
                let id =
                    FreeFunctionLongId(module_id, FunctionWithBodyPtr(stable_ptr)).intern(ctx.db);
                GenericFunctionId::Free(id)
            }
            GenericFunctionCached::Extern(id) => {
                let (module_id, stable_ptr) = id.get_embedded(&ctx.defs_loading_data);
                let id = ExternFunctionLongId(module_id, ItemExternFunctionPtr(stable_ptr))
                    .intern(ctx.db);
                GenericFunctionId::Extern(id)
            }
            GenericFunctionCached::Impl(id, name) => {
                let impl_id = id.embed(ctx);
                let (module_id, stable_ptr) = name.get_embedded(&ctx.defs_loading_data);
                let trait_function_id =
                    TraitFunctionLongId(module_id, TraitItemFunctionPtr(stable_ptr)).intern(ctx.db);

                GenericFunctionId::Impl(ImplGenericFunctionId {
                    impl_id,
                    function: trait_function_id,
                })
            }
        }
    }
}

#[derive(Serialize, Deserialize, Clone)]
pub struct SemanticConcreteFunctionWithBodyCached {
    generic_function: GenericFunctionWithBodyCached,
    generic_args: Vec<GenericArgumentCached>,
}
impl SemanticConcreteFunctionWithBodyCached {
    pub fn new<'db>(
        function_id: ConcreteFunctionWithBodyId<'db>,
        ctx: &mut SemanticCacheSavingContext<'db>,
    ) -> Self {
        Self {
            generic_function: GenericFunctionWithBodyCached::new(
                function_id.generic_function(ctx.db),
                ctx,
            ),
            generic_args: function_id
                .long(ctx.db)
                .generic_args
                .clone()
                .into_iter()
                .map(|arg| GenericArgumentCached::new(arg, ctx))
                .collect(),
        }
    }
    pub fn get_embedded<'db>(
        self,
        data: &Arc<SemanticCacheLoadingData<'db>>,
        db: &'db dyn Database,
    ) -> ConcreteFunctionWithBodyId<'db> {
        let generic_function = self.generic_function.get_embedded(data, db);
        let generic_args =
            self.generic_args.into_iter().map(|arg| arg.get_embedded(data, db)).collect();
        ConcreteFunctionWithBody { generic_function, generic_args }.intern(db)
    }
}

#[derive(Serialize, Deserialize, Clone)]
enum GenericFunctionWithBodyCached {
    Free(LanguageElementCached),
    Impl(ConcreteImplCached, ImplFunctionBodyCached),
    Trait(ConcreteTraitCached, LanguageElementCached),
}

impl GenericFunctionWithBodyCached {
    fn new<'db>(
        generic_function: GenericFunctionWithBodyId<'db>,
        ctx: &mut SemanticCacheSavingContext<'db>,
    ) -> Self {
        match generic_function {
            GenericFunctionWithBodyId::Free(id) => GenericFunctionWithBodyCached::Free(
                LanguageElementCached::new(id, &mut ctx.defs_ctx),
            ),
            GenericFunctionWithBodyId::Impl(id) => GenericFunctionWithBodyCached::Impl(
                ConcreteImplCached::new(id.concrete_impl_id, ctx),
                ImplFunctionBodyCached::new(id.function_body, ctx),
            ),
            GenericFunctionWithBodyId::Trait(id) => GenericFunctionWithBodyCached::Trait(
                ConcreteTraitCached::new(id.concrete_trait(ctx.db), ctx),
                LanguageElementCached::new(id.trait_function(ctx.db), &mut ctx.defs_ctx),
            ),
        }
    }
    pub fn get_embedded<'db>(
        self,
        data: &Arc<SemanticCacheLoadingData<'db>>,
        db: &'db dyn Database,
    ) -> GenericFunctionWithBodyId<'db> {
        match self {
            GenericFunctionWithBodyCached::Free(id) => {
                let (module_id, stable_ptr) = id.get_embedded(&data.defs_loading_data);
                GenericFunctionWithBodyId::Free(
                    FreeFunctionLongId(module_id, FunctionWithBodyPtr(stable_ptr)).intern(db),
                )
            }
            GenericFunctionWithBodyCached::Impl(id, function_body) => {
                GenericFunctionWithBodyId::Impl(ImplGenericFunctionWithBodyId {
                    concrete_impl_id: id.get_embedded(data, db),
                    function_body: function_body.get_embedded(data, db),
                })
            }
            GenericFunctionWithBodyCached::Trait(id, name) => {
                let (module_id, stable_ptr) = name.get_embedded(&data.defs_loading_data);
                GenericFunctionWithBodyId::Trait(
                    ConcreteTraitGenericFunctionLongId::new(
                        db,
                        id.get_embedded(data, db),
                        TraitFunctionLongId(module_id, TraitItemFunctionPtr(stable_ptr)).intern(db),
                    )
                    .intern(db),
                )
            }
        }
    }
}

#[derive(Serialize, Deserialize, Clone)]
enum ImplFunctionBodyCached {
    Impl(LanguageElementCached),
    Trait(LanguageElementCached),
}
impl ImplFunctionBodyCached {
    fn new<'db>(
        function_body: ImplFunctionBodyId<'db>,
        ctx: &mut SemanticCacheSavingContext<'db>,
    ) -> Self {
        match function_body {
            ImplFunctionBodyId::Impl(id) => {
                ImplFunctionBodyCached::Impl(LanguageElementCached::new(id, &mut ctx.defs_ctx))
            }
            ImplFunctionBodyId::Trait(id) => {
                ImplFunctionBodyCached::Trait(LanguageElementCached::new(id, &mut ctx.defs_ctx))
            }
        }
    }
    pub fn get_embedded<'db>(
        self,
        data: &Arc<SemanticCacheLoadingData<'db>>,
        db: &'db dyn Database,
    ) -> ImplFunctionBodyId<'db> {
        match self {
            ImplFunctionBodyCached::Impl(id) => {
                let (module_id, stable_ptr) = id.get_embedded(&data.defs_loading_data);
                ImplFunctionBodyId::Impl(
                    ImplFunctionLongId(module_id, FunctionWithBodyPtr(stable_ptr)).intern(db),
                )
            }
            ImplFunctionBodyCached::Trait(id) => {
                let (module_id, stable_ptr) = id.get_embedded(&data.defs_loading_data);
                ImplFunctionBodyId::Trait(
                    TraitFunctionLongId(module_id, TraitItemFunctionPtr(stable_ptr)).intern(db),
                )
            }
        }
    }
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq)]
enum GenericArgumentCached {
    Type(TypeIdCached),
    Value(ConstValueCached),
    Impl(ImplIdCached),
    NegImpl(NegativeImplIdCached),
}

impl GenericArgumentCached {
    fn new<'db>(
        generic_argument_id: GenericArgumentId<'db>,
        ctx: &mut SemanticCacheSavingContext<'db>,
    ) -> Self {
        match generic_argument_id {
            GenericArgumentId::Type(type_id) => {
                GenericArgumentCached::Type(TypeIdCached::new(type_id, ctx))
            }
            GenericArgumentId::Constant(const_value_id) => GenericArgumentCached::Value(
                ConstValueCached::new(const_value_id.long(ctx.db).clone(), ctx),
            ),
            GenericArgumentId::Impl(impl_id) => {
                GenericArgumentCached::Impl(ImplIdCached::new(impl_id, ctx))
            }
            GenericArgumentId::NegImpl(negative_impl) => {
                GenericArgumentCached::NegImpl(NegativeImplIdCached::new(negative_impl, ctx))
            }
        }
    }
    fn embed<'db>(self, ctx: &mut SemanticCacheLoadingContext<'db>) -> GenericArgumentId<'db> {
        match self {
            GenericArgumentCached::Type(ty) => GenericArgumentId::Type(ty.embed(ctx)),
            GenericArgumentCached::Value(value) => {
                GenericArgumentId::Constant(value.embed(ctx).intern(ctx.db))
            }
            GenericArgumentCached::Impl(imp) => GenericArgumentId::Impl(imp.embed(ctx)),
            GenericArgumentCached::NegImpl(negative_impl) => {
                GenericArgumentId::NegImpl(negative_impl.embed(ctx))
            }
        }
    }
    pub fn get_embedded<'db>(
        self,
        data: &Arc<SemanticCacheLoadingData<'db>>,
        db: &'db dyn Database,
    ) -> GenericArgumentId<'db> {
        match self {
            GenericArgumentCached::Type(ty) => GenericArgumentId::Type(ty.get_embedded(data)),
            GenericArgumentCached::Value(value) => {
                GenericArgumentId::Constant(value.get_embedded(data, db).intern(db))
            }
            GenericArgumentCached::Impl(imp) => GenericArgumentId::Impl(imp.get_embedded(data)),
            GenericArgumentCached::NegImpl(negative_impl) => {
                GenericArgumentId::NegImpl(negative_impl.get_embedded(data))
            }
        }
    }
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq)]
enum TypeCached {
    Concrete(ConcreteTypeCached),
    Tuple(Vec<TypeIdCached>),
    Snapshot(Box<TypeIdCached>),
    GenericParameter(GenericParamCached),
    ImplType(ImplTypeCached),
    FixedSizeArray(TypeIdCached, ConstValueCached),
    ClosureType(ClosureTypeCached),
}

impl TypeCached {
    fn new<'db>(type_id: TypeLongId<'db>, ctx: &mut SemanticCacheSavingContext<'db>) -> Self {
        match type_id {
            TypeLongId::Concrete(concrete_type_id) => {
                TypeCached::Concrete(ConcreteTypeCached::new(concrete_type_id, ctx))
            }
            TypeLongId::Tuple(vec) => {
                TypeCached::Tuple(vec.into_iter().map(|ty| TypeIdCached::new(ty, ctx)).collect())
            }
            TypeLongId::Snapshot(type_id) => {
                TypeCached::Snapshot(Box::new(TypeIdCached::new(type_id, ctx)))
            }
            TypeLongId::GenericParameter(generic_param_id) => TypeCached::GenericParameter(
                GenericParamCached::new(generic_param_id, &mut ctx.defs_ctx),
            ),
            TypeLongId::ImplType(impl_type_id) => {
                TypeCached::ImplType(ImplTypeCached::new(impl_type_id, ctx))
            }
            TypeLongId::FixedSizeArray { type_id, size } => TypeCached::FixedSizeArray(
                TypeIdCached::new(type_id, ctx),
                ConstValueCached::new(size.long(ctx.db).clone(), ctx),
            ),
            TypeLongId::Closure(closure_ty) => {
                TypeCached::ClosureType(ClosureTypeCached::new(closure_ty, ctx))
            }
            TypeLongId::Var(_) | TypeLongId::Missing(_) | TypeLongId::Coupon(_) => {
                unreachable!("type {:?} is not supported for caching", type_id.debug(ctx.db))
            }
        }
    }
    fn embed<'db>(self, ctx: &mut SemanticCacheLoadingContext<'db>) -> TypeLongId<'db> {
        match self {
            TypeCached::Concrete(concrete_type) => TypeLongId::Concrete(concrete_type.embed(ctx)),
            TypeCached::Tuple(vec) => {
                TypeLongId::Tuple(vec.into_iter().map(|ty| ty.embed(ctx)).collect())
            }
            TypeCached::Snapshot(type_id) => TypeLongId::Snapshot(type_id.embed(ctx)),
            TypeCached::GenericParameter(generic_param) => TypeLongId::GenericParameter(
                generic_param.get_embedded(&ctx.defs_loading_data, ctx.db),
            ),
            TypeCached::ImplType(impl_type) => TypeLongId::ImplType(impl_type.embed(ctx)),
            TypeCached::FixedSizeArray(type_id, size) => TypeLongId::FixedSizeArray {
                type_id: type_id.embed(ctx),
                size: size.embed(ctx).intern(ctx.db),
            },
            TypeCached::ClosureType(closure_ty) => TypeLongId::Closure(closure_ty.embed(ctx)),
        }
    }
}

#[derive(Serialize, Deserialize, Copy, Clone, PartialEq, Eq, Hash, salsa::Update)]
pub struct TypeIdCached(usize);

impl TypeIdCached {
    pub fn new<'db>(ty: TypeId<'db>, ctx: &mut SemanticCacheSavingContext<'db>) -> Self {
        if let Some(id) = ctx.type_ids.get(&ty) {
            return *id;
        }
        let ty_long = TypeCached::new(ty.long(ctx.db).clone(), ctx);
        let id = TypeIdCached(ctx.type_ids_lookup.len());
        ctx.type_ids_lookup.push(ty_long);
        ctx.type_ids.insert(ty, id);
        id
    }
    fn embed<'db>(self, ctx: &mut SemanticCacheLoadingContext<'db>) -> TypeId<'db> {
        if let Some(type_id) = ctx.type_ids.get(&self) {
            return *type_id;
        }

        let ty = ctx.type_ids_lookup[self.0].clone();
        let ty = ty.embed(ctx).intern(ctx.db);
        ctx.type_ids.insert(self, ty);
        ty
    }
    pub fn get_embedded<'db>(self, data: &Arc<SemanticCacheLoadingData<'db>>) -> TypeId<'db> {
        data.type_ids[&self]
    }
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq)]
enum ConcreteTypeCached {
    Struct(ConcreteStructCached),
    Enum(ConcreteEnumCached),
    Extern(ConcreteExternTypeCached),
}

impl ConcreteTypeCached {
    fn new<'db>(
        concrete_type_id: ConcreteTypeId<'db>,
        ctx: &mut SemanticCacheSavingContext<'db>,
    ) -> Self {
        match concrete_type_id {
            ConcreteTypeId::Struct(id) => {
                ConcreteTypeCached::Struct(ConcreteStructCached::new(id, ctx))
            }
            ConcreteTypeId::Enum(id) => ConcreteTypeCached::Enum(ConcreteEnumCached::new(id, ctx)),
            ConcreteTypeId::Extern(id) => {
                ConcreteTypeCached::Extern(ConcreteExternTypeCached::new(id, ctx))
            }
        }
    }
    fn embed<'db>(self, ctx: &mut SemanticCacheLoadingContext<'db>) -> ConcreteTypeId<'db> {
        match self {
            ConcreteTypeCached::Struct(s) => ConcreteTypeId::Struct(s.embed(ctx)),
            ConcreteTypeCached::Enum(e) => ConcreteTypeId::Enum(e.embed(ctx)),
            ConcreteTypeCached::Extern(e) => ConcreteTypeId::Extern(e.embed(ctx)),
        }
    }
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq)]
struct ImplTypeCached {
    impl_id: ImplIdCached,
    trait_type: TraitTypeCached,
}
impl ImplTypeCached {
    fn new<'db>(impl_type_id: ImplTypeId<'db>, ctx: &mut SemanticCacheSavingContext<'db>) -> Self {
        Self {
            impl_id: ImplIdCached::new(impl_type_id.impl_id(), ctx),
            trait_type: TraitTypeCached::new(impl_type_id.ty(), ctx),
        }
    }
    fn embed<'db>(self, ctx: &mut SemanticCacheLoadingContext<'db>) -> ImplTypeId<'db> {
        let impl_id = self.impl_id.embed(ctx);
        let ty = self.trait_type.embed(ctx);
        ImplTypeId::new(impl_id, ty, ctx.db)
    }
}
#[derive(Serialize, Deserialize, Clone, PartialEq, Eq)]
struct ClosureTypeCached {
    param_tys: Vec<TypeIdCached>,
    ret_ty: TypeIdCached,
    captured_types: Vec<TypeIdCached>,
    parent_function: SemanticFunctionIdCached,
    wrapper_location: SyntaxStablePtrIdCached,
}

impl ClosureTypeCached {
    fn new<'db>(
        closure_type_id: ClosureTypeLongId<'db>,
        ctx: &mut SemanticCacheSavingContext<'db>,
    ) -> Self {
        Self {
            param_tys: closure_type_id
                .param_tys
                .iter()
                .map(|ty| TypeIdCached::new(*ty, ctx))
                .collect(),
            ret_ty: TypeIdCached::new(closure_type_id.ret_ty, ctx),
            captured_types: closure_type_id
                .captured_types
                .iter()
                .map(|ty| TypeIdCached::new(*ty, ctx))
                .collect(),
            parent_function: SemanticFunctionIdCached::new(
                closure_type_id.parent_function.unwrap(),
                ctx,
            ),
            wrapper_location: SyntaxStablePtrIdCached::new(
                closure_type_id.wrapper_location.stable_ptr(),
                &mut ctx.defs_ctx,
            ),
        }
    }
    fn embed<'db>(self, ctx: &mut SemanticCacheLoadingContext<'db>) -> ClosureTypeLongId<'db> {
        ClosureTypeLongId {
            param_tys: self.param_tys.into_iter().map(|ty| ty.embed(ctx)).collect(),
            ret_ty: self.ret_ty.embed(ctx),
            captured_types: self.captured_types.into_iter().map(|ty| ty.embed(ctx)).collect(),
            parent_function: Ok(self.parent_function.embed(ctx)),
            wrapper_location: StableLocation::new(
                self.wrapper_location.get_embedded(&ctx.defs_loading_data),
            ),
        }
    }
}

#[derive(Serialize, Deserialize, Clone, Hash, PartialEq, Eq)]
struct TraitTypeCached {
    language_element: LanguageElementCached,
}
impl TraitTypeCached {
    fn new<'db>(
        trait_type_id: TraitTypeId<'db>,
        ctx: &mut SemanticCacheSavingContext<'db>,
    ) -> Self {
        Self { language_element: LanguageElementCached::new(trait_type_id, &mut ctx.defs_ctx) }
    }
    fn embed<'db>(self, ctx: &mut SemanticCacheLoadingContext<'db>) -> TraitTypeId<'db> {
        let (module_id, stable_ptr) = self.language_element.get_embedded(&ctx.defs_loading_data);
        TraitTypeLongId(module_id, TraitItemTypePtr(stable_ptr)).intern(ctx.db)
    }
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq)]
enum ImplCached {
    Concrete(ConcreteImplCached),
    GenericParameter(GenericParamCached),
    ImplImpl(ImplImplCached),
    GeneratedImpl(GeneratedImplCached),
    SelfImpl(ConcreteTraitCached),
}
impl ImplCached {
    fn new<'db>(impl_id: ImplLongId<'db>, ctx: &mut SemanticCacheSavingContext<'db>) -> Self {
        match impl_id {
            ImplLongId::Concrete(concrete_impl) => {
                ImplCached::Concrete(ConcreteImplCached::new(concrete_impl, ctx))
            }
            ImplLongId::GenericParameter(generic_param_id) => ImplCached::GenericParameter(
                GenericParamCached::new(generic_param_id, &mut ctx.defs_ctx),
            ),
            ImplLongId::GeneratedImpl(generated_impl) => {
                ImplCached::GeneratedImpl(GeneratedImplCached::new(generated_impl, ctx))
            }
            ImplLongId::ImplImpl(impl_impl) => {
                ImplCached::ImplImpl(ImplImplCached::new(impl_impl, ctx))
            }
            ImplLongId::SelfImpl(concrete_trait) => {
                ImplCached::SelfImpl(ConcreteTraitCached::new(concrete_trait, ctx))
            }
            ImplLongId::ImplVar(_) => {
                unreachable!("impl {:?} is not supported for caching", impl_id.debug(ctx.db))
            }
        }
    }
    fn embed<'db>(self, ctx: &mut SemanticCacheLoadingContext<'db>) -> ImplLongId<'db> {
        match self {
            ImplCached::Concrete(concrete_impl) => ImplLongId::Concrete(concrete_impl.embed(ctx)),
            ImplCached::ImplImpl(impl_impl) => ImplLongId::ImplImpl(impl_impl.embed(ctx)),
            ImplCached::GenericParameter(generic_param) => ImplLongId::GenericParameter(
                generic_param.get_embedded(&ctx.defs_loading_data, ctx.db),
            ),
            ImplCached::GeneratedImpl(generated_impl) => {
                ImplLongId::GeneratedImpl(generated_impl.embed(ctx))
            }
            ImplCached::SelfImpl(concrete_trait) => ImplLongId::SelfImpl(concrete_trait.embed(ctx)),
        }
    }
}
#[derive(Serialize, Deserialize, Copy, Clone, PartialEq, Eq, Hash, salsa::Update)]
pub struct ImplIdCached(usize);

impl ImplIdCached {
    pub fn new<'db>(impl_id: ImplId<'db>, ctx: &mut SemanticCacheSavingContext<'db>) -> Self {
        if let Some(id) = ctx.impl_ids.get(&impl_id) {
            return *id;
        }
        let imp = ImplCached::new(impl_id.long(ctx.db).clone(), ctx);
        let id = ImplIdCached(ctx.impl_ids_lookup.len());
        ctx.impl_ids_lookup.push(imp);
        ctx.impl_ids.insert(impl_id, id);
        id
    }
    fn embed<'db>(self, ctx: &mut SemanticCacheLoadingContext<'db>) -> ImplId<'db> {
        if let Some(impl_id) = ctx.impl_ids.get(&self) {
            return *impl_id;
        }

        let imp = ctx.impl_ids_lookup[self.0].clone();
        let imp = imp.embed(ctx).intern(ctx.db);
        ctx.impl_ids.insert(self, imp);
        imp
    }
    pub fn get_embedded<'db>(self, data: &Arc<SemanticCacheLoadingData<'db>>) -> ImplId<'db> {
        data.impl_ids[&self]
    }
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq)]
struct ConcreteImplCached {
    impl_def_id: ImplDefIdCached,
    generic_args: Vec<GenericArgumentCached>,
}
impl ConcreteImplCached {
    fn new<'db>(
        concrete_impl: ConcreteImplId<'db>,
        ctx: &mut SemanticCacheSavingContext<'db>,
    ) -> Self {
        let long_id = concrete_impl.long(ctx.db);
        Self {
            impl_def_id: ImplDefIdCached::new(long_id.impl_def_id, &mut ctx.defs_ctx),
            generic_args: long_id
                .generic_args
                .clone()
                .into_iter()
                .map(|arg| GenericArgumentCached::new(arg, ctx))
                .collect(),
        }
    }
    fn embed<'db>(self, ctx: &mut SemanticCacheLoadingContext<'db>) -> ConcreteImplId<'db> {
        let impl_def_id = self.impl_def_id.get_embedded(&ctx.defs_loading_data);
        let long_id = ConcreteImplLongId {
            impl_def_id,
            generic_args: self.generic_args.into_iter().map(|arg| arg.embed(ctx)).collect(),
        };
        long_id.intern(ctx.db)
    }
    pub fn get_embedded<'db>(
        self,
        data: &Arc<SemanticCacheLoadingData<'db>>,
        db: &'db dyn Database,
    ) -> ConcreteImplId<'db> {
        let impl_def_id = self.impl_def_id.get_embedded(&data.defs_loading_data);
        let long_id = ConcreteImplLongId {
            impl_def_id,
            generic_args: self
                .generic_args
                .into_iter()
                .map(|arg| arg.get_embedded(data, db))
                .collect(),
        };
        long_id.intern(db)
    }
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq)]
struct ImplImplCached {
    impl_id: ImplIdCached,
    trait_impl_id: TraitImplCached,
}
impl ImplImplCached {
    fn new<'db>(impl_impl_id: ImplImplId<'db>, ctx: &mut SemanticCacheSavingContext<'db>) -> Self {
        Self {
            impl_id: ImplIdCached::new(impl_impl_id.impl_id(), ctx),
            trait_impl_id: TraitImplCached::new(impl_impl_id.trait_impl_id(), ctx),
        }
    }
    fn embed<'db>(self, ctx: &mut SemanticCacheLoadingContext<'db>) -> ImplImplId<'db> {
        let impl_id = self.impl_id.embed(ctx);
        let trait_impl_id = self.trait_impl_id.embed(ctx);
        ImplImplId::new(impl_id, trait_impl_id, ctx.db)
    }
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq)]
struct TraitImplCached {
    language_element: LanguageElementCached,
}
impl TraitImplCached {
    fn new<'db>(
        trait_impl_id: TraitImplId<'db>,
        ctx: &mut SemanticCacheSavingContext<'db>,
    ) -> Self {
        Self { language_element: LanguageElementCached::new(trait_impl_id, &mut ctx.defs_ctx) }
    }
    fn embed<'db>(self, ctx: &mut SemanticCacheLoadingContext<'db>) -> TraitImplId<'db> {
        let (module_id, stable_ptr) = self.language_element.get_embedded(&ctx.defs_loading_data);
        TraitImplLongId(module_id, TraitItemImplPtr(stable_ptr)).intern(ctx.db)
    }
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq)]
struct GeneratedImplCached {
    pub concrete_trait: ConcreteTraitCached,
    pub generic_params: Vec<SemanticGenericParamCached>,
    pub impl_items: OrderedHashMap<TraitTypeCached, TypeIdCached>,
}
impl GeneratedImplCached {
    fn new<'db>(
        generated_impl: GeneratedImplId<'db>,
        ctx: &mut SemanticCacheSavingContext<'db>,
    ) -> Self {
        let generated_impl = generated_impl.long(ctx.db);
        Self {
            concrete_trait: ConcreteTraitCached::new(generated_impl.concrete_trait, ctx),
            generic_params: generated_impl
                .generic_params
                .clone()
                .into_iter()
                .map(|param| SemanticGenericParamCached::new(param, ctx))
                .collect(),
            impl_items: generated_impl
                .impl_items
                .0
                .clone()
                .into_iter()
                .map(|(k, v)| (TraitTypeCached::new(k, ctx), TypeIdCached::new(v, ctx)))
                .collect(),
        }
    }
    fn embed<'db>(self, ctx: &mut SemanticCacheLoadingContext<'db>) -> GeneratedImplId<'db> {
        GeneratedImplLongId {
            concrete_trait: self.concrete_trait.embed(ctx),
            generic_params: self.generic_params.into_iter().map(|param| param.embed(ctx)).collect(),
            impl_items: GeneratedImplItems(
                self.impl_items.into_iter().map(|(k, v)| (k.embed(ctx), v.embed(ctx))).collect(),
            ),
        }
        .intern(ctx.db)
    }
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq)]
enum NegativeImplCached {
    Solved(ConcreteTraitCached),
    GenericParameter(GenericParamCached),
}
impl NegativeImplCached {
    fn new<'db>(
        negative_impl_id: NegativeImplLongId<'db>,
        ctx: &mut SemanticCacheSavingContext<'db>,
    ) -> Self {
        match negative_impl_id {
            NegativeImplLongId::Solved(concrete_trait_id) => {
                NegativeImplCached::Solved(ConcreteTraitCached::new(concrete_trait_id, ctx))
            }
            NegativeImplLongId::GenericParameter(generic_param_id) => {
                NegativeImplCached::GenericParameter(GenericParamCached::new(
                    generic_param_id,
                    &mut ctx.defs_ctx,
                ))
            }
            NegativeImplLongId::NegativeImplVar(_) => {
                unreachable!("negative impl var is not supported for caching",)
            }
        }
    }
    fn embed<'db>(self, ctx: &mut SemanticCacheLoadingContext<'db>) -> NegativeImplLongId<'db> {
        match self {
            NegativeImplCached::Solved(concrete_trait) => {
                NegativeImplLongId::Solved(concrete_trait.embed(ctx))
            }
            NegativeImplCached::GenericParameter(generic_param) => {
                NegativeImplLongId::GenericParameter(
                    generic_param.get_embedded(&ctx.defs_loading_data, ctx.db),
                )
            }
        }
    }
}

#[derive(Serialize, Deserialize, Copy, Clone, PartialEq, Eq, Hash, salsa::Update)]
struct NegativeImplIdCached(usize);

impl NegativeImplIdCached {
    fn new<'db>(
        negative_impl_id: NegativeImplId<'db>,
        ctx: &mut SemanticCacheSavingContext<'db>,
    ) -> Self {
        if let Some(id) = ctx.negative_impl_ids.get(&negative_impl_id) {
            return *id;
        }
        let imp = NegativeImplCached::new(negative_impl_id.long(ctx.db).clone(), ctx);
        let id = NegativeImplIdCached(ctx.negative_impl_ids_lookup.len());
        ctx.negative_impl_ids_lookup.push(imp);
        ctx.negative_impl_ids.insert(negative_impl_id, id);
        id
    }
    fn embed<'db>(self, ctx: &mut SemanticCacheLoadingContext<'db>) -> NegativeImplId<'db> {
        if let Some(negative_impl_id) = ctx.negative_impl_ids.get(&self) {
            return *negative_impl_id;
        }

        let imp = ctx.negative_impl_ids_lookup[self.0].clone();
        let imp = imp.embed(ctx).intern(ctx.db);
        ctx.negative_impl_ids.insert(self, imp);
        imp
    }
    pub fn get_embedded<'db>(
        self,
        data: &Arc<SemanticCacheLoadingData<'db>>,
    ) -> NegativeImplId<'db> {
        data.negative_impl_ids[&self]
    }
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq)]
enum SemanticGenericParamCached {
    Type(GenericParamTypeCached),
    Const(GenericParamConstCached),
    Impl(GenericParamImplCached),
    NegImpl(GenericParamImplCached),
}
impl SemanticGenericParamCached {
    fn new<'db>(
        generic_param_id: GenericParam<'db>,
        ctx: &mut SemanticCacheSavingContext<'db>,
    ) -> Self {
        match generic_param_id {
            GenericParam::Type(generic_param) => {
                SemanticGenericParamCached::Type(GenericParamTypeCached::new(generic_param, ctx))
            }
            GenericParam::Const(generic_param) => {
                SemanticGenericParamCached::Const(GenericParamConstCached::new(generic_param, ctx))
            }
            GenericParam::Impl(generic_param) => {
                SemanticGenericParamCached::Impl(GenericParamImplCached::new(generic_param, ctx))
            }
            GenericParam::NegImpl(generic_param) => {
                SemanticGenericParamCached::NegImpl(GenericParamImplCached::new(generic_param, ctx))
            }
        }
    }
    fn embed<'db>(self, ctx: &mut SemanticCacheLoadingContext<'db>) -> GenericParam<'db> {
        match self {
            SemanticGenericParamCached::Type(generic_param) => {
                GenericParam::Type(generic_param.embed(ctx))
            }
            SemanticGenericParamCached::Const(generic_param) => {
                GenericParam::Const(generic_param.embed(ctx))
            }
            SemanticGenericParamCached::Impl(generic_param) => {
                GenericParam::Impl(generic_param.embed(ctx))
            }
            SemanticGenericParamCached::NegImpl(generic_param) => {
                GenericParam::NegImpl(generic_param.embed(ctx))
            }
        }
    }
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq)]
struct GenericParamTypeCached {
    id: GenericParamCached,
}

impl GenericParamTypeCached {
    fn new<'db>(
        generic_param: GenericParamType<'db>,
        ctx: &mut SemanticCacheSavingContext<'db>,
    ) -> Self {
        Self { id: GenericParamCached::new(generic_param.id, &mut ctx.defs_ctx) }
    }
    fn embed<'db>(self, ctx: &mut SemanticCacheLoadingContext<'db>) -> GenericParamType<'db> {
        GenericParamType { id: self.id.get_embedded(&ctx.defs_loading_data, ctx.db) }
    }
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq)]
struct GenericParamConstCached {
    id: GenericParamCached,
    ty: TypeIdCached,
}

impl GenericParamConstCached {
    fn new<'db>(
        generic_param: GenericParamConst<'db>,
        ctx: &mut SemanticCacheSavingContext<'db>,
    ) -> Self {
        Self {
            id: GenericParamCached::new(generic_param.id, &mut ctx.defs_ctx),
            ty: TypeIdCached::new(generic_param.ty, ctx),
        }
    }
    fn embed<'db>(self, ctx: &mut SemanticCacheLoadingContext<'db>) -> GenericParamConst<'db> {
        GenericParamConst {
            id: self.id.get_embedded(&ctx.defs_loading_data, ctx.db),
            ty: self.ty.embed(ctx),
        }
    }
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq)]
struct GenericParamImplCached {
    id: GenericParamCached,
    concrete_trait: ConcreteTraitCached,
    type_constraints: OrderedHashMap<TraitTypeCached, TypeIdCached>,
}

impl GenericParamImplCached {
    fn new<'db>(
        generic_param: GenericParamImpl<'db>,
        ctx: &mut SemanticCacheSavingContext<'db>,
    ) -> Self {
        Self {
            id: GenericParamCached::new(generic_param.id, &mut ctx.defs_ctx),
            concrete_trait: ConcreteTraitCached::new(generic_param.concrete_trait.unwrap(), ctx),

            type_constraints: generic_param
                .type_constraints
                .into_iter()
                .map(|(k, v)| (TraitTypeCached::new(k, ctx), TypeIdCached::new(v, ctx)))
                .collect(),
        }
    }
    fn embed<'db>(self, ctx: &mut SemanticCacheLoadingContext<'db>) -> GenericParamImpl<'db> {
        GenericParamImpl {
            id: self.id.get_embedded(&ctx.defs_loading_data, ctx.db),
            concrete_trait: Ok(self.concrete_trait.embed(ctx)),
            type_constraints: self
                .type_constraints
                .into_iter()
                .map(|(k, v)| (k.embed(ctx), v.embed(ctx)))
                .collect(),
        }
    }
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq)]
pub struct ConcreteVariantCached {
    concrete_enum_id: ConcreteEnumCached,
    id: LanguageElementCached,
    ty: TypeIdCached,
    idx: usize,
}
impl ConcreteVariantCached {
    pub fn new<'db>(
        concrete_variant: ConcreteVariant<'db>,
        ctx: &mut SemanticCacheSavingContext<'db>,
    ) -> Self {
        Self {
            concrete_enum_id: ConcreteEnumCached::new(concrete_variant.concrete_enum_id, ctx),
            id: LanguageElementCached::new(concrete_variant.id, &mut ctx.defs_ctx),
            ty: TypeIdCached::new(concrete_variant.ty, ctx),
            idx: concrete_variant.idx,
        }
    }
    fn embed<'db>(self, ctx: &mut SemanticCacheLoadingContext<'db>) -> ConcreteVariant<'db> {
        let concrete_enum_id = self.concrete_enum_id.embed(ctx);
        let ty = self.ty.embed(ctx);
        let (module_id, stable_ptr) = self.id.get_embedded(&ctx.defs_loading_data);

        let id = VariantLongId(module_id, VariantPtr(stable_ptr)).intern(ctx.db);
        ConcreteVariant { concrete_enum_id, id, ty, idx: self.idx }
    }
    pub fn get_embedded<'db>(
        self,
        data: &Arc<SemanticCacheLoadingData<'db>>,
        db: &'db dyn Database,
    ) -> ConcreteVariant<'db> {
        let concrete_enum_id = self.concrete_enum_id.get_embedded(data, db);
        let ty = self.ty.get_embedded(data);
        let (module_id, stable_ptr) = self.id.get_embedded(&data.defs_loading_data);
        let id = VariantLongId(module_id, VariantPtr(stable_ptr)).intern(db);
        ConcreteVariant { concrete_enum_id, id, ty, idx: self.idx }
    }
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq)]
pub struct ConcreteEnumCached {
    enum_id: LanguageElementCached,
    generic_args: Vec<GenericArgumentCached>,
}

impl ConcreteEnumCached {
    pub fn new<'db>(
        concrete_enum: ConcreteEnumId<'db>,
        ctx: &mut SemanticCacheSavingContext<'db>,
    ) -> Self {
        let long_id = concrete_enum.long(ctx.db);
        Self {
            enum_id: LanguageElementCached::new(long_id.enum_id, &mut ctx.defs_ctx),
            generic_args: long_id
                .generic_args
                .clone()
                .into_iter()
                .map(|arg| GenericArgumentCached::new(arg, ctx))
                .collect(),
        }
    }
    fn embed<'db>(self, ctx: &mut SemanticCacheLoadingContext<'db>) -> ConcreteEnumId<'db> {
        let (module_id, stable_ptr) = self.enum_id.get_embedded(&ctx.defs_loading_data);

        let long_id = ConcreteEnumLongId {
            enum_id: EnumLongId(module_id, ItemEnumPtr(stable_ptr)).intern(ctx.db),
            generic_args: self.generic_args.into_iter().map(|arg| arg.embed(ctx)).collect(),
        };
        long_id.intern(ctx.db)
    }
    pub fn get_embedded<'db>(
        self,
        data: &Arc<SemanticCacheLoadingData<'db>>,
        db: &'db dyn Database,
    ) -> ConcreteEnumId<'db> {
        let (module_id, stable_ptr) = self.enum_id.get_embedded(&data.defs_loading_data);
        let id = EnumLongId(module_id, ItemEnumPtr(stable_ptr)).intern(db);
        ConcreteEnumLongId {
            enum_id: id,
            generic_args: self
                .generic_args
                .into_iter()
                .map(|arg| arg.get_embedded(data, db))
                .collect(),
        }
        .intern(db)
    }
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq)]
pub struct ConcreteStructCached {
    struct_id: LanguageElementCached,
    generic_args: Vec<GenericArgumentCached>,
}
impl ConcreteStructCached {
    fn new<'db>(
        concrete_struct: ConcreteStructId<'db>,
        ctx: &mut SemanticCacheSavingContext<'db>,
    ) -> Self {
        let long_id = concrete_struct.long(ctx.db);
        Self {
            struct_id: LanguageElementCached::new(long_id.struct_id, &mut ctx.defs_ctx),
            generic_args: long_id
                .generic_args
                .clone()
                .into_iter()
                .map(|arg| GenericArgumentCached::new(arg, ctx))
                .collect(),
        }
    }
    fn embed<'db>(self, ctx: &mut SemanticCacheLoadingContext<'db>) -> ConcreteStructId<'db> {
        let (module_id, stable_ptr) = self.struct_id.get_embedded(&ctx.defs_loading_data);

        let long_id = ConcreteStructLongId {
            struct_id: StructLongId(module_id, ItemStructPtr(stable_ptr)).intern(ctx.db),
            generic_args: self.generic_args.into_iter().map(|arg| arg.embed(ctx)).collect(),
        };
        long_id.intern(ctx.db)
    }
    pub fn get_embedded<'db>(
        self,
        data: &Arc<SemanticCacheLoadingData<'db>>,
        db: &'db dyn Database,
    ) -> ConcreteStructId<'db> {
        let (module_id, stable_ptr) = self.struct_id.get_embedded(&data.defs_loading_data);
        let long_id = ConcreteStructLongId {
            struct_id: StructLongId(module_id, ItemStructPtr(stable_ptr)).intern(db),
            generic_args: self
                .generic_args
                .into_iter()
                .map(|arg| arg.get_embedded(data, db))
                .collect(),
        };
        long_id.intern(db)
    }
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq)]
struct ConcreteExternTypeCached {
    language_element: LanguageElementCached,
    generic_args: Vec<GenericArgumentCached>,
}
impl ConcreteExternTypeCached {
    fn new<'db>(
        concrete_extern_type: ConcreteExternTypeId<'db>,
        ctx: &mut SemanticCacheSavingContext<'db>,
    ) -> Self {
        let long_id = concrete_extern_type.long(ctx.db);
        Self {
            language_element: LanguageElementCached::new(long_id.extern_type_id, &mut ctx.defs_ctx),
            generic_args: long_id
                .generic_args
                .clone()
                .into_iter()
                .map(|arg| GenericArgumentCached::new(arg, ctx))
                .collect(),
        }
    }
    fn embed<'db>(self, ctx: &mut SemanticCacheLoadingContext<'db>) -> ConcreteExternTypeId<'db> {
        let (module_id, stable_ptr) = self.language_element.get_embedded(&ctx.defs_loading_data);

        let long_id = ConcreteExternTypeLongId {
            extern_type_id: ExternTypeLongId(module_id, ItemExternTypePtr(stable_ptr))
                .intern(ctx.db),
            generic_args: self.generic_args.into_iter().map(|arg| arg.embed(ctx)).collect(),
        };
        long_id.intern(ctx.db)
    }
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq)]
struct ConcreteTraitCached {
    trait_id: LanguageElementCached,
    generic_args: Vec<GenericArgumentCached>,
}

impl ConcreteTraitCached {
    fn new<'db>(
        concrete_trait: ConcreteTraitId<'db>,
        ctx: &mut SemanticCacheSavingContext<'db>,
    ) -> Self {
        let long_id = concrete_trait.long(ctx.db);
        Self {
            trait_id: LanguageElementCached::new(long_id.trait_id, &mut ctx.defs_ctx),
            generic_args: long_id
                .generic_args
                .clone()
                .into_iter()
                .map(|arg| GenericArgumentCached::new(arg, ctx))
                .collect(),
        }
    }
    fn embed<'db>(self, ctx: &mut SemanticCacheLoadingContext<'db>) -> ConcreteTraitId<'db> {
        let (module_id, stable_ptr) = self.trait_id.get_embedded(&ctx.defs_loading_data);

        let long_id = ConcreteTraitLongId {
            trait_id: TraitLongId(module_id, ItemTraitPtr(stable_ptr)).intern(ctx.db),
            generic_args: self.generic_args.into_iter().map(|arg| arg.embed(ctx)).collect(),
        };
        long_id.intern(ctx.db)
    }
    pub fn get_embedded<'db>(
        self,
        data: &Arc<SemanticCacheLoadingData<'db>>,
        db: &'db dyn Database,
    ) -> ConcreteTraitId<'db> {
        let (module_id, stable_ptr) = self.trait_id.get_embedded(&data.defs_loading_data);
        let long_id = ConcreteTraitLongId {
            trait_id: TraitLongId(module_id, ItemTraitPtr(stable_ptr)).intern(db),
            generic_args: self
                .generic_args
                .into_iter()
                .map(|arg| arg.get_embedded(data, db))
                .collect(),
        };
        long_id.intern(db)
    }
}
