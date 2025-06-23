#[cfg(test)]
#[path = "test.rs"]
mod test;

use std::ops::{Deref, DerefMut};
use std::sync::Arc;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::cache::{
    CachedCrateMetadata, DefCacheLoadingData, DefCacheSavingContext, GenericParamCached,
    ImplDefIdCached, LanguageElementCached, SyntaxStablePtrIdCached, generate_crate_def_cache,
};
use cairo_lang_defs::diagnostic_utils::StableLocation;
use cairo_lang_defs::ids::{
    EnumLongId, ExternFunctionLongId, ExternTypeLongId, FreeFunctionLongId, FunctionWithBodyId,
    ImplFunctionLongId, LocalVarId, LocalVarLongId, MemberLongId, ParamLongId,
    StatementConstLongId, StatementItemId, StatementUseLongId, StructLongId, TraitConstantId,
    TraitConstantLongId, TraitFunctionLongId, TraitImplId, TraitImplLongId, TraitLongId,
    TraitTypeId, TraitTypeLongId, VariantLongId,
};
use cairo_lang_diagnostics::{Maybe, skip_diagnostic};
use cairo_lang_filesystem::ids::CrateId;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::expr::inference::InferenceError;
use cairo_lang_semantic::items::constant::{ConstValue, ImplConstantId};
use cairo_lang_semantic::items::functions::{
    ConcreteFunctionWithBody, GenericFunctionId, GenericFunctionWithBodyId, ImplFunctionBodyId,
    ImplGenericFunctionId, ImplGenericFunctionWithBodyId,
};
use cairo_lang_semantic::items::generics::{GenericParamConst, GenericParamImpl, GenericParamType};
use cairo_lang_semantic::items::imp::{
    GeneratedImplId, GeneratedImplItems, GeneratedImplLongId, ImplId, ImplImplId, ImplLongId,
};
use cairo_lang_semantic::items::trt::ConcreteTraitGenericFunctionLongId;
use cairo_lang_semantic::types::{
    ClosureTypeLongId, ConcreteEnumLongId, ConcreteExternTypeLongId, ConcreteStructLongId,
    ImplTypeId,
};
use cairo_lang_semantic::{
    ConcreteFunction, ConcreteImplLongId, ConcreteTraitLongId, GenericParam, MatchArmSelector,
    TypeId, TypeLongId, ValueSelectorArm,
};
use cairo_lang_syntax::node::TypedStablePtr;
use cairo_lang_syntax::node::ast::{
    ExprPtr, FunctionWithBodyPtr, ItemConstantPtr, ItemEnumPtr, ItemExternFunctionPtr,
    ItemExternTypePtr, ItemStructPtr, ItemTraitPtr, MemberPtr, ParamPtr, TerminalIdentifierPtr,
    TraitItemConstantPtr, TraitItemFunctionPtr, TraitItemImplPtr, TraitItemTypePtr, UsePathLeafPtr,
    VariantPtr,
};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::{Intern, LookupIntern};
use id_arena::Arena;
use num_bigint::BigInt;
use serde::{Deserialize, Serialize};
use {cairo_lang_defs as defs, cairo_lang_semantic as semantic};

use crate::blocks::BlocksBuilder;
use crate::db::LoweringGroup;
use crate::ids::{
    FunctionId, FunctionLongId, GeneratedFunction, GeneratedFunctionKey, LocationId, Signature,
};
use crate::lower::{MultiLowering, lower_semantic_function};
use crate::objects::{
    BlockId, MatchExternInfo, Statement, StatementCall, StatementConst, StatementStructDestructure,
    VariableId,
};
use crate::{
    Block, BlockEnd, Location, Lowered, MatchArm, MatchEnumInfo, MatchEnumValue, MatchInfo,
    StatementDesnap, StatementEnumConstruct, StatementSnapshot, StatementStructConstruct,
    VarRemapping, VarUsage, Variable,
};

type LookupCache =
    (CacheLookups, SemanticCacheLookups, Vec<(DefsFunctionWithBodyIdCached, MultiLoweringCached)>);

/// Load the cached lowering of a crate if it has a cache file configuration.
pub fn load_cached_crate_functions(
    db: &dyn LoweringGroup,
    crate_id: CrateId,
) -> Option<Arc<OrderedHashMap<defs::ids::FunctionWithBodyId, MultiLowering>>> {
    let blob_id = db.crate_config(crate_id)?.cache_file?;
    let Some(content) = db.blob_content(blob_id) else {
        return Default::default();
    };

    let def_loading_data = db.cached_crate_modules(crate_id)?.1;

    let def_size = usize::from_be_bytes(content[..8].try_into().unwrap()); // def_size is the first 8 bytes of the blob.

    let content = &content[16 + def_size..]; // 16 bytes for both sizes.

    let ((lookups, semantic_lookups, lowerings), _): (LookupCache, _) =
        bincode::serde::decode_from_slice(content, bincode::config::standard()).unwrap_or_else(
            |e| {
                panic!(
                    "failed to deserialize lookup cache for crate `{}`: {e}",
                    crate_id.name(db),
                )
            },
        );

    // TODO(tomer): Fail on version, cfg, and dependencies mismatch.

    let mut ctx = CacheLoadingContext::new(db, lookups, semantic_lookups, def_loading_data);

    Some(
        lowerings
            .into_iter()
            .map(|(function_id, lowering)| {
                let function_id = function_id.embed(&mut ctx.semantic_ctx);

                let lowering = lowering.embed(&mut ctx);
                (function_id, lowering)
            })
            .collect::<OrderedHashMap<_, _>>()
            .into(),
    )
}

/// Cache the lowering of each function in the crate into a blob.
pub fn generate_crate_cache(
    db: &dyn LoweringGroup,
    crate_id: cairo_lang_filesystem::ids::CrateId,
) -> Maybe<Arc<[u8]>> {
    let modules = db.crate_modules(crate_id);
    let mut function_ids = Vec::new();
    for module_id in modules.iter() {
        for free_func in db.module_free_functions_ids(*module_id)?.iter() {
            function_ids.push(FunctionWithBodyId::Free(*free_func));
        }
        for impl_id in db.module_impls_ids(*module_id)?.iter() {
            for impl_func in db.impl_functions(*impl_id)?.values() {
                function_ids.push(FunctionWithBodyId::Impl(*impl_func));
            }
        }
        for trait_id in db.module_traits_ids(*module_id)?.iter() {
            for trait_func in db.trait_functions(*trait_id)?.values() {
                function_ids.push(FunctionWithBodyId::Trait(*trait_func));
            }
        }
        for trait_id in db.module_traits_ids(*module_id)?.iter() {
            for trait_func in db.trait_functions(*trait_id)?.values() {
                function_ids.push(FunctionWithBodyId::Trait(*trait_func));
            }
        }
    }

    let mut ctx = CacheSavingContext::new(db, crate_id);

    let def_cache = generate_crate_def_cache(db, crate_id, &mut ctx.semantic_ctx.defs_ctx)?;

    let cached = function_ids
        .iter()
        .filter_map(|id| {
            db.function_body(*id).ok()?;
            let multi = match lower_semantic_function(db, *id).map(Arc::new) {
                Ok(multi) => multi,
                Err(err) => return Some(Err(err)),
            };

            Some(Ok((
                DefsFunctionWithBodyIdCached::new(*id, &mut ctx.semantic_ctx),
                MultiLoweringCached::new((*multi).clone(), &mut ctx),
            )))
        })
        .collect::<Maybe<Vec<_>>>()?;

    let mut artifact = Vec::<u8>::new();

    if let Ok(def) = bincode::serde::encode_to_vec(
        &(CachedCrateMetadata::new(crate_id, db), def_cache, &ctx.semantic_ctx.defs_ctx.lookups),
        bincode::config::standard(),
    ) {
        artifact.extend(def.len().to_be_bytes());
        artifact.extend(def);
    }

    if let Ok(lowered) = bincode::serde::encode_to_vec(
        &(&ctx.lookups, &ctx.semantic_ctx.lookups, cached),
        bincode::config::standard(),
    ) {
        artifact.extend(lowered.len().to_be_bytes());
        artifact.extend(lowered);
    }
    Ok(Arc::from(artifact.as_slice()))
}

/// Context for loading cache into the database.
struct CacheLoadingContext<'db> {
    /// The variable ids of the flat lowered that is currently being loaded.
    lowered_variables_id: Vec<VariableId>,
    db: &'db dyn LoweringGroup,

    /// data for loading the entire cache into the database.
    data: CacheLoadingData,

    semantic_ctx: SemanticCacheLoadingContext<'db>,
}

impl<'db> CacheLoadingContext<'db> {
    fn new(
        db: &'db dyn LoweringGroup,
        lookups: CacheLookups,
        semantic_lookups: SemanticCacheLookups,
        defs_loading_data: Arc<DefCacheLoadingData>,
    ) -> Self {
        Self {
            lowered_variables_id: Vec::new(),
            db,
            data: CacheLoadingData {
                function_ids: OrderedHashMap::default(),
                location_ids: OrderedHashMap::default(),
                lookups,
            },
            semantic_ctx: SemanticCacheLoadingContext::<'db> {
                db,
                data: SemanticCacheLoadingData::new(semantic_lookups),
                defs_loading_data,
            },
        }
    }
}

impl Deref for CacheLoadingContext<'_> {
    type Target = CacheLoadingData;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}
impl DerefMut for CacheLoadingContext<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.data
    }
}

/// Data for loading cache into the database.
struct CacheLoadingData {
    function_ids: OrderedHashMap<FunctionIdCached, FunctionId>,
    location_ids: OrderedHashMap<LocationIdCached, LocationId>,
    lookups: CacheLookups,
}
impl Deref for CacheLoadingData {
    type Target = CacheLookups;

    fn deref(&self) -> &Self::Target {
        &self.lookups
    }
}
impl DerefMut for CacheLoadingData {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.lookups
    }
}

/// Context for saving cache from the database.
struct CacheSavingContext<'db> {
    db: &'db dyn LoweringGroup,
    data: CacheSavingData,
    semantic_ctx: SemanticCacheSavingContext<'db>,
}
impl Deref for CacheSavingContext<'_> {
    type Target = CacheSavingData;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}
impl DerefMut for CacheSavingContext<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.data
    }
}
impl<'db> CacheSavingContext<'db> {
    fn new(db: &'db dyn LoweringGroup, self_crate_id: CrateId) -> Self {
        Self {
            db,
            data: CacheSavingData::default(),
            semantic_ctx: SemanticCacheSavingContext {
                db,
                data: SemanticCacheSavingData::default(),
                defs_ctx: DefCacheSavingContext::new(db, self_crate_id),
            },
        }
    }
}

/// Data for saving cache from the database.
#[derive(Default)]
struct CacheSavingData {
    function_ids: OrderedHashMap<FunctionId, FunctionIdCached>,
    location_ids: OrderedHashMap<LocationId, LocationIdCached>,
    lookups: CacheLookups,
}
impl Deref for CacheSavingData {
    type Target = CacheLookups;

    fn deref(&self) -> &Self::Target {
        &self.lookups
    }
}
impl DerefMut for CacheSavingData {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.lookups
    }
}

/// Saved interned items for the cache.
#[derive(Serialize, Deserialize, Default)]
struct CacheLookups {
    function_ids_lookup: Vec<FunctionCached>,
    location_ids_lookup: Vec<LocationCached>,
}

/// Context for loading cache into the database.
struct SemanticCacheLoadingContext<'db> {
    db: &'db dyn SemanticGroup,
    data: SemanticCacheLoadingData,
    defs_loading_data: Arc<DefCacheLoadingData>,
}

impl Deref for SemanticCacheLoadingContext<'_> {
    type Target = SemanticCacheLoadingData;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}
impl DerefMut for SemanticCacheLoadingContext<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.data
    }
}

/// Data for loading cache into the database.
struct SemanticCacheLoadingData {
    function_ids: OrderedHashMap<SemanticFunctionIdCached, semantic::FunctionId>,
    type_ids: OrderedHashMap<TypeIdCached, TypeId>,
    impl_ids: OrderedHashMap<ImplIdCached, ImplId>,
    lookups: SemanticCacheLookups,
}

impl SemanticCacheLoadingData {
    fn new(lookups: SemanticCacheLookups) -> Self {
        Self {
            function_ids: OrderedHashMap::default(),
            type_ids: OrderedHashMap::default(),
            impl_ids: OrderedHashMap::default(),
            lookups,
        }
    }
}

impl Deref for SemanticCacheLoadingData {
    type Target = SemanticCacheLookups;

    fn deref(&self) -> &Self::Target {
        &self.lookups
    }
}
impl DerefMut for SemanticCacheLoadingData {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.lookups
    }
}

/// Context for saving cache from the database.
struct SemanticCacheSavingContext<'db> {
    db: &'db dyn SemanticGroup,
    data: SemanticCacheSavingData,
    defs_ctx: DefCacheSavingContext<'db>,
}
impl Deref for SemanticCacheSavingContext<'_> {
    type Target = SemanticCacheSavingData;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}
impl DerefMut for SemanticCacheSavingContext<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.data
    }
}

/// Data for saving cache from the database.
#[derive(Default)]
struct SemanticCacheSavingData {
    function_ids: OrderedHashMap<semantic::FunctionId, SemanticFunctionIdCached>,

    type_ids: OrderedHashMap<TypeId, TypeIdCached>,

    impl_ids: OrderedHashMap<ImplId, ImplIdCached>,

    lookups: SemanticCacheLookups,
}

impl Deref for SemanticCacheSavingData {
    type Target = SemanticCacheLookups;

    fn deref(&self) -> &Self::Target {
        &self.lookups
    }
}
impl DerefMut for SemanticCacheSavingData {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.lookups
    }
}

/// Saved interned items for the cache.
#[derive(Serialize, Deserialize, Default)]
struct SemanticCacheLookups {
    function_ids_lookup: Vec<SemanticFunctionCached>,
    type_ids_lookup: Vec<TypeCached>,
    impl_ids_lookup: Vec<ImplCached>,
}

/// Cached version of [defs::ids::FunctionWithBodyId]
/// used as a key in the cache for the [MultiLowering] struct.
#[derive(Serialize, Deserialize, Hash, Eq, PartialEq)]
enum DefsFunctionWithBodyIdCached {
    Free(LanguageElementCached),
    Impl(LanguageElementCached),
    Trait(LanguageElementCached),
}
impl DefsFunctionWithBodyIdCached {
    fn new(id: defs::ids::FunctionWithBodyId, ctx: &mut SemanticCacheSavingContext<'_>) -> Self {
        match id {
            defs::ids::FunctionWithBodyId::Free(id) => DefsFunctionWithBodyIdCached::Free(
                LanguageElementCached::new(id, &mut ctx.defs_ctx),
            ),
            defs::ids::FunctionWithBodyId::Impl(id) => DefsFunctionWithBodyIdCached::Impl(
                LanguageElementCached::new(id, &mut ctx.defs_ctx),
            ),
            defs::ids::FunctionWithBodyId::Trait(id) => DefsFunctionWithBodyIdCached::Trait(
                LanguageElementCached::new(id, &mut ctx.defs_ctx),
            ),
        }
    }

    fn embed(self, ctx: &mut SemanticCacheLoadingContext<'_>) -> defs::ids::FunctionWithBodyId {
        match self {
            DefsFunctionWithBodyIdCached::Free(id) => {
                let (module_file_id, function_stable_ptr) = id.get_embedded(&ctx.defs_loading_data);
                defs::ids::FunctionWithBodyId::Free(
                    FreeFunctionLongId(module_file_id, FunctionWithBodyPtr(function_stable_ptr))
                        .intern(ctx.db),
                )
            }
            DefsFunctionWithBodyIdCached::Impl(id) => {
                let (module_file_id, function_stable_ptr) = id.get_embedded(&ctx.defs_loading_data);
                defs::ids::FunctionWithBodyId::Impl(
                    ImplFunctionLongId(module_file_id, FunctionWithBodyPtr(function_stable_ptr))
                        .intern(ctx.db),
                )
            }
            DefsFunctionWithBodyIdCached::Trait(id) => {
                let (module_file_id, function_stable_ptr) = id.get_embedded(&ctx.defs_loading_data);
                defs::ids::FunctionWithBodyId::Trait(
                    TraitFunctionLongId(module_file_id, TraitItemFunctionPtr(function_stable_ptr))
                        .intern(ctx.db),
                )
            }
        }
    }
}

/// Cached version of [MultiLowering].
#[derive(Serialize, Deserialize)]
struct MultiLoweringCached {
    main_lowering: LoweredCached,
    generated_lowerings: Vec<(GeneratedFunctionKeyCached, LoweredCached)>,
}
impl MultiLoweringCached {
    fn new(lowering: MultiLowering, ctx: &mut CacheSavingContext<'_>) -> Self {
        Self {
            main_lowering: LoweredCached::new(lowering.main_lowering, ctx),
            generated_lowerings: lowering
                .generated_lowerings
                .into_iter()
                .map(|(key, lowered)| {
                    (GeneratedFunctionKeyCached::new(key, ctx), LoweredCached::new(lowered, ctx))
                })
                .collect(),
        }
    }
    fn embed(self, ctx: &mut CacheLoadingContext<'_>) -> MultiLowering {
        MultiLowering {
            main_lowering: self.main_lowering.embed(ctx),
            generated_lowerings: self
                .generated_lowerings
                .into_iter()
                .map(|(key, lowered)| (key.embed(ctx), lowered.embed(ctx)))
                .collect(),
        }
    }
}

#[derive(Serialize, Deserialize)]
struct LoweredCached {
    /// Function signature.
    signature: SignatureCached,
    /// Arena of allocated lowered variables.
    variables: Vec<VariableCached>,
    /// Arena of allocated lowered blocks.
    blocks: Vec<BlockCached>,
    /// function parameters, including implicits.
    parameters: Vec<usize>,
}
impl LoweredCached {
    fn new(lowered: Lowered, ctx: &mut CacheSavingContext<'_>) -> Self {
        Self {
            signature: SignatureCached::new(lowered.signature, ctx),
            variables: lowered
                .variables
                .into_iter()
                .map(|var| VariableCached::new(var.1, ctx))
                .collect(),
            blocks: lowered
                .blocks
                .into_iter()
                .map(|block: (BlockId, &Block)| BlockCached::new(block.1.clone(), ctx))
                .collect(),
            parameters: lowered.parameters.iter().map(|var| var.index()).collect(),
        }
    }
    fn embed(self, ctx: &mut CacheLoadingContext<'_>) -> Lowered {
        ctx.lowered_variables_id.clear();
        let mut variables = Arena::new();
        for var in self.variables {
            let id = variables.alloc(var.embed(ctx));
            ctx.lowered_variables_id.push(id);
        }

        let mut blocks = BlocksBuilder::new();
        for block in self.blocks {
            blocks.alloc(block.embed(ctx));
        }
        Lowered {
            diagnostics: Default::default(),
            signature: self.signature.embed(ctx),
            variables,
            blocks: blocks.build().unwrap(),
            parameters: self
                .parameters
                .into_iter()
                .map(|var_id| ctx.lowered_variables_id[var_id])
                .collect(),
        }
    }
}

#[derive(Serialize, Deserialize)]
struct SignatureCached {
    /// Function parameters.
    params: Vec<ExprVarMemberPathCached>,
    /// Extra return values.
    extra_rets: Vec<ExprVarMemberPathCached>,
    /// Return type.
    return_type: TypeIdCached,
    /// Implicit parameters.
    implicits: Vec<TypeIdCached>,
    /// Whether the function is panicable.
    panicable: bool,
    location: LocationIdCached,
}
impl SignatureCached {
    fn new(signature: Signature, ctx: &mut CacheSavingContext<'_>) -> Self {
        Self {
            params: signature
                .params
                .into_iter()
                .map(|var| ExprVarMemberPathCached::new(var, &mut ctx.semantic_ctx))
                .collect(),
            extra_rets: signature
                .extra_rets
                .into_iter()
                .map(|var| ExprVarMemberPathCached::new(var, &mut ctx.semantic_ctx))
                .collect(),

            return_type: TypeIdCached::new(signature.return_type, &mut ctx.semantic_ctx),
            implicits: signature
                .implicits
                .into_iter()
                .map(|ty| TypeIdCached::new(ty, &mut ctx.semantic_ctx))
                .collect(),
            panicable: signature.panicable,
            location: LocationIdCached::new(signature.location, ctx),
        }
    }
    fn embed(self, ctx: &mut CacheLoadingContext<'_>) -> Signature {
        Signature {
            params: self.params.into_iter().map(|var| var.embed(&mut ctx.semantic_ctx)).collect(),
            extra_rets: self
                .extra_rets
                .into_iter()
                .map(|var| var.embed(&mut ctx.semantic_ctx))
                .collect(),
            return_type: self.return_type.embed(&mut ctx.semantic_ctx),
            implicits: self
                .implicits
                .into_iter()
                .map(|ty| ty.embed(&mut ctx.semantic_ctx))
                .collect(),
            panicable: self.panicable,
            location: self.location.embed(ctx),
        }
    }
}

#[derive(Serialize, Deserialize)]
enum ExprVarMemberPathCached {
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
    fn new(
        expr_var_member_path: semantic::ExprVarMemberPath,
        ctx: &mut SemanticCacheSavingContext<'_>,
    ) -> Self {
        match expr_var_member_path {
            semantic::ExprVarMemberPath::Var(var) => {
                ExprVarMemberPathCached::Var(ExprVarCached::new(var, ctx))
            }
            semantic::ExprVarMemberPath::Member {
                parent,
                member_id,
                concrete_struct_id,
                stable_ptr,
                ty,
            } => ExprVarMemberPathCached::Member {
                parent: Box::new(ExprVarMemberPathCached::new(*parent, ctx)),
                member_id: LanguageElementCached::new(member_id, &mut ctx.defs_ctx),
                concrete_struct_id: ConcreteStructCached::new(concrete_struct_id, ctx),
                stable_ptr: SyntaxStablePtrIdCached::new(stable_ptr.untyped(), &mut ctx.defs_ctx),
                ty: TypeIdCached::new(ty, ctx),
            },
        }
    }
    fn embed(self, ctx: &mut SemanticCacheLoadingContext<'_>) -> semantic::ExprVarMemberPath {
        match self {
            ExprVarMemberPathCached::Var(var) => semantic::ExprVarMemberPath::Var(var.embed(ctx)),
            ExprVarMemberPathCached::Member {
                parent,
                member_id,
                concrete_struct_id,
                stable_ptr,
                ty,
            } => {
                let parent = Box::new(parent.embed(ctx));
                let (module_file_id, member_stable_ptr) =
                    member_id.get_embedded(&ctx.defs_loading_data);
                let member_id =
                    MemberLongId(module_file_id, MemberPtr(member_stable_ptr)).intern(ctx.db);
                semantic::ExprVarMemberPath::Member {
                    parent,
                    member_id,
                    concrete_struct_id: concrete_struct_id.embed(ctx),
                    stable_ptr: ExprPtr(stable_ptr.get_embedded(&ctx.defs_loading_data)),
                    ty: ty.embed(ctx),
                }
            }
        }
    }
}

#[derive(Serialize, Deserialize)]
struct ExprVarCached {
    var: SemanticVarIdCached,
    /// Variable type.
    ty: TypeIdCached,
    stable_ptr: SyntaxStablePtrIdCached,
}
impl ExprVarCached {
    fn new(expr_var: semantic::ExprVar, ctx: &mut SemanticCacheSavingContext<'_>) -> Self {
        Self {
            var: SemanticVarIdCached::new(expr_var.var, ctx),
            ty: TypeIdCached::new(expr_var.ty, ctx),
            stable_ptr: SyntaxStablePtrIdCached::new(
                expr_var.stable_ptr.untyped(),
                &mut ctx.defs_ctx,
            ),
        }
    }
    fn embed(self, ctx: &mut SemanticCacheLoadingContext<'_>) -> semantic::ExprVar {
        semantic::ExprVar {
            var: self.var.embed(ctx),
            ty: self.ty.embed(ctx),
            stable_ptr: ExprPtr(self.stable_ptr.get_embedded(&ctx.defs_loading_data)),
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
    fn new(var_id: semantic::VarId, ctx: &mut SemanticCacheSavingContext<'_>) -> Self {
        match var_id {
            semantic::VarId::Param(id) => {
                SemanticVarIdCached::Param(SemanticParamIdCached::new(id, ctx))
            }
            semantic::VarId::Local(id) => {
                SemanticVarIdCached::Local(SemanticLocalVarIdCached::new(id, ctx))
            }
            semantic::VarId::Item(id) => {
                SemanticVarIdCached::Item(SemanticStatementItemIdCached::new(id, ctx))
            }
        }
    }
    fn embed(self, ctx: &mut SemanticCacheLoadingContext<'_>) -> semantic::VarId {
        match self {
            SemanticVarIdCached::Param(id) => semantic::VarId::Param(id.embed(ctx)),
            SemanticVarIdCached::Local(id) => semantic::VarId::Local(id.embed(ctx)),
            SemanticVarIdCached::Item(id) => semantic::VarId::Item(id.embed(ctx)),
        }
    }
}

#[derive(Serialize, Deserialize)]
struct SemanticParamIdCached {
    language_element: LanguageElementCached,
}
impl SemanticParamIdCached {
    fn new(param_id: semantic::ParamId, ctx: &mut SemanticCacheSavingContext<'_>) -> Self {
        Self { language_element: LanguageElementCached::new(param_id, &mut ctx.defs_ctx) }
    }
    fn embed(self, ctx: &mut SemanticCacheLoadingContext<'_>) -> semantic::ParamId {
        let (module_id, stable_ptr) = self.language_element.get_embedded(&ctx.defs_loading_data);
        ParamLongId(module_id, ParamPtr(stable_ptr)).intern(ctx.db)
    }
}

#[derive(Serialize, Deserialize)]
struct SemanticLocalVarIdCached {
    language_element: LanguageElementCached,
}
impl SemanticLocalVarIdCached {
    fn new(local_var_id: LocalVarId, ctx: &mut SemanticCacheSavingContext<'_>) -> Self {
        Self { language_element: LanguageElementCached::new(local_var_id, &mut ctx.defs_ctx) }
    }
    fn embed(self, ctx: &mut SemanticCacheLoadingContext<'_>) -> LocalVarId {
        let (module_id, stable_ptr) = self.language_element.get_embedded(&ctx.defs_loading_data);
        LocalVarLongId(module_id, TerminalIdentifierPtr(stable_ptr)).intern(ctx.db)
    }
}

#[derive(Serialize, Deserialize)]
enum SemanticStatementItemIdCached {
    Constant(LanguageElementCached),
    Use(LanguageElementCached),
}

impl SemanticStatementItemIdCached {
    fn new(item_id: StatementItemId, ctx: &mut SemanticCacheSavingContext<'_>) -> Self {
        match item_id {
            StatementItemId::Constant(id) => SemanticStatementItemIdCached::Constant(
                LanguageElementCached::new(id, &mut ctx.defs_ctx),
            ),
            StatementItemId::Use(id) => SemanticStatementItemIdCached::Use(
                LanguageElementCached::new(id, &mut ctx.defs_ctx),
            ),
        }
    }
    fn embed(self, ctx: &mut SemanticCacheLoadingContext<'_>) -> StatementItemId {
        match self {
            SemanticStatementItemIdCached::Constant(id) => {
                let (module_id, stable_ptr) = id.get_embedded(&ctx.defs_loading_data);
                StatementItemId::Constant(
                    StatementConstLongId(module_id, ItemConstantPtr(stable_ptr)).intern(ctx.db),
                )
            }
            SemanticStatementItemIdCached::Use(id) => {
                let (module_id, stable_ptr) = id.get_embedded(&ctx.defs_loading_data);
                StatementItemId::Use(
                    StatementUseLongId(module_id, UsePathLeafPtr(stable_ptr)).intern(ctx.db),
                )
            }
        }
    }
}

#[derive(Serialize, Deserialize)]
struct VariableCached {
    droppable: Option<ImplIdCached>,
    /// Can the type be (trivially) copied.
    copyable: Option<ImplIdCached>,
    /// A Destruct impl for the type, if found.
    destruct_impl: Option<ImplIdCached>,
    /// A PanicDestruct impl for the type, if found.
    panic_destruct_impl: Option<ImplIdCached>,
    /// Semantic type of the variable.
    ty: TypeIdCached,
    location: LocationIdCached,
}
impl VariableCached {
    fn new(variable: Variable, ctx: &mut CacheSavingContext<'_>) -> Self {
        Self {
            droppable: variable
                .droppable
                .map(|impl_id| ImplIdCached::new(impl_id, &mut ctx.semantic_ctx))
                .ok(),
            copyable: variable
                .copyable
                .map(|impl_id| ImplIdCached::new(impl_id, &mut ctx.semantic_ctx))
                .ok(),
            destruct_impl: variable
                .destruct_impl
                .map(|impl_id| ImplIdCached::new(impl_id, &mut ctx.semantic_ctx))
                .ok(),
            panic_destruct_impl: variable
                .panic_destruct_impl
                .map(|impl_id| ImplIdCached::new(impl_id, &mut ctx.semantic_ctx))
                .ok(),
            ty: TypeIdCached::new(variable.ty, &mut ctx.semantic_ctx),
            location: LocationIdCached::new(variable.location, ctx),
        }
    }
    fn embed(self, ctx: &mut CacheLoadingContext<'_>) -> Variable {
        Variable {
            droppable: self
                .droppable
                .map(|impl_id| impl_id.embed(&mut ctx.semantic_ctx))
                .ok_or(InferenceError::Reported(skip_diagnostic())),
            copyable: self
                .copyable
                .map(|impl_id| impl_id.embed(&mut ctx.semantic_ctx))
                .ok_or(InferenceError::Reported(skip_diagnostic())),
            destruct_impl: self
                .destruct_impl
                .map(|impl_id| impl_id.embed(&mut ctx.semantic_ctx))
                .ok_or(InferenceError::Reported(skip_diagnostic())),
            panic_destruct_impl: self
                .panic_destruct_impl
                .map(|impl_id| impl_id.embed(&mut ctx.semantic_ctx))
                .ok_or(InferenceError::Reported(skip_diagnostic())),
            ty: self.ty.embed(&mut ctx.semantic_ctx),
            location: self.location.embed(ctx),
        }
    }
}

#[derive(Serialize, Deserialize)]
struct VarUsageCached {
    /// Variable id.
    var_id: usize,
    /// Location of the usage.
    location: LocationIdCached,
}

impl VarUsageCached {
    fn new(var_usage: VarUsage, ctx: &mut CacheSavingContext<'_>) -> Self {
        Self {
            var_id: var_usage.var_id.index(),
            location: LocationIdCached::new(var_usage.location, ctx),
        }
    }
    fn embed(self, ctx: &mut CacheLoadingContext<'_>) -> VarUsage {
        VarUsage {
            var_id: ctx.lowered_variables_id[self.var_id],
            location: self.location.embed(ctx),
        }
    }
}

#[derive(Serialize, Deserialize)]
struct BlockCached {
    /// Statements in the block.
    statements: Vec<StatementCached>,
    /// Block end.
    end: BlockEndCached,
}
impl BlockCached {
    fn new(block: Block, ctx: &mut CacheSavingContext<'_>) -> Self {
        Self {
            statements: block
                .statements
                .into_iter()
                .map(|stmt| StatementCached::new(stmt, ctx))
                .collect(),
            end: BlockEndCached::new(block.end, ctx),
        }
    }
    fn embed(self, ctx: &mut CacheLoadingContext<'_>) -> Block {
        Block {
            statements: self.statements.into_iter().map(|stmt| stmt.embed(ctx)).collect(),
            end: self.end.embed(ctx),
        }
    }
}
#[derive(Serialize, Deserialize)]
enum BlockEndCached {
    /// The block was created but still needs to be populated. Block must not be in this state in
    /// the end of the lowering phase.
    NotSet,
    /// This block ends with a `return` statement, exiting the function.
    Return(Vec<VarUsageCached>, LocationIdCached),
    /// This block ends with a panic.
    Panic(VarUsageCached),
    /// This block ends with a jump to a different block.
    Goto(usize, VarRemappingCached),
    Match {
        info: MatchInfoCached,
    },
}
impl BlockEndCached {
    fn new(block_end: BlockEnd, ctx: &mut CacheSavingContext<'_>) -> Self {
        match block_end {
            BlockEnd::Return(returns, location) => BlockEndCached::Return(
                returns.iter().map(|var| VarUsageCached::new(*var, ctx)).collect(),
                LocationIdCached::new(location, ctx),
            ),
            BlockEnd::Panic(data) => BlockEndCached::Panic(VarUsageCached::new(data, ctx)),
            BlockEnd::Goto(block_id, remapping) => {
                BlockEndCached::Goto(block_id.0, VarRemappingCached::new(remapping, ctx))
            }
            BlockEnd::NotSet => BlockEndCached::NotSet,
            BlockEnd::Match { info } => {
                BlockEndCached::Match { info: MatchInfoCached::new(info, ctx) }
            }
        }
    }
    fn embed(self, ctx: &mut CacheLoadingContext<'_>) -> BlockEnd {
        match self {
            BlockEndCached::Return(returns, location) => BlockEnd::Return(
                returns.into_iter().map(|var_usage| var_usage.embed(ctx)).collect(),
                location.embed(ctx),
            ),
            BlockEndCached::Panic(var_id) => BlockEnd::Panic(var_id.embed(ctx)),
            BlockEndCached::Goto(block_id, remapping) => {
                BlockEnd::Goto(BlockId(block_id), remapping.embed(ctx))
            }
            BlockEndCached::NotSet => BlockEnd::NotSet,
            BlockEndCached::Match { info } => BlockEnd::Match { info: info.embed(ctx) },
        }
    }
}

#[derive(Serialize, Deserialize)]
struct VarRemappingCached {
    /// Map from new_var to old_var (since new_var cannot appear twice, but old_var can).
    remapping: OrderedHashMap<usize, VarUsageCached>,
}
impl VarRemappingCached {
    fn new(var_remapping: VarRemapping, ctx: &mut CacheSavingContext<'_>) -> Self {
        Self {
            remapping: var_remapping
                .iter()
                .map(|(dst, src)| (dst.index(), VarUsageCached::new(*src, ctx)))
                .collect(),
        }
    }
    fn embed(self, ctx: &mut CacheLoadingContext<'_>) -> VarRemapping {
        let mut remapping = OrderedHashMap::default();
        for (dst, src) in self.remapping {
            remapping.insert(ctx.lowered_variables_id[dst], src.embed(ctx));
        }
        VarRemapping { remapping }
    }
}

#[derive(Serialize, Deserialize)]
enum MatchInfoCached {
    Enum(MatchEnumInfoCached),
    Extern(MatchExternInfoCached),
    Value(MatchEnumValueCached),
}
impl MatchInfoCached {
    fn new(match_info: MatchInfo, ctx: &mut CacheSavingContext<'_>) -> Self {
        match match_info {
            MatchInfo::Enum(info) => MatchInfoCached::Enum(MatchEnumInfoCached::new(info, ctx)),
            MatchInfo::Extern(info) => {
                MatchInfoCached::Extern(MatchExternInfoCached::new(info, ctx))
            }
            MatchInfo::Value(info) => MatchInfoCached::Value(MatchEnumValueCached::new(info, ctx)),
        }
    }
    fn embed(self, ctx: &mut CacheLoadingContext<'_>) -> MatchInfo {
        match self {
            MatchInfoCached::Enum(info) => MatchInfo::Enum(info.embed(ctx)),
            MatchInfoCached::Extern(info) => MatchInfo::Extern(info.embed(ctx)),
            MatchInfoCached::Value(info) => MatchInfo::Value(info.embed(ctx)),
        }
    }
}

#[derive(Serialize, Deserialize)]
struct MatchEnumInfoCached {
    concrete_enum_id: ConcreteEnumCached,
    /// A living variable in current scope to match on.
    input: VarUsageCached,
    /// Match arms. All blocks should have the same rets.
    /// Order must be identical to the order in the definition of the enum.
    arms: Vec<MatchArmCached>,
    location: LocationIdCached,
}
impl MatchEnumInfoCached {
    fn new(match_enum_info: MatchEnumInfo, ctx: &mut CacheSavingContext<'_>) -> Self {
        Self {
            concrete_enum_id: ConcreteEnumCached::new(
                match_enum_info.concrete_enum_id,
                &mut ctx.semantic_ctx,
            ),
            input: VarUsageCached::new(match_enum_info.input, ctx),
            arms: match_enum_info
                .arms
                .into_iter()
                .map(|arm| MatchArmCached::new(arm, ctx))
                .collect(),
            location: LocationIdCached::new(match_enum_info.location, ctx),
        }
    }
    fn embed(self, ctx: &mut CacheLoadingContext<'_>) -> MatchEnumInfo {
        MatchEnumInfo {
            concrete_enum_id: self.concrete_enum_id.embed(&mut ctx.semantic_ctx),
            input: self.input.embed(ctx),
            arms: self.arms.into_iter().map(|arm| arm.embed(ctx)).collect(),
            location: self.location.embed(ctx),
        }
    }
}

#[derive(Serialize, Deserialize)]
struct MatchExternInfoCached {
    /// A concrete external function to call.
    function: FunctionIdCached,
    /// Living variables in current scope to move to the function, as arguments.
    inputs: Vec<VarUsageCached>,
    /// Match arms. All blocks should have the same rets.
    /// Order must be identical to the order in the definition of the enum.
    arms: Vec<MatchArmCached>,
    location: LocationIdCached,
}

impl MatchExternInfoCached {
    fn new(match_extern_info: MatchExternInfo, ctx: &mut CacheSavingContext<'_>) -> Self {
        Self {
            function: FunctionIdCached::new(match_extern_info.function, ctx),
            inputs: match_extern_info
                .inputs
                .iter()
                .map(|var| VarUsageCached::new(*var, ctx))
                .collect(),
            arms: match_extern_info
                .arms
                .into_iter()
                .map(|arm| MatchArmCached::new(arm, ctx))
                .collect(),
            location: LocationIdCached::new(match_extern_info.location, ctx),
        }
    }
    fn embed(self, ctx: &mut CacheLoadingContext<'_>) -> MatchExternInfo {
        MatchExternInfo {
            function: self.function.embed(ctx),
            inputs: self.inputs.into_iter().map(|var_id| var_id.embed(ctx)).collect(),
            arms: self.arms.into_iter().map(|arm| arm.embed(ctx)).collect(),
            location: self.location.embed(ctx),
        }
    }
}

#[derive(Serialize, Deserialize)]
struct MatchEnumValueCached {
    num_of_arms: usize,

    /// A living variable in current scope to match on.
    input: VarUsageCached,
    /// Match arms. All blocks should have the same rets.
    arms: Vec<MatchArmCached>,
    location: LocationIdCached,
}

impl MatchEnumValueCached {
    fn new(match_enum_value: MatchEnumValue, ctx: &mut CacheSavingContext<'_>) -> Self {
        Self {
            num_of_arms: match_enum_value.num_of_arms,
            input: VarUsageCached::new(match_enum_value.input, ctx),
            arms: match_enum_value
                .arms
                .into_iter()
                .map(|arm| MatchArmCached::new(arm, ctx))
                .collect(),
            location: LocationIdCached::new(match_enum_value.location, ctx),
        }
    }
    fn embed(self, ctx: &mut CacheLoadingContext<'_>) -> MatchEnumValue {
        MatchEnumValue {
            num_of_arms: self.num_of_arms,
            input: self.input.embed(ctx),
            arms: self.arms.into_iter().map(|arm| arm.embed(ctx)).collect(),
            location: self.location.embed(ctx),
        }
    }
}
/// An arm of a match statement.
#[derive(Serialize, Deserialize)]
struct MatchArmCached {
    /// The selector of the arm.
    arm_selector: MatchArmSelectorCached,

    /// The block_id where the relevant arm is implemented.
    block_id: usize,

    /// The list of variable ids introduced in this arm.
    var_ids: Vec<usize>,
}

impl MatchArmCached {
    fn new(match_arm: MatchArm, ctx: &mut CacheSavingContext<'_>) -> Self {
        Self {
            arm_selector: MatchArmSelectorCached::new(
                match_arm.arm_selector,
                &mut ctx.semantic_ctx,
            ),
            block_id: match_arm.block_id.0,
            var_ids: match_arm.var_ids.iter().map(|var| var.index()).collect(),
        }
    }
    fn embed(self, ctx: &mut CacheLoadingContext<'_>) -> MatchArm {
        MatchArm {
            arm_selector: self.arm_selector.embed(ctx),
            block_id: BlockId(self.block_id),
            var_ids: self
                .var_ids
                .into_iter()
                .map(|var_id| ctx.lowered_variables_id[var_id])
                .collect(),
        }
    }
}

#[derive(Serialize, Deserialize)]
enum MatchArmSelectorCached {
    VariantId(ConcreteVariantCached),
    Value(usize),
}

impl MatchArmSelectorCached {
    fn new(match_arm_selector: MatchArmSelector, ctx: &mut SemanticCacheSavingContext<'_>) -> Self {
        match match_arm_selector {
            MatchArmSelector::VariantId(variant_id) => {
                MatchArmSelectorCached::VariantId(ConcreteVariantCached::new(variant_id, ctx))
            }
            MatchArmSelector::Value(value) => MatchArmSelectorCached::Value(value.value),
        }
    }
    fn embed(self, ctx: &mut CacheLoadingContext<'_>) -> MatchArmSelector {
        match self {
            MatchArmSelectorCached::VariantId(variant_id) => {
                MatchArmSelector::VariantId(variant_id.embed(&mut ctx.semantic_ctx))
            }
            MatchArmSelectorCached::Value(value) => {
                MatchArmSelector::Value(ValueSelectorArm { value })
            }
        }
    }
}

#[derive(Serialize, Deserialize)]
enum StatementCached {
    // Values.
    Const(StatementConstCached),

    // Flow control.
    Call(StatementCallCached),

    // Structs (including tuples).
    StructConstruct(StatementStructConstructCached),
    StructDestructure(StatementStructDestructureCached),

    // Enums.
    EnumConstruct(StatementEnumConstructCached),

    Snapshot(StatementSnapshotCached),
    Desnap(StatementDesnapCached),
}

impl StatementCached {
    fn new(stmt: Statement, ctx: &mut CacheSavingContext<'_>) -> Self {
        match stmt {
            Statement::Const(stmt) => StatementCached::Const(StatementConstCached::new(stmt, ctx)),
            Statement::Call(stmt) => StatementCached::Call(StatementCallCached::new(stmt, ctx)),
            Statement::StructConstruct(stmt) => {
                StatementCached::StructConstruct(StatementStructConstructCached::new(stmt, ctx))
            }
            Statement::StructDestructure(stmt) => {
                StatementCached::StructDestructure(StatementStructDestructureCached::new(stmt, ctx))
            }
            Statement::EnumConstruct(stmt) => {
                StatementCached::EnumConstruct(StatementEnumConstructCached::new(stmt, ctx))
            }
            Statement::Snapshot(stmt) => {
                StatementCached::Snapshot(StatementSnapshotCached::new(stmt, ctx))
            }
            Statement::Desnap(stmt) => {
                StatementCached::Desnap(StatementDesnapCached::new(stmt, ctx))
            }
        }
    }
    fn embed(self, ctx: &mut CacheLoadingContext<'_>) -> Statement {
        match self {
            StatementCached::Const(stmt) => Statement::Const(stmt.embed(ctx)),
            StatementCached::Call(stmt) => Statement::Call(stmt.embed(ctx)),
            StatementCached::StructConstruct(stmt) => Statement::StructConstruct(stmt.embed(ctx)),
            StatementCached::StructDestructure(stmt) => {
                Statement::StructDestructure(stmt.embed(ctx))
            }
            StatementCached::EnumConstruct(stmt) => Statement::EnumConstruct(stmt.embed(ctx)),
            StatementCached::Snapshot(stmt) => Statement::Snapshot(stmt.embed(ctx)),
            StatementCached::Desnap(stmt) => Statement::Desnap(stmt.embed(ctx)),
        }
    }
}

#[derive(Serialize, Deserialize)]
struct StatementConstCached {
    /// The value of the const.
    value: ConstValueCached,
    /// The variable to bind the value to.
    output: usize,
}
impl StatementConstCached {
    fn new(stmt: StatementConst, ctx: &mut CacheSavingContext<'_>) -> Self {
        Self {
            value: ConstValueCached::new(stmt.value, &mut ctx.semantic_ctx),
            output: stmt.output.index(),
        }
    }
    fn embed(self, ctx: &mut CacheLoadingContext<'_>) -> StatementConst {
        StatementConst {
            value: self.value.embed(&mut ctx.semantic_ctx),
            output: ctx.lowered_variables_id[self.output],
        }
    }
}

#[derive(Serialize, Deserialize, Clone)]
enum ConstValueCached {
    Int(BigInt, TypeIdCached),
    Struct(Vec<ConstValueCached>, TypeIdCached),
    Enum(ConcreteVariantCached, Box<ConstValueCached>),
    NonZero(Box<ConstValueCached>),
    Boxed(Box<ConstValueCached>),
    Generic(GenericParamCached),
    ImplConstant(ImplConstantCached),
}
impl ConstValueCached {
    fn new(const_value_id: ConstValue, ctx: &mut SemanticCacheSavingContext<'_>) -> Self {
        match const_value_id {
            ConstValue::Int(value, ty) => ConstValueCached::Int(value, TypeIdCached::new(ty, ctx)),
            ConstValue::Struct(values, ty) => ConstValueCached::Struct(
                values.into_iter().map(|v| ConstValueCached::new(v, ctx)).collect(),
                TypeIdCached::new(ty, ctx),
            ),
            ConstValue::Enum(variant, value) => ConstValueCached::Enum(
                ConcreteVariantCached::new(variant, ctx),
                Box::new(ConstValueCached::new(*value, ctx)),
            ),
            ConstValue::NonZero(value) => {
                ConstValueCached::NonZero(Box::new(ConstValueCached::new(*value, ctx)))
            }
            ConstValue::Boxed(value) => {
                ConstValueCached::Boxed(Box::new(ConstValueCached::new(*value, ctx)))
            }
            ConstValue::Generic(generic_param) => {
                ConstValueCached::Generic(GenericParamCached::new(generic_param, &mut ctx.defs_ctx))
            }
            ConstValue::ImplConstant(impl_constant_id) => {
                ConstValueCached::ImplConstant(ImplConstantCached::new(impl_constant_id, ctx))
            }
            ConstValue::Var(_, _) | ConstValue::Missing(_) => {
                unreachable!(
                    "Const {:#?} is not supported for caching",
                    const_value_id.debug(ctx.db.elongate())
                )
            }
        }
    }
    fn embed(self, ctx: &mut SemanticCacheLoadingContext<'_>) -> ConstValue {
        match self {
            ConstValueCached::Int(value, ty) => ConstValue::Int(value, ty.embed(ctx)),
            ConstValueCached::Struct(values, ty) => ConstValue::Struct(
                values.into_iter().map(|v| v.embed(ctx)).collect(),
                ty.embed(ctx),
            ),
            ConstValueCached::Enum(variant, value) => {
                ConstValue::Enum(variant.embed(ctx), Box::new(value.embed(ctx)))
            }
            ConstValueCached::NonZero(value) => ConstValue::NonZero(Box::new(value.embed(ctx))),
            ConstValueCached::Boxed(value) => ConstValue::Boxed(Box::new(value.embed(ctx))),
            ConstValueCached::Generic(generic_param) => {
                ConstValue::Generic(generic_param.get_embedded(&ctx.defs_loading_data, ctx.db))
            }
            ConstValueCached::ImplConstant(impl_constant_id) => {
                ConstValue::ImplConstant(impl_constant_id.embed(ctx))
            }
        }
    }
}

#[derive(Serialize, Deserialize, Clone)]
struct ImplConstantCached {
    impl_id: ImplIdCached,
    trait_constant: TraitConstantCached,
}
impl ImplConstantCached {
    fn new(impl_constant_id: ImplConstantId, ctx: &mut SemanticCacheSavingContext<'_>) -> Self {
        Self {
            impl_id: ImplIdCached::new(impl_constant_id.impl_id(), ctx),
            trait_constant: TraitConstantCached::new(impl_constant_id.trait_constant_id(), ctx),
        }
    }
    fn embed(self, ctx: &mut SemanticCacheLoadingContext<'_>) -> ImplConstantId {
        ImplConstantId::new(self.impl_id.embed(ctx), self.trait_constant.embed(ctx), ctx.db)
    }
}

#[derive(Serialize, Deserialize, Clone)]
struct TraitConstantCached {
    language_element: LanguageElementCached,
}
impl TraitConstantCached {
    fn new(trait_constant_id: TraitConstantId, ctx: &mut SemanticCacheSavingContext<'_>) -> Self {
        Self { language_element: LanguageElementCached::new(trait_constant_id, &mut ctx.defs_ctx) }
    }
    fn embed(self, ctx: &mut SemanticCacheLoadingContext<'_>) -> TraitConstantId {
        let (module_id, stable_ptr) = self.language_element.get_embedded(&ctx.defs_loading_data);
        TraitConstantLongId(module_id, TraitItemConstantPtr(stable_ptr)).intern(ctx.db)
    }
}

#[derive(Serialize, Deserialize)]
struct ConstStatementCached {
    /// Value of the constant.
    value: i32,
}

#[derive(Serialize, Deserialize)]
struct StatementCallCached {
    /// A function to "call".
    function: FunctionIdCached,
    /// Living variables in current scope to move to the function, as arguments.
    inputs: Vec<VarUsageCached>,
    /// Is the last input a coupon for the function call. See
    /// [semantic::ExprFunctionCall::coupon_arg] for more information.
    with_coupon: bool,
    /// New variables to be introduced into the current scope from the function outputs.
    outputs: Vec<usize>,
    location: LocationIdCached,
}
impl StatementCallCached {
    fn new(stmt: StatementCall, ctx: &mut CacheSavingContext<'_>) -> Self {
        Self {
            function: FunctionIdCached::new(stmt.function, ctx),
            inputs: stmt.inputs.iter().map(|var| VarUsageCached::new(*var, ctx)).collect(),
            with_coupon: stmt.with_coupon,
            outputs: stmt.outputs.iter().map(|var| var.index()).collect(),
            location: LocationIdCached::new(stmt.location, ctx),
        }
    }
    fn embed(self, ctx: &mut CacheLoadingContext<'_>) -> StatementCall {
        StatementCall {
            function: self.function.embed(ctx),
            inputs: self.inputs.into_iter().map(|var_id| var_id.embed(ctx)).collect(),
            with_coupon: self.with_coupon,
            outputs: self
                .outputs
                .into_iter()
                .map(|var_id| ctx.lowered_variables_id[var_id])
                .collect(),
            location: self.location.embed(ctx),
        }
    }
}

#[derive(Serialize, Deserialize, Clone)]
enum FunctionCached {
    /// An original function from the user code.
    Semantic(SemanticFunctionIdCached),
    /// A function generated by the compiler.
    Generated(GeneratedFunctionCached),
}
impl FunctionCached {
    fn new(function: FunctionLongId, ctx: &mut CacheSavingContext<'_>) -> Self {
        match function {
            FunctionLongId::Semantic(id) => {
                FunctionCached::Semantic(SemanticFunctionIdCached::new(id, &mut ctx.semantic_ctx))
            }
            FunctionLongId::Generated(id) => {
                FunctionCached::Generated(GeneratedFunctionCached::new(id, ctx))
            }
            FunctionLongId::Specialized(_) => {
                unreachable!("Specialization of functions only occurs post concretization.")
            }
        }
    }
    fn embed(self, ctx: &mut CacheLoadingContext<'_>) -> FunctionId {
        match self {
            FunctionCached::Semantic(id) => {
                FunctionLongId::Semantic(id.embed(&mut ctx.semantic_ctx))
            }
            FunctionCached::Generated(id) => FunctionLongId::Generated(id.embed(ctx)),
        }
        .intern(ctx.db)
    }
}

#[derive(Serialize, Deserialize, Copy, Clone, PartialEq, Eq, Hash)]
struct FunctionIdCached(usize);
impl FunctionIdCached {
    fn new(function_id: FunctionId, ctx: &mut CacheSavingContext<'_>) -> Self {
        if let Some(id) = ctx.function_ids.get(&function_id) {
            return *id;
        }
        let function = FunctionCached::new(function_id.lookup_intern(ctx.db), ctx);
        let id = FunctionIdCached(ctx.function_ids_lookup.len());
        ctx.function_ids_lookup.push(function);
        ctx.function_ids.insert(function_id, id);
        id
    }
    fn embed(self, ctx: &mut CacheLoadingContext<'_>) -> FunctionId {
        if let Some(function_id) = ctx.function_ids.get(&self) {
            return *function_id;
        }

        let function = ctx.function_ids_lookup[self.0].clone();
        let function_id = function.embed(ctx);
        ctx.function_ids.insert(self, function_id);
        function_id
    }
}

#[derive(Serialize, Deserialize, Clone)]
struct SemanticFunctionCached {
    generic_function: GenericFunctionCached,

    generic_args: Vec<GenericArgumentCached>,
}
impl SemanticFunctionCached {
    fn new(
        function_id: semantic::FunctionLongId,
        ctx: &mut SemanticCacheSavingContext<'_>,
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
    fn embed(self, ctx: &mut SemanticCacheLoadingContext<'_>) -> semantic::FunctionLongId {
        semantic::FunctionLongId {
            function: ConcreteFunction {
                generic_function: self.generic_function.embed(ctx),
                generic_args: self.generic_args.into_iter().map(|arg| arg.embed(ctx)).collect(),
            },
        }
    }
}
#[derive(Serialize, Deserialize, Copy, Clone, PartialEq, Eq, Hash)]
struct SemanticFunctionIdCached(usize);
impl SemanticFunctionIdCached {
    fn new(function_id: semantic::FunctionId, ctx: &mut SemanticCacheSavingContext<'_>) -> Self {
        if let Some(id) = ctx.function_ids.get(&function_id) {
            return *id;
        }
        let function = SemanticFunctionCached::new(function_id.lookup_intern(ctx.db), ctx);
        let id = SemanticFunctionIdCached(ctx.function_ids_lookup.len());
        ctx.function_ids_lookup.push(function);
        ctx.function_ids.insert(function_id, id);
        id
    }
    fn embed(self, ctx: &mut SemanticCacheLoadingContext<'_>) -> semantic::FunctionId {
        if let Some(function_id) = ctx.function_ids.get(&self) {
            return *function_id;
        }

        let function = ctx.function_ids_lookup[self.0].clone();
        let function_id = function.embed(ctx).intern(ctx.db);
        ctx.function_ids.insert(self, function_id);
        function_id
    }
}

#[derive(Serialize, Deserialize, Clone)]
enum GenericFunctionCached {
    /// A generic free function.
    Free(LanguageElementCached),
    /// A generic extern function.
    Extern(LanguageElementCached),
    /// A generic function of an impl.
    Impl(ImplIdCached, LanguageElementCached),
}
impl GenericFunctionCached {
    fn new(generic_function: GenericFunctionId, ctx: &mut SemanticCacheSavingContext<'_>) -> Self {
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
    fn embed(self, ctx: &mut SemanticCacheLoadingContext<'_>) -> GenericFunctionId {
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
                let (module_file_id, stable_ptr) = name.get_embedded(&ctx.defs_loading_data);
                let trait_function_id =
                    TraitFunctionLongId(module_file_id, TraitItemFunctionPtr(stable_ptr))
                        .intern(ctx.db);

                GenericFunctionId::Impl(ImplGenericFunctionId {
                    impl_id,
                    function: trait_function_id,
                })
            }
        }
    }
}

#[derive(Serialize, Deserialize, Clone)]
struct GeneratedFunctionCached {
    parent: SemanticConcreteFunctionWithBodyCached,
    key: GeneratedFunctionKeyCached,
}
impl GeneratedFunctionCached {
    fn new(function: GeneratedFunction, ctx: &mut CacheSavingContext<'_>) -> Self {
        Self {
            parent: SemanticConcreteFunctionWithBodyCached::new(
                function.parent,
                &mut ctx.semantic_ctx,
            ),
            key: GeneratedFunctionKeyCached::new(function.key, ctx),
        }
    }
    fn embed(self, ctx: &mut CacheLoadingContext<'_>) -> GeneratedFunction {
        GeneratedFunction {
            parent: self.parent.embed(&mut ctx.semantic_ctx),
            key: self.key.embed(ctx),
        }
    }
}
#[derive(Serialize, Deserialize, Clone)]
struct SemanticConcreteFunctionWithBodyCached {
    generic_function: GenericFunctionWithBodyCached,
    generic_args: Vec<GenericArgumentCached>,
}
impl SemanticConcreteFunctionWithBodyCached {
    fn new(
        function_id: semantic::ConcreteFunctionWithBodyId,
        ctx: &mut SemanticCacheSavingContext<'_>,
    ) -> Self {
        Self {
            generic_function: GenericFunctionWithBodyCached::new(
                function_id.generic_function(ctx.db),
                ctx,
            ),
            generic_args: function_id
                .lookup_intern(ctx.db)
                .generic_args
                .into_iter()
                .map(|arg| GenericArgumentCached::new(arg, ctx))
                .collect(),
        }
    }
    fn embed(
        self,
        ctx: &mut SemanticCacheLoadingContext<'_>,
    ) -> semantic::ConcreteFunctionWithBodyId {
        let generic_function = self.generic_function.embed(ctx);
        let generic_args = self.generic_args.into_iter().map(|arg| arg.embed(ctx)).collect();
        ConcreteFunctionWithBody { generic_function, generic_args }.intern(ctx.db)
    }
}

#[derive(Serialize, Deserialize, Clone)]
enum GenericFunctionWithBodyCached {
    Free(LanguageElementCached),
    Impl(ConcreteImplCached, ImplFunctionBodyCached),
    Trait(ConcreteTraitCached, LanguageElementCached),
}

impl GenericFunctionWithBodyCached {
    fn new(
        generic_function: GenericFunctionWithBodyId,
        ctx: &mut SemanticCacheSavingContext<'_>,
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
    fn embed(self, ctx: &mut SemanticCacheLoadingContext<'_>) -> GenericFunctionWithBodyId {
        match self {
            GenericFunctionWithBodyCached::Free(id) => {
                let (module_id, stable_ptr) = id.get_embedded(&ctx.defs_loading_data);
                let id =
                    FreeFunctionLongId(module_id, FunctionWithBodyPtr(stable_ptr)).intern(ctx.db);
                GenericFunctionWithBodyId::Free(id)
            }
            GenericFunctionWithBodyCached::Impl(id, function_body) => {
                // todo handle trait functions
                GenericFunctionWithBodyId::Impl(ImplGenericFunctionWithBodyId {
                    concrete_impl_id: id.embed(ctx),
                    function_body: function_body.embed(ctx),
                })
            }
            GenericFunctionWithBodyCached::Trait(id, name) => {
                let concrete_trait_id = id.embed(ctx);
                let (module_file_id, stable_ptr) = name.get_embedded(&ctx.defs_loading_data);
                let trait_function_id =
                    TraitFunctionLongId(module_file_id, TraitItemFunctionPtr(stable_ptr))
                        .intern(ctx.db);

                GenericFunctionWithBodyId::Trait(
                    ConcreteTraitGenericFunctionLongId::new(
                        ctx.db,
                        concrete_trait_id,
                        trait_function_id,
                    )
                    .intern(ctx.db),
                )
            }
        }
    }
}

#[derive(Serialize, Deserialize, Clone)]
enum ImplFunctionBodyCached {
    /// A function that was implemented in the impl.
    Impl(LanguageElementCached),
    /// The default implementation of a trait function in the trait.
    Trait(LanguageElementCached),
}
impl ImplFunctionBodyCached {
    fn new(function_body: ImplFunctionBodyId, ctx: &mut SemanticCacheSavingContext<'_>) -> Self {
        match function_body {
            ImplFunctionBodyId::Impl(id) => {
                ImplFunctionBodyCached::Impl(LanguageElementCached::new(id, &mut ctx.defs_ctx))
            }
            ImplFunctionBodyId::Trait(id) => {
                ImplFunctionBodyCached::Trait(LanguageElementCached::new(id, &mut ctx.defs_ctx))
            }
        }
    }
    fn embed(self, ctx: &mut SemanticCacheLoadingContext<'_>) -> ImplFunctionBodyId {
        match self {
            ImplFunctionBodyCached::Impl(id) => {
                let (module_file_id, stable_ptr) = id.get_embedded(&ctx.defs_loading_data);
                ImplFunctionBodyId::Impl(
                    ImplFunctionLongId(module_file_id, FunctionWithBodyPtr(stable_ptr))
                        .intern(ctx.db),
                )
            }
            ImplFunctionBodyCached::Trait(id) => {
                let (module_file_id, stable_ptr) = id.get_embedded(&ctx.defs_loading_data);
                ImplFunctionBodyId::Trait(
                    TraitFunctionLongId(module_file_id, TraitItemFunctionPtr(stable_ptr))
                        .intern(ctx.db),
                )
            }
        }
    }
}

#[derive(Serialize, Deserialize, Clone, Hash, PartialEq, Eq)]
enum GeneratedFunctionKeyCached {
    Loop(SyntaxStablePtrIdCached),
    TraitFunc(LanguageElementCached, SyntaxStablePtrIdCached),
}

impl GeneratedFunctionKeyCached {
    fn new(key: GeneratedFunctionKey, ctx: &mut CacheSavingContext<'_>) -> Self {
        match key {
            GeneratedFunctionKey::Loop(id) => GeneratedFunctionKeyCached::Loop(
                SyntaxStablePtrIdCached::new(id.untyped(), &mut ctx.semantic_ctx.defs_ctx),
            ),
            GeneratedFunctionKey::TraitFunc(id, stable_location) => {
                GeneratedFunctionKeyCached::TraitFunc(
                    LanguageElementCached::new(id, &mut ctx.semantic_ctx.defs_ctx),
                    SyntaxStablePtrIdCached::new(
                        stable_location.stable_ptr(),
                        &mut ctx.semantic_ctx.defs_ctx,
                    ),
                )
            }
        }
    }
    fn embed(self, ctx: &mut CacheLoadingContext<'_>) -> GeneratedFunctionKey {
        match self {
            GeneratedFunctionKeyCached::Loop(id) => GeneratedFunctionKey::Loop(ExprPtr(
                id.get_embedded(&ctx.semantic_ctx.defs_loading_data),
            )),
            GeneratedFunctionKeyCached::TraitFunc(id, stable_location) => {
                let (module_file_id, stable_ptr) =
                    id.get_embedded(&ctx.semantic_ctx.defs_loading_data);
                GeneratedFunctionKey::TraitFunc(
                    TraitFunctionLongId(module_file_id, TraitItemFunctionPtr(stable_ptr))
                        .intern(ctx.db),
                    StableLocation::new(
                        stable_location.get_embedded(&ctx.semantic_ctx.defs_loading_data),
                    ),
                )
            }
        }
    }
}

#[derive(Serialize, Deserialize)]
struct StatementStructConstructCached {
    inputs: Vec<VarUsageCached>,
    /// The variable to bind the value to.
    output: usize,
}
impl StatementStructConstructCached {
    fn new(stmt: StatementStructConstruct, _ctx: &mut CacheSavingContext<'_>) -> Self {
        Self {
            inputs: stmt.inputs.iter().map(|var| VarUsageCached::new(*var, _ctx)).collect(),
            output: stmt.output.index(),
        }
    }
    fn embed(self, ctx: &mut CacheLoadingContext<'_>) -> StatementStructConstruct {
        StatementStructConstruct {
            inputs: self.inputs.into_iter().map(|var_id| var_id.embed(ctx)).collect(),
            output: ctx.lowered_variables_id[self.output],
        }
    }
}
#[derive(Serialize, Deserialize)]
struct StatementStructDestructureCached {
    /// A living variable in current scope to destructure.
    input: VarUsageCached,
    /// The variables to bind values to.
    outputs: Vec<usize>,
}
impl StatementStructDestructureCached {
    fn new(stmt: StatementStructDestructure, _ctx: &mut CacheSavingContext<'_>) -> Self {
        Self {
            input: VarUsageCached::new(stmt.input, _ctx),
            outputs: stmt.outputs.iter().map(|var| var.index()).collect(),
        }
    }
    fn embed(self, ctx: &mut CacheLoadingContext<'_>) -> StatementStructDestructure {
        StatementStructDestructure {
            input: self.input.embed(ctx),
            outputs: self
                .outputs
                .into_iter()
                .map(|var_id| ctx.lowered_variables_id[var_id])
                .collect(),
        }
    }
}

#[derive(Serialize, Deserialize)]
struct StatementEnumConstructCached {
    variant: ConcreteVariantCached,
    /// A living variable in current scope to wrap with the variant.
    input: VarUsageCached,
    /// The variable to bind the value to.
    output: usize,
}
impl StatementEnumConstructCached {
    fn new(stmt: StatementEnumConstruct, ctx: &mut CacheSavingContext<'_>) -> Self {
        Self {
            variant: ConcreteVariantCached::new(stmt.variant, &mut ctx.semantic_ctx),
            input: VarUsageCached::new(stmt.input, ctx),
            output: stmt.output.index(),
        }
    }
    fn embed(self, ctx: &mut CacheLoadingContext<'_>) -> StatementEnumConstruct {
        StatementEnumConstruct {
            variant: self.variant.embed(&mut ctx.semantic_ctx),
            input: self.input.embed(ctx),
            output: ctx.lowered_variables_id[self.output],
        }
    }
}

#[derive(Serialize, Deserialize)]
struct StatementSnapshotCached {
    input: VarUsageCached,
    outputs: [usize; 2],
}
impl StatementSnapshotCached {
    fn new(stmt: StatementSnapshot, _ctx: &mut CacheSavingContext<'_>) -> Self {
        Self {
            input: VarUsageCached::new(stmt.input, _ctx),
            outputs: stmt.outputs.map(|var| var.index()),
        }
    }
    fn embed(self, ctx: &mut CacheLoadingContext<'_>) -> StatementSnapshot {
        StatementSnapshot {
            input: self.input.embed(ctx),
            outputs: [
                ctx.lowered_variables_id[self.outputs[0]],
                ctx.lowered_variables_id[self.outputs[1]],
            ],
        }
    }
}

#[derive(Serialize, Deserialize)]
struct StatementDesnapCached {
    input: VarUsageCached,
    /// The variable to bind the value to.
    output: usize,
}
impl StatementDesnapCached {
    fn new(stmt: StatementDesnap, ctx: &mut CacheSavingContext<'_>) -> Self {
        Self { input: VarUsageCached::new(stmt.input, ctx), output: stmt.output.index() }
    }
    fn embed(self, ctx: &mut CacheLoadingContext<'_>) -> StatementDesnap {
        StatementDesnap {
            input: self.input.embed(ctx),
            output: ctx.lowered_variables_id[self.output],
        }
    }
}

#[derive(Serialize, Deserialize, Clone)]
enum GenericArgumentCached {
    Type(TypeIdCached),
    Value(ConstValueCached),
    Impl(ImplIdCached),
    NegImpl,
}

impl GenericArgumentCached {
    fn new(
        generic_argument_id: semantic::GenericArgumentId,
        ctx: &mut SemanticCacheSavingContext<'_>,
    ) -> Self {
        match generic_argument_id {
            semantic::GenericArgumentId::Type(type_id) => {
                GenericArgumentCached::Type(TypeIdCached::new(type_id, ctx))
            }
            semantic::GenericArgumentId::Constant(const_value_id) => {
                GenericArgumentCached::Value(ConstValueCached::new(
                    const_value_id.lookup_intern(ctx.db), // todo intern
                    ctx,
                ))
            }
            semantic::GenericArgumentId::Impl(impl_id) => {
                GenericArgumentCached::Impl(ImplIdCached::new(impl_id, ctx))
            }
            semantic::GenericArgumentId::NegImpl => GenericArgumentCached::NegImpl,
        }
    }
    fn embed(self, ctx: &mut SemanticCacheLoadingContext<'_>) -> semantic::GenericArgumentId {
        match self {
            GenericArgumentCached::Type(ty) => semantic::GenericArgumentId::Type(ty.embed(ctx)),
            GenericArgumentCached::Value(value) => {
                semantic::GenericArgumentId::Constant(value.embed(ctx).intern(ctx.db))
            }
            GenericArgumentCached::Impl(imp) => semantic::GenericArgumentId::Impl(imp.embed(ctx)),
            GenericArgumentCached::NegImpl => semantic::GenericArgumentId::NegImpl,
        }
    }
}

#[derive(Serialize, Deserialize, Clone)]
enum TypeCached {
    Concrete(ConcreteTypeCached),
    /// Some expressions might have invalid types during processing, either due to errors or
    /// during inference.
    Tuple(Vec<TypeIdCached>),
    Snapshot(Box<TypeIdCached>),
    GenericParameter(GenericParamCached),
    ImplType(ImplTypeCached),
    FixedSizeArray(TypeIdCached, ConstValueCached),
    ClosureType(ClosureTypeCached),
}

impl TypeCached {
    fn new(type_id: TypeLongId, ctx: &mut SemanticCacheSavingContext<'_>) -> Self {
        match type_id {
            semantic::TypeLongId::Concrete(concrete_type_id) => {
                TypeCached::Concrete(ConcreteTypeCached::new(concrete_type_id, ctx))
            }
            semantic::TypeLongId::Tuple(vec) => {
                TypeCached::Tuple(vec.into_iter().map(|ty| TypeIdCached::new(ty, ctx)).collect())
            }
            semantic::TypeLongId::Snapshot(type_id) => {
                TypeCached::Snapshot(Box::new(TypeIdCached::new(type_id, ctx)))
            }
            semantic::TypeLongId::GenericParameter(generic_param_id) => {
                TypeCached::GenericParameter(GenericParamCached::new(
                    generic_param_id,
                    &mut ctx.defs_ctx,
                ))
            }
            semantic::TypeLongId::ImplType(impl_type_id) => {
                TypeCached::ImplType(ImplTypeCached::new(impl_type_id, ctx))
            }
            semantic::TypeLongId::FixedSizeArray { type_id, size } => TypeCached::FixedSizeArray(
                TypeIdCached::new(type_id, ctx),
                ConstValueCached::new(size.lookup_intern(ctx.db), ctx),
            ),
            TypeLongId::Closure(closure_ty) => {
                TypeCached::ClosureType(ClosureTypeCached::new(closure_ty, ctx))
            }
            TypeLongId::Var(_) | TypeLongId::Missing(_) | TypeLongId::Coupon(_) => {
                unreachable!(
                    "type {:?} is not supported for caching",
                    type_id.debug(ctx.db.elongate())
                )
            }
        }
    }
    fn embed(self, ctx: &mut SemanticCacheLoadingContext<'_>) -> TypeLongId {
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

#[derive(Serialize, Deserialize, Copy, Clone, PartialEq, Eq, Hash)]
struct TypeIdCached(usize);

impl TypeIdCached {
    fn new(ty: TypeId, ctx: &mut SemanticCacheSavingContext<'_>) -> Self {
        if let Some(id) = ctx.type_ids.get(&ty) {
            return *id;
        }
        let ty_long = TypeCached::new(ty.lookup_intern(ctx.db), ctx);
        let id = TypeIdCached(ctx.type_ids_lookup.len());
        ctx.type_ids_lookup.push(ty_long);
        ctx.type_ids.insert(ty, id);
        id
    }
    fn embed(self, ctx: &mut SemanticCacheLoadingContext<'_>) -> TypeId {
        if let Some(type_id) = ctx.type_ids.get(&self) {
            return *type_id;
        }

        let ty = ctx.type_ids_lookup[self.0].clone();
        let ty = ty.embed(ctx).intern(ctx.db);
        ctx.type_ids.insert(self, ty);
        ty
    }
}

#[derive(Serialize, Deserialize, Clone)]
enum ConcreteTypeCached {
    Struct(ConcreteStructCached),
    Enum(ConcreteEnumCached),
    Extern(ConcreteExternTypeCached),
}

impl ConcreteTypeCached {
    fn new(
        concrete_type_id: semantic::ConcreteTypeId,
        ctx: &mut SemanticCacheSavingContext<'_>,
    ) -> Self {
        match concrete_type_id {
            semantic::ConcreteTypeId::Struct(id) => {
                ConcreteTypeCached::Struct(ConcreteStructCached::new(id, ctx))
            }
            semantic::ConcreteTypeId::Enum(id) => {
                ConcreteTypeCached::Enum(ConcreteEnumCached::new(id, ctx))
            }
            semantic::ConcreteTypeId::Extern(id) => {
                ConcreteTypeCached::Extern(ConcreteExternTypeCached::new(id, ctx))
            }
        }
    }
    fn embed(self, ctx: &mut SemanticCacheLoadingContext<'_>) -> semantic::ConcreteTypeId {
        match self {
            ConcreteTypeCached::Struct(s) => semantic::ConcreteTypeId::Struct(s.embed(ctx)),
            ConcreteTypeCached::Enum(e) => semantic::ConcreteTypeId::Enum(e.embed(ctx)),
            ConcreteTypeCached::Extern(e) => semantic::ConcreteTypeId::Extern(e.embed(ctx)),
        }
    }
}

#[derive(Serialize, Deserialize, Clone)]
struct ImplTypeCached {
    impl_id: ImplIdCached,
    trait_type: TraitTypeCached,
}
impl ImplTypeCached {
    fn new(impl_type_id: ImplTypeId, ctx: &mut SemanticCacheSavingContext<'_>) -> Self {
        Self {
            impl_id: ImplIdCached::new(impl_type_id.impl_id(), ctx),
            trait_type: TraitTypeCached::new(impl_type_id.ty(), ctx),
        }
    }
    fn embed(self, ctx: &mut SemanticCacheLoadingContext<'_>) -> ImplTypeId {
        let impl_id = self.impl_id.embed(ctx);
        let ty = self.trait_type.embed(ctx);
        ImplTypeId::new(impl_id, ty, ctx.db)
    }
}
#[derive(Serialize, Deserialize, Clone)]
struct ClosureTypeCached {
    param_tys: Vec<TypeIdCached>,
    ret_ty: TypeIdCached,
    captured_types: Vec<TypeIdCached>,
    parent_function: SemanticFunctionIdCached,
    wrapper_location: SyntaxStablePtrIdCached,
}

impl ClosureTypeCached {
    fn new(closure_type_id: ClosureTypeLongId, ctx: &mut SemanticCacheSavingContext<'_>) -> Self {
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
    fn embed(self, ctx: &mut SemanticCacheLoadingContext<'_>) -> ClosureTypeLongId {
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
    fn new(trait_type_id: TraitTypeId, ctx: &mut SemanticCacheSavingContext<'_>) -> Self {
        Self { language_element: LanguageElementCached::new(trait_type_id, &mut ctx.defs_ctx) }
    }
    fn embed(self, ctx: &mut SemanticCacheLoadingContext<'_>) -> TraitTypeId {
        let (module_file_id, stable_ptr) =
            self.language_element.get_embedded(&ctx.defs_loading_data);
        TraitTypeLongId(module_file_id, TraitItemTypePtr(stable_ptr)).intern(ctx.db)
    }
}

#[derive(Serialize, Deserialize, Clone)]
enum ImplCached {
    Concrete(ConcreteImplCached),
    GenericParameter(GenericParamCached),
    ImplImpl(ImplImplCached),
    GeneratedImpl(GeneratedImplCached),
    SelfImpl(ConcreteTraitCached),
}
impl ImplCached {
    fn new(impl_id: ImplLongId, ctx: &mut SemanticCacheSavingContext<'_>) -> Self {
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
                unreachable!(
                    "impl {:?} is not supported for caching",
                    impl_id.debug(ctx.db.elongate())
                )
            }
        }
    }
    fn embed(self, ctx: &mut SemanticCacheLoadingContext<'_>) -> ImplLongId {
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
#[derive(Serialize, Deserialize, Copy, Clone, PartialEq, Eq, Hash)]
struct ImplIdCached(usize);

impl ImplIdCached {
    fn new(impl_id: ImplId, ctx: &mut SemanticCacheSavingContext<'_>) -> Self {
        if let Some(id) = ctx.impl_ids.get(&impl_id) {
            return *id;
        }
        let imp = ImplCached::new(impl_id.lookup_intern(ctx.db), ctx);
        let id = ImplIdCached(ctx.impl_ids_lookup.len());
        ctx.impl_ids_lookup.push(imp);
        ctx.impl_ids.insert(impl_id, id);
        id
    }
    fn embed(self, ctx: &mut SemanticCacheLoadingContext<'_>) -> ImplId {
        if let Some(impl_id) = ctx.impl_ids.get(&self) {
            return *impl_id;
        }

        let imp = ctx.impl_ids_lookup[self.0].clone();
        let imp = imp.embed(ctx).intern(ctx.db);
        ctx.impl_ids.insert(self, imp);
        imp
    }
}

#[derive(Serialize, Deserialize, Clone)]
struct ConcreteImplCached {
    impl_def_id: ImplDefIdCached,
    generic_args: Vec<GenericArgumentCached>,
}
impl ConcreteImplCached {
    fn new(
        concrete_impl: semantic::ConcreteImplId,
        ctx: &mut SemanticCacheSavingContext<'_>,
    ) -> Self {
        let long_id = concrete_impl.lookup_intern(ctx.db);
        Self {
            impl_def_id: ImplDefIdCached::new(long_id.impl_def_id, &mut ctx.defs_ctx),
            generic_args: long_id
                .generic_args
                .into_iter()
                .map(|arg| GenericArgumentCached::new(arg, ctx))
                .collect(),
        }
    }
    fn embed(self, ctx: &mut SemanticCacheLoadingContext<'_>) -> semantic::ConcreteImplId {
        let impl_def_id = self.impl_def_id.get_embedded(&ctx.defs_loading_data);
        let long_id = ConcreteImplLongId {
            impl_def_id,
            generic_args: self.generic_args.into_iter().map(|arg| arg.embed(ctx)).collect(),
        };
        long_id.intern(ctx.db)
    }
}

#[derive(Serialize, Deserialize, Clone)]
struct ImplImplCached {
    impl_id: ImplIdCached,
    trait_impl_id: TraitImplCached,
}
impl ImplImplCached {
    fn new(impl_impl_id: ImplImplId, ctx: &mut SemanticCacheSavingContext<'_>) -> Self {
        Self {
            impl_id: ImplIdCached::new(impl_impl_id.impl_id(), ctx),
            trait_impl_id: TraitImplCached::new(impl_impl_id.trait_impl_id(), ctx),
        }
    }
    fn embed(self, ctx: &mut SemanticCacheLoadingContext<'_>) -> ImplImplId {
        let impl_id = self.impl_id.embed(ctx);
        let trait_impl_id = self.trait_impl_id.embed(ctx);
        ImplImplId::new(impl_id, trait_impl_id, ctx.db)
    }
}

#[derive(Serialize, Deserialize, Clone)]
struct TraitImplCached {
    language_element: LanguageElementCached,
}
impl TraitImplCached {
    fn new(trait_impl_id: TraitImplId, ctx: &mut SemanticCacheSavingContext<'_>) -> Self {
        Self { language_element: LanguageElementCached::new(trait_impl_id, &mut ctx.defs_ctx) }
    }
    fn embed(self, ctx: &mut SemanticCacheLoadingContext<'_>) -> TraitImplId {
        let (module_file_id, stable_ptr) =
            self.language_element.get_embedded(&ctx.defs_loading_data);
        TraitImplLongId(module_file_id, TraitItemImplPtr(stable_ptr)).intern(ctx.db)
    }
}

#[derive(Serialize, Deserialize, Clone)]
struct GeneratedImplCached {
    pub concrete_trait: ConcreteTraitCached,
    /// The generic params required for the impl. Typically impls and negative impls.
    /// We save the params so that we can validate negative impls.
    pub generic_params: Vec<SemanticGenericParamCached>,
    pub impl_items: OrderedHashMap<TraitTypeCached, TypeIdCached>,
}
impl GeneratedImplCached {
    fn new(generated_impl: GeneratedImplId, ctx: &mut SemanticCacheSavingContext<'_>) -> Self {
        let generated_impl = generated_impl.lookup_intern(ctx.db);
        Self {
            concrete_trait: ConcreteTraitCached::new(generated_impl.concrete_trait, ctx),
            generic_params: generated_impl
                .generic_params
                .into_iter()
                .map(|param| SemanticGenericParamCached::new(param, ctx))
                .collect(),
            impl_items: generated_impl
                .impl_items
                .0
                .into_iter()
                .map(|(k, v)| (TraitTypeCached::new(k, ctx), TypeIdCached::new(v, ctx)))
                .collect(),
        }
    }
    fn embed(self, ctx: &mut SemanticCacheLoadingContext<'_>) -> GeneratedImplId {
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

#[derive(Serialize, Deserialize, Clone)]
enum SemanticGenericParamCached {
    Type(GenericParamTypeCached),
    Const(GenericParamConstCached),
    Impl(GenericParamImplCached),
    NegImpl(GenericParamImplCached),
}
impl SemanticGenericParamCached {
    fn new(generic_param_id: GenericParam, ctx: &mut SemanticCacheSavingContext<'_>) -> Self {
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
    fn embed(self, ctx: &mut SemanticCacheLoadingContext<'_>) -> GenericParam {
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

#[derive(Serialize, Deserialize, Clone)]
struct GenericParamTypeCached {
    id: GenericParamCached,
}

impl GenericParamTypeCached {
    fn new(generic_param: GenericParamType, ctx: &mut SemanticCacheSavingContext<'_>) -> Self {
        Self { id: GenericParamCached::new(generic_param.id, &mut ctx.defs_ctx) }
    }
    fn embed(self, ctx: &mut SemanticCacheLoadingContext<'_>) -> GenericParamType {
        GenericParamType { id: self.id.get_embedded(&ctx.defs_loading_data, ctx.db) }
    }
}

#[derive(Serialize, Deserialize, Clone)]
struct GenericParamConstCached {
    id: GenericParamCached,
    ty: TypeIdCached,
}

impl GenericParamConstCached {
    fn new(generic_param: GenericParamConst, ctx: &mut SemanticCacheSavingContext<'_>) -> Self {
        Self {
            id: GenericParamCached::new(generic_param.id, &mut ctx.defs_ctx),
            ty: TypeIdCached::new(generic_param.ty, ctx),
        }
    }
    fn embed(self, ctx: &mut SemanticCacheLoadingContext<'_>) -> GenericParamConst {
        GenericParamConst {
            id: self.id.get_embedded(&ctx.defs_loading_data, ctx.db),
            ty: self.ty.embed(ctx),
        }
    }
}

#[derive(Serialize, Deserialize, Clone)]
struct GenericParamImplCached {
    id: GenericParamCached,
    concrete_trait: ConcreteTraitCached,
    type_constraints: OrderedHashMap<TraitTypeCached, TypeIdCached>,
}

impl GenericParamImplCached {
    fn new(generic_param: GenericParamImpl, ctx: &mut SemanticCacheSavingContext<'_>) -> Self {
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
    fn embed(self, ctx: &mut SemanticCacheLoadingContext<'_>) -> GenericParamImpl {
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

#[derive(Serialize, Deserialize, Clone)]
struct ConcreteVariantCached {
    concrete_enum_id: ConcreteEnumCached,
    id: LanguageElementCached,
    ty: TypeIdCached,
    /// The index of the variant from within the variant list.
    idx: usize,
}
impl ConcreteVariantCached {
    fn new(
        concrete_variant: semantic::ConcreteVariant,
        ctx: &mut SemanticCacheSavingContext<'_>,
    ) -> Self {
        Self {
            concrete_enum_id: ConcreteEnumCached::new(concrete_variant.concrete_enum_id, ctx),
            id: LanguageElementCached::new(concrete_variant.id, &mut ctx.defs_ctx),
            ty: TypeIdCached::new(concrete_variant.ty, ctx),
            idx: concrete_variant.idx,
        }
    }
    fn embed(self, ctx: &mut SemanticCacheLoadingContext<'_>) -> semantic::ConcreteVariant {
        let concrete_enum_id = self.concrete_enum_id.embed(ctx);
        let ty = self.ty.embed(ctx);
        let (module_file_id, stable_ptr) = self.id.get_embedded(&ctx.defs_loading_data);

        let id = VariantLongId(module_file_id, VariantPtr(stable_ptr)).intern(ctx.db);
        semantic::ConcreteVariant { concrete_enum_id, id, ty, idx: self.idx }
    }
}

#[derive(Serialize, Deserialize, Clone)]
struct ConcreteEnumCached {
    enum_id: LanguageElementCached,
    generic_args: Vec<GenericArgumentCached>,
}

impl ConcreteEnumCached {
    fn new(
        concrete_enum: semantic::ConcreteEnumId,
        ctx: &mut SemanticCacheSavingContext<'_>,
    ) -> Self {
        let long_id = concrete_enum.lookup_intern(ctx.db);
        Self {
            enum_id: LanguageElementCached::new(long_id.enum_id, &mut ctx.defs_ctx),
            generic_args: long_id
                .generic_args
                .into_iter()
                .map(|arg| GenericArgumentCached::new(arg, ctx))
                .collect(),
        }
    }
    fn embed(self, ctx: &mut SemanticCacheLoadingContext<'_>) -> semantic::ConcreteEnumId {
        let (module_file_id, stable_ptr) = self.enum_id.get_embedded(&ctx.defs_loading_data);

        let long_id = ConcreteEnumLongId {
            enum_id: EnumLongId(module_file_id, ItemEnumPtr(stable_ptr)).intern(ctx.db),
            generic_args: self.generic_args.into_iter().map(|arg| arg.embed(ctx)).collect(),
        };
        long_id.intern(ctx.db)
    }
}

#[derive(Serialize, Deserialize, Clone)]
struct ConcreteStructCached {
    struct_id: LanguageElementCached,
    generic_args: Vec<GenericArgumentCached>,
}
impl ConcreteStructCached {
    fn new(
        concrete_struct: semantic::ConcreteStructId,
        ctx: &mut SemanticCacheSavingContext<'_>,
    ) -> Self {
        let long_id = concrete_struct.lookup_intern(ctx.db);
        Self {
            struct_id: LanguageElementCached::new(long_id.struct_id, &mut ctx.defs_ctx),
            generic_args: long_id
                .generic_args
                .into_iter()
                .map(|arg| GenericArgumentCached::new(arg, ctx))
                .collect(),
        }
    }
    fn embed(self, ctx: &mut SemanticCacheLoadingContext<'_>) -> semantic::ConcreteStructId {
        let (module_file_id, stable_ptr) = self.struct_id.get_embedded(&ctx.defs_loading_data);

        let long_id = ConcreteStructLongId {
            struct_id: StructLongId(module_file_id, ItemStructPtr(stable_ptr)).intern(ctx.db),
            generic_args: self.generic_args.into_iter().map(|arg| arg.embed(ctx)).collect(),
        };
        long_id.intern(ctx.db)
    }
}

#[derive(Serialize, Deserialize, Clone)]
struct ConcreteExternTypeCached {
    language_element: LanguageElementCached,
    generic_args: Vec<GenericArgumentCached>,
}
impl ConcreteExternTypeCached {
    fn new(
        concrete_extern_type: semantic::ConcreteExternTypeId,
        ctx: &mut SemanticCacheSavingContext<'_>,
    ) -> Self {
        let long_id = concrete_extern_type.lookup_intern(ctx.db);
        Self {
            language_element: LanguageElementCached::new(long_id.extern_type_id, &mut ctx.defs_ctx),
            generic_args: long_id
                .generic_args
                .into_iter()
                .map(|arg| GenericArgumentCached::new(arg, ctx))
                .collect(),
        }
    }
    fn embed(self, ctx: &mut SemanticCacheLoadingContext<'_>) -> semantic::ConcreteExternTypeId {
        let (module_file_id, stable_ptr) =
            self.language_element.get_embedded(&ctx.defs_loading_data);

        let long_id = ConcreteExternTypeLongId {
            extern_type_id: ExternTypeLongId(module_file_id, ItemExternTypePtr(stable_ptr))
                .intern(ctx.db),
            generic_args: self.generic_args.into_iter().map(|arg| arg.embed(ctx)).collect(),
        };
        long_id.intern(ctx.db)
    }
}

#[derive(Serialize, Deserialize, Clone)]
struct ConcreteTraitCached {
    trait_id: LanguageElementCached,
    generic_args: Vec<GenericArgumentCached>,
}

impl ConcreteTraitCached {
    fn new(
        concrete_trait: semantic::ConcreteTraitId,
        ctx: &mut SemanticCacheSavingContext<'_>,
    ) -> Self {
        let long_id = concrete_trait.lookup_intern(ctx.db);
        Self {
            trait_id: LanguageElementCached::new(long_id.trait_id, &mut ctx.defs_ctx),
            generic_args: long_id
                .generic_args
                .into_iter()
                .map(|arg| GenericArgumentCached::new(arg, ctx))
                .collect(),
        }
    }
    fn embed(self, ctx: &mut SemanticCacheLoadingContext<'_>) -> semantic::ConcreteTraitId {
        let (module_file_id, stable_ptr) = self.trait_id.get_embedded(&ctx.defs_loading_data);

        let long_id = ConcreteTraitLongId {
            trait_id: TraitLongId(module_file_id, ItemTraitPtr(stable_ptr)).intern(ctx.db),
            generic_args: self.generic_args.into_iter().map(|arg| arg.embed(ctx)).collect(),
        };
        long_id.intern(ctx.db)
    }
}

#[derive(Serialize, Deserialize, Clone)]
struct LocationCached {
    /// The stable location of the object.
    stable_location: SyntaxStablePtrIdCached,
    /// Function call locations where this value was inlined from.
    inline_locations: Vec<SyntaxStablePtrIdCached>,
}
impl LocationCached {
    fn new(location: Location, ctx: &mut CacheSavingContext<'_>) -> Self {
        Self {
            stable_location: SyntaxStablePtrIdCached::new(
                location.stable_location.stable_ptr(),
                &mut ctx.semantic_ctx.defs_ctx,
            ),
            inline_locations: location
                .inline_locations
                .iter()
                .map(|loc| {
                    SyntaxStablePtrIdCached::new(loc.stable_ptr(), &mut ctx.semantic_ctx.defs_ctx)
                })
                .collect(),
        }
    }
    fn embed(self, ctx: &mut CacheLoadingContext<'_>) -> Location {
        Location {
            stable_location: StableLocation::new(
                self.stable_location.get_embedded(&ctx.semantic_ctx.defs_loading_data),
            ),
            inline_locations: self
                .inline_locations
                .into_iter()
                .map(|loc| {
                    StableLocation::new(loc.get_embedded(&ctx.semantic_ctx.defs_loading_data))
                })
                .collect(),
            notes: Default::default(),
        }
    }
}

#[derive(Serialize, Deserialize, Copy, Clone, PartialEq, Eq, Hash)]
struct LocationIdCached(usize);

impl LocationIdCached {
    fn new(location_id: LocationId, ctx: &mut CacheSavingContext<'_>) -> Self {
        if let Some(id) = ctx.location_ids.get(&location_id) {
            return *id;
        }
        let location = LocationCached::new(location_id.lookup_intern(ctx.db), ctx);
        let id = LocationIdCached(ctx.location_ids_lookup.len());
        ctx.location_ids_lookup.push(location);
        ctx.location_ids.insert(location_id, id);
        id
    }
    fn embed(self, ctx: &mut CacheLoadingContext<'_>) -> LocationId {
        if let Some(location_id) = ctx.location_ids.get(&self) {
            return *location_id;
        }
        let location = ctx.location_ids_lookup[self.0].clone();
        let location = location.embed(ctx).intern(ctx.db);
        ctx.location_ids.insert(self, location);
        location
    }
}
