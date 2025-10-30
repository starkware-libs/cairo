#[cfg(test)]
#[path = "test.rs"]
mod test;

use std::ops::{Deref, DerefMut};
use std::sync::Arc;

use bincode::error::EncodeError;
use cairo_lang_defs as defs;
use cairo_lang_defs::cache::{
    CachedCrateMetadata, DefCacheLoadingData, DefCacheSavingContext, LanguageElementCached,
    SyntaxStablePtrIdCached, generate_crate_def_cache,
};
use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::diagnostic_utils::StableLocation;
use cairo_lang_defs::ids::{
    FreeFunctionLongId, FunctionWithBodyId, ImplFunctionLongId, TraitFunctionLongId,
};
use cairo_lang_diagnostics::{DiagnosticAdded, Maybe, skip_diagnostic};
use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_filesystem::ids::CrateId;
use cairo_lang_semantic::cache::{
    ConcreteEnumCached, ConcreteVariantCached, ConstValueIdCached, ExprVarMemberPathCached,
    ImplIdCached, MatchArmSelectorCached, SemanticCacheLoadingData, SemanticCacheSavingContext,
    SemanticCacheSavingData, SemanticConcreteFunctionWithBodyCached, SemanticFunctionIdCached,
    TypeIdCached, generate_crate_semantic_cache,
};
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::expr::inference::InferenceError;
use cairo_lang_semantic::items::function_with_body::FunctionWithBodySemantic;
use cairo_lang_semantic::items::imp::ImplSemantic;
use cairo_lang_semantic::items::trt::TraitSemantic;
use cairo_lang_semantic::types::TypeInfo;
use cairo_lang_syntax::node::TypedStablePtr;
use cairo_lang_syntax::node::ast::{ExprPtr, FunctionWithBodyPtr, TraitItemFunctionPtr};
use cairo_lang_utils::Intern;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use id_arena::Arena;
use salsa::Database;
use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::blocks::BlocksBuilder;
use crate::ids::{
    FunctionId, FunctionLongId, GeneratedFunction, GeneratedFunctionKey, LocationId, LoweredParam,
    Signature,
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

type LookupCache = (CacheLookups, Vec<(DefsFunctionWithBodyIdCached, MultiLoweringCached)>);

/// Load the cached lowering of a crate if it has a cache file configuration.
pub fn load_cached_crate_functions<'db>(
    db: &'db dyn Database,
    crate_id: CrateId<'db>,
) -> Option<OrderedHashMap<defs::ids::FunctionWithBodyId<'db>, MultiLowering<'db>>> {
    let blob_id = db.crate_config(crate_id)?.cache_file?;
    let Some(content) = db.blob_content(blob_id) else {
        return Default::default();
    };

    let semantic_loading_data = db.cached_crate_semantic_data(crate_id)?.loading_data;

    let def_size = usize::from_be_bytes(content[..8].try_into().unwrap()); // def_size is the first 8 bytes of the blob.
    let semantic_start = 8 + def_size;
    let semantic_size =
        usize::from_be_bytes(content[semantic_start..semantic_start + 8].try_into().unwrap()); // semantic_size is the next 8 bytes.

    let lowering_start = semantic_start + semantic_size + 8;
    let lowering_size =
        usize::from_be_bytes(content[lowering_start..lowering_start + 8].try_into().unwrap()); // lowering_size is the next 8 bytes.

    let content = &content[lowering_start + 8..lowering_start + 8 + lowering_size];

    let ((lookups, lowerings), _): (LookupCache, _) =
        bincode::serde::decode_from_slice(content, bincode::config::standard()).unwrap_or_else(
            |e| {
                panic!(
                    "failed to deserialize lookup cache for crate `{}`: {e}",
                    crate_id.long(db).name().long(db),
                )
            },
        );

    // TODO(tomer): Fail on version, cfg, and dependencies mismatch.

    let mut ctx = CacheLoadingContext::new(db, lookups, semantic_loading_data);

    Some(
        lowerings
            .into_iter()
            .map(|(function_id, lowering)| {
                let function_id =
                    function_id.embed(&ctx.semantic_loading_data.defs_loading_data, ctx.db);

                let lowering = lowering.embed(&mut ctx);
                (function_id, lowering)
            })
            .collect::<OrderedHashMap<_, _>>(),
    )
}

#[derive(Debug, Error)]
pub enum CrateCacheError {
    #[error("Failed compilation of crate.")]
    CompilationFailed,
    #[error("Cache encoding failed: {0}")]
    EncodingError(#[from] EncodeError),
}

impl From<DiagnosticAdded> for CrateCacheError {
    fn from(_e: DiagnosticAdded) -> Self {
        Self::CompilationFailed
    }
}

/// Cache the lowering of each function in the crate into a blob.
pub fn generate_crate_cache<'db>(
    db: &'db dyn Database,
    crate_id: CrateId<'db>,
) -> Result<Vec<u8>, CrateCacheError> {
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
    let semantic_cache = generate_crate_semantic_cache(crate_id, &mut ctx.semantic_ctx)?;

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

    let def = bincode::serde::encode_to_vec(
        &(CachedCrateMetadata::new(crate_id, db), def_cache, &ctx.semantic_ctx.defs_ctx.lookups),
        bincode::config::standard(),
    )?;
    artifact.extend(def.len().to_be_bytes());
    artifact.extend(def);

    let semantic = bincode::serde::encode_to_vec(
        &(semantic_cache, &ctx.semantic_ctx.lookups),
        bincode::config::standard(),
    )?;
    artifact.extend(semantic.len().to_be_bytes());
    artifact.extend(semantic);

    let lowered =
        bincode::serde::encode_to_vec(&(&ctx.lookups, cached), bincode::config::standard())?;
    artifact.extend(lowered.len().to_be_bytes());
    artifact.extend(lowered);

    Ok(artifact)
}

/// Context for loading cache into the database.
struct CacheLoadingContext<'db> {
    /// The variable ids of the flat lowered that is currently being loaded.
    lowered_variables_id: Vec<VariableId>,
    db: &'db dyn Database,

    /// data for loading the entire cache into the database.
    data: CacheLoadingData<'db>,

    semantic_loading_data: Arc<SemanticCacheLoadingData<'db>>,
}

impl<'db> CacheLoadingContext<'db> {
    fn new(
        db: &'db dyn Database,
        lookups: CacheLookups,
        semantic_loading_data: Arc<SemanticCacheLoadingData<'db>>,
    ) -> Self {
        Self {
            lowered_variables_id: Vec::new(),
            db,
            data: CacheLoadingData {
                function_ids: OrderedHashMap::default(),
                location_ids: OrderedHashMap::default(),
                lookups,
            },
            semantic_loading_data,
        }
    }
}

impl<'db> Deref for CacheLoadingContext<'db> {
    type Target = CacheLoadingData<'db>;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}
impl<'db> DerefMut for CacheLoadingContext<'db> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.data
    }
}

/// Data for loading cache into the database.
struct CacheLoadingData<'db> {
    function_ids: OrderedHashMap<FunctionIdCached, FunctionId<'db>>,
    location_ids: OrderedHashMap<LocationIdCached, LocationId<'db>>,
    lookups: CacheLookups,
}
impl Deref for CacheLoadingData<'_> {
    type Target = CacheLookups;

    fn deref(&self) -> &Self::Target {
        &self.lookups
    }
}
impl DerefMut for CacheLoadingData<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.lookups
    }
}

/// Context for saving cache from the database.
struct CacheSavingContext<'db> {
    db: &'db dyn Database,
    data: CacheSavingData<'db>,
    semantic_ctx: SemanticCacheSavingContext<'db>,
}
impl<'db> Deref for CacheSavingContext<'db> {
    type Target = CacheSavingData<'db>;

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
    fn new(db: &'db dyn Database, self_crate_id: CrateId<'db>) -> Self {
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
struct CacheSavingData<'db> {
    function_ids: OrderedHashMap<FunctionId<'db>, FunctionIdCached>,
    location_ids: OrderedHashMap<LocationId<'db>, LocationIdCached>,
    lookups: CacheLookups,
}
impl<'db> Deref for CacheSavingData<'db> {
    type Target = CacheLookups;

    fn deref(&self) -> &Self::Target {
        &self.lookups
    }
}
impl<'db> DerefMut for CacheSavingData<'db> {
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

/// Cached version of [defs::ids::FunctionWithBodyId]
/// used as a key in the cache for the [MultiLowering] struct.
#[derive(Serialize, Deserialize, Hash, Eq, PartialEq)]
enum DefsFunctionWithBodyIdCached {
    Free(LanguageElementCached),
    Impl(LanguageElementCached),
    Trait(LanguageElementCached),
}
impl DefsFunctionWithBodyIdCached {
    fn new<'db>(
        id: defs::ids::FunctionWithBodyId<'db>,
        ctx: &mut SemanticCacheSavingContext<'db>,
    ) -> Self {
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

    fn embed<'db>(
        self,
        defs_loading_data: &Arc<DefCacheLoadingData<'db>>,
        db: &'db dyn Database,
    ) -> defs::ids::FunctionWithBodyId<'db> {
        match self {
            DefsFunctionWithBodyIdCached::Free(id) => {
                let (module_id, function_stable_ptr) = id.get_embedded(defs_loading_data);
                defs::ids::FunctionWithBodyId::Free(
                    FreeFunctionLongId(module_id, FunctionWithBodyPtr(function_stable_ptr))
                        .intern(db),
                )
            }
            DefsFunctionWithBodyIdCached::Impl(id) => {
                let (module_id, function_stable_ptr) = id.get_embedded(defs_loading_data);
                defs::ids::FunctionWithBodyId::Impl(
                    ImplFunctionLongId(module_id, FunctionWithBodyPtr(function_stable_ptr))
                        .intern(db),
                )
            }
            DefsFunctionWithBodyIdCached::Trait(id) => {
                let (module_id, function_stable_ptr) = id.get_embedded(defs_loading_data);
                defs::ids::FunctionWithBodyId::Trait(
                    TraitFunctionLongId(module_id, TraitItemFunctionPtr(function_stable_ptr))
                        .intern(db),
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
    fn new<'db>(lowering: MultiLowering<'db>, ctx: &mut CacheSavingContext<'db>) -> Self {
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
    fn embed<'db>(self, ctx: &mut CacheLoadingContext<'db>) -> MultiLowering<'db> {
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
    signature: LoweredSignatureCached,
    /// Arena of allocated lowered variables.
    variables: Vec<VariableCached>,
    /// Arena of allocated lowered blocks.
    blocks: Vec<BlockCached>,
    /// function parameters, including implicits.
    parameters: Vec<usize>,
}
impl LoweredCached {
    fn new<'db>(lowered: Lowered<'db>, ctx: &mut CacheSavingContext<'db>) -> Self {
        Self {
            signature: LoweredSignatureCached::new(lowered.signature, ctx),
            variables: lowered
                .variables
                .into_iter()
                .map(|var| VariableCached::new(var.1, ctx))
                .collect(),
            blocks: lowered
                .blocks
                .iter()
                .map(|block: (BlockId, &Block<'_>)| BlockCached::new(block.1.clone(), ctx))
                .collect(),
            parameters: lowered.parameters.iter().map(|var| var.index()).collect(),
        }
    }
    fn embed<'db>(self, ctx: &mut CacheLoadingContext<'db>) -> Lowered<'db> {
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
struct LoweredSignatureCached {
    /// Function parameters.
    params: Vec<LoweredParamCached>,
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
impl LoweredSignatureCached {
    fn new<'db>(signature: Signature<'db>, ctx: &mut CacheSavingContext<'db>) -> Self {
        Self {
            params: signature
                .params
                .iter()
                .map(|param| LoweredParamCached::new(param, ctx))
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
    fn embed<'db>(self, ctx: &mut CacheLoadingContext<'db>) -> Signature<'db> {
        Signature {
            params: self.params.into_iter().map(|var| var.embed(ctx)).collect(),
            extra_rets: self
                .extra_rets
                .into_iter()
                .map(|var| var.get_embedded(&ctx.semantic_loading_data, ctx.db))
                .collect(),
            return_type: self.return_type.get_embedded(&ctx.semantic_loading_data),
            implicits: self
                .implicits
                .into_iter()
                .map(|ty| ty.get_embedded(&ctx.semantic_loading_data))
                .collect(),
            panicable: self.panicable,
            location: self.location.embed(ctx),
        }
    }
}

#[derive(Serialize, Deserialize)]
struct LoweredParamCached {
    ty: TypeIdCached,
    stable_ptr: SyntaxStablePtrIdCached,
}
impl LoweredParamCached {
    fn new<'db>(param: &LoweredParam<'db>, ctx: &mut CacheSavingContext<'db>) -> Self {
        Self {
            ty: TypeIdCached::new(param.ty, &mut ctx.semantic_ctx),
            stable_ptr: SyntaxStablePtrIdCached::new(
                param.stable_ptr.untyped(),
                &mut ctx.semantic_ctx.defs_ctx,
            ),
        }
    }
    fn embed<'db>(self, ctx: &mut CacheLoadingContext<'db>) -> LoweredParam<'db> {
        LoweredParam {
            ty: self.ty.get_embedded(&ctx.semantic_loading_data),
            stable_ptr: ExprPtr(
                self.stable_ptr.get_embedded(&ctx.semantic_loading_data.defs_loading_data),
            ),
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
    fn new<'db>(variable: Variable<'db>, ctx: &mut CacheSavingContext<'db>) -> Self {
        let TypeInfo { droppable, copyable, destruct_impl, panic_destruct_impl } = variable.info;
        Self {
            droppable: droppable
                .map(|impl_id| ImplIdCached::new(impl_id, &mut ctx.semantic_ctx))
                .ok(),
            copyable: copyable
                .map(|impl_id| ImplIdCached::new(impl_id, &mut ctx.semantic_ctx))
                .ok(),
            destruct_impl: destruct_impl
                .map(|impl_id| ImplIdCached::new(impl_id, &mut ctx.semantic_ctx))
                .ok(),
            panic_destruct_impl: panic_destruct_impl
                .map(|impl_id| ImplIdCached::new(impl_id, &mut ctx.semantic_ctx))
                .ok(),
            ty: TypeIdCached::new(variable.ty, &mut ctx.semantic_ctx),
            location: LocationIdCached::new(variable.location, ctx),
        }
    }
    fn embed<'db>(self, ctx: &mut CacheLoadingContext<'db>) -> Variable<'db> {
        Variable {
            ty: self.ty.get_embedded(&ctx.semantic_loading_data),
            location: self.location.embed(ctx),
            info: TypeInfo {
                droppable: self
                    .droppable
                    .map(|impl_id| impl_id.get_embedded(&ctx.semantic_loading_data))
                    .ok_or(InferenceError::Reported(skip_diagnostic())),
                copyable: self
                    .copyable
                    .map(|impl_id| impl_id.get_embedded(&ctx.semantic_loading_data))
                    .ok_or(InferenceError::Reported(skip_diagnostic())),
                destruct_impl: self
                    .destruct_impl
                    .map(|impl_id| impl_id.get_embedded(&ctx.semantic_loading_data))
                    .ok_or(InferenceError::Reported(skip_diagnostic())),
                panic_destruct_impl: self
                    .panic_destruct_impl
                    .map(|impl_id| impl_id.get_embedded(&ctx.semantic_loading_data))
                    .ok_or(InferenceError::Reported(skip_diagnostic())),
            },
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
    fn new<'db>(var_usage: VarUsage<'db>, ctx: &mut CacheSavingContext<'db>) -> Self {
        Self {
            var_id: var_usage.var_id.index(),
            location: LocationIdCached::new(var_usage.location, ctx),
        }
    }
    fn embed<'db>(self, ctx: &mut CacheLoadingContext<'db>) -> VarUsage<'db> {
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
    fn new<'db>(block: Block<'db>, ctx: &mut CacheSavingContext<'db>) -> Self {
        Self {
            statements: block
                .statements
                .into_iter()
                .map(|stmt| StatementCached::new(stmt, ctx))
                .collect(),
            end: BlockEndCached::new(block.end, ctx),
        }
    }
    fn embed<'db>(self, ctx: &mut CacheLoadingContext<'db>) -> Block<'db> {
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
    fn new<'db>(block_end: BlockEnd<'db>, ctx: &mut CacheSavingContext<'db>) -> Self {
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
    fn embed<'db>(self, ctx: &mut CacheLoadingContext<'db>) -> BlockEnd<'db> {
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
    fn new<'db>(var_remapping: VarRemapping<'db>, ctx: &mut CacheSavingContext<'db>) -> Self {
        Self {
            remapping: var_remapping
                .iter()
                .map(|(dst, src)| (dst.index(), VarUsageCached::new(*src, ctx)))
                .collect(),
        }
    }
    fn embed<'db>(self, ctx: &mut CacheLoadingContext<'db>) -> VarRemapping<'db> {
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
    fn new<'db>(match_info: MatchInfo<'db>, ctx: &mut CacheSavingContext<'db>) -> Self {
        match match_info {
            MatchInfo::Enum(info) => MatchInfoCached::Enum(MatchEnumInfoCached::new(info, ctx)),
            MatchInfo::Extern(info) => {
                MatchInfoCached::Extern(MatchExternInfoCached::new(info, ctx))
            }
            MatchInfo::Value(info) => MatchInfoCached::Value(MatchEnumValueCached::new(info, ctx)),
        }
    }
    fn embed<'db>(self, ctx: &mut CacheLoadingContext<'db>) -> MatchInfo<'db> {
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
    fn new<'db>(match_enum_info: MatchEnumInfo<'db>, ctx: &mut CacheSavingContext<'db>) -> Self {
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
    fn embed<'db>(self, ctx: &mut CacheLoadingContext<'db>) -> MatchEnumInfo<'db> {
        MatchEnumInfo {
            concrete_enum_id: self
                .concrete_enum_id
                .get_embedded(&ctx.semantic_loading_data, ctx.db),
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
    fn new<'db>(
        match_extern_info: MatchExternInfo<'db>,
        ctx: &mut CacheSavingContext<'db>,
    ) -> Self {
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
    fn embed<'db>(self, ctx: &mut CacheLoadingContext<'db>) -> MatchExternInfo<'db> {
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
    fn new<'db>(match_enum_value: MatchEnumValue<'db>, ctx: &mut CacheSavingContext<'db>) -> Self {
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
    fn embed<'db>(self, ctx: &mut CacheLoadingContext<'db>) -> MatchEnumValue<'db> {
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
    fn new<'db>(match_arm: MatchArm<'db>, ctx: &mut CacheSavingContext<'db>) -> Self {
        Self {
            arm_selector: MatchArmSelectorCached::new(
                match_arm.arm_selector,
                &mut ctx.semantic_ctx,
            ),
            block_id: match_arm.block_id.0,
            var_ids: match_arm.var_ids.iter().map(|var| var.index()).collect(),
        }
    }
    fn embed<'db>(self, ctx: &mut CacheLoadingContext<'db>) -> MatchArm<'db> {
        MatchArm {
            arm_selector: self.arm_selector.get_embedded(&ctx.semantic_loading_data, ctx.db),
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
    fn new<'db>(stmt: Statement<'db>, ctx: &mut CacheSavingContext<'db>) -> Self {
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
    fn embed<'db>(self, ctx: &mut CacheLoadingContext<'db>) -> Statement<'db> {
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
    value: ConstValueIdCached,
    /// The variable to bind the value to.
    output: usize,
    /// Is the const boxed.
    boxed: bool,
}
impl StatementConstCached {
    fn new<'db>(stmt: StatementConst<'db>, ctx: &mut CacheSavingContext<'db>) -> Self {
        Self {
            value: ConstValueIdCached::new(stmt.value, &mut ctx.semantic_ctx),
            output: stmt.output.index(),
            boxed: stmt.boxed,
        }
    }
    fn embed<'db>(self, ctx: &mut CacheLoadingContext<'db>) -> StatementConst<'db> {
        StatementConst::new(
            self.value.get_embedded(&ctx.semantic_loading_data),
            ctx.lowered_variables_id[self.output],
            self.boxed,
        )
    }
}

#[derive(Serialize, Deserialize)]
struct StatementCallCached {
    function: FunctionIdCached,
    inputs: Vec<VarUsageCached>,
    with_coupon: bool,
    outputs: Vec<usize>,
    location: LocationIdCached,
}
impl StatementCallCached {
    fn new<'db>(stmt: StatementCall<'db>, ctx: &mut CacheSavingContext<'db>) -> Self {
        Self {
            function: FunctionIdCached::new(stmt.function, ctx),
            inputs: stmt.inputs.iter().map(|var| VarUsageCached::new(*var, ctx)).collect(),
            with_coupon: stmt.with_coupon,
            outputs: stmt.outputs.iter().map(|var| var.index()).collect(),
            location: LocationIdCached::new(stmt.location, ctx),
        }
    }
    fn embed<'db>(self, ctx: &mut CacheLoadingContext<'db>) -> StatementCall<'db> {
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
    fn new<'db>(function: FunctionLongId<'db>, ctx: &mut CacheSavingContext<'db>) -> Self {
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
    fn embed<'db>(self, ctx: &mut CacheLoadingContext<'db>) -> FunctionId<'db> {
        match self {
            FunctionCached::Semantic(id) => {
                FunctionLongId::Semantic(id.get_embedded(&ctx.semantic_loading_data))
            }
            FunctionCached::Generated(id) => FunctionLongId::Generated(id.embed(ctx)),
        }
        .intern(ctx.db)
    }
}

#[derive(Serialize, Deserialize, Copy, Clone, PartialEq, Eq, Hash)]
struct FunctionIdCached(usize);
impl FunctionIdCached {
    fn new<'db>(function_id: FunctionId<'db>, ctx: &mut CacheSavingContext<'db>) -> Self {
        if let Some(id) = ctx.function_ids.get(&function_id) {
            return *id;
        }
        let function = FunctionCached::new(function_id.long(ctx.db).clone(), ctx);
        let id = FunctionIdCached(ctx.function_ids_lookup.len());
        ctx.function_ids_lookup.push(function);
        ctx.function_ids.insert(function_id, id);
        id
    }
    fn embed<'db>(self, ctx: &mut CacheLoadingContext<'db>) -> FunctionId<'db> {
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
struct GeneratedFunctionCached {
    parent: SemanticConcreteFunctionWithBodyCached,
    key: GeneratedFunctionKeyCached,
}
impl GeneratedFunctionCached {
    fn new<'db>(function: GeneratedFunction<'db>, ctx: &mut CacheSavingContext<'db>) -> Self {
        Self {
            parent: SemanticConcreteFunctionWithBodyCached::new(
                function.parent,
                &mut ctx.semantic_ctx,
            ),
            key: GeneratedFunctionKeyCached::new(function.key, ctx),
        }
    }
    fn embed<'db>(self, ctx: &mut CacheLoadingContext<'db>) -> GeneratedFunction<'db> {
        GeneratedFunction {
            parent: self.parent.get_embedded(&ctx.semantic_loading_data, ctx.db),
            key: self.key.embed(ctx),
        }
    }
}

#[derive(Serialize, Deserialize, Clone, Hash, PartialEq, Eq)]
enum GeneratedFunctionKeyCached {
    Loop(SyntaxStablePtrIdCached),
    TraitFunc(LanguageElementCached, SyntaxStablePtrIdCached),
}

impl GeneratedFunctionKeyCached {
    fn new<'db>(key: GeneratedFunctionKey<'db>, ctx: &mut CacheSavingContext<'db>) -> Self {
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
    fn embed<'db>(self, ctx: &mut CacheLoadingContext<'db>) -> GeneratedFunctionKey<'db> {
        match self {
            GeneratedFunctionKeyCached::Loop(id) => GeneratedFunctionKey::Loop(ExprPtr(
                id.get_embedded(&ctx.semantic_loading_data.defs_loading_data),
            )),
            GeneratedFunctionKeyCached::TraitFunc(id, stable_location) => {
                let (module_id, stable_ptr) =
                    id.get_embedded(&ctx.semantic_loading_data.defs_loading_data);
                GeneratedFunctionKey::TraitFunc(
                    TraitFunctionLongId(module_id, TraitItemFunctionPtr(stable_ptr)).intern(ctx.db),
                    StableLocation::new(
                        stable_location.get_embedded(&ctx.semantic_loading_data.defs_loading_data),
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
    fn new<'db>(stmt: StatementStructConstruct<'db>, _ctx: &mut CacheSavingContext<'db>) -> Self {
        Self {
            inputs: stmt.inputs.iter().map(|var| VarUsageCached::new(*var, _ctx)).collect(),
            output: stmt.output.index(),
        }
    }
    fn embed<'db>(self, ctx: &mut CacheLoadingContext<'db>) -> StatementStructConstruct<'db> {
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
    fn new<'db>(stmt: StatementStructDestructure<'db>, _ctx: &mut CacheSavingContext<'db>) -> Self {
        Self {
            input: VarUsageCached::new(stmt.input, _ctx),
            outputs: stmt.outputs.iter().map(|var| var.index()).collect(),
        }
    }
    fn embed<'db>(self, ctx: &mut CacheLoadingContext<'db>) -> StatementStructDestructure<'db> {
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
    fn new<'db>(stmt: StatementEnumConstruct<'db>, ctx: &mut CacheSavingContext<'db>) -> Self {
        Self {
            variant: ConcreteVariantCached::new(stmt.variant, &mut ctx.semantic_ctx),
            input: VarUsageCached::new(stmt.input, ctx),
            output: stmt.output.index(),
        }
    }
    fn embed<'db>(self, ctx: &mut CacheLoadingContext<'db>) -> StatementEnumConstruct<'db> {
        StatementEnumConstruct {
            variant: self.variant.get_embedded(&ctx.semantic_loading_data, ctx.db),
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
    fn new<'db>(stmt: StatementSnapshot<'db>, _ctx: &mut CacheSavingContext<'db>) -> Self {
        Self {
            input: VarUsageCached::new(stmt.input, _ctx),
            outputs: stmt.outputs.map(|var| var.index()),
        }
    }
    fn embed<'db>(self, ctx: &mut CacheLoadingContext<'db>) -> StatementSnapshot<'db> {
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
    fn new<'db>(stmt: StatementDesnap<'db>, ctx: &mut CacheSavingContext<'db>) -> Self {
        Self { input: VarUsageCached::new(stmt.input, ctx), output: stmt.output.index() }
    }
    fn embed<'db>(self, ctx: &mut CacheLoadingContext<'db>) -> StatementDesnap<'db> {
        StatementDesnap {
            input: self.input.embed(ctx),
            output: ctx.lowered_variables_id[self.output],
        }
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
    fn new<'db>(location: Location<'db>, ctx: &mut CacheSavingContext<'db>) -> Self {
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
    fn embed<'db>(self, ctx: &mut CacheLoadingContext<'db>) -> Location<'db> {
        Location {
            stable_location: StableLocation::new(
                self.stable_location.get_embedded(&ctx.semantic_loading_data.defs_loading_data),
            ),
            inline_locations: self
                .inline_locations
                .into_iter()
                .map(|loc| {
                    StableLocation::new(
                        loc.get_embedded(&ctx.semantic_loading_data.defs_loading_data),
                    )
                })
                .collect(),
            notes: Default::default(),
        }
    }
}

#[derive(Serialize, Deserialize, Copy, Clone, PartialEq, Eq, Hash)]
struct LocationIdCached(usize);

impl LocationIdCached {
    fn new<'db>(location_id: LocationId<'db>, ctx: &mut CacheSavingContext<'db>) -> Self {
        if let Some(id) = ctx.location_ids.get(&location_id) {
            return *id;
        }
        let location = LocationCached::new(location_id.long(ctx.db).clone(), ctx);
        let id = LocationIdCached(ctx.location_ids_lookup.len());
        ctx.location_ids_lookup.push(location);
        ctx.location_ids.insert(location_id, id);
        id
    }
    fn embed<'db>(self, ctx: &mut CacheLoadingContext<'db>) -> LocationId<'db> {
        if let Some(location_id) = ctx.location_ids.get(&self) {
            return *location_id;
        }
        let location = ctx.location_ids_lookup[self.0].clone();
        let location = location.embed(ctx).intern(ctx.db);
        ctx.location_ids.insert(self, location);
        location
    }
}
