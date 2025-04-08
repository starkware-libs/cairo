use cairo_lang_defs::ids::{
    EnumId, ExternFunctionId, ExternTypeId, FreeFunctionId, GenericParamId, ImplAliasId, ImplDefId,
    ImplFunctionId, ImplImplDefId, LocalVarId, MemberId, ParamId, StructId, TraitConstantId,
    TraitFunctionId, TraitId, TraitImplId, TraitTypeId, VarId, VariantId,
};
use cairo_lang_proc_macros::SemanticObject;
use cairo_lang_utils::LookupIntern;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use super::{
    ConstVar, ImplVar, ImplVarId, ImplVarTraitItemMappings, Inference, InferenceId, InferenceVar,
    LocalConstVarId, LocalImplVarId, LocalTypeVarId, TypeVar,
};
use crate::db::SemanticGroup;
use crate::items::constant::{ConstValue, ConstValueId, ImplConstantId};
use crate::items::functions::{
    ConcreteFunctionWithBody, ConcreteFunctionWithBodyId, GenericFunctionId,
    GenericFunctionWithBodyId, ImplFunctionBodyId, ImplGenericFunctionId,
    ImplGenericFunctionWithBodyId,
};
use crate::items::generics::{GenericParamConst, GenericParamImpl, GenericParamType};
use crate::items::imp::{
    GeneratedImplId, GeneratedImplItems, GeneratedImplLongId, ImplId, ImplImplId, ImplLongId,
    UninferredGeneratedImplId, UninferredGeneratedImplLongId, UninferredImpl,
};
use crate::items::trt::{
    ConcreteTraitGenericFunctionId, ConcreteTraitGenericFunctionLongId, ConcreteTraitTypeId,
    ConcreteTraitTypeLongId,
};
use crate::substitution::{HasDb, RewriteResult, SemanticObject, SemanticRewriter};
use crate::types::{
    ClosureTypeLongId, ConcreteEnumLongId, ConcreteExternTypeLongId, ConcreteStructLongId,
    ImplTypeId,
};
use crate::{
    ConcreteEnumId, ConcreteExternTypeId, ConcreteFunction, ConcreteImplId, ConcreteImplLongId,
    ConcreteStructId, ConcreteTraitId, ConcreteTraitLongId, ConcreteTypeId, ConcreteVariant,
    ExprId, ExprVar, ExprVarMemberPath, FunctionId, FunctionLongId, GenericArgumentId,
    GenericParam, MatchArmSelector, Parameter, Signature, TypeId, TypeLongId, ValueSelectorArm,
    add_basic_rewrites,
};

/// A canonical representation of a concrete trait that needs to be solved.
#[derive(Clone, PartialEq, Hash, Eq, Debug, SemanticObject)]
pub struct CanonicalTrait {
    pub id: ConcreteTraitId,
    pub mappings: ImplVarTraitItemMappings,
}

impl CanonicalTrait {
    /// Canonicalizes a concrete trait that is part of an [Inference].
    pub fn canonicalize(
        db: &dyn SemanticGroup,
        source_inference_id: InferenceId,
        trait_id: ConcreteTraitId,
        impl_var_mappings: ImplVarTraitItemMappings,
    ) -> (Self, CanonicalMapping) {
        Canonicalizer::canonicalize(
            db,
            source_inference_id,
            Self { id: trait_id, mappings: impl_var_mappings },
        )
    }
    /// Embeds a canonical trait into an [Inference].
    pub fn embed(&self, inference: &mut Inference<'_>) -> (CanonicalTrait, CanonicalMapping) {
        Embedder::embed(inference, self.clone())
    }
}

/// A solution for a [CanonicalTrait].
#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct CanonicalImpl(pub ImplId);
impl CanonicalImpl {
    /// Canonicalizes a concrete impl that is part of an [Inference].
    /// Uses the same canonicalization of the trait, to be consistent.
    pub fn canonicalize(
        db: &dyn SemanticGroup,
        impl_id: ImplId,
        mapping: &CanonicalMapping,
    ) -> Result<Self, MapperError> {
        Ok(Self(Mapper::map(db, impl_id, &mapping.to_canonic)?))
    }
    /// Embeds a canonical impl into an [Inference].
    /// Uses the same embedding of the trait, to be consistent.
    pub fn embed(&self, inference: &Inference<'_>, mapping: &CanonicalMapping) -> ImplId {
        Mapper::map(inference.db, self.0, &mapping.from_canonic)
            .expect("Tried to embed a non canonical impl")
    }
}

/// Mapping between canonical space and inference space.
/// Created by either canonicalizing or embedding a trait.
#[derive(Debug)]
pub struct CanonicalMapping {
    to_canonic: VarMapping,
    from_canonic: VarMapping,
}
impl CanonicalMapping {
    fn from_to_canonic(to_canonic: VarMapping) -> CanonicalMapping {
        let from_canonic = VarMapping {
            type_var_mapping: to_canonic.type_var_mapping.iter().map(|(k, v)| (*v, *k)).collect(),
            const_var_mapping: to_canonic.const_var_mapping.iter().map(|(k, v)| (*v, *k)).collect(),
            impl_var_mapping: to_canonic.impl_var_mapping.iter().map(|(k, v)| (*v, *k)).collect(),
            source_inference_id: to_canonic.target_inference_id,
            target_inference_id: to_canonic.source_inference_id,
        };
        Self { to_canonic, from_canonic }
    }
    fn from_from_canonic(from_canonic: VarMapping) -> CanonicalMapping {
        let to_canonic = VarMapping {
            type_var_mapping: from_canonic.type_var_mapping.iter().map(|(k, v)| (*v, *k)).collect(),
            const_var_mapping: from_canonic
                .const_var_mapping
                .iter()
                .map(|(k, v)| (*v, *k))
                .collect(),
            impl_var_mapping: from_canonic.impl_var_mapping.iter().map(|(k, v)| (*v, *k)).collect(),
            source_inference_id: from_canonic.target_inference_id,
            target_inference_id: from_canonic.source_inference_id,
        };
        Self { to_canonic, from_canonic }
    }
}

// Mappings.
#[derive(Debug)]
pub struct VarMapping {
    type_var_mapping: OrderedHashMap<LocalTypeVarId, LocalTypeVarId>,
    const_var_mapping: OrderedHashMap<LocalConstVarId, LocalConstVarId>,
    impl_var_mapping: OrderedHashMap<LocalImplVarId, LocalImplVarId>,
    source_inference_id: InferenceId,
    target_inference_id: InferenceId,
}
impl VarMapping {
    fn new_to_canonic(source_inference_id: InferenceId) -> Self {
        Self {
            type_var_mapping: OrderedHashMap::default(),
            const_var_mapping: OrderedHashMap::default(),
            impl_var_mapping: OrderedHashMap::default(),
            source_inference_id,
            target_inference_id: InferenceId::Canonical,
        }
    }
    fn new_from_canonic(target_inference_id: InferenceId) -> Self {
        Self {
            type_var_mapping: OrderedHashMap::default(),
            const_var_mapping: OrderedHashMap::default(),
            impl_var_mapping: OrderedHashMap::default(),
            source_inference_id: InferenceId::Canonical,
            target_inference_id,
        }
    }
}

/// A 'never' error.
#[derive(Debug)]
pub enum NoError {}
pub trait ResultNoErrEx<T> {
    fn no_err(self) -> T;
}
impl<T> ResultNoErrEx<T> for Result<T, NoError> {
    fn no_err(self) -> T {
        match self {
            Ok(v) => v,
            #[allow(unreachable_patterns)]
            Err(err) => match err {},
        }
    }
}

/// Canonicalization rewriter. Each encountered variable is mapped to a new free variable,
/// in pre-order.
struct Canonicalizer<'db> {
    db: &'db dyn SemanticGroup,
    to_canonic: VarMapping,
}
impl<'db> Canonicalizer<'db> {
    fn canonicalize<T>(
        db: &'db dyn SemanticGroup,
        source_inference_id: InferenceId,
        value: T,
    ) -> (T, CanonicalMapping)
    where
        Self: SemanticRewriter<T, NoError>,
    {
        let mut canonicalizer =
            Self { db, to_canonic: VarMapping::new_to_canonic(source_inference_id) };
        let value = canonicalizer.rewrite(value).no_err();
        let mapping = CanonicalMapping::from_to_canonic(canonicalizer.to_canonic);
        (value, mapping)
    }
}
impl<'a> HasDb<&'a dyn SemanticGroup> for Canonicalizer<'a> {
    fn get_db(&self) -> &'a dyn SemanticGroup {
        self.db
    }
}

add_basic_rewrites!(
    <'a>,
    Canonicalizer<'a>,
    NoError,
    @exclude TypeLongId TypeId ImplLongId ImplId ConstValue
);

impl SemanticRewriter<TypeId, NoError> for Canonicalizer<'_> {
    fn internal_rewrite(&mut self, value: &mut TypeId) -> Result<RewriteResult, NoError> {
        if value.is_var_free(self.db) {
            return Ok(RewriteResult::NoChange);
        }
        value.default_rewrite(self)
    }
}
impl SemanticRewriter<TypeLongId, NoError> for Canonicalizer<'_> {
    fn internal_rewrite(&mut self, value: &mut TypeLongId) -> Result<RewriteResult, NoError> {
        let TypeLongId::Var(var) = value else {
            return value.default_rewrite(self);
        };
        if var.inference_id != self.to_canonic.source_inference_id {
            return value.default_rewrite(self);
        }
        let next_id = LocalTypeVarId(self.to_canonic.type_var_mapping.len());
        *value = TypeLongId::Var(TypeVar {
            id: *self.to_canonic.type_var_mapping.entry(var.id).or_insert(next_id),
            inference_id: InferenceId::Canonical,
        });
        Ok(RewriteResult::Modified)
    }
}
impl SemanticRewriter<ConstValue, NoError> for Canonicalizer<'_> {
    fn internal_rewrite(&mut self, value: &mut ConstValue) -> Result<RewriteResult, NoError> {
        let ConstValue::Var(var, mut ty) = value else {
            return value.default_rewrite(self);
        };
        if var.inference_id != self.to_canonic.source_inference_id {
            return value.default_rewrite(self);
        }
        let next_id = LocalConstVarId(self.to_canonic.const_var_mapping.len());
        ty.default_rewrite(self)?;
        *value = ConstValue::Var(
            ConstVar {
                id: *self.to_canonic.const_var_mapping.entry(var.id).or_insert(next_id),
                inference_id: InferenceId::Canonical,
            },
            ty,
        );
        Ok(RewriteResult::Modified)
    }
}
impl SemanticRewriter<ImplId, NoError> for Canonicalizer<'_> {
    fn internal_rewrite(&mut self, value: &mut ImplId) -> Result<RewriteResult, NoError> {
        if value.is_var_free(self.db) {
            return Ok(RewriteResult::NoChange);
        }
        value.default_rewrite(self)
    }
}
impl SemanticRewriter<ImplLongId, NoError> for Canonicalizer<'_> {
    fn internal_rewrite(&mut self, value: &mut ImplLongId) -> Result<RewriteResult, NoError> {
        let ImplLongId::ImplVar(var_id) = value else {
            if value.is_var_free(self.db) {
                return Ok(RewriteResult::NoChange);
            }
            return value.default_rewrite(self);
        };
        let var = var_id.lookup_intern(self.db);
        if var.inference_id != self.to_canonic.source_inference_id {
            return value.default_rewrite(self);
        }
        let next_id = LocalImplVarId(self.to_canonic.impl_var_mapping.len());

        let mut var = ImplVar {
            id: *self.to_canonic.impl_var_mapping.entry(var.id).or_insert(next_id),
            inference_id: InferenceId::Canonical,
            lookup_context: var.lookup_context,
            concrete_trait_id: var.concrete_trait_id,
        };
        var.concrete_trait_id.default_rewrite(self)?;
        *value = ImplLongId::ImplVar(var.intern(self.db));
        Ok(RewriteResult::Modified)
    }
}

/// Embedder rewriter. Each canonical variable is mapped to a new inference variable.
struct Embedder<'a, 'db> {
    inference: &'a mut Inference<'db>,
    from_canonic: VarMapping,
}
impl<'a, 'db> Embedder<'a, 'db> {
    fn embed<T>(inference: &'a mut Inference<'db>, value: T) -> (T, CanonicalMapping)
    where
        Self: SemanticRewriter<T, NoError>,
    {
        let from_canonic = VarMapping::new_from_canonic(inference.inference_id);
        let mut embedder = Self { inference, from_canonic };
        let value = embedder.rewrite(value).no_err();
        let mapping = CanonicalMapping::from_from_canonic(embedder.from_canonic);
        (value, mapping)
    }
}

impl<'a> HasDb<&'a dyn SemanticGroup> for Embedder<'a, '_> {
    fn get_db(&self) -> &'a dyn SemanticGroup {
        self.inference.db
    }
}

add_basic_rewrites!(
    <'a,'b>,
    Embedder<'a,'b>,
    NoError,
    @exclude TypeLongId TypeId ConstValue ImplLongId ImplId
);

impl SemanticRewriter<TypeId, NoError> for Embedder<'_, '_> {
    fn internal_rewrite(&mut self, value: &mut TypeId) -> Result<RewriteResult, NoError> {
        if value.is_var_free(self.get_db()) {
            return Ok(RewriteResult::NoChange);
        }
        value.default_rewrite(self)
    }
}
impl SemanticRewriter<TypeLongId, NoError> for Embedder<'_, '_> {
    fn internal_rewrite(&mut self, value: &mut TypeLongId) -> Result<RewriteResult, NoError> {
        let TypeLongId::Var(var) = value else {
            return value.default_rewrite(self);
        };
        if var.inference_id != InferenceId::Canonical {
            return value.default_rewrite(self);
        }
        let new_id = self
            .from_canonic
            .type_var_mapping
            .entry(var.id)
            .or_insert_with(|| self.inference.new_type_var_raw(None).id);
        *value = TypeLongId::Var(self.inference.type_vars[new_id.0]);
        Ok(RewriteResult::Modified)
    }
}
impl SemanticRewriter<ConstValue, NoError> for Embedder<'_, '_> {
    fn internal_rewrite(&mut self, value: &mut ConstValue) -> Result<RewriteResult, NoError> {
        let ConstValue::Var(var, mut ty) = value else {
            return value.default_rewrite(self);
        };
        if var.inference_id != InferenceId::Canonical {
            return value.default_rewrite(self);
        }
        ty.default_rewrite(self)?;
        let new_id = self
            .from_canonic
            .const_var_mapping
            .entry(var.id)
            .or_insert_with(|| self.inference.new_const_var_raw(None).id);
        *value = ConstValue::Var(self.inference.const_vars[new_id.0], ty);
        Ok(RewriteResult::Modified)
    }
}
impl SemanticRewriter<ImplId, NoError> for Embedder<'_, '_> {
    fn internal_rewrite(&mut self, value: &mut ImplId) -> Result<RewriteResult, NoError> {
        if value.is_var_free(self.get_db()) {
            return Ok(RewriteResult::NoChange);
        }
        value.default_rewrite(self)
    }
}
impl SemanticRewriter<ImplLongId, NoError> for Embedder<'_, '_> {
    fn internal_rewrite(&mut self, value: &mut ImplLongId) -> Result<RewriteResult, NoError> {
        let ImplLongId::ImplVar(var_id) = value else {
            if value.is_var_free(self.get_db()) {
                return Ok(RewriteResult::NoChange);
            }
            return value.default_rewrite(self);
        };
        let var = var_id.lookup_intern(self.get_db());
        if var.inference_id != InferenceId::Canonical {
            return value.default_rewrite(self);
        }
        let concrete_trait_id = self.rewrite(var.concrete_trait_id)?;
        let new_id = self.from_canonic.impl_var_mapping.entry(var.id).or_insert_with(|| {
            self.inference.new_impl_var_raw(var.lookup_context.clone(), concrete_trait_id, None)
        });
        *value = ImplLongId::ImplVar(self.inference.impl_vars[new_id.0].intern(self.get_db()));
        Ok(RewriteResult::Modified)
    }
}

/// Mapper rewriter. Maps variables according to a given [VarMapping].
#[derive(Clone, Debug)]
pub struct MapperError(pub InferenceVar);
struct Mapper<'db> {
    db: &'db dyn SemanticGroup,
    mapping: &'db VarMapping,
}
impl<'db> Mapper<'db> {
    fn map<T>(
        db: &'db dyn SemanticGroup,
        value: T,
        mapping: &'db VarMapping,
    ) -> Result<T, MapperError>
    where
        Self: SemanticRewriter<T, MapperError>,
    {
        let mut mapper = Self { db, mapping };
        mapper.rewrite(value)
    }
}

impl<'db> HasDb<&'db dyn SemanticGroup> for Mapper<'db> {
    fn get_db(&self) -> &'db dyn SemanticGroup {
        self.db
    }
}

add_basic_rewrites!(
    <'a>,
    Mapper<'a>,
    MapperError,
    @exclude TypeLongId TypeId ImplLongId ImplId ConstValue
);

impl SemanticRewriter<TypeId, MapperError> for Mapper<'_> {
    fn internal_rewrite(&mut self, value: &mut TypeId) -> Result<RewriteResult, MapperError> {
        if value.is_var_free(self.db) {
            return Ok(RewriteResult::NoChange);
        }
        value.default_rewrite(self)
    }
}
impl SemanticRewriter<TypeLongId, MapperError> for Mapper<'_> {
    fn internal_rewrite(&mut self, value: &mut TypeLongId) -> Result<RewriteResult, MapperError> {
        let TypeLongId::Var(var) = value else {
            return value.default_rewrite(self);
        };
        let id = self
            .mapping
            .type_var_mapping
            .get(&var.id)
            .copied()
            .ok_or(MapperError(InferenceVar::Type(var.id)))?;
        *value = TypeLongId::Var(TypeVar { id, inference_id: self.mapping.target_inference_id });
        Ok(RewriteResult::Modified)
    }
}
impl SemanticRewriter<ConstValue, MapperError> for Mapper<'_> {
    fn internal_rewrite(&mut self, value: &mut ConstValue) -> Result<RewriteResult, MapperError> {
        let ConstValue::Var(var, mut ty) = value else {
            return value.default_rewrite(self);
        };
        let id = self
            .mapping
            .const_var_mapping
            .get(&var.id)
            .copied()
            .ok_or(MapperError(InferenceVar::Const(var.id)))?;
        ty.default_rewrite(self)?;
        *value =
            ConstValue::Var(ConstVar { id, inference_id: self.mapping.target_inference_id }, ty);
        Ok(RewriteResult::Modified)
    }
}
impl SemanticRewriter<ImplId, MapperError> for Mapper<'_> {
    fn internal_rewrite(&mut self, value: &mut ImplId) -> Result<RewriteResult, MapperError> {
        if value.is_var_free(self.db) {
            return Ok(RewriteResult::NoChange);
        }
        value.default_rewrite(self)
    }
}
impl SemanticRewriter<ImplLongId, MapperError> for Mapper<'_> {
    fn internal_rewrite(&mut self, value: &mut ImplLongId) -> Result<RewriteResult, MapperError> {
        let ImplLongId::ImplVar(var_id) = value else {
            return value.default_rewrite(self);
        };
        let var = var_id.lookup_intern(self.get_db());
        let id = self
            .mapping
            .impl_var_mapping
            .get(&var.id)
            .copied()
            .ok_or(MapperError(InferenceVar::Impl(var.id)))?;
        let var = ImplVar { id, inference_id: self.mapping.target_inference_id, ..var };

        *value = ImplLongId::ImplVar(var.intern(self.get_db()));
        Ok(RewriteResult::Modified)
    }
}
