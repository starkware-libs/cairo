use cairo_lang_defs::ids::{
    EnumId, ExternFunctionId, ExternTypeId, FreeFunctionId, GenericParamId, ImplAliasId, ImplDefId,
    ImplFunctionId, ImplImplDefId, LocalVarId, MemberId, ParamId, StructId, TraitConstantId,
    TraitFunctionId, TraitId, TraitImplId, TraitTypeId, VarId, VariantId,
};
use cairo_lang_proc_macros::SemanticObject;
use cairo_lang_utils::Intern;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use salsa::Database;

use super::{
    ConstVar, ImplVar, ImplVarId, ImplVarTraitItemMappings, Inference, InferenceId, InferenceVar,
    LocalConstVarId, LocalImplVarId, LocalNegativeImplVarId, LocalTypeVarId, NegativeImplVar,
    NegativeImplVarId, TypeVar,
};
use crate::items::constant::{ConstValue, ConstValueId, ImplConstantId};
use crate::items::functions::{
    ConcreteFunctionWithBody, ConcreteFunctionWithBodyId, GenericFunctionId,
    GenericFunctionWithBodyId, ImplFunctionBodyId, ImplGenericFunctionId,
    ImplGenericFunctionWithBodyId,
};
use crate::items::generics::{GenericParamConst, GenericParamImpl, GenericParamType};
use crate::items::imp::{
    GeneratedImplId, GeneratedImplItems, GeneratedImplLongId, ImplId, ImplImplId, ImplLongId,
    NegativeImplId, NegativeImplLongId, UninferredGeneratedImplId, UninferredGeneratedImplLongId,
    UninferredImpl,
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
pub struct CanonicalTrait<'db> {
    pub id: ConcreteTraitId<'db>,
    pub mappings: ImplVarTraitItemMappings<'db>,
}

impl<'db> CanonicalTrait<'db> {
    /// Canonicalizes a concrete trait that is part of an [Inference].
    pub fn canonicalize(
        db: &'db dyn Database,
        source_inference_id: InferenceId<'db>,
        trait_id: ConcreteTraitId<'db>,
        impl_var_mappings: ImplVarTraitItemMappings<'db>,
    ) -> (Self, CanonicalMapping<'db>) {
        Canonicalizer::canonicalize(
            db,
            source_inference_id,
            Self { id: trait_id, mappings: impl_var_mappings },
        )
    }
    /// Embeds a canonical trait into an [Inference].
    pub fn embed(
        &self,
        inference: &mut Inference<'db, '_>,
    ) -> (CanonicalTrait<'db>, CanonicalMapping<'db>) {
        Embedder::embed(inference, self.clone())
    }
}

/// A solution for a [CanonicalTrait].
#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug, salsa::Update)]
pub struct CanonicalImpl<'db>(pub ImplId<'db>);
impl<'db> CanonicalImpl<'db> {
    /// Canonicalizes a concrete impl that is part of an [Inference].
    /// Uses the same canonicalization of the trait, to be consistent.
    pub fn canonicalize(
        db: &'db dyn Database,
        impl_id: ImplId<'db>,
        mapping: &CanonicalMapping<'db>,
    ) -> Result<Self, MapperError> {
        Ok(Self(Mapper::map(db, impl_id, &mapping.to_canonic)?))
    }
    /// Embeds a canonical impl into an [Inference].
    /// Uses the same embedding of the trait, to be consistent.
    pub fn embed(
        &self,
        inference: &Inference<'db, '_>,
        mapping: &CanonicalMapping<'db>,
    ) -> ImplId<'db> {
        Mapper::map(inference.db, self.0, &mapping.from_canonic)
            .expect("Tried to embed a non canonical impl")
    }
}

/// Mapping between canonical space and inference space.
/// Created by either canonicalizing or embedding a trait.
#[derive(Debug)]
pub struct CanonicalMapping<'db> {
    to_canonic: VarMapping<'db>,
    from_canonic: VarMapping<'db>,
}
impl<'db> CanonicalMapping<'db> {
    fn from_to_canonic(to_canonic: VarMapping<'db>) -> CanonicalMapping<'db> {
        let from_canonic = VarMapping {
            type_var_mapping: to_canonic.type_var_mapping.iter().map(|(k, v)| (*v, *k)).collect(),
            const_var_mapping: to_canonic.const_var_mapping.iter().map(|(k, v)| (*v, *k)).collect(),
            impl_var_mapping: to_canonic.impl_var_mapping.iter().map(|(k, v)| (*v, *k)).collect(),
            negative_impl_var_mapping: to_canonic
                .negative_impl_var_mapping
                .iter()
                .map(|(k, v)| (*v, *k))
                .collect(),
            source_inference_id: to_canonic.target_inference_id,
            target_inference_id: to_canonic.source_inference_id,
        };
        Self { to_canonic, from_canonic }
    }
    fn from_from_canonic(from_canonic: VarMapping<'db>) -> CanonicalMapping<'db> {
        let to_canonic = VarMapping {
            type_var_mapping: from_canonic.type_var_mapping.iter().map(|(k, v)| (*v, *k)).collect(),
            const_var_mapping: from_canonic
                .const_var_mapping
                .iter()
                .map(|(k, v)| (*v, *k))
                .collect(),
            impl_var_mapping: from_canonic.impl_var_mapping.iter().map(|(k, v)| (*v, *k)).collect(),
            negative_impl_var_mapping: from_canonic
                .negative_impl_var_mapping
                .iter()
                .map(|(k, v)| (*v, *k))
                .collect(),
            source_inference_id: from_canonic.target_inference_id,
            target_inference_id: from_canonic.source_inference_id,
        };
        Self { to_canonic, from_canonic }
    }
}

// Mappings.
#[derive(Debug)]
pub struct VarMapping<'db> {
    type_var_mapping: OrderedHashMap<LocalTypeVarId, LocalTypeVarId>,
    const_var_mapping: OrderedHashMap<LocalConstVarId, LocalConstVarId>,
    impl_var_mapping: OrderedHashMap<LocalImplVarId, LocalImplVarId>,
    negative_impl_var_mapping: OrderedHashMap<LocalNegativeImplVarId, LocalNegativeImplVarId>,
    source_inference_id: InferenceId<'db>,
    target_inference_id: InferenceId<'db>,
}
impl<'db> VarMapping<'db> {
    fn new_to_canonic(source_inference_id: InferenceId<'db>) -> Self {
        Self {
            type_var_mapping: OrderedHashMap::default(),
            const_var_mapping: OrderedHashMap::default(),
            impl_var_mapping: OrderedHashMap::default(),
            negative_impl_var_mapping: OrderedHashMap::default(),
            source_inference_id,
            target_inference_id: InferenceId::Canonical,
        }
    }
    fn new_from_canonic(target_inference_id: InferenceId<'db>) -> Self {
        Self {
            type_var_mapping: OrderedHashMap::default(),
            const_var_mapping: OrderedHashMap::default(),
            impl_var_mapping: OrderedHashMap::default(),
            negative_impl_var_mapping: OrderedHashMap::default(),
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
    db: &'db dyn Database,
    to_canonic: VarMapping<'db>,
}
impl<'db> Canonicalizer<'db> {
    fn canonicalize<T>(
        db: &'db dyn Database,
        source_inference_id: InferenceId<'db>,
        value: T,
    ) -> (T, CanonicalMapping<'db>)
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
impl<'a> HasDb<&'a dyn Database> for Canonicalizer<'a> {
    fn get_db(&self) -> &'a dyn Database {
        self.db
    }
}

add_basic_rewrites!(
    <'a>,
    Canonicalizer<'a>,
    NoError,
    @exclude TypeLongId TypeId ImplLongId ImplId ConstValue NegativeImplLongId NegativeImplId
);

impl<'db> SemanticRewriter<TypeId<'db>, NoError> for Canonicalizer<'db> {
    fn internal_rewrite(&mut self, value: &mut TypeId<'db>) -> Result<RewriteResult, NoError> {
        if value.is_var_free(self.db) {
            return Ok(RewriteResult::NoChange);
        }
        value.default_rewrite(self)
    }
}
impl<'db> SemanticRewriter<TypeLongId<'db>, NoError> for Canonicalizer<'db> {
    fn internal_rewrite(&mut self, value: &mut TypeLongId<'db>) -> Result<RewriteResult, NoError> {
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
impl<'db> SemanticRewriter<ConstValue<'db>, NoError> for Canonicalizer<'db> {
    fn internal_rewrite(&mut self, value: &mut ConstValue<'db>) -> Result<RewriteResult, NoError> {
        let ConstValue::Var(var, ty) = value else {
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
            *ty,
        );
        Ok(RewriteResult::Modified)
    }
}
impl<'db> SemanticRewriter<ImplId<'db>, NoError> for Canonicalizer<'db> {
    fn internal_rewrite(&mut self, value: &mut ImplId<'db>) -> Result<RewriteResult, NoError> {
        if value.is_var_free(self.db) {
            return Ok(RewriteResult::NoChange);
        }
        value.default_rewrite(self)
    }
}
impl<'db> SemanticRewriter<ImplLongId<'db>, NoError> for Canonicalizer<'db> {
    fn internal_rewrite(&mut self, value: &mut ImplLongId<'db>) -> Result<RewriteResult, NoError> {
        let ImplLongId::ImplVar(var_id) = value else {
            if value.is_var_free(self.db) {
                return Ok(RewriteResult::NoChange);
            }
            return value.default_rewrite(self);
        };
        let var = var_id.long(self.db);
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
impl<'db> SemanticRewriter<NegativeImplId<'db>, NoError> for Canonicalizer<'db> {
    fn internal_rewrite(
        &mut self,
        value: &mut NegativeImplId<'db>,
    ) -> Result<RewriteResult, NoError> {
        if value.is_var_free(self.db) {
            return Ok(RewriteResult::NoChange);
        }
        value.default_rewrite(self)
    }
}
impl<'db> SemanticRewriter<NegativeImplLongId<'db>, NoError> for Canonicalizer<'db> {
    fn internal_rewrite(
        &mut self,
        value: &mut NegativeImplLongId<'db>,
    ) -> Result<RewriteResult, NoError> {
        let NegativeImplLongId::NegativeImplVar(var_id) = value else {
            if value.is_var_free(self.db) {
                return Ok(RewriteResult::NoChange);
            }
            return value.default_rewrite(self);
        };
        let var = var_id.long(self.db);
        if var.inference_id != self.to_canonic.source_inference_id {
            return value.default_rewrite(self);
        }
        let next_id = LocalNegativeImplVarId(self.to_canonic.negative_impl_var_mapping.len());

        let mut var = NegativeImplVar {
            id: *self.to_canonic.negative_impl_var_mapping.entry(var.id).or_insert(next_id),
            inference_id: InferenceId::Canonical,
            lookup_context: var.lookup_context,
            concrete_trait_id: var.concrete_trait_id,
        };
        var.concrete_trait_id.default_rewrite(self)?;
        *value = NegativeImplLongId::NegativeImplVar(var.intern(self.db));
        Ok(RewriteResult::Modified)
    }
}

/// Embedder rewriter. Each canonical variable is mapped to a new inference variable.
struct Embedder<'db, 'id, 'mt> {
    inference: &'mt mut Inference<'db, 'id>,
    from_canonic: VarMapping<'db>,
}
impl<'db, 'id, 'mt> Embedder<'db, 'id, 'mt> {
    fn embed<T>(inference: &'mt mut Inference<'db, 'id>, value: T) -> (T, CanonicalMapping<'db>)
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

impl<'db> HasDb<&'db dyn Database> for Embedder<'db, '_, '_> {
    fn get_db(&self) -> &'db dyn Database {
        self.inference.db
    }
}

add_basic_rewrites!(
    <'a, 'id, 'mt>,
    Embedder<'a, 'id, 'mt>,
    NoError,
    @exclude TypeLongId TypeId ConstValue ImplLongId ImplId NegativeImplLongId NegativeImplId
);

impl<'db> SemanticRewriter<TypeId<'db>, NoError> for Embedder<'db, '_, '_> {
    fn internal_rewrite(&mut self, value: &mut TypeId<'db>) -> Result<RewriteResult, NoError> {
        if value.is_var_free(self.get_db()) {
            return Ok(RewriteResult::NoChange);
        }
        value.default_rewrite(self)
    }
}
impl<'db> SemanticRewriter<TypeLongId<'db>, NoError> for Embedder<'db, '_, '_> {
    fn internal_rewrite(&mut self, value: &mut TypeLongId<'db>) -> Result<RewriteResult, NoError> {
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
impl<'db> SemanticRewriter<ConstValue<'db>, NoError> for Embedder<'db, '_, '_> {
    fn internal_rewrite(&mut self, value: &mut ConstValue<'db>) -> Result<RewriteResult, NoError> {
        let ConstValue::Var(var, ty) = value else {
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
        *value = ConstValue::Var(self.inference.const_vars[new_id.0], *ty);
        Ok(RewriteResult::Modified)
    }
}
impl<'db> SemanticRewriter<ImplId<'db>, NoError> for Embedder<'db, '_, '_> {
    fn internal_rewrite(&mut self, value: &mut ImplId<'db>) -> Result<RewriteResult, NoError> {
        if value.is_var_free(self.get_db()) {
            return Ok(RewriteResult::NoChange);
        }
        value.default_rewrite(self)
    }
}
impl<'db> SemanticRewriter<ImplLongId<'db>, NoError> for Embedder<'db, '_, '_> {
    fn internal_rewrite(&mut self, value: &mut ImplLongId<'db>) -> Result<RewriteResult, NoError> {
        let ImplLongId::ImplVar(var_id) = value else {
            if value.is_var_free(self.get_db()) {
                return Ok(RewriteResult::NoChange);
            }
            return value.default_rewrite(self);
        };
        let var = var_id.long(self.get_db());
        if var.inference_id != InferenceId::Canonical {
            return value.default_rewrite(self);
        }
        let concrete_trait_id = self.rewrite(var.concrete_trait_id)?;
        let new_id = self.from_canonic.impl_var_mapping.entry(var.id).or_insert_with(|| {
            self.inference.new_impl_var_raw(var.lookup_context, concrete_trait_id, None)
        });
        *value =
            ImplLongId::ImplVar(self.inference.impl_vars[new_id.0].clone().intern(self.get_db()));
        Ok(RewriteResult::Modified)
    }
}
impl<'db> SemanticRewriter<NegativeImplId<'db>, NoError> for Embedder<'db, '_, '_> {
    fn internal_rewrite(
        &mut self,
        value: &mut NegativeImplId<'db>,
    ) -> Result<RewriteResult, NoError> {
        if value.is_var_free(self.get_db()) {
            return Ok(RewriteResult::NoChange);
        }
        value.default_rewrite(self)
    }
}
impl<'db> SemanticRewriter<NegativeImplLongId<'db>, NoError> for Embedder<'db, '_, '_> {
    fn internal_rewrite(
        &mut self,
        value: &mut NegativeImplLongId<'db>,
    ) -> Result<RewriteResult, NoError> {
        let NegativeImplLongId::NegativeImplVar(var_id) = value else {
            if value.is_var_free(self.get_db()) {
                return Ok(RewriteResult::NoChange);
            }
            return value.default_rewrite(self);
        };
        let var = var_id.long(self.get_db());
        if var.inference_id != InferenceId::Canonical {
            return value.default_rewrite(self);
        }
        let concrete_trait_id = self.rewrite(var.concrete_trait_id)?;
        let new_id =
            self.from_canonic.negative_impl_var_mapping.entry(var.id).or_insert_with(|| {
                self.inference.new_negative_impl_var_raw(
                    var.lookup_context,
                    concrete_trait_id,
                    None,
                )
            });
        *value = NegativeImplLongId::NegativeImplVar(
            self.inference.negative_impl_vars[new_id.0].clone().intern(self.get_db()),
        );
        Ok(RewriteResult::Modified)
    }
}

/// Mapper rewriter. Maps variables according to a given [VarMapping].
#[derive(Clone, Debug)]
pub struct MapperError(pub InferenceVar);
struct Mapper<'db, 'v> {
    db: &'db dyn Database,
    mapping: &'v VarMapping<'db>,
}
impl<'db, 'v> Mapper<'db, 'v> {
    fn map<T>(
        db: &'db dyn Database,
        value: T,
        mapping: &'v VarMapping<'db>,
    ) -> Result<T, MapperError>
    where
        Self: SemanticRewriter<T, MapperError>,
    {
        let mut mapper = Self { db, mapping };
        mapper.rewrite(value)
    }
}

impl<'db> HasDb<&'db dyn Database> for Mapper<'db, '_> {
    fn get_db(&self) -> &'db dyn Database {
        self.db
    }
}

add_basic_rewrites!(
    <'a>,
    Mapper<'a, '_>,
    MapperError,
    @exclude TypeLongId TypeId ImplLongId ImplId ConstValue NegativeImplLongId NegativeImplId
);

impl<'db> SemanticRewriter<TypeId<'db>, MapperError> for Mapper<'db, '_> {
    fn internal_rewrite(&mut self, value: &mut TypeId<'db>) -> Result<RewriteResult, MapperError> {
        if value.is_var_free(self.db) {
            return Ok(RewriteResult::NoChange);
        }
        value.default_rewrite(self)
    }
}
impl<'db> SemanticRewriter<TypeLongId<'db>, MapperError> for Mapper<'db, '_> {
    fn internal_rewrite(
        &mut self,
        value: &mut TypeLongId<'db>,
    ) -> Result<RewriteResult, MapperError> {
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
impl<'db> SemanticRewriter<ConstValue<'db>, MapperError> for Mapper<'db, '_> {
    fn internal_rewrite(
        &mut self,
        value: &mut ConstValue<'db>,
    ) -> Result<RewriteResult, MapperError> {
        let ConstValue::Var(var, ty) = value else {
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
            ConstValue::Var(ConstVar { id, inference_id: self.mapping.target_inference_id }, *ty);
        Ok(RewriteResult::Modified)
    }
}
impl<'db> SemanticRewriter<ImplId<'db>, MapperError> for Mapper<'db, '_> {
    fn internal_rewrite(&mut self, value: &mut ImplId<'db>) -> Result<RewriteResult, MapperError> {
        if value.is_var_free(self.db) {
            return Ok(RewriteResult::NoChange);
        }
        value.default_rewrite(self)
    }
}
impl<'db> SemanticRewriter<ImplLongId<'db>, MapperError> for Mapper<'db, '_> {
    fn internal_rewrite(
        &mut self,
        value: &mut ImplLongId<'db>,
    ) -> Result<RewriteResult, MapperError> {
        let ImplLongId::ImplVar(var_id) = value else {
            return value.default_rewrite(self);
        };
        let var = var_id.long(self.get_db());
        let id = self
            .mapping
            .impl_var_mapping
            .get(&var.id)
            .copied()
            .ok_or(MapperError(InferenceVar::Impl(var.id)))?;
        let var = ImplVar { id, inference_id: self.mapping.target_inference_id, ..var.clone() };

        *value = ImplLongId::ImplVar(var.intern(self.get_db()));
        Ok(RewriteResult::Modified)
    }
}
impl<'db> SemanticRewriter<NegativeImplId<'db>, MapperError> for Mapper<'db, '_> {
    fn internal_rewrite(
        &mut self,
        value: &mut NegativeImplId<'db>,
    ) -> Result<RewriteResult, MapperError> {
        if value.is_var_free(self.db) {
            return Ok(RewriteResult::NoChange);
        }
        value.default_rewrite(self)
    }
}
impl<'db> SemanticRewriter<NegativeImplLongId<'db>, MapperError> for Mapper<'db, '_> {
    fn internal_rewrite(
        &mut self,
        value: &mut NegativeImplLongId<'db>,
    ) -> Result<RewriteResult, MapperError> {
        let NegativeImplLongId::NegativeImplVar(var_id) = value else {
            return value.default_rewrite(self);
        };
        let var = var_id.long(self.get_db());
        let id = self
            .mapping
            .negative_impl_var_mapping
            .get(&var.id)
            .copied()
            .ok_or(MapperError(InferenceVar::NegativeImpl(var.id)))?;
        let var =
            NegativeImplVar { id, inference_id: self.mapping.target_inference_id, ..var.clone() };

        *value = NegativeImplLongId::NegativeImplVar(var.intern(self.get_db()));
        Ok(RewriteResult::Modified)
    }
}
