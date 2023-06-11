use cairo_lang_defs::ids::{
    EnumId, ExternFunctionId, ExternTypeId, FreeFunctionId, GenericParamId, ImplAliasId, ImplDefId,
    ImplFunctionId, LocalVarId, MemberId, ParamId, StructId, TraitFunctionId, TraitId, VarId,
    VariantId,
};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use super::{ImplVar, ImplVarId, Inference, InferenceVar, LocalImplVarId, LocalTypeVarId, TypeVar};
use crate::db::SemanticGroup;
use crate::items::functions::{
    ConcreteFunctionWithBody, ConcreteFunctionWithBodyId, GenericFunctionId,
    GenericFunctionWithBodyId, ImplGenericFunctionId, ImplGenericFunctionWithBodyId,
};
use crate::items::generics::{GenericParamConst, GenericParamImpl, GenericParamType};
use crate::items::imp::{ImplId, UninferredImpl};
use crate::items::trt::{ConcreteTraitGenericFunctionId, ConcreteTraitGenericFunctionLongId};
use crate::literals::LiteralId;
use crate::substitution::{HasDb, SemanticObject, SemanticRewriter};
use crate::types::{ConcreteEnumLongId, ConcreteExternTypeLongId, ConcreteStructLongId};
use crate::{
    add_basic_rewrites, ConcreteEnumId, ConcreteExternTypeId, ConcreteFunction, ConcreteImplId,
    ConcreteImplLongId, ConcreteStructId, ConcreteTraitId, ConcreteTraitLongId, ConcreteTypeId,
    ConcreteVariant, ExprVar, ExprVarMemberPath, FunctionId, FunctionLongId, GenericArgumentId,
    GenericParam, Parameter, Signature, TypeId, TypeLongId,
};

/// A canonical representation of a concrete trait that needs to be solved.
#[derive(Copy, Clone, PartialEq, Hash, Eq, Debug)]
pub struct CanonicalTrait(pub ConcreteTraitId);
impl CanonicalTrait {
    /// Canonicalizes a concrete trait that is part of an [Inference].
    pub fn canonicalize(
        db: &dyn SemanticGroup,
        trait_id: ConcreteTraitId,
    ) -> (Self, CanonicalMapping) {
        let (t, mapping) = Canonicalizer::canonicalize(db, trait_id);
        (Self(t), mapping)
    }
    /// Embeds a canonical trait into an [Inference].
    pub fn embed(&self, inference: &mut Inference<'_>) -> (ConcreteTraitId, CanonicalMapping) {
        Embedder::embed(inference, self.0)
    }
}

/// A solution for a [CanonicalTrait].
#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct CanonicalImpl(pub ImplId);
impl CanonicalImpl {
    /// Canonicalizes a concrete impl that is part of an [Inference].
    /// Uses the same same canonicalization of the trait, to be consistent.
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
/// Created by a either canonicalizing or embedding a trait.
#[derive(Debug)]
pub struct CanonicalMapping {
    to_canonic: VarMapping,
    from_canonic: VarMapping,
}
impl CanonicalMapping {
    fn from_to_canonic(to_canonic: VarMapping) -> CanonicalMapping {
        let from_canonic = VarMapping {
            type_var_mapping: to_canonic.type_var_mapping.iter().map(|(k, v)| (*v, *k)).collect(),
            impl_var_mapping: to_canonic.impl_var_mapping.iter().map(|(k, v)| (*v, *k)).collect(),
        };
        Self { to_canonic, from_canonic }
    }
    fn from_from_canonic(from_canonic: VarMapping) -> CanonicalMapping {
        let to_canonic = VarMapping {
            type_var_mapping: from_canonic.type_var_mapping.iter().map(|(k, v)| (*v, *k)).collect(),
            impl_var_mapping: from_canonic.impl_var_mapping.iter().map(|(k, v)| (*v, *k)).collect(),
        };
        Self { to_canonic, from_canonic }
    }
}

// Mappings.
#[derive(Default, Debug)]
pub struct VarMapping {
    type_var_mapping: OrderedHashMap<LocalTypeVarId, LocalTypeVarId>,
    impl_var_mapping: OrderedHashMap<LocalImplVarId, LocalImplVarId>,
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
    fn canonicalize<T>(db: &'db dyn SemanticGroup, value: T) -> (T, CanonicalMapping)
    where
        Self: SemanticRewriter<T, NoError>,
    {
        let mut canonicalizer = Self { db, to_canonic: Default::default() };
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
add_basic_rewrites!(<'a>, Canonicalizer<'a>, NoError, @exclude TypeLongId ImplId);
impl<'a> SemanticRewriter<TypeLongId, NoError> for Canonicalizer<'a> {
    fn rewrite(&mut self, value: TypeLongId) -> Result<TypeLongId, NoError> {
        let TypeLongId::Var(var) = value else { return value.default_rewrite(self); };
        let next_id = LocalTypeVarId(self.to_canonic.type_var_mapping.len());
        Ok(TypeLongId::Var(TypeVar {
            id: *self.to_canonic.type_var_mapping.entry(var.id).or_insert(next_id),
        }))
    }
}
impl<'a> SemanticRewriter<ImplId, NoError> for Canonicalizer<'a> {
    fn rewrite(&mut self, value: ImplId) -> Result<ImplId, NoError> {
        let ImplId::ImplVar(var_id) = value else { return value.default_rewrite(self); };
        let var = var_id.get(self.db);
        let next_id = LocalImplVarId(self.to_canonic.impl_var_mapping.len());
        let var = ImplVar {
            id: *self.to_canonic.impl_var_mapping.entry(var.id).or_insert(next_id),
            ..var
        };
        Ok(ImplId::ImplVar(var.intern(self.db)))
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
        let mut embedder = Self { inference, from_canonic: Default::default() };
        let value = embedder.rewrite(value).no_err();
        let mapping = CanonicalMapping::from_from_canonic(embedder.from_canonic);
        (value, mapping)
    }
}

impl<'a, 'b> HasDb<&'a dyn SemanticGroup> for Embedder<'a, 'b> {
    fn get_db(&self) -> &'a dyn SemanticGroup {
        self.inference.db
    }
}
add_basic_rewrites!(<'a,'b>, Embedder<'a,'b>, NoError, @exclude TypeLongId ImplId);
impl<'a, 'b> SemanticRewriter<TypeLongId, NoError> for Embedder<'a, 'b> {
    fn rewrite(&mut self, value: TypeLongId) -> Result<TypeLongId, NoError> {
        let TypeLongId::Var(var) = value else { return value.default_rewrite(self); };
        Ok(TypeLongId::Var(TypeVar {
            id: *self
                .from_canonic
                .type_var_mapping
                .entry(var.id)
                .or_insert_with(|| self.inference.new_type_var_raw(None).id),
        }))
    }
}
impl<'a, 'b> SemanticRewriter<ImplId, NoError> for Embedder<'a, 'b> {
    fn rewrite(&mut self, value: ImplId) -> Result<ImplId, NoError> {
        let ImplId::ImplVar(var_id) = value else { return value.default_rewrite(self); };
        let var = var_id.get(self.get_db());
        let concrete_trait_id = self.rewrite(var.concrete_trait_id)?;
        let var = ImplVar {
            id: *self.from_canonic.impl_var_mapping.entry(var.id).or_insert_with(|| {
                self.inference.new_impl_var_raw(var.lookup_context.clone(), concrete_trait_id, None)
            }),
            ..var
        };
        Ok(ImplId::ImplVar(var.intern(self.get_db())))
    }
}

/// Mapper rewriter. Maps variables according to a given [VarMapping].
#[derive(Debug)]
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
add_basic_rewrites!(<'a>, Mapper<'a>, MapperError, @exclude TypeLongId ImplId);
impl<'db> SemanticRewriter<TypeLongId, MapperError> for Mapper<'db> {
    fn rewrite(&mut self, value: TypeLongId) -> Result<TypeLongId, MapperError> {
        let TypeLongId::Var(var) = value else { return value.default_rewrite(self); };
        let id = self
            .mapping
            .type_var_mapping
            .get(&var.id)
            .copied()
            .ok_or(MapperError(InferenceVar::Type(var.id)))?;
        Ok(TypeLongId::Var(TypeVar { id }))
    }
}
impl<'db> SemanticRewriter<ImplId, MapperError> for Mapper<'db> {
    fn rewrite(&mut self, value: ImplId) -> Result<ImplId, MapperError> {
        let ImplId::ImplVar(var_id) = value else { return value.default_rewrite(self); };
        let var = var_id.get(self.get_db());
        let id = self
            .mapping
            .impl_var_mapping
            .get(&var.id)
            .copied()
            .ok_or(MapperError(InferenceVar::Impl(var.id)))?;
        let var = ImplVar { id, ..var };
        Ok(ImplId::ImplVar(var.intern(self.get_db())))
    }
}
