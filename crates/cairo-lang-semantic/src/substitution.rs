use std::collections::VecDeque;
use std::hash::Hash;
use std::ops::{Deref, DerefMut};

use cairo_lang_defs::ids::{
    EnumId, ExternFunctionId, ExternTypeId, FreeFunctionId, GenericParamId, ImplAliasId, ImplDefId,
    ImplFunctionId, ImplImplDefId, LanguageElementId, LocalVarId, MemberId, ParamId, StructId,
    TraitConstantId, TraitFunctionId, TraitId, TraitImplId, TraitTypeId, VariantId,
};
use cairo_lang_diagnostics::{DiagnosticAdded, Maybe};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::{LookupIntern, extract_matches};
use itertools::zip_eq;

use crate::db::SemanticGroup;
use crate::expr::inference::canonic::CanonicalTrait;
use crate::expr::inference::{
    ConstVar, ImplVar, ImplVarId, ImplVarTraitItemMappings, InferenceId, InferenceVar,
    LocalConstVarId, LocalImplVarId, LocalTypeVarId, TypeVar,
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
    UninferredGeneratedImplId, UninferredGeneratedImplLongId, UninferredImpl,
};
use crate::items::trt::{
    ConcreteTraitGenericFunctionId, ConcreteTraitGenericFunctionLongId, ConcreteTraitTypeId,
    ConcreteTraitTypeLongId,
};
use crate::types::{
    ClosureTypeLongId, ConcreteEnumLongId, ConcreteExternTypeLongId, ConcreteStructLongId,
    ImplTypeId,
};
use crate::{
    ConcreteEnumId, ConcreteExternTypeId, ConcreteFunction, ConcreteImplId, ConcreteImplLongId,
    ConcreteStructId, ConcreteTraitId, ConcreteTraitLongId, ConcreteTypeId, ConcreteVariant,
    ExprId, ExprVar, ExprVarMemberPath, FunctionId, FunctionLongId, GenericArgumentId,
    GenericParam, MatchArmSelector, Parameter, Signature, TypeId, TypeLongId, ValueSelectorArm,
    VarId,
};

pub enum RewriteResult {
    Modified,
    NoChange,
}

/// A substitution of generic arguments in generic parameters as well as the `Self` of traits. Used
/// for concretization.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct GenericSubstitution {
    param_to_arg: OrderedHashMap<GenericParamId, GenericArgumentId>,
    self_impl: Option<ImplId>,
}
impl GenericSubstitution {
    pub fn from_impl(self_impl: ImplId) -> Self {
        GenericSubstitution { param_to_arg: OrderedHashMap::default(), self_impl: Some(self_impl) }
    }
    pub fn new(generic_params: &[GenericParam], generic_args: &[GenericArgumentId]) -> Self {
        GenericSubstitution {
            param_to_arg: zip_eq(generic_params, generic_args)
                .map(|(param, arg)| (param.id(), *arg))
                .collect(),
            self_impl: None,
        }
    }
    pub fn concat(mut self, other: GenericSubstitution) -> Self {
        for (key, value) in other.param_to_arg.into_iter() {
            self.param_to_arg.insert(key, value);
        }
        if let Some(self_impl) = other.self_impl {
            self.self_impl = Some(self_impl);
        }
        self
    }
    pub fn substitute<'a, Obj>(&'a self, db: &'a dyn SemanticGroup, obj: Obj) -> Maybe<Obj>
    where
        SubstitutionRewriter<'a>: SemanticRewriter<Obj, DiagnosticAdded>,
    {
        SubstitutionRewriter { db: db.upcast(), substitution: self }.rewrite(obj)
    }
}
impl Deref for GenericSubstitution {
    type Target = OrderedHashMap<GenericParamId, GenericArgumentId>;

    fn deref(&self) -> &Self::Target {
        &self.param_to_arg
    }
}
impl DerefMut for GenericSubstitution {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.param_to_arg
    }
}
#[allow(clippy::derived_hash_with_manual_eq)]
impl std::hash::Hash for GenericSubstitution {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.param_to_arg.len().hash(state);
        for e in self.param_to_arg.iter() {
            e.hash(state);
        }
    }
}

#[macro_export]
macro_rules! semantic_object_for_id {
    ($name:ident, $lookup:ident, $intern:ident, $long_ty:ident) => {
        impl<
            'a,
            Error,
            TRewriter: $crate::substitution::HasDb<&'a dyn $crate::db::SemanticGroup>
                + $crate::substitution::SemanticRewriter<$long_ty, Error>,
        > $crate::substitution::SemanticObject<TRewriter, Error> for $name
        {
            fn default_rewrite(
                &mut self,
                rewriter: &mut TRewriter,
            ) -> Result<$crate::substitution::RewriteResult, Error> where {
                let db = $crate::substitution::HasDb::get_db(rewriter);
                let mut val = db.$lookup(*self);
                Ok(
                    match $crate::substitution::SemanticRewriter::internal_rewrite(
                        rewriter, &mut val,
                    )? {
                        $crate::substitution::RewriteResult::Modified => {
                            *self = db.$intern(val);
                            $crate::substitution::RewriteResult::Modified
                        }
                        $crate::substitution::RewriteResult::NoChange => {
                            $crate::substitution::RewriteResult::NoChange
                        }
                    },
                )
            }
        }
    };
}

#[macro_export]
macro_rules! add_rewrite {
    (<$($generics:lifetime),*>, $self_ty:ty, $err_ty:ty, $ty:ident) => {
        impl <$($generics),*> SemanticRewriter<$ty, $err_ty> for $self_ty {
            fn internal_rewrite(
                &mut self,
                value: &mut $ty
            ) -> Result<$crate::substitution::RewriteResult, $err_ty> {
                $crate::substitution::SemanticObject::default_rewrite(value, self)
            }
        }
    };
}

#[macro_export]
macro_rules! add_rewrite_identity {
    (<$($generics:lifetime),*>, $self_ty:ty, $err_ty:ty, $ty:ident) => {
        impl <$($generics),*> SemanticRewriter<$ty, $err_ty> for $self_ty {
            fn internal_rewrite(
                &mut self,
                _value: &mut $ty
            ) -> Result<$crate::substitution::RewriteResult, $err_ty> {
                Ok(RewriteResult::NoChange)
            }
        }
    };
}

pub trait SemanticObject<TRewriter, Error>: Sized {
    fn default_rewrite(&mut self, rewriter: &mut TRewriter) -> Result<RewriteResult, Error>;
}
impl<T: Clone, E, TRewriter: SemanticRewriter<T, E>> SemanticRewriter<Vec<T>, E> for TRewriter {
    fn internal_rewrite(&mut self, value: &mut Vec<T>) -> Result<RewriteResult, E> {
        let mut result = RewriteResult::NoChange;
        for el in value.iter_mut() {
            match self.internal_rewrite(el)? {
                RewriteResult::Modified => {
                    result = RewriteResult::Modified;
                }
                RewriteResult::NoChange => {}
            }
        }

        Ok(result)
    }
}
impl<T, E, TRewriter: SemanticRewriter<T, E>> SemanticRewriter<VecDeque<T>, E> for TRewriter {
    fn internal_rewrite(&mut self, value: &mut VecDeque<T>) -> Result<RewriteResult, E> {
        let mut result = RewriteResult::NoChange;
        for el in value.iter_mut() {
            match self.internal_rewrite(el)? {
                RewriteResult::Modified => {
                    result = RewriteResult::Modified;
                }
                RewriteResult::NoChange => {}
            }
        }

        Ok(result)
    }
}
impl<T, E, TRewriter: SemanticRewriter<T, E>> SemanticRewriter<Box<T>, E> for TRewriter {
    fn internal_rewrite(&mut self, value: &mut Box<T>) -> Result<RewriteResult, E> {
        self.internal_rewrite(value.as_mut())
    }
}

impl<K: Hash + Eq + LanguageElementId, V: Clone, E, TRewriter: SemanticRewriter<V, E>>
    SemanticRewriter<OrderedHashMap<K, V>, E> for TRewriter
{
    fn internal_rewrite(&mut self, value: &mut OrderedHashMap<K, V>) -> Result<RewriteResult, E> {
        let mut result = RewriteResult::NoChange;
        for (_, v) in value.iter_mut() {
            match self.internal_rewrite(v)? {
                RewriteResult::Modified => {
                    result = RewriteResult::Modified;
                }
                RewriteResult::NoChange => {}
            }
        }
        Ok(result)
    }
}
impl<T0, T1, E, TRewriter: SemanticRewriter<T0, E> + SemanticRewriter<T1, E>>
    SemanticRewriter<(T0, T1), E> for TRewriter
{
    fn internal_rewrite(&mut self, value: &mut (T0, T1)) -> Result<RewriteResult, E> {
        match (self.internal_rewrite(&mut value.0)?, self.internal_rewrite(&mut value.1)?) {
            (RewriteResult::NoChange, RewriteResult::NoChange) => Ok(RewriteResult::NoChange),
            _ => Ok(RewriteResult::Modified),
        }
    }
}
impl<T, E, TRewriter: SemanticRewriter<T, E>> SemanticRewriter<Option<T>, E> for TRewriter {
    fn internal_rewrite(&mut self, value: &mut Option<T>) -> Result<RewriteResult, E> {
        Ok(match value {
            Some(val) => self.internal_rewrite(val)?,
            None => RewriteResult::NoChange,
        })
    }
}
impl<T, E, TRewriter: SemanticRewriter<T, E>, E2> SemanticRewriter<Result<T, E2>, E> for TRewriter {
    fn internal_rewrite(&mut self, value: &mut Result<T, E2>) -> Result<RewriteResult, E> {
        Ok(match value {
            Ok(val) => self.internal_rewrite(val)?,
            Err(_) => RewriteResult::NoChange,
        })
    }
}
pub trait HasDb<T> {
    fn get_db(&self) -> T;
}
pub trait SemanticRewriter<T, Error> {
    fn rewrite(&mut self, mut value: T) -> Result<T, Error> {
        self.internal_rewrite(&mut value)?;
        Ok(value)
    }

    fn internal_rewrite(&mut self, value: &mut T) -> Result<RewriteResult, Error>;
}

#[macro_export]
macro_rules! prune_single {
    ($macro:ident, $item:ident, ) => {$macro!($item);};
    ($macro:ident, $item:ident, $item0:ident $($item_rest:ident)*) => {
        macro_rules! __inner_helper {
            // Identifiers equal, skip.
            ($item $item) => { };
            // Identifiers not equal, continue scanning.
            ($item $item0) => { $crate::prune_single!($macro, $item, $($item_rest)*); };
        }
        __inner_helper!($item $item0);
    }
}

#[macro_export]
macro_rules! add_basic_rewrites {
    (<$($generics:lifetime),*>, $self_ty:ty, $err_ty:ty, @exclude $($exclude:ident)*) => {
        macro_rules! __identity_helper {
            ($item:ident) => {
                $crate::add_rewrite_identity!(<$($generics),*>, $self_ty, $err_ty, $item);
            }
        }
        macro_rules! __regular_helper {
            ($item:ident) => { $crate::add_rewrite!(<$($generics),*>, $self_ty, $err_ty, $item); }
        }

        $crate::prune_single!(__identity_helper, InferenceId, $($exclude)*);
        $crate::prune_single!(__identity_helper, ParamId, $($exclude)*);
        $crate::prune_single!(__identity_helper, FreeFunctionId, $($exclude)*);
        $crate::prune_single!(__identity_helper, ExternFunctionId, $($exclude)*);
        $crate::prune_single!(__identity_helper, ExternTypeId, $($exclude)*);
        $crate::prune_single!(__identity_helper, ImplDefId, $($exclude)*);
        $crate::prune_single!(__identity_helper, ImplImplDefId, $($exclude)*);
        $crate::prune_single!(__identity_helper, ImplAliasId, $($exclude)*);
        $crate::prune_single!(__identity_helper, TraitId, $($exclude)*);
        $crate::prune_single!(__identity_helper, TraitFunctionId, $($exclude)*);
        $crate::prune_single!(__identity_helper, VariantId, $($exclude)*);
        $crate::prune_single!(__identity_helper, ImplFunctionId, $($exclude)*);
        $crate::prune_single!(__identity_helper, EnumId, $($exclude)*);
        $crate::prune_single!(__identity_helper, StructId, $($exclude)*);
        $crate::prune_single!(__identity_helper, GenericParamId, $($exclude)*);
        $crate::prune_single!(__identity_helper, TraitTypeId, $($exclude)*);
        $crate::prune_single!(__identity_helper, TraitImplId, $($exclude)*);
        $crate::prune_single!(__identity_helper, TraitConstantId, $($exclude)*);
        $crate::prune_single!(__identity_helper, TypeVar, $($exclude)*);
        $crate::prune_single!(__identity_helper, ConstVar, $($exclude)*);
        $crate::prune_single!(__identity_helper, VarId, $($exclude)*);
        $crate::prune_single!(__identity_helper, MemberId, $($exclude)*);
        $crate::prune_single!(__identity_helper, LocalVarId, $($exclude)*);
        $crate::prune_single!(__identity_helper, LocalImplVarId, $($exclude)*);
        $crate::prune_single!(__identity_helper, LocalTypeVarId, $($exclude)*);
        $crate::prune_single!(__identity_helper, LocalConstVarId, $($exclude)*);
        $crate::prune_single!(__identity_helper, InferenceVar, $($exclude)*);
        $crate::prune_single!(__identity_helper, ImplFunctionBodyId, $($exclude)*);
        $crate::prune_single!(__identity_helper, ExprId, $($exclude)*);

        $crate::prune_single!(__regular_helper, Signature, $($exclude)*);
        $crate::prune_single!(__regular_helper, GenericFunctionId, $($exclude)*);
        $crate::prune_single!(__regular_helper, GenericFunctionWithBodyId, $($exclude)*);
        $crate::prune_single!(__regular_helper, ConcreteFunction, $($exclude)*);
        $crate::prune_single!(__regular_helper, ConcreteFunctionWithBody, $($exclude)*);
        $crate::prune_single!(__regular_helper, ConcreteFunctionWithBodyId, $($exclude)*);
        $crate::prune_single!(__regular_helper, ImplGenericFunctionId, $($exclude)*);
        $crate::prune_single!(__regular_helper, ImplGenericFunctionWithBodyId, $($exclude)*);
        $crate::prune_single!(__regular_helper, ImplVar, $($exclude)*);
        $crate::prune_single!(__regular_helper, ImplVarId, $($exclude)*);
        $crate::prune_single!(__regular_helper, Parameter, $($exclude)*);
        $crate::prune_single!(__regular_helper, GenericParam, $($exclude)*);
        $crate::prune_single!(__regular_helper, GenericParamType, $($exclude)*);
        $crate::prune_single!(__regular_helper, GenericParamConst, $($exclude)*);
        $crate::prune_single!(__regular_helper, GenericParamImpl, $($exclude)*);
        $crate::prune_single!(__regular_helper, GenericArgumentId, $($exclude)*);
        $crate::prune_single!(__regular_helper, FunctionId, $($exclude)*);
        $crate::prune_single!(__regular_helper, FunctionLongId, $($exclude)*);
        $crate::prune_single!(__regular_helper, TypeId, $($exclude)*);
        $crate::prune_single!(__regular_helper, TypeLongId, $($exclude)*);
        $crate::prune_single!(__regular_helper, ConstValueId, $($exclude)*);
        $crate::prune_single!(__regular_helper, ConstValue, $($exclude)*);
        $crate::prune_single!(__regular_helper, ConcreteVariant, $($exclude)*);
        $crate::prune_single!(__regular_helper, ValueSelectorArm, $($exclude)*);
        $crate::prune_single!(__regular_helper, MatchArmSelector, $($exclude)*);
        $crate::prune_single!(__regular_helper, ClosureTypeLongId, $($exclude)*);
        $crate::prune_single!(__regular_helper, ConcreteTypeId, $($exclude)*);
        $crate::prune_single!(__regular_helper, ConcreteStructId, $($exclude)*);
        $crate::prune_single!(__regular_helper, ConcreteStructLongId, $($exclude)*);
        $crate::prune_single!(__regular_helper, ConcreteEnumId, $($exclude)*);
        $crate::prune_single!(__regular_helper, ConcreteEnumLongId, $($exclude)*);
        $crate::prune_single!(__regular_helper, ConcreteExternTypeId, $($exclude)*);
        $crate::prune_single!(__regular_helper, ConcreteExternTypeLongId, $($exclude)*);
        $crate::prune_single!(__regular_helper, ConcreteTraitId, $($exclude)*);
        $crate::prune_single!(__regular_helper, ConcreteTraitLongId, $($exclude)*);
        $crate::prune_single!(__regular_helper, ConcreteTraitTypeId, $($exclude)*);
        $crate::prune_single!(__regular_helper, ConcreteTraitTypeLongId, $($exclude)*);
        $crate::prune_single!(__regular_helper, ConcreteImplId, $($exclude)*);
        $crate::prune_single!(__regular_helper, ConcreteImplLongId, $($exclude)*);
        $crate::prune_single!(__regular_helper, ConcreteTraitGenericFunctionLongId, $($exclude)*);
        $crate::prune_single!(__regular_helper, ConcreteTraitGenericFunctionId, $($exclude)*);
        $crate::prune_single!(__regular_helper, GeneratedImplId, $($exclude)*);
        $crate::prune_single!(__regular_helper, GeneratedImplLongId, $($exclude)*);
        $crate::prune_single!(__regular_helper, GeneratedImplItems, $($exclude)*);
        $crate::prune_single!(__regular_helper, ImplLongId, $($exclude)*);
        $crate::prune_single!(__regular_helper, ImplId, $($exclude)*);
        $crate::prune_single!(__regular_helper, ImplTypeId, $($exclude)*);
        $crate::prune_single!(__regular_helper, ImplConstantId, $($exclude)*);
        $crate::prune_single!(__regular_helper, ImplImplId, $($exclude)*);
        $crate::prune_single!(__regular_helper, UninferredGeneratedImplId, $($exclude)*);
        $crate::prune_single!(__regular_helper, UninferredGeneratedImplLongId, $($exclude)*);
        $crate::prune_single!(__regular_helper, UninferredImpl, $($exclude)*);
        $crate::prune_single!(__regular_helper, ExprVarMemberPath, $($exclude)*);
        $crate::prune_single!(__regular_helper, ExprVar, $($exclude)*);
        $crate::prune_single!(__regular_helper, ImplVarTraitItemMappings, $($exclude)*);
        $crate::prune_single!(__regular_helper, CanonicalTrait, $($exclude)*);
    };
}

#[macro_export]
macro_rules! add_expr_rewrites {
    (<$($generics:lifetime),*>, $self_ty:ty, $err_ty:ty, @exclude $($exclude:ident)*) => {
        macro_rules! __identity_helper {
            ($item:ident) => {
                 $crate::add_rewrite_identity!(<$($generics),*>, $self_ty, $err_ty, $item);
            }
        }
        macro_rules! __regular_helper {
            ($item:ident) => { $crate::add_rewrite!(<$($generics),*>, $self_ty, $err_ty, $item); }
        }

        $crate::prune_single!(__identity_helper, PatternId, $($exclude)*);
        $crate::prune_single!(__identity_helper, StatementId, $($exclude)*);
        $crate::prune_single!(__identity_helper, ConstantId, $($exclude)*);

        $crate::prune_single!(__regular_helper, Expr, $($exclude)*);
        $crate::prune_single!(__regular_helper, ExprTuple, $($exclude)*);
        $crate::prune_single!(__regular_helper, ExprSnapshot, $($exclude)*);
        $crate::prune_single!(__regular_helper, ExprDesnap, $($exclude)*);
        $crate::prune_single!(__regular_helper, ExprAssignment, $($exclude)*);
        $crate::prune_single!(__regular_helper, ExprLogicalOperator, $($exclude)*);
        $crate::prune_single!(__regular_helper, ExprBlock, $($exclude)*);
        $crate::prune_single!(__regular_helper, ExprFunctionCall, $($exclude)*);
        $crate::prune_single!(__regular_helper, ExprMatch, $($exclude)*);
        $crate::prune_single!(__regular_helper, ExprIf, $($exclude)*);
        $crate::prune_single!(__regular_helper, Condition, $($exclude)*);
        $crate::prune_single!(__regular_helper, ExprLoop, $($exclude)*);
        $crate::prune_single!(__regular_helper, ExprWhile, $($exclude)*);
        $crate::prune_single!(__regular_helper, ExprFor, $($exclude)*);
        $crate::prune_single!(__regular_helper, ExprLiteral, $($exclude)*);
        $crate::prune_single!(__regular_helper, ExprStringLiteral, $($exclude)*);
        $crate::prune_single!(__regular_helper, ExprMemberAccess, $($exclude)*);
        $crate::prune_single!(__regular_helper, ExprStructCtor, $($exclude)*);
        $crate::prune_single!(__regular_helper, ExprEnumVariantCtor, $($exclude)*);
        $crate::prune_single!(__regular_helper, ExprPropagateError, $($exclude)*);
        $crate::prune_single!(__regular_helper, ExprConstant, $($exclude)*);
        $crate::prune_single!(__regular_helper, ExprFixedSizeArray, $($exclude)*);
        $crate::prune_single!(__regular_helper, ExprClosure, $($exclude)*);
        $crate::prune_single!(__regular_helper, ExprMissing, $($exclude)*);
        $crate::prune_single!(__regular_helper, ExprFunctionCallArg, $($exclude)*);
        $crate::prune_single!(__regular_helper, FixedSizeArrayItems, $($exclude)*);
        $crate::prune_single!(__regular_helper, MatchArm, $($exclude)*);
        $crate::prune_single!(__regular_helper, Statement, $($exclude)*);
        $crate::prune_single!(__regular_helper, StatementExpr, $($exclude)*);
        $crate::prune_single!(__regular_helper, StatementLet, $($exclude)*);
        $crate::prune_single!(__regular_helper, StatementReturn, $($exclude)*);
        $crate::prune_single!(__regular_helper, StatementContinue, $($exclude)*);
        $crate::prune_single!(__regular_helper, StatementBreak, $($exclude)*);
        $crate::prune_single!(__regular_helper, StatementItem, $($exclude)*);
        $crate::prune_single!(__regular_helper, Pattern, $($exclude)*);
        $crate::prune_single!(__regular_helper, PatternLiteral, $($exclude)*);
        $crate::prune_single!(__regular_helper, PatternStringLiteral, $($exclude)*);
        $crate::prune_single!(__regular_helper, PatternVariable, $($exclude)*);
        $crate::prune_single!(__regular_helper, PatternStruct, $($exclude)*);
        $crate::prune_single!(__regular_helper, PatternTuple, $($exclude)*);
        $crate::prune_single!(__regular_helper, PatternFixedSizeArray, $($exclude)*);
        $crate::prune_single!(__regular_helper, PatternEnumVariant, $($exclude)*);
        $crate::prune_single!(__regular_helper, PatternOtherwise, $($exclude)*);
        $crate::prune_single!(__regular_helper, PatternMissing, $($exclude)*);
        $crate::prune_single!(__regular_helper, LocalVariable, $($exclude)*);
        $crate::prune_single!(__regular_helper, Member, $($exclude)*);
    };
}

pub struct SubstitutionRewriter<'a> {
    db: &'a dyn SemanticGroup,
    substitution: &'a GenericSubstitution,
}
impl<'a> HasDb<&'a dyn SemanticGroup> for SubstitutionRewriter<'a> {
    fn get_db(&self) -> &'a dyn SemanticGroup {
        self.db
    }
}

add_basic_rewrites!(
    <'a>,
    SubstitutionRewriter<'a>,
    DiagnosticAdded,
    @exclude TypeId TypeLongId ImplId ImplLongId ConstValue GenericFunctionWithBodyId
);

impl SemanticRewriter<TypeId, DiagnosticAdded> for SubstitutionRewriter<'_> {
    fn internal_rewrite(&mut self, value: &mut TypeId) -> Maybe<RewriteResult> {
        if value.is_fully_concrete(self.db) {
            return Ok(RewriteResult::NoChange);
        }
        value.default_rewrite(self)
    }
}

impl SemanticRewriter<ImplId, DiagnosticAdded> for SubstitutionRewriter<'_> {
    fn internal_rewrite(&mut self, value: &mut ImplId) -> Maybe<RewriteResult> {
        if value.is_fully_concrete(self.db) {
            return Ok(RewriteResult::NoChange);
        }
        value.default_rewrite(self)
    }
}

impl SemanticRewriter<TypeLongId, DiagnosticAdded> for SubstitutionRewriter<'_> {
    fn internal_rewrite(&mut self, value: &mut TypeLongId) -> Maybe<RewriteResult> {
        match value {
            TypeLongId::GenericParameter(generic_param) => {
                if let Some(generic_arg) = self.substitution.get(generic_param) {
                    let type_id = *extract_matches!(generic_arg, GenericArgumentId::Type);
                    // return self.rewrite(type_id.lookup_intern(self.db));
                    *value = type_id.lookup_intern(self.db);
                    return Ok(RewriteResult::Modified);
                }
            }
            TypeLongId::ImplType(impl_type_id) => {
                let impl_type_id_rewrite_result = self.internal_rewrite(impl_type_id)?;
                let new_value =
                    self.db.impl_type_concrete_implized(*impl_type_id)?.lookup_intern(self.db);
                if new_value != *value {
                    *value = new_value;
                    return Ok(RewriteResult::Modified);
                } else {
                    return Ok(impl_type_id_rewrite_result);
                }
            }
            _ => {}
        }
        value.default_rewrite(self)
    }
}
impl SemanticRewriter<ConstValue, DiagnosticAdded> for SubstitutionRewriter<'_> {
    fn internal_rewrite(&mut self, value: &mut ConstValue) -> Maybe<RewriteResult> {
        match value {
            ConstValue::Generic(param_id) => {
                if let Some(generic_arg) = self.substitution.get(param_id) {
                    let const_value_id = extract_matches!(generic_arg, GenericArgumentId::Constant);

                    *value = const_value_id.lookup_intern(self.db);
                    return Ok(RewriteResult::Modified);
                }
            }
            ConstValue::ImplConstant(impl_constant_id) => {
                let impl_const_id_rewrite_result = self.internal_rewrite(impl_constant_id)?;
                let new_value = self
                    .db
                    .impl_constant_concrete_implized_value(*impl_constant_id)?
                    .lookup_intern(self.db);
                if new_value != *value {
                    *value = new_value;
                    return Ok(RewriteResult::Modified);
                } else {
                    return Ok(impl_const_id_rewrite_result);
                }
            }
            _ => {}
        }

        value.default_rewrite(self)
    }
}
impl SemanticRewriter<ImplLongId, DiagnosticAdded> for SubstitutionRewriter<'_> {
    fn internal_rewrite(&mut self, value: &mut ImplLongId) -> Maybe<RewriteResult> {
        match value {
            ImplLongId::GenericParameter(generic_param) => {
                if let Some(generic_arg) = self.substitution.get(generic_param) {
                    *value = extract_matches!(generic_arg, GenericArgumentId::Impl)
                        .lookup_intern(self.db);
                    // TODO(GIL): Reduce and check for cycles when the substitution is created.
                    // Substitution is guaranteed to not contain its own variables.
                    return Ok(RewriteResult::Modified);
                }
            }
            ImplLongId::ImplImpl(impl_impl_id) => {
                let impl_impl_id_rewrite_result = self.internal_rewrite(impl_impl_id)?;
                let new_value =
                    self.db.impl_impl_concrete_implized(*impl_impl_id)?.lookup_intern(self.db);
                if new_value != *value {
                    *value = new_value;
                    return Ok(RewriteResult::Modified);
                } else {
                    return Ok(impl_impl_id_rewrite_result);
                }
            }
            ImplLongId::SelfImpl(concrete_trait_id) => {
                let rewrite_result = self.internal_rewrite(concrete_trait_id)?;
                if let Some(self_impl) = &self.substitution.self_impl {
                    if *concrete_trait_id == self_impl.concrete_trait(self.db)? {
                        *value = self_impl.lookup_intern(self.db);
                        return Ok(RewriteResult::Modified);
                    }
                } else {
                    return Ok(rewrite_result);
                }
            }
            _ => {}
        }
        value.default_rewrite(self)
    }
}
impl SemanticRewriter<GenericFunctionWithBodyId, DiagnosticAdded> for SubstitutionRewriter<'_> {
    fn internal_rewrite(&mut self, value: &mut GenericFunctionWithBodyId) -> Maybe<RewriteResult> {
        if let GenericFunctionWithBodyId::Trait(id) = value {
            if let Some(self_impl) = &self.substitution.self_impl {
                if let ImplLongId::Concrete(concrete_impl_id) = self_impl.lookup_intern(self.db) {
                    if self.rewrite(id.concrete_trait(self.db.upcast()))?
                        == self_impl.concrete_trait(self.db)?
                    {
                        *value = GenericFunctionWithBodyId::Impl(ImplGenericFunctionWithBodyId {
                            concrete_impl_id,
                            function_body: ImplFunctionBodyId::Trait(id.trait_function(self.db)),
                        });
                        return Ok(RewriteResult::Modified);
                    }
                }
            }
        }
        value.default_rewrite(self)
    }
}
