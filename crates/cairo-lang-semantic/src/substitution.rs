use std::ops::Deref;

use cairo_lang_defs::ids::{
    EnumId, ExternFunctionId, ExternTypeId, FreeFunctionId, GenericParamId, ImplDefId,
    ImplFunctionId, ParamId, StructId, TraitFunctionId, TraitId, VariantId,
};
use cairo_lang_diagnostics::{DiagnosticAdded, Maybe};
use cairo_lang_utils::extract_matches;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use itertools::{zip_eq, Itertools};

use crate::db::SemanticGroup;
use crate::expr::inference::{ImplVar, TypeVar};
use crate::items::functions::{
    ConcreteFunctionWithBody, GenericFunctionId, GenericFunctionWithBodyId, ImplGenericFunctionId,
    ImplGenericFunctionWithBodyId,
};
use crate::items::generics::{GenericParamConst, GenericParamImpl, GenericParamType};
use crate::items::imp::{ImplId, UninferredImpl};
use crate::items::trt::{ConcreteTraitGenericFunctionId, ConcreteTraitGenericFunctionLongId};
use crate::literals::LiteralId;
use crate::types::{ConcreteEnumLongId, ConcreteExternTypeLongId, ConcreteStructLongId};
use crate::{
    ConcreteEnumId, ConcreteExternTypeId, ConcreteFunction, ConcreteImplId, ConcreteImplLongId,
    ConcreteStructId, ConcreteTraitId, ConcreteTraitLongId, ConcreteTypeId, ConcreteVariant,
    FunctionId, FunctionLongId, GenericArgumentId, GenericParam, Parameter, Signature, TypeId,
    TypeLongId,
};

/// A substitution of generic arguments in generic parameters. Used for concretization.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct GenericSubstitution(pub OrderedHashMap<GenericParamId, GenericArgumentId>);
impl GenericSubstitution {
    pub fn new(generic_params: &[GenericParam], generic_args: &[GenericArgumentId]) -> Self {
        GenericSubstitution(
            zip_eq(generic_params.iter().map(|param| param.id()), generic_args.iter().copied())
                .collect(),
        )
    }
    pub fn concat(mut self, other: GenericSubstitution) -> Self {
        for (key, value) in other.0.into_iter() {
            self.0.insert(key, value);
        }
        self
    }
}
impl Deref for GenericSubstitution {
    type Target = OrderedHashMap<GenericParamId, GenericArgumentId>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
#[allow(clippy::derived_hash_with_manual_eq)]
impl std::hash::Hash for GenericSubstitution {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.iter().collect_vec().hash(state);
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
            fn default_rewrite(self, rewriter: &mut TRewriter) -> Result<Self, Error> {
                let val = $crate::substitution::HasDb::get_db(rewriter).$lookup(self);
                let val = $crate::substitution::SemanticRewriter::rewrite(rewriter, val)?;
                Ok($crate::substitution::HasDb::get_db(rewriter).$intern(val))
            }
        }
    };
}

#[macro_export]
macro_rules! add_rewrite {
    (<$($generics:lifetime)*>, $self_ty:ty, $err_ty:ty, $ty:ident) => {
        impl <$($generics)*> SemanticRewriter<$ty, $err_ty> for $self_ty {
            fn rewrite(&mut self, value: $ty) -> Result<$ty, $err_ty> {
                $crate::substitution::SemanticObject::default_rewrite(value, self)
            }
        }
    };
}

#[macro_export]
macro_rules! add_rewrite_identity {
    (<$($generics:lifetime)*>, $self_ty:ty, $err_ty:ty, $ty:ident) => {
        impl <$($generics)*> SemanticRewriter<$ty, $err_ty> for $self_ty {
            fn rewrite(&mut self, value: $ty) -> Result<$ty, $err_ty> {
                Ok(value)
            }
        }
    };
}

pub trait SemanticObject<TRewriter, Error>: Sized {
    fn default_rewrite(self, rewriter: &mut TRewriter) -> Result<Self, Error>;
}
impl<T, E, TRewriter: SemanticRewriter<T, E>> SemanticRewriter<Vec<T>, E> for TRewriter {
    fn rewrite(&mut self, value: Vec<T>) -> Result<Vec<T>, E> {
        value.into_iter().map(|el| self.rewrite(el)).collect()
    }
}
impl<T: Clone, E, TRewriter: SemanticRewriter<T, E>> SemanticRewriter<Box<T>, E> for TRewriter {
    fn rewrite(&mut self, value: Box<T>) -> Result<Box<T>, E> {
        Ok(Box::new(self.rewrite((*value).clone())?))
    }
}
impl<T0, T1, E, TRewriter: SemanticRewriter<T0, E> + SemanticRewriter<T1, E>>
    SemanticRewriter<(T0, T1), E> for TRewriter
{
    fn rewrite(&mut self, value: (T0, T1)) -> Result<(T0, T1), E> {
        let (a, b) = value;
        Ok((self.rewrite(a)?, self.rewrite(b)?))
    }
}
impl<T, E, TRewriter: SemanticRewriter<T, E>> SemanticRewriter<Option<T>, E> for TRewriter {
    fn rewrite(&mut self, value: Option<T>) -> Result<Option<T>, E> {
        match value {
            Some(val) => Ok(Some(self.rewrite(val)?)),
            None => Ok(None),
        }
    }
}
impl<T, E, TRewriter: SemanticRewriter<T, E>, E2> SemanticRewriter<Result<T, E2>, E> for TRewriter {
    fn rewrite(&mut self, value: Result<T, E2>) -> Result<Result<T, E2>, E> {
        match value {
            Ok(val) => Ok(Ok(self.rewrite(val)?)),
            Err(err) => Ok(Err(err)),
        }
    }
}
pub trait HasDb<T> {
    fn get_db(&self) -> T;
}
pub trait SemanticRewriter<T, Error> {
    fn rewrite(&mut self, value: T) -> Result<T, Error>;
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
    (<$($generics:lifetime)*>, $self_ty:ty, $err_ty:ty, @exclude $($exclude:ident)*) => {
        macro_rules! __identitity_helper {
            ($item:ident) => { $crate::add_rewrite_identity!(<$($generics)*>, $self_ty, $err_ty, $item); }
        }
        macro_rules! __regular_helper {
            ($item:ident) => { $crate::add_rewrite!(<$($generics)*>, $self_ty, $err_ty, $item); }
        }

        $crate::prune_single!(__identitity_helper, ParamId, $($exclude)*);
        $crate::prune_single!(__identitity_helper, LiteralId, $($exclude)*);
        $crate::prune_single!(__identitity_helper, FreeFunctionId, $($exclude)*);
        $crate::prune_single!(__identitity_helper, ExternFunctionId, $($exclude)*);
        $crate::prune_single!(__identitity_helper, ExternTypeId, $($exclude)*);
        $crate::prune_single!(__identitity_helper, ImplDefId, $($exclude)*);
        $crate::prune_single!(__identitity_helper, TraitId, $($exclude)*);
        $crate::prune_single!(__identitity_helper, TraitFunctionId, $($exclude)*);
        $crate::prune_single!(__identitity_helper, VariantId, $($exclude)*);
        $crate::prune_single!(__identitity_helper, ImplFunctionId, $($exclude)*);
        $crate::prune_single!(__identitity_helper, EnumId, $($exclude)*);
        $crate::prune_single!(__identitity_helper, StructId, $($exclude)*);
        $crate::prune_single!(__identitity_helper, GenericParamId, $($exclude)*);
        $crate::prune_single!(__identitity_helper, TypeVar, $($exclude)*);
        $crate::prune_single!(__identitity_helper, ImplVar, $($exclude)*);

        $crate::prune_single!(__regular_helper, Signature, $($exclude)*);
        $crate::prune_single!(__regular_helper, GenericFunctionId, $($exclude)*);
        $crate::prune_single!(__regular_helper, GenericFunctionWithBodyId, $($exclude)*);
        $crate::prune_single!(__regular_helper, ConcreteFunction, $($exclude)*);
        $crate::prune_single!(__regular_helper, ConcreteFunctionWithBody, $($exclude)*);
        $crate::prune_single!(__regular_helper, ImplGenericFunctionId, $($exclude)*);
        $crate::prune_single!(__regular_helper, ImplGenericFunctionWithBodyId, $($exclude)*);
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
        $crate::prune_single!(__regular_helper, ConcreteVariant, $($exclude)*);
        $crate::prune_single!(__regular_helper, ConcreteTypeId, $($exclude)*);
        $crate::prune_single!(__regular_helper, ConcreteStructId, $($exclude)*);
        $crate::prune_single!(__regular_helper, ConcreteStructLongId, $($exclude)*);
        $crate::prune_single!(__regular_helper, ConcreteEnumId, $($exclude)*);
        $crate::prune_single!(__regular_helper, ConcreteEnumLongId, $($exclude)*);
        $crate::prune_single!(__regular_helper, ConcreteExternTypeId, $($exclude)*);
        $crate::prune_single!(__regular_helper, ConcreteExternTypeLongId, $($exclude)*);
        $crate::prune_single!(__regular_helper, ConcreteTraitId, $($exclude)*);
        $crate::prune_single!(__regular_helper, ConcreteTraitLongId, $($exclude)*);
        $crate::prune_single!(__regular_helper, ConcreteImplId, $($exclude)*);
        $crate::prune_single!(__regular_helper, ConcreteImplLongId, $($exclude)*);
        $crate::prune_single!(__regular_helper, ConcreteTraitGenericFunctionLongId, $($exclude)*);
        $crate::prune_single!(__regular_helper, ConcreteTraitGenericFunctionId, $($exclude)*);
        $crate::prune_single!(__regular_helper, ImplId, $($exclude)*);
        $crate::prune_single!(__regular_helper, UninferredImpl, $($exclude)*);
    };
}

#[macro_export]
macro_rules! add_expr_rewrites {
    (<$($generics:lifetime)*>, $self_ty:ty, $err_ty:ty, @exclude $($exclude:ident)*) => {
        macro_rules! __identitity_helper {
            ($item:ident) => { $crate::add_rewrite_identity!(<$($generics)*>, $self_ty, $err_ty, $item); }
        }
        macro_rules! __regular_helper {
            ($item:ident) => { $crate::add_rewrite!(<$($generics)*>, $self_ty, $err_ty, $item); }
        }

        $crate::prune_single!(__identitity_helper, ExprId, $($exclude)*);
        $crate::prune_single!(__identitity_helper, StatementId, $($exclude)*);
        $crate::prune_single!(__identitity_helper, VarId, $($exclude)*);
        $crate::prune_single!(__identitity_helper, MemberId, $($exclude)*);
        $crate::prune_single!(__identitity_helper, ConstantId, $($exclude)*);
        $crate::prune_single!(__identitity_helper, LocalVarId, $($exclude)*);

        $crate::prune_single!(__regular_helper, Expr, $($exclude)*);
        $crate::prune_single!(__regular_helper, ExprTuple, $($exclude)*);
        $crate::prune_single!(__regular_helper, ExprSnapshot, $($exclude)*);
        $crate::prune_single!(__regular_helper, ExprDesnap, $($exclude)*);
        $crate::prune_single!(__regular_helper, ExprAssignment, $($exclude)*);
        $crate::prune_single!(__regular_helper, ExprBlock, $($exclude)*);
        $crate::prune_single!(__regular_helper, ExprFunctionCall, $($exclude)*);
        $crate::prune_single!(__regular_helper, ExprMatch, $($exclude)*);
        $crate::prune_single!(__regular_helper, ExprIf, $($exclude)*);
        $crate::prune_single!(__regular_helper, ExprVar, $($exclude)*);
        $crate::prune_single!(__regular_helper, ExprLiteral, $($exclude)*);
        $crate::prune_single!(__regular_helper, ExprMemberAccess, $($exclude)*);
        $crate::prune_single!(__regular_helper, ExprStructCtor, $($exclude)*);
        $crate::prune_single!(__regular_helper, ExprEnumVariantCtor, $($exclude)*);
        $crate::prune_single!(__regular_helper, ExprPropagateError, $($exclude)*);
        $crate::prune_single!(__regular_helper, ExprConstant, $($exclude)*);
        $crate::prune_single!(__regular_helper, ExprMissing, $($exclude)*);
        $crate::prune_single!(__regular_helper, VarMemberPath, $($exclude)*);
        $crate::prune_single!(__regular_helper, ExprFunctionCallArg, $($exclude)*);
        $crate::prune_single!(__regular_helper, MatchArm, $($exclude)*);
        $crate::prune_single!(__regular_helper, Statement, $($exclude)*);
        $crate::prune_single!(__regular_helper, StatementExpr, $($exclude)*);
        $crate::prune_single!(__regular_helper, StatementLet, $($exclude)*);
        $crate::prune_single!(__regular_helper, StatementReturn, $($exclude)*);
        $crate::prune_single!(__regular_helper, Pattern, $($exclude)*);
        $crate::prune_single!(__regular_helper, PatternLiteral, $($exclude)*);
        $crate::prune_single!(__regular_helper, PatternVariable, $($exclude)*);
        $crate::prune_single!(__regular_helper, PatternStruct, $($exclude)*);
        $crate::prune_single!(__regular_helper, PatternTuple, $($exclude)*);
        $crate::prune_single!(__regular_helper, PatternEnumVariant, $($exclude)*);
        $crate::prune_single!(__regular_helper, PatternOtherwise, $($exclude)*);
        $crate::prune_single!(__regular_helper, LocalVariable, $($exclude)*);
        $crate::prune_single!(__regular_helper, Member, $($exclude)*);
    };
}

pub struct SubstitutionRewriter<'a> {
    pub db: &'a dyn SemanticGroup,
    pub substitution: &'a GenericSubstitution,
}
impl<'a> HasDb<&'a dyn SemanticGroup> for SubstitutionRewriter<'a> {
    fn get_db(&self) -> &'a dyn SemanticGroup {
        self.db
    }
}
add_basic_rewrites!(<'a>, SubstitutionRewriter<'a>, DiagnosticAdded, @exclude TypeLongId ImplId);
impl<'a> SemanticRewriter<TypeLongId, DiagnosticAdded> for SubstitutionRewriter<'a> {
    fn rewrite(&mut self, value: TypeLongId) -> Maybe<TypeLongId> {
        if let TypeLongId::GenericParameter(generic_param) = value {
            if let Some(generic_arg) = self.substitution.get(&generic_param) {
                let type_id = *extract_matches!(generic_arg, GenericArgumentId::Type);
                // return self.rewrite(self.db.lookup_intern_type(type_id));
                return Ok(self.db.lookup_intern_type(type_id));
            }
        }
        value.default_rewrite(self)
    }
}
impl<'a> SemanticRewriter<ImplId, DiagnosticAdded> for SubstitutionRewriter<'a> {
    fn rewrite(&mut self, value: ImplId) -> Maybe<ImplId> {
        if let ImplId::GenericParameter(generic_param) = value {
            if let Some(generic_arg) = self.substitution.get(&generic_param) {
                let impl_id = *extract_matches!(generic_arg, GenericArgumentId::Impl);
                return self.rewrite(impl_id);
            }
        }
        value.default_rewrite(self)
    }
}
