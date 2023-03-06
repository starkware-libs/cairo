use std::ops::Deref;

use cairo_lang_defs::ids::{
    EnumId, ExternFunctionId, ExternTypeId, FreeFunctionId, GenericParamId, ImplDefId,
    ImplFunctionId, ParamId, StructId, TraitFunctionId, TraitId, VariantId,
};
use cairo_lang_diagnostics::Maybe;
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
use crate::items::imp::ImplId;
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
#[allow(clippy::derive_hash_xor_eq)]
impl std::hash::Hash for GenericSubstitution {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.iter().collect_vec().hash(state);
    }
}

#[macro_export]
macro_rules! semantic_object_for_id {
    ($name:ident, $lookup:ident, $intern:ident, $rewrite_long:ident) => {
        impl $crate::SemanticObject for $name {
            fn default_rewrite<T: $crate::substitution::SemanticRewriter + ?Sized>(
                self,
                rewriter: &mut T,
            ) -> Maybe<Self> {
                let val = rewriter.get_db().$lookup(self);
                let val = rewriter.$rewrite_long(val)?;
                Ok(rewriter.get_db().$intern(val))
            }
        }
    };
}
macro_rules! add_rewrite {
    ($name:ident, $ty:ident) => {
        fn $name(&mut self, obj: $ty) -> Maybe<$ty> {
            obj.default_rewrite(self)
        }
    };
}

macro_rules! add_rewrite_identity {
    ($name:ident, $ty:ident) => {
        fn $name(&mut self, obj: $ty) -> Maybe<$ty> {
            Ok(obj)
        }
    };
}

pub trait SemanticObject: Sized {
    fn default_rewrite<T: SemanticRewriter + ?Sized>(self, rewriter: &mut T) -> Maybe<Self>;
}
pub trait SemanticRewriter {
    fn get_db(&self) -> &dyn SemanticGroup;
    add_rewrite_identity!(rewrite_param_id, ParamId);
    add_rewrite_identity!(rewrite_literal_id, LiteralId);
    add_rewrite_identity!(rewrite_free_function_id, FreeFunctionId);
    add_rewrite_identity!(rewrite_extern_function_id, ExternFunctionId);
    add_rewrite_identity!(rewrite_extern_type_id, ExternTypeId);
    add_rewrite_identity!(rewrite_impl_def_id, ImplDefId);
    add_rewrite_identity!(rewrite_trait_id, TraitId);
    add_rewrite_identity!(rewrite_trait_function_id, TraitFunctionId);
    add_rewrite_identity!(rewrite_variant_id, VariantId);
    add_rewrite_identity!(rewrite_impl_function_id, ImplFunctionId);
    add_rewrite_identity!(rewrite_enum_id, EnumId);
    add_rewrite_identity!(rewrite_struct_id, StructId);
    add_rewrite_identity!(rewrite_type_var, TypeVar);
    add_rewrite_identity!(rewrite_impl_var, ImplVar);
    add_rewrite_identity!(rewrite_generic_param_id, GenericParamId);
    add_rewrite!(rewrite_signature, Signature);
    add_rewrite!(rewrite_generic_function_id, GenericFunctionId);
    add_rewrite!(rewrite_generic_function_with_body_id, GenericFunctionWithBodyId);
    add_rewrite!(rewrite_concrete_function, ConcreteFunction);
    add_rewrite!(rewrite_concrete_function_with_body, ConcreteFunctionWithBody);
    add_rewrite!(rewrite_impl_generic_function_id, ImplGenericFunctionId);
    add_rewrite!(rewrite_impl_generic_function_with_body_id, ImplGenericFunctionWithBodyId);
    add_rewrite!(rewrite_parameter, Parameter);
    add_rewrite!(rewrite_generic_param, GenericParam);
    add_rewrite!(rewrite_generic_param_type, GenericParamType);
    add_rewrite!(rewrite_generic_param_const, GenericParamConst);
    add_rewrite!(rewrite_generic_param_impl, GenericParamImpl);
    add_rewrite!(rewrite_generic_argument_id, GenericArgumentId);
    add_rewrite!(rewrite_function_id, FunctionId);
    add_rewrite!(rewrite_function_long_id, FunctionLongId);
    add_rewrite!(rewrite_type_id, TypeId);
    add_rewrite!(rewrite_type_long_id, TypeLongId);
    add_rewrite!(rewrite_concrete_variant, ConcreteVariant);
    add_rewrite!(rewrite_concrete_type_id, ConcreteTypeId);
    add_rewrite!(rewrite_concrete_struct_id, ConcreteStructId);
    add_rewrite!(rewrite_concrete_struct_long_id, ConcreteStructLongId);
    add_rewrite!(rewrite_concrete_enum_id, ConcreteEnumId);
    add_rewrite!(rewrite_concrete_enum_long_id, ConcreteEnumLongId);
    add_rewrite!(rewrite_concrete_extern_type_id, ConcreteExternTypeId);
    add_rewrite!(rewrite_concrete_extern_type_long_id, ConcreteExternTypeLongId);
    add_rewrite!(rewrite_concrete_trait_id, ConcreteTraitId);
    add_rewrite!(rewrite_concrete_trait_long_id, ConcreteTraitLongId);
    add_rewrite!(rewrite_concrete_impl_id, ConcreteImplId);
    add_rewrite!(rewrite_concrete_impl_long_id, ConcreteImplLongId);
    add_rewrite!(
        rewrite_concrete_trait_generic_function_long_id,
        ConcreteTraitGenericFunctionLongId
    );
    add_rewrite!(rewrite_concrete_trait_generic_function_id, ConcreteTraitGenericFunctionId);
    add_rewrite!(rewrite_impl_id, ImplId);
}

pub struct SubstitutionRewriter<'a> {
    pub db: &'a dyn SemanticGroup,
    pub substitution: &'a GenericSubstitution,
}
impl<'a> SemanticRewriter for SubstitutionRewriter<'a> {
    fn get_db(&self) -> &dyn SemanticGroup {
        self.db
    }
    fn rewrite_type_long_id(&mut self, value: TypeLongId) -> Maybe<TypeLongId> {
        if let TypeLongId::GenericParameter(generic_param) = value {
            if let Some(generic_arg) = self.substitution.get(&generic_param) {
                let type_id = *extract_matches!(generic_arg, GenericArgumentId::Type);
                // return self.rewrite_type_long_id(self.db.lookup_intern_type(type_id));
                return Ok(self.db.lookup_intern_type(type_id));
            }
        }
        value.default_rewrite(self)
    }
    fn rewrite_impl_id(&mut self, value: ImplId) -> Maybe<ImplId> {
        if let ImplId::GenericParameter(generic_param) = value {
            if let Some(generic_arg) = self.substitution.get(&generic_param) {
                let impl_id = *extract_matches!(generic_arg, GenericArgumentId::Impl);
                // return self.rewrite_impl_id(impl_id);
                return Ok(impl_id);
            }
        }
        value.default_rewrite(self)
    }
}
