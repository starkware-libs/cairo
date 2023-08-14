use cairo_lang_defs::ids::{ImplAliasId, ImplDefId, TraitFunctionId};
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_utils::extract_matches;
use itertools::Itertools;

use super::canonic::ResultNoErrEx;
use super::conform::InferenceConform;
use super::{Inference, InferenceError, InferenceResult};
use crate::items::functions::{GenericFunctionId, ImplGenericFunctionId};
use crate::items::imp::{ImplId, ImplLookupContext, UninferredImpl};
use crate::items::trt::ConcreteTraitGenericFunctionId;
use crate::substitution::{GenericSubstitution, SemanticRewriter, SubstitutionRewriter};
use crate::{
    ConcreteFunction, ConcreteImplLongId, ConcreteTraitId, ConcreteTraitLongId, FunctionId,
    FunctionLongId, GenericArgumentId, GenericParam, TypeId,
};

/// Functions for embedding generic semantic objects in an existing [Inference] object, by
/// introducing new variables.
pub trait InferenceEmbeddings {
    fn infer_impl(
        &mut self,
        uninferred_impl: UninferredImpl,
        concrete_trait_id: ConcreteTraitId,
        lookup_context: &ImplLookupContext,
        stable_ptr: Option<SyntaxStablePtrId>,
    ) -> InferenceResult<ImplId>;
    fn infer_impl_def(
        &mut self,
        impl_def_id: ImplDefId,
        concrete_trait_id: ConcreteTraitId,
        lookup_context: &ImplLookupContext,
        stable_ptr: Option<SyntaxStablePtrId>,
    ) -> InferenceResult<ImplId>;
    fn infer_impl_alias(
        &mut self,
        impl_alias_id: ImplAliasId,
        concrete_trait_id: ConcreteTraitId,
        lookup_context: &ImplLookupContext,
        stable_ptr: Option<SyntaxStablePtrId>,
    ) -> InferenceResult<ImplId>;
    fn infer_generic_assignment(
        &mut self,
        generic_params: &[GenericParam],
        generic_args: &[GenericArgumentId],
        expected_generic_args: &[GenericArgumentId],
        lookup_context: &ImplLookupContext,
        stable_ptr: Option<SyntaxStablePtrId>,
    ) -> InferenceResult<Vec<GenericArgumentId>>;
    fn infer_generic_args(
        &mut self,
        generic_params: &[GenericParam],
        lookup_context: &ImplLookupContext,
        stable_ptr: Option<SyntaxStablePtrId>,
    ) -> InferenceResult<Vec<GenericArgumentId>>;
    fn infer_concrete_trait_by_self(
        &mut self,
        trait_function: TraitFunctionId,
        self_ty: TypeId,
        lookup_context: &ImplLookupContext,
        stable_ptr: Option<SyntaxStablePtrId>,
    ) -> Option<(ConcreteTraitId, usize)>;
    fn infer_generic_arg(
        &mut self,
        param: &GenericParam,
        lookup_context: ImplLookupContext,
        stable_ptr: Option<SyntaxStablePtrId>,
    ) -> InferenceResult<GenericArgumentId>;
    fn infer_trait_function(
        &mut self,
        concrete_trait_function: ConcreteTraitGenericFunctionId,
        lookup_context: &ImplLookupContext,
        stable_ptr: Option<SyntaxStablePtrId>,
    ) -> InferenceResult<FunctionId>;
    fn infer_generic_function(
        &mut self,
        generic_function: GenericFunctionId,
        lookup_context: &ImplLookupContext,
        stable_ptr: Option<SyntaxStablePtrId>,
    ) -> InferenceResult<FunctionId>;
    fn infer_trait_generic_function(
        &mut self,
        trait_function: ConcreteTraitGenericFunctionId,
        lookup_context: &ImplLookupContext,
        stable_ptr: Option<SyntaxStablePtrId>,
    ) -> InferenceResult<GenericFunctionId>;
}

impl<'db> InferenceEmbeddings for Inference<'db> {
    /// Infers all the variables required to make an uninferred impl provide a concrete trait.
    fn infer_impl(
        &mut self,
        uninferred_impl: UninferredImpl,
        concrete_trait_id: ConcreteTraitId,
        lookup_context: &ImplLookupContext,
        stable_ptr: Option<SyntaxStablePtrId>,
    ) -> InferenceResult<ImplId> {
        let impl_id = match uninferred_impl {
            UninferredImpl::Def(impl_def_id) => {
                self.infer_impl_def(impl_def_id, concrete_trait_id, lookup_context, stable_ptr)?
            }
            UninferredImpl::ImplAlias(impl_alias_id) => {
                self.infer_impl_alias(impl_alias_id, concrete_trait_id, lookup_context, stable_ptr)?
            }
            UninferredImpl::GenericParam(param_id) => {
                let param =
                    self.db.generic_param_semantic(param_id).map_err(InferenceError::Failed)?;
                let param = extract_matches!(param, GenericParam::Impl);
                let imp_concrete_trait_id = param.concrete_trait.unwrap();
                self.conform_traits(concrete_trait_id, imp_concrete_trait_id)?;
                ImplId::GenericParameter(param_id)
            }
        };
        Ok(impl_id)
    }

    /// Infers all the variables required to make an impl (possibly with free generic params)
    /// provide a concrete trait.
    fn infer_impl_def(
        &mut self,
        impl_def_id: ImplDefId,
        concrete_trait_id: ConcreteTraitId,
        lookup_context: &ImplLookupContext,
        stable_ptr: Option<SyntaxStablePtrId>,
    ) -> InferenceResult<ImplId> {
        let imp_generic_params = self.db.impl_def_generic_params(impl_def_id)?;
        let imp_concrete_trait = self.db.impl_def_concrete_trait(impl_def_id)?;
        if imp_concrete_trait.trait_id(self.db) != concrete_trait_id.trait_id(self.db) {
            return Err(InferenceError::TraitMismatch {
                trt0: imp_concrete_trait.trait_id(self.db),
                trt1: concrete_trait_id.trait_id(self.db),
            });
        }

        let long_concrete_trait = self.db.lookup_intern_concrete_trait(concrete_trait_id);
        let long_imp_concrete_trait = self.db.lookup_intern_concrete_trait(imp_concrete_trait);
        let generic_args = self.infer_generic_assignment(
            &imp_generic_params,
            &long_imp_concrete_trait.generic_args,
            &long_concrete_trait.generic_args,
            lookup_context,
            stable_ptr,
        )?;
        Ok(ImplId::Concrete(
            self.db.intern_concrete_impl(ConcreteImplLongId { impl_def_id, generic_args }),
        ))
    }

    /// Infers all the variables required to make an impl alias (possibly with free generic params)
    /// provide a concrete trait.
    fn infer_impl_alias(
        &mut self,
        impl_alias_id: ImplAliasId,
        concrete_trait_id: ConcreteTraitId,
        lookup_context: &ImplLookupContext,
        stable_ptr: Option<SyntaxStablePtrId>,
    ) -> InferenceResult<ImplId> {
        let impl_alias_generic_params = self.db.impl_alias_generic_params(impl_alias_id)?;
        let impl_id = self.db.impl_alias_resolved_impl(impl_alias_id)?;
        let imp_concrete_trait = impl_id.concrete_trait(self.db)?;
        if imp_concrete_trait.trait_id(self.db) != concrete_trait_id.trait_id(self.db) {
            return Err(InferenceError::TraitMismatch {
                trt0: imp_concrete_trait.trait_id(self.db),
                trt1: concrete_trait_id.trait_id(self.db),
            });
        }

        let long_concrete_trait = self.db.lookup_intern_concrete_trait(concrete_trait_id);
        let long_imp_concrete_trait = self.db.lookup_intern_concrete_trait(imp_concrete_trait);
        let generic_args = self.infer_generic_assignment(
            &impl_alias_generic_params,
            &long_imp_concrete_trait.generic_args,
            &long_concrete_trait.generic_args,
            lookup_context,
            stable_ptr,
        )?;

        Ok(SubstitutionRewriter {
            db: self.db,
            substitution: &GenericSubstitution::new(&impl_alias_generic_params, &generic_args),
        }
        .rewrite(impl_id)?)
    }

    /// Chooses and assignment to generic_params s.t. generic_args will be substituted to
    /// expected_generic_args.
    /// Returns the generic_params assignment.
    fn infer_generic_assignment(
        &mut self,
        generic_params: &[GenericParam],
        generic_args: &[GenericArgumentId],
        expected_generic_args: &[GenericArgumentId],
        lookup_context: &ImplLookupContext,
        stable_ptr: Option<SyntaxStablePtrId>,
    ) -> InferenceResult<Vec<GenericArgumentId>> {
        let new_generic_args =
            self.infer_generic_args(generic_params, lookup_context, stable_ptr)?;
        let substitution = GenericSubstitution::new(generic_params, &new_generic_args);
        let mut rewriter = SubstitutionRewriter { db: self.db, substitution: &substitution };
        let generic_args = rewriter.rewrite(generic_args.iter().copied().collect_vec())?;
        self.conform_generic_args(&generic_args, expected_generic_args)?;
        Ok(self.rewrite(new_generic_args).no_err())
    }

    /// Infers all generic_arguments given the parameters.
    fn infer_generic_args(
        &mut self,
        generic_params: &[GenericParam],
        lookup_context: &ImplLookupContext,
        stable_ptr: Option<SyntaxStablePtrId>,
    ) -> InferenceResult<Vec<GenericArgumentId>> {
        let mut generic_args = vec![];
        let mut substitution = GenericSubstitution::default();
        for generic_param in generic_params {
            let generic_param = SubstitutionRewriter { db: self.db, substitution: &substitution }
                .rewrite(*generic_param)
                .map_err(InferenceError::Failed)?;
            let generic_arg =
                self.infer_generic_arg(&generic_param, lookup_context.clone(), stable_ptr)?;
            generic_args.push(generic_arg);
            substitution.0.insert(generic_param.id(), generic_arg);
        }
        Ok(generic_args)
    }

    /// Tries to infer a trait function as a method for `self_ty`.
    /// Supports snapshot snapshot coercions.
    ///
    /// Returns the deduced type and the number of snapshots that need to be added to it.
    fn infer_concrete_trait_by_self(
        &mut self,
        trait_function: TraitFunctionId,
        self_ty: TypeId,
        lookup_context: &ImplLookupContext,
        stable_ptr: Option<SyntaxStablePtrId>,
    ) -> Option<(ConcreteTraitId, usize)> {
        let trait_id = trait_function.trait_id(self.db.upcast());
        let signature = self.db.trait_function_signature(trait_function).ok()?;
        let first_param = signature.params.into_iter().next()?;
        if first_param.name != "self" {
            return None;
        }
        let generic_params = self.db.trait_generic_params(trait_id).ok()?;
        let generic_args =
            self.infer_generic_args(&generic_params, lookup_context, stable_ptr).ok()?;
        let substitution = GenericSubstitution::new(&generic_params, &generic_args);
        let mut rewriter = SubstitutionRewriter { db: self.db, substitution: &substitution };

        let fixed_param_ty = rewriter.rewrite(first_param.ty).ok()?;
        let (_, n_snapshots) = self.conform_ty_ex(self_ty, fixed_param_ty, true).ok()?;
        let generic_args = self.rewrite(generic_args).no_err();

        Some((
            self.db.intern_concrete_trait(ConcreteTraitLongId { trait_id, generic_args }),
            n_snapshots,
        ))
    }

    /// Infers a generic argument to be passed as a generic parameter.
    /// Allocates a new inference variable of the correct kind, and wraps in a generic argument.
    fn infer_generic_arg(
        &mut self,
        param: &GenericParam,
        lookup_context: ImplLookupContext,
        stable_ptr: Option<SyntaxStablePtrId>,
    ) -> InferenceResult<GenericArgumentId> {
        match param {
            GenericParam::Type(_) => Ok(GenericArgumentId::Type(self.new_type_var(stable_ptr))),
            GenericParam::Impl(param) => Ok(GenericArgumentId::Impl(self.new_impl_var(
                param.concrete_trait?,
                stable_ptr,
                lookup_context,
            )?)),
            GenericParam::Const(_) => Err(InferenceError::ConstInferenceNotSupported),
        }
    }

    /// Infers the impl to be substituted instead of a trait for a given trait function,
    /// and the generic arguments to be passed to the function.
    /// Returns the resulting impl function.
    fn infer_trait_function(
        &mut self,
        concrete_trait_function: ConcreteTraitGenericFunctionId,
        lookup_context: &ImplLookupContext,
        stable_ptr: Option<SyntaxStablePtrId>,
    ) -> InferenceResult<FunctionId> {
        let generic_function =
            self.infer_trait_generic_function(concrete_trait_function, lookup_context, stable_ptr)?;
        self.infer_generic_function(generic_function, lookup_context, stable_ptr)
    }

    /// Infers generic arguments to be passed to a generic function.
    /// Returns the resulting specialized function.
    fn infer_generic_function(
        &mut self,
        generic_function: GenericFunctionId,
        lookup_context: &ImplLookupContext,
        stable_ptr: Option<SyntaxStablePtrId>,
    ) -> InferenceResult<FunctionId> {
        let generic_params = generic_function.generic_params(self.db)?;
        let generic_args = self.infer_generic_args(&generic_params, lookup_context, stable_ptr)?;
        Ok(self.db.intern_function(FunctionLongId {
            function: ConcreteFunction { generic_function, generic_args },
        }))
    }

    /// Infers the impl to be substituted instead of a trait for a given trait function.
    /// Returns the resulting impl generic function.
    fn infer_trait_generic_function(
        &mut self,
        trait_function: ConcreteTraitGenericFunctionId,
        lookup_context: &ImplLookupContext,
        stable_ptr: Option<SyntaxStablePtrId>,
    ) -> InferenceResult<GenericFunctionId> {
        let impl_id = self.new_impl_var(
            trait_function.concrete_trait_id(self.db),
            stable_ptr,
            lookup_context.clone(),
        )?;
        Ok(GenericFunctionId::Impl(ImplGenericFunctionId {
            impl_id,
            function: trait_function.function_id(self.db),
        }))
    }
}
