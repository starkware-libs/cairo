use cairo_lang_defs::ids::{ImplAliasId, ImplDefId, TraitFunctionId};
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_utils::{Intern, LookupIntern, extract_matches, require};
use itertools::Itertools;

use super::canonic::ResultNoErrEx;
use super::conform::InferenceConform;
use super::{Inference, InferenceError, InferenceResult};
use crate::items::constant::ImplConstantId;
use crate::items::functions::{GenericFunctionId, ImplGenericFunctionId};
use crate::items::generics::GenericParamConst;
use crate::items::imp::{
    GeneratedImplLongId, ImplId, ImplImplId, ImplLongId, ImplLookupContext, UninferredImpl,
};
use crate::items::trt::{
    ConcreteTraitConstantId, ConcreteTraitGenericFunctionId, ConcreteTraitImplId,
    ConcreteTraitTypeId,
};
use crate::substitution::{GenericSubstitution, SemanticRewriter};
use crate::types::ImplTypeId;
use crate::{
    ConcreteFunction, ConcreteImplLongId, ConcreteTraitId, ConcreteTraitLongId, FunctionId,
    FunctionLongId, GenericArgumentId, GenericParam, TypeId, TypeLongId,
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
        inference_error_cb: impl FnOnce(InferenceError),
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
        concrete_trait_function: ConcreteTraitGenericFunctionId,
        lookup_context: &ImplLookupContext,
        stable_ptr: Option<SyntaxStablePtrId>,
    ) -> ImplGenericFunctionId;
    fn infer_trait_type(
        &mut self,
        concrete_trait_type: ConcreteTraitTypeId,
        lookup_context: &ImplLookupContext,
        stable_ptr: Option<SyntaxStablePtrId>,
    ) -> TypeId;
    fn infer_trait_constant(
        &mut self,
        concrete_trait_constant: ConcreteTraitConstantId,
        lookup_context: &ImplLookupContext,
        stable_ptr: Option<SyntaxStablePtrId>,
    ) -> ImplConstantId;
    fn infer_trait_impl(
        &mut self,
        concrete_trait_constant: ConcreteTraitImplId,
        lookup_context: &ImplLookupContext,
        stable_ptr: Option<SyntaxStablePtrId>,
    ) -> ImplImplId;
}

impl InferenceEmbeddings for Inference<'_> {
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
            UninferredImpl::ImplImpl(impl_impl_id) => {
                ImplLongId::ImplImpl(impl_impl_id).intern(self.db)
            }
            UninferredImpl::GenericParam(param_id) => {
                let param = self
                    .db
                    .generic_param_semantic(param_id)
                    .map_err(|diag_added| self.set_error(InferenceError::Reported(diag_added)))?;
                let param = extract_matches!(param, GenericParam::Impl);
                let imp_concrete_trait_id = param.concrete_trait.unwrap();
                self.conform_traits(concrete_trait_id, imp_concrete_trait_id)?;
                ImplLongId::GenericParameter(param_id).intern(self.db)
            }
            UninferredImpl::GeneratedImpl(generated_impl) => {
                let long_id = generated_impl.lookup_intern(self.db);

                // Only making sure the args can be inferred - as they are unused later.
                self.infer_generic_args(&long_id.generic_params[..], lookup_context, stable_ptr)?;

                ImplLongId::GeneratedImpl(
                    GeneratedImplLongId {
                        concrete_trait: long_id.concrete_trait,
                        generic_params: long_id.generic_params,
                        impl_items: long_id.impl_items,
                    }
                    .intern(self.db),
                )
                .intern(self.db)
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
        let imp_generic_params = self
            .db
            .impl_def_generic_params(impl_def_id)
            .map_err(|diag_added| self.set_error(InferenceError::Reported(diag_added)))?;
        let imp_concrete_trait = self
            .db
            .impl_def_concrete_trait(impl_def_id)
            .map_err(|diag_added| self.set_error(InferenceError::Reported(diag_added)))?;
        if imp_concrete_trait.trait_id(self.db) != concrete_trait_id.trait_id(self.db) {
            return Err(self.set_error(InferenceError::TraitMismatch {
                trt0: imp_concrete_trait.trait_id(self.db),
                trt1: concrete_trait_id.trait_id(self.db),
            }));
        }

        let long_concrete_trait = concrete_trait_id.lookup_intern(self.db);
        let long_imp_concrete_trait = imp_concrete_trait.lookup_intern(self.db);
        let generic_args = self.infer_generic_assignment(
            &imp_generic_params,
            &long_imp_concrete_trait.generic_args,
            &long_concrete_trait.generic_args,
            lookup_context,
            stable_ptr,
        )?;
        Ok(ImplLongId::Concrete(ConcreteImplLongId { impl_def_id, generic_args }.intern(self.db))
            .intern(self.db))
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
        let impl_alias_generic_params = self
            .db
            .impl_alias_generic_params(impl_alias_id)
            .map_err(|diag_added| self.set_error(InferenceError::Reported(diag_added)))?;
        let impl_id = self
            .db
            .impl_alias_resolved_impl(impl_alias_id)
            .map_err(|diag_added| self.set_error(InferenceError::Reported(diag_added)))?;
        let imp_concrete_trait = impl_id
            .concrete_trait(self.db)
            .map_err(|diag_added| self.set_error(InferenceError::Reported(diag_added)))?;
        if imp_concrete_trait.trait_id(self.db) != concrete_trait_id.trait_id(self.db) {
            return Err(self.set_error(InferenceError::TraitMismatch {
                trt0: imp_concrete_trait.trait_id(self.db),
                trt1: concrete_trait_id.trait_id(self.db),
            }));
        }

        let long_concrete_trait = concrete_trait_id.lookup_intern(self.db);
        let long_imp_concrete_trait = imp_concrete_trait.lookup_intern(self.db);
        let generic_args = self.infer_generic_assignment(
            &impl_alias_generic_params,
            &long_imp_concrete_trait.generic_args,
            &long_concrete_trait.generic_args,
            lookup_context,
            stable_ptr,
        )?;

        GenericSubstitution::new(&impl_alias_generic_params, &generic_args)
            .substitute(self.db, impl_id)
            .map_err(|diag_added| self.set_error(InferenceError::Reported(diag_added)))
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
        let generic_args = substitution
            .substitute(self.db, generic_args.iter().copied().collect_vec())
            .map_err(|diag_added| self.set_error(InferenceError::Reported(diag_added)))?;
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
            let generic_param = substitution
                .substitute(self.db, generic_param.clone())
                .map_err(|diag_added| self.set_error(InferenceError::Reported(diag_added)))?;
            let generic_arg =
                self.infer_generic_arg(&generic_param, lookup_context.clone(), stable_ptr)?;
            generic_args.push(generic_arg);
            substitution.insert(generic_param.id(), generic_arg);
        }
        Ok(generic_args)
    }

    /// Tries to infer a trait function as a method for `self_ty`.
    /// Supports snapshot snapshot coercions.
    ///
    /// Returns the deduced type and the number of snapshots that need to be added to it.
    ///
    /// `inference_error_cb` is called for inference errors, but they are not reported here as
    /// diagnostics. The caller has to make sure the diagnostics are reported appropriately.
    fn infer_concrete_trait_by_self(
        &mut self,
        trait_function: TraitFunctionId,
        self_ty: TypeId,
        lookup_context: &ImplLookupContext,
        stable_ptr: Option<SyntaxStablePtrId>,
        inference_error_cb: impl FnOnce(InferenceError),
    ) -> Option<(ConcreteTraitId, usize)> {
        let trait_id = trait_function.trait_id(self.db.upcast());
        let signature = self.db.trait_function_signature(trait_function).ok()?;
        let first_param = signature.params.into_iter().next()?;
        require(first_param.name == "self")?;

        let trait_generic_params = self.db.trait_generic_params(trait_id).ok()?;
        let trait_generic_args =
            match self.infer_generic_args(&trait_generic_params, lookup_context, stable_ptr) {
                Ok(generic_args) => generic_args,
                Err(err_set) => {
                    if let Some(err) = self.consume_error_without_reporting(err_set) {
                        inference_error_cb(err);
                    }
                    return None;
                }
            };

        // TODO(yuval): Try to not temporary clone.
        let mut tmp_inference_data = self.temporary_clone();
        let mut tmp_inference = tmp_inference_data.inference(self.db);
        let function_generic_params =
            tmp_inference.db.trait_function_generic_params(trait_function).ok()?;
        let function_generic_args =
            // TODO(yuval): consider getting the substitution from inside `infer_generic_args`
            // instead of creating it again here.
            match tmp_inference.infer_generic_args(&function_generic_params, lookup_context, stable_ptr) {
                Ok(generic_args) => generic_args,
                Err(err_set) => {
                    if let Some(err) = self.consume_error_without_reporting(err_set) {
                        inference_error_cb(err);
                    }
                    return None;
                }
            };

        let trait_substitution =
            GenericSubstitution::new(&trait_generic_params, &trait_generic_args);
        let function_substitution =
            GenericSubstitution::new(&function_generic_params, &function_generic_args);
        let substitution = trait_substitution.concat(function_substitution);

        let fixed_param_ty = substitution.substitute(self.db, first_param.ty).ok()?;
        let (_, n_snapshots) = match self.conform_ty_ex(self_ty, fixed_param_ty, true) {
            Ok(conform) => conform,
            Err(err_set) => {
                if let Some(err) = self.consume_error_without_reporting(err_set) {
                    inference_error_cb(err);
                }
                return None;
            }
        };

        let generic_args = self.rewrite(trait_generic_args).no_err();

        Some((ConcreteTraitLongId { trait_id, generic_args }.intern(self.db), n_snapshots))
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
            GenericParam::Impl(param) => {
                let concrete_trait_id = param
                    .concrete_trait
                    .map_err(|diag_added| self.set_error(InferenceError::Reported(diag_added)))?;
                let impl_id = self.new_impl_var(concrete_trait_id, stable_ptr, lookup_context);
                for (trait_ty, ty1) in param.type_constraints.iter() {
                    let ty0 = self.reduce_impl_ty(ImplTypeId::new(impl_id, *trait_ty, self.db))?;
                    // Conforming the type will always work as the impl is a new inference variable.
                    self.conform_ty(ty0, *ty1).ok();
                }
                Ok(GenericArgumentId::Impl(impl_id))
            }
            GenericParam::Const(GenericParamConst { ty, .. }) => {
                Ok(GenericArgumentId::Constant(self.new_const_var(stable_ptr, *ty)))
            }
            GenericParam::NegImpl(_) => Ok(GenericArgumentId::NegImpl),
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
        let generic_function = GenericFunctionId::Impl(self.infer_trait_generic_function(
            concrete_trait_function,
            lookup_context,
            stable_ptr,
        ));
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
        let generic_params = generic_function
            .generic_params(self.db)
            .map_err(|diag_added| self.set_error(InferenceError::Reported(diag_added)))?;
        let generic_args = self.infer_generic_args(&generic_params, lookup_context, stable_ptr)?;
        Ok(FunctionLongId { function: ConcreteFunction { generic_function, generic_args } }
            .intern(self.db))
    }

    /// Infers the impl to be substituted instead of a trait for a given trait function.
    /// Returns the resulting impl generic function.
    fn infer_trait_generic_function(
        &mut self,
        concrete_trait_function: ConcreteTraitGenericFunctionId,
        lookup_context: &ImplLookupContext,
        stable_ptr: Option<SyntaxStablePtrId>,
    ) -> ImplGenericFunctionId {
        let impl_id = self.new_impl_var(
            concrete_trait_function.concrete_trait(self.db),
            stable_ptr,
            lookup_context.clone(),
        );
        ImplGenericFunctionId { impl_id, function: concrete_trait_function.trait_function(self.db) }
    }

    /// Infers the impl to be substituted instead of a trait for a given trait type.
    /// Returns the resulting impl type.
    fn infer_trait_type(
        &mut self,
        concrete_trait_type: ConcreteTraitTypeId,
        lookup_context: &ImplLookupContext,
        stable_ptr: Option<SyntaxStablePtrId>,
    ) -> TypeId {
        let impl_id = self.new_impl_var(
            concrete_trait_type.concrete_trait(self.db),
            stable_ptr,
            lookup_context.clone(),
        );
        TypeLongId::ImplType(ImplTypeId::new(
            impl_id,
            concrete_trait_type.trait_type(self.db),
            self.db,
        ))
        .intern(self.db)
    }

    /// Infers the impl to be substituted instead of a trait for a given trait constant.
    /// Returns the resulting impl constant.
    fn infer_trait_constant(
        &mut self,
        concrete_trait_constant: ConcreteTraitConstantId,
        lookup_context: &ImplLookupContext,
        stable_ptr: Option<SyntaxStablePtrId>,
    ) -> ImplConstantId {
        let impl_id = self.new_impl_var(
            concrete_trait_constant.concrete_trait(self.db),
            stable_ptr,
            lookup_context.clone(),
        );

        ImplConstantId::new(impl_id, concrete_trait_constant.trait_constant(self.db), self.db)
    }

    /// Infers the impl to be substituted instead of a trait for a given trait impl.
    /// Returns the resulting impl impl.
    fn infer_trait_impl(
        &mut self,
        concrete_trait_impl: ConcreteTraitImplId,
        lookup_context: &ImplLookupContext,
        stable_ptr: Option<SyntaxStablePtrId>,
    ) -> ImplImplId {
        let impl_id = self.new_impl_var(
            concrete_trait_impl.concrete_trait(self.db),
            stable_ptr,
            lookup_context.clone(),
        );

        ImplImplId::new(impl_id, concrete_trait_impl.trait_impl(self.db), self.db)
    }
}
