//! Bidirectional type inference.

use std::collections::HashMap;

use cairo_lang_defs::ids::{GenericKind, ImplDefId, TraitFunctionId, TraitId};
use cairo_lang_diagnostics::DiagnosticAdded;
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use itertools::{zip_eq, Itertools};

use crate::corelib::never_ty;
use crate::db::SemanticGroup;
use crate::items::imp::ImplId;
use crate::types::{
    peel_snapshots, substitute_generics_args_inplace, substitute_ty, ConcreteEnumLongId,
    GenericSubstitution,
};
use crate::{
    ConcreteEnumId, ConcreteImplLongId, ConcreteTraitId, ConcreteTraitLongId, ConcreteTypeId,
    ConcreteVariant, GenericArgumentId, GenericParam, Pattern, TypeId, TypeLongId,
};

/// A type variable, created when a generic type argument is not passed, and thus is not known
/// yet and needs to be inferred.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct TypeVar {
    pub id: usize,
}

// TODO(spapini): Add to diagnostics.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum InferenceError {
    Disabled,
    Failed(DiagnosticAdded),
    Cycle { type_var: TypeVar },
    KindMismatch { ty0: TypeId, ty1: TypeId },
    GenericArgMismatch { garg0: GenericArgumentId, garg1: GenericArgumentId },
    TraitMismatch { trt0: TraitId, trt1: TraitId },
}

/// State of inference.
#[derive(Clone)]
pub struct Inference<'db> {
    db: &'db dyn SemanticGroup,
    /// Current inferred assignment for type variables.
    assignment: HashMap<TypeVar, TypeId>,
    /// Stable pointers for each type variable, used for reporting diagnostics properly.
    var_ptrs: Vec<SyntaxStablePtrId>,
    /// Whether inference is enabled.
    pub enabled: bool,
    // TODO(spapini): Rank.
}
impl<'db> Inference<'db> {
    /// Creates a new [Inference] instance.
    pub fn new(db: &'db dyn SemanticGroup) -> Self {
        Self { db, assignment: Default::default(), var_ptrs: vec![], enabled: true }
    }

    /// Creates a disabled [Inference] instance, where no inference is being performed.
    pub fn disabled(db: &'db dyn SemanticGroup) -> Self {
        Self { db, assignment: Default::default(), var_ptrs: vec![], enabled: false }
    }

    /// Allocated a new [TypeVar] for an unknown type that needs to be inferred,
    pub fn new_var(&mut self, stable_ptr: SyntaxStablePtrId) -> TypeId {
        let res = self.db.intern_type(TypeLongId::Var(TypeVar { id: self.var_ptrs.len() }));
        self.var_ptrs.push(stable_ptr);
        res
    }

    /// Retrieves the first variable that is still not inferred, or None, if everything is
    /// inferred.
    pub fn first_undetermined_variable(&self) -> Option<SyntaxStablePtrId> {
        for (id, stable_ptr) in self.var_ptrs.iter().enumerate() {
            if !self.assignment.contains_key(&TypeVar { id }) {
                return Some(*stable_ptr);
            }
        }
        None
    }

    /// Gets current canonical representation for a [TypeId] after all known substitutions.
    pub fn reduce_ty(&mut self, ty: TypeId) -> TypeId {
        let long_type_id = self.db.lookup_intern_type(ty);
        let long_type_id = match long_type_id {
            TypeLongId::Concrete(concrete) => {
                let generic_args = self.reduce_generic_args(&concrete.generic_args(self.db));
                TypeLongId::Concrete(ConcreteTypeId::new(
                    self.db,
                    concrete.generic_type(self.db),
                    generic_args,
                ))
            }
            TypeLongId::Tuple(tys) => {
                TypeLongId::Tuple(tys.into_iter().map(|ty| self.reduce_ty(ty)).collect())
            }
            TypeLongId::Snapshot(ty) => TypeLongId::Snapshot(self.reduce_ty(ty)),
            TypeLongId::Var(var) => return self.reduce_var(var),
            TypeLongId::GenericParameter(_) | TypeLongId::Missing(_) => return ty,
        };
        self.db.intern_type(long_type_id)
    }

    /// Gets current canonical representation for a [ConcreteVariant] after all known substitutions.
    pub fn reduce_concrete_variant(&mut self, concrete_variant: &mut ConcreteVariant) {
        concrete_variant.ty = self.reduce_ty(concrete_variant.ty);
        concrete_variant.concrete_enum_id =
            self.reduce_concrete_enum(concrete_variant.concrete_enum_id);
    }

    /// Gets current canonical representation for a [ConcreteEnumId] after all known substitutions.
    pub fn reduce_concrete_enum(&mut self, concrete_enum_id: ConcreteEnumId) -> ConcreteEnumId {
        let concrete_enum = self.db.lookup_intern_concrete_enum(concrete_enum_id);
        let generic_args = self.reduce_generic_args(&concrete_enum.generic_args);

        self.db.intern_concrete_enum(ConcreteEnumLongId { generic_args, ..concrete_enum })
    }

    /// Gets current canonical representation for a [TypeVar] after all known substitutions.
    pub fn reduce_var(&mut self, var: TypeVar) -> TypeId {
        if let Some(new_ty) = self.assignment.get(&var) {
            let new_ty = self.reduce_ty(*new_ty);
            self.assignment.insert(var, new_ty);
            return new_ty;
        }
        self.db.intern_type(TypeLongId::Var(var))
    }

    /// Gets current canonical representation for a [GenericArgumentId] vector after all known
    /// substitutions.
    pub fn reduce_generic_args(
        &mut self,
        generic_args: &[GenericArgumentId],
    ) -> Vec<GenericArgumentId> {
        generic_args.iter().copied().map(|garg| self.reduce_generic_arg(garg)).collect()
    }

    /// Gets current canonical representation for a [GenericArgumentId] after all known
    /// substitutions.
    fn reduce_generic_arg(&mut self, garg: GenericArgumentId) -> GenericArgumentId {
        match garg {
            GenericArgumentId::Type(ty) => GenericArgumentId::Type(self.reduce_ty(ty)),
            GenericArgumentId::Literal(_) | GenericArgumentId::Impl(_) => garg,
        }
    }

    /// Gets current canonical representation for a [Pattern] after all known substitutions, and
    /// updates inplace.
    pub fn reduce_pattern(&mut self, pattern: &mut Pattern) {
        match pattern {
            Pattern::Variable(pat) => pat.var.ty = self.reduce_ty(pat.var.ty),
            Pattern::Struct(pat) => {
                pat.ty = self.reduce_ty(pat.ty);
                for (_, pat) in pat.field_patterns.iter_mut() {
                    self.reduce_pattern(pat);
                }
            }
            Pattern::Tuple(pat) => {
                pat.ty = self.reduce_ty(pat.ty);
                for pat in pat.field_patterns.iter_mut() {
                    self.reduce_pattern(pat);
                }
            }
            Pattern::EnumVariant(pat) => {
                self.reduce_concrete_variant(&mut pat.variant);
                pat.ty = self.reduce_ty(pat.ty);
                self.reduce_pattern(&mut pat.inner_pattern);
            }
            Pattern::Literal(_) | Pattern::Otherwise(_) => {}
        }
    }

    /// Conforms ty0 to ty1. Should be called when ty0 should be coerced to ty1. Not symmetric.
    /// Returns the reduced type for ty0, or an error if the type is no coercible.
    pub fn conform_ty(&mut self, ty0: TypeId, ty1: TypeId) -> Result<TypeId, InferenceError> {
        Ok(self.conform_ty_ex(ty0, ty1, false)?.0)
    }

    /// Same as conform_ty but supports adding snapshots to ty0 if `ty0_is_self` is true.
    /// Returns the reduced type for ty0 and the number of snapshots that needs to be added
    /// for the types to conform.
    pub fn conform_ty_ex(
        &mut self,
        ty0: TypeId,
        ty1: TypeId,
        ty0_is_self: bool,
    ) -> Result<(TypeId, usize), InferenceError> {
        let n_snapshots = 0;

        let ty0 = self.reduce_ty(ty0);
        let ty1 = self.reduce_ty(ty1);
        if ty0 == never_ty(self.db) {
            return Ok((ty1, n_snapshots));
        }
        if ty0 == ty1 {
            return Ok((ty0, n_snapshots));
        }
        let long_ty1 = self.db.lookup_intern_type(ty1);
        match long_ty1 {
            TypeLongId::Var(var) => return Ok((self.assign(var, ty0)?, n_snapshots)),
            TypeLongId::Missing(_) => return Ok((ty1, n_snapshots)),
            TypeLongId::Snapshot(inner_ty) if ty0_is_self && inner_ty == ty0 => {
                return Ok((ty1, n_snapshots));
            }
            _ => {}
        }
        let long_ty0 = self.db.lookup_intern_type(ty0);

        match long_ty0 {
            TypeLongId::Concrete(concrete0) => {
                let (n_snapshots, long_ty1) = peel_snapshots(self.db, ty1);

                let TypeLongId::Concrete(concrete1) = long_ty1 else {
                    return Err(InferenceError::KindMismatch { ty0, ty1 });
                };
                if concrete0.generic_type(self.db) != concrete1.generic_type(self.db) {
                    return Err(InferenceError::KindMismatch { ty0, ty1 });
                }
                let gargs0 = concrete0.generic_args(self.db);
                let gargs1 = concrete1.generic_args(self.db);
                let gargs = self.conform_generic_args(&gargs0, &gargs1)?;
                let long_ty = TypeLongId::Concrete(ConcreteTypeId::new(
                    self.db,
                    concrete0.generic_type(self.db),
                    gargs,
                ));
                Ok((self.db.intern_type(long_ty), n_snapshots))
            }
            TypeLongId::Tuple(tys0) => {
                let (n_snapshots, long_ty1) = peel_snapshots(self.db, ty1);
                let TypeLongId::Tuple(tys1) = long_ty1 else {
                    return Err(InferenceError::KindMismatch { ty0, ty1 });
                };
                if tys0.len() != tys1.len() {
                    return Err(InferenceError::KindMismatch { ty0, ty1 });
                }
                let tys = zip_eq(tys0, tys1)
                    .map(|(subty0, subty1)| self.conform_ty(subty0, subty1))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok((self.db.intern_type(TypeLongId::Tuple(tys)), n_snapshots))
            }
            TypeLongId::Snapshot(ty0) => {
                let TypeLongId::Snapshot(ty1) = long_ty1 else {
                    return Err(InferenceError::KindMismatch { ty0, ty1 });
                };
                let (ty, n_snapshots) = self.conform_ty_ex(ty0, ty1, ty0_is_self)?;
                Ok((self.db.intern_type(TypeLongId::Snapshot(ty)), n_snapshots))
            }
            TypeLongId::GenericParameter(_) => Err(InferenceError::KindMismatch { ty0, ty1 }),
            TypeLongId::Var(var) => Ok((self.assign(var, ty1)?, n_snapshots)),
            TypeLongId::Missing(_) => Ok((ty0, n_snapshots)),
        }
    }

    /// Conforms generics args. See `conform_ty()`.
    fn conform_generic_args(
        &mut self,
        gargs0: &[GenericArgumentId],
        gargs1: &[GenericArgumentId],
    ) -> Result<Vec<GenericArgumentId>, InferenceError> {
        zip_eq(gargs0, gargs1)
            .map(|(garg0, garg1)| self.conform_generic_arg(*garg0, *garg1))
            .collect::<Result<Vec<_>, _>>()
    }

    /// Conforms a generics arg. See `conform_ty()`.
    pub fn conform_generic_arg(
        &mut self,
        garg0: GenericArgumentId,
        garg1: GenericArgumentId,
    ) -> Result<GenericArgumentId, InferenceError> {
        if garg0 == garg1 {
            return Ok(garg0);
        }
        match garg0 {
            GenericArgumentId::Type(gty0) => {
                let GenericArgumentId::Type(gty1) = garg1 else {
                    return Err(InferenceError::GenericArgMismatch { garg0, garg1 });
                };
                Ok(GenericArgumentId::Type(self.conform_ty(gty0, gty1)?))
            }
            GenericArgumentId::Literal(_) | GenericArgumentId::Impl(_) => {
                Err(InferenceError::GenericArgMismatch { garg0, garg1 })
            }
        }
    }

    /// Conforms generics traits. See `conform_ty()`.
    pub fn conform_traits(
        &mut self,
        trt0: ConcreteTraitId,
        trt1: ConcreteTraitId,
    ) -> Result<ConcreteTraitId, InferenceError> {
        let trt0 = self.db.lookup_intern_concrete_trait(trt0);
        let trt1 = self.db.lookup_intern_concrete_trait(trt1);
        if trt0.trait_id != trt1.trait_id {
            return Err(InferenceError::TraitMismatch { trt0: trt0.trait_id, trt1: trt1.trait_id });
        }
        let generic_args = self.conform_generic_args(&trt0.generic_args, &trt1.generic_args)?;
        Ok(self
            .db
            .intern_concrete_trait(ConcreteTraitLongId { trait_id: trt0.trait_id, generic_args }))
    }

    /// Assigns a value to a [TypeVar]. Return the assigned type, or an error.
    /// Assumes the variable is not already assigned.
    fn assign(&mut self, var: TypeVar, ty: TypeId) -> Result<TypeId, InferenceError> {
        if !self.enabled {
            return Err(InferenceError::Disabled);
        }
        assert!(!self.assignment.contains_key(&var), "Cannot reassign variable.");
        if self.ty_contains_var(ty, var) {
            return Err(InferenceError::Cycle { type_var: var });
        }
        self.assignment.insert(var, ty);
        Ok(ty)
    }

    /// Checks if a type tree contains a certain [TypeVar] somewhere. Used to avoid inference
    /// cycles.
    pub fn ty_contains_var(&mut self, ty: TypeId, var: TypeVar) -> bool {
        match self.db.lookup_intern_type(self.reduce_ty(ty)) {
            TypeLongId::Concrete(concrete) => {
                let generic_args = concrete.generic_args(self.db);
                self.generic_args_contain_var(&generic_args, var)
            }
            TypeLongId::Tuple(tys) => tys.into_iter().any(|ty| self.ty_contains_var(ty, var)),
            TypeLongId::Snapshot(ty) => self.ty_contains_var(ty, var),
            TypeLongId::Var(new_var) => {
                if new_var == var {
                    return true;
                }
                if let Some(ty) = self.assignment.get(&new_var) {
                    return self.ty_contains_var(*ty, var);
                }
                false
            }
            TypeLongId::GenericParameter(_) | TypeLongId::Missing(_) => false,
        }
    }

    /// Checks if a slice of generics arguments contain a certain [TypeVar] somewhere. Used to avoid
    /// inference cycles.
    fn generic_args_contain_var(
        &mut self,
        generic_args: &[GenericArgumentId],
        var: TypeVar,
    ) -> bool {
        generic_args.iter().any(|garg| match garg {
            GenericArgumentId::Type(ty) => self.ty_contains_var(*ty, var),
            GenericArgumentId::Literal(_) => false,
            GenericArgumentId::Impl(impl_id) => match impl_id {
                ImplId::Concrete(concrete_impl_id) => self.generic_args_contain_var(
                    &self.db.lookup_intern_concrete_impl(*concrete_impl_id).generic_args,
                    var,
                ),
                ImplId::GenericParameter(_) => false,
            },
        })
    }

    /// Determines if an assignment to `generic_params` can be chosen s.t. `generic_args` will be
    /// substituted to `expected_generic_args`.
    pub fn can_infer_generics(
        &self,
        generic_params: &[GenericParam],
        generic_args: &[GenericArgumentId],
        expected_generic_args: &[GenericArgumentId],
        stable_ptr: SyntaxStablePtrId,
    ) -> bool {
        if generic_args.len() != expected_generic_args.len() {
            return false;
        }
        if generic_params.iter().any(|param| param.kind() != GenericKind::Type) {
            // Inference for non type generics are not supported yet.
            return false;
        }
        let mut inference = self.clone();
        let res = inference.infer_generics(
            generic_params,
            generic_args,
            expected_generic_args,
            stable_ptr,
        );
        res.is_ok()
    }

    /// Determines if a impl (possibly with free generic params) can provide a concrete trait.
    pub fn can_impl_trait(
        &self,
        impl_def_id: ImplDefId,
        concrete_trait_id: ConcreteTraitId,
        stable_ptr: SyntaxStablePtrId,
    ) -> bool {
        let Ok(imp_generic_param) = self.db.impl_def_generic_params(impl_def_id) else {
            return false
        };
        let Ok(imp_concrete_trait) = self.db.impl_def_concrete_trait(impl_def_id) else {
            return false
        };
        if imp_concrete_trait.trait_id(self.db) != concrete_trait_id.trait_id(self.db) {
            return false;
        }

        let long_concrete_trait = self.db.lookup_intern_concrete_trait(concrete_trait_id);
        let long_imp_concrete_trait = self.db.lookup_intern_concrete_trait(imp_concrete_trait);
        self.can_infer_generics(
            &imp_generic_param,
            &long_imp_concrete_trait.generic_args,
            &long_concrete_trait.generic_args,
            stable_ptr,
        )
    }

    /// Infers all the variables required to make an impl (possibly with free generic params) can
    /// provide a concrete trait.
    pub fn infer_impl_trait(
        &mut self,
        impl_def_id: ImplDefId,
        concrete_trait_id: ConcreteTraitId,
        stable_ptr: SyntaxStablePtrId,
    ) -> Result<ImplId, InferenceError> {
        let imp_generic_params =
            self.db.impl_def_generic_params(impl_def_id).map_err(InferenceError::Failed)?;
        let imp_concrete_trait =
            self.db.impl_def_concrete_trait(impl_def_id).map_err(InferenceError::Failed)?;
        if imp_concrete_trait.trait_id(self.db) != concrete_trait_id.trait_id(self.db) {
            return Err(InferenceError::TraitMismatch {
                trt0: imp_concrete_trait.trait_id(self.db),
                trt1: concrete_trait_id.trait_id(self.db),
            });
        }

        let long_concrete_trait = self.db.lookup_intern_concrete_trait(concrete_trait_id);
        let long_imp_concrete_trait = self.db.lookup_intern_concrete_trait(imp_concrete_trait);
        let generic_args = self.infer_generics(
            &imp_generic_params,
            &long_imp_concrete_trait.generic_args,
            &long_concrete_trait.generic_args,
            stable_ptr,
        )?;
        Ok(ImplId::Concrete(
            self.db.intern_concrete_impl(ConcreteImplLongId { impl_def_id, generic_args }),
        ))
    }

    /// Chooses and assignment to generic_params s.t. generic_args will be substituted to
    /// expected_generic_args.
    /// Returns the generic_params assignment.
    pub fn infer_generics(
        &mut self,
        generic_params: &[GenericParam],
        generic_args: &[GenericArgumentId],
        expected_generic_args: &[GenericArgumentId],
        stable_ptr: SyntaxStablePtrId,
    ) -> Result<Vec<GenericArgumentId>, InferenceError> {
        // TODO(spapini): Handle non-type generic args.
        let substitution = GenericSubstitution(
            generic_params
                .iter()
                .map(|param| (param.id(), GenericArgumentId::Type(self.new_var(stable_ptr))))
                .collect(),
        );
        let mut generic_args = generic_args.iter().copied().collect_vec();
        substitute_generics_args_inplace(self.db, &substitution, &mut generic_args);
        self.conform_generic_args(&generic_args, expected_generic_args)?;

        let generic_args =
            generic_params.iter().map(|param| substitution[param.id()]).collect_vec();
        Ok(self.reduce_generic_args(&generic_args))
    }

    /// Tries to infer a trait function as a method for `self_ty`.
    /// Supports snapshot snapshot coercions.
    ///
    /// Returns the deduced type and the number of snapshots that need to be added to it.
    pub fn infer_concrete_trait_by_self(
        &mut self,
        trait_function: TraitFunctionId,
        self_ty: TypeId,
        stable_ptr: SyntaxStablePtrId,
    ) -> Option<(ConcreteTraitId, usize)> {
        let trait_id = trait_function.trait_id(self.db.upcast());
        let signature = self.db.trait_function_signature(trait_function).ok()?;
        let first_param = signature.params.into_iter().next()?;
        if first_param.name != "self" {
            return None;
        }
        let generic_params = self.db.trait_generic_params(trait_id).ok()?;

        let substitution = GenericSubstitution(
            generic_params
                .iter()
                .map(|param| (param.id(), GenericArgumentId::Type(self.new_var(stable_ptr))))
                .collect(),
        );

        let fixed_param_ty = substitute_ty(self.db, &substitution, first_param.ty);
        let (_, n_snapshots) = self.conform_ty_ex(self_ty, fixed_param_ty, true).ok()?;

        let generic_args =
            generic_params.iter().map(|param| substitution[param.id()]).collect_vec();

        Some((
            self.db.intern_concrete_trait(ConcreteTraitLongId { trait_id, generic_args }),
            n_snapshots,
        ))
    }
}
