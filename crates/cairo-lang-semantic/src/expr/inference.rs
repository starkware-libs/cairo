//! Bidirectional type inference.

use std::collections::HashMap;

use cairo_lang_defs::ids::{GenericParamId, TraitFunctionId};
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use itertools::{zip_eq, Itertools};

use crate::corelib::never_ty;
use crate::db::SemanticGroup;
use crate::types::{substitute_generics_args, ConcreteEnumLongId, GenericSubstitution};
use crate::{
    ConcreteEnumId, ConcreteTraitId, ConcreteTraitLongId, ConcreteTypeId, ConcreteVariant,
    GenericArgumentId, Pattern, TypeId, TypeLongId,
};

/// A type variable, created when a generic type argument is not passed, and thus is not known
/// yet and needs to be inferred.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct TypeVar {
    pub id: usize,
}

// TODO(spapini): Add to diagnostics.
#[derive(Debug)]
pub enum InferenceError {
    Disabled,
    Cycle { type_var: TypeVar },
    KindMismatch { ty0: TypeId, ty1: TypeId },
    GenericArgMismatch { garg0: GenericArgumentId, garg1: GenericArgumentId },
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
            GenericArgumentId::Literal(_) => garg,
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
        let ty0 = self.reduce_ty(ty0);
        let ty1 = self.reduce_ty(ty1);
        if ty0 == never_ty(self.db) {
            return Ok(ty1);
        }
        if ty0 == ty1 {
            return Ok(ty0);
        }
        let long_ty0 = self.db.lookup_intern_type(ty0);
        let long_ty1 = self.db.lookup_intern_type(ty1);
        match long_ty1 {
            TypeLongId::Var(var) => return self.assign(var, ty0),
            TypeLongId::Missing(_) => return Ok(ty1),
            _ => {}
        }
        match long_ty0 {
            TypeLongId::Concrete(concrete0) => {
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
                Ok(self.db.intern_type(long_ty))
            }
            TypeLongId::Tuple(tys0) => {
                let TypeLongId::Tuple(tys1) = long_ty1 else {
                    return Err(InferenceError::KindMismatch { ty0, ty1 });
                };
                if tys0.len() != tys1.len() {
                    return Err(InferenceError::KindMismatch { ty0, ty1 });
                }
                let tys = zip_eq(tys0, tys1)
                    .into_iter()
                    .map(|(subty0, subty1)| self.conform_ty(subty0, subty1))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(self.db.intern_type(TypeLongId::Tuple(tys)))
            }
            TypeLongId::GenericParameter(_) => Err(InferenceError::KindMismatch { ty0, ty1 }),
            TypeLongId::Var(var) => self.assign(var, ty1),
            TypeLongId::Missing(_) => Ok(ty0),
        }
    }

    /// Conforms generics args. See `conform_ty()`.
    fn conform_generic_args(
        &mut self,
        gargs0: &[GenericArgumentId],
        gargs1: &[GenericArgumentId],
    ) -> Result<Vec<GenericArgumentId>, InferenceError> {
        zip_eq(gargs0, gargs1)
            .into_iter()
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
            GenericArgumentId::Literal(_) => {
                Err(InferenceError::GenericArgMismatch { garg0, garg1 })
            }
        }
    }

    /// Assigns a value to a [TypeVar]. Return the assigned type, or an error.
    /// Assumes the variable is not already assigned.
    fn assign(&mut self, var: TypeVar, ty: TypeId) -> Result<TypeId, InferenceError> {
        if !self.enabled {
            return Err(InferenceError::Disabled);
        }
        assert!(!self.assignment.contains_key(&var), "Cannot reassign variable.");
        if self.contains_var(ty, var) {
            return Err(InferenceError::Cycle { type_var: var });
        }
        self.assignment.insert(var, ty);
        Ok(ty)
    }

    /// Checks if the a type tree contains a certain [TypeVar] somewhere. Used to avoid inference
    /// cycles.
    pub fn contains_var(&mut self, ty: TypeId, var: TypeVar) -> bool {
        match self.db.lookup_intern_type(self.reduce_ty(ty)) {
            TypeLongId::Concrete(concrete) => {
                concrete.generic_args(self.db).into_iter().any(|garg| match garg {
                    GenericArgumentId::Type(ty) => self.contains_var(ty, var),
                    GenericArgumentId::Literal(_) => false,
                })
            }
            TypeLongId::Tuple(tys) => tys.into_iter().any(|ty| self.contains_var(ty, var)),
            TypeLongId::Var(new_var) => {
                if new_var == var {
                    return true;
                }
                if let Some(ty) = self.assignment.get(&new_var) {
                    return self.contains_var(*ty, var);
                }
                false
            }
            TypeLongId::GenericParameter(_) | TypeLongId::Missing(_) => false,
        }
    }

    /// Determines if an assignment to `generic_params` can be chosen s.t. `generic_args` will be
    /// substituted to `expected_generic_args`.
    pub fn can_infer_generics(
        &self,
        generic_params: &[GenericParamId],
        generic_args: &[GenericArgumentId],
        expected_generic_args: &[GenericArgumentId],
    ) -> bool {
        if generic_args.len() != expected_generic_args.len() {
            return false;
        }
        let mut inference = self.clone();
        let res = inference.infer_generics(generic_params, generic_args, expected_generic_args);
        res.is_ok()
    }

    /// Chooses and assignment to generic_params s.t. generic_args will be substituted to
    /// expected_generic_args.
    /// Returns the generic_params assignment.
    pub fn infer_generics(
        &mut self,
        generic_params: &[GenericParamId],
        generic_args: &[GenericArgumentId],
        expected_generic_args: &[GenericArgumentId],
    ) -> Result<Vec<GenericArgumentId>, InferenceError> {
        // TODO(spapini): Handle non-type generic args.
        let substitution = GenericSubstitution(
            generic_params
                .iter()
                .map(|param| {
                    (
                        *param,
                        GenericArgumentId::Type(
                            self.new_var(param.stable_ptr(self.db.upcast()).untyped()),
                        ),
                    )
                })
                .collect(),
        );
        let mut generic_args = generic_args.iter().copied().collect_vec();
        substitute_generics_args(self.db, &substitution, &mut generic_args);
        self.conform_generic_args(&generic_args, expected_generic_args)?;
        Ok(generic_params.iter().map(|param| substitution[*param]).collect())
    }

    /// Tries to infer a trait function as a method for `self_ty`.
    pub fn infer_concrete_trait_by_self(
        &mut self,
        trait_function: TraitFunctionId,
        self_ty: TypeId,
    ) -> Option<ConcreteTraitId> {
        let trait_id = trait_function.trait_id(self.db.upcast());
        let signature = self.db.trait_function_signature(trait_function).ok()?;
        let first_param = signature.params.into_iter().next()?;
        if first_param.name != "self" {
            return None;
        }
        let generic_params = self.db.trait_generic_params(trait_id).ok()?;

        let generic_args = self
            .infer_generics(
                &generic_params,
                &[GenericArgumentId::Type(first_param.ty)],
                &[GenericArgumentId::Type(self_ty)],
            )
            .ok()?;
        Some(self.db.intern_concrete_trait(ConcreteTraitLongId { trait_id, generic_args }))
    }
}
