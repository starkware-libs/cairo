use itertools::zip_eq;

use super::canonic::ResultNoErrEx;
use super::{Inference, InferenceError, InferenceResult, InferenceVar};
use crate::corelib::never_ty;
use crate::items::imp::ImplId;
use crate::substitution::SemanticRewriter;
use crate::types::peel_snapshots;
use crate::{
    ConcreteImplLongId, ConcreteTraitId, ConcreteTraitLongId, ConcreteTypeId, GenericArgumentId,
    TypeId, TypeLongId,
};

/// Functions for conforming semantic objects with each other.
pub trait InferenceConform {
    fn conform_ty(&mut self, ty0: TypeId, ty1: TypeId) -> Result<TypeId, InferenceError>;
    fn conform_ty_ex(
        &mut self,
        ty0: TypeId,
        ty1: TypeId,
        ty0_is_self: bool,
    ) -> Result<(TypeId, usize), InferenceError>;
    fn maybe_peel_snapshots(&mut self, ty0_is_self: bool, ty1: TypeId) -> (usize, TypeLongId);
    fn conform_generic_args(
        &mut self,
        gargs0: &[GenericArgumentId],
        gargs1: &[GenericArgumentId],
    ) -> Result<Vec<GenericArgumentId>, InferenceError>;
    fn conform_generic_arg(
        &mut self,
        garg0: GenericArgumentId,
        garg1: GenericArgumentId,
    ) -> Result<GenericArgumentId, InferenceError>;
    fn conform_impl(&mut self, impl0: ImplId, impl1: ImplId) -> InferenceResult<ImplId>;
    fn conform_traits(
        &mut self,
        trt0: ConcreteTraitId,
        trt1: ConcreteTraitId,
    ) -> Result<ConcreteTraitId, InferenceError>;
    fn ty_contains_var(&mut self, ty: TypeId, var: InferenceVar) -> InferenceResult<bool>;
    fn generic_args_contain_var(
        &mut self,
        generic_args: &[GenericArgumentId],
        var: InferenceVar,
    ) -> InferenceResult<bool>;
    fn impl_contains_var(
        &mut self,
        impl_id: &ImplId,
        var: InferenceVar,
    ) -> Result<bool, InferenceError>;
}

impl<'db> InferenceConform for Inference<'db> {
    /// Conforms ty0 to ty1. Should be called when ty0 should be coerced to ty1. Not symmetric.
    /// Returns the reduced type for ty0, or an error if the type is no coercible.
    fn conform_ty(&mut self, ty0: TypeId, ty1: TypeId) -> Result<TypeId, InferenceError> {
        Ok(self.conform_ty_ex(ty0, ty1, false)?.0)
    }

    /// Same as conform_ty but supports adding snapshots to ty0 if `ty0_is_self` is true.
    /// Returns the reduced type for ty0 and the number of snapshots that needs to be added
    /// for the types to conform.
    fn conform_ty_ex(
        &mut self,
        ty0: TypeId,
        ty1: TypeId,
        ty0_is_self: bool,
    ) -> Result<(TypeId, usize), InferenceError> {
        let ty0 = self.rewrite(ty0).no_err();
        let ty1 = self.rewrite(ty1).no_err();
        if ty0 == never_ty(self.db) {
            return Ok((ty1, 0));
        }
        if ty0 == ty1 {
            return Ok((ty0, 0));
        }
        let long_ty1 = self.db.lookup_intern_type(ty1);
        match long_ty1 {
            TypeLongId::Var(var) => return Ok((self.assign_ty(var, ty0)?, 0)),
            TypeLongId::Missing(_) => return Ok((ty1, 0)),
            TypeLongId::Snapshot(inner_ty) => {
                if ty0_is_self {
                    if inner_ty == ty0 {
                        return Ok((ty1, 1));
                    }
                    if !matches!(self.db.lookup_intern_type(ty0), TypeLongId::Snapshot(_)) {
                        if let TypeLongId::Var(var) = self.db.lookup_intern_type(inner_ty) {
                            return Ok((self.assign_ty(var, ty0)?, 1));
                        }
                    }
                }
            }
            _ => {}
        }
        let n_snapshots = 0;
        let long_ty0 = self.db.lookup_intern_type(ty0);

        match long_ty0 {
            TypeLongId::Concrete(concrete0) => {
                let (n_snapshots, long_ty1) = self.maybe_peel_snapshots(ty0_is_self, ty1);
                let TypeLongId::Concrete(concrete1) = long_ty1 else {
                    return Err(InferenceError::TypeKindMismatch { ty0, ty1 });
                };
                if concrete0.generic_type(self.db) != concrete1.generic_type(self.db) {
                    return Err(InferenceError::TypeKindMismatch { ty0, ty1 });
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
                let (n_snapshots, long_ty1) = self.maybe_peel_snapshots(ty0_is_self, ty1);
                let TypeLongId::Tuple(tys1) = long_ty1 else {
                    return Err(InferenceError::TypeKindMismatch { ty0, ty1 });
                };
                if tys0.len() != tys1.len() {
                    return Err(InferenceError::TypeKindMismatch { ty0, ty1 });
                }
                let tys = zip_eq(tys0, tys1)
                    .map(|(subty0, subty1)| self.conform_ty(subty0, subty1))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok((self.db.intern_type(TypeLongId::Tuple(tys)), n_snapshots))
            }
            TypeLongId::Snapshot(ty0) => {
                let TypeLongId::Snapshot(ty1) = long_ty1 else {
                    return Err(InferenceError::TypeKindMismatch { ty0, ty1 });
                };
                let (ty, n_snapshots) = self.conform_ty_ex(ty0, ty1, ty0_is_self)?;
                Ok((self.db.intern_type(TypeLongId::Snapshot(ty)), n_snapshots))
            }
            TypeLongId::GenericParameter(_) => Err(InferenceError::TypeKindMismatch { ty0, ty1 }),
            TypeLongId::Var(var) => Ok((self.assign_ty(var, ty1)?, n_snapshots)),
            TypeLongId::Missing(_) => Ok((ty0, n_snapshots)),
        }
    }

    // Conditionally peels snapshots.
    fn maybe_peel_snapshots(&mut self, ty0_is_self: bool, ty1: TypeId) -> (usize, TypeLongId) {
        let (n_snapshots, long_ty1) = if ty0_is_self {
            peel_snapshots(self.db, ty1)
        } else {
            (0, self.db.lookup_intern_type(ty1))
        };
        (n_snapshots, long_ty1)
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
    fn conform_generic_arg(
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
            GenericArgumentId::Impl(impl0) => {
                let GenericArgumentId::Impl(impl1) = garg1 else {
                    return Err(InferenceError::GenericArgMismatch { garg0, garg1 });
                };
                Ok(GenericArgumentId::Impl(self.conform_impl(impl0, impl1)?))
            }
        }
    }

    /// Conforms an impl. See `conform_ty()`.
    fn conform_impl(&mut self, impl0: ImplId, impl1: ImplId) -> InferenceResult<ImplId> {
        let impl0 = self.rewrite(impl0).no_err();
        let impl1 = self.rewrite(impl1).no_err();
        if impl0 == impl1 {
            return Ok(impl0);
        }
        if let ImplId::ImplVar(var) = impl1 {
            self.conform_traits(
                var.get(self.db).concrete_trait_id,
                self.db.impl_concrete_trait(impl0)?,
            )?;
            let impl_id = self.rewrite(impl0).no_err();
            return self.assign_impl(var.get(self.db).id, impl_id);
        }
        match impl0 {
            ImplId::ImplVar(var) => {
                self.conform_traits(
                    var.get(self.db).concrete_trait_id,
                    self.db.impl_concrete_trait(impl1)?,
                )?;
                let impl_id = self.rewrite(impl1).no_err();
                self.assign_impl(var.get(self.db).id, impl_id)
            }
            ImplId::Concrete(concrete0) => {
                let ImplId::Concrete(concrete1) = impl1 else {
                    return Err(InferenceError::ImplKindMismatch { impl0, impl1 });
                };
                let concrete0 = self.db.lookup_intern_concrete_impl(concrete0);
                let concrete1 = self.db.lookup_intern_concrete_impl(concrete1);
                if concrete0.impl_def_id != concrete1.impl_def_id {
                    return Err(InferenceError::ImplKindMismatch { impl0, impl1 });
                }
                let gargs0 = concrete0.generic_args;
                let gargs1 = concrete1.generic_args;
                let generic_args = self.conform_generic_args(&gargs0, &gargs1)?;
                Ok(ImplId::Concrete(self.db.intern_concrete_impl(ConcreteImplLongId {
                    impl_def_id: concrete0.impl_def_id,
                    generic_args,
                })))
            }
            ImplId::GenericParameter(_) => Err(InferenceError::ImplKindMismatch { impl0, impl1 }),
        }
    }

    /// Conforms generics traits. See `conform_ty()`.
    fn conform_traits(
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

    /// Checks if a type tree contains a certain [InferenceVar] somewhere. Used to avoid inference
    /// cycles.
    fn ty_contains_var(&mut self, ty: TypeId, var: InferenceVar) -> InferenceResult<bool> {
        Ok(match self.db.lookup_intern_type(self.rewrite(ty).no_err()) {
            TypeLongId::Concrete(concrete) => {
                let generic_args = concrete.generic_args(self.db);
                self.generic_args_contain_var(&generic_args, var)?
            }
            TypeLongId::Tuple(tys) => tys
                .into_iter()
                .map(|ty| self.ty_contains_var(ty, var))
                .collect::<InferenceResult<Vec<_>>>()?
                .into_iter()
                .any(|x| x),
            TypeLongId::Snapshot(ty) => self.ty_contains_var(ty, var)?,
            TypeLongId::Var(new_var) => {
                if InferenceVar::Type(new_var.id) == var {
                    return Ok(true);
                }
                if let Some(ty) = self.type_assignment.get(&new_var.id) {
                    return self.ty_contains_var(*ty, var);
                }
                false
            }
            TypeLongId::GenericParameter(_) | TypeLongId::Missing(_) => false,
        })
    }

    /// Checks if a slice of generics arguments contain a certain [InferenceVar] somewhere. Used to
    /// avoid inference cycles.
    fn generic_args_contain_var(
        &mut self,
        generic_args: &[GenericArgumentId],
        var: InferenceVar,
    ) -> InferenceResult<bool> {
        for garg in generic_args {
            if match garg {
                GenericArgumentId::Type(ty) => self.ty_contains_var(*ty, var)?,
                GenericArgumentId::Literal(_) => false,
                GenericArgumentId::Impl(impl_id) => self.impl_contains_var(impl_id, var)?,
            } {
                return Ok(true);
            }
        }
        Ok(false)
    }

    /// Checks if an impl contains a certain [InferenceVar] somewhere. Used to avoid inference
    /// cycles.
    fn impl_contains_var(
        &mut self,
        impl_id: &ImplId,
        var: InferenceVar,
    ) -> Result<bool, InferenceError> {
        Ok(match impl_id {
            ImplId::Concrete(concrete_impl_id) => self.generic_args_contain_var(
                &self.db.lookup_intern_concrete_impl(*concrete_impl_id).generic_args,
                var,
            )?,
            ImplId::GenericParameter(_) => false,
            ImplId::ImplVar(new_var) => {
                if InferenceVar::Impl(new_var.get(self.db).id) == var {
                    return Ok(true);
                }
                if let Some(impl_id) = self.impl_assignment(new_var.get(self.db).id) {
                    return self.impl_contains_var(&impl_id, var);
                }
                false
            }
        })
    }
}
