use itertools::zip_eq;

use super::canonic::ResultNoErrEx;
use super::{Inference, InferenceError, InferenceResult, InferenceVar};
use crate::corelib::never_ty;
use crate::items::constant::{ConstValue, ConstValueId};
use crate::items::functions::{GenericFunctionId, ImplGenericFunctionId};
use crate::items::imp::ImplId;
use crate::substitution::SemanticRewriter;
use crate::types::peel_snapshots;
use crate::{
    ConcreteFunction, ConcreteImplLongId, ConcreteTraitId, ConcreteTraitLongId, ConcreteTypeId,
    FunctionId, FunctionLongId, GenericArgumentId, TypeId, TypeLongId,
};

/// Functions for conforming semantic objects with each other.
pub trait InferenceConform {
    fn conform_ty(&mut self, ty0: TypeId, ty1: TypeId) -> InferenceResult<TypeId>;
    fn conform_ty_ex(
        &mut self,
        ty0: TypeId,
        ty1: TypeId,
        ty0_is_self: bool,
    ) -> InferenceResult<(TypeId, usize)>;
    fn conform_const(
        &mut self,
        ty0: ConstValueId,
        ty1: ConstValueId,
    ) -> InferenceResult<ConstValueId>;
    fn maybe_peel_snapshots(&mut self, ty0_is_self: bool, ty1: TypeId) -> (usize, TypeLongId);
    fn conform_generic_args(
        &mut self,
        gargs0: &[GenericArgumentId],
        gargs1: &[GenericArgumentId],
    ) -> InferenceResult<Vec<GenericArgumentId>>;
    fn conform_generic_arg(
        &mut self,
        garg0: GenericArgumentId,
        garg1: GenericArgumentId,
    ) -> InferenceResult<GenericArgumentId>;
    fn conform_impl(&mut self, impl0: ImplId, impl1: ImplId) -> InferenceResult<ImplId>;
    fn conform_traits(
        &mut self,
        trt0: ConcreteTraitId,
        trt1: ConcreteTraitId,
    ) -> InferenceResult<ConcreteTraitId>;
    fn conform_generic_function(
        &mut self,
        trt0: GenericFunctionId,
        trt1: GenericFunctionId,
    ) -> InferenceResult<GenericFunctionId>;
    fn ty_contains_var(&mut self, ty: TypeId, var: InferenceVar) -> bool;
    fn generic_args_contain_var(
        &mut self,
        generic_args: &[GenericArgumentId],
        var: InferenceVar,
    ) -> bool;
    fn impl_contains_var(&mut self, impl_id: &ImplId, var: InferenceVar) -> bool;
    fn function_contains_var(&mut self, function_id: FunctionId, var: InferenceVar) -> bool;
}

impl<'db> InferenceConform for Inference<'db> {
    /// Conforms ty0 to ty1. Should be called when ty0 should be coerced to ty1. Not symmetric.
    /// Returns the reduced type for ty0, or an error if the type is no coercible.
    fn conform_ty(&mut self, ty0: TypeId, ty1: TypeId) -> InferenceResult<TypeId> {
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
    ) -> InferenceResult<(TypeId, usize)> {
        let ty0 = self.rewrite(ty0).no_err();
        let ty1 = self.rewrite(ty1).no_err();
        if ty0 == never_ty(self.db) || ty0.is_missing(self.db) {
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
                    return Err(self.set_error(InferenceError::TypeKindMismatch { ty0, ty1 }));
                };
                if concrete0.generic_type(self.db) != concrete1.generic_type(self.db) {
                    return Err(self.set_error(InferenceError::TypeKindMismatch { ty0, ty1 }));
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
                    return Err(self.set_error(InferenceError::TypeKindMismatch { ty0, ty1 }));
                };
                if tys0.len() != tys1.len() {
                    return Err(self.set_error(InferenceError::TypeKindMismatch { ty0, ty1 }));
                }
                let tys = zip_eq(tys0, tys1)
                    .map(|(subty0, subty1)| self.conform_ty(subty0, subty1))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok((self.db.intern_type(TypeLongId::Tuple(tys)), n_snapshots))
            }
            TypeLongId::FixedSizeArray { type_id, size } => {
                let (n_snapshots, long_ty1) = self.maybe_peel_snapshots(ty0_is_self, ty1);
                let TypeLongId::FixedSizeArray { type_id: type_id1, size: size1 } = long_ty1 else {
                    return Err(self.set_error(InferenceError::TypeKindMismatch { ty0, ty1 }));
                };
                let size = self.conform_const(size, size1)?;
                let ty = self.conform_ty(type_id, type_id1)?;
                Ok((
                    self.db.intern_type(TypeLongId::FixedSizeArray { type_id: ty, size }),
                    n_snapshots,
                ))
            }
            TypeLongId::Snapshot(ty0) => {
                let TypeLongId::Snapshot(ty1) = long_ty1 else {
                    return Err(self.set_error(InferenceError::TypeKindMismatch { ty0, ty1 }));
                };
                let (ty, n_snapshots) = self.conform_ty_ex(ty0, ty1, ty0_is_self)?;
                Ok((self.db.intern_type(TypeLongId::Snapshot(ty)), n_snapshots))
            }
            TypeLongId::GenericParameter(_) => {
                Err(self.set_error(InferenceError::TypeKindMismatch { ty0, ty1 }))
            }
            TypeLongId::Var(var) => Ok((self.assign_ty(var, ty1)?, n_snapshots)),
            TypeLongId::Missing(_) => Ok((ty0, n_snapshots)),
            TypeLongId::Coupon(function_id0) => {
                let TypeLongId::Coupon(function_id1) = long_ty1 else {
                    return Err(self.set_error(InferenceError::TypeKindMismatch { ty0, ty1 }));
                };

                let func0 = self.db.lookup_intern_function(function_id0).function;
                let func1 = self.db.lookup_intern_function(function_id1).function;

                let generic_function =
                    self.conform_generic_function(func0.generic_function, func1.generic_function)?;

                if func0.generic_args.len() != func1.generic_args.len() {
                    return Err(self.set_error(InferenceError::TypeKindMismatch { ty0, ty1 }));
                }

                let generic_args =
                    self.conform_generic_args(&func0.generic_args, &func1.generic_args)?;

                Ok((
                    self.db.intern_type(TypeLongId::Coupon(self.db.intern_function(
                        FunctionLongId {
                            function: ConcreteFunction { generic_function, generic_args },
                        },
                    ))),
                    n_snapshots,
                ))
            }
        }
    }

    /// Conforms id0 to id1. Should be called when id0 should be coerced to id1. Not symmetric.
    /// Returns the reduced const for id0, or an error if the const is no coercible.
    fn conform_const(
        &mut self,
        id0: ConstValueId,
        id1: ConstValueId,
    ) -> InferenceResult<ConstValueId> {
        let id0 = self.rewrite(id0).no_err();
        let id1 = self.rewrite(id1).no_err();
        if id0 == id1 {
            return Ok(id0);
        }
        let const_value0 = self.db.lookup_intern_const_value(id0);
        if matches!(const_value0, ConstValue::Missing(_)) {
            return Ok(id1);
        }
        match self.db.lookup_intern_const_value(id1) {
            ConstValue::Missing(_) => return Ok(id1),
            ConstValue::Var(var) => return self.assign_const(var, id0),
            _ => {}
        }
        match const_value0 {
            ConstValue::Var(var) => Ok(self.assign_const(var, id1)?),
            _ => {
                Err(self.set_error(InferenceError::ConstKindMismatch { const0: id0, const1: id1 }))
            }
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
    ) -> InferenceResult<Vec<GenericArgumentId>> {
        zip_eq(gargs0, gargs1)
            .map(|(garg0, garg1)| self.conform_generic_arg(*garg0, *garg1))
            .collect::<Result<Vec<_>, _>>()
    }

    /// Conforms a generics arg. See `conform_ty()`.
    fn conform_generic_arg(
        &mut self,
        garg0: GenericArgumentId,
        garg1: GenericArgumentId,
    ) -> InferenceResult<GenericArgumentId> {
        if garg0 == garg1 {
            return Ok(garg0);
        }
        match garg0 {
            GenericArgumentId::Type(gty0) => {
                let GenericArgumentId::Type(gty1) = garg1 else {
                    return Err(self.set_error(InferenceError::GenericArgMismatch { garg0, garg1 }));
                };
                Ok(GenericArgumentId::Type(self.conform_ty(gty0, gty1)?))
            }
            GenericArgumentId::Constant(gc0) => {
                let GenericArgumentId::Constant(gc1) = garg1 else {
                    return Err(self.set_error(InferenceError::GenericArgMismatch { garg0, garg1 }));
                };

                Ok(GenericArgumentId::Constant(self.conform_const(gc0, gc1)?))
            }
            GenericArgumentId::Impl(impl0) => {
                let GenericArgumentId::Impl(impl1) = garg1 else {
                    return Err(self.set_error(InferenceError::GenericArgMismatch { garg0, garg1 }));
                };
                Ok(GenericArgumentId::Impl(self.conform_impl(impl0, impl1)?))
            }
            GenericArgumentId::NegImpl => match garg1 {
                GenericArgumentId::NegImpl => Ok(GenericArgumentId::NegImpl),
                GenericArgumentId::Constant(_)
                | GenericArgumentId::Type(_)
                | GenericArgumentId::Impl(_) => {
                    Err(self.set_error(InferenceError::GenericArgMismatch { garg0, garg1 }))
                }
            },
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
            let impl_concrete_trait = self
                .db
                .impl_concrete_trait(impl0)
                .map_err(|diag_added| self.set_error(InferenceError::Reported(diag_added)))?;
            self.conform_traits(var.get(self.db).concrete_trait_id, impl_concrete_trait)?;
            let impl_id = self.rewrite(impl0).no_err();
            return self.assign_impl(var, impl_id);
        }
        match impl0 {
            ImplId::ImplVar(var) => {
                let impl_concrete_trait = self
                    .db
                    .impl_concrete_trait(impl1)
                    .map_err(|diag_added| self.set_error(InferenceError::Reported(diag_added)))?;
                self.conform_traits(var.get(self.db).concrete_trait_id, impl_concrete_trait)?;
                let impl_id = self.rewrite(impl1).no_err();
                self.assign_impl(var, impl_id)
            }
            ImplId::Concrete(concrete0) => {
                let ImplId::Concrete(concrete1) = impl1 else {
                    return Err(self.set_error(InferenceError::ImplKindMismatch { impl0, impl1 }));
                };
                let concrete0 = self.db.lookup_intern_concrete_impl(concrete0);
                let concrete1 = self.db.lookup_intern_concrete_impl(concrete1);
                if concrete0.impl_def_id != concrete1.impl_def_id {
                    return Err(self.set_error(InferenceError::ImplKindMismatch { impl0, impl1 }));
                }
                let gargs0 = concrete0.generic_args;
                let gargs1 = concrete1.generic_args;
                let generic_args = self.conform_generic_args(&gargs0, &gargs1)?;
                Ok(ImplId::Concrete(self.db.intern_concrete_impl(ConcreteImplLongId {
                    impl_def_id: concrete0.impl_def_id,
                    generic_args,
                })))
            }
            ImplId::GenericParameter(_) => {
                Err(self.set_error(InferenceError::ImplKindMismatch { impl0, impl1 }))
            }
        }
    }

    /// Conforms generics traits. See `conform_ty()`.
    fn conform_traits(
        &mut self,
        trt0: ConcreteTraitId,
        trt1: ConcreteTraitId,
    ) -> InferenceResult<ConcreteTraitId> {
        let trt0 = self.db.lookup_intern_concrete_trait(trt0);
        let trt1 = self.db.lookup_intern_concrete_trait(trt1);
        if trt0.trait_id != trt1.trait_id {
            return Err(self.set_error(InferenceError::TraitMismatch {
                trt0: trt0.trait_id,
                trt1: trt1.trait_id,
            }));
        }
        let generic_args = self.conform_generic_args(&trt0.generic_args, &trt1.generic_args)?;
        Ok(self
            .db
            .intern_concrete_trait(ConcreteTraitLongId { trait_id: trt0.trait_id, generic_args }))
    }

    fn conform_generic_function(
        &mut self,
        func0: GenericFunctionId,
        func1: GenericFunctionId,
    ) -> InferenceResult<GenericFunctionId> {
        if let (GenericFunctionId::Impl(id0), GenericFunctionId::Impl(id1)) = (func0, func1) {
            if id0.function != id1.function {
                return Err(
                    self.set_error(InferenceError::GenericFunctionMismatch { func0, func1 })
                );
            }
            let function = id0.function;
            let impl_id = self.conform_impl(id0.impl_id, id1.impl_id)?;
            return Ok(GenericFunctionId::Impl(ImplGenericFunctionId { impl_id, function }));
        }

        if func0 != func1 {
            return Err(self.set_error(InferenceError::GenericFunctionMismatch { func0, func1 }));
        }
        Ok(func0)
    }

    /// Checks if a type tree contains a certain [InferenceVar] somewhere. Used to avoid inference
    /// cycles.
    fn ty_contains_var(&mut self, ty: TypeId, var: InferenceVar) -> bool {
        let ty = self.rewrite(ty).no_err();
        self.internal_ty_contains_var(ty, var)
    }

    /// Checks if a slice of generics arguments contain a certain [InferenceVar] somewhere. Used to
    /// avoid inference cycles.
    fn generic_args_contain_var(
        &mut self,
        generic_args: &[GenericArgumentId],
        var: InferenceVar,
    ) -> bool {
        for garg in generic_args {
            if match garg {
                GenericArgumentId::Type(ty) => self.internal_ty_contains_var(*ty, var),
                GenericArgumentId::Constant(_) => false,
                GenericArgumentId::Impl(impl_id) => self.impl_contains_var(impl_id, var),
                GenericArgumentId::NegImpl => false,
            } {
                return true;
            }
        }
        false
    }

    /// Checks if an impl contains a certain [InferenceVar] somewhere. Used to avoid inference
    /// cycles.
    fn impl_contains_var(&mut self, impl_id: &ImplId, var: InferenceVar) -> bool {
        match impl_id {
            ImplId::Concrete(concrete_impl_id) => self.generic_args_contain_var(
                &self.db.lookup_intern_concrete_impl(*concrete_impl_id).generic_args,
                var,
            ),
            ImplId::GenericParameter(_) => false,
            ImplId::ImplVar(new_var) => {
                if InferenceVar::Impl(new_var.get(self.db).id) == var {
                    return true;
                }
                if let Some(impl_id) = self.impl_assignment(new_var.get(self.db).id) {
                    return self.impl_contains_var(&impl_id, var);
                }
                false
            }
        }
    }

    /// Checks if a function contains a certain [InferenceVar] in its generic arguments or in the
    /// generic arguments of the impl containing the function (in case the function is an impl
    /// function).
    ///
    /// Used to avoid inference cycles.
    fn function_contains_var(&mut self, function_id: FunctionId, var: InferenceVar) -> bool {
        let function = function_id.get_concrete(self.db);
        let generic_args = function.generic_args;
        // Look in the generic arguments of the function and in the impl generic arguments.
        self.generic_args_contain_var(&generic_args, var)
            || matches!(function.generic_function,
                GenericFunctionId::Impl(impl_generic_function_id)
                if self.impl_contains_var(&impl_generic_function_id.impl_id, var)
            )
    }
}

impl Inference<'_> {
    /// helper function for ty_contains_var
    /// Assumes ty was already rewritten.
    #[doc(hidden)]
    fn internal_ty_contains_var(&mut self, ty: TypeId, var: InferenceVar) -> bool {
        match self.db.lookup_intern_type(ty) {
            TypeLongId::Concrete(concrete) => {
                let generic_args = concrete.generic_args(self.db);
                self.generic_args_contain_var(&generic_args, var)
            }
            TypeLongId::Tuple(tys) => {
                tys.into_iter().any(|ty| self.internal_ty_contains_var(ty, var))
            }
            TypeLongId::Snapshot(ty) => self.internal_ty_contains_var(ty, var),
            TypeLongId::Var(new_var) => {
                if InferenceVar::Type(new_var.id) == var {
                    return true;
                }
                if let Some(ty) = self.type_assignment.get(&new_var.id) {
                    return self.internal_ty_contains_var(*ty, var);
                }
                false
            }
            TypeLongId::GenericParameter(_) | TypeLongId::Missing(_) => false,
            TypeLongId::Coupon(function_id) => self.function_contains_var(function_id, var),
            TypeLongId::FixedSizeArray { type_id, .. } => {
                self.internal_ty_contains_var(type_id, var)
            }
        }
    }
}
