use cairo_lang_utils::{Intern, LookupIntern};
use itertools::zip_eq;

use super::canonic::ResultNoErrEx;
use super::{ErrorSet, Inference, InferenceError, InferenceResult, InferenceVar};
use crate::corelib::never_ty;
use crate::items::constant::{ConstValue, ConstValueId, ImplConstantId};
use crate::items::functions::{GenericFunctionId, ImplGenericFunctionId};
use crate::items::imp::ImplId;
use crate::substitution::SemanticRewriter;
use crate::types::{peel_snapshots, ImplTypeId};
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
        let long_ty1 = ty1.lookup_intern(self.db);
        match long_ty1 {
            TypeLongId::Var(var) => return Ok((self.assign_ty(var, ty0)?, 0)),
            TypeLongId::Missing(_) => return Ok((ty1, 0)),
            TypeLongId::Snapshot(inner_ty) => {
                if ty0_is_self {
                    if inner_ty == ty0 {
                        return Ok((ty1, 1));
                    }
                    if !matches!(ty0.lookup_intern(self.db), TypeLongId::Snapshot(_)) {
                        if let TypeLongId::Var(var) = inner_ty.lookup_intern(self.db) {
                            return Ok((self.assign_ty(var, ty0)?, 1));
                        }
                    }
                }
            }
            TypeLongId::ImplType(impl_type_id) => {
                if !matches!(impl_type_id.impl_id(), ImplId::GenericParameter(_)) {
                    let ty = self.reduce_impl_ty(impl_type_id)?;
                    return self.conform_ty_ex(ty0, ty, ty0_is_self);
                }
            }
            _ => {}
        }
        let n_snapshots = 0;
        let long_ty0 = ty0.lookup_intern(self.db);

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
                Ok((long_ty.intern(self.db), n_snapshots))
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
                Ok((TypeLongId::Tuple(tys).intern(self.db), n_snapshots))
            }
            TypeLongId::FixedSizeArray { type_id, size } => {
                let (n_snapshots, long_ty1) = self.maybe_peel_snapshots(ty0_is_self, ty1);
                let TypeLongId::FixedSizeArray { type_id: type_id1, size: size1 } = long_ty1 else {
                    return Err(self.set_error(InferenceError::TypeKindMismatch { ty0, ty1 }));
                };
                let size = self.conform_const(size, size1)?;
                let ty = self.conform_ty(type_id, type_id1)?;
                Ok((TypeLongId::FixedSizeArray { type_id: ty, size }.intern(self.db), n_snapshots))
            }
            TypeLongId::Snapshot(ty0) => {
                let TypeLongId::Snapshot(ty1) = long_ty1 else {
                    return Err(self.set_error(InferenceError::TypeKindMismatch { ty0, ty1 }));
                };
                let (ty, n_snapshots) = self.conform_ty_ex(ty0, ty1, ty0_is_self)?;
                Ok((TypeLongId::Snapshot(ty).intern(self.db), n_snapshots))
            }
            TypeLongId::GenericParameter(_) => {
                Err(self.set_error(InferenceError::TypeKindMismatch { ty0, ty1 }))
            }
            TypeLongId::TraitType(_) => {
                // This should never happen as the trait type should be implized when conformed, but
                // don't panic in case of a bug.
                Err(self.set_error(InferenceError::TypeKindMismatch { ty0, ty1 }))
            }
            TypeLongId::Var(var) => Ok((self.assign_ty(var, ty1)?, n_snapshots)),
            TypeLongId::ImplType(impl_type_id) => {
                if !matches!(impl_type_id.impl_id(), ImplId::GenericParameter(_)) {
                    let ty = self.reduce_impl_ty(impl_type_id)?;
                    self.conform_ty_ex(ty, ty1, ty0_is_self)
                } else {
                    Err(self.set_error(InferenceError::TypeKindMismatch { ty0, ty1 }))
                }
            }
            TypeLongId::Missing(_) => Ok((ty0, n_snapshots)),
            TypeLongId::Coupon(function_id0) => {
                let TypeLongId::Coupon(function_id1) = long_ty1 else {
                    return Err(self.set_error(InferenceError::TypeKindMismatch { ty0, ty1 }));
                };

                let func0 = function_id0.lookup_intern(self.db).function;
                let func1 = function_id1.lookup_intern(self.db).function;

                let generic_function =
                    self.conform_generic_function(func0.generic_function, func1.generic_function)?;

                if func0.generic_args.len() != func1.generic_args.len() {
                    return Err(self.set_error(InferenceError::TypeKindMismatch { ty0, ty1 }));
                }

                let generic_args =
                    self.conform_generic_args(&func0.generic_args, &func1.generic_args)?;

                Ok((
                    TypeLongId::Coupon(
                        FunctionLongId {
                            function: ConcreteFunction { generic_function, generic_args },
                        }
                        .intern(self.db),
                    )
                    .intern(self.db),
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
        self.conform_ty(id0.ty(self.db).unwrap(), id1.ty(self.db).unwrap())?;
        if id0 == id1 {
            return Ok(id0);
        }
        let const_value0 = id0.lookup_intern(self.db);
        if matches!(const_value0, ConstValue::Missing(_)) {
            return Ok(id1);
        }
        match id1.lookup_intern(self.db) {
            ConstValue::Missing(_) => return Ok(id1),
            ConstValue::Var(var, _) => return self.assign_const(var, id0),
            ConstValue::ImplConstant(impl_const_id) => {
                if !impl_const_id.impl_id().is_var_free(self.db) {
                    let constant = self.reduce_impl_constant(impl_const_id)?;
                    return self.conform_const(id0, constant);
                }
            }
            _ => {}
        }
        match const_value0 {
            ConstValue::Var(var, _) => Ok(self.assign_const(var, id1)?),
            ConstValue::ImplConstant(impl_const_id) => {
                if !impl_const_id.impl_id().is_var_free(self.db) {
                    let constant = self.reduce_impl_constant(impl_const_id)?;
                    self.conform_const(constant, id1)
                } else {
                    Err(self
                        .set_error(InferenceError::ConstKindMismatch { const0: id0, const1: id1 }))
                }
            }
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
            (0, ty1.lookup_intern(self.db))
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
            self.conform_traits(var.lookup_intern(self.db).concrete_trait_id, impl_concrete_trait)?;
            let impl_id = self.rewrite(impl0).no_err();
            return self.assign_impl(var, impl_id);
        }
        match impl0 {
            ImplId::ImplVar(var) => {
                let impl_concrete_trait = self
                    .db
                    .impl_concrete_trait(impl1)
                    .map_err(|diag_added| self.set_error(InferenceError::Reported(diag_added)))?;
                self.conform_traits(
                    var.lookup_intern(self.db).concrete_trait_id,
                    impl_concrete_trait,
                )?;
                let impl_id = self.rewrite(impl1).no_err();
                self.assign_impl(var, impl_id)
            }
            ImplId::Concrete(concrete0) => {
                let ImplId::Concrete(concrete1) = impl1 else {
                    return Err(self.set_error(InferenceError::ImplKindMismatch { impl0, impl1 }));
                };
                let concrete0 = concrete0.lookup_intern(self.db);
                let concrete1 = concrete1.lookup_intern(self.db);
                if concrete0.impl_def_id != concrete1.impl_def_id {
                    return Err(self.set_error(InferenceError::ImplKindMismatch { impl0, impl1 }));
                }
                let gargs0 = concrete0.generic_args;
                let gargs1 = concrete1.generic_args;
                let generic_args = self.conform_generic_args(&gargs0, &gargs1)?;
                Ok(ImplId::Concrete(
                    ConcreteImplLongId { impl_def_id: concrete0.impl_def_id, generic_args }
                        .intern(self.db),
                ))
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
        let trt0 = trt0.lookup_intern(self.db);
        let trt1 = trt1.lookup_intern(self.db);
        if trt0.trait_id != trt1.trait_id {
            return Err(self.set_error(InferenceError::TraitMismatch {
                trt0: trt0.trait_id,
                trt1: trt1.trait_id,
            }));
        }
        let generic_args = self.conform_generic_args(&trt0.generic_args, &trt1.generic_args)?;
        Ok(ConcreteTraitLongId { trait_id: trt0.trait_id, generic_args }.intern(self.db))
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
                &concrete_impl_id.lookup_intern(self.db).generic_args,
                var,
            ),
            ImplId::GenericParameter(_) => false,
            ImplId::ImplVar(new_var) => {
                let new_var_long_id = new_var.lookup_intern(self.db);
                let new_var_local_id = new_var_long_id.id;
                if InferenceVar::Impl(new_var_local_id) == var {
                    return true;
                }
                if let Some(impl_id) = self.impl_assignment(new_var_local_id) {
                    return self.impl_contains_var(&impl_id, var);
                }
                self.generic_args_contain_var(
                    &new_var_long_id.concrete_trait_id.generic_args(self.db),
                    var,
                )
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
    /// Reduces an impl type to a concrete type.
    pub fn reduce_impl_ty(&mut self, impl_type_id: ImplTypeId) -> InferenceResult<TypeId> {
        let reduced_impl = self.reduce_impl(impl_type_id.impl_id())?;
        if let Ok(Some(ty)) = self.db.impl_type_concrete_implized(ImplTypeId::new(
            reduced_impl,
            impl_type_id.ty(),
            self.db,
        )) {
            Ok(ty)
        } else {
            Err(self.set_error(
                reduced_impl
                    .concrete_trait(self.db)
                    .map(InferenceError::NoImplsFound)
                    .unwrap_or_else(InferenceError::Reported),
            ))
        }
    }

    /// Reduces an impl constant to a concrete const.
    fn reduce_impl_constant(
        &mut self,
        impl_const_id: ImplConstantId,
    ) -> InferenceResult<ConstValueId> {
        let reduced_impl = self.reduce_impl(impl_const_id.impl_id())?;

        if let Ok(constant) = self.db.impl_constant_concrete_implized_value(ImplConstantId::new(
            reduced_impl,
            impl_const_id.trait_constant_id(),
            self.db,
        )) {
            Ok(constant)
        } else {
            Err(self.set_error(
                reduced_impl
                    .concrete_trait(self.db)
                    .map(InferenceError::NoImplsFound)
                    .unwrap_or_else(InferenceError::Reported),
            ))
        }
    }
    /// Reduces an impl var to a concrete impl.
    fn reduce_impl(&mut self, impl_id: ImplId) -> InferenceResult<ImplId> {
        Ok(if let ImplId::ImplVar(var) = impl_id {
            println!("reduce_impl");
            println!("var: {:?}", var.lookup_intern(self.db));
            self.solve_single_pending(var.id(self.db))?;
            self.rewrite(impl_id).no_err()
        } else {
            impl_id
        })
    }

    /// Conforms a type to a type. Returning the reduced types on failure.
    /// Useful for immediately reporting a diagnostic based on the compared types.
    pub fn conform_ty_for_diag(
        &mut self,
        ty0: TypeId,
        ty1: TypeId,
    ) -> Result<(), (ErrorSet, TypeId, TypeId)> {
        match self.conform_ty(ty0, ty1) {
            Ok(_ty) => Ok(()),
            Err(err) => Err((err, self.rewrite(ty0).no_err(), self.rewrite(ty1).no_err())),
        }
    }

    /// helper function for ty_contains_var
    /// Assumes ty was already rewritten.
    #[doc(hidden)]
    fn internal_ty_contains_var(&mut self, ty: TypeId, var: InferenceVar) -> bool {
        match ty.lookup_intern(self.db) {
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
            TypeLongId::ImplType(id) => self.impl_contains_var(&id.impl_id(), var),
            TypeLongId::TraitType(_) | TypeLongId::GenericParameter(_) | TypeLongId::Missing(_) => {
                false
            }
            TypeLongId::Coupon(function_id) => self.function_contains_var(function_id, var),
            TypeLongId::FixedSizeArray { type_id, .. } => {
                self.internal_ty_contains_var(type_id, var)
            }
        }
    }
}
