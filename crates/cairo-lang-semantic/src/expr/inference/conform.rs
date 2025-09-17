use std::hash::Hash;

use cairo_lang_defs::ids::{LanguageElementId, TraitConstantId, TraitTypeId};
use cairo_lang_diagnostics::Maybe;
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_utils::Intern;
use cairo_lang_utils::ordered_hash_map::{Entry, OrderedHashMap};
use itertools::zip_eq;

use super::canonic::{NoError, ResultNoErrEx};
use super::{
    ErrorSet, ImplVarId, ImplVarTraitItemMappings, Inference, InferenceError, InferenceResult,
    InferenceVar, LocalTypeVarId, TypeVar,
};
use crate::corelib::never_ty;
use crate::diagnostic::{SemanticDiagnosticKind, SemanticDiagnostics, SemanticDiagnosticsBuilder};
use crate::items::constant::{ConstValue, ConstValueId, ImplConstantId};
use crate::items::functions::{GenericFunctionId, ImplGenericFunctionId};
use crate::items::imp::{
    ImplId, ImplImplId, ImplLongId, ImplLookupContext, ImplSemantic, NegativeImplId,
    NegativeImplLongId,
};
use crate::items::trt::{ConcreteTraitImplId, TraitSemantic};
use crate::substitution::SemanticRewriter;
use crate::types::{ClosureTypeLongId, ImplTypeId, peel_snapshots};
use crate::{
    ConcreteFunction, ConcreteImplLongId, ConcreteTraitId, ConcreteTraitLongId, ConcreteTypeId,
    FunctionId, FunctionLongId, GenericArgumentId, TypeId, TypeLongId,
};

/// Functions for conforming semantic objects with each other.
pub trait InferenceConform<'db> {
    fn conform_ty(&mut self, ty0: TypeId<'db>, ty1: TypeId<'db>) -> InferenceResult<TypeId<'db>>;
    fn conform_ty_ex(
        &mut self,
        ty0: TypeId<'db>,
        ty1: TypeId<'db>,
        ty0_is_self: bool,
    ) -> InferenceResult<(TypeId<'db>, usize)>;
    fn conform_const(
        &mut self,
        ty0: ConstValueId<'db>,
        ty1: ConstValueId<'db>,
    ) -> InferenceResult<ConstValueId<'db>>;
    fn maybe_peel_snapshots(
        &mut self,
        ty0_is_self: bool,
        ty1: TypeId<'db>,
    ) -> (usize, TypeLongId<'db>);
    fn conform_generic_args(
        &mut self,
        gargs0: &[GenericArgumentId<'db>],
        gargs1: &[GenericArgumentId<'db>],
    ) -> InferenceResult<Vec<GenericArgumentId<'db>>>;
    fn conform_generic_arg(
        &mut self,
        garg0: GenericArgumentId<'db>,
        garg1: GenericArgumentId<'db>,
    ) -> InferenceResult<GenericArgumentId<'db>>;
    fn conform_impl(
        &mut self,
        impl0: ImplId<'db>,
        impl1: ImplId<'db>,
    ) -> InferenceResult<ImplId<'db>>;
    fn conform_neg_impl(
        &mut self,
        neg_impl0: NegativeImplId<'db>,
        neg_impl1: NegativeImplId<'db>,
    ) -> InferenceResult<NegativeImplId<'db>>;
    fn conform_traits(
        &mut self,
        trt0: ConcreteTraitId<'db>,
        trt1: ConcreteTraitId<'db>,
    ) -> InferenceResult<ConcreteTraitId<'db>>;
    fn conform_generic_function(
        &mut self,
        trt0: GenericFunctionId<'db>,
        trt1: GenericFunctionId<'db>,
    ) -> InferenceResult<GenericFunctionId<'db>>;
    fn ty_contains_var(&mut self, ty: TypeId<'db>, var: InferenceVar) -> bool;
    fn generic_args_contain_var(
        &mut self,
        generic_args: &[GenericArgumentId<'db>],
        var: InferenceVar,
    ) -> bool;
    fn impl_contains_var(&mut self, impl_id: ImplId<'db>, var: InferenceVar) -> bool;
    fn negative_impl_contains_var(
        &mut self,
        neg_impl_id: NegativeImplId<'db>,
        var: InferenceVar,
    ) -> bool;
    fn function_contains_var(&mut self, function_id: FunctionId<'db>, var: InferenceVar) -> bool;
}

impl<'db> InferenceConform<'db> for Inference<'db, '_> {
    /// Conforms ty0 to ty1. Should be called when ty0 should be coerced to ty1. Not symmetric.
    /// Returns the reduced type for ty0, or an error if the type is no coercible.
    fn conform_ty(&mut self, ty0: TypeId<'db>, ty1: TypeId<'db>) -> InferenceResult<TypeId<'db>> {
        Ok(self.conform_ty_ex(ty0, ty1, false)?.0)
    }

    /// Same as conform_ty but supports adding snapshots to ty0 if `ty0_is_self` is true.
    /// Returns the reduced type for ty0 and the number of snapshots that needs to be added
    /// for the types to conform.
    fn conform_ty_ex(
        &mut self,
        ty0: TypeId<'db>,
        ty1: TypeId<'db>,
        ty0_is_self: bool,
    ) -> InferenceResult<(TypeId<'db>, usize)> {
        let ty0 = self.rewrite(ty0).no_err();
        let ty1 = self.rewrite(ty1).no_err();
        if ty0 == never_ty(self.db) || ty0.is_missing(self.db) {
            return Ok((ty1, 0));
        }
        if ty0 == ty1 {
            return Ok((ty0, 0));
        }
        let long_ty1 = ty1.long(self.db);
        match long_ty1 {
            TypeLongId::Var(var) => return Ok((self.assign_ty(*var, ty0)?, 0)),
            TypeLongId::Missing(_) => return Ok((ty1, 0)),
            TypeLongId::Snapshot(inner_ty) => {
                if ty0_is_self {
                    if *inner_ty == ty0 {
                        return Ok((ty1, 1));
                    }
                    if !matches!(ty0.long(self.db), TypeLongId::Snapshot(_))
                        && let TypeLongId::Var(var) = inner_ty.long(self.db)
                    {
                        return Ok((self.assign_ty(*var, ty0)?, 1));
                    }
                }
            }
            TypeLongId::ImplType(impl_type) => {
                if let Some(ty) = self.impl_type_bounds.get(&(*impl_type).into()) {
                    return self.conform_ty_ex(ty0, *ty, ty0_is_self);
                }
            }
            _ => {}
        }
        let long_ty0 = ty0.long(self.db);

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
                    .map(|(subty0, subty1)| self.conform_ty(*subty0, subty1))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok((TypeLongId::Tuple(tys).intern(self.db), n_snapshots))
            }
            TypeLongId::Closure(closure0) => {
                let (n_snapshots, long_ty1) = self.maybe_peel_snapshots(ty0_is_self, ty1);
                let TypeLongId::Closure(closure1) = long_ty1 else {
                    return Err(self.set_error(InferenceError::TypeKindMismatch { ty0, ty1 }));
                };
                if closure0.wrapper_location != closure1.wrapper_location {
                    return Err(self.set_error(InferenceError::TypeKindMismatch { ty0, ty1 }));
                }
                let param_tys = zip_eq(closure0.param_tys.clone(), closure1.param_tys)
                    .map(|(subty0, subty1)| self.conform_ty(subty0, subty1))
                    .collect::<Result<Vec<_>, _>>()?;
                let captured_types =
                    zip_eq(closure0.captured_types.clone(), closure1.captured_types)
                        .map(|(subty0, subty1)| self.conform_ty(subty0, subty1))
                        .collect::<Result<Vec<_>, _>>()?;
                let ret_ty = self.conform_ty(closure0.ret_ty, closure1.ret_ty)?;
                Ok((
                    TypeLongId::Closure(ClosureTypeLongId {
                        param_tys,
                        ret_ty,
                        captured_types,
                        wrapper_location: closure0.wrapper_location,
                        parent_function: closure0.parent_function,
                    })
                    .intern(self.db),
                    n_snapshots,
                ))
            }
            TypeLongId::FixedSizeArray { type_id, size } => {
                let (n_snapshots, long_ty1) = self.maybe_peel_snapshots(ty0_is_self, ty1);
                let TypeLongId::FixedSizeArray { type_id: type_id1, size: size1 } = long_ty1 else {
                    return Err(self.set_error(InferenceError::TypeKindMismatch { ty0, ty1 }));
                };
                let size = self.conform_const(*size, size1)?;
                let ty = self.conform_ty(*type_id, type_id1)?;
                Ok((TypeLongId::FixedSizeArray { type_id: ty, size }.intern(self.db), n_snapshots))
            }
            TypeLongId::Snapshot(inner_ty0) => {
                let TypeLongId::Snapshot(inner_ty1) = long_ty1 else {
                    return Err(self.set_error(InferenceError::TypeKindMismatch { ty0, ty1 }));
                };
                let (ty, n_snapshots) = self.conform_ty_ex(*inner_ty0, *inner_ty1, ty0_is_self)?;
                Ok((TypeLongId::Snapshot(ty).intern(self.db), n_snapshots))
            }
            TypeLongId::GenericParameter(_) => {
                Err(self.set_error(InferenceError::TypeKindMismatch { ty0, ty1 }))
            }
            TypeLongId::Var(var) => Ok((self.assign_ty(*var, ty1)?, 0)),
            TypeLongId::ImplType(impl_type) => {
                if let Some(ty) = self.impl_type_bounds.get(&(*impl_type).into()) {
                    return self.conform_ty_ex(*ty, ty1, ty0_is_self);
                }
                Err(self.set_error(InferenceError::TypeKindMismatch { ty0, ty1 }))
            }
            TypeLongId::Missing(_) => Ok((ty0, 0)),
            TypeLongId::Coupon(function_id0) => {
                let TypeLongId::Coupon(function_id1) = long_ty1 else {
                    return Err(self.set_error(InferenceError::TypeKindMismatch { ty0, ty1 }));
                };

                let func0 = function_id0.long(self.db).function.clone();
                let func1 = function_id1.long(self.db).function.clone();

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
                    0,
                ))
            }
        }
    }

    /// Conforms id0 to id1. Should be called when id0 should be coerced to id1. Not symmetric.
    /// Returns the reduced const for id0, or an error if the const is no coercible.
    fn conform_const(
        &mut self,
        id0: ConstValueId<'db>,
        id1: ConstValueId<'db>,
    ) -> InferenceResult<ConstValueId<'db>> {
        let id0 = self.rewrite(id0).no_err();
        let id1 = self.rewrite(id1).no_err();
        self.conform_ty(id0.ty(self.db).unwrap(), id1.ty(self.db).unwrap())?;
        if id0 == id1 {
            return Ok(id0);
        }
        let const_value0 = id0.long(self.db);
        if matches!(const_value0, ConstValue::Missing(_)) {
            return Ok(id1);
        }
        match id1.long(self.db) {
            ConstValue::Missing(_) => return Ok(id1),
            ConstValue::Var(var, _) => return self.assign_const(*var, id0),
            _ => {}
        }
        match const_value0 {
            ConstValue::Var(var, _) => Ok(self.assign_const(*var, id1)?),
            ConstValue::ImplConstant(_) => {
                Err(self.set_error(InferenceError::ConstKindMismatch { const0: id0, const1: id1 }))
            }
            _ => {
                Err(self.set_error(InferenceError::ConstKindMismatch { const0: id0, const1: id1 }))
            }
        }
    }

    // Conditionally peels snapshots.
    fn maybe_peel_snapshots(
        &mut self,
        ty0_is_self: bool,
        ty1: TypeId<'db>,
    ) -> (usize, TypeLongId<'db>) {
        let (n_snapshots, long_ty1) =
            if ty0_is_self { peel_snapshots(self.db, ty1) } else { (0, ty1.long(self.db).clone()) };
        (n_snapshots, long_ty1)
    }

    /// Conforms generic args. See `conform_ty()`.
    fn conform_generic_args(
        &mut self,
        gargs0: &[GenericArgumentId<'db>],
        gargs1: &[GenericArgumentId<'db>],
    ) -> InferenceResult<Vec<GenericArgumentId<'db>>> {
        zip_eq(gargs0, gargs1)
            .map(|(garg0, garg1)| self.conform_generic_arg(*garg0, *garg1))
            .collect::<Result<Vec<_>, _>>()
    }

    /// Conforms a generic arg. See `conform_ty()`.
    fn conform_generic_arg(
        &mut self,
        garg0: GenericArgumentId<'db>,
        garg1: GenericArgumentId<'db>,
    ) -> InferenceResult<GenericArgumentId<'db>> {
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
            GenericArgumentId::NegImpl(neg_impl0) => {
                let GenericArgumentId::NegImpl(neg_impl1) = garg1 else {
                    return Err(self.set_error(InferenceError::GenericArgMismatch { garg0, garg1 }));
                };
                Ok(GenericArgumentId::NegImpl(self.conform_neg_impl(neg_impl0, neg_impl1)?))
            }
        }
    }

    /// Conforms an impl. See `conform_ty()`.
    fn conform_impl(
        &mut self,
        impl0: ImplId<'db>,
        impl1: ImplId<'db>,
    ) -> InferenceResult<ImplId<'db>> {
        let impl0 = self.rewrite(impl0).no_err();
        let impl1 = self.rewrite(impl1).no_err();
        let long_impl1 = impl1.long(self.db);
        if impl0 == impl1 {
            return Ok(impl0);
        }
        if let ImplLongId::ImplVar(var) = long_impl1 {
            let impl_concrete_trait = self
                .db
                .impl_concrete_trait(impl0)
                .map_err(|diag_added| self.set_error(InferenceError::Reported(diag_added)))?;
            self.conform_traits(var.long(self.db).concrete_trait_id, impl_concrete_trait)?;
            let impl_id = self.rewrite(impl0).no_err();
            return self.assign_impl(*var, impl_id);
        }
        match impl0.long(self.db) {
            ImplLongId::ImplVar(var) => {
                let impl_concrete_trait = self
                    .db
                    .impl_concrete_trait(impl1)
                    .map_err(|diag_added| self.set_error(InferenceError::Reported(diag_added)))?;
                self.conform_traits(var.long(self.db).concrete_trait_id, impl_concrete_trait)?;
                let impl_id = self.rewrite(impl1).no_err();
                self.assign_impl(*var, impl_id)
            }
            ImplLongId::Concrete(concrete0) => {
                let ImplLongId::Concrete(concrete1) = long_impl1 else {
                    return Err(self.set_error(InferenceError::ImplKindMismatch { impl0, impl1 }));
                };
                let concrete0 = concrete0.long(self.db);
                let concrete1 = concrete1.long(self.db);
                if concrete0.impl_def_id != concrete1.impl_def_id {
                    return Err(self.set_error(InferenceError::ImplKindMismatch { impl0, impl1 }));
                }
                let gargs0 = concrete0.generic_args.clone();
                let gargs1 = concrete1.generic_args.clone();
                let generic_args = self.conform_generic_args(&gargs0, &gargs1)?;
                Ok(ImplLongId::Concrete(
                    ConcreteImplLongId { impl_def_id: concrete0.impl_def_id, generic_args }
                        .intern(self.db),
                )
                .intern(self.db))
            }
            ImplLongId::GenericParameter(_)
            | ImplLongId::ImplImpl(_)
            | ImplLongId::SelfImpl(_)
            | ImplLongId::GeneratedImpl(_) => {
                Err(self.set_error(InferenceError::ImplKindMismatch { impl0, impl1 }))
            }
        }
    }

    fn conform_neg_impl(
        &mut self,
        neg_impl0: NegativeImplId<'db>,
        neg_impl1: NegativeImplId<'db>,
    ) -> InferenceResult<NegativeImplId<'db>> {
        let neg_impl0 = self.rewrite(neg_impl0).no_err();
        let neg_impl1 = self.rewrite(neg_impl1).no_err();
        let long_neg_impl1 = neg_impl1.long(self.db);
        if neg_impl0 == neg_impl1 {
            return Ok(neg_impl0);
        }
        if let NegativeImplLongId::NegativeImplVar(var) = long_neg_impl1 {
            let impl_concrete_trait = neg_impl0
                .concrete_trait(self.db)
                .map_err(|diag_added| self.set_error(InferenceError::Reported(diag_added)))?;
            self.conform_traits(var.long(self.db).concrete_trait_id, impl_concrete_trait)?;
            let neg_impl_id = self.rewrite(neg_impl0).no_err();
            return self.assign_neg_impl(*var, neg_impl_id);
        }
        if let NegativeImplLongId::NegativeImplVar(var) = neg_impl0.long(self.db) {
            let impl_concrete_trait = neg_impl1
                .concrete_trait(self.db)
                .map_err(|diag_added| self.set_error(InferenceError::Reported(diag_added)))?;
            self.conform_traits(var.long(self.db).concrete_trait_id, impl_concrete_trait)?;
            let neg_impl_id = self.rewrite(neg_impl1).no_err();
            return self.assign_neg_impl(*var, neg_impl_id);
        }
        // we do not care about multiple negative impls, so we do not check they are of the same
        // variant, we just conform the traits.
        let trt0 = neg_impl0
            .concrete_trait(self.db)
            .map_err(|diag_added| self.set_error(InferenceError::Reported(diag_added)))?;
        let trt1 = neg_impl1
            .concrete_trait(self.db)
            .map_err(|diag_added| self.set_error(InferenceError::Reported(diag_added)))?;
        self.conform_traits(trt0, trt1)?;
        Ok(self.rewrite(neg_impl1).no_err())
    }

    /// Conforms generic traits. See `conform_ty()`.
    fn conform_traits(
        &mut self,
        trt0: ConcreteTraitId<'db>,
        trt1: ConcreteTraitId<'db>,
    ) -> InferenceResult<ConcreteTraitId<'db>> {
        let trt0 = trt0.long(self.db);
        let trt1 = trt1.long(self.db);
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
        func0: GenericFunctionId<'db>,
        func1: GenericFunctionId<'db>,
    ) -> InferenceResult<GenericFunctionId<'db>> {
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
    fn ty_contains_var(&mut self, ty: TypeId<'db>, var: InferenceVar) -> bool {
        let ty = self.rewrite(ty).no_err();
        self.internal_ty_contains_var(ty, var)
    }

    /// Checks if a slice of generic arguments contain a certain [InferenceVar] somewhere. Used to
    /// avoid inference cycles.
    fn generic_args_contain_var(
        &mut self,
        generic_args: &[GenericArgumentId<'db>],
        var: InferenceVar,
    ) -> bool {
        for garg in generic_args {
            if match *garg {
                GenericArgumentId::Type(ty) => self.internal_ty_contains_var(ty, var),
                GenericArgumentId::Constant(_) => false,
                GenericArgumentId::Impl(impl_id) => self.impl_contains_var(impl_id, var),
                GenericArgumentId::NegImpl(neg_impl_id) => {
                    self.negative_impl_contains_var(neg_impl_id, var)
                }
            } {
                return true;
            }
        }
        false
    }

    /// Checks if an impl contains a certain [InferenceVar] somewhere. Used to avoid inference
    /// cycles.
    fn impl_contains_var(&mut self, impl_id: ImplId<'db>, var: InferenceVar) -> bool {
        match impl_id.long(self.db) {
            ImplLongId::Concrete(concrete_impl_id) => {
                self.generic_args_contain_var(&concrete_impl_id.long(self.db).generic_args, var)
            }
            ImplLongId::SelfImpl(concrete_trait_id) => {
                self.generic_args_contain_var(concrete_trait_id.generic_args(self.db), var)
            }
            ImplLongId::GenericParameter(_) => false,
            ImplLongId::ImplVar(new_var) => {
                let new_var_long_id = new_var.long(self.db);
                let new_var_local_id = new_var_long_id.id;
                if InferenceVar::Impl(new_var_local_id) == var {
                    return true;
                }
                if let Some(impl_id) = self.impl_assignment(new_var_local_id) {
                    return self.impl_contains_var(impl_id, var);
                }
                self.generic_args_contain_var(
                    new_var_long_id.concrete_trait_id.generic_args(self.db),
                    var,
                )
            }
            ImplLongId::ImplImpl(impl_impl) => self.impl_contains_var(impl_impl.impl_id(), var),
            ImplLongId::GeneratedImpl(generated_impl) => self.generic_args_contain_var(
                generated_impl.concrete_trait(self.db).generic_args(self.db),
                var,
            ),
        }
    }

    /// Checks if a negative impl contains a certain [InferenceVar] somewhere. Used to avoid
    /// inference cycles.
    fn negative_impl_contains_var(
        &mut self,
        neg_impl_id: NegativeImplId<'db>,
        var: InferenceVar,
    ) -> bool {
        match neg_impl_id.long(self.db) {
            NegativeImplLongId::Solved(concrete_trait_id) => {
                self.generic_args_contain_var(&concrete_trait_id.long(self.db).generic_args, var)
            }
            NegativeImplLongId::GenericParameter(_) => false,
            NegativeImplLongId::NegativeImplVar(new_var) => {
                let new_var_long_id = new_var.long(self.db);
                let new_var_local_id = new_var_long_id.id;
                if InferenceVar::NegativeImpl(new_var_local_id) == var {
                    return true;
                }
                if let Some(neg_impl_id) = self.negative_impl_assignment(new_var_local_id) {
                    return self.negative_impl_contains_var(neg_impl_id, var);
                }
                self.generic_args_contain_var(
                    new_var_long_id.concrete_trait_id.generic_args(self.db),
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
    fn function_contains_var(&mut self, function_id: FunctionId<'db>, var: InferenceVar) -> bool {
        let function = function_id.get_concrete(self.db);
        let generic_args = function.generic_args;
        // Look in the generic arguments of the function and in the impl generic arguments.
        self.generic_args_contain_var(&generic_args, var)
            || matches!(function.generic_function,
                GenericFunctionId::Impl(impl_generic_function_id)
                if self.impl_contains_var(impl_generic_function_id.impl_id, var)
            )
    }
}

impl<'db> Inference<'db, '_> {
    /// Reduces an impl type to a concrete type.
    pub fn reduce_impl_ty(
        &mut self,
        impl_type_id: ImplTypeId<'db>,
    ) -> InferenceResult<TypeId<'db>> {
        let impl_id = impl_type_id.impl_id();
        let trait_ty = impl_type_id.ty();
        if let ImplLongId::ImplVar(var) = impl_id.long(self.db) {
            Ok(self.rewritten_impl_type(*var, trait_ty))
        } else if let Ok(ty) =
            self.db.impl_type_concrete_implized(ImplTypeId::new(impl_id, trait_ty, self.db))
        {
            Ok(ty)
        } else {
            Err(self.set_impl_reduction_error(impl_id))
        }
    }

    /// Reduces an impl constant to a concrete const.
    pub fn reduce_impl_constant(
        &mut self,
        impl_const_id: ImplConstantId<'db>,
    ) -> InferenceResult<ConstValueId<'db>> {
        let impl_id = impl_const_id.impl_id();
        let trait_constant = impl_const_id.trait_constant_id();
        if let ImplLongId::ImplVar(var) = impl_id.long(self.db) {
            Ok(self.rewritten_impl_constant(*var, trait_constant))
        } else if let Ok(constant) = self.db.impl_constant_concrete_implized_value(
            ImplConstantId::new(impl_id, trait_constant, self.db),
        ) {
            Ok(constant)
        } else {
            Err(self.set_impl_reduction_error(impl_id))
        }
    }

    /// Reduces an impl impl to a concrete impl.
    pub fn reduce_impl_impl(
        &mut self,
        impl_impl_id: ImplImplId<'db>,
    ) -> InferenceResult<ImplId<'db>> {
        let impl_id = impl_impl_id.impl_id();
        let concrete_trait_impl = impl_impl_id
            .concrete_trait_impl_id(self.db)
            .map_err(|diag_added| self.set_error(InferenceError::Reported(diag_added)))?;

        if let ImplLongId::ImplVar(var) = impl_id.long(self.db) {
            Ok(self.rewritten_impl_impl(*var, concrete_trait_impl))
        } else if let Ok(imp) = self.db.impl_impl_concrete_implized(ImplImplId::new(
            impl_id,
            impl_impl_id.trait_impl_id(),
            self.db,
        )) {
            Ok(imp)
        } else {
            Err(self.set_impl_reduction_error(impl_id))
        }
    }

    /// Returns the type of an impl var's type item.
    /// The type may be a variable itself, but it may previously exist, so may be more specific due
    /// to rewriting.
    pub fn rewritten_impl_type(
        &mut self,
        id: ImplVarId<'db>,
        trait_type_id: TraitTypeId<'db>,
    ) -> TypeId<'db> {
        self.rewritten_impl_item(
            id,
            trait_type_id,
            |m| &mut m.types,
            |inference, stable_ptr| inference.new_type_var(stable_ptr),
        )
    }

    /// Returns the constant value of an impl var's constant item.
    /// The constant may be a variable itself, but it may previously exist, so may be more specific
    /// due to rewriting.
    pub fn rewritten_impl_constant(
        &mut self,
        id: ImplVarId<'db>,
        trait_constant: TraitConstantId<'db>,
    ) -> ConstValueId<'db> {
        self.rewritten_impl_item(
            id,
            trait_constant,
            |m| &mut m.constants,
            |inference, stable_ptr| {
                inference.new_const_var(
                    stable_ptr,
                    inference.db.trait_constant_type(trait_constant).unwrap(),
                )
            },
        )
    }

    /// Returns the inner_impl value of an impl var's impl item.
    /// The inner_impl may be a variable itself, but it may previously exist, so may be more
    /// specific due to rewriting.
    pub fn rewritten_impl_impl(
        &mut self,
        id: ImplVarId<'db>,
        concrete_trait_impl: ConcreteTraitImplId<'db>,
    ) -> ImplId<'db> {
        let trait_crate = concrete_trait_impl
            .trait_impl(self.db)
            .trait_id(self.db)
            .parent_module(self.db)
            .owning_crate(self.db);
        self.rewritten_impl_item(
            id,
            concrete_trait_impl.trait_impl(self.db),
            |m| &mut m.impls,
            |inference, stable_ptr| {
                inference.new_impl_var(
                    inference.db.concrete_trait_impl_concrete_trait(concrete_trait_impl).unwrap(),
                    stable_ptr,
                    ImplLookupContext::new_from_crate(trait_crate).intern(self.db),
                )
            },
        )
    }

    /// Helper function for getting an impl vars item ids.
    /// These ids are likely to be variables, but may have more specific information due to
    /// rewriting.
    fn rewritten_impl_item<K: Hash + PartialEq + Eq, V: Copy>(
        &mut self,
        id: ImplVarId<'db>,
        key: K,
        get_map: impl for<'a> Fn(&'a mut ImplVarTraitItemMappings<'db>) -> &'a mut OrderedHashMap<K, V>,
        new_var: impl FnOnce(&mut Self, Option<SyntaxStablePtrId<'db>>) -> V,
    ) -> V
    where
        Self: SemanticRewriter<V, NoError>,
    {
        let var_id = id.id(self.db);
        if let Some(value) = self
            .data
            .impl_vars_trait_item_mappings
            .get_mut(&var_id)
            .and_then(|mappings| get_map(mappings).get(&key))
        {
            // Copy the value to allow usage of `self`.
            let value = *value;
            // If the value already exists, rewrite it before returning.
            self.rewrite(value).no_err()
        } else {
            let value =
                new_var(self, self.data.stable_ptrs.get(&InferenceVar::Impl(var_id)).cloned());
            get_map(self.data.impl_vars_trait_item_mappings.entry(var_id).or_default())
                .insert(key, value);
            value
        }
    }

    /// Sets an error for an impl reduction failure.
    fn set_impl_reduction_error(&mut self, impl_id: ImplId<'db>) -> ErrorSet {
        self.set_error(
            impl_id
                .concrete_trait(self.db)
                .map(InferenceError::NoImplsFound)
                .unwrap_or_else(InferenceError::Reported),
        )
    }

    /// Conforms a type to a type. Returning the reduced types on failure.
    /// Useful for immediately reporting a diagnostic based on the compared types.
    pub fn conform_ty_for_diag(
        &mut self,
        ty0: TypeId<'db>,
        ty1: TypeId<'db>,
        diagnostics: &mut SemanticDiagnostics<'db>,
        diag_stable_ptr: impl FnOnce() -> SyntaxStablePtrId<'db>,
        diag_kind: impl FnOnce(TypeId<'db>, TypeId<'db>) -> SemanticDiagnosticKind<'db>,
    ) -> Maybe<()> {
        match self.conform_ty(ty0, ty1) {
            Ok(_ty) => Ok(()),
            Err(err) => {
                let ty0 = self.rewrite(ty0).no_err();
                let ty1 = self.rewrite(ty1).no_err();
                Err(if ty0 != ty1 {
                    let diag_added = diagnostics.report(diag_stable_ptr(), diag_kind(ty0, ty1));
                    self.consume_reported_error(err, diag_added);
                    diag_added
                } else {
                    self.report_on_pending_error(err, diagnostics, diag_stable_ptr())
                })
            }
        }
    }

    /// helper function for ty_contains_var
    /// Assumes ty was already rewritten.
    #[doc(hidden)]
    fn internal_ty_contains_var(&mut self, ty: TypeId<'db>, var: InferenceVar) -> bool {
        match ty.long(self.db) {
            TypeLongId::Concrete(concrete) => {
                let generic_args = concrete.generic_args(self.db);
                self.generic_args_contain_var(&generic_args, var)
            }
            TypeLongId::Tuple(tys) => tys.iter().any(|ty| self.internal_ty_contains_var(*ty, var)),
            TypeLongId::Snapshot(ty) => self.internal_ty_contains_var(*ty, var),
            TypeLongId::Var(new_var) => {
                if InferenceVar::Type(new_var.id) == var {
                    return true;
                }
                if let Some(ty) = self.type_assignment.get(&new_var.id) {
                    return self.internal_ty_contains_var(*ty, var);
                }
                false
            }
            TypeLongId::ImplType(id) => self.impl_contains_var(id.impl_id(), var),
            TypeLongId::GenericParameter(_) | TypeLongId::Missing(_) => false,
            TypeLongId::Coupon(function_id) => self.function_contains_var(*function_id, var),
            TypeLongId::FixedSizeArray { type_id, .. } => {
                self.internal_ty_contains_var(*type_id, var)
            }
            TypeLongId::Closure(closure) => {
                closure
                    .param_tys
                    .clone()
                    .into_iter()
                    .any(|ty| self.internal_ty_contains_var(ty, var))
                    || self.internal_ty_contains_var(closure.ret_ty, var)
            }
        }
    }

    /// Creates a var for each constrained impl_type and conforms the types.
    pub fn conform_generic_params_type_constraints(
        &mut self,
        constraints: &[(TypeId<'db>, TypeId<'db>)],
    ) {
        let mut impl_type_bounds = Default::default();
        for (ty0, ty1) in constraints {
            let ty0 = if let TypeLongId::ImplType(impl_type) = ty0.long(self.db) {
                self.impl_type_assignment(*impl_type, &mut impl_type_bounds)
            } else {
                *ty0
            };
            let ty1 = if let TypeLongId::ImplType(impl_type) = ty1.long(self.db) {
                self.impl_type_assignment(*impl_type, &mut impl_type_bounds)
            } else {
                *ty1
            };
            self.conform_ty(ty0, ty1).ok();
        }
        self.set_impl_type_bounds(impl_type_bounds);
    }

    /// A helper function for getting for an impl type assignment.
    /// Creates a new type var if the impl type is not yet assigned.
    fn impl_type_assignment(
        &mut self,
        impl_type: ImplTypeId<'db>,
        impl_type_bounds: &mut OrderedHashMap<ImplTypeId<'db>, TypeId<'db>>,
    ) -> TypeId<'db> {
        match impl_type_bounds.entry(impl_type) {
            Entry::Occupied(entry) => *entry.get(),
            Entry::Vacant(entry) => {
                let inference_id = self.data.inference_id;
                let id = LocalTypeVarId(self.data.type_vars.len());
                let var = TypeVar { inference_id, id };
                let ty = TypeLongId::Var(var).intern(self.db);
                entry.insert(ty);
                self.type_vars.push(var);
                ty
            }
        }
    }
}
