use std::sync::Arc;

use cairo_lang_sierra::extensions::coupon::{CouponBuyLibfunc, CouponRefundLibfunc, CouponType};
use cairo_lang_sierra::extensions::function_call::CouponCallLibfunc;
use cairo_lang_sierra::extensions::{get_unit_type, NamedLibfunc, NamedType};
use cairo_lang_sierra::ids::{ConcreteTypeId, FunctionId};
use cairo_lang_sierra::program::{self, ConcreteTypeLongId, GenStatement, GenericArg};
use cairo_lang_utils::unordered_hash_set::UnorderedHashSet;

use crate::db::{SierraGenGroup, SierraGeneratorTypeLongId};
use crate::pre_sierra;
use crate::specialization_context::SierraSignatureSpecializationContext;
use crate::utils::{drop_libfunc_id, struct_construct_libfunc_id};

/// Replaces unused coupons (coupons that are not used in `coupon_call`) with the unit type (`()`).
pub struct CouponProgramFixer<'a> {
    db: &'a dyn SierraGenGroup,
    /// The set of coupon functions that are used in a `coupon_call` libfunc in the program.
    used_coupon_funcs: UnorderedHashSet<FunctionId>,
    unit_ty: ConcreteTypeId,
}
impl<'a> CouponProgramFixer<'a> {
    /// Constructs a new [CouponProgramFixer].
    pub fn new(db: &'a dyn SierraGenGroup, statements: &[pre_sierra::Statement]) -> Self {
        // Fetch the user functions that are used in `coupon_call` statements.
        let used_coupon_funcs = statements
            .iter()
            .filter_map(|statement| {
                let pre_sierra::Statement::Sierra(GenStatement::Invocation(invocation)) = statement
                else {
                    return None;
                };
                let libfunc = db.lookup_intern_concrete_lib_func(invocation.libfunc_id.clone());
                if libfunc.generic_id != CouponCallLibfunc::STR_ID.into() {
                    return None;
                }
                let [GenericArg::UserFunc(function_id)] = &libfunc.generic_args[..] else {
                    panic!("Invalid generic args for coupon_call.");
                };
                Some(function_id.clone())
            })
            .collect::<UnorderedHashSet<_>>();

        let unit_ty = get_unit_type(&SierraSignatureSpecializationContext(db))
            .expect("Failed to construct unit type.");

        Self { db, used_coupon_funcs, unit_ty }
    }

    /// Replaces unused coupons in the statements with the unit type.
    pub fn fix_statements(&self, statements: &mut Vec<pre_sierra::Statement>) {
        for statement in statements {
            if let pre_sierra::Statement::Sierra(GenStatement::Invocation(invocation)) = statement {
                let libfunc =
                    self.db.lookup_intern_concrete_lib_func(invocation.libfunc_id.clone());
                if libfunc.generic_id == CouponBuyLibfunc::STR_ID.into()
                    && self.is_unused_coupon_in_libfunc(&libfunc)
                {
                    // Replace `coupon_buy` with `struct_construct<Unit>`.
                    invocation.libfunc_id =
                        struct_construct_libfunc_id(self.db, self.unit_ty.clone());
                } else if libfunc.generic_id == CouponRefundLibfunc::STR_ID.into()
                    && self.is_unused_coupon_in_libfunc(&libfunc)
                {
                    // Replace `coupon_refund` with `drop<Unit>`.
                    invocation.libfunc_id = drop_libfunc_id(self.db, self.unit_ty.clone());
                } else {
                    // Replace unused coupons in the libfunc generic arguments.
                    let mut libfunc =
                        self.db.lookup_intern_concrete_lib_func(invocation.libfunc_id.clone());
                    if self.modify_generic_args(&mut libfunc.generic_args) {
                        invocation.libfunc_id = self.db.intern_concrete_lib_func(libfunc);
                    }
                }
            }
        }
    }

    /// Replaces unused coupons in the generic arguments with the unit type.
    fn modify_generic_args(&self, generic_args: &mut Vec<GenericArg>) -> bool {
        let mut changed = false;
        for generic_arg in generic_args {
            if let GenericArg::Type(coupon_ty) = generic_arg {
                let res = self.fix_concrete_type(&coupon_ty);
                if &res != coupon_ty {
                    *coupon_ty = res;
                    changed = true;
                }
            }
        }
        changed
    }

    /// Replaces unused coupons in the type (including its generic arguments) with the unit type.
    fn fix_concrete_type(&self, ty: &ConcreteTypeId) -> ConcreteTypeId {
        let SierraGeneratorTypeLongId::Regular(long_id) =
            self.db.lookup_intern_concrete_type(ty.clone())
        else {
            return ty.clone();
        };

        if self.is_unused_coupon(&long_id) {
            return self.unit_ty.clone();
        }

        let mut generic_args = long_id.generic_args.clone();
        if self.modify_generic_args(&mut generic_args) {
            return self.db.intern_concrete_type(SierraGeneratorTypeLongId::Regular(Arc::new(
                ConcreteTypeLongId { generic_id: long_id.generic_id.clone(), generic_args },
            )));
        } else {
            return ty.clone();
        }
    }

    /// Returns `true` if the given type is an unused `Coupon`.
    fn is_unused_coupon(&self, long_id: &ConcreteTypeLongId) -> bool {
        if long_id.generic_id != CouponType::id() {
            return false;
        }
        let [GenericArg::UserFunc(function_id)] = &long_id.generic_args[..] else {
            panic!("Invalid generic args for Coupon type.");
        };
        !self.used_coupon_funcs.contains(function_id)
    }

    /// Returns `true` if the given type is an unused `Coupon`.
    fn is_unused_coupon_concrete_type_id(&self, concrete_type_id: &ConcreteTypeId) -> bool {
        let SierraGeneratorTypeLongId::Regular(long_id) =
            self.db.lookup_intern_concrete_type(concrete_type_id.clone())
        else {
            return false;
        };
        self.is_unused_coupon(&long_id)
    }

    /// Returns `true` if the generic argument of the libfunc is an unused `Coupon`.
    /// Assumes the libfunc is `coupon_buy` or `coupon_refund`.
    fn is_unused_coupon_in_libfunc(&self, libfunc: &program::ConcreteLibfuncLongId) -> bool {
        let [GenericArg::Type(coupon_ty)] = &libfunc.generic_args[..] else {
            panic!("Invalid generic args for coupon libfunc.");
        };
        self.is_unused_coupon_concrete_type_id(coupon_ty)
    }
}
