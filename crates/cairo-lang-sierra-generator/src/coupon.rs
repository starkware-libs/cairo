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
pub struct CouponProgramFixer {
    /// The set of coupon functions that are used in a `coupon_call` libfunc in the program.
    used_coupon_funcs: UnorderedHashSet<FunctionId>,
}
impl CouponProgramFixer {
    /// Constructs a new [CouponProgramFixer].
    pub fn new(db: &dyn SierraGenGroup, statements: &[pre_sierra::Statement]) -> Self {
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

        Self { used_coupon_funcs }
    }

    pub fn fix_statements(
        &self,
        db: &dyn SierraGenGroup,
        statements: &mut Vec<pre_sierra::Statement>,
    ) {
        let unit_ty = get_unit_type(&SierraSignatureSpecializationContext(db))
            .expect("Failed to construct unit type.");
        for statement in statements {
            if let pre_sierra::Statement::Sierra(GenStatement::Invocation(invocation)) = statement {
                let libfunc = db.lookup_intern_concrete_lib_func(invocation.libfunc_id.clone());
                if libfunc.generic_id == CouponBuyLibfunc::STR_ID.into()
                    && self.is_unused_coupon_in_libfunc(db, &libfunc)
                {
                    // Replace `coupon_buy` with `struct_construct<Unit>`.
                    invocation.libfunc_id = struct_construct_libfunc_id(db, unit_ty.clone());
                } else if libfunc.generic_id == CouponRefundLibfunc::STR_ID.into()
                    && self.is_unused_coupon_in_libfunc(db, &libfunc)
                {
                    // Replace `coupon_refund` with `drop<Unit>`.
                    invocation.libfunc_id = drop_libfunc_id(db, unit_ty.clone());
                } else {
                    // TODO(lior): Replace unused coupons in the libfunc generic arguments.
                }
            }
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
    fn is_unused_coupon_concrete_type_id(
        &self,
        db: &dyn SierraGenGroup,
        concrete_type_id: &ConcreteTypeId,
    ) -> bool {
        let SierraGeneratorTypeLongId::Regular(long_id) =
            db.lookup_intern_concrete_type(concrete_type_id.clone())
        else {
            return false;
        };
        self.is_unused_coupon(&long_id)
    }

    /// Returns `true` if the generic argument of the libfunc is an unused `Coupon`.
    /// Assumes the libfunc is `coupon_buy` or `coupon_refund`.
    fn is_unused_coupon_in_libfunc(
        &self,
        db: &dyn SierraGenGroup,
        libfunc: &program::ConcreteLibfuncLongId,
    ) -> bool {
        let [GenericArg::Type(coupon_ty)] = &libfunc.generic_args[..] else {
            panic!("Invalid generic args for coupon libfunc.");
        };
        self.is_unused_coupon_concrete_type_id(db, coupon_ty)
    }
}
