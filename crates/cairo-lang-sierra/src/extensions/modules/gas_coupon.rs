use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    BranchSignature, DeferredOutputKind, LibfuncSignature, OutputVarInfo, ParamSignature,
    SierraApChange, SignatureSpecializationContext,
};
use crate::extensions::modules::gas::GasBuiltinType;
use crate::extensions::modules::int::unsigned128::Uint128Type;
use crate::extensions::modules::range_check::RangeCheckType;
use crate::extensions::{
    NamedType, NoGenericArgsGenericLibfunc, NoGenericArgsGenericType, OutputVarReferenceInfo,
    SpecializationError,
};
use crate::ids::GenericTypeId;

/// Represents a gas coupon.
///
/// Gas coupons can be bought at any point using gas from the gas counter,
/// and can be sold back at a later point in time.
#[derive(Default)]
pub struct GasCouponType {}
impl NoGenericArgsGenericType for GasCouponType {
    const ID: GenericTypeId = GenericTypeId::new_inline("GasCoupon");
    const STORABLE: bool = true;
    const DUPLICATABLE: bool = false;
    const DROPPABLE: bool = true;
    const ZERO_SIZED: bool = false;
}

define_libfunc_hierarchy! {
    pub enum GasCouponLibfunc {
        Buy(GasCouponBuyLibfunc),
        Redeposit(RedepositGasCouponLibfunc),
    }, GasCouponConcreteLibfunc
}

/// Libfunc for buying a gas coupon.
#[derive(Default)]
pub struct GasCouponBuyLibfunc {}
impl NoGenericArgsGenericLibfunc for GasCouponBuyLibfunc {
    const STR_ID: &'static str = "gas_coupon_buy";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let gas_builtin_type = context.get_concrete_type(GasBuiltinType::id(), &[])?;
        let range_check_type = context.get_concrete_type(RangeCheckType::id(), &[])?;
        let u128_type = context.get_concrete_type(Uint128Type::id(), &[])?;
        let gas_coupon_type = context.get_concrete_type(GasCouponType::id(), &[])?;

        let rc_output_info = OutputVarInfo::new_builtin(range_check_type.clone(), 0);
        Ok(LibfuncSignature {
            param_signatures: vec![
                ParamSignature::new(range_check_type).with_allow_add_const(),
                ParamSignature::new(gas_builtin_type.clone()),
                ParamSignature::new(u128_type),
            ],
            branch_signatures: vec![
                // Success.
                BranchSignature {
                    vars: vec![
                        rc_output_info.clone(),
                        OutputVarInfo {
                            ty: gas_builtin_type.clone(),
                            ref_info: OutputVarReferenceInfo::NewTempVar { idx: 0 },
                        },
                        OutputVarInfo {
                            ty: gas_coupon_type.clone(),
                            ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 2 },
                        },
                    ],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
                // Failure.
                BranchSignature {
                    vars: vec![
                        rc_output_info,
                        OutputVarInfo {
                            ty: gas_builtin_type,
                            ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 1 },
                        },
                    ],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
            ],
            fallthrough: Some(0),
        })
    }
}

/// Libfunc for redepositing a gas coupon back into the gas counter.
#[derive(Default)]
pub struct RedepositGasCouponLibfunc {}
impl NoGenericArgsGenericLibfunc for RedepositGasCouponLibfunc {
    const STR_ID: &'static str = "redeposit_gas_coupon";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let gas_builtin_type = context.get_concrete_type(GasBuiltinType::id(), &[])?;
        let gas_coupon_type = context.get_concrete_type(GasCouponType::id(), &[])?;

        Ok(LibfuncSignature::new_non_branch(
            vec![gas_builtin_type.clone(), gas_coupon_type],
            vec![OutputVarInfo {
                ty: gas_builtin_type,
                ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
            }],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}
