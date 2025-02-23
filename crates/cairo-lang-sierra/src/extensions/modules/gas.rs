use convert_case::Casing;
use itertools::chain;
use serde::{Deserialize, Serialize};

use super::int::unsigned128::Uint128Type;
// Module providing the gas related extensions.
use super::range_check::RangeCheckType;
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    BranchSignature, DeferredOutputKind, LibfuncSignature, OutputVarInfo, ParamSignature,
    SierraApChange, SignatureSpecializationContext,
};
use crate::extensions::{
    NamedType, NoGenericArgsGenericLibfunc, NoGenericArgsGenericType, OutputVarReferenceInfo,
    SpecializationError,
};
use crate::ids::GenericTypeId;

/// Type for gas actions.
#[derive(Default)]
pub struct GasBuiltinType {}
impl NoGenericArgsGenericType for GasBuiltinType {
    const ID: GenericTypeId = GenericTypeId::new_inline("GasBuiltin");
    const STORABLE: bool = true;
    const DUPLICATABLE: bool = false;
    const DROPPABLE: bool = false;
    const ZERO_SIZED: bool = false;
}

define_libfunc_hierarchy! {
    pub enum GasLibfunc {
        WithdrawGas(WithdrawGasLibfunc),
        RedepositGas(RedepositGasLibfunc),
        GetAvailableGas(GetAvailableGasLibfunc),
        GetUnspentGas(GetUnspentGasLibfunc),
        BuiltinWithdrawGas(BuiltinCostWithdrawGasLibfunc),
        GetBuiltinCosts(BuiltinCostGetBuiltinCostsLibfunc),
    }, GasConcreteLibfunc
}

/// Libfunc for withdrawing gas.
#[derive(Default)]
pub struct WithdrawGasLibfunc {}
impl NoGenericArgsGenericLibfunc for WithdrawGasLibfunc {
    const STR_ID: &'static str = "withdraw_gas";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let gas_builtin_type = context.get_concrete_type(GasBuiltinType::id(), &[])?;
        let range_check_type = context.get_concrete_type(RangeCheckType::id(), &[])?;
        let rc_output_info = OutputVarInfo::new_builtin(range_check_type.clone(), 0);
        Ok(LibfuncSignature {
            param_signatures: vec![
                ParamSignature::new(range_check_type).with_allow_add_const(),
                ParamSignature::new(gas_builtin_type.clone()),
            ],
            branch_signatures: vec![
                // Success:
                BranchSignature {
                    vars: vec![
                        rc_output_info.clone(),
                        OutputVarInfo {
                            ty: gas_builtin_type.clone(),
                            ref_info: OutputVarReferenceInfo::NewTempVar { idx: 0 },
                        },
                    ],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
                // Failure:
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

/// Libfunc for returning unused gas.
#[derive(Default)]
pub struct RedepositGasLibfunc {}
impl NoGenericArgsGenericLibfunc for RedepositGasLibfunc {
    const STR_ID: &'static str = "redeposit_gas";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let gas_builtin_type = context.get_concrete_type(GasBuiltinType::id(), &[])?;
        Ok(LibfuncSignature::new_non_branch(
            vec![gas_builtin_type.clone()],
            vec![OutputVarInfo {
                ty: gas_builtin_type,
                ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
            }],
            SierraApChange::Known { new_vars_only: false },
        ))
    }
}

/// Libfunc for returning the amount of available gas.
#[derive(Default)]
pub struct GetAvailableGasLibfunc {}
impl NoGenericArgsGenericLibfunc for GetAvailableGasLibfunc {
    const STR_ID: &'static str = "get_available_gas";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let gas_builtin_type = context.get_concrete_type(GasBuiltinType::id(), &[])?;
        Ok(LibfuncSignature::new_non_branch(
            vec![gas_builtin_type.clone()],
            vec![
                OutputVarInfo {
                    ty: gas_builtin_type,
                    ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 0 },
                },
                OutputVarInfo {
                    ty: context.get_concrete_type(Uint128Type::id(), &[])?,
                    ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 0 },
                },
            ],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}

/// Libfunc for returning the amount of available gas, including gas in local wallet.
#[derive(Default)]
pub struct GetUnspentGasLibfunc {}
impl NoGenericArgsGenericLibfunc for GetUnspentGasLibfunc {
    const STR_ID: &'static str = "get_unspent_gas";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let gas_builtin_type = context.get_concrete_type(GasBuiltinType::id(), &[])?;
        Ok(LibfuncSignature::new_non_branch(
            vec![gas_builtin_type.clone()],
            vec![
                OutputVarInfo {
                    ty: gas_builtin_type,
                    ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 0 },
                },
                OutputVarInfo {
                    ty: context.get_concrete_type(Uint128Type::id(), &[])?,
                    ref_info: OutputVarReferenceInfo::SimpleDerefs,
                },
            ],
            SierraApChange::Known { new_vars_only: false },
        ))
    }
}

/// Represents different type of costs.
/// Note that if you add a type here you should update 'iter_precost'
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub enum CostTokenType {
    /// A compile time known cost unit. This is a linear combination of the runtime tokens
    /// ([CostTokenType::Step], [CostTokenType::Hole], [CostTokenType::RangeCheck],
    /// [CostTokenType::RangeCheck96]).
    Const,

    // Runtime post-cost token types:
    /// The number of steps.
    Step,
    /// The number of memory holes (untouched memory addresses).
    Hole,
    /// The number of 128-bit range check builtins.
    RangeCheck,
    /// The number of 96-bit range check builtins.
    RangeCheck96,

    // Pre-cost token types (builtins):
    /// One invocation of the pedersen hash function.
    Pedersen,
    /// One invocation of the Poseidon hades permutation.
    Poseidon,
    /// One invocation of the bitwise builtin.
    Bitwise,
    /// One invocation of the EC op builtin.
    EcOp,
    // Add mod builtin.
    AddMod,
    // mul mod builtin.
    MulMod,
}
impl CostTokenType {
    /// Iterates over the pre-cost token types.
    pub fn iter_precost() -> std::slice::Iter<'static, Self> {
        [
            CostTokenType::Pedersen,
            CostTokenType::Poseidon,
            CostTokenType::Bitwise,
            CostTokenType::EcOp,
            CostTokenType::AddMod,
            CostTokenType::MulMod,
        ]
        .iter()
    }

    /// Iterates over the tokens that are used in the Sierra->Casm compilation (pre-cost token types
    /// and [CostTokenType::Const]).
    pub fn iter_casm_tokens()
    -> std::iter::Chain<std::slice::Iter<'static, Self>, std::slice::Iter<'static, Self>> {
        chain!(Self::iter_precost(), [CostTokenType::Const].iter())
    }

    /// Returns the name of the token type, in snake_case.
    pub fn name(&self) -> String {
        match self {
            CostTokenType::Const => "const",
            CostTokenType::Step => "step",
            CostTokenType::Hole => "hole",
            CostTokenType::RangeCheck => "range_check",
            CostTokenType::RangeCheck96 => "range_check96",
            CostTokenType::Pedersen => "pedersen",
            CostTokenType::Bitwise => "bitwise",
            CostTokenType::EcOp => "ec_op",
            CostTokenType::Poseidon => "poseidon",
            CostTokenType::AddMod => "add_mod",
            CostTokenType::MulMod => "mul_mod",
        }
        .into()
    }

    pub fn camel_case_name(&self) -> String {
        self.name().to_case(convert_case::Case::UpperCamel)
    }

    pub fn offset_in_builtin_costs(&self) -> i16 {
        match self {
            CostTokenType::Const
            | CostTokenType::Step
            | CostTokenType::Hole
            | CostTokenType::RangeCheck
            | CostTokenType::RangeCheck96 => {
                panic!("offset_in_builtin_costs is not supported for '{}'.", self.camel_case_name())
            }

            CostTokenType::Pedersen => 0,
            CostTokenType::Bitwise => 1,
            CostTokenType::EcOp => 2,
            CostTokenType::Poseidon => 3,
            // TODO(ilya): Update the actual table.
            CostTokenType::AddMod => 4,
            CostTokenType::MulMod => 5,
        }
    }
}

/// Represents a pointer to an array with the builtin costs.
/// Every element in the array is the cost of a single invocation of a builtin.
///
/// Offsets to the array are given by [CostTokenType::offset_in_builtin_costs].
#[derive(Default)]
pub struct BuiltinCostsType {}
impl NoGenericArgsGenericType for BuiltinCostsType {
    const ID: GenericTypeId = GenericTypeId::new_inline("BuiltinCosts");
    const STORABLE: bool = true;
    const DUPLICATABLE: bool = true;
    const DROPPABLE: bool = true;
    const ZERO_SIZED: bool = false;
}
impl BuiltinCostsType {
    /// Returns the number of steps required for the computation of the requested cost, given the
    /// number of requested token usages. The number of steps is also the change in `ap`.
    /// If `table_available` is false, the number of steps includes the cost of fetching the builtin
    /// cost table.
    pub fn cost_computation_steps<TokenUsages: Fn(CostTokenType) -> usize>(
        table_available: bool,
        token_usages: TokenUsages,
    ) -> usize {
        let calculation_steps = CostTokenType::iter_precost()
            .map(|token_type| match token_usages(*token_type) {
                0 => 0,
                1 => 2,
                _ => 3,
            })
            .sum();
        if calculation_steps > 0 && !table_available {
            calculation_steps + 4
        } else {
            calculation_steps
        }
    }
}

/// Libfunc for withdrawing gas to be used by a builtin.
#[derive(Default)]
pub struct BuiltinCostWithdrawGasLibfunc;
impl NoGenericArgsGenericLibfunc for BuiltinCostWithdrawGasLibfunc {
    const STR_ID: &'static str = "withdraw_gas_all";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let gas_builtin_type = context.get_concrete_type(GasBuiltinType::id(), &[])?;
        let range_check_type = context.get_concrete_type(RangeCheckType::id(), &[])?;
        let builtin_costs_type = context.get_concrete_type(BuiltinCostsType::id(), &[])?;
        let rc_output_info = OutputVarInfo::new_builtin(range_check_type.clone(), 0);
        Ok(LibfuncSignature {
            param_signatures: vec![
                ParamSignature::new(range_check_type).with_allow_add_const(),
                ParamSignature::new(gas_builtin_type.clone()),
                ParamSignature::new(builtin_costs_type),
            ],
            branch_signatures: vec![
                // Success:
                BranchSignature {
                    vars: vec![
                        rc_output_info.clone(),
                        OutputVarInfo {
                            ty: gas_builtin_type.clone(),
                            ref_info: OutputVarReferenceInfo::NewTempVar { idx: 0 },
                        },
                    ],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
                // Failure:
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

/// Libfunc for getting the pointer to the gas cost array.
/// See [BuiltinCostsType].
#[derive(Default)]
pub struct BuiltinCostGetBuiltinCostsLibfunc {}

impl NoGenericArgsGenericLibfunc for BuiltinCostGetBuiltinCostsLibfunc {
    const STR_ID: &'static str = "get_builtin_costs";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let builtin_costs_type = context.get_concrete_type(BuiltinCostsType::id(), &[])?;
        Ok(LibfuncSignature::new_non_branch(
            vec![],
            vec![OutputVarInfo {
                ty: builtin_costs_type,
                ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
            }],
            SierraApChange::Known { new_vars_only: false },
        ))
    }
}
