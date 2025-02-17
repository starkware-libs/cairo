use std::iter;
use std::ops::Shl;

use cairo_lang_sierra::extensions::array::ArrayConcreteLibfunc;
use cairo_lang_sierra::extensions::boolean::BoolConcreteLibfunc;
use cairo_lang_sierra::extensions::bounded_int::{
    BoundedIntConcreteLibfunc, BoundedIntDivRemAlgorithm,
};
use cairo_lang_sierra::extensions::boxing::BoxConcreteLibfunc;
use cairo_lang_sierra::extensions::bytes31::Bytes31ConcreteLibfunc;
use cairo_lang_sierra::extensions::casts::{CastConcreteLibfunc, CastType};
use cairo_lang_sierra::extensions::circuit::{
    CircuitConcreteLibfunc, CircuitInfo, MOD_BUILTIN_INSTANCE_SIZE,
};
use cairo_lang_sierra::extensions::const_type::ConstConcreteLibfunc;
use cairo_lang_sierra::extensions::core::CoreConcreteLibfunc::{self, *};
use cairo_lang_sierra::extensions::coupon::CouponConcreteLibfunc;
use cairo_lang_sierra::extensions::ec::EcConcreteLibfunc;
use cairo_lang_sierra::extensions::enm::EnumConcreteLibfunc;
use cairo_lang_sierra::extensions::felt252::{
    Felt252BinaryOperationConcrete, Felt252BinaryOperator, Felt252Concrete,
};
use cairo_lang_sierra::extensions::felt252_dict::{
    Felt252DictConcreteLibfunc, Felt252DictEntryConcreteLibfunc,
};
use cairo_lang_sierra::extensions::function_call::SignatureAndFunctionConcreteLibfunc;
use cairo_lang_sierra::extensions::gas::GasConcreteLibfunc::{
    BuiltinWithdrawGas, GetAvailableGas, GetBuiltinCosts, GetUnspentGas, RedepositGas, WithdrawGas,
};
use cairo_lang_sierra::extensions::gas::{BuiltinCostsType, CostTokenType};
use cairo_lang_sierra::extensions::int::signed::{SintConcrete, SintTraits};
use cairo_lang_sierra::extensions::int::signed128::Sint128Concrete;
use cairo_lang_sierra::extensions::int::unsigned::{UintConcrete, UintTraits};
use cairo_lang_sierra::extensions::int::unsigned128::Uint128Concrete;
use cairo_lang_sierra::extensions::int::unsigned256::Uint256Concrete;
use cairo_lang_sierra::extensions::int::unsigned512::Uint512Concrete;
use cairo_lang_sierra::extensions::int::{IntMulTraits, IntOperator};
use cairo_lang_sierra::extensions::is_zero::IsZeroTraits;
use cairo_lang_sierra::extensions::mem::MemConcreteLibfunc::{
    AllocLocal, FinalizeLocals, Rename, StoreLocal, StoreTemp,
};
use cairo_lang_sierra::extensions::nullable::NullableConcreteLibfunc;
use cairo_lang_sierra::extensions::pedersen::PedersenConcreteLibfunc;
use cairo_lang_sierra::extensions::poseidon::PoseidonConcreteLibfunc;
use cairo_lang_sierra::extensions::range::IntRangeConcreteLibfunc;
use cairo_lang_sierra::extensions::structure::StructConcreteLibfunc;
use cairo_lang_sierra::ids::ConcreteTypeId;
use cairo_lang_sierra::program::Function;
use cairo_lang_utils::casts::IntoOrPanic;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use itertools::{Itertools, chain};
use num_bigint::BigInt;
use num_traits::{One, Zero};

use crate::objects::{
    BranchCost, BranchCostSign, ConstCost, CostInfoProvider, PreCost, WithdrawGasBranchInfo,
};
use crate::starknet_libfunc_cost_base::starknet_libfunc_cost_base;

/// The cost per each unique key in the dictionary.
///
/// This cost is pre-charged for each access (read/write/entry), and the overhead cost is refunded
/// for each repeated access. Repeated access is access to a key that has already been accessed
/// before.
pub const DICT_SQUASH_UNIQUE_KEY_COST: ConstCost =
    ConstCost { steps: 46, holes: 0, range_checks: 6, range_checks96: 0 };
/// The cost per each access to a key after the first access.
pub const DICT_SQUASH_REPEATED_ACCESS_COST: ConstCost =
    ConstCost { steps: 9, holes: 0, range_checks: 1, range_checks96: 0 };
/// The cost not dependent on the number of keys and access.
pub const DICT_SQUASH_FIXED_COST: ConstCost =
    ConstCost { steps: 57, holes: 0, range_checks: 3, range_checks96: 0 };

/// The cost of allocating a segment in the segment arena. This is charged to pay for the
/// finalization step of the segment arena.
pub const SEGMENT_ARENA_ALLOCATION_COST: ConstCost =
    ConstCost { steps: 8, holes: 0, range_checks: 0, range_checks96: 0 };

/// The operation required for extracting a libfunc's cost.
pub trait CostOperations {
    type CostType: Clone;

    /// Gets a cost from a constant value (of type [CostTokenType::Const]).
    fn const_cost(&self, value: ConstCost) -> Self::CostType {
        self.cost_token(value.cost(), CostTokenType::Const)
    }

    /// Gets a cost from step count.
    fn steps(&self, steps: i32) -> Self::CostType {
        self.const_cost(ConstCost { steps, ..ConstCost::default() })
    }

    /// Gets a cost of the given token type.
    fn cost_token(&self, count: i32, token_type: CostTokenType) -> Self::CostType;
    /// Gets a cost for the content of a function.
    fn function_token_cost(
        &mut self,
        function: &Function,
        token_type: CostTokenType,
    ) -> Self::CostType;
    /// Gets a cost for a variable for the current statement.
    fn statement_var_cost(&self, token_type: CostTokenType) -> Self::CostType;
    /// Adds costs.
    fn add(&self, lhs: Self::CostType, rhs: Self::CostType) -> Self::CostType;
    /// Subtracts costs.
    fn sub(&self, lhs: Self::CostType, rhs: Self::CostType) -> Self::CostType;
}

/// Trait for providing extra information required for calculating costs for a specific libfunc
/// invocation.
pub trait InvocationCostInfoProvider {
    /// Provides the sizes of types.
    fn type_size(&self, ty: &ConcreteTypeId) -> usize;
    /// Number of tokens provided by the libfunc invocation (currently only relevant for
    /// `withdraw_gas_all`).
    fn token_usages(&self, token_type: CostTokenType) -> usize;
    /// Provides the ap change variable value of the current statement.
    fn ap_change_var_value(&self) -> usize;
    /// Provides the info for the circuit.
    fn circuit_info(&self, ty: &ConcreteTypeId) -> &CircuitInfo;
}

impl<InfoProvider: InvocationCostInfoProvider> CostInfoProvider for InfoProvider {
    fn type_size(&self, ty: &ConcreteTypeId) -> usize {
        self.type_size(ty)
    }

    fn circuit_info(&self, ty: &ConcreteTypeId) -> &CircuitInfo {
        self.circuit_info(ty)
    }
}

/// Returns a postcost value for a libfunc - the cost of step token.
pub fn core_libfunc_cost(
    libfunc: &CoreConcreteLibfunc,
    info_provider: &dyn CostInfoProvider,
) -> Vec<BranchCost> {
    match libfunc {
        FunctionCall(SignatureAndFunctionConcreteLibfunc { function, .. }) => {
            vec![BranchCost::FunctionCost {
                const_cost: ConstCost::steps(2),
                function: function.clone(),
                sign: BranchCostSign::Subtract,
            }]
        }
        CouponCall(_) => vec![ConstCost::steps(2).into()],
        Bool(libfunc) => match libfunc {
            BoolConcreteLibfunc::And(_) => vec![ConstCost::steps(0).into()],
            BoolConcreteLibfunc::Not(_) => vec![ConstCost::steps(1).into()],
            BoolConcreteLibfunc::Xor(_) => vec![ConstCost::steps(1).into()],
            BoolConcreteLibfunc::Or(_) => vec![ConstCost::steps(2).into()],
            BoolConcreteLibfunc::ToFelt252(_) => vec![ConstCost::steps(0).into()],
        },
        Cast(libfunc) => match libfunc {
            CastConcreteLibfunc::Downcast(libfunc) => {
                if libfunc.from_range.is_full_felt252_range() {
                    let success_extra_steps = if libfunc.to_range.lower.is_zero() { 0 } else { 1 }
                        + if &libfunc.to_range.upper - 1 == u128::MAX.into() { 0 } else { 1 };
                    let failure_extra_steps = if libfunc.to_range.upper.is_zero() { 0 } else { 1 };
                    vec![
                        ConstCost {
                            steps: success_extra_steps + 3,
                            holes: 0,
                            range_checks: 2,
                            range_checks96: 0,
                        }
                        .into(),
                        ConstCost {
                            steps: failure_extra_steps + 9,
                            holes: 0,
                            range_checks: 3,
                            range_checks96: 0,
                        }
                        .into(),
                    ]
                } else {
                    // Overflow tests are more expensive when asserting a value is above non-zero
                    // value.
                    let extra_below = if libfunc.to_range.lower.is_zero() { 0 } else { 1 };
                    let extra_above = if libfunc.to_range.upper.is_zero() { 0 } else { 1 };
                    match libfunc.cast_type() {
                        CastType { overflow_above: false, overflow_below: false } => {
                            vec![ConstCost::steps(0).into(), ConstCost::steps(0).into()]
                        }
                        CastType { overflow_above: true, overflow_below: false } => vec![
                            (ConstCost::steps(3) + ConstCost::range_checks(1)).into(),
                            (ConstCost::steps(3 + extra_above) + ConstCost::range_checks(1)).into(),
                        ],
                        CastType { overflow_above: false, overflow_below: true } => {
                            vec![
                                (ConstCost::steps(2 + extra_below) + ConstCost::range_checks(1))
                                    .into(),
                                (ConstCost::steps(4) + ConstCost::range_checks(1)).into(),
                            ]
                        }
                        CastType { overflow_above: true, overflow_below: true } => {
                            vec![
                                (ConstCost::steps(4 + extra_below) + ConstCost::range_checks(2))
                                    .into(),
                                (ConstCost::steps(5) + ConstCost::range_checks(1)).into(),
                            ]
                        }
                    }
                }
            }
            CastConcreteLibfunc::Upcast(_) => vec![ConstCost::default().into()],
        },
        Ec(libfunc) => match libfunc {
            EcConcreteLibfunc::IsZero(_) => {
                vec![ConstCost::steps(1).into(), ConstCost::steps(1).into()]
            }
            EcConcreteLibfunc::Neg(_) => vec![ConstCost::default().into()],
            EcConcreteLibfunc::StateAdd(_) => vec![ConstCost::steps(10).into()],
            EcConcreteLibfunc::TryNew(_) => {
                vec![ConstCost::steps(7).into(), ConstCost::steps(7).into()]
            }
            EcConcreteLibfunc::StateFinalize(_) => {
                vec![ConstCost::steps(12).into(), ConstCost::steps(6).into()]
            }
            EcConcreteLibfunc::StateInit(_) => vec![ConstCost::steps(7).into()],
            EcConcreteLibfunc::StateAddMul(_) => {
                vec![BranchCost::Regular {
                    const_cost: ConstCost::steps(5),
                    pre_cost: PreCost::builtin(CostTokenType::EcOp),
                }]
            }
            EcConcreteLibfunc::PointFromX(_) => vec![
                (ConstCost::steps(14) + ConstCost::range_checks(3)).into(), // Success.
                ConstCost::steps(9).into(),                                 // Failure.
            ],
            EcConcreteLibfunc::UnwrapPoint(_) => vec![ConstCost::default().into()],
            EcConcreteLibfunc::Zero(_) => vec![ConstCost::default().into()],
        },
        Gas(libfunc) => match libfunc {
            WithdrawGas(_) => vec![
                BranchCost::WithdrawGas(WithdrawGasBranchInfo {
                    success: true,
                    with_builtin_costs: false,
                }),
                BranchCost::WithdrawGas(WithdrawGasBranchInfo {
                    success: false,
                    with_builtin_costs: false,
                }),
            ],
            RedepositGas(_) => vec![BranchCost::RedepositGas],
            GetAvailableGas(_) => vec![ConstCost::default().into()],
            GetUnspentGas(_) => vec![
                ConstCost::steps(
                    BuiltinCostsType::cost_computation_steps(false, |_| 2).into_or_panic::<i32>()
                        + 1,
                )
                .into(),
            ],
            BuiltinWithdrawGas(_) => {
                vec![
                    BranchCost::WithdrawGas(WithdrawGasBranchInfo {
                        success: true,
                        with_builtin_costs: true,
                    }),
                    BranchCost::WithdrawGas(WithdrawGasBranchInfo {
                        success: false,
                        with_builtin_costs: true,
                    }),
                ]
            }
            GetBuiltinCosts(_) => vec![ConstCost::steps(3).into()],
        },
        BranchAlign(_) => vec![BranchCost::BranchAlign],
        Array(libfunc) => match libfunc {
            ArrayConcreteLibfunc::New(_) => vec![ConstCost::steps(1).into()],
            ArrayConcreteLibfunc::SpanFromTuple(_) => vec![ConstCost::steps(0).into()],
            ArrayConcreteLibfunc::TupleFromSpan(_) => {
                vec![ConstCost::steps(3).into(), ConstCost::steps(3).into()]
            }
            ArrayConcreteLibfunc::Append(libfunc) => {
                vec![ConstCost::steps(info_provider.type_size(&libfunc.ty) as i32).into()]
            }
            ArrayConcreteLibfunc::PopFront(_)
            | ArrayConcreteLibfunc::PopFrontConsume(_)
            | ArrayConcreteLibfunc::SnapshotPopFront(_)
            | ArrayConcreteLibfunc::SnapshotPopBack(_) => {
                vec![ConstCost::steps(2).into(), ConstCost::steps(3).into()]
            }
            ArrayConcreteLibfunc::SnapshotMultiPopFront(_)
            | ArrayConcreteLibfunc::SnapshotMultiPopBack(_) => {
                vec![
                    (ConstCost::steps(4) + ConstCost::range_checks(1)).into(),
                    (ConstCost::steps(5) + ConstCost::range_checks(1)).into(),
                ]
            }
            ArrayConcreteLibfunc::Get(libfunc) => {
                if info_provider.type_size(&libfunc.ty) == 1 {
                    vec![
                        (ConstCost::steps(5) + ConstCost::range_checks(1)).into(),
                        (ConstCost::steps(5) + ConstCost::range_checks(1)).into(),
                    ]
                } else {
                    vec![
                        (ConstCost::steps(6) + ConstCost::range_checks(1)).into(),
                        (ConstCost::steps(6) + ConstCost::range_checks(1)).into(),
                    ]
                }
            }
            ArrayConcreteLibfunc::Slice(libfunc) => {
                if info_provider.type_size(&libfunc.ty) == 1 {
                    vec![
                        (ConstCost::steps(5) + ConstCost::range_checks(1)).into(),
                        (ConstCost::steps(7) + ConstCost::range_checks(1)).into(),
                    ]
                } else {
                    vec![
                        (ConstCost::steps(7) + ConstCost::range_checks(1)).into(),
                        (ConstCost::steps(8) + ConstCost::range_checks(1)).into(),
                    ]
                }
            }
            ArrayConcreteLibfunc::Len(libfunc) => {
                vec![
                    ConstCost::steps(if info_provider.type_size(&libfunc.ty) == 1 { 0 } else { 1 })
                        .into(),
                ]
            }
        },
        Uint8(libfunc) => uint_libfunc_cost(libfunc),
        Uint16(libfunc) => uint_libfunc_cost(libfunc),
        Uint32(libfunc) => uint_libfunc_cost(libfunc),
        Uint64(libfunc) => uint_libfunc_cost(libfunc),
        Uint128(libfunc) => u128_libfunc_cost(libfunc),
        Uint256(libfunc) => u256_libfunc_cost(libfunc).into_iter().map(BranchCost::from).collect(),
        Uint512(libfunc) => u512_libfunc_cost(libfunc).into_iter().map(BranchCost::from).collect(),
        Sint8(libfunc) => sint_libfunc_cost(libfunc),
        Sint16(libfunc) => sint_libfunc_cost(libfunc),
        Sint32(libfunc) => sint_libfunc_cost(libfunc),
        Sint64(libfunc) => sint_libfunc_cost(libfunc),
        Sint128(libfunc) => s128_libfunc_cost(libfunc),
        Felt252(libfunc) => {
            felt252_libfunc_cost(libfunc).into_iter().map(BranchCost::from).collect()
        }
        Drop(_) | Dup(_) | ApTracking(_) | UnwrapNonZero(_) => {
            vec![ConstCost::default().into()]
        }
        Box(libfunc) => match libfunc {
            BoxConcreteLibfunc::Into(libfunc) => {
                let n_steps =
                    std::cmp::max(1, info_provider.type_size(&libfunc.ty).try_into().unwrap());
                vec![ConstCost::steps(n_steps).into()]
            }
            BoxConcreteLibfunc::Unbox(_) | BoxConcreteLibfunc::ForwardSnapshot(_) => {
                vec![ConstCost::default().into()]
            }
        },
        Mem(libfunc) => match libfunc {
            StoreTemp(libfunc) => {
                vec![ConstCost::steps(info_provider.type_size(&libfunc.ty) as i32).into()]
            }
            StoreLocal(libfunc) => {
                let size = info_provider.type_size(&libfunc.ty) as i32;
                vec![(ConstCost::steps(size) + ConstCost::holes(-size)).into()]
            }
            AllocLocal(libfunc) => {
                vec![ConstCost::holes(info_provider.type_size(&libfunc.ty) as i32).into()]
            }
            FinalizeLocals(_) => vec![ConstCost::steps(1).into()],
            Rename(_) => vec![ConstCost::default().into()],
        },
        UnconditionalJump(_) => {
            vec![ConstCost::steps(1).into()]
        }
        Enum(libfunc) => match libfunc {
            EnumConcreteLibfunc::Init(_) => vec![ConstCost::default().into()],
            EnumConcreteLibfunc::FromBoundedInt(libfunc) => match libfunc.n_variants {
                1 | 2 => vec![ConstCost::default().into()],
                _ => vec![ConstCost::steps(1).into()],
            },
            EnumConcreteLibfunc::Match(sig) | EnumConcreteLibfunc::SnapshotMatch(sig) => {
                let n = sig.signature.branch_signatures.len();
                match n {
                    0 => vec![],
                    1 => vec![ConstCost::default().into()],
                    2 => vec![ConstCost::steps(1).into(); 2],
                    _ => chain!(
                        iter::once(ConstCost::steps(1).into()),
                        itertools::repeat_n(ConstCost::steps(2).into(), n - 1)
                    )
                    .collect_vec(),
                }
            }
        },
        Struct(
            StructConcreteLibfunc::Construct(_)
            | StructConcreteLibfunc::Deconstruct(_)
            | StructConcreteLibfunc::SnapshotDeconstruct(_),
        ) => {
            vec![ConstCost::default().into()]
        }
        Felt252Dict(libfunc) => match libfunc {
            Felt252DictConcreteLibfunc::New(_) => {
                vec![(ConstCost::steps(9) + SEGMENT_ARENA_ALLOCATION_COST).into()]
            }
            Felt252DictConcreteLibfunc::Squash(_) => {
                // The `dict_squash` operation incurs a fixed cost of `DICT_SQUASH_CONST_COST`,
                // plus an additional cost of `DICT_SQUASH_UNIQUE_KEY_COST` for each read or write
                // access to the dictionary. However, the true cost of processing each unique key
                // is `DICT_SQUASH_UNIQUE_KEY_COST`, while accessing an existing key incurs a lower
                // cost of `DICT_SQUASH_REPEATED_ACCESS_COST`.
                //
                // To accurately reflect these costs, we charge `DICT_SQUASH_UNIQUE_KEY_COST`
                // for each read or write access, and within the `dict_squash` operation an amount
                // of `DICT_SQUASH_UNIQUE_KEY_COST - DICT_SQUASH_REPEATED_ACCESS_COST` is refunded
                // for each subsequent access to an existing key.
                vec![DICT_SQUASH_FIXED_COST.into()]
            }
        },
        Felt252SquashedDict(_) => vec![ConstCost::default().into()],
        Pedersen(libfunc) => match libfunc {
            PedersenConcreteLibfunc::PedersenHash(_) => {
                vec![BranchCost::Regular {
                    const_cost: ConstCost::steps(2),
                    pre_cost: PreCost::builtin(CostTokenType::Pedersen),
                }]
            }
        },
        Poseidon(libfunc) => match libfunc {
            PoseidonConcreteLibfunc::HadesPermutation(_) => vec![BranchCost::Regular {
                const_cost: ConstCost::steps(3),
                pre_cost: PreCost::builtin(CostTokenType::Poseidon),
            }],
        },
        Starknet(libfunc) => {
            starknet_libfunc_cost_base(libfunc).into_iter().map(BranchCost::from).collect()
        }
        Nullable(libfunc) => match libfunc {
            NullableConcreteLibfunc::Null(_)
            | NullableConcreteLibfunc::NullableFromBox(_)
            | NullableConcreteLibfunc::ForwardSnapshot(_) => vec![ConstCost::default().into()],
            NullableConcreteLibfunc::MatchNullable(_) => {
                vec![ConstCost::steps(1).into(), ConstCost::steps(1).into()]
            }
        },
        Debug(_) => vec![ConstCost::steps(1).into()],
        SnapshotTake(_) => vec![ConstCost::default().into()],
        Felt252DictEntry(libfunc) => match libfunc {
            Felt252DictEntryConcreteLibfunc::Get(_) => {
                vec![(ConstCost::steps(1) + DICT_SQUASH_UNIQUE_KEY_COST).into()]
            }
            Felt252DictEntryConcreteLibfunc::Finalize(_) => vec![ConstCost::steps(1).into()],
        },
        Bytes31(libfunc) => match libfunc {
            Bytes31ConcreteLibfunc::Const(_) | Bytes31ConcreteLibfunc::ToFelt252(_) => {
                vec![ConstCost::default().into()]
            }
            Bytes31ConcreteLibfunc::TryFromFelt252(_) => vec![
                (ConstCost { steps: 7, holes: 0, range_checks: 3, range_checks96: 0 }).into(),
                (ConstCost { steps: 9, holes: 0, range_checks: 3, range_checks96: 0 }).into(),
            ],
        },
        Const(libfunc) => match libfunc {
            ConstConcreteLibfunc::AsBox(_) => vec![ConstCost::steps(3).into()],
            ConstConcreteLibfunc::AsImmediate(_) => vec![ConstCost::steps(0).into()],
        },
        Coupon(libfunc) => match libfunc {
            CouponConcreteLibfunc::Buy(libfunc) => {
                vec![BranchCost::FunctionCost {
                    const_cost: ConstCost::default(),
                    function: libfunc.function.clone(),
                    sign: BranchCostSign::Subtract,
                }]
            }
            CouponConcreteLibfunc::Refund(libfunc) => {
                vec![BranchCost::FunctionCost {
                    const_cost: ConstCost::default(),
                    function: libfunc.function.clone(),
                    sign: BranchCostSign::Add,
                }]
            }
        },
        BoundedInt(libfunc) => match libfunc {
            BoundedIntConcreteLibfunc::Add(_)
            | BoundedIntConcreteLibfunc::Sub(_)
            | BoundedIntConcreteLibfunc::Mul(_) => vec![ConstCost::steps(0).into()],
            BoundedIntConcreteLibfunc::DivRem(libfunc) => {
                vec![
                    match BoundedIntDivRemAlgorithm::try_new(&libfunc.lhs, &libfunc.rhs).unwrap() {
                        BoundedIntDivRemAlgorithm::KnownSmallRhs => {
                            ConstCost { steps: 7, holes: 0, range_checks: 3, range_checks96: 0 }
                        }
                        BoundedIntDivRemAlgorithm::KnownSmallQuotient { .. } => {
                            ConstCost { steps: 9, holes: 0, range_checks: 4, range_checks96: 0 }
                        }
                        BoundedIntDivRemAlgorithm::KnownSmallLhs { .. } => {
                            ConstCost { steps: 11, holes: 0, range_checks: 4, range_checks96: 0 }
                        }
                    }
                    .into(),
                ]
            }
            BoundedIntConcreteLibfunc::Constrain(libfunc) => {
                vec![
                    (ConstCost {
                        steps: 2 + if libfunc.boundary == BigInt::one().shl(128) { 0 } else { 1 },
                        holes: 0,
                        range_checks: 1,
                        range_checks96: 0,
                    })
                    .into(),
                    (ConstCost {
                        steps: 3 + if libfunc.boundary.is_zero() { 0 } else { 1 },
                        holes: 0,
                        range_checks: 1,
                        range_checks96: 0,
                    })
                    .into(),
                ]
            }
            BoundedIntConcreteLibfunc::TrimMin(libfunc)
            | BoundedIntConcreteLibfunc::TrimMax(libfunc) => {
                let steps: BranchCost =
                    ConstCost::steps(if libfunc.trimmed_value.is_zero() { 1 } else { 2 }).into();
                vec![steps.clone(), steps]
            }
            BoundedIntConcreteLibfunc::IsZero(_) => {
                vec![ConstCost::steps(1).into(), ConstCost::steps(1).into()]
            }
            BoundedIntConcreteLibfunc::WrapNonZero(_) => {
                vec![ConstCost::steps(0).into()]
            }
        },
        Circuit(libfunc) => match libfunc {
            CircuitConcreteLibfunc::AddInput(_) => {
                vec![ConstCost::steps(7).into(), ConstCost::steps(7).into()]
            }
            CircuitConcreteLibfunc::Eval(libfunc) => {
                let info = info_provider.circuit_info(&libfunc.ty);

                let instance_size: i32 = MOD_BUILTIN_INSTANCE_SIZE.into_or_panic();
                let mut steps: i32 = 8 + instance_size;

                if !info.add_offsets.is_empty() {
                    steps += instance_size;
                }

                vec![
                    // Failure.
                    BranchCost::Regular {
                        const_cost: ConstCost::steps(steps),
                        pre_cost: PreCost(OrderedHashMap::from_iter([
                            (CostTokenType::AddMod, info.add_offsets.len().into_or_panic()),
                            (CostTokenType::MulMod, info.mul_offsets.len().into_or_panic()),
                        ])),
                    },
                    // Success.
                    BranchCost::Regular {
                        const_cost: ConstCost::steps(steps),
                        pre_cost: PreCost(OrderedHashMap::from_iter([
                            (CostTokenType::AddMod, info.add_offsets.len().into_or_panic()),
                            (CostTokenType::MulMod, info.mul_offsets.len().into_or_panic()),
                        ])),
                    },
                ]
            }
            CircuitConcreteLibfunc::GetOutput(_) => {
                vec![ConstCost::steps(5).into()]
            }
            CircuitConcreteLibfunc::TryIntoCircuitModulus(_) => {
                vec![ConstCost::steps(6).into(), ConstCost::steps(7).into()]
            }
            CircuitConcreteLibfunc::GetDescriptor(_) => {
                vec![ConstCost::steps(6).into()]
            }
            CircuitConcreteLibfunc::IntoU96Guarantee(_) => {
                vec![ConstCost::steps(0).into()]
            }
            CircuitConcreteLibfunc::U96GuaranteeVerify(_) => {
                vec![ConstCost { steps: 1, holes: 0, range_checks: 0, range_checks96: 1 }.into()]
            }
            CircuitConcreteLibfunc::U96LimbsLessThanGuaranteeVerify(_) => {
                vec![ConstCost::steps(2).into(), ConstCost::steps(2).into()]
            }
            CircuitConcreteLibfunc::U96SingleLimbLessThanGuaranteeVerify(_) => {
                vec![ConstCost::steps(0).into()]
            }
            CircuitConcreteLibfunc::InitCircuitData(libfunc) => {
                let info = info_provider.circuit_info(&libfunc.ty);
                vec![
                    ConstCost {
                        steps: 0,
                        holes: 0,
                        range_checks: 0,
                        range_checks96: info.rc96_usage().into_or_panic(),
                    }
                    .into(),
                ]
            }
            CircuitConcreteLibfunc::FailureGuaranteeVerify(_) => {
                // The libfunc costs 1 MulMod instance (in addition to the cost below).
                // However, in the failure case `eval_circuit` uses at least one MulMod gate less
                // than it actually costs, so the MulMod is already paid for.
                vec![ConstCost { steps: 32, holes: 0, range_checks: 0, range_checks96: 6 }.into()]
            }
        },
        IntRange(libfunc) => match libfunc {
            IntRangeConcreteLibfunc::TryNew(_) => {
                vec![
                    ConstCost { steps: 3, holes: 0, range_checks: 1, range_checks96: 0 }.into(),
                    ConstCost { steps: 5, holes: 0, range_checks: 1, range_checks96: 0 }.into(),
                ]
            }
            IntRangeConcreteLibfunc::PopFront(_) => {
                vec![ConstCost::steps(2).into(), ConstCost::steps(2).into()]
            }
        },
        // TODO(ilya): Add blake token to blake gas cost..
        Blake(_) => vec![ConstCost::steps(1).into()],
    }
}

/// Returns a postcost value for a libfunc - the cost of step token.
/// This is a helper function to implement costing both for creating
/// gas equations and getting actual gas cost after having a solution.
// TODO(lior): Remove this function once it's not used.
pub fn core_libfunc_postcost<Ops: CostOperations, InfoProvider: InvocationCostInfoProvider>(
    ops: &mut Ops,
    libfunc: &CoreConcreteLibfunc,
    info_provider: &InfoProvider,
) -> Vec<Ops::CostType> {
    let res = core_libfunc_cost(libfunc, info_provider);
    res.into_iter()
        .map(|cost| match cost {
            BranchCost::Regular { const_cost, pre_cost: _ } => ops.const_cost(const_cost),
            BranchCost::FunctionCost { const_cost, function, sign } => {
                let func_content_cost = ops.function_token_cost(&function, CostTokenType::Const);
                let cost = ops.add(ops.const_cost(const_cost), func_content_cost);

                match sign {
                    BranchCostSign::Add => {
                        // The refund may be at most `cost`. It can be smaller if the refund cannot
                        // be used later (e.g., if the next statement is `return`).
                        ops.sub(ops.statement_var_cost(CostTokenType::Const), cost)
                    }
                    BranchCostSign::Subtract => cost,
                }
            }
            BranchCost::BranchAlign => {
                let ap_change = info_provider.ap_change_var_value();
                let burnt_cost = ops.statement_var_cost(CostTokenType::Const);
                if ap_change == 0 {
                    burnt_cost
                } else {
                    ops.add(
                        burnt_cost,
                        ops.const_cost(ConstCost {
                            steps: 1,
                            holes: ap_change as i32,
                            range_checks: 0,
                            range_checks96: 0,
                        }),
                    )
                }
            }
            BranchCost::WithdrawGas(info) => {
                let total_cost = ops.const_cost(
                    info.const_cost(|token_type| info_provider.token_usages(token_type)),
                );
                if info.success {
                    ops.sub(total_cost, ops.statement_var_cost(CostTokenType::Const))
                } else {
                    total_cost
                }
            }
            BranchCost::RedepositGas => ops.add(
                ops.const_cost(ConstCost::steps(
                    BuiltinCostsType::cost_computation_steps(false, |token_type| {
                        info_provider.token_usages(token_type)
                    })
                    .into_or_panic(),
                )),
                ops.statement_var_cost(CostTokenType::Const),
            ),
        })
        .collect()
}

/// Returns a precost value for a libfunc - the cost of non-step tokens.
/// This is a helper function to implement costing both for creating
/// gas equations and getting actual gas cost after having a solution.
pub fn core_libfunc_precost<Ops: CostOperations, InfoProvider: CostInfoProvider>(
    ops: &mut Ops,
    libfunc: &CoreConcreteLibfunc,
    info_provider: &InfoProvider,
) -> Vec<Ops::CostType> {
    let res = core_libfunc_cost(libfunc, info_provider);

    res.into_iter()
        .map(|cost| match cost {
            BranchCost::Regular { const_cost: _, pre_cost } => {
                let mut res = ops.steps(0);
                for (token_type, val) in pre_cost.0 {
                    res = ops.add(res, ops.cost_token(val, token_type));
                }
                res
            }
            BranchCost::FunctionCost { const_cost: _, function, sign } => {
                let func_content_cost = CostTokenType::iter_precost()
                    .map(|token| ops.function_token_cost(&function, *token))
                    .collect_vec()
                    .into_iter()
                    .reduce(|x, y| ops.add(x, y));

                match sign {
                    BranchCostSign::Add => {
                        // The refund may be at most `cost`. It can be smaller if the refund cannot
                        // be used later (e.g., if the next statement is
                        // `return`).
                        ops.sub(
                            statement_vars_cost(ops, CostTokenType::iter_precost()),
                            func_content_cost.unwrap(),
                        )
                    }
                    BranchCostSign::Subtract => func_content_cost.unwrap(),
                }
            }
            BranchCost::BranchAlign | BranchCost::RedepositGas => {
                statement_vars_cost(ops, CostTokenType::iter_precost())
            }
            BranchCost::WithdrawGas(info) => {
                if info.success {
                    ops.sub(ops.steps(0), statement_vars_cost(ops, CostTokenType::iter_precost()))
                } else {
                    ops.steps(0)
                }
            }
        })
        .collect()
}

/// Returns the sum of statement variables for all the requested tokens.
fn statement_vars_cost<'a, Ops: CostOperations, TokenTypes: Iterator<Item = &'a CostTokenType>>(
    ops: &Ops,
    token_types: TokenTypes,
) -> Ops::CostType {
    token_types
        .map(|token_type| ops.statement_var_cost(*token_type))
        .reduce(|x, y| ops.add(x, y))
        .unwrap()
}

/// Returns costs for u64/u32/u16/u8 libfuncs.
fn uint_libfunc_cost<TUintTraits: UintTraits + IsZeroTraits + IntMulTraits>(
    libfunc: &UintConcrete<TUintTraits>,
) -> Vec<BranchCost> {
    match libfunc {
        UintConcrete::Const(_) | UintConcrete::ToFelt252(_) | UintConcrete::WideMul(_) => {
            vec![ConstCost::default().into()]
        }
        UintConcrete::Operation(libfunc) => match libfunc.operator {
            IntOperator::OverflowingAdd => {
                vec![
                    (ConstCost { steps: 4, holes: 0, range_checks: 1, range_checks96: 0 }).into(),
                    (ConstCost { steps: 5, holes: 0, range_checks: 1, range_checks96: 0 }).into(),
                ]
            }
            IntOperator::OverflowingSub => {
                vec![
                    (ConstCost { steps: 3, holes: 0, range_checks: 1, range_checks96: 0 }).into(),
                    (ConstCost { steps: 5, holes: 0, range_checks: 1, range_checks96: 0 }).into(),
                ]
            }
        },
        UintConcrete::SquareRoot(_) => {
            vec![(ConstCost { steps: 9, holes: 0, range_checks: 4, range_checks96: 0 }).into()]
        }
        UintConcrete::Equal(_) => {
            vec![ConstCost::steps(2).into(), ConstCost::steps(3).into()]
        }
        UintConcrete::FromFelt252(_) => {
            vec![
                (ConstCost { steps: 4, holes: 0, range_checks: 2, range_checks96: 0 }).into(),
                (ConstCost { steps: 10, holes: 0, range_checks: 3, range_checks96: 0 }).into(),
            ]
        }
        UintConcrete::IsZero(_) => vec![ConstCost::steps(1).into(), ConstCost::steps(1).into()],
        UintConcrete::Divmod(_) => {
            vec![BranchCost::from(ConstCost {
                steps: 7,
                holes: 0,
                range_checks: 3,
                range_checks96: 0,
            })]
        }
        UintConcrete::Bitwise(_) => {
            vec![BranchCost::Regular {
                const_cost: ConstCost::steps(2),
                pre_cost: PreCost::builtin(CostTokenType::Bitwise),
            }]
        }
    }
}

/// Returns costs for u128 libfuncs.
fn u128_libfunc_cost(libfunc: &Uint128Concrete) -> Vec<BranchCost> {
    match libfunc {
        Uint128Concrete::Operation(libfunc) => match libfunc.operator {
            IntOperator::OverflowingAdd | IntOperator::OverflowingSub => {
                vec![
                    ConstCost { steps: 3, holes: 0, range_checks: 1, range_checks96: 0 }.into(),
                    ConstCost { steps: 5, holes: 0, range_checks: 1, range_checks96: 0 }.into(),
                ]
            }
        },
        Uint128Concrete::Divmod(_) => {
            vec![ConstCost { steps: 11, holes: 0, range_checks: 4, range_checks96: 0 }.into()]
        }
        Uint128Concrete::GuaranteeMul(_) => {
            vec![ConstCost::steps(1).into()]
        }
        Uint128Concrete::MulGuaranteeVerify(_) => {
            vec![ConstCost { steps: 23, holes: 0, range_checks: 9, range_checks96: 0 }.into()]
        }
        Uint128Concrete::Const(_) | Uint128Concrete::ToFelt252(_) => {
            vec![ConstCost::default().into()]
        }
        Uint128Concrete::FromFelt252(_) => {
            vec![
                ConstCost { steps: 2, holes: 0, range_checks: 1, range_checks96: 0 }.into(),
                ConstCost { steps: 11, holes: 0, range_checks: 3, range_checks96: 0 }.into(),
            ]
        }
        Uint128Concrete::IsZero(_) => {
            vec![ConstCost::steps(1).into(), ConstCost::steps(1).into()]
        }
        Uint128Concrete::Equal(_) => {
            vec![ConstCost::steps(2).into(), ConstCost::steps(3).into()]
        }
        Uint128Concrete::SquareRoot(_) => {
            vec![ConstCost { steps: 9, holes: 0, range_checks: 4, range_checks96: 0 }.into()]
        }
        Uint128Concrete::Bitwise(_) => {
            vec![BranchCost::Regular {
                const_cost: ConstCost::steps(2),
                pre_cost: PreCost::builtin(CostTokenType::Bitwise),
            }]
        }
        Uint128Concrete::ByteReverse(_) => vec![BranchCost::Regular {
            const_cost: ConstCost::steps(24),
            pre_cost: PreCost(OrderedHashMap::from_iter([(CostTokenType::Bitwise, 4)])),
        }],
    }
}

/// Returns costs for u256 libfuncs.
fn u256_libfunc_cost(libfunc: &Uint256Concrete) -> Vec<ConstCost> {
    let steps = |value| ConstCost { steps: value, ..Default::default() };
    match libfunc {
        Uint256Concrete::IsZero(_) => {
            vec![steps(2), steps(2)]
        }
        Uint256Concrete::Divmod(_) => {
            vec![ConstCost { steps: 26, holes: 0, range_checks: 6, range_checks96: 0 }]
        }
        Uint256Concrete::SquareRoot(_) => {
            vec![ConstCost { steps: 30, holes: 0, range_checks: 7, range_checks96: 0 }]
        }
        Uint256Concrete::InvModN(_) => {
            vec![
                ConstCost { steps: 40, holes: 0, range_checks: 9, range_checks96: 0 },
                ConstCost { steps: 25, holes: 0, range_checks: 7, range_checks96: 0 },
            ]
        }
    }
}

/// Returns costs for u512 libfuncs.
fn u512_libfunc_cost(libfunc: &Uint512Concrete) -> Vec<ConstCost> {
    match libfunc {
        Uint512Concrete::DivModU256(_) => {
            vec![ConstCost { steps: 47, holes: 0, range_checks: 12, range_checks96: 0 }]
        }
    }
}

/// Returns costs for i64/i32/i16/i8 libfuncs.
fn sint_libfunc_cost<TSintTraits: SintTraits + IsZeroTraits + IntMulTraits>(
    libfunc: &SintConcrete<TSintTraits>,
) -> Vec<BranchCost> {
    match libfunc {
        SintConcrete::Const(_) | SintConcrete::ToFelt252(_) | SintConcrete::WideMul(_) => {
            vec![ConstCost::steps(0).into()]
        }
        SintConcrete::Equal(_) => {
            vec![ConstCost::steps(2).into(), ConstCost::steps(3).into()]
        }
        SintConcrete::FromFelt252(_) => {
            vec![
                ConstCost { steps: 5, holes: 0, range_checks: 2, range_checks96: 0 }.into(),
                ConstCost { steps: 10, holes: 0, range_checks: 3, range_checks96: 0 }.into(),
            ]
        }
        SintConcrete::IsZero(_) => vec![ConstCost::steps(1).into(), ConstCost::steps(1).into()],
        SintConcrete::Operation(_) => vec![
            ConstCost { steps: 6, holes: 0, range_checks: 2, range_checks96: 0 }.into(),
            ConstCost { steps: 6, holes: 0, range_checks: 1, range_checks96: 0 }.into(),
            ConstCost { steps: 6, holes: 0, range_checks: 1, range_checks96: 0 }.into(),
        ],
        SintConcrete::Diff(_) => vec![
            (ConstCost { steps: 3, holes: 0, range_checks: 1, range_checks96: 0 }).into(),
            (ConstCost { steps: 5, holes: 0, range_checks: 1, range_checks96: 0 }).into(),
        ],
    }
}

/// Returns costs for i128 libfuncs.
fn s128_libfunc_cost(libfunc: &Sint128Concrete) -> Vec<BranchCost> {
    let steps = |value| ConstCost { steps: value, ..Default::default() };
    match libfunc {
        Sint128Concrete::Const(_) | Sint128Concrete::ToFelt252(_) => {
            vec![ConstCost::default().into()]
        }
        Sint128Concrete::FromFelt252(_) => {
            vec![
                ConstCost { steps: 3, holes: 0, range_checks: 1, range_checks96: 0 }.into(),
                ConstCost { steps: 10, holes: 0, range_checks: 3, range_checks96: 0 }.into(),
            ]
        }
        Sint128Concrete::IsZero(_) => {
            vec![steps(1).into(), steps(1).into()]
        }
        Sint128Concrete::Equal(_) => {
            vec![steps(2).into(), steps(3).into()]
        }
        Sint128Concrete::Operation(_) => vec![
            ConstCost { steps: 4, holes: 0, range_checks: 1, range_checks96: 0 }.into(),
            ConstCost { steps: 6, holes: 0, range_checks: 1, range_checks96: 0 }.into(),
            ConstCost { steps: 6, holes: 0, range_checks: 1, range_checks96: 0 }.into(),
        ],
        Sint128Concrete::Diff(_) => vec![
            ConstCost { steps: 3, holes: 0, range_checks: 1, range_checks96: 0 }.into(),
            ConstCost { steps: 5, holes: 0, range_checks: 1, range_checks96: 0 }.into(),
        ],
    }
}

/// Returns costs for felt252 libfuncs.
fn felt252_libfunc_cost(libfunc: &Felt252Concrete) -> Vec<ConstCost> {
    match libfunc {
        Felt252Concrete::BinaryOperation(bin_op) => {
            let op = match bin_op {
                Felt252BinaryOperationConcrete::WithVar(op) => op.operator,
                Felt252BinaryOperationConcrete::WithConst(op) => op.operator,
            };
            if op == Felt252BinaryOperator::Div {
                vec![ConstCost::steps(5)]
            } else {
                vec![ConstCost::default()]
            }
        }
        Felt252Concrete::Const(_) => vec![ConstCost::default()],
        Felt252Concrete::IsZero(_) => {
            vec![ConstCost::steps(1), ConstCost::steps(1)]
        }
    }
}
