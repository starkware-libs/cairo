use cairo_lang_sierra::extensions::ap_tracking::ApTrackingConcreteLibfunc;
use cairo_lang_sierra::extensions::array::ArrayConcreteLibfunc;
use cairo_lang_sierra::extensions::boolean::BoolConcreteLibfunc;
use cairo_lang_sierra::extensions::boxing::BoxConcreteLibfunc;
use cairo_lang_sierra::extensions::bytes31::Bytes31ConcreteLibfunc;
use cairo_lang_sierra::extensions::casts::{CastConcreteLibfunc, CastType};
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
use cairo_lang_sierra::extensions::gas::{BuiltinCostsType, CostTokenType, GasConcreteLibfunc};
use cairo_lang_sierra::extensions::int::signed::{SintConcrete, SintTraits};
use cairo_lang_sierra::extensions::int::signed128::Sint128Concrete;
use cairo_lang_sierra::extensions::int::unsigned::{UintConcrete, UintTraits};
use cairo_lang_sierra::extensions::int::unsigned128::Uint128Concrete;
use cairo_lang_sierra::extensions::int::unsigned256::Uint256Concrete;
use cairo_lang_sierra::extensions::int::unsigned512::Uint512Concrete;
use cairo_lang_sierra::extensions::int::{IntMulTraits, IntOperator};
use cairo_lang_sierra::extensions::is_zero::IsZeroTraits;
use cairo_lang_sierra::extensions::mem::MemConcreteLibfunc;
use cairo_lang_sierra::extensions::nullable::NullableConcreteLibfunc;
use cairo_lang_sierra::extensions::pedersen::PedersenConcreteLibfunc;
use cairo_lang_sierra::extensions::poseidon::PoseidonConcreteLibfunc;
use cairo_lang_sierra::extensions::starknet::testing::TestingConcreteLibfunc;
use cairo_lang_sierra::extensions::starknet::StarkNetConcreteLibfunc;
use cairo_lang_sierra::extensions::structure::StructConcreteLibfunc;
use cairo_lang_sierra::ids::ConcreteTypeId;
use num_traits::Zero;

use crate::ApChange;

/// Trait for providing extra information required for AP changes for a specific libfunc invocation.
pub trait InvocationApChangeInfoProvider {
    /// Provides the sizes of types.
    fn type_size(&self, ty: &ConcreteTypeId) -> usize;
    /// Number of tokens provided by the libfunc invocation (currently only relevant for
    /// `withdraw_gas_all`).
    fn token_usages(&self, token_type: CostTokenType) -> usize;
}

/// Returns the ap change for a core libfunc.
/// Values with unknown values will return as None.
pub fn core_libfunc_ap_change<InfoProvider: InvocationApChangeInfoProvider>(
    libfunc: &CoreConcreteLibfunc,
    info_provider: &InfoProvider,
) -> Vec<ApChange> {
    match libfunc {
        ApTracking(ApTrackingConcreteLibfunc::Revoke(_)) => {
            vec![ApChange::Unknown]
        }
        ApTracking(ApTrackingConcreteLibfunc::Enable(_)) => {
            vec![ApChange::EnableApTracking]
        }
        ApTracking(ApTrackingConcreteLibfunc::Disable(_)) => {
            vec![ApChange::DisableApTracking]
        }
        Array(libfunc) => match libfunc {
            ArrayConcreteLibfunc::New(_) => vec![ApChange::Known(1)],
            ArrayConcreteLibfunc::SpanFromTuple(_) => vec![ApChange::Known(0)],
            ArrayConcreteLibfunc::Append(_) => vec![ApChange::Known(0)],
            ArrayConcreteLibfunc::PopFront(_)
            | ArrayConcreteLibfunc::PopFrontConsume(_)
            | ArrayConcreteLibfunc::SnapshotPopFront(_)
            | ArrayConcreteLibfunc::SnapshotPopBack(_) => {
                vec![ApChange::Known(1), ApChange::Known(1)]
            }
            ArrayConcreteLibfunc::Get(libfunc) => {
                if info_provider.type_size(&libfunc.ty) == 1 { [4, 3] } else { [5, 4] }
                    .map(ApChange::Known)
                    .to_vec()
            }
            ArrayConcreteLibfunc::Slice(libfunc) => {
                if info_provider.type_size(&libfunc.ty) == 1 { [4, 5] } else { [6, 6] }
                    .map(ApChange::Known)
                    .to_vec()
            }
            ArrayConcreteLibfunc::Len(libfunc) => {
                vec![ApChange::Known(if info_provider.type_size(&libfunc.ty) == 1 { 0 } else { 1 })]
            }
        },
        BranchAlign(_) => vec![ApChange::FromMetadata],
        Bool(libfunc) => match libfunc {
            BoolConcreteLibfunc::And(_) => vec![ApChange::Known(0)],
            BoolConcreteLibfunc::Not(_) => vec![ApChange::Known(1)],
            BoolConcreteLibfunc::Xor(_) => vec![ApChange::Known(1)],
            BoolConcreteLibfunc::Or(_) => vec![ApChange::Known(2)],
            BoolConcreteLibfunc::ToFelt252(_) => vec![ApChange::Known(0)],
        },
        Box(libfunc) => match libfunc {
            BoxConcreteLibfunc::Into(_) => vec![ApChange::Known(1)],
            BoxConcreteLibfunc::Unbox(_) => vec![ApChange::Known(0)],
            BoxConcreteLibfunc::ForwardSnapshot(_) => vec![ApChange::Known(0)],
        },
        Cast(libfunc) => match libfunc {
            CastConcreteLibfunc::Downcast(libfunc) => {
                if libfunc.from_range.is_full_felt252_range() {
                    let success_extra_steps = if libfunc.to_range.lower.is_zero() { 0 } else { 1 }
                        + if &libfunc.to_range.upper - 1 == u128::MAX.into() { 0 } else { 1 };
                    let failure_extra_steps = if libfunc.to_range.upper.is_zero() { 0 } else { 1 };
                    vec![
                        ApChange::Known(success_extra_steps + 1),
                        ApChange::Known(failure_extra_steps + 6),
                    ]
                } else {
                    // Overflow tests are more expensive when asserting a value is above non-zero
                    // value.
                    let extra_below = if libfunc.to_range.lower.is_zero() { 0 } else { 1 };
                    let extra_above = if libfunc.to_range.upper.is_zero() { 0 } else { 1 };
                    match libfunc.cast_type() {
                        CastType { overflow_above: false, overflow_below: false } => {
                            vec![ApChange::Known(0), ApChange::Known(0)]
                        }
                        CastType { overflow_above: true, overflow_below: false } => {
                            vec![ApChange::Known(2), ApChange::Known(1 + extra_above)]
                        }
                        CastType { overflow_above: false, overflow_below: true } => {
                            vec![ApChange::Known(1 + extra_below), ApChange::Known(2)]
                        }
                        CastType { overflow_above: true, overflow_below: true } => {
                            vec![ApChange::Known(2 + extra_below), ApChange::Known(3)]
                        }
                    }
                }
            }
            CastConcreteLibfunc::Upcast(_) => vec![ApChange::Known(0)],
        },
        Ec(libfunc) => match libfunc {
            EcConcreteLibfunc::IsZero(_) => vec![ApChange::Known(0), ApChange::Known(0)],
            EcConcreteLibfunc::Neg(_) => vec![ApChange::Known(0)],
            EcConcreteLibfunc::StateAdd(_) => vec![ApChange::Known(9)],
            EcConcreteLibfunc::TryNew(_) => vec![ApChange::Known(6), ApChange::Known(6)],
            EcConcreteLibfunc::StateFinalize(_) => vec![ApChange::Known(11), ApChange::Known(3)],
            EcConcreteLibfunc::StateInit(_) => vec![ApChange::Known(7)],
            EcConcreteLibfunc::StateAddMul(_) => vec![ApChange::Known(0)],
            EcConcreteLibfunc::PointFromX(_) => vec![ApChange::Known(11), ApChange::Known(7)],
            EcConcreteLibfunc::UnwrapPoint(_) => vec![ApChange::Known(0)],
            EcConcreteLibfunc::Zero(_) => vec![ApChange::Known(0)],
        },
        Drop(_) | Dup(_) => vec![ApChange::Known(0)],
        Felt252(libfunc) => match libfunc {
            Felt252Concrete::Const(_) => {
                vec![ApChange::Known(0)]
            }
            Felt252Concrete::BinaryOperation(bin_op) => {
                let op = match bin_op {
                    Felt252BinaryOperationConcrete::WithVar(op) => op.operator,
                    Felt252BinaryOperationConcrete::WithConst(op) => op.operator,
                };
                vec![ApChange::Known(if op == Felt252BinaryOperator::Div { 1 } else { 0 })]
            }
            Felt252Concrete::IsZero(_) => vec![ApChange::Known(0), ApChange::Known(0)],
        },
        FunctionCall(libfunc) | CouponCall(libfunc) => {
            vec![ApChange::FunctionCall(libfunc.function.id.clone())]
        }
        Gas(libfunc) => match libfunc {
            GasConcreteLibfunc::WithdrawGas(_) => {
                let cost_computation_ap_change: usize =
                    BuiltinCostsType::cost_computation_steps(false, |token_type| {
                        info_provider.token_usages(token_type)
                    });
                if cost_computation_ap_change == 0 {
                    vec![ApChange::Known(2), ApChange::Known(2)]
                } else {
                    vec![
                        ApChange::Known(cost_computation_ap_change + 2),
                        ApChange::Known(cost_computation_ap_change + 3),
                    ]
                }
            }
            GasConcreteLibfunc::RedepositGas(_) => {
                vec![ApChange::Known(BuiltinCostsType::cost_computation_steps(
                    false,
                    |token_type| info_provider.token_usages(token_type),
                ))]
            }
            GasConcreteLibfunc::GetAvailableGas(_) => vec![ApChange::Known(0)],
            GasConcreteLibfunc::BuiltinWithdrawGas(_) => {
                let cost_computation_ap_change: usize =
                    BuiltinCostsType::cost_computation_steps(true, |token_type| {
                        info_provider.token_usages(token_type)
                    });
                vec![
                    ApChange::Known(cost_computation_ap_change + 2),
                    ApChange::Known(cost_computation_ap_change + 3),
                ]
            }
            GasConcreteLibfunc::GetBuiltinCosts(_) => vec![ApChange::Known(3)],
        },
        Uint8(libfunc) => uint_ap_change(libfunc),
        Uint16(libfunc) => uint_ap_change(libfunc),
        Uint32(libfunc) => uint_ap_change(libfunc),
        Uint64(libfunc) => uint_ap_change(libfunc),
        Uint128(libfunc) => match libfunc {
            Uint128Concrete::Operation(libfunc) => match libfunc.operator {
                IntOperator::OverflowingAdd | IntOperator::OverflowingSub => {
                    vec![ApChange::Known(2), ApChange::Known(3)]
                }
            },
            Uint128Concrete::Divmod(_) => vec![ApChange::Known(7)],
            Uint128Concrete::GuaranteeMul(_) => vec![ApChange::Known(2)],
            Uint128Concrete::MulGuaranteeVerify(_) => vec![ApChange::Known(15)],
            Uint128Concrete::Equal(_) => vec![ApChange::Known(1), ApChange::Known(1)],
            Uint128Concrete::SquareRoot(_) => vec![ApChange::Known(6)],
            Uint128Concrete::FromFelt252(_) => vec![ApChange::Known(1), ApChange::Known(6)],
            Uint128Concrete::Const(_) | Uint128Concrete::ToFelt252(_) => {
                vec![ApChange::Known(0)]
            }
            Uint128Concrete::IsZero(_) => vec![ApChange::Known(0), ApChange::Known(0)],
            Uint128Concrete::ByteReverse(_) => vec![ApChange::Known(16)],
            Uint128Concrete::Bitwise(_) => vec![ApChange::Known(0)],
        },
        Uint256(libfunc) => match libfunc {
            Uint256Concrete::IsZero(_) => vec![ApChange::Known(0), ApChange::Known(0)],
            Uint256Concrete::Divmod(_) => vec![ApChange::Known(19)],
            Uint256Concrete::SquareRoot(_) => vec![ApChange::Known(25)],
            Uint256Concrete::InvModN(_) => vec![ApChange::Known(46), ApChange::Known(14)],
        },
        Uint512(libfunc) => match libfunc {
            Uint512Concrete::DivModU256(_) => vec![ApChange::Known(43)],
        },
        Sint8(libfunc) => sint_ap_change(libfunc),
        Sint16(libfunc) => sint_ap_change(libfunc),
        Sint32(libfunc) => sint_ap_change(libfunc),
        Sint64(libfunc) => sint_ap_change(libfunc),
        Sint128(libfunc) => match libfunc {
            Sint128Concrete::Equal(_) => vec![ApChange::Known(1), ApChange::Known(1)],
            Sint128Concrete::FromFelt252(_) => vec![ApChange::Known(2), ApChange::Known(7)],
            Sint128Concrete::Const(_) | Sint128Concrete::ToFelt252(_) => {
                vec![ApChange::Known(0)]
            }
            Sint128Concrete::IsZero(_) => vec![ApChange::Known(0), ApChange::Known(0)],
            Sint128Concrete::Operation(_) => {
                vec![ApChange::Known(3), ApChange::Known(4), ApChange::Known(4)]
            }
            Sint128Concrete::Diff(_) => vec![ApChange::Known(2), ApChange::Known(3)],
        },
        Mem(libfunc) => match libfunc {
            MemConcreteLibfunc::StoreTemp(libfunc) => {
                vec![ApChange::Known(info_provider.type_size(&libfunc.ty))]
            }
            MemConcreteLibfunc::StoreLocal(_) => vec![ApChange::Known(0)],
            MemConcreteLibfunc::FinalizeLocals(_) => vec![ApChange::FinalizeLocals],
            MemConcreteLibfunc::AllocLocal(libfunc) => {
                vec![ApChange::AtLocalsFinalization(info_provider.type_size(&libfunc.ty))]
            }
            MemConcreteLibfunc::Rename(_) => {
                vec![ApChange::Known(0)]
            }
        },
        UnwrapNonZero(_) => vec![ApChange::Known(0)],
        UnconditionalJump(_) => vec![ApChange::Known(0)],
        Enum(libfunc) => match libfunc {
            EnumConcreteLibfunc::Init(_) => vec![ApChange::Known(0)],
            EnumConcreteLibfunc::FromBoundedInt(libfunc) => match libfunc.n_variants {
                1 | 2 => vec![ApChange::Known(0)],
                _ => vec![ApChange::Known(1)],
            },
            EnumConcreteLibfunc::Match(libfunc) | EnumConcreteLibfunc::SnapshotMatch(libfunc) => {
                vec![ApChange::Known(0); libfunc.signature.branch_signatures.len()]
            }
        },
        Struct(libfunc) => match libfunc {
            StructConcreteLibfunc::Construct(_)
            | StructConcreteLibfunc::Deconstruct(_)
            | StructConcreteLibfunc::SnapshotDeconstruct(_) => {
                vec![ApChange::Known(0)]
            }
        },
        Felt252Dict(libfunc) => match libfunc {
            Felt252DictConcreteLibfunc::New(_) => vec![ApChange::Known(6)],
            Felt252DictConcreteLibfunc::Squash(_) => vec![ApChange::Unknown],
        },
        Pedersen(libfunc) => match libfunc {
            PedersenConcreteLibfunc::PedersenHash(_) => vec![ApChange::Known(0)],
        },
        Poseidon(libfunc) => match libfunc {
            PoseidonConcreteLibfunc::HadesPermutation(_) => vec![ApChange::Known(0)],
        },
        StarkNet(libfunc) => match libfunc {
            StarkNetConcreteLibfunc::ClassHashConst(_)
            | StarkNetConcreteLibfunc::ContractAddressConst(_) => vec![ApChange::Known(0)],

            StarkNetConcreteLibfunc::ClassHashTryFromFelt252(_)
            | StarkNetConcreteLibfunc::ContractAddressTryFromFelt252(_)
            | StarkNetConcreteLibfunc::StorageAddressTryFromFelt252(_) => {
                vec![ApChange::Known(5), ApChange::Known(6)]
            }
            StarkNetConcreteLibfunc::ClassHashToFelt252(_)
            | StarkNetConcreteLibfunc::ContractAddressToFelt252(_)
            | StarkNetConcreteLibfunc::StorageAddressToFelt252(_) => vec![ApChange::Known(0)],
            StarkNetConcreteLibfunc::StorageBaseAddressConst(_) => vec![ApChange::Known(0)],
            StarkNetConcreteLibfunc::StorageBaseAddressFromFelt252(_) => {
                vec![ApChange::Known(7)]
            }
            StarkNetConcreteLibfunc::StorageAddressFromBase(_) => vec![ApChange::Known(0)],
            StarkNetConcreteLibfunc::StorageAddressFromBaseAndOffset(_) => vec![ApChange::Known(0)],
            StarkNetConcreteLibfunc::CallContract(_)
            | StarkNetConcreteLibfunc::StorageRead(_)
            | StarkNetConcreteLibfunc::StorageWrite(_)
            | StarkNetConcreteLibfunc::EmitEvent(_)
            | StarkNetConcreteLibfunc::GetBlockHash(_)
            | StarkNetConcreteLibfunc::GetExecutionInfo(_)
            | StarkNetConcreteLibfunc::GetExecutionInfoV2(_)
            | StarkNetConcreteLibfunc::Deploy(_)
            | StarkNetConcreteLibfunc::Keccak(_)
            | StarkNetConcreteLibfunc::SHA256ProcessBlock(_)
            | StarkNetConcreteLibfunc::LibraryCall(_)
            | StarkNetConcreteLibfunc::ReplaceClass(_)
            | StarkNetConcreteLibfunc::SendMessageToL1(_)
            | StarkNetConcreteLibfunc::Secp256(_) => {
                vec![ApChange::Known(2), ApChange::Known(2)]
            }
            StarkNetConcreteLibfunc::Testing(libfunc) => match libfunc {
                TestingConcreteLibfunc::Cheatcode(_) => vec![ApChange::Known(2)],
            },
            StarkNetConcreteLibfunc::SHA256StateHandleInit(_) => vec![ApChange::Known(0)],
            StarkNetConcreteLibfunc::SHA256StateHandleDigest(_) => vec![ApChange::Known(0)],
        },
        Nullable(libfunc) => match libfunc {
            NullableConcreteLibfunc::Null(_)
            | NullableConcreteLibfunc::NullableFromBox(_)
            | NullableConcreteLibfunc::ForwardSnapshot(_) => vec![ApChange::Known(0)],
            NullableConcreteLibfunc::MatchNullable(_) => {
                vec![ApChange::Known(0), ApChange::Known(0)]
            }
        },
        Debug(_) => vec![ApChange::Known(0)],
        SnapshotTake(_) => vec![ApChange::Known(0)],
        Felt252DictEntry(libfunc) => match libfunc {
            Felt252DictEntryConcreteLibfunc::Get(_) => vec![ApChange::Known(0)],
            Felt252DictEntryConcreteLibfunc::Finalize(_) => vec![ApChange::Known(0)],
        },
        Bytes31(libfunc) => match libfunc {
            Bytes31ConcreteLibfunc::Const(_) | Bytes31ConcreteLibfunc::ToFelt252(_) => {
                vec![ApChange::Known(0)]
            }
            Bytes31ConcreteLibfunc::TryFromFelt252(_) => {
                vec![ApChange::Known(5), ApChange::Known(6)]
            }
        },
        Const(libfunc) => match libfunc {
            ConstConcreteLibfunc::AsBox(_) => vec![ApChange::Known(3)],
            ConstConcreteLibfunc::AsImmediate(_) => vec![ApChange::Known(0)],
        },
        Coupon(libfunc) => match libfunc {
            CouponConcreteLibfunc::Buy(_) => vec![ApChange::Known(0)],
            CouponConcreteLibfunc::Refund(_) => vec![ApChange::Known(0)],
        },
    }
}

/// Returns the ap changes for u8/u16/u32/u64 libfuncs.
fn uint_ap_change<TUintTraits: UintTraits + IntMulTraits + IsZeroTraits>(
    libfunc: &UintConcrete<TUintTraits>,
) -> Vec<ApChange> {
    match libfunc {
        UintConcrete::Const(_) | UintConcrete::ToFelt252(_) => vec![ApChange::Known(0)],
        UintConcrete::Operation(libfunc) => match libfunc.operator {
            IntOperator::OverflowingAdd => {
                vec![ApChange::Known(3), ApChange::Known(3)]
            }
            IntOperator::OverflowingSub => {
                vec![ApChange::Known(2), ApChange::Known(3)]
            }
        },
        UintConcrete::SquareRoot(_) => vec![ApChange::Known(6)],
        UintConcrete::Equal(_) => vec![ApChange::Known(1), ApChange::Known(1)],
        UintConcrete::FromFelt252(_) => vec![ApChange::Known(2), ApChange::Known(7)],
        UintConcrete::IsZero(_) => vec![ApChange::Known(0), ApChange::Known(0)],
        UintConcrete::Divmod(_) => vec![ApChange::Known(5)],
        UintConcrete::WideMul(_) => vec![ApChange::Known(0)],
        UintConcrete::Bitwise(_) => vec![ApChange::Known(0)],
    }
}

/// Returns the ap changes for s8/s16/s32/s64 libfuncs.
fn sint_ap_change<TSintTraits: SintTraits + IntMulTraits + IsZeroTraits>(
    libfunc: &SintConcrete<TSintTraits>,
) -> Vec<ApChange> {
    match libfunc {
        SintConcrete::Const(_) | SintConcrete::ToFelt252(_) => vec![ApChange::Known(0)],
        SintConcrete::Equal(_) => vec![ApChange::Known(1), ApChange::Known(1)],
        SintConcrete::FromFelt252(_) => vec![ApChange::Known(3), ApChange::Known(7)],
        SintConcrete::IsZero(_) => vec![ApChange::Known(0), ApChange::Known(0)],
        SintConcrete::WideMul(_) => vec![ApChange::Known(0)],
        SintConcrete::Operation(_) => {
            vec![ApChange::Known(4), ApChange::Known(4), ApChange::Known(4)]
        }
        SintConcrete::Diff(_) => vec![ApChange::Known(2), ApChange::Known(3)],
    }
}
