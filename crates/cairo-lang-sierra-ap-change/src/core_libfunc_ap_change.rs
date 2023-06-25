use cairo_lang_sierra::extensions::ap_tracking::ApTrackingConcreteLibfunc;
use cairo_lang_sierra::extensions::array::ArrayConcreteLibfunc;
use cairo_lang_sierra::extensions::boolean::BoolConcreteLibfunc;
use cairo_lang_sierra::extensions::boxing::BoxConcreteLibfunc;
use cairo_lang_sierra::extensions::bytes31::Bytes31ConcreteLibfunc;
use cairo_lang_sierra::extensions::casts::CastConcreteLibfunc;
use cairo_lang_sierra::extensions::core::CoreConcreteLibfunc;
use cairo_lang_sierra::extensions::ec::EcConcreteLibfunc;
use cairo_lang_sierra::extensions::enm::EnumConcreteLibfunc;
use cairo_lang_sierra::extensions::felt252::{
    Felt252BinaryOperationConcrete, Felt252BinaryOperator, Felt252Concrete,
};
use cairo_lang_sierra::extensions::felt252_dict::{
    Felt252DictConcreteLibfunc, Felt252DictEntryConcreteLibfunc,
};
use cairo_lang_sierra::extensions::gas::{
    BuiltinCostWithdrawGasLibfunc, CostTokenType, GasConcreteLibfunc,
};
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
        CoreConcreteLibfunc::ApTracking(ApTrackingConcreteLibfunc::Revoke(_)) => {
            vec![ApChange::Unknown]
        }
        CoreConcreteLibfunc::ApTracking(ApTrackingConcreteLibfunc::Enable(_)) => {
            vec![ApChange::EnableApTracking]
        }
        CoreConcreteLibfunc::ApTracking(ApTrackingConcreteLibfunc::Disable(_)) => {
            vec![ApChange::DisableApTracking]
        }
        CoreConcreteLibfunc::Array(libfunc) => match libfunc {
            ArrayConcreteLibfunc::New(_) => vec![ApChange::Known(1)],
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
        CoreConcreteLibfunc::BranchAlign(_) => vec![ApChange::FromMetadata],
        CoreConcreteLibfunc::Bool(libfunc) => match libfunc {
            BoolConcreteLibfunc::And(_) => vec![ApChange::Known(0)],
            BoolConcreteLibfunc::Not(_) => vec![ApChange::Known(1)],
            BoolConcreteLibfunc::Xor(_) => vec![ApChange::Known(1)],
            BoolConcreteLibfunc::Or(_) => vec![ApChange::Known(2)],
            BoolConcreteLibfunc::ToFelt252(_) => vec![ApChange::Known(0)],
        },
        CoreConcreteLibfunc::Box(libfunc) => match libfunc {
            BoxConcreteLibfunc::Into(_) => vec![ApChange::Known(1)],
            BoxConcreteLibfunc::Unbox(_) => vec![ApChange::Known(0)],
        },
        CoreConcreteLibfunc::Cast(libfunc) => match libfunc {
            CastConcreteLibfunc::Downcast(libfunc) => {
                let can_underflow = libfunc.from_info.signed;
                if !can_underflow {
                    vec![ApChange::Known(2), ApChange::Known(2)]
                } else {
                    let can_overflow = libfunc.from_info.nbits > libfunc.to_info.nbits
                        || (libfunc.from_info.nbits == libfunc.to_info.nbits
                            && libfunc.to_info.signed);
                    // Underflow test is more expensive for casting into signed types.
                    let mut success_ap_change = if libfunc.to_info.signed { 2 } else { 1 };
                    success_ap_change += if can_overflow { 1 } else { 0 };
                    // If overflow is possible, an additional non-deterministic jump is required.
                    let failure_ap_change = if can_overflow { 3 } else { 2 };
                    vec![ApChange::Known(success_ap_change), ApChange::Known(failure_ap_change)]
                }
            }
            CastConcreteLibfunc::Upcast(_) => vec![ApChange::Known(0)],
        },
        CoreConcreteLibfunc::Ec(libfunc) => match libfunc {
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
        CoreConcreteLibfunc::Drop(_) | CoreConcreteLibfunc::Dup(_) => vec![ApChange::Known(0)],
        CoreConcreteLibfunc::Felt252(libfunc) => match libfunc {
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
        CoreConcreteLibfunc::FunctionCall(libfunc) => {
            vec![ApChange::FunctionCall(libfunc.function.id.clone())]
        }
        CoreConcreteLibfunc::Gas(libfunc) => match libfunc {
            GasConcreteLibfunc::WithdrawGas(_) => vec![ApChange::Known(2), ApChange::Known(2)],
            GasConcreteLibfunc::RedepositGas(_) => vec![ApChange::Known(0)],
            GasConcreteLibfunc::GetAvailableGas(_) => vec![ApChange::Known(0)],
            GasConcreteLibfunc::BuiltinWithdrawGas(_) => {
                let cost_computation_ap_change: usize =
                    BuiltinCostWithdrawGasLibfunc::cost_computation_steps(|token_type| {
                        info_provider.token_usages(token_type)
                    });
                vec![
                    ApChange::Known(cost_computation_ap_change + 2),
                    ApChange::Known(cost_computation_ap_change + 3),
                ]
            }
            GasConcreteLibfunc::GetBuiltinCosts(_) => vec![ApChange::Known(3)],
        },
        CoreConcreteLibfunc::Uint8(libfunc) => uint_ap_change(libfunc),
        CoreConcreteLibfunc::Uint16(libfunc) => uint_ap_change(libfunc),
        CoreConcreteLibfunc::Uint32(libfunc) => uint_ap_change(libfunc),
        CoreConcreteLibfunc::Uint64(libfunc) => uint_ap_change(libfunc),
        CoreConcreteLibfunc::Uint128(libfunc) => match libfunc {
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
        CoreConcreteLibfunc::Uint256(libfunc) => match libfunc {
            Uint256Concrete::IsZero(_) => vec![ApChange::Known(0), ApChange::Known(0)],
            Uint256Concrete::Divmod(_) => vec![ApChange::Known(19)],
            Uint256Concrete::SquareRoot(_) => vec![ApChange::Known(25)],
        },
        CoreConcreteLibfunc::Uint512(libfunc) => match libfunc {
            Uint512Concrete::DivModU256(_) => vec![ApChange::Known(43)],
        },
        CoreConcreteLibfunc::Sint8(libfunc) => sint_ap_change(libfunc),
        CoreConcreteLibfunc::Sint16(libfunc) => sint_ap_change(libfunc),
        CoreConcreteLibfunc::Sint32(libfunc) => sint_ap_change(libfunc),
        CoreConcreteLibfunc::Sint64(libfunc) => sint_ap_change(libfunc),
        CoreConcreteLibfunc::Sint128(libfunc) => match libfunc {
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
        CoreConcreteLibfunc::Mem(libfunc) => match libfunc {
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
        CoreConcreteLibfunc::UnwrapNonZero(_) => vec![ApChange::Known(0)],
        CoreConcreteLibfunc::UnconditionalJump(_) => vec![ApChange::Known(0)],
        CoreConcreteLibfunc::Enum(libfunc) => match libfunc {
            EnumConcreteLibfunc::Init(_) => vec![ApChange::Known(0)],
            EnumConcreteLibfunc::Match(libfunc) | EnumConcreteLibfunc::SnapshotMatch(libfunc) => {
                vec![ApChange::Known(0); libfunc.signature.branch_signatures.len()]
            }
        },
        CoreConcreteLibfunc::Struct(libfunc) => match libfunc {
            StructConcreteLibfunc::Construct(_)
            | StructConcreteLibfunc::Deconstruct(_)
            | StructConcreteLibfunc::SnapshotDeconstruct(_) => {
                vec![ApChange::Known(0)]
            }
        },
        CoreConcreteLibfunc::Felt252Dict(libfunc) => match libfunc {
            Felt252DictConcreteLibfunc::New(_) => vec![ApChange::Known(6)],
            Felt252DictConcreteLibfunc::Squash(_) => vec![ApChange::Unknown],
        },
        CoreConcreteLibfunc::Pedersen(libfunc) => match libfunc {
            PedersenConcreteLibfunc::PedersenHash(_) => vec![ApChange::Known(0)],
        },
        CoreConcreteLibfunc::Poseidon(libfunc) => match libfunc {
            PoseidonConcreteLibfunc::HadesPermutation(_) => vec![ApChange::Known(0)],
        },
        CoreConcreteLibfunc::StarkNet(libfunc) => match libfunc {
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
            | StarkNetConcreteLibfunc::Deploy(_)
            | StarkNetConcreteLibfunc::Keccak(_)
            | StarkNetConcreteLibfunc::LibraryCall(_)
            | StarkNetConcreteLibfunc::ReplaceClass(_)
            | StarkNetConcreteLibfunc::SendMessageToL1(_)
            | StarkNetConcreteLibfunc::Secp256(_) => {
                vec![ApChange::Known(2), ApChange::Known(2)]
            }
            StarkNetConcreteLibfunc::Testing(libfunc) => match libfunc {
                TestingConcreteLibfunc::Cheatcode(_) => vec![ApChange::Known(2)],
            },
        },
        CoreConcreteLibfunc::Nullable(libfunc) => match libfunc {
            NullableConcreteLibfunc::Null(_) => vec![ApChange::Known(0)],
            NullableConcreteLibfunc::NullableFromBox(_) => vec![ApChange::Known(0)],
            NullableConcreteLibfunc::MatchNullable(_) => {
                vec![ApChange::Known(0), ApChange::Known(0)]
            }
        },
        CoreConcreteLibfunc::Debug(_) => vec![ApChange::Known(0)],
        CoreConcreteLibfunc::SnapshotTake(_) => vec![ApChange::Known(0)],
        CoreConcreteLibfunc::Felt252DictEntry(libfunc) => match libfunc {
            Felt252DictEntryConcreteLibfunc::Get(_) => vec![ApChange::Known(0)],
            Felt252DictEntryConcreteLibfunc::Finalize(_) => vec![ApChange::Known(0)],
        },
        CoreConcreteLibfunc::Bytes31(libfunc) => match libfunc {
            Bytes31ConcreteLibfunc::Const(_) | Bytes31ConcreteLibfunc::ToFelt252(_) => {
                vec![ApChange::Known(0)]
            }
            Bytes31ConcreteLibfunc::TryFromFelt252(_) => {
                vec![ApChange::Known(5), ApChange::Known(6)]
            }
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
