use cairo_lang_sierra::extensions::array::ArrayConcreteLibfunc;
use cairo_lang_sierra::extensions::boolean::BoolConcreteLibfunc;
use cairo_lang_sierra::extensions::boxing::BoxConcreteLibfunc;
use cairo_lang_sierra::extensions::builtin_cost::{
    BuiltinCostConcreteLibfunc, BuiltinCostGetGasLibfunc, CostTokenType,
};
use cairo_lang_sierra::extensions::casts::CastConcreteLibfunc;
use cairo_lang_sierra::extensions::core::CoreConcreteLibfunc;
use cairo_lang_sierra::extensions::dict_felt_to::DictFeltToConcreteLibfunc;
use cairo_lang_sierra::extensions::ec::EcConcreteLibfunc;
use cairo_lang_sierra::extensions::enm::EnumConcreteLibfunc;
use cairo_lang_sierra::extensions::felt::FeltConcrete;
use cairo_lang_sierra::extensions::gas::GasConcreteLibfunc;
use cairo_lang_sierra::extensions::mem::MemConcreteLibfunc;
use cairo_lang_sierra::extensions::nullable::NullableConcreteLibfunc;
use cairo_lang_sierra::extensions::starknet::StarkNetConcreteLibfunc;
use cairo_lang_sierra::extensions::structure::StructConcreteLibfunc;
use cairo_lang_sierra::extensions::uint::{
    IntOperator, Uint16Concrete, Uint32Concrete, Uint64Concrete, Uint8Concrete,
};
use cairo_lang_sierra::extensions::uint128::Uint128Concrete;
use cairo_lang_sierra::ids::ConcreteTypeId;

use crate::ApChange;

/// Trait for providing extra information required for AP changes for a specific libfunc invocation.
pub trait InvocationApChangeInfoProvider {
    /// Provides the sizes of types.
    fn type_size(&self, ty: &ConcreteTypeId) -> usize;
    /// Number of tokens provided by the libfunc invocation (currently only relevant for
    /// `get_gas_all`).
    fn token_usages(&self, token_type: CostTokenType) -> usize;
}

/// Returns the ap change for a core libfunc.
/// Values with unknown values will return as None.
pub fn core_libfunc_ap_change<InfoProvider: InvocationApChangeInfoProvider>(
    libfunc: &CoreConcreteLibfunc,
    info_provider: &InfoProvider,
) -> Vec<ApChange> {
    match libfunc {
        CoreConcreteLibfunc::ApTracking(_) => vec![ApChange::Unknown],
        CoreConcreteLibfunc::Array(libfunc) => match libfunc {
            ArrayConcreteLibfunc::New(_) => vec![ApChange::Known(1)],
            ArrayConcreteLibfunc::Append(_) => vec![ApChange::Known(0)],
            ArrayConcreteLibfunc::PopFront(_) => vec![ApChange::Known(1), ApChange::Known(1)],
            ArrayConcreteLibfunc::Get(libfunc) => {
                if info_provider.type_size(&libfunc.ty) == 1 { [5, 3] } else { [6, 5] }
                    .map(ApChange::Known)
                    .to_vec()
            }
            ArrayConcreteLibfunc::Len(libfunc) => {
                vec![ApChange::Known(usize::from(info_provider.type_size(&libfunc.ty) != 1))]
            }
        },
        CoreConcreteLibfunc::Bitwise(_) => vec![ApChange::Known(0)],
        CoreConcreteLibfunc::BranchAlign(_) => vec![ApChange::FromMetadata],
        CoreConcreteLibfunc::Bool(libfunc) => match libfunc {
            BoolConcreteLibfunc::And(_) => vec![ApChange::Known(0)],
            BoolConcreteLibfunc::Not(_) => vec![ApChange::Known(1)],
            BoolConcreteLibfunc::Xor(_) => vec![ApChange::Known(1)],
            BoolConcreteLibfunc::Or(_) => vec![ApChange::Known(2)],
            BoolConcreteLibfunc::Equal(_) => vec![ApChange::Known(1), ApChange::Known(1)],
        },
        CoreConcreteLibfunc::Box(libfunc) => match libfunc {
            BoxConcreteLibfunc::Into(_) => vec![ApChange::Known(1)],
            BoxConcreteLibfunc::Unbox(_) => vec![ApChange::Known(0)],
        },
        CoreConcreteLibfunc::BuiltinCost(libfunc) => match libfunc {
            BuiltinCostConcreteLibfunc::BuiltinGetGas(_) => {
                let cost_computation_ap_change: usize =
                    BuiltinCostGetGasLibfunc::cost_computation_steps(|token_type| {
                        info_provider.token_usages(token_type)
                    });
                vec![
                    ApChange::Known(cost_computation_ap_change + 2),
                    ApChange::Known(cost_computation_ap_change + 3),
                ]
            }
            BuiltinCostConcreteLibfunc::GetBuiltinCosts(_) => vec![ApChange::Known(3)],
        },
        CoreConcreteLibfunc::Cast(libfunc) => match libfunc {
            CastConcreteLibfunc::Upcast(_) => vec![ApChange::Known(0)],
        },
        CoreConcreteLibfunc::Ec(libfunc) => match libfunc {
            EcConcreteLibfunc::IsZero(_) => vec![ApChange::Known(0), ApChange::Known(0)],
            EcConcreteLibfunc::Neg(_) => vec![ApChange::Known(0)],
            EcConcreteLibfunc::StateAdd(_) => vec![ApChange::Known(9)],
            EcConcreteLibfunc::TryNew(_) => vec![ApChange::Known(6), ApChange::Known(6)],
            EcConcreteLibfunc::StateFinalize(_) => vec![ApChange::Known(11), ApChange::Known(3)],
            EcConcreteLibfunc::StateInit(_) => vec![ApChange::Known(8)],
            EcConcreteLibfunc::StateAddMul(_) => vec![ApChange::Known(0)],
            EcConcreteLibfunc::PointFromX(_) => vec![ApChange::Known(7), ApChange::Known(7)],
            EcConcreteLibfunc::UnwrapPoint(_) => vec![ApChange::Known(0)],
            EcConcreteLibfunc::Zero(_) => vec![ApChange::Known(0)],
        },
        CoreConcreteLibfunc::Drop(_) | CoreConcreteLibfunc::Dup(_) => vec![ApChange::Known(0)],
        CoreConcreteLibfunc::Felt(libfunc) => match libfunc {
            FeltConcrete::BinaryOperation(_) | FeltConcrete::Const(_) => vec![ApChange::Known(0)],
            FeltConcrete::IsZero(_) => vec![ApChange::Known(0), ApChange::Known(0)],
        },
        CoreConcreteLibfunc::FunctionCall(libfunc) => {
            vec![ApChange::FunctionCall(libfunc.function.id.clone())]
        }
        CoreConcreteLibfunc::Gas(libfunc) => match libfunc {
            GasConcreteLibfunc::GetGas(_) => vec![ApChange::Known(2), ApChange::Known(2)],
            GasConcreteLibfunc::RefundGas(_) => vec![ApChange::Known(0)],
        },
        CoreConcreteLibfunc::Uint8(libfunc) => match libfunc {
            Uint8Concrete::Const(_) | Uint8Concrete::ToFelt(_) => vec![ApChange::Known(0)],
            Uint8Concrete::Operation(libfunc) => match libfunc.operator {
                IntOperator::OverflowingAdd => {
                    vec![ApChange::Known(3), ApChange::Known(3)]
                }
                IntOperator::OverflowingSub => {
                    vec![ApChange::Known(2), ApChange::Known(4)]
                }
            },
            Uint8Concrete::LessThan(_) => vec![ApChange::Known(2), ApChange::Known(3)],
            Uint8Concrete::SquareRoot(_) => vec![ApChange::Known(6)],
            Uint8Concrete::Equal(_) => vec![ApChange::Known(1), ApChange::Known(1)],
            Uint8Concrete::LessThanOrEqual(_) => vec![ApChange::Known(3), ApChange::Known(2)],
            Uint8Concrete::FromFelt(_) => vec![ApChange::Known(2), ApChange::Known(7)],
            Uint8Concrete::IsZero(_) => vec![ApChange::Known(0), ApChange::Known(0)],
            Uint8Concrete::Divmod(_) => vec![ApChange::Known(5)],
            Uint8Concrete::WideMul(_) => vec![ApChange::Known(0)],
        },
        CoreConcreteLibfunc::Uint16(libfunc) => match libfunc {
            Uint16Concrete::Const(_) | Uint16Concrete::ToFelt(_) => vec![ApChange::Known(0)],
            Uint16Concrete::Operation(libfunc) => match libfunc.operator {
                IntOperator::OverflowingAdd => {
                    vec![ApChange::Known(3), ApChange::Known(3)]
                }
                IntOperator::OverflowingSub => {
                    vec![ApChange::Known(2), ApChange::Known(4)]
                }
            },
            Uint16Concrete::LessThan(_) => vec![ApChange::Known(2), ApChange::Known(3)],
            Uint16Concrete::SquareRoot(_) => vec![ApChange::Known(6)],
            Uint16Concrete::Equal(_) => vec![ApChange::Known(1), ApChange::Known(1)],
            Uint16Concrete::LessThanOrEqual(_) => vec![ApChange::Known(3), ApChange::Known(2)],
            Uint16Concrete::FromFelt(_) => vec![ApChange::Known(2), ApChange::Known(7)],
            Uint16Concrete::IsZero(_) => vec![ApChange::Known(0), ApChange::Known(0)],
            Uint16Concrete::Divmod(_) => vec![ApChange::Known(5)],
            Uint16Concrete::WideMul(_) => vec![ApChange::Known(0)],
        },
        CoreConcreteLibfunc::Uint32(libfunc) => match libfunc {
            Uint32Concrete::Const(_) | Uint32Concrete::ToFelt(_) => vec![ApChange::Known(0)],
            Uint32Concrete::Operation(libfunc) => match libfunc.operator {
                IntOperator::OverflowingAdd => {
                    vec![ApChange::Known(3), ApChange::Known(3)]
                }
                IntOperator::OverflowingSub => {
                    vec![ApChange::Known(2), ApChange::Known(4)]
                }
            },
            Uint32Concrete::LessThan(_) => vec![ApChange::Known(2), ApChange::Known(3)],
            Uint32Concrete::SquareRoot(_) => vec![ApChange::Known(6)],
            Uint32Concrete::Equal(_) => vec![ApChange::Known(1), ApChange::Known(1)],
            Uint32Concrete::LessThanOrEqual(_) => vec![ApChange::Known(3), ApChange::Known(2)],
            Uint32Concrete::FromFelt(_) => vec![ApChange::Known(2), ApChange::Known(7)],
            Uint32Concrete::IsZero(_) => vec![ApChange::Known(0), ApChange::Known(0)],
            Uint32Concrete::Divmod(_) => vec![ApChange::Known(5)],
            Uint32Concrete::WideMul(_) => vec![ApChange::Known(0)],
        },
        CoreConcreteLibfunc::Uint64(libfunc) => match libfunc {
            Uint64Concrete::Const(_) | Uint64Concrete::ToFelt(_) => vec![ApChange::Known(0)],
            Uint64Concrete::Operation(libfunc) => match libfunc.operator {
                IntOperator::OverflowingAdd => {
                    vec![ApChange::Known(3), ApChange::Known(3)]
                }
                IntOperator::OverflowingSub => {
                    vec![ApChange::Known(2), ApChange::Known(4)]
                }
            },
            Uint64Concrete::LessThan(_) => vec![ApChange::Known(2), ApChange::Known(3)],
            Uint64Concrete::SquareRoot(_) => vec![ApChange::Known(6)],
            Uint64Concrete::Equal(_) => vec![ApChange::Known(1), ApChange::Known(1)],
            Uint64Concrete::LessThanOrEqual(_) => vec![ApChange::Known(3), ApChange::Known(2)],
            Uint64Concrete::FromFelt(_) => vec![ApChange::Known(2), ApChange::Known(7)],
            Uint64Concrete::IsZero(_) => vec![ApChange::Known(0), ApChange::Known(0)],
            Uint64Concrete::Divmod(_) => vec![ApChange::Known(5)],
            Uint64Concrete::WideMul(_) => vec![ApChange::Known(0)],
        },
        CoreConcreteLibfunc::Uint128(libfunc) => match libfunc {
            Uint128Concrete::Operation(libfunc) => match libfunc.operator {
                IntOperator::OverflowingAdd | IntOperator::OverflowingSub => {
                    vec![ApChange::Known(2), ApChange::Known(3)]
                }
            },
            Uint128Concrete::Divmod(_) => vec![ApChange::Known(7)],
            Uint128Concrete::WideMul(_) => vec![ApChange::Known(17)],
            Uint128Concrete::LessThan(_) => vec![ApChange::Known(2), ApChange::Known(3)],
            Uint128Concrete::Equal(_) => vec![ApChange::Known(1), ApChange::Known(1)],
            Uint128Concrete::SquareRoot(_) => vec![ApChange::Known(6)],
            Uint128Concrete::LessThanOrEqual(_) => vec![ApChange::Known(3), ApChange::Known(2)],
            Uint128Concrete::FromFelt(_) => vec![ApChange::Known(1), ApChange::Known(6)],
            Uint128Concrete::Const(_) | Uint128Concrete::ToFelt(_) => vec![ApChange::Known(0)],
            Uint128Concrete::IsZero(_) => vec![ApChange::Known(0), ApChange::Known(0)],
        },
        CoreConcreteLibfunc::Mem(libfunc) => match libfunc {
            MemConcreteLibfunc::StoreTemp(libfunc) => {
                vec![ApChange::Known(info_provider.type_size(&libfunc.ty))]
            }
            MemConcreteLibfunc::AlignTemps(libfunc) => {
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
        CoreConcreteLibfunc::DictFeltTo(libfunc) => match libfunc {
            DictFeltToConcreteLibfunc::New(_) => vec![ApChange::Known(6)],
            DictFeltToConcreteLibfunc::Read(_) => vec![ApChange::Known(1)],
            DictFeltToConcreteLibfunc::Write(_) => vec![ApChange::Known(1)],
            DictFeltToConcreteLibfunc::Squash(_) => vec![ApChange::Unknown],
        },
        CoreConcreteLibfunc::Pedersen(_) => vec![ApChange::Known(0)],
        CoreConcreteLibfunc::StarkNet(libfunc) => match libfunc {
            StarkNetConcreteLibfunc::ContractAddressConst(_) => vec![ApChange::Known(0)],
            StarkNetConcreteLibfunc::ContractAddressTryFromFelt(_) => {
                vec![ApChange::Known(5), ApChange::Known(6)]
            }
            StarkNetConcreteLibfunc::ContractAddressToFelt(_) => vec![ApChange::Known(0)],
            StarkNetConcreteLibfunc::CallContract(_) => {
                vec![ApChange::Known(2), ApChange::Known(2)]
            }
            StarkNetConcreteLibfunc::StorageRead(_) => vec![ApChange::Known(2), ApChange::Known(2)],
            StarkNetConcreteLibfunc::StorageWrite(_) => {
                vec![ApChange::Known(2), ApChange::Known(2)]
            }
            StarkNetConcreteLibfunc::StorageBaseAddressConst(_) => vec![ApChange::Known(0)],
            StarkNetConcreteLibfunc::StorageBaseAddressFromFelt(_) => vec![ApChange::Known(7)],
            StarkNetConcreteLibfunc::StorageAddressFromBase(_) => vec![ApChange::Known(0)],
            StarkNetConcreteLibfunc::StorageAddressFromBaseAndOffset(_) => vec![ApChange::Known(0)],
            StarkNetConcreteLibfunc::EmitEvent(_) => vec![ApChange::Known(2), ApChange::Known(2)],
            StarkNetConcreteLibfunc::GetCallerAddress(_) => {
                vec![ApChange::Known(2), ApChange::Known(2)]
            }
        },
        CoreConcreteLibfunc::Nullable(libfunc) => match libfunc {
            NullableConcreteLibfunc::Null(_) => vec![ApChange::Known(0)],
            NullableConcreteLibfunc::IntoNullable(_) => vec![ApChange::Known(0)],
            NullableConcreteLibfunc::FromNullable(_) => {
                vec![ApChange::Known(0), ApChange::Known(0)]
            }
        },
        CoreConcreteLibfunc::Debug(_) => vec![ApChange::Known(0)],
        CoreConcreteLibfunc::SnapshotTake(_) => vec![ApChange::Known(0)],
    }
}
