use cairo_lang_sierra::extensions::array::ArrayConcreteLibfunc;
use cairo_lang_sierra::extensions::boolean::BoolConcreteLibfunc;
use cairo_lang_sierra::extensions::boxing::BoxConcreteLibfunc;
use cairo_lang_sierra::extensions::builtin_cost::{
    BuiltinCostConcreteLibfunc, BuiltinCostGetGasLibfunc, CostTokenType,
};
use cairo_lang_sierra::extensions::core::CoreConcreteLibfunc;
use cairo_lang_sierra::extensions::dict_felt_to::DictFeltToConcreteLibfunc;
use cairo_lang_sierra::extensions::ec::EcConcreteLibfunc;
use cairo_lang_sierra::extensions::enm::EnumConcreteLibfunc;
use cairo_lang_sierra::extensions::felt::FeltConcrete;
use cairo_lang_sierra::extensions::gas::GasConcreteLibfunc;
use cairo_lang_sierra::extensions::mem::MemConcreteLibfunc;
use cairo_lang_sierra::extensions::nullable::NullableConcreteLibfunc;
use cairo_lang_sierra::extensions::starknet::StarkNetConcreteLibfunc;
use cairo_lang_sierra::extensions::strct::StructConcreteLibfunc;
use cairo_lang_sierra::extensions::uint::{IntOperator, Uint8Concrete};
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
            ArrayConcreteLibfunc::At(libfunc) => {
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
                let cost_computation_ap_change =
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
        CoreConcreteLibfunc::Ec(libfunc) => match libfunc {
            EcConcreteLibfunc::Neg(_) => vec![ApChange::Known(0)],
            EcConcreteLibfunc::StateAdd(_) => vec![ApChange::Known(9)],
            EcConcreteLibfunc::TryNew(_) => vec![ApChange::Known(6), ApChange::Known(6)],
            EcConcreteLibfunc::StateFinalize(_) => vec![ApChange::Known(11), ApChange::Known(3)],
            EcConcreteLibfunc::StateInit(_) => vec![ApChange::Known(8)],
            EcConcreteLibfunc::StateAddMul(_) => vec![ApChange::Known(0)],
            EcConcreteLibfunc::PointFromX(_) => vec![ApChange::Known(7), ApChange::Known(7)],
            EcConcreteLibfunc::UnwrapPoint(_) => vec![ApChange::Known(0)],
        },
        CoreConcreteLibfunc::Drop(_) | CoreConcreteLibfunc::Dup(_) => vec![ApChange::Known(0)],
        CoreConcreteLibfunc::Felt(libfunc) => match libfunc {
            FeltConcrete::BinaryOperation(_) | FeltConcrete::Const(_) => vec![ApChange::Known(0)],
            FeltConcrete::JumpNotZero(_) => vec![ApChange::Known(0), ApChange::Known(0)],
        },
        CoreConcreteLibfunc::FunctionCall(libfunc) => {
            vec![ApChange::FunctionCall(libfunc.function.id.clone())]
        }
        CoreConcreteLibfunc::Gas(libfunc) => match libfunc {
            GasConcreteLibfunc::GetGas(_) => vec![ApChange::Known(2), ApChange::Known(2)],
            GasConcreteLibfunc::RefundGas(_) => vec![ApChange::Known(0)],
        },
        CoreConcreteLibfunc::Uint8(libfunc) => match libfunc {
            Uint8Concrete::Const(_) => vec![ApChange::Known(0)],
            Uint8Concrete::Operation(libfunc) => match libfunc.operator {
                IntOperator::OverflowingAdd => {
                    vec![ApChange::Known(3), ApChange::Known(3)]
                }
                IntOperator::OverflowingSub => {
                    vec![ApChange::Known(2), ApChange::Known(4)]
                }
            },
            Uint8Concrete::LessThan(_) => vec![ApChange::Known(2), ApChange::Known(3)],
            Uint8Concrete::Equal(_) => vec![ApChange::Known(1), ApChange::Known(1)],
            Uint8Concrete::LessThanOrEqual(_) => vec![ApChange::Known(3), ApChange::Known(2)],
        },
        CoreConcreteLibfunc::Uint128(libfunc) => match libfunc {
            Uint128Concrete::Operation(libfunc) => match libfunc.operator {
                IntOperator::OverflowingAdd | IntOperator::OverflowingSub => {
                    vec![ApChange::Known(2), ApChange::Known(3)]
                }
            },
            Uint128Concrete::DivMod(_) => vec![ApChange::Known(7)],
            Uint128Concrete::WideMul(_) => vec![ApChange::Known(17)],
            Uint128Concrete::LessThan(_) => vec![ApChange::Known(2), ApChange::Known(3)],
            Uint128Concrete::Equal(_) => vec![ApChange::Known(1), ApChange::Known(1)],
            Uint128Concrete::LessThanOrEqual(_) => vec![ApChange::Known(3), ApChange::Known(2)],
            Uint128Concrete::FromFelt(_) => vec![ApChange::Known(1), ApChange::Known(6)],
            Uint128Concrete::Const(_) | Uint128Concrete::ToFelt(_) => vec![ApChange::Known(0)],
            Uint128Concrete::JumpNotZero(_) => vec![ApChange::Known(0), ApChange::Known(0)],
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
            EnumConcreteLibfunc::Match(libfunc) => {
                vec![ApChange::Known(0); libfunc.signature.branch_signatures.len()]
            }
        },
        CoreConcreteLibfunc::Struct(libfunc) => match libfunc {
            StructConcreteLibfunc::Construct(_) | StructConcreteLibfunc::Deconstruct(_) => {
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
        },
        CoreConcreteLibfunc::Nullable(libfunc) => match libfunc {
            NullableConcreteLibfunc::Null(_) => vec![ApChange::Known(0)],
            NullableConcreteLibfunc::IntoNullable(_) => vec![ApChange::Known(0)],
            NullableConcreteLibfunc::FromNullable(_) => {
                vec![ApChange::Known(0), ApChange::Known(0)]
            }
        },
        CoreConcreteLibfunc::Debug(_) => vec![ApChange::Known(0)],
    }
}
