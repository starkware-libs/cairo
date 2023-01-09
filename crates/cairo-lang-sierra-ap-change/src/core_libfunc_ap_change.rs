use cairo_lang_sierra::extensions::array::ArrayConcreteLibfunc;
use cairo_lang_sierra::extensions::boolean::BoolConcreteLibfunc;
use cairo_lang_sierra::extensions::boxing::BoxConcreteLibfunc;
use cairo_lang_sierra::extensions::builtin_cost::{
    BuiltinCostConcreteLibfunc, BuiltinCostGetGasLibfunc,
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
use cairo_lang_sierra::extensions::uint128::{IntOperator, Uint128Concrete};

use crate::ApChange;

/// Returns the ap change for a core libfunc.
/// Values with unknown values will return as None.
pub fn core_libfunc_ap_change(libfunc: &CoreConcreteLibfunc) -> Vec<ApChange> {
    match libfunc {
        CoreConcreteLibfunc::ApTracking(_) => vec![ApChange::Unknown],
        CoreConcreteLibfunc::Array(libfunc) => match libfunc {
            ArrayConcreteLibfunc::New(_) => vec![ApChange::Known(1)],
            ArrayConcreteLibfunc::Append(_) => vec![ApChange::Known(0)],
            ArrayConcreteLibfunc::PopFront(_) => vec![ApChange::Known(1), ApChange::Known(1)],
            ArrayConcreteLibfunc::At(_) => vec![ApChange::Known(6), ApChange::Known(5)],
            ArrayConcreteLibfunc::Len(_) => vec![ApChange::Known(2)],
        },
        CoreConcreteLibfunc::Bitwise(_) => vec![ApChange::Known(0)],
        CoreConcreteLibfunc::BranchAlign(_) => vec![ApChange::FromMetadata],
        CoreConcreteLibfunc::Bool(libfunc) => match libfunc {
            BoolConcreteLibfunc::And(_) => vec![ApChange::Known(0)],
            BoolConcreteLibfunc::Not(_) => vec![ApChange::Known(1)],
            BoolConcreteLibfunc::Xor(_) => vec![ApChange::Known(1)],
            BoolConcreteLibfunc::Equal(_) => vec![ApChange::Known(1), ApChange::Known(1)],
        },
        CoreConcreteLibfunc::Box(libfunc) => match libfunc {
            BoxConcreteLibfunc::Into(_) => vec![ApChange::Known(1)],
            BoxConcreteLibfunc::Unbox(_) => vec![ApChange::Known(0)],
        },
        CoreConcreteLibfunc::BuiltinCost(libfunc) => match libfunc {
            BuiltinCostConcreteLibfunc::BuiltinGetGas(_) => vec![
                ApChange::Known(BuiltinCostGetGasLibfunc::cost_computation_max_steps() + 2),
                ApChange::Known(BuiltinCostGetGasLibfunc::cost_computation_max_steps() + 3),
            ],
            BuiltinCostConcreteLibfunc::GetBuiltinCosts(_) => vec![ApChange::Known(3)],
        },
        CoreConcreteLibfunc::Ec(libfunc) => match libfunc {
            EcConcreteLibfunc::AddToState(_) => vec![ApChange::Known(9)],
            EcConcreteLibfunc::CreatePoint(_) => vec![ApChange::Known(6), ApChange::Known(6)],
            EcConcreteLibfunc::FinalizeState(_) => vec![ApChange::Known(11), ApChange::Known(3)],
            EcConcreteLibfunc::InitState(_) => vec![ApChange::Known(8)],
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
        CoreConcreteLibfunc::Uint128(libfunc) => match libfunc {
            Uint128Concrete::Operation(libfunc) => match libfunc.operator {
                IntOperator::OverflowingAdd | IntOperator::OverflowingSub => {
                    vec![ApChange::Known(2), ApChange::Known(3)]
                }
                IntOperator::OverflowingMul => todo!(),
                IntOperator::DivMod => vec![ApChange::Known(7)],
                IntOperator::WideMul => vec![ApChange::Known(17)],
            },
            Uint128Concrete::LessThan(_) => vec![ApChange::Known(2), ApChange::Known(3)],
            Uint128Concrete::Equal(_) => vec![ApChange::Known(1), ApChange::Known(1)],
            Uint128Concrete::LessThanOrEqual(_) => vec![ApChange::Known(3), ApChange::Known(2)],
            Uint128Concrete::FromFelt(_) => vec![ApChange::Known(1), ApChange::Known(6)],
            Uint128Concrete::Const(_) | Uint128Concrete::ToFelt(_) => vec![ApChange::Known(0)],
            Uint128Concrete::JumpNotZero(_) => vec![ApChange::Known(0), ApChange::Known(0)],
        },
        CoreConcreteLibfunc::Mem(libfunc) => match libfunc {
            MemConcreteLibfunc::StoreTemp(libfunc) => {
                vec![ApChange::KnownByTypeSize(libfunc.ty.clone())]
            }
            MemConcreteLibfunc::AlignTemps(libfunc) => {
                vec![ApChange::KnownByTypeSize(libfunc.ty.clone())]
            }
            MemConcreteLibfunc::StoreLocal(_) => vec![ApChange::Known(0)],
            MemConcreteLibfunc::FinalizeLocals(_) => vec![ApChange::FinalizeLocals],
            MemConcreteLibfunc::AllocLocal(libfunc) => {
                vec![ApChange::AtLocalsFinalizationByTypeSize(libfunc.ty.clone())]
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
            StarkNetConcreteLibfunc::StorageAddressConst(_) => vec![ApChange::Known(0)],
            StarkNetConcreteLibfunc::StorageAddressFromFelt(_) => vec![ApChange::Known(7)],
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
