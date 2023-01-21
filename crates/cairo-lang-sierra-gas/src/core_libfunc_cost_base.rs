use cairo_lang_sierra::extensions::array::ArrayConcreteLibfunc;
use cairo_lang_sierra::extensions::boolean::BoolConcreteLibfunc;
use cairo_lang_sierra::extensions::boxing::BoxConcreteLibfunc;
use cairo_lang_sierra::extensions::builtin_cost::{
    BuiltinCostConcreteLibfunc, BuiltinCostGetGasLibfunc, CostTokenType,
};
use cairo_lang_sierra::extensions::core::CoreConcreteLibfunc::{
    self, ApTracking, Array, Bitwise, Bool, Box, BranchAlign, BuiltinCost, DictFeltTo, Drop, Dup,
    Ec, Enum, Felt, FunctionCall, Gas, Mem, Pedersen, Struct, Uint128, Uint8, UnconditionalJump,
    UnwrapNonZero,
};
use cairo_lang_sierra::extensions::dict_felt_to::DictFeltToConcreteLibfunc;
use cairo_lang_sierra::extensions::ec::EcConcreteLibfunc;
use cairo_lang_sierra::extensions::enm::EnumConcreteLibfunc;
use cairo_lang_sierra::extensions::felt::FeltConcrete;
use cairo_lang_sierra::extensions::function_call::FunctionCallConcreteLibfunc;
use cairo_lang_sierra::extensions::gas::GasConcreteLibfunc::{GetGas, RefundGas};
use cairo_lang_sierra::extensions::mem::MemConcreteLibfunc::{
    AlignTemps, AllocLocal, FinalizeLocals, Rename, StoreLocal, StoreTemp,
};
use cairo_lang_sierra::extensions::nullable::NullableConcreteLibfunc;
use cairo_lang_sierra::extensions::strct::StructConcreteLibfunc;
use cairo_lang_sierra::extensions::uint::{IntOperator, Uint8Concrete};
use cairo_lang_sierra::extensions::uint128::Uint128Concrete;
use cairo_lang_sierra::ids::ConcreteTypeId;
use cairo_lang_sierra::program::Function;

use crate::starknet_libfunc_cost_base::starknet_libfunc_cost_base;

/// The operation required for extracting a libfunc's cost.
pub trait CostOperations {
    type CostType: Clone;

    /// Gets a cost from a constant value (of type [CostTokenType::Step]).
    fn const_cost(&self, value: i32) -> Self::CostType;
    /// Gets a cost from a constant value of the given token type.
    fn const_cost_token(&self, value: i32, token_type: CostTokenType) -> Self::CostType;
    /// Gets a cost for the content of a function.
    fn function_cost(&mut self, function: &Function) -> Self::CostType;
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
}

/// Returns some cost value for a libfunc - a helper function to implement costing both for creating
/// gas equations and getting actual gas usage after having a solution.
pub fn core_libfunc_cost_base<Ops: CostOperations, InfoProvider: InvocationCostInfoProvider>(
    ops: &mut Ops,
    libfunc: &CoreConcreteLibfunc,
    info_provider: &InfoProvider,
) -> Vec<Ops::CostType> {
    match libfunc {
        // For the case of function calls - assumes a variable for the cost of running from a
        // function entry point and on - while also adding the call cost.
        FunctionCall(FunctionCallConcreteLibfunc { function, .. }) => {
            let func_content_cost = ops.function_cost(function);
            vec![ops.add(ops.const_cost(2), func_content_cost)]
        }
        Bitwise(_) => {
            vec![ops.add(ops.const_cost(2), ops.const_cost_token(1, CostTokenType::Bitwise))]
        }
        Bool(BoolConcreteLibfunc::And(_)) => vec![ops.const_cost(0)],
        Bool(BoolConcreteLibfunc::Not(_)) => vec![ops.const_cost(1)],
        Bool(BoolConcreteLibfunc::Xor(_)) => vec![ops.const_cost(1)],
        Bool(BoolConcreteLibfunc::Or(_)) => vec![ops.const_cost(2)],
        Bool(BoolConcreteLibfunc::Equal(_)) => vec![ops.const_cost(2), ops.const_cost(2)],
        Ec(libfunc) => match libfunc {
            EcConcreteLibfunc::Neg(_) => vec![ops.const_cost(0)],
            EcConcreteLibfunc::StateAdd(_) => vec![ops.const_cost(9)],
            EcConcreteLibfunc::TryNew(_) => vec![ops.const_cost(6), ops.const_cost(6)],
            EcConcreteLibfunc::StateFinalize(_) => vec![ops.const_cost(13), ops.const_cost(6)],
            EcConcreteLibfunc::StateInit(_) => vec![ops.const_cost(8)],
            EcConcreteLibfunc::StateAddMul(_) => {
                vec![ops.add(ops.const_cost(5), ops.const_cost_token(1, CostTokenType::EcOp))]
            }
            EcConcreteLibfunc::PointFromX(_) => vec![
                ops.const_cost(8), // Success.
                ops.const_cost(9), // Failure.
            ],
            EcConcreteLibfunc::UnwrapPoint(_) => vec![ops.const_cost(0)],
        },
        Gas(GetGas(_)) => {
            vec![
                ops.sub(ops.const_cost(3), ops.statement_var_cost(CostTokenType::Step)),
                ops.const_cost(4),
            ]
        }
        Gas(RefundGas(_)) => vec![ops.statement_var_cost(CostTokenType::Step)],
        BranchAlign(_) => {
            let cost = CostTokenType::iter()
                .map(|token_type| ops.statement_var_cost(*token_type))
                .reduce(|x, y| ops.add(x, y));
            vec![ops.add(cost.unwrap(), ops.const_cost(1))]
        }
        Array(ArrayConcreteLibfunc::New(_)) => vec![ops.const_cost(1)],
        Array(ArrayConcreteLibfunc::Append(libfunc)) => {
            vec![ops.const_cost(info_provider.type_size(&libfunc.ty) as i32)]
        }
        Array(ArrayConcreteLibfunc::PopFront(_)) => vec![ops.const_cost(2), ops.const_cost(3)],
        Array(ArrayConcreteLibfunc::At(_)) => vec![ops.const_cost(4), ops.const_cost(3)],
        Array(ArrayConcreteLibfunc::Len(_)) => vec![ops.const_cost(0)],
        Uint128(libfunc) => u128_libfunc_cost(ops, libfunc),
        Uint8(libfunc) => u8_libfunc_cost(ops, libfunc),
        Felt(libfunc) => felt_libfunc_cost(ops, libfunc),
        Drop(_) | Dup(_) | ApTracking(_) | UnwrapNonZero(_) | Mem(Rename(_)) => {
            vec![ops.const_cost(0)]
        }
        Box(libfunc) => match libfunc {
            BoxConcreteLibfunc::Into(libfunc) => {
                vec![ops.const_cost(info_provider.type_size(&libfunc.ty).try_into().unwrap())]
            }
            BoxConcreteLibfunc::Unbox(_) => vec![ops.const_cost(0)],
        },
        Mem(StoreLocal(libfunc) | StoreTemp(libfunc)) => {
            vec![ops.const_cost(info_provider.type_size(&libfunc.ty) as i32)]
        }
        Mem(AllocLocal(_) | AlignTemps(_) | FinalizeLocals(_)) | UnconditionalJump(_) => {
            vec![ops.const_cost(1)]
        }
        Enum(EnumConcreteLibfunc::Init(_)) => vec![ops.const_cost(1)],
        Enum(EnumConcreteLibfunc::Match(sig)) => {
            vec![ops.const_cost(1); sig.signature.branch_signatures.len()]
        }
        Struct(StructConcreteLibfunc::Construct(_) | StructConcreteLibfunc::Deconstruct(_)) => {
            vec![ops.const_cost(0)]
        }
        DictFeltTo(DictFeltToConcreteLibfunc::New(_)) => {
            // 7 for dict_new + 92 for the fixed cost of dict_squash (see below).
            vec![ops.const_cost(99)]
        }
        DictFeltTo(DictFeltToConcreteLibfunc::Read(_)) => {
            // 4 for the dict_read + 82 for the variable cost of dict_squash (see below).
            vec![ops.const_cost(86)]
        }
        DictFeltTo(DictFeltToConcreteLibfunc::Write(_)) => {
            // 4 for the dict_write + 82 for the variable cost of dict_squash (see below).
            vec![ops.const_cost(86)]
        }
        DictFeltTo(DictFeltToConcreteLibfunc::Squash(_)) => {
            // Dict squash have a fixed cost of 92 + 82 for each dict access. The fixed cost is
            // precharged in dict_new to for pay dict_squash in case of out of gas.
            // The cost of the proccesing of the first key is 82, and each access for an existing
            // key costs only 12. In each read/write we charge 82 gas and 70 gas are
            // refunded per each succesive access in dict squash.
            vec![ops.const_cost(0)]
        }
        Pedersen(_) => {
            vec![ops.add(ops.const_cost(2), ops.const_cost_token(1, CostTokenType::Pedersen))]
        }
        BuiltinCost(libfunc) => match libfunc {
            BuiltinCostConcreteLibfunc::BuiltinGetGas(_) => {
                let cost = CostTokenType::iter()
                    .map(|token_type| ops.statement_var_cost(*token_type))
                    .reduce(|x, y| ops.add(x, y));
                // Compute the (maximal) number of steps for the computation of the requested cost.
                let compute_requested_cost_steps =
                    BuiltinCostGetGasLibfunc::cost_computation_max_steps() as i32;
                vec![
                    ops.sub(ops.const_cost(compute_requested_cost_steps + 3), cost.unwrap()),
                    ops.const_cost(compute_requested_cost_steps + 5),
                ]
            }
            BuiltinCostConcreteLibfunc::GetBuiltinCosts(_) => vec![ops.const_cost(3)],
        },
        CoreConcreteLibfunc::StarkNet(libfunc) => starknet_libfunc_cost_base(ops, libfunc),
        CoreConcreteLibfunc::Nullable(libfunc) => match libfunc {
            NullableConcreteLibfunc::Null(_) => vec![ops.const_cost(0)],
            NullableConcreteLibfunc::IntoNullable(_) => vec![ops.const_cost(0)],
            NullableConcreteLibfunc::FromNullable(_) => vec![ops.const_cost(1), ops.const_cost(1)],
        },
        CoreConcreteLibfunc::Debug(_) => vec![ops.const_cost(0)],
    }
}

/// Returns costs for u8 libfuncs.
fn u8_libfunc_cost<Ops: CostOperations>(ops: &Ops, libfunc: &Uint8Concrete) -> Vec<Ops::CostType> {
    match libfunc {
        Uint8Concrete::Const(_) => vec![ops.const_cost(0)],
        Uint8Concrete::Operation(libfunc) => match libfunc.operator {
            IntOperator::OverflowingAdd => {
                vec![ops.const_cost(4), ops.const_cost(4)]
            }
            IntOperator::OverflowingSub => {
                vec![ops.const_cost(3), ops.const_cost(5)]
            }
        },
        Uint8Concrete::LessThan(_) => {
            vec![ops.const_cost(4), ops.const_cost(3)]
        }
        Uint8Concrete::Equal(_) => {
            vec![ops.const_cost(2), ops.const_cost(2)]
        }
        Uint8Concrete::LessThanOrEqual(_) => {
            vec![ops.const_cost(3), ops.const_cost(4)]
        }
    }
}

/// Returns costs for u128 libfuncs.
fn u128_libfunc_cost<Ops: CostOperations>(
    ops: &Ops,
    libfunc: &Uint128Concrete,
) -> Vec<Ops::CostType> {
    // TODO(orizi): When sierra_to_casm actually supports integers - fix costs.
    match libfunc {
        Uint128Concrete::Operation(libfunc) => match libfunc.operator {
            IntOperator::OverflowingAdd | IntOperator::OverflowingSub => {
                vec![ops.const_cost(3), ops.const_cost(4)]
            }
        },
        Uint128Concrete::DivMod(_) => vec![ops.const_cost(7)],
        Uint128Concrete::WideMul(_) => vec![ops.const_cost(25)],
        Uint128Concrete::Const(_) | Uint128Concrete::ToFelt(_) => {
            vec![ops.const_cost(0)]
        }
        Uint128Concrete::FromFelt(_) => {
            vec![ops.const_cost(2), ops.const_cost(11)]
        }
        Uint128Concrete::JumpNotZero(_) => {
            vec![ops.const_cost(1), ops.const_cost(1)]
        }
        Uint128Concrete::LessThan(_) => {
            vec![ops.const_cost(4), ops.const_cost(3)]
        }
        Uint128Concrete::Equal(_) => {
            vec![ops.const_cost(2), ops.const_cost(2)]
        }
        Uint128Concrete::LessThanOrEqual(_) => {
            vec![ops.const_cost(3), ops.const_cost(4)]
        }
    }
}

/// Returns costs for felt libfuncs.
fn felt_libfunc_cost<Ops: CostOperations>(ops: &Ops, libfunc: &FeltConcrete) -> Vec<Ops::CostType> {
    match libfunc {
        FeltConcrete::Const(_) | FeltConcrete::BinaryOperation(_) => vec![ops.const_cost(0)],
        FeltConcrete::JumpNotZero(_) => {
            vec![ops.const_cost(1), ops.const_cost(1)]
        }
    }
}
