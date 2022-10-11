use sierra::extensions::arithmetic::{
    BinaryOperationConcreteLibFunc, OperationConcreteLibFunc, OperationWithConstConcreteLibFunc,
    Operator,
};
use sierra::extensions::array::ArrayConcreteLibFunc;
use sierra::extensions::core::CoreConcreteLibFunc::{
    self, ApTracking, Array, Box, Drop, Dup, Enum, Felt, FunctionCall, Gas, Integer, Mem,
    UnconditionalJump, UnwrapNonZero,
};
use sierra::extensions::enm::EnumConcreteLibFunc;
use sierra::extensions::felt::FeltConcrete;
use sierra::extensions::function_call::FunctionCallConcreteLibFunc;
use sierra::extensions::gas::GasConcreteLibFunc::{GetGas, RefundGas};
use sierra::extensions::integer::IntegerConcrete;
use sierra::extensions::mem::MemConcreteLibFunc::{
    AlignTemps, AllocLocal, FinalizeLocals, Rename, StoreLocal, StoreTemp,
};
use sierra::program::Function;

pub const FUNCTION_CALL_COST: i32 = 2;

/// Returns some cost value for a libfunc - a helper function to implement costing both for creating
/// gas equations and getting actual gas usage after having a solution.
pub fn core_libfunc_cost_base<
    CostType: std::ops::Add<Output = CostType> + std::ops::Sub<Output = CostType> + Clone,
    FromConst: Fn(i32) -> CostType,
    FromFunction: FnOnce(&Function) -> CostType,
    FromVar: FnOnce() -> CostType,
>(
    from_const: FromConst,
    from_function: FromFunction,
    from_var: FromVar,
    libfunc: &CoreConcreteLibFunc,
) -> Vec<CostType> {
    match libfunc {
        // For the case of function calls - assumes a variable for the cost of running from a
        // function entry point and on - while also adding the call cost.
        FunctionCall(FunctionCallConcreteLibFunc { function, .. }) => {
            vec![from_const(FUNCTION_CALL_COST) + from_function(function)]
        }
        Gas(GetGas(_)) => vec![from_const(1) - from_var(), from_const(1)],
        Gas(RefundGas(_)) => {
            vec![from_const(1) + from_var()]
        }
        Array(ArrayConcreteLibFunc::New(_)) => vec![from_const(1)],
        Array(ArrayConcreteLibFunc::Append(_)) => vec![from_const(2)],
        Integer(libfunc) => integer_libfunc_cost(from_const, libfunc),
        Felt(libfunc) => felt_libfunc_cost(from_const, libfunc),
        Drop(_) | Dup(_) | ApTracking(_) | UnwrapNonZero(_) | Mem(Rename(_)) | Box(_) => {
            vec![from_const(0)]
        }
        Mem(StoreLocal(_) | AllocLocal(_) | StoreTemp(_) | AlignTemps(_) | FinalizeLocals(_))
        | UnconditionalJump(_) => vec![from_const(1)],
        Enum(EnumConcreteLibFunc::Init(_)) => vec![from_const(1)],
        Enum(EnumConcreteLibFunc::Match(sig)) => {
            vec![from_const(1); sig.signature.branch_signatures.len()]
        }
    }
}

/// Returns costs for integer libfuncs.
fn integer_libfunc_cost<
    CostType: std::ops::Add<Output = CostType> + std::ops::Sub<Output = CostType>,
    FromConst: Fn(i32) -> CostType,
>(
    from_const: FromConst,
    libfunc: &IntegerConcrete,
) -> Vec<CostType> {
    // TODO(orizi): When sierra_to_casm actually supports integers - fix costs.
    match libfunc {
        IntegerConcrete::Operation(OperationConcreteLibFunc::Binary(
            BinaryOperationConcreteLibFunc { operator, .. },
        )) => match operator {
            Operator::Add | Operator::Sub => vec![from_const(5)],
            Operator::Mul | Operator::Div | Operator::Mod => {
                vec![from_const(7)]
            }
        },
        IntegerConcrete::Operation(OperationConcreteLibFunc::Const(
            OperationWithConstConcreteLibFunc { operator, .. },
        )) => match operator {
            Operator::Add | Operator::Sub => vec![from_const(3)],
            Operator::Mul | Operator::Div | Operator::Mod => {
                vec![from_const(5)]
            }
        },
        IntegerConcrete::Const(_) => {
            vec![from_const(0)]
        }
        IntegerConcrete::JumpNotZero(_) => {
            vec![from_const(1), from_const(1)]
        }
    }
}

/// Returns costs for felt libfuncs.
fn felt_libfunc_cost<
    CostType: std::ops::Add<Output = CostType> + std::ops::Sub<Output = CostType>,
    FromConst: Fn(i32) -> CostType,
>(
    from_const: FromConst,
    libfunc: &FeltConcrete,
) -> Vec<CostType> {
    match libfunc {
        FeltConcrete::Const(_) | FeltConcrete::Operation(_) => vec![from_const(0)],
        FeltConcrete::JumpNotZero(_) => {
            vec![from_const(1), from_const(1)]
        }
    }
}
