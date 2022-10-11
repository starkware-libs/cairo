use sierra::extensions::arithmetic::{
    BinaryOperationConcreteLibFunc, OperationConcreteLibFunc, OperationWithConstConcreteLibFunc,
    Operator,
};
use sierra::extensions::array::ArrayConcreteLibFunc;
use sierra::extensions::core::CoreConcreteLibFunc::{
    self, ApTracking, Array, Box, Drop, Dup, Felt, FunctionCall, Gas, Integer, Mem,
    UnconditionalJump, UnwrapNonZero,
};
use sierra::extensions::felt::FeltConcrete;
use sierra::extensions::function_call::FunctionCallConcreteLibFunc;
use sierra::extensions::gas::GasConcreteLibFunc::{GetGas, RefundGas};
use sierra::extensions::integer::IntegerConcrete;
use sierra::extensions::mem::MemConcreteLibFunc::{
    AlignTemps, AllocLocal, FinalizeLocals, Rename, StoreLocal, StoreTemp,
};
use sierra::program::StatementIdx;

use crate::cost_expr::{CostExpr, Var};
use crate::generate_equations::StatementFutureCost;

/// The cost for the call of a user function.
pub const FUNCTION_CALL_COST: i32 = 2;

/// Returns an expression for the gas cost for core libfuncs.
pub fn core_libfunc_cost(
    statement_future_cost: &mut dyn StatementFutureCost,
    idx: &StatementIdx,
    libfunc: &CoreConcreteLibFunc,
) -> Vec<CostExpr> {
    match libfunc {
        // For the case of function calls - assumes a variable for the cost of running from a
        // function entry point and on - while also adding the call cost.
        FunctionCall(FunctionCallConcreteLibFunc { function, .. }) => {
            vec![
                CostExpr::from_const(FUNCTION_CALL_COST)
                    + statement_future_cost.get_future_cost(&function.entry_point).clone(),
            ]
        }
        Gas(GetGas(_)) => vec![
            CostExpr::from_const(1) - CostExpr::from_var(Var::LibFuncImplicitGasVariable(*idx)),
            CostExpr::from_const(1),
        ],
        Gas(RefundGas(_)) => {
            vec![
                CostExpr::from_const(1) + CostExpr::from_var(Var::LibFuncImplicitGasVariable(*idx)),
            ]
        }
        Array(ArrayConcreteLibFunc::New(_)) => vec![CostExpr::from_const(1)],
        Array(ArrayConcreteLibFunc::Append(_)) => vec![CostExpr::from_const(2)],
        Integer(libfunc) => integer_libfunc_cost(libfunc),
        Felt(libfunc) => felt_libfunc_cost(libfunc),
        Drop(_) | Dup(_) | ApTracking(_) | UnwrapNonZero(_) | Mem(Rename(_)) | Box(_) => {
            vec![CostExpr::from_const(0)]
        }
        Mem(StoreLocal(_) | AllocLocal(_) | StoreTemp(_) | AlignTemps(_) | FinalizeLocals(_))
        | UnconditionalJump(_) => vec![CostExpr::from_const(1)],
    }
}

/// Returns costs for integer libfuncs.
fn integer_libfunc_cost(libfunc: &IntegerConcrete) -> Vec<CostExpr> {
    // TODO(orizi): When sierra_to_casm actually supports integers - fix costs.
    match libfunc {
        IntegerConcrete::Operation(OperationConcreteLibFunc::Binary(
            BinaryOperationConcreteLibFunc { operator, .. },
        )) => match operator {
            Operator::Add | Operator::Sub => vec![CostExpr::from_const(5)],
            Operator::Mul | Operator::Div | Operator::Mod => {
                vec![CostExpr::from_const(7)]
            }
        },
        IntegerConcrete::Operation(OperationConcreteLibFunc::Const(
            OperationWithConstConcreteLibFunc { operator, .. },
        )) => match operator {
            Operator::Add | Operator::Sub => vec![CostExpr::from_const(3)],
            Operator::Mul | Operator::Div | Operator::Mod => {
                vec![CostExpr::from_const(5)]
            }
        },
        IntegerConcrete::Const(_) => {
            vec![CostExpr::from_const(0)]
        }
        IntegerConcrete::JumpNotZero(_) => {
            vec![CostExpr::from_const(1), CostExpr::from_const(1)]
        }
    }
}

/// Returns costs for felt libfuncs.
fn felt_libfunc_cost(libfunc: &FeltConcrete) -> Vec<CostExpr> {
    match libfunc {
        FeltConcrete::Const(_) | FeltConcrete::Operation(_) => vec![CostExpr::from_const(0)],
        FeltConcrete::JumpNotZero(_) => {
            vec![CostExpr::from_const(1), CostExpr::from_const(1)]
        }
    }
}
