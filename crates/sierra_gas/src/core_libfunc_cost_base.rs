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

/// The operation required for extracting a libfunc's cost.
pub trait CostOperations {
    type CostType: Clone;

    /// Get a cost from a constant value.
    fn const_cost(&self, value: i32) -> Self::CostType;
    /// Get a cost for the content of a function.
    fn function_cost(&mut self, function: &Function) -> Self::CostType;
    /// Get a cost for a variable for the current statement.
    fn statement_var_cost(&self) -> Self::CostType;
    /// Adds costs.
    fn add(&self, lhs: Self::CostType, rhs: Self::CostType) -> Self::CostType;
    /// Subtracts costs.
    fn sub(&self, lhs: Self::CostType, rhs: Self::CostType) -> Self::CostType;
}

/// Returns some cost value for a libfunc - a helper function to implement costing both for creating
/// gas equations and getting actual gas usage after having a solution.
pub fn core_libfunc_cost_base<Ops: CostOperations>(
    ops: &mut Ops,
    libfunc: &CoreConcreteLibFunc,
) -> Vec<Ops::CostType> {
    match libfunc {
        // For the case of function calls - assumes a variable for the cost of running from a
        // function entry point and on - while also adding the call cost.
        FunctionCall(FunctionCallConcreteLibFunc { function, .. }) => {
            let func_content_cost = ops.function_cost(function);
            vec![ops.add(ops.const_cost(2), func_content_cost)]
        }
        Gas(GetGas(_)) => {
            vec![ops.sub(ops.const_cost(1), ops.statement_var_cost()), ops.const_cost(1)]
        }
        Gas(RefundGas(_)) => vec![ops.add(ops.const_cost(1), ops.statement_var_cost())],
        Array(ArrayConcreteLibFunc::New(_)) => vec![ops.const_cost(1)],
        Array(ArrayConcreteLibFunc::Append(_)) => vec![ops.const_cost(2)],
        Integer(libfunc) => integer_libfunc_cost(ops, libfunc),
        Felt(libfunc) => felt_libfunc_cost(ops, libfunc),
        Drop(_) | Dup(_) | ApTracking(_) | UnwrapNonZero(_) | Mem(Rename(_)) | Box(_) => {
            vec![ops.const_cost(0)]
        }
        Mem(StoreLocal(_) | AllocLocal(_) | StoreTemp(_) | AlignTemps(_) | FinalizeLocals(_))
        | UnconditionalJump(_) => vec![ops.const_cost(1)],
        Enum(EnumConcreteLibFunc::Init(_)) => vec![ops.const_cost(1)],
        Enum(EnumConcreteLibFunc::Match(sig)) => {
            vec![ops.const_cost(1); sig.signature.branch_signatures.len()]
        }
    }
}

/// Returns costs for integer libfuncs.
fn integer_libfunc_cost<Ops: CostOperations>(
    ops: &Ops,
    libfunc: &IntegerConcrete,
) -> Vec<Ops::CostType> {
    // TODO(orizi): When sierra_to_casm actually supports integers - fix costs.
    match libfunc {
        IntegerConcrete::Operation(OperationConcreteLibFunc::Binary(
            BinaryOperationConcreteLibFunc { operator, .. },
        )) => match operator {
            Operator::Add | Operator::Sub => vec![ops.const_cost(5)],
            Operator::Mul | Operator::Div | Operator::Mod => {
                vec![ops.const_cost(7)]
            }
        },
        IntegerConcrete::Operation(OperationConcreteLibFunc::Const(
            OperationWithConstConcreteLibFunc { operator, .. },
        )) => match operator {
            Operator::Add | Operator::Sub => vec![ops.const_cost(3)],
            Operator::Mul | Operator::Div | Operator::Mod => {
                vec![ops.const_cost(5)]
            }
        },
        IntegerConcrete::Const(_) => {
            vec![ops.const_cost(0)]
        }
        IntegerConcrete::JumpNotZero(_) => {
            vec![ops.const_cost(1), ops.const_cost(1)]
        }
    }
}

/// Returns costs for felt libfuncs.
fn felt_libfunc_cost<Ops: CostOperations>(ops: &Ops, libfunc: &FeltConcrete) -> Vec<Ops::CostType> {
    match libfunc {
        FeltConcrete::Const(_) | FeltConcrete::Operation(_) => vec![ops.const_cost(0)],
        FeltConcrete::JumpNotZero(_) => {
            vec![ops.const_cost(1), ops.const_cost(1)]
        }
    }
}
