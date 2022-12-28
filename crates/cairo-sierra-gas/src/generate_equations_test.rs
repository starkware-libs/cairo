use std::collections::HashMap;

use indoc::indoc;
use sierra::extensions::builtin_cost::CostTokenType;
use sierra::ids::ConcreteLibFuncId;
use sierra::program::StatementIdx;
use test_case::test_case;

use super::generate_equations;
use crate::core_libfunc_cost_expr::CostExprMap;
use crate::cost_expr::{CostExpr, Var};
use crate::CostError;

/// Returns a cost expression for a statement future variable.
fn future_statement_cost(idx: usize) -> CostExpr {
    CostExpr::from_var(Var::StatementFuture(StatementIdx(idx), CostTokenType::Step))
}

/// Returns a cost expression for a libfunc variable.
fn libfunc_cost(idx: usize) -> CostExpr {
    CostExpr::from_var(Var::LibFuncImplicitGasVariable(StatementIdx(idx), CostTokenType::Step))
}

#[test_case(indoc! {"
                return();
                test_program@0() -> ();
            "},
            HashMap::new() =>
            Ok(vec![(future_statement_cost(0))]);
            "return only")]
#[test_case(indoc! {"
                cost1() -> ();
                cost2() -> ();
                return();
                test_program@0() -> ();
            "},
            HashMap::from([
                ("cost1".into(), vec![CostExpr::from_const(1)]),
                ("cost2".into(), vec![CostExpr::from_const(2)]),
            ]) =>
            Ok(vec![(future_statement_cost(0) - CostExpr::from_const(3))]);
            "simple cost sum")]
#[test_case(indoc! {"
                var_x() -> ();
                cost1() -> ();
                return();
                test_program@0() -> ();
            "},
            HashMap::from([
                ("var_x".into(), vec![libfunc_cost(0)]),
                ("cost1".into(), vec![CostExpr::from_const(1)]),
            ]) =>
            Ok(vec![
                (future_statement_cost(0) - (CostExpr::from_const(1) + libfunc_cost(0)))
            ]);
            "single var")]
#[test_case(indoc! {"
                // Traversal index = 3 - future_next = Var::Statement(0) - due to function.
                cost1() -> ();
                // Traversal index = 2 - future_next = Var::Statement(1) - due to cycle.
                cost2() -> ();
                // Traversal index = 1 - future_next = Var::Statement(1) + 4 - first branch + step.
                jump_back() { 1() fallthrough() };
                // Traversal index = 0 - future_next = 0 - since it is a return.
                return();
                test_program@0() -> ();
            "},
            HashMap::from([
                ("cost1".into(), vec![CostExpr::from_const(1)]),
                ("cost2".into(), vec![CostExpr::from_const(2)]),
                ("jump_back".into(), vec![CostExpr::from_const(3), CostExpr::from_const(4)]),
            ]) =>
            Ok(vec![
                // Since 'jump_back' is the first non trivial traversed statement, a variable
                // for statement 1 would be created, and this value add with the cost of the step
                // to it would be equal to the cost of the other step.
                (future_statement_cost(1) - CostExpr::from_const(1)),
                // Next 'cost2' is used, so the equation is the variable of the line, equals to the
                // cost of the next line + the step cost.
                (CostExpr::from_const(-(3 + 2))),
                // The equation for the function and the step to the following cycle variable.
                (future_statement_cost(0) -future_statement_cost(1) - CostExpr::from_const(1)),
            ]);
            "simple cycle")]
#[test_case(indoc! {"
                return();
                test_program@1() -> ();
            "},
            HashMap::new() =>
            Err(CostError::StatementOutOfBounds(StatementIdx(1)));
            "handle bad function entry point")]
#[test_case(indoc! {"
                jump() { 4() };
                return();
                test_program@0() -> ();
            "},
            HashMap::new() =>
            Err(CostError::StatementOutOfBounds(StatementIdx(4)));
            "handle bad jump target")]
fn generate(
    code: &str,
    costs: HashMap<ConcreteLibFuncId, Vec<CostExpr>>,
) -> Result<Vec<CostExpr>, CostError> {
    Ok(generate_equations(
        &sierra::ProgramParser::new().parse(code).unwrap(),
        |_, _idx, libfunc_id| {
            costs
                .get(libfunc_id)
                .unwrap()
                .iter()
                .map(|x| CostExprMap::from_iter([(CostTokenType::Step, x.clone())]))
                .collect()
        },
    )?[CostTokenType::Step]
        .clone())
}
