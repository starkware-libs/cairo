use core::fmt;

use itertools::zip_eq;
use sierra::ids::{ConcreteLibFuncId, FunctionId};
use sierra::program::{Program, StatementIdx};
use solver::Expr;

use crate::{ApChange, ApChangeError};

/// Variable parts of an Ap change expression.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Var {
    /// Variables supplied for a libfunc at statement id (e.g. branch_align).
    LibFuncImplicitApChangeVariable(StatementIdx),
    /// Variable marking on a statement's past ap change (any route from function start to it).
    FunctionApChange(FunctionId),
}
impl fmt::Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Var::LibFuncImplicitApChangeVariable(idx) => {
                write!(f, "libfunc@{idx}")
            }
            Var::FunctionApChange(id) => write!(f, "function@{id}"),
        }
    }
}

type ApChangeExpr = Expr<Var>;

/// The effects of a libfunc.
pub struct Effects {
    pub ap_change: ApChange,
    pub locals: usize,
}

/// Information per statement in the analyzed Sierra program.
#[derive(Clone, Debug)]
struct StatementInfo {
    /// The function id that reaches the statement.
    func_id: FunctionId,
    /// The ap change up until the statement.
    past_ap_change: Option<ApChangeExpr>,
    /// The total size of allocated locals up until the statement.
    past_locals: usize,
}

/// Generates a set of equations from a program, and a function to extract cost expressions from a
/// library function id.
pub fn generate_equations<
    GetApChange: Fn(&ConcreteLibFuncId) -> Result<Vec<Effects>, ApChangeError>,
>(
    program: &Program,
    get_effects: GetApChange,
) -> Result<Vec<ApChangeExpr>, ApChangeError> {
    let mut generator = EquationGenerator::new(vec![None; program.statements.len()]);
    for func in &program.funcs {
        generator.set_or_add_constraint(
            &func.entry_point,
            StatementInfo {
                func_id: func.id.clone(),
                past_ap_change: Some(Expr::from_const(0)),
                past_locals: 0,
            },
        )?;
    }
    for idx in (0..program.statements.len()).map(StatementIdx) {
        let base_info = generator.get_info(&idx)?;
        match &program.get_statement(&idx).unwrap() {
            sierra::program::Statement::Return(_) => {
                generator.set_or_add_constraint(
                    &idx,
                    StatementInfo {
                        func_id: base_info.func_id.clone(),
                        past_ap_change: Some(Expr::from_var(Var::FunctionApChange(
                            base_info.func_id,
                        ))),
                        past_locals: base_info.past_locals,
                    },
                )?;
            }
            sierra::program::Statement::Invocation(invocation) => {
                let libfunc_effects = get_effects(&invocation.libfunc_id)?;
                if invocation.branches.len() != libfunc_effects.len() {
                    return Err(ApChangeError::IllegalInvocation(idx));
                }
                for (branch, branch_effects) in zip_eq(&invocation.branches, libfunc_effects) {
                    let branch_ap_change = match branch_effects.ap_change {
                        ApChange::Unknown => None,
                        ApChange::Known(x) => Some(Expr::from_const(x as i32)),
                        ApChange::FromMetadata => {
                            Some(Expr::from_var(Var::LibFuncImplicitApChangeVariable(idx)))
                        }
                        ApChange::FunctionCall(func_id) => Some(
                            Expr::from_var(Var::FunctionApChange(func_id)) + Expr::from_const(2),
                        ),
                        ApChange::FinalizeLocals => {
                            Some(Expr::from_const(base_info.past_locals as i32))
                        }
                        ApChange::KnownByTypeSize(_)
                        | ApChange::AtLocalsFinalizationByTypeSize(_) => {
                            unreachable!(
                                "These arms are translated to `ApChange::Known` in the \
                                 `get_effects` call."
                            )
                        }
                    };
                    let next_idx = idx.next(&branch.target);
                    generator.set_or_add_constraint(
                        &next_idx,
                        StatementInfo {
                            func_id: base_info.func_id.clone(),
                            past_ap_change: if let (Some(a), Some(b)) =
                                (&base_info.past_ap_change, branch_ap_change)
                            {
                                Some(a.clone() + b)
                            } else {
                                None
                            },
                            past_locals: base_info.past_locals + branch_effects.locals,
                        },
                    )?;
                }
            }
        }
    }
    Ok(generator.equations)
}

/// Helper to generate the equations for calculating gas variables.
struct EquationGenerator {
    pub statement_info: Vec<Option<StatementInfo>>,
    pub equations: Vec<Expr<Var>>,
}
impl EquationGenerator {
    fn new(statement_info: Vec<Option<StatementInfo>>) -> Self {
        Self { statement_info, equations: vec![] }
    }

    /// Sets some future or adds a matching equation if already set.
    fn set_or_add_constraint(
        &mut self,
        idx: &StatementIdx,
        info: StatementInfo,
    ) -> Result<(), ApChangeError> {
        let entry =
            self.statement_info.get_mut(idx.0).ok_or(ApChangeError::StatementOutOfBounds(*idx))?;
        if let Some(other) = entry {
            assert_eq!(other.past_locals, info.past_locals);
            if let (Some(a), Some(b)) = (&other.past_ap_change, info.past_ap_change) {
                self.equations.push(a.clone() - b);
            }
        } else {
            *entry = Some(info);
        }
        Ok(())
    }

    /// Returns `StatementInfo` for statement, will additionally make sure this information was
    /// initialized.
    fn get_info(&mut self, idx: &StatementIdx) -> Result<StatementInfo, ApChangeError> {
        self.statement_info[idx.0].clone().ok_or(ApChangeError::StatementOutOfOrder(*idx))
    }
}
