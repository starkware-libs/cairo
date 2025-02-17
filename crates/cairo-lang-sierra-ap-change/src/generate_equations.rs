use core::fmt;

use cairo_lang_eq_solver::Expr;
use cairo_lang_sierra::ids::{ConcreteLibfuncId, FunctionId};
use cairo_lang_sierra::program::{Program, StatementIdx};
use itertools::zip_eq;

use crate::{ApChange, ApChangeError};

/// Variable parts of an Ap change expression.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Var {
    /// Variables supplied for a libfunc at statement id (e.g. branch_align).
    LibfuncImplicitApChangeVariable(StatementIdx),
    /// Variable marking on a statement's past ap change (any route from function start to it).
    FunctionApChange(FunctionId),
}
impl fmt::Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Var::LibfuncImplicitApChangeVariable(idx) => {
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

/// Base points to align ap from.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
enum ChangeBase {
    FunctionStart(FunctionId),
    EnableApTrackingPoint(StatementIdx),
}

/// Information per statement in the analyzed Sierra program.
#[derive(Clone, Debug)]
struct StatementInfo {
    /// The base point to align ap over.
    base: ChangeBase,
    /// The ap change up until the statement.
    past_ap_change: Option<ApChangeExpr>,
    /// The total size of allocated locals up until the statement.
    past_locals: usize,
}

/// Generates a set of equations from a program, and a function to extract cost expressions from a
/// library function id.
pub fn generate_equations<
    GetApChange: Fn(StatementIdx, &ConcreteLibfuncId) -> Result<Vec<Effects>, ApChangeError>,
>(
    program: &Program,
    get_effects: GetApChange,
) -> Result<Vec<ApChangeExpr>, ApChangeError> {
    let mut generator = EquationGenerator::new(program.statements.len());
    for func in &program.funcs {
        generator.set_or_add_constraint(
            &func.entry_point,
            StatementInfo {
                base: ChangeBase::FunctionStart(func.id.clone()),
                past_ap_change: Some(Expr::from_const(0)),
                past_locals: 0,
            },
        )?;
    }
    for idx in (0..program.statements.len()).map(StatementIdx) {
        let base_info = generator.get_info(&idx)?;
        match &program.get_statement(&idx).unwrap() {
            cairo_lang_sierra::program::Statement::Return(_) => {
                if let ChangeBase::FunctionStart(func_id) = &base_info.base {
                    generator.set_or_add_constraint(
                        &idx,
                        StatementInfo {
                            base: base_info.base.clone(),
                            past_ap_change: Some(Expr::from_var(Var::FunctionApChange(
                                func_id.clone(),
                            ))),
                            past_locals: base_info.past_locals,
                        },
                    )?;
                }
            }
            cairo_lang_sierra::program::Statement::Invocation(invocation) => {
                let libfunc_effects = get_effects(idx, &invocation.libfunc_id)?;
                if invocation.branches.len() != libfunc_effects.len() {
                    return Err(ApChangeError::WrongNumApChangeBranches(idx));
                }
                for (branch, branch_effects) in zip_eq(&invocation.branches, libfunc_effects) {
                    let enable_tracking =
                        matches!(branch_effects.ap_change, ApChange::EnableApTracking);
                    let branch_ap_change = match branch_effects.ap_change {
                        ApChange::Unknown | ApChange::DisableApTracking => None,
                        ApChange::Known(x) => Some(Expr::from_const(x as i32)),
                        ApChange::FromMetadata => {
                            Some(Expr::from_var(Var::LibfuncImplicitApChangeVariable(idx)))
                        }
                        ApChange::FunctionCall(func_id) => Some(
                            Expr::from_var(Var::FunctionApChange(func_id)) + Expr::from_const(2),
                        ),
                        ApChange::FinalizeLocals => {
                            Some(Expr::from_const(base_info.past_locals as i32))
                        }
                        ApChange::AtLocalsFinalization(_) => {
                            unreachable!(
                                "These arms are translated to `ApChange::Known` in the \
                                 `get_effects` call."
                            )
                        }
                        ApChange::EnableApTracking => Some(Expr::from_const(0)),
                    };
                    let next_idx = idx.next(&branch.target);
                    let past_locals = base_info.past_locals + branch_effects.locals;
                    generator.set_or_add_constraint(
                        &next_idx,
                        if enable_tracking {
                            StatementInfo {
                                base: ChangeBase::EnableApTrackingPoint(next_idx),
                                past_ap_change: branch_ap_change,
                                past_locals,
                            }
                        } else {
                            StatementInfo {
                                base: base_info.base.clone(),
                                past_ap_change: if let (Some(a), Some(b)) =
                                    (&base_info.past_ap_change, branch_ap_change)
                                {
                                    Some(a.clone() + b)
                                } else {
                                    None
                                },
                                past_locals,
                            }
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
    fn new(n_statements: usize) -> Self {
        Self { statement_info: vec![None; n_statements], equations: vec![] }
    }

    /// Sets some future or adds a matching equation if already set.
    fn set_or_add_constraint(
        &mut self,
        idx: &StatementIdx,
        info: StatementInfo,
    ) -> Result<(), ApChangeError> {
        let entry = &mut self.statement_info[idx.0];
        if let Some(other) = entry {
            if other.past_locals != info.past_locals {
                return Err(ApChangeError::BadMergeAllocatedLocalsMismatch(*idx));
            }
            if let (Some(a), Some(b)) = (&other.past_ap_change, info.past_ap_change) {
                if other.base != info.base {
                    return Err(ApChangeError::BadMergeBaseMismatch(*idx));
                }
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
