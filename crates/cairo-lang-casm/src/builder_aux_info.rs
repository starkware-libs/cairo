use std::collections::HashMap;

use num_bigint::BigInt;

use crate::builder::{CasmBuilder, Var};
use crate::cell_expression::CellExpression;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct VarBaseDesc {
    pub name: String,
    pub var_expr: CellExpression,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ExprDesc {
    pub expr: String,
    pub var_a: VarBaseDesc,
    pub op: String,
    pub var_b: Option<VarBaseDesc>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct VarDesc {
    pub name: String,
    pub var_expr: CellExpression,
    pub expr: Option<ExprDesc>,
    pub ap_change: usize,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct AssertDesc {
    pub lhs: VarBaseDesc,
    pub expr: ExprDesc,
    pub ap_change: usize,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ConstDesc {
    pub name: String,
    pub expr: String,
    pub value: CellExpression,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct JumpDesc {
    pub target: String,
    pub cond_var: Option<VarBaseDesc>,
    pub ap_change: usize,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct LabelDesc {
    pub label: String,
    pub ap_change: usize,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct RetExprDesc {
    pub names: Vec<String>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct RetBranchDesc {
    pub name: String,
    pub exprs: Vec<RetExprDesc>,
}

impl RetBranchDesc {
    pub fn get_expr_at_pos(&self, pos: usize) -> Option<String> {
        let mut skipped: usize = 0;
        for expr in &self.exprs {
            if skipped + expr.names.len() > pos {
                return Some(expr.names[pos - skipped].clone());
            }
            skipped += expr.names.len();
        }
        None
    }

    /// Returns the name at the given position from the end: 0 is the last
    /// name, 1 the one before it, etc.
    pub fn get_expr_at_pos_from_end(&self, pos: usize, ignore_at_start: usize) -> Option<String> {
        if ignore_at_start != 0 {
            let arg_num: usize = self.exprs.iter().fold(0, |a, e| a + e.names.len());
            if arg_num < ignore_at_start || arg_num - ignore_at_start <= pos {
                return None;
            }
        }
        let mut skipped: usize = 0;
        for expr in self.exprs.iter().rev() {
            let names_len = expr.names.len();
            if skipped + names_len > pos {
                return Some(expr.names[names_len - 1 - (pos - skipped)].clone());
            }
            skipped += expr.names.len();
        }
        None
    }

    pub fn flat_exprs(&self) -> Vec<String> {
        self.exprs.iter().flat_map(|exprs| exprs.names.iter().map(|s| s.clone())).collect()
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum StatementDesc {
    TempVar(VarDesc),
    LocalVar(VarDesc),
    Let(AssertDesc),
    Assert(AssertDesc),
    ApPlus(usize),
    Jump(JumpDesc),
    Label(LabelDesc),
    Fail,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum LibfuncAlgTypeDesc {
    /// The lhs is small enough so that its square root plus 1 can be multiplied by `2**128`
    /// without wraparound.
    /// `lhs_upper_sqrt` is the square root of the upper bound of the lhs, rounded up.
    DivRemKnownSmallLhs { lhs_upper_sqrt: BigInt },
    /// The rhs is small enough to be multiplied by `2**128` without wraparound.
    DivRemKnownSmallRhs,
    /// The quotient is small enough to be multiplied by `2**128` without wraparound.
    DivRemKnownSmallQuotient { q_upper_bound: BigInt },
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CasmBuilderAuxiliaryInfo {
    pub var_names: HashMap<Var, String>,
    pub consts: Vec<ConstDesc>,
    pub args: Vec<(Var, CellExpression)>,
    pub statements: Vec<StatementDesc>,
    // TODO: The fields below are not set by the casm builder, but outside of it.
    // Therefore, they do not really belong here, but should be stored on a separate
    // structure. Currently, no such structure exists.
    pub return_branches: Vec<RetBranchDesc>,
    pub core_libfunc_instr_num: usize,
    pub alg_type: Option<LibfuncAlgTypeDesc>,
}

impl CasmBuilderAuxiliaryInfo {
    pub fn not_empty(&self) -> bool {
        if 0 == self.statements.len() {
            false
        } else if self.statements.len() == 1 {
            // Heuristic: check that this block is not simply a non-conditional 'jump'
            // Where such blocks come from is not yet clear to me.
            match &self.statements[0] {
                StatementDesc::Jump(jmp) => !jmp.cond_var.is_none(),
                _ => true,
            }
        } else {
            true
        }
    }

    pub fn add_arg(&mut self, var_name: &str, var: Var, cell_expr: CellExpression) {
        self.var_names.insert(var, String::from(var_name));
        self.args.push((var, cell_expr));
    }

    pub fn add_return_branch(&mut self, branch_name: &str, vars: &[&[Var]]) {
        self.return_branches.push(RetBranchDesc {
            name: String::from(branch_name),
            exprs: vars
                .iter()
                .map(|v| RetExprDesc {
                    names: v
                        .iter()
                        .map(|var| self.var_names.get(var).unwrap_or(&String::from("ρ")).clone())
                        .collect(),
                })
                .collect(),
        });
    }

    pub fn add_tempvar(
        &mut self,
        var_name: &str,
        var: Var,
        var_expr: CellExpression,
        ap_change: usize,
    ) {
        self.var_names.insert(var, String::from(var_name));
        self.statements.push(StatementDesc::TempVar(VarDesc {
            name: String::from(var_name),
            var_expr,
            expr: None,
            ap_change,
        }));
    }

    pub fn add_localvar(
        &mut self,
        var_name: &str,
        var: Var,
        var_expr: CellExpression,
        ap_change: usize,
    ) {
        self.var_names.insert(var, String::from(var_name));
        self.statements.push(StatementDesc::LocalVar(VarDesc {
            name: String::from(var_name),
            var_expr,
            expr: None,
            ap_change,
        }));
    }

    pub fn add_const(&mut self, var_name: &str, var: Var, expr: &str, value: CellExpression) {
        self.var_names.insert(var, String::from(var_name));
        self.consts.push(ConstDesc {
            name: String::from(var_name),
            expr: String::from(expr),
            value,
        });
    }

    pub fn add_let(
        &mut self,
        lhs: VarBaseDesc,
        lhs_id: Var,
        expr: &str,
        var_a: VarBaseDesc,
        op: &str,
        var_b: Option<VarBaseDesc>,
        ap_change: usize,
    ) {
        self.var_names.insert(lhs_id, String::from(&lhs.name));
        self.statements.push(StatementDesc::Let(AssertDesc {
            lhs,
            expr: ExprDesc { expr: String::from(expr), var_a, op: String::from(op), var_b },
            ap_change,
        }));
    }

    pub fn add_ap_plus(&mut self, step_size: usize) {
        self.statements.push(StatementDesc::ApPlus(step_size));
    }

    pub fn make_var_desc(&self, name: &str, id: Var, expr: CellExpression) -> VarBaseDesc {
        VarBaseDesc { name: name.into(), var_expr: expr }
    }

    pub fn add_assert(
        &mut self,
        lhs: VarBaseDesc,
        rhs: &str,
        var_a: VarBaseDesc,
        op: &str,
        var_b: Option<VarBaseDesc>,
        ap_change: usize,
    ) {
        match self.statements.last_mut() {
            Some(StatementDesc::TempVar(tv)) => {
                if tv.name == lhs.name && tv.expr.is_none() {
                    tv.expr = Some(ExprDesc {
                        expr: String::from(rhs),
                        var_a,
                        op: String::from(op),
                        var_b,
                    });
                    return;
                }
            }
            Some(StatementDesc::LocalVar(lv)) => {
                if lv.name == lhs.name && lv.expr.is_none() {
                    lv.expr = Some(ExprDesc {
                        expr: String::from(rhs),
                        var_a,
                        op: String::from(op),
                        var_b,
                    });
                    return;
                }
            }
            _ => {}
        }

        self.statements.push(StatementDesc::Assert(AssertDesc {
            lhs,
            expr: ExprDesc { expr: String::from(rhs), var_a, op: String::from(op), var_b },
            ap_change,
        }));
    }

    pub fn add_jump(&mut self, label: &str, ap_change: usize) {
        self.statements.push(StatementDesc::Jump(JumpDesc {
            target: String::from(label),
            cond_var: None,
            ap_change,
        }));
    }

    pub fn add_jump_nz(&mut self, label: &str, cond_var: VarBaseDesc, ap_change: usize) {
        self.statements.push(StatementDesc::Jump(JumpDesc {
            target: String::from(label),
            cond_var: Some(cond_var),
            ap_change,
        }));
    }

    pub fn add_label(&mut self, label: &str, ap_change: usize) {
        self.statements
            .push(StatementDesc::Label(LabelDesc { label: String::from(label), ap_change }));
    }

    pub fn add_fail(&mut self) {
        self.statements.push(StatementDesc::Fail);
    }

    pub fn set_alg_type(&mut self, alg_type: LibfuncAlgTypeDesc) {
        self.alg_type = Some(alg_type);
    }

    pub fn finalize(&mut self, num_casm_instructions: usize) {
        // Record the number of instructions generated for the code built up to
        // here (additional instructions may be added later).
        self.core_libfunc_instr_num = num_casm_instructions;
    }
}

impl Default for CasmBuilderAuxiliaryInfo {
    fn default() -> Self {
        Self {
            var_names: Default::default(),
            consts: Default::default(),
            args: Default::default(),
            statements: Default::default(),
            return_branches: Default::default(),
            core_libfunc_instr_num: 0,
            alg_type: None,
        }
    }
}
