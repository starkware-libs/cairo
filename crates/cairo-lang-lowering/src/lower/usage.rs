//! Introduces [BlockUsages], which is responsible for computing variables usage in semantic blocks\
//! of a function.

use cairo_lang_proc_macros::DebugWithDb;
use cairo_lang_semantic::expr::fmt::ExprFormatter;
use cairo_lang_semantic::{
    Expr, ExprFunctionCallArg, ExprId, FunctionBody, Pattern, Statement, VarId, VarMemberPath,
};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;

#[cfg(test)]
#[path = "usage_test.rs"]
mod test;

/// Usages of variables and member paths in semantic code.
#[derive(Debug, Default, DebugWithDb)]
#[debug_db(ExprFormatter<'a>)]
pub struct Usage {
    /// Member paths that are read.
    pub usage: OrderedHashSet<VarMemberPath>,
    /// Member paths that are assigned to.
    pub changes: OrderedHashSet<VarMemberPath>,
    /// Variables that are defined.
    pub introductions: OrderedHashSet<VarId>,
}

/// Usages of variables and member paths in each semantic block of a function.
#[derive(Debug, DebugWithDb)]
#[debug_db(ExprFormatter<'a>)]
pub struct BlockUsages {
    /// Mapping from an [ExprId] for an block expression, to its [Usage].
    pub block_usages: OrderedHashMap<ExprId, Usage>,
}
impl BlockUsages {
    pub fn from_function_body(function_body: &FunctionBody) -> Self {
        let mut current = Usage::default();
        let mut block_usages = Self { block_usages: Default::default() };
        block_usages.handle_expr(function_body, function_body.body_expr, &mut current);
        block_usages
    }

    fn handle_expr(&mut self, function_body: &FunctionBody, expr_id: ExprId, current: &mut Usage) {
        match &function_body.exprs[expr_id] {
            Expr::Tuple(expr) => {
                for expr_id in &expr.items {
                    self.handle_expr(function_body, *expr_id, current);
                }
            }
            Expr::Snapshot(expr) => self.handle_expr(function_body, expr.inner, current),
            Expr::Desnap(expr) => self.handle_expr(function_body, expr.inner, current),
            Expr::Assignment(expr) => {
                self.handle_expr(function_body, expr.rhs, current);
                current.changes.insert(expr.ref_arg.clone());
            }
            Expr::Block(expr) => {
                let mut usage = Default::default();
                for stmt in &expr.statements {
                    match &function_body.statements[*stmt] {
                        Statement::Let(stmt) => {
                            self.handle_expr(function_body, stmt.expr, &mut usage);
                            Self::handle_pattern(&stmt.pattern, &mut usage);
                        }
                        Statement::Expr(stmt) => {
                            self.handle_expr(function_body, stmt.expr, &mut usage)
                        }
                        Statement::Return(stmt) => {
                            self.handle_expr(function_body, stmt.expr, &mut usage)
                        }
                        Statement::Break(stmt) => {
                            self.handle_expr(function_body, stmt.expr, &mut usage)
                        }
                    };
                }
                if let Some(expr_id) = expr.tail {
                    self.handle_expr(function_body, expr_id, &mut usage)
                }
                // Prune introductions from usages.
                for member_path in usage.usage.clone() {
                    if usage.introductions.contains(&member_path.base_var()) {
                        usage.usage.swap_remove(&member_path);
                    }
                }
                for member_path in usage.changes.clone() {
                    if usage.introductions.contains(&member_path.base_var()) {
                        usage.changes.swap_remove(&member_path);
                    }
                }
                current.usage.extend(usage.usage.iter().cloned());
                current.changes.extend(usage.changes.iter().cloned());
                self.block_usages.insert(expr_id, usage);
            }
            Expr::Loop(expr) => self.handle_expr(function_body, expr.body, current),
            Expr::FunctionCall(expr) => {
                for arg in &expr.args {
                    match arg {
                        ExprFunctionCallArg::Reference(member_path) => {
                            current.usage.insert(member_path.clone());
                            current.changes.insert(member_path.clone());
                        }
                        ExprFunctionCallArg::Value(expr) => {
                            self.handle_expr(function_body, *expr, current)
                        }
                    }
                }
            }
            Expr::Match(expr) => {
                self.handle_expr(function_body, expr.matched_expr, current);
                for arm in &expr.arms {
                    Self::handle_pattern(&arm.pattern, current);
                    self.handle_expr(function_body, arm.expression, current);
                }
            }
            Expr::If(expr) => {
                self.handle_expr(function_body, expr.condition, current);
                self.handle_expr(function_body, expr.if_block, current);
                if let Some(expr) = expr.else_block {
                    self.handle_expr(function_body, expr, current);
                }
            }
            Expr::Var(expr) => {
                current.usage.insert(VarMemberPath::Var(expr.clone()));
            }
            Expr::Literal(_) => {}
            Expr::MemberAccess(expr) => {
                if let Some(member_path) = &expr.member_path {
                    current.usage.insert(member_path.clone());
                } else {
                    self.handle_expr(function_body, expr.expr, current);
                }
            }
            Expr::StructCtor(expr) => {
                for (_, expr_id) in &expr.members {
                    self.handle_expr(function_body, *expr_id, current);
                }
            }
            Expr::EnumVariantCtor(expr) => {
                self.handle_expr(function_body, expr.value_expr, current)
            }
            Expr::PropagateError(expr) => self.handle_expr(function_body, expr.inner, current),
            Expr::Constant(_) => {}
            Expr::Missing(_) => {}
        }
    }

    fn handle_pattern(pat: &Pattern, current: &mut Usage) {
        match pat {
            Pattern::Literal(_) => {}
            Pattern::Variable(pat) => {
                current.introductions.insert(VarId::Local(pat.var.id));
            }
            Pattern::Struct(pat) => {
                for (_, pat) in &pat.field_patterns {
                    Self::handle_pattern(pat, current);
                }
            }
            Pattern::Tuple(pat) => {
                for pat in &pat.field_patterns {
                    Self::handle_pattern(pat, current);
                }
            }
            Pattern::EnumVariant(pat) => {
                Self::handle_pattern(&pat.inner_pattern, current);
            }
            Pattern::Otherwise(_) => {}
        }
    }
}
