//! Introduces [BlockUsages], which is responsible for computing variables usage in semantic blocks\
//! of a function.

use cairo_lang_defs::ids::MemberId;
use cairo_lang_proc_macros::DebugWithDb;
use cairo_lang_semantic::expr::fmt::ExprFormatter;
use cairo_lang_semantic::{
    self as semantic, Expr, ExprFunctionCallArg, ExprId, ExprVarMemberPath, FunctionBody, Pattern,
    Statement, VarId,
};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use semantic::ConcreteStructId;

#[cfg(test)]
#[path = "usage_test.rs"]
mod test;

/// Member path (e.g. a.b.c). Unlike [ExprVarMemberPath], this is not an expression, and has no
/// syntax pointers.
#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(ExprFormatter<'a>)]
pub enum MemberPath {
    Var(semantic::VarId),
    Member { parent: Box<MemberPath>, member_id: MemberId, concrete_struct_id: ConcreteStructId },
}
impl MemberPath {
    pub fn base_var(&self) -> VarId {
        match self {
            MemberPath::Var(var) => *var,
            MemberPath::Member { parent, .. } => parent.base_var(),
        }
    }
}
impl From<&ExprVarMemberPath> for MemberPath {
    fn from(value: &ExprVarMemberPath) -> Self {
        match value {
            ExprVarMemberPath::Var(expr) => MemberPath::Var(expr.var),
            ExprVarMemberPath::Member { parent, member_id, concrete_struct_id, .. } => {
                MemberPath::Member {
                    parent: Box::new(parent.as_ref().into()),
                    member_id: *member_id,
                    concrete_struct_id: *concrete_struct_id,
                }
            }
        }
    }
}

/// Usages of variables and member paths in semantic code.
#[derive(Debug, Default, DebugWithDb)]
#[debug_db(ExprFormatter<'a>)]
pub struct Usage {
    /// Member paths that are read.
    pub usage: OrderedHashMap<MemberPath, ExprVarMemberPath>,
    /// Member paths that are assigned to.
    pub changes: OrderedHashMap<MemberPath, ExprVarMemberPath>,
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
                current.usage.insert((&expr.ref_arg).into(), expr.ref_arg.clone());
                current.changes.insert((&expr.ref_arg).into(), expr.ref_arg.clone());
            }
            Expr::LogicalOperator(expr) => {
                self.handle_expr(function_body, expr.lhs, current);
                self.handle_expr(function_body, expr.rhs, current);
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
                        Statement::Continue(_) => (),
                        Statement::Return(stmt) => {
                            if let Some(expr) = stmt.expr_option {
                                self.handle_expr(function_body, expr, &mut usage)
                            };
                        }
                        Statement::Break(stmt) => {
                            if let Some(expr) = stmt.expr_option {
                                self.handle_expr(function_body, expr, &mut usage)
                            };
                        }
                    };
                }
                if let Some(expr_id) = expr.tail {
                    self.handle_expr(function_body, expr_id, &mut usage)
                }
                for (member_path, _) in usage.usage.clone() {
                    // Prune introductions and members of other usages from usages.
                    if usage.introductions.contains(&member_path.base_var())
                        || get_containing_member(&member_path, &usage.usage).is_some()
                    {
                        usage.usage.swap_remove(&member_path);
                    }
                }
                for (member_path, _) in usage.changes.clone() {
                    // Prune introductions and members of other changes from changes.
                    if usage.introductions.contains(&member_path.base_var())
                        || get_containing_member(&member_path, &usage.changes).is_some()
                    {
                        usage.changes.swap_remove(&member_path);
                    } else if let Some(including_usage) =
                        get_containing_member(&member_path, &usage.usage)
                    {
                        // TODO(orizi): Instead of expanding the change to be the containing usage,
                        // we could split the usage into all its members so we'd include the change.
                        usage.changes.swap_remove(&member_path);
                        usage.changes.insert(
                            including_usage.clone(),
                            usage.usage.get(including_usage).unwrap().clone(),
                        );
                    }
                }

                for (path, expr) in usage.usage.iter() {
                    current.usage.insert(path.clone(), expr.clone());
                }
                for (path, expr) in usage.changes.iter() {
                    current.changes.insert(path.clone(), expr.clone());
                }

                self.block_usages.insert(expr_id, usage);
            }
            Expr::Loop(expr) => self.handle_expr(function_body, expr.body, current),
            Expr::FunctionCall(expr) => {
                for arg in &expr.args {
                    match arg {
                        ExprFunctionCallArg::Reference(member_path) => {
                            current.usage.insert(member_path.into(), member_path.clone());
                            current.changes.insert(member_path.into(), member_path.clone());
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
                current
                    .usage
                    .insert(MemberPath::Var(expr.var), ExprVarMemberPath::Var(expr.clone()));
            }
            Expr::Literal(_) => {}
            Expr::MemberAccess(expr) => {
                if let Some(member_path) = &expr.member_path {
                    current.usage.insert(member_path.into(), member_path.clone());
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
                if let Some(inner_pattern) = &pat.inner_pattern {
                    Self::handle_pattern(inner_pattern, current);
                }
            }
            Pattern::Otherwise(_) => {}
        }
    }
}

/// Returns the member path in `usage` that `path` is a member of, if any.
fn get_containing_member<'a>(
    mut path: &'a MemberPath,
    usage: &'a OrderedHashMap<MemberPath, ExprVarMemberPath>,
) -> Option<&'a MemberPath> {
    while let MemberPath::Member { parent, .. } = path {
        path = parent.as_ref();
        if usage.contains_key(path) {
            return Some(path);
        }
    }
    None
}
