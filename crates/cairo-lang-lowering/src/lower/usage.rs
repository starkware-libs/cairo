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
use id_arena::Arena;
use semantic::{ConcreteStructId, PatternId};

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
#[derive(Clone, Debug, Default, DebugWithDb)]
#[debug_db(ExprFormatter<'a>)]
pub struct Usage {
    /// Member paths that are read.
    pub usage: OrderedHashMap<MemberPath, ExprVarMemberPath>,
    /// Member paths that are assigned to.
    pub changes: OrderedHashMap<MemberPath, ExprVarMemberPath>,
    /// Variables that are defined.
    pub introductions: OrderedHashSet<VarId>,
}

impl Usage {
    /// Adds the usage and changes from 'usage' to self, Ignoring `introductions`.
    pub fn add_usage_and_changes(&mut self, usage: &Usage) {
        for (path, expr) in usage.usage.iter() {
            self.usage.insert(path.clone(), expr.clone());
        }
        for (path, expr) in usage.changes.iter() {
            self.changes.insert(path.clone(), expr.clone());
        }
    }

    /// Removes usage that was introduced current block and usage that is already covered
    /// by containing variables.
    pub fn finalize_as_scope(&mut self) {
        for (member_path, _) in self.usage.clone() {
            // Prune introductions from usages.
            if self.introductions.contains(&member_path.base_var()) {
                self.usage.swap_remove(&member_path);
                continue;
            }

            // Prune usages that are members of other usages.
            let mut current_path = member_path.clone();
            while let MemberPath::Member { parent, .. } = current_path {
                current_path = *parent.clone();
                if self.usage.contains_key(&current_path) {
                    self.usage.swap_remove(&member_path);
                    break;
                }
            }
        }
        for (member_path, _) in self.changes.clone() {
            // Prune introductions from changes.
            if self.introductions.contains(&member_path.base_var()) {
                self.changes.swap_remove(&member_path);
            }

            // Prune changes that are members of other changes.
            let mut current_path = member_path.clone();
            while let MemberPath::Member { parent, .. } = current_path {
                current_path = *parent.clone();
                if self.changes.contains_key(&current_path) {
                    self.changes.swap_remove(&member_path);
                    break;
                }
            }
        }
    }
}

/// Usages of variables and member paths in each semantic block of a function.
#[derive(Debug, DebugWithDb)]
#[debug_db(ExprFormatter<'a>)]
pub struct BlockUsages {
    /// Mapping from an [ExprId] for a block expression or loop, to its [Usage].
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
            Expr::FixedSizeArray(expr) => match &expr.items {
                semantic::FixedSizeArrayItems::Items(items) => {
                    for expr_id in items {
                        self.handle_expr(function_body, *expr_id, current);
                    }
                }
                semantic::FixedSizeArrayItems::ValueAndSize(value, _) => {
                    self.handle_expr(function_body, *value, current);
                }
            },
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
                            Self::handle_pattern(&function_body.patterns, stmt.pattern, &mut usage);
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
                usage.finalize_as_scope();
                current.add_usage_and_changes(&usage);
                self.block_usages.insert(expr_id, usage);
            }
            Expr::Loop(expr) => {
                self.handle_expr(function_body, expr.body, current);
                // Copy body usage to loop usage.
                self.block_usages.insert(expr_id, self.block_usages[&expr.body].clone());
            }
            Expr::While(expr) => {
                let mut usage = Default::default();
                match &expr.condition {
                    semantic::Condition::BoolExpr(expr) => {
                        self.handle_expr(function_body, *expr, &mut usage);
                    }
                    semantic::Condition::Let(expr, pattterns) => {
                        self.handle_expr(function_body, *expr, &mut usage);
                        for pattern in pattterns {
                            Self::handle_pattern(&function_body.patterns, *pattern, &mut usage);
                        }
                    }
                }
                self.handle_expr(function_body, expr.body, &mut usage);
                usage.finalize_as_scope();
                current.add_usage_and_changes(&usage);

                self.block_usages.insert(expr_id, usage);
            }
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
                    for pattern in &arm.patterns {
                        Self::handle_pattern(&function_body.patterns, *pattern, current);
                    }
                    self.handle_expr(function_body, arm.expression, current);
                }
            }
            Expr::If(expr) => {
                match &expr.condition {
                    semantic::Condition::BoolExpr(expr) => {
                        self.handle_expr(function_body, *expr, current);
                    }
                    semantic::Condition::Let(expr, patterns) => {
                        self.handle_expr(function_body, *expr, current);
                        for pattern in patterns {
                            Self::handle_pattern(&function_body.patterns, *pattern, current);
                        }
                    }
                }

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
            Expr::Literal(_) | Expr::StringLiteral(_) => {}
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
            Expr::ParamConstant(_) => {}
            Expr::Missing(_) => {}
        }
    }

    fn handle_pattern(arena: &Arena<semantic::Pattern>, pattern: PatternId, current: &mut Usage) {
        let pattern = &arena[pattern];
        match pattern {
            Pattern::Literal(_) | Pattern::StringLiteral(_) => {}
            Pattern::Variable(pattern) => {
                current.introductions.insert(VarId::Local(pattern.var.id));
            }
            Pattern::Struct(pattern) => {
                for (_, pattern) in &pattern.field_patterns {
                    Self::handle_pattern(arena, *pattern, current);
                }
            }
            Pattern::Tuple(pattern) => {
                for pattern in &pattern.field_patterns {
                    Self::handle_pattern(arena, *pattern, current);
                }
            }
            Pattern::FixedSizeArray(pattern) => {
                for pattern in &pattern.elements_patterns {
                    Self::handle_pattern(arena, *pattern, current);
                }
            }
            Pattern::EnumVariant(pattern) => {
                if let Some(inner_pattern) = &pattern.inner_pattern {
                    Self::handle_pattern(arena, *inner_pattern, current);
                }
            }
            Pattern::Otherwise(_) => {}
            Pattern::Missing(_) => {}
        }
    }
}
