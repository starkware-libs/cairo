//! Introduces [Usages], which is responsible for computing variables usage in semantic blocks\
//! of a function.

use cairo_lang_defs::ids::MemberId;
use cairo_lang_proc_macros::DebugWithDb;
use cairo_lang_utils::extract_matches;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;

use crate::expr::fmt::ExprFormatter;
use crate::expr::objects::Arenas;
use crate::{
    ConcreteStructId, Condition, Expr, ExprClosure, ExprFor, ExprFunctionCall, ExprFunctionCallArg,
    ExprId, ExprLoop, ExprVarMemberPath, ExprWhile, FixedSizeArrayItems, FunctionBody, Parameter,
    Pattern, PatternArena, PatternId, Statement, StatementBreak, StatementExpr, StatementLet,
    StatementReturn, VarId,
};

#[cfg(test)]
mod test;

/// Member path (e.g. a.b.c). Unlike [ExprVarMemberPath], this is not an expression, and has no
/// syntax pointers.
#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(ExprFormatter<'db>)]
pub enum MemberPath<'db> {
    Var(VarId<'db>),
    Member {
        parent: Box<MemberPath<'db>>,
        member_id: MemberId<'db>,
        concrete_struct_id: ConcreteStructId<'db>,
    },
}
impl<'db> MemberPath<'db> {
    pub fn base_var(&self) -> VarId<'db> {
        match self {
            MemberPath::Var(var) => *var,
            MemberPath::Member { parent, .. } => parent.base_var(),
        }
    }
}
impl<'db> From<&ExprVarMemberPath<'db>> for MemberPath<'db> {
    fn from(value: &ExprVarMemberPath<'db>) -> Self {
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
#[debug_db(ExprFormatter<'db>)]
pub struct Usage<'db> {
    /// Member paths that are read.
    pub usage: OrderedHashMap<MemberPath<'db>, ExprVarMemberPath<'db>>,
    /// Member paths that are assigned to.
    pub changes: OrderedHashMap<MemberPath<'db>, ExprVarMemberPath<'db>>,
    /// Member paths that are read as snapshots.
    pub snap_usage: OrderedHashMap<MemberPath<'db>, ExprVarMemberPath<'db>>,
    /// Variables that are defined.
    pub introductions: OrderedHashSet<VarId<'db>>,
    /// indicates that the expression has an early return.
    pub has_early_return: bool,
}

impl<'db> Usage<'db> {
    /// Adds the usage and changes from 'usage' to self, Ignoring `introductions`.
    pub fn add_usage_and_changes(&mut self, usage: &Usage<'db>) {
        for (path, expr) in usage.usage.iter() {
            self.usage.insert(path.clone(), expr.clone());
        }
        for (path, expr) in usage.changes.iter() {
            self.changes.insert(path.clone(), expr.clone());
        }
        for (path, expr) in usage.snap_usage.iter() {
            self.snap_usage.insert(path.clone(), expr.clone());
        }
        self.has_early_return |= usage.has_early_return;
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
        for (member_path, _) in self.snap_usage.clone() {
            // Prune usages from snap_usage.
            if self.usage.contains_key(&member_path) {
                self.snap_usage.swap_remove(&member_path);
                continue;
            }

            // Prune introductions from snap_usage.
            if self.introductions.contains(&member_path.base_var()) {
                self.snap_usage.swap_remove(&member_path);
            }

            // Prune snap_usage that are members of other snap_usage or usages.
            let mut current_path = member_path.clone();
            while let MemberPath::Member { parent, .. } = current_path {
                current_path = *parent.clone();
                if self.snap_usage.contains_key(&current_path)
                    | self.usage.contains_key(&current_path)
                {
                    self.snap_usage.swap_remove(&member_path);
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
            // Also if a child is changed and its parent is used, then we change the parent.
            // TODO(TomerStarkware): Deconstruct the parent, and snap_use other members.
            let mut current_path = member_path.clone();
            while let MemberPath::Member { parent, .. } = current_path {
                current_path = *parent.clone();
                if self.snap_usage.contains_key(&current_path) {
                    // Note that current_path must be top most usage as we prune snap_usage and
                    // usage.
                    if let Some(value) = self.snap_usage.swap_remove(&current_path) {
                        self.usage.insert(current_path.clone(), value.clone());
                        self.changes.insert(current_path.clone(), value);
                    };
                }
                if self.changes.contains_key(&current_path) {
                    self.changes.swap_remove(&member_path);
                    break;
                }
            }
        }
    }
}

/// Usages of member paths in expressions of interest, currently loops and closures.
#[derive(Debug, DebugWithDb)]
#[debug_db(ExprFormatter<'db>)]
pub struct Usages<'db> {
    /// Mapping from an [ExprId] to its [Usage].
    pub usages: OrderedHashMap<ExprId, Usage<'db>>,
}
impl<'db> Usages<'db> {
    pub fn from_function_body(function_body: &FunctionBody<'db>) -> Self {
        let mut current = Usage::default();
        let mut usages = Self { usages: Default::default() };
        usages.handle_expr(&function_body.arenas, function_body.body_expr, &mut current);
        usages
    }

    pub fn handle_closure(
        &mut self,
        arenas: &Arenas<'db>,
        param_ids: &[Parameter<'db>],
        body: ExprId,
    ) -> Usage<'db> {
        let mut usage: Usage<'_> = Default::default();

        usage.introductions.extend(param_ids.iter().map(|param| VarId::Param(param.id)));
        self.handle_expr(arenas, body, &mut usage);
        usage.finalize_as_scope();
        usage
    }

    fn handle_expr(
        &mut self,
        arenas: &Arenas<'db>,
        curr_expr_id: ExprId,
        current: &mut Usage<'db>,
    ) {
        match &arenas.exprs[curr_expr_id] {
            Expr::Tuple(expr) => {
                for expr_id in &expr.items {
                    self.handle_expr(arenas, *expr_id, current);
                }
            }
            Expr::FixedSizeArray(expr) => match &expr.items {
                FixedSizeArrayItems::Items(items) => {
                    for expr_id in items {
                        self.handle_expr(arenas, *expr_id, current);
                    }
                }
                FixedSizeArrayItems::ValueAndSize(value, _) => {
                    self.handle_expr(arenas, *value, current);
                }
            },
            Expr::Snapshot(expr) => {
                let expr_id = expr.inner;

                match &arenas.exprs[expr_id] {
                    Expr::Var(expr_var) => {
                        current.snap_usage.insert(
                            MemberPath::Var(expr_var.var),
                            ExprVarMemberPath::Var(expr_var.clone()),
                        );
                    }
                    Expr::MemberAccess(expr) => {
                        if let Some(member_path) = &expr.member_path {
                            current.snap_usage.insert(member_path.into(), member_path.clone());
                        } else {
                            self.handle_expr(arenas, expr.expr, current);
                        }
                    }
                    _ => self.handle_expr(arenas, expr_id, current),
                }
            }
            Expr::Desnap(expr) => self.handle_expr(arenas, expr.inner, current),
            Expr::Assignment(expr) => {
                self.handle_expr(arenas, expr.rhs, current);
                current.usage.insert((&expr.ref_arg).into(), expr.ref_arg.clone());
                current.changes.insert((&expr.ref_arg).into(), expr.ref_arg.clone());
            }
            Expr::LogicalOperator(expr) => {
                self.handle_expr(arenas, expr.lhs, current);
                self.handle_expr(arenas, expr.rhs, current);
            }
            Expr::Block(expr) => {
                let mut usage = Default::default();
                for stmt in &expr.statements {
                    match &arenas.statements[*stmt] {
                        Statement::Let(StatementLet {
                            pattern,
                            expr,
                            else_clause,
                            stable_ptr: _,
                        }) => {
                            self.handle_expr(arenas, *expr, &mut usage);
                            Self::handle_pattern(&arenas.patterns, *pattern, &mut usage);

                            if let Some(else_clause) = else_clause {
                                self.handle_expr(arenas, *else_clause, &mut usage);
                            }
                        }
                        Statement::Expr(StatementExpr { expr, stable_ptr: _ }) => {
                            self.handle_expr(arenas, *expr, &mut usage)
                        }
                        Statement::Continue(_) => (),
                        Statement::Return(StatementReturn { expr_option, stable_ptr: _ }) => {
                            usage.has_early_return = true;
                            if let Some(expr) = expr_option {
                                self.handle_expr(arenas, *expr, &mut usage)
                            };
                        }
                        Statement::Break(StatementBreak { expr_option, stable_ptr: _ }) => {
                            if let Some(expr) = expr_option {
                                self.handle_expr(arenas, *expr, &mut usage)
                            };
                        }
                        Statement::Item(_) => {}
                    };
                }
                if let Some(expr_id) = expr.tail {
                    self.handle_expr(arenas, expr_id, &mut usage)
                }
                usage.finalize_as_scope();
                current.add_usage_and_changes(&usage);
            }
            Expr::Loop(ExprLoop { body, ty: _, stable_ptr: _ }) => {
                let mut usage = Default::default();
                self.handle_expr(arenas, *body, &mut usage);
                current.add_usage_and_changes(&usage);
                self.usages.insert(curr_expr_id, usage);
            }
            Expr::While(ExprWhile { condition, body, stable_ptr: _, ty: _ }) => {
                let mut usage = Default::default();
                match condition {
                    Condition::BoolExpr(expr) => {
                        self.handle_expr(arenas, *expr, &mut usage);
                    }
                    Condition::Let(expr, patterns) => {
                        self.handle_expr(arenas, *expr, &mut usage);
                        for pattern in patterns {
                            Self::handle_pattern(&arenas.patterns, *pattern, &mut usage);
                        }
                    }
                }
                self.handle_expr(arenas, *body, &mut usage);
                usage.finalize_as_scope();
                current.add_usage_and_changes(&usage);

                self.usages.insert(curr_expr_id, usage);
            }
            Expr::For(ExprFor {
                expr_id,
                into_iter_member_path,
                pattern,
                body,
                stable_ptr: _,
                into_iter: _,
                next_function_id: _,
                ty: _,
            }) => {
                self.handle_expr(arenas, *expr_id, current);
                current
                    .introductions
                    .insert(extract_matches!(into_iter_member_path, ExprVarMemberPath::Var).var);
                let mut usage: Usage<'_> = Default::default();
                usage.usage.insert(into_iter_member_path.into(), into_iter_member_path.clone());
                usage.changes.insert(into_iter_member_path.into(), into_iter_member_path.clone());
                Self::handle_pattern(&arenas.patterns, *pattern, &mut usage);
                self.handle_expr(arenas, *body, &mut usage);
                usage.finalize_as_scope();
                current.add_usage_and_changes(&usage);
                self.usages.insert(curr_expr_id, usage);
            }
            Expr::ExprClosure(ExprClosure { body, params, stable_ptr: _, ty: _ }) => {
                let usage = self.handle_closure(arenas, params, *body);

                current.add_usage_and_changes(&usage);
                self.usages.insert(curr_expr_id, usage);
            }
            Expr::FunctionCall(ExprFunctionCall {
                args,
                function: _,
                coupon_arg: _,
                stable_ptr: _,
                ty: _,
            }) => {
                for arg in args {
                    match arg {
                        ExprFunctionCallArg::Reference(member_path) => {
                            current.usage.insert(member_path.into(), member_path.clone());
                            current.changes.insert(member_path.into(), member_path.clone());
                        }
                        ExprFunctionCallArg::Value(expr) => {
                            self.handle_expr(arenas, *expr, current)
                        }
                    }
                }
            }
            Expr::Match(expr) => {
                self.handle_expr(arenas, expr.matched_expr, current);
                for arm in &expr.arms {
                    for pattern in &arm.patterns {
                        Self::handle_pattern(&arenas.patterns, *pattern, current);
                    }
                    self.handle_expr(arenas, arm.expression, current);
                }
            }
            Expr::If(expr) => {
                for condition in &expr.conditions {
                    match condition {
                        Condition::BoolExpr(expr) => {
                            self.handle_expr(arenas, *expr, current);
                        }
                        Condition::Let(expr, patterns) => {
                            self.handle_expr(arenas, *expr, current);
                            for pattern in patterns {
                                Self::handle_pattern(&arenas.patterns, *pattern, current);
                            }
                        }
                    }
                }

                self.handle_expr(arenas, expr.if_block, current);
                if let Some(expr) = expr.else_block {
                    self.handle_expr(arenas, expr, current);
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
                    self.handle_expr(arenas, expr.expr, current);
                }
            }
            Expr::StructCtor(expr) => {
                for (expr_id, _) in &expr.members {
                    self.handle_expr(arenas, *expr_id, current);
                }
                if let Some(base) = &expr.base_struct {
                    self.handle_expr(arenas, *base, current);
                }
            }
            Expr::EnumVariantCtor(expr) => self.handle_expr(arenas, expr.value_expr, current),
            Expr::PropagateError(expr) => {
                current.has_early_return = true;
                self.handle_expr(arenas, expr.inner, current)
            }
            Expr::Constant(_) => {}
            Expr::Missing(_) => {}
        }
    }

    fn handle_pattern(arena: &PatternArena<'db>, pattern: PatternId, current: &mut Usage<'db>) {
        let pattern = &arena[pattern];
        match pattern {
            Pattern::Literal(_) | Pattern::StringLiteral(_) => {}
            Pattern::Variable(pattern) => {
                current.introductions.insert(VarId::Local(pattern.var.id));
            }
            Pattern::Struct(pattern) => {
                for (pattern, _) in &pattern.field_patterns {
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
