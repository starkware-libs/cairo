//! This module is responsible of computing the semantic model of expressions and statements in
//! the code, while type checking.
//! It is invoked by queries for function bodies and other code blocks.

use std::collections::HashMap;

use ast::{BinaryOperator, PathSegment};
use defs::ids::{LocalVarLongId, MemberId};
use id_arena::Arena;
use itertools::zip_eq;
use smol_str::SmolStr;
use syntax::node::db::SyntaxGroup;
use syntax::node::helpers::{GetIdentifier, PathSegmentEx};
use syntax::node::{ast, Terminal, TypedSyntaxNode};
use utils::ordered_hash_map::OrderedHashMap;
use utils::{try_extract_matches, OptionHelper};

use super::objects::*;
use super::pattern::{
    Pattern, PatternEnum, PatternLiteral, PatternOtherwise, PatternTuple, PatternVariable,
};
use crate::corelib::{
    core_binary_operator, core_felt_ty, false_literal_expr, true_literal_expr, unit_ty,
};
use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnosticKind::*;
use crate::diagnostic::SemanticDiagnostics;
use crate::items::enm::SemanticEnumEx;
use crate::items::modifiers::compute_modifiers;
use crate::items::strct::SemanticStructEx;
use crate::resolve_path::{ResolvedConcreteItem, ResolvedGenericItem, Resolver};
use crate::semantic::{self, FunctionId, LocalVariable, TypeId, TypeLongId, Variable};
use crate::types::{resolve_type, ConcreteTypeId};

/// Context for computing the semantic model of expression trees.
pub struct ComputationContext<'ctx> {
    pub db: &'ctx dyn SemanticGroup,
    pub diagnostics: &'ctx mut SemanticDiagnostics,
    pub resolver: Resolver<'ctx>,
    return_ty: TypeId,
    environment: Box<Environment>,
    pub exprs: Arena<semantic::Expr>,
    pub statements: Arena<semantic::Statement>,
}
impl<'ctx> ComputationContext<'ctx> {
    pub fn new(
        db: &'ctx dyn SemanticGroup,
        diagnostics: &'ctx mut SemanticDiagnostics,
        resolver: Resolver<'ctx>,
        return_ty: TypeId,
        environment: Environment,
    ) -> Self {
        Self {
            db,
            diagnostics,
            resolver,
            return_ty,
            environment: Box::new(environment),
            exprs: Arena::default(),
            statements: Arena::default(),
        }
    }

    /// Runs a function with a modified context, with a new environment for a subscope.
    /// This environment holds no variable of its own, but points to the current environment as a
    /// parent.
    /// Used for block expressions.
    fn run_in_subscope<T, F>(&mut self, f: F) -> T
    where
        F: FnOnce(&mut Self) -> T,
    {
        // Push an environment to the stack.
        let new_environment = Box::new(Environment { parent: None, variables: HashMap::new() });
        let old_environment = std::mem::replace(&mut self.environment, new_environment);
        self.environment.parent = Some(old_environment);

        let res = f(self);

        // Pop the environment from the stack.
        let parent = self.environment.parent.take();
        self.environment = parent.unwrap();
        res
    }
}

pub type EnvVariables = HashMap<SmolStr, Variable>;

// TODO(spapini): Consider using identifiers instead of SmolStr everywhere in the code.
/// A state which contains all the variables defined at the current resolver until now, and a
/// pointer to the parent environment.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Environment {
    parent: Option<Box<Environment>>,
    variables: EnvVariables,
}
impl Environment {
    pub fn new(variables: EnvVariables) -> Self {
        Self { parent: None, variables }
    }
}

/// Computes the semantic model of an expression.
pub fn compute_expr_semantic(ctx: &mut ComputationContext<'_>, syntax: &ast::Expr) -> Expr {
    maybe_compute_expr_semantic(ctx, syntax).unwrap_or_else(|| {
        Expr::Missing(ExprMissing { ty: TypeId::missing(ctx.db), stable_ptr: syntax.stable_ptr() })
    })
}

/// Computes the semantic model of an expression, or returns a SemanticDiagnosticKind on error,
pub fn maybe_compute_expr_semantic(
    ctx: &mut ComputationContext<'_>,
    syntax: &ast::Expr,
) -> Option<Expr> {
    let db = ctx.db;
    let syntax_db = db.upcast();
    // TODO: When Expr holds the syntax pointer, add it here as well.
    match syntax {
        ast::Expr::Path(path) => resolve_variable(ctx, path),
        ast::Expr::Literal(literal_syntax) => {
            Some(Expr::Literal(literal_to_semantic(ctx, literal_syntax)?))
        }
        ast::Expr::False(syntax) => Some(false_literal_expr(ctx, syntax.stable_ptr().into())),
        ast::Expr::True(syntax) => Some(true_literal_expr(ctx, syntax.stable_ptr().into())),
        ast::Expr::Parenthesized(paren_syntax) => {
            maybe_compute_expr_semantic(ctx, &paren_syntax.expr(syntax_db))
        }
        ast::Expr::Unary(_) => {
            ctx.diagnostics.report(syntax, Unsupported);
            None
        }
        ast::Expr::Binary(binary_op_syntax) => compute_expr_binary_semantic(ctx, binary_op_syntax),
        ast::Expr::Tuple(tuple_syntax) => compute_expr_tuple_semantic(ctx, tuple_syntax),
        ast::Expr::FunctionCall(call_syntax) => {
            compute_expr_function_call_semantic(ctx, call_syntax)
        }
        ast::Expr::StructCtorCall(ctor_syntax) => struct_ctor_expr(ctx, ctor_syntax),
        ast::Expr::Block(block_syntax) => compute_expr_block_semantic(ctx, block_syntax),
        ast::Expr::Match(expr_match) => compute_expr_match_semantic(ctx, expr_match),
        ast::Expr::If(expr_if) => compute_expr_if_semantic(ctx, expr_if),
        ast::Expr::Missing(_) => {
            ctx.diagnostics.report(syntax, Unsupported);
            None
        }
    }
}

fn compute_expr_binary_semantic(
    ctx: &mut ComputationContext<'_>,
    syntax: &ast::ExprBinary,
) -> Option<Expr> {
    let db = ctx.db;
    let syntax_db = db.upcast();

    let stable_ptr = syntax.stable_ptr().into();
    let binary_op = syntax.op(syntax_db);
    let lhs_syntax = &syntax.lhs(syntax_db);
    let lexpr = compute_expr_semantic(ctx, lhs_syntax);
    let rhs_syntax = syntax.rhs(syntax_db);
    if matches!(binary_op, BinaryOperator::Dot(_)) {
        return member_access_expr(ctx, lexpr, rhs_syntax, stable_ptr);
    }
    let rexpr = compute_expr_semantic(ctx, &rhs_syntax);
    if matches!(binary_op, BinaryOperator::Eq(_)) {
        return match lexpr {
            Expr::Var(ExprVar { var, .. }) => Some(Expr::Assignment(ExprAssignment {
                var,
                rhs: ctx.exprs.alloc(rexpr),
                ty: unit_ty(db),
                stable_ptr,
            })),
            _ => {
                ctx.diagnostics.report(lhs_syntax, InvalidLhsForAssignment);
                None
            }
        };
    }
    let function = core_binary_operator(db, &binary_op)
        .on_none(|| ctx.diagnostics.report(&binary_op, UnknownBinaryOperator))?;
    expr_function_call(ctx, function, vec![lexpr, rexpr], stable_ptr)
}

fn compute_expr_tuple_semantic(
    ctx: &mut ComputationContext<'_>,
    syntax: &ast::ExprTuple,
) -> Option<Expr> {
    let db = ctx.db;
    let syntax_db = db.upcast();

    let mut items: Vec<ExprId> = vec![];
    let mut types: Vec<TypeId> = vec![];
    for expr_syntax in syntax.expressions(syntax_db).elements(syntax_db) {
        let expr_semantic = compute_expr_semantic(ctx, &expr_syntax);
        types.push(expr_semantic.ty());
        items.push(ctx.exprs.alloc(expr_semantic));
    }
    Some(Expr::Tuple(ExprTuple {
        items,
        ty: db.intern_type(TypeLongId::Tuple(types)),
        stable_ptr: syntax.stable_ptr().into(),
    }))
}

fn compute_expr_function_call_semantic(
    ctx: &mut ComputationContext<'_>,
    syntax: &ast::ExprFunctionCall,
) -> Option<Expr> {
    let db = ctx.db;
    let syntax_db = db.upcast();

    let path = syntax.path(syntax_db);
    let item = ctx.resolver.resolve_concrete_path(ctx.diagnostics, &path)?;
    let args_syntax = syntax.arguments(syntax_db);
    let arg_exprs: Vec<_> = args_syntax
        .expressions(syntax_db)
        .elements(syntax_db)
        .into_iter()
        .map(|arg_syntax| compute_expr_semantic(ctx, &arg_syntax))
        .collect();
    match item {
        ResolvedConcreteItem::Function(function) => {
            expr_function_call(ctx, function, arg_exprs, syntax.stable_ptr().into())
        }
        ResolvedConcreteItem::Variant(concrete_variant) => {
            if arg_exprs.len() != 1 {
                ctx.diagnostics.report(
                    &args_syntax,
                    WrongNumberOfArguments { expected: 1, actual: arg_exprs.len() },
                );
                return None;
            }
            let arg = arg_exprs[0].clone();
            if concrete_variant.ty != arg.ty() {
                ctx.diagnostics.report(
                    &args_syntax,
                    WrongArgumentType { expected_ty: concrete_variant.ty, actual_ty: arg.ty() },
                );
                return None;
            }
            let concrete_enum_id = concrete_variant.concrete_enum_id;
            Some(semantic::Expr::EnumVariantCtor(semantic::ExprEnumVariantCtor {
                variant: concrete_variant,
                value_expr: ctx.exprs.alloc(arg),
                ty: db.intern_type(TypeLongId::Concrete(ConcreteTypeId::Enum(concrete_enum_id))),
                stable_ptr: syntax.stable_ptr().into(),
            }))
        }
        _ => {
            ctx.diagnostics.report(&path, NotAFunction);
            None
        }
    }
}

/// Computes the semantic model of an expression of type [ast::ExprBlock].
pub fn compute_expr_block_semantic(
    ctx: &mut ComputationContext<'_>,
    syntax: &ast::ExprBlock,
) -> Option<Expr> {
    let db = ctx.db;
    let syntax_db = db.upcast();

    ctx.run_in_subscope(|new_ctx| {
        let mut statements = syntax.statements(syntax_db).elements(syntax_db);
        // Remove the tail expression, if exists.
        // TODO(spapini): Consider splitting tail expression in the parser.
        let tail = get_tail_expression(syntax_db, statements.as_slice());
        if tail.is_some() {
            statements.pop();
        }

        // Convert statements to semantic model.
        let statements_semantic: Vec<_> = statements
            .into_iter()
            .filter_map(|statement_syntax| compute_statement_semantic(new_ctx, statement_syntax))
            .collect();

        // Convert tail expression (if exists) to semantic model.
        let tail_semantic_expr = tail.map(|tail_expr| compute_expr_semantic(new_ctx, &tail_expr));
        let ty = if let Some(t) = &tail_semantic_expr {
            t.ty()
        } else if let Some(statement) = statements_semantic.last() {
            if let Statement::Return(_) = &new_ctx.statements[*statement] {
                TypeId::never(new_ctx.db)
            } else {
                unit_ty(db)
            }
        } else {
            unit_ty(db)
        };
        Some(Expr::Block(ExprBlock {
            statements: statements_semantic,
            tail: tail_semantic_expr.map(|expr| new_ctx.exprs.alloc(expr)),
            ty,
            stable_ptr: syntax.stable_ptr().into(),
        }))
    })
}

/// Helper for merging the return types of branch blocks (match or if else).
struct FlowMergeTypeHelper {
    never_type: TypeId,
    missing_type: TypeId,
    final_type: Option<TypeId>,
}
impl FlowMergeTypeHelper {
    fn new(db: &dyn SemanticGroup) -> Self {
        Self { never_type: TypeId::never(db), missing_type: TypeId::missing(db), final_type: None }
    }

    /// Attempt merge a branch into the helper, on error will return the conflicting types.
    fn try_merge_types(&mut self, ty: TypeId) -> Result<(), (TypeId, TypeId)> {
        if ty != self.never_type && ty != self.missing_type {
            if let Some(existing) = &self.final_type {
                if ty != *existing {
                    return Err((*existing, ty));
                }
            } else {
                self.final_type = Some(ty);
            }
        }
        Ok(())
    }

    /// Returns the merged type.
    fn get_final_type(self) -> TypeId {
        self.final_type.unwrap_or(self.never_type)
    }
}

fn compute_expr_match_semantic(
    ctx: &mut ComputationContext<'_>,
    syntax: &ast::ExprMatch,
) -> Option<Expr> {
    // TODO(yuval): verify exhaustiveness.
    let db = ctx.db;
    let syntax_db = db.upcast();

    let syntax_arms = syntax.arms(syntax_db).elements(syntax_db);
    let expr = compute_expr_semantic(ctx, &syntax.expr(syntax_db));
    // Run compute_pattern_semantic on every arm, even if other arms failed, to get as many
    // diagnostics as possible.
    let pattern_and_expr_options: Vec<_> = syntax_arms
        .iter()
        .map(|syntax_arm| {
            let arm_expr_syntax = syntax_arm.expression(syntax_db);
            ctx.run_in_subscope(|new_ctx| {
                // Typecheck pattern, and introduce the new variables to the subscope.
                // Note that if the arm expr is a block, there will be *another* subscope
                // for it.
                let pattern =
                    compute_pattern_semantic(new_ctx, syntax_arm.pattern(syntax_db), expr.ty())?;
                let variables = pattern.variables();
                for v in variables {
                    new_ctx
                        .environment
                        .variables
                        .insert(v.name.clone(), Variable::Local(v.var.clone()));
                }
                let arm_expr = compute_expr_semantic(new_ctx, &arm_expr_syntax);
                Some((pattern, arm_expr))
            })
        })
        .collect();
    // Unify arm types.
    let mut helper = FlowMergeTypeHelper::new(ctx.db);
    for (_, expr) in pattern_and_expr_options.iter().flatten() {
        if let Err((match_ty, arm_ty)) = helper.try_merge_types(expr.ty()) {
            ctx.diagnostics.report_by_ptr(
                expr.stable_ptr().untyped(),
                IncompatibleMatchArms { match_ty, arm_ty },
            );
        }
    }
    // Compute semantic representation of the match arms.
    let pattern_and_exprs: Vec<_> = pattern_and_expr_options.into_iter().collect::<Option<_>>()?;
    let semantic_arms = pattern_and_exprs
        .into_iter()
        .map(|(pattern, arm_expr)| MatchArm { pattern, expression: ctx.exprs.alloc(arm_expr) })
        .collect();
    Some(Expr::Match(ExprMatch {
        matched_expr: ctx.exprs.alloc(expr),
        arms: semantic_arms,
        ty: helper.get_final_type(),
        stable_ptr: syntax.stable_ptr().into(),
    }))
}

/// Computes the semantic model of an expression of type [ast::ExprIf].
fn compute_expr_if_semantic(
    ctx: &mut ComputationContext<'_>,
    syntax: &ast::ExprIf,
) -> Option<Expr> {
    let syntax_db = ctx.db.upcast();

    let expr = compute_expr_semantic(ctx, &syntax.condition(syntax_db));
    let if_block = compute_expr_block_semantic(ctx, &syntax.if_block(syntax_db))?;
    let else_block = compute_expr_block_semantic(ctx, &syntax.else_block(syntax_db))?;
    let mut helper = FlowMergeTypeHelper::new(ctx.db);
    helper
        .try_merge_types(if_block.ty())
        .and(helper.try_merge_types(else_block.ty()))
        .unwrap_or_else(|(block_if_ty, block_else_ty)| {
            ctx.diagnostics.report(syntax, IncompatibleIfBlockTypes { block_if_ty, block_else_ty });
        });
    Some(Expr::If(ExprIf {
        condition: ctx.exprs.alloc(expr),
        if_block: ctx.exprs.alloc(if_block),
        else_block: ctx.exprs.alloc(else_block),
        ty: helper.get_final_type(),
        stable_ptr: syntax.stable_ptr().into(),
    }))
}

/// Computes the semantic model of a pattern, or None if invalid.
fn compute_pattern_semantic(
    ctx: &mut ComputationContext<'_>,
    pattern_syntax: ast::Pattern,
    ty: TypeId,
) -> Option<Pattern> {
    // TODO(spapini): Check for missing type, and don't reemit an error.
    let syntax_db = ctx.db.upcast();
    Some(match pattern_syntax {
        ast::Pattern::Underscore(_) => Pattern::Otherwise(PatternOtherwise { ty }),
        ast::Pattern::Literal(literal_pattern) => {
            let literal = literal_to_semantic(ctx, &literal_pattern)?;
            if ty != core_felt_ty(ctx.db) {
                ctx.diagnostics.report(&literal_pattern, UnexpectedLiteralPattern { ty });
                return None;
            }
            Pattern::Literal(PatternLiteral { literal, ty })
        }
        ast::Pattern::Enum(enum_pattern) => {
            // Check that type is an enum, and get the concrete enum from it.
            let concrete_enum =
                try_extract_matches!(ctx.db.lookup_intern_type(ty), TypeLongId::Concrete)
                    .and_then(|c| try_extract_matches!(c, ConcreteTypeId::Enum))
                    .on_none(|| {
                        ctx.diagnostics.report(&enum_pattern, UnexpectedEnumPattern { ty });
                    })?;

            // Extract the enum variant from the path syntax.
            let path = enum_pattern.path(syntax_db);
            let item = ctx.resolver.resolve_generic_path(ctx.diagnostics, &path)?;
            let generic_variant = try_extract_matches!(item, ResolvedGenericItem::Variant)
                .on_none(|| {
                    ctx.diagnostics.report(&path, NotAVariant);
                })?;

            // Check that these are the same enums.
            if generic_variant.enum_id != concrete_enum.enum_id(ctx.db) {
                ctx.diagnostics.report(
                    &path,
                    WrongEnum {
                        expected_enum: concrete_enum.enum_id(ctx.db),
                        actual_enum: generic_variant.enum_id,
                    },
                );
                return None;
            }
            let concrete_variant =
                ctx.db.concrete_enum_variant(concrete_enum, &generic_variant).on_none(|| {
                    ctx.diagnostics.report(&path, UnknownEnum);
                })?;

            // Compute inner pattern.
            let ty = concrete_variant.ty;
            let inner_pattern =
                compute_pattern_semantic(ctx, enum_pattern.pattern(syntax_db), ty)?.into();
            Pattern::Enum(PatternEnum { variant: concrete_variant, inner_pattern, ty })
        }
        ast::Pattern::Path(path) => {
            // A path of length 1 is an identifier, which will result in a variable pattern.
            // Currently, other paths are not supported (and not clear if ever will be).
            if path.elements(syntax_db).len() > 1 {
                ctx.diagnostics.report(&path, Unsupported);
                return None;
            }
            // TODO(spapini): Make sure this is a simple identifier. In particular, no generics.
            let identifier = path.elements(syntax_db)[0].identifier_ast(syntax_db);
            create_variable_pattern(ctx, identifier, &[], ty)
        }
        ast::Pattern::Identifier(identifier) => create_variable_pattern(
            ctx,
            identifier.name(syntax_db),
            &identifier.modifiers(syntax_db).elements(syntax_db),
            ty,
        ),
        ast::Pattern::Struct(pattern_struct) => {
            // Check that type is an struct, and get the concrete struct from it.
            let _concrete_struct =
                try_extract_matches!(ctx.db.lookup_intern_type(ty), TypeLongId::Concrete)
                    .and_then(|c| try_extract_matches!(c, ConcreteTypeId::Struct))
                    .on_none(|| {
                        ctx.diagnostics.report(&pattern_struct, UnexpectedEnumPattern { ty });
                    })?;

            // TODO(spapini): Support struct patterns.
            ctx.diagnostics.report(&pattern_struct, Unsupported);
            return None;
        }
        ast::Pattern::Tuple(pattern_tuple) => {
            let tys = try_extract_matches!(ctx.db.lookup_intern_type(ty), TypeLongId::Tuple)
                .on_none(|| {
                    ctx.diagnostics.report(&pattern_tuple, UnexpectedTuplePattern { ty });
                })?;

            let patterns_ast = pattern_tuple.patterns(syntax_db).elements(syntax_db);
            if tys.len() != patterns_ast.len() {
                ctx.diagnostics.report(
                    &pattern_tuple,
                    WrongNumberOfGenericArguments {
                        expected: tys.len(),
                        actual: patterns_ast.len(),
                    },
                );
                return None;
            }
            // Iterator of Option<Pattern?, for each field.
            let pattern_options = zip_eq(patterns_ast.into_iter(), tys).map(|(pattern_ast, ty)| {
                Some(Box::new(compute_pattern_semantic(ctx, pattern_ast, ty)?))
            });
            // If all are Some, collect into a Vec.
            let field_patterns: Vec<_> = pattern_options.collect::<Option<_>>()?;

            Pattern::Tuple(PatternTuple { field_patterns, ty })
        }
    })
}

/// Creates a variable pattern.
fn create_variable_pattern(
    ctx: &mut ComputationContext<'_>,
    identifier: ast::TerminalIdentifier,
    modifier_list: &[ast::Modifier],
    ty: TypeId,
) -> Pattern {
    let syntax_db = ctx.db.upcast();
    let var_id =
        ctx.db.intern_local_var(LocalVarLongId(ctx.resolver.module_id, identifier.stable_ptr()));

    Pattern::Variable(PatternVariable {
        name: identifier.text(syntax_db),
        var: LocalVariable {
            id: var_id,
            ty,
            modifiers: compute_modifiers(ctx.diagnostics, syntax_db, modifier_list),
        },
    })
}

/// Creates a struct constructor semantic expression from its AST.
fn struct_ctor_expr(
    ctx: &mut ComputationContext<'_>,
    ctor_syntax: &ast::ExprStructCtorCall,
) -> Option<Expr> {
    let db = ctx.db;
    let syntax_db = db.upcast();
    let path = ctor_syntax.path(syntax_db);

    // Extract struct.
    let ty = resolve_type(db, ctx.diagnostics, &mut ctx.resolver, &ast::Expr::Path(path.clone()));

    let concrete_struct = try_extract_matches!(ctx.db.lookup_intern_type(ty), TypeLongId::Concrete)
        .and_then(|c| try_extract_matches!(c, ConcreteTypeId::Struct))
        .on_none(|| {
            ctx.diagnostics.report(&path, NotAStruct);
        })?;

    let members = db.concrete_struct_members(concrete_struct)?;
    let mut member_exprs: OrderedHashMap<MemberId, ExprId> = OrderedHashMap::default();
    for arg in ctor_syntax.arguments(syntax_db).arguments(syntax_db).elements(syntax_db) {
        // TODO: Extract to a function for results.
        let arg = match arg {
            ast::StructArg::StructArgSingle(arg) => arg,
            ast::StructArg::StructArgTail(tail_expr) => {
                ctx.diagnostics.report(&tail_expr, Unsupported);
                continue;
            }
        };
        let arg_identifier = arg.identifier(syntax_db);
        let arg_name = arg_identifier.text(syntax_db);
        // Find struct member by name.
        let member = if let Some(member) = members.get(&arg_name) {
            member
        } else {
            ctx.diagnostics.report(&arg_identifier, UnknownMember);
            continue;
        };
        // Extract expression.
        let arg_expr = match arg.arg_expr(syntax_db) {
            ast::OptionStructArgExpr::Empty(_) => {
                resolve_variable_by_name(ctx, &arg_identifier, &path)?
            }
            ast::OptionStructArgExpr::Some(arg_expr) => {
                compute_expr_semantic(ctx, &arg_expr.expr(syntax_db))
            }
        };
        // Check types.
        if arg_expr.ty() != member.ty {
            ctx.diagnostics.report(
                &arg_identifier,
                WrongArgumentType { expected_ty: member.ty, actual_ty: arg_expr.ty() },
            );
            continue;
        }
        // Insert and check for duplicates.
        if member_exprs.insert(member.id, ctx.exprs.alloc(arg_expr)).is_some() {
            ctx.diagnostics.report(&arg_identifier, MemberSpecifiedMoreThanOnce);
        }
    }
    // Report errors for missing members.
    for (member_name, member) in members.iter() {
        if !member_exprs.contains_key(&member.id) {
            ctx.diagnostics.report(ctor_syntax, MissingMember { member_name: member_name.clone() });
        }
    }
    Some(Expr::StructCtor(ExprStructCtor {
        struct_id: concrete_struct.struct_id(db),
        members: member_exprs.into_iter().collect(),
        ty: db.intern_type(TypeLongId::Concrete(ConcreteTypeId::Struct(concrete_struct))),
        stable_ptr: ctor_syntax.stable_ptr().into(),
    }))
}

/// Returns the tail expression of the given list of statements, if exists.
/// A tail expression is the last statement in the list, if it is an expression and
/// it does not end with a semicolon.
fn get_tail_expression(
    syntax_db: &dyn SyntaxGroup,
    statements: &[ast::Statement],
) -> Option<ast::Expr> {
    let last = statements.last()?;
    let statement_expr = try_extract_matches!(last, ast::Statement::Expr)?;
    try_extract_matches!(statement_expr.semicolon(syntax_db), ast::OptionSemicolon::Empty)?;
    Some(statement_expr.expr(syntax_db))
}

/// Creates the semantic model of a literal expression from its AST.
fn literal_to_semantic(
    ctx: &mut ComputationContext<'_>,
    literal_syntax: &ast::TerminalLiteralNumber,
) -> Option<ExprLiteral> {
    let db = ctx.db;
    let syntax_db = db.upcast();
    let text = literal_syntax.text(syntax_db);
    let value = text
        .parse::<usize>()
        .ok()
        .on_none(|| ctx.diagnostics.report(literal_syntax, UnknownLiteral))?;
    let ty = db.core_felt_ty();
    Some(ExprLiteral { value, ty, stable_ptr: literal_syntax.stable_ptr().into() })
}

/// Given an expression syntax, if it's an identifier, returns it. Otherwise, returns the proper
/// error.
fn expr_as_identifier(
    ctx: &mut ComputationContext<'_>,
    rhs_syntax: &ast::Expr,
    syntax_db: &dyn SyntaxGroup,
) -> Option<SmolStr> {
    if let ast::Expr::Path(path) = rhs_syntax {
        let segments = path.elements(syntax_db);
        if segments.len() == 1 {
            return Some(segments[0].identifier(syntax_db));
        }
    };
    ctx.diagnostics.report(rhs_syntax, InvalidMemberExpression);
    None
}

// TODO(spapini): Consider moving some checks here to the responsibility of the parser.
/// Computes the semantic model of a member access expression (e.g. "expr.member").
fn member_access_expr(
    ctx: &mut ComputationContext<'_>,
    lexpr: Expr,
    rhs_syntax: ast::Expr,
    stable_ptr: ast::ExprPtr,
) -> Option<Expr> {
    let syntax_db = ctx.db.upcast();

    // Find MemberId.
    let member_name = expr_as_identifier(ctx, &rhs_syntax, syntax_db)?;
    match ctx.db.lookup_intern_type(lexpr.ty()) {
        TypeLongId::Concrete(concrete) => match concrete {
            ConcreteTypeId::Struct(concrete_struct_id) => {
                let member = ctx
                    .db
                    .concrete_struct_members(concrete_struct_id)
                    .and_then(|members| members.get(&member_name).cloned())
                    .on_none(|| {
                        ctx.diagnostics.report(
                            &rhs_syntax,
                            NoSuchMember {
                                struct_id: concrete_struct_id.struct_id(ctx.db),
                                member_name,
                            },
                        )
                    })?;
                let lexpr_id = ctx.exprs.alloc(lexpr);
                return Some(Expr::MemberAccess(ExprMemberAccess {
                    expr: lexpr_id,
                    member: member.id,
                    ty: member.ty,
                    stable_ptr,
                }));
            }
            _ => {
                ctx.diagnostics
                    .report(&rhs_syntax, TypeHasNoMembers { ty: lexpr.ty(), member_name });
            }
        },
        TypeLongId::Tuple(_) => {
            // TODO(spapini): Handle .0, .1, ...;
            ctx.diagnostics.report(&rhs_syntax, Unsupported);
        }
        TypeLongId::GenericParameter(_) => {
            ctx.diagnostics.report(&rhs_syntax, TypeHasNoMembers { ty: lexpr.ty(), member_name });
        }
        TypeLongId::Missing | TypeLongId::Never => {}
    }
    None
}

/// Resolves a variable given a context and a path expression.
fn resolve_variable(ctx: &mut ComputationContext<'_>, path: &ast::ExprPath) -> Option<Expr> {
    let db = ctx.db;
    let syntax_db = db.upcast();
    let segments = path.elements(syntax_db);
    if segments.len() != 1 {
        ctx.diagnostics.report(path, Unsupported);
        return None;
    }

    match &segments[0] {
        PathSegment::Simple(ident_segment) => {
            resolve_variable_by_name(ctx, &ident_segment.ident(syntax_db), path)
        }
        PathSegment::WithGenericArgs(generic_args_segment) => {
            // TODO(ilya, 10/10/2022): Generics are not supported yet.
            ctx.diagnostics.report(generic_args_segment, Unsupported);
            None
        }
    }
}

/// Resolves a variable given a context and a simple name.
pub fn resolve_variable_by_name(
    ctx: &mut ComputationContext<'_>,
    identifier: &ast::TerminalIdentifier,
    path: &ast::ExprPath,
) -> Option<Expr> {
    let variable_name = identifier.text(ctx.db.upcast());
    let mut maybe_env = Some(&*ctx.environment);
    while let Some(env) = maybe_env {
        if let Some(var) = env.variables.get(&variable_name) {
            return Some(Expr::Var(ExprVar {
                var: var.id(),
                ty: var.ty(),
                stable_ptr: path.stable_ptr().into(),
            }));
        }
        maybe_env = env.parent.as_deref();
    }
    ctx.diagnostics.report(identifier, VariableNotFound { name: variable_name });
    None
}

/// Typechecks a function call.
fn expr_function_call(
    ctx: &mut ComputationContext<'_>,
    function: FunctionId,
    arg_exprs: Vec<Expr>,
    stable_ptr: ast::ExprPtr,
) -> Option<Expr> {
    // TODO(spapini): Better location for these diagnsotics after the refactor for generics resolve.
    let signature = ctx
        .db
        .concrete_function_signature(function)
        .on_none(|| ctx.diagnostics.report_by_ptr(stable_ptr.untyped(), UnknownFunction))?;

    if arg_exprs.len() != signature.params.len() {
        ctx.diagnostics.report_by_ptr(
            stable_ptr.untyped(),
            WrongNumberOfArguments { expected: signature.params.len(), actual: arg_exprs.len() },
        );
        return None;
    }

    // Check argument types.
    let mut ref_args = Vec::new();
    let mut args = Vec::new();
    for (arg, param) in arg_exprs.into_iter().zip(signature.params.iter()) {
        let arg_typ = arg.ty();
        let param_typ = param.ty;
        // Don't add diagnostic if the type is missing (a diagnostic should have already been
        // added).
        // TODO(lior): Add a test to missing type once possible.
        if arg_typ != param_typ && arg_typ != TypeId::missing(ctx.db) {
            ctx.diagnostics.report_by_ptr(
                arg.stable_ptr().untyped(),
                WrongArgumentType { expected_ty: param_typ, actual_ty: arg_typ },
            );
        }

        if param.modifiers.is_ref {
            let expr_var = try_extract_matches!(&arg, Expr::Var).on_none(|| {
                ctx.diagnostics.report_by_ptr(arg.stable_ptr().untyped(), RefArgNotAVariable);
            })?;
            ref_args.push(expr_var.var);
        } else {
            args.push(ctx.exprs.alloc(arg));
        }
    }
    Some(Expr::FunctionCall(ExprFunctionCall {
        function,
        ref_args,
        args,
        ty: signature.return_type,
        stable_ptr,
    }))
}

/// Computes the semantic model of a statement.
pub fn compute_statement_semantic(
    ctx: &mut ComputationContext<'_>,
    syntax: ast::Statement,
) -> Option<StatementId> {
    let db = ctx.db;
    let syntax_db = db.upcast();
    let statement = match &syntax {
        ast::Statement::Let(let_syntax) => {
            let expr = compute_expr_semantic(ctx, &let_syntax.rhs(syntax_db));
            let inferred_type = expr.ty();
            let rhs_expr_id = ctx.exprs.alloc(expr);

            let ty = match let_syntax.type_clause(syntax_db) {
                ast::OptionTypeClause::Empty(_) => inferred_type,
                ast::OptionTypeClause::TypeClause(type_clause) => {
                    let var_type_path = type_clause.ty(syntax_db);
                    let explicit_type =
                        resolve_type(db, ctx.diagnostics, &mut ctx.resolver, &var_type_path);
                    if inferred_type != TypeId::missing(db) && explicit_type != inferred_type {
                        ctx.diagnostics.report(
                            &let_syntax.rhs(syntax_db),
                            WrongArgumentType {
                                expected_ty: explicit_type,
                                actual_ty: inferred_type,
                            },
                        )
                    }
                    explicit_type
                }
            };

            let pattern = compute_pattern_semantic(ctx, let_syntax.pattern(syntax_db), ty)?;
            let variables = pattern.variables();
            for v in variables {
                ctx.environment.variables.insert(v.name.clone(), Variable::Local(v.var.clone()));
            }
            semantic::Statement::Let(semantic::StatementLet {
                pattern,
                expr: rhs_expr_id,
                stable_ptr: syntax.stable_ptr(),
            })
        }
        ast::Statement::Expr(expr_syntax) => {
            let expr = compute_expr_semantic(ctx, &expr_syntax.expr(syntax_db));
            semantic::Statement::Expr(semantic::StatementExpr {
                expr: ctx.exprs.alloc(expr),
                stable_ptr: syntax.stable_ptr(),
            })
        }
        ast::Statement::Return(return_syntax) => {
            let expr_syntax = return_syntax.expr(syntax_db);
            let expr = compute_expr_semantic(ctx, &expr_syntax);
            if expr.ty() != ctx.return_ty {
                ctx.diagnostics.report(
                    &expr_syntax,
                    WrongReturnType { expected_ty: ctx.return_ty, actual_ty: expr.ty() },
                )
            }
            semantic::Statement::Return(semantic::StatementReturn {
                expr: ctx.exprs.alloc(expr),
                stable_ptr: syntax.stable_ptr(),
            })
        }
        ast::Statement::Missing(_) => todo!(),
    };
    Some(ctx.statements.alloc(statement))
}
