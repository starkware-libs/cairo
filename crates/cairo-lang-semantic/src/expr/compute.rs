//! This module is responsible of computing the semantic model of expressions and statements in
//! the code, while type checking.
//! It is invoked by queries for function bodies and other code blocks.

use std::ops::Deref;
use std::sync::Arc;

use ast::PathSegment;
use cairo_lang_defs::db::validate_attributes_flat;
use cairo_lang_defs::ids::{
    FunctionTitleId, FunctionWithBodyId, GenericKind, LanguageElementId, LocalVarLongId, MemberId,
    TraitFunctionId, TraitId,
};
use cairo_lang_diagnostics::{Maybe, ToOption};
use cairo_lang_filesystem::ids::{FileKind, FileLongId, VirtualFile};
use cairo_lang_syntax::attribute::consts::MUST_USE_ATTR;
use cairo_lang_syntax::node::ast::{BlockOrIf, ExprPtr, PatternStructParam, UnaryOperator};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::{GetIdentifier, PathSegmentEx};
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_syntax::node::{ast, Terminal, TypedSyntaxNode};
use cairo_lang_utils::ordered_hash_map::{Entry, OrderedHashMap};
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use cairo_lang_utils::unordered_hash_set::UnorderedHashSet;
use cairo_lang_utils::{extract_matches, try_extract_matches, OptionHelper};
use id_arena::Arena;
use itertools::{chain, zip_eq};
use num_bigint::BigInt;
use smol_str::SmolStr;

use super::inference::canonic::ResultNoErrEx;
use super::inference::conform::InferenceConform;
use super::inference::infers::InferenceEmbeddings;
use super::inference::{Inference, InferenceError};
use super::objects::*;
use super::pattern::{
    Pattern, PatternEnumVariant, PatternLiteral, PatternMissing, PatternOtherwise, PatternTuple,
    PatternVariable,
};
use crate::corelib::{
    core_binary_operator, core_bool_ty, core_unary_operator, false_literal_expr, get_core_trait,
    never_ty, true_literal_expr, try_get_core_ty_by_name, unit_expr, unit_ty,
    unwrap_error_propagation_type,
};
use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnosticKind::{self, *};
use crate::diagnostic::{
    ElementKind, NotFoundItemType, SemanticDiagnostics, TraitInferenceErrors,
    UnsupportedOutsideOfFunctionFeatureName,
};
use crate::items::attribute::SemanticQueryAttrs;
use crate::items::enm::SemanticEnumEx;
use crate::items::imp::{filter_candidate_traits, infer_impl_by_self};
use crate::items::modifiers::compute_mutability;
use crate::items::structure::SemanticStructEx;
use crate::items::visibility;
use crate::literals::try_extract_minus_literal;
use crate::resolve::{ResolvedConcreteItem, ResolvedGenericItem, Resolver};
use crate::semantic::{self, FunctionId, LocalVariable, TypeId, TypeLongId, Variable};
use crate::substitution::SemanticRewriter;
use crate::types::{peel_snapshots, resolve_type, wrap_in_snapshots, ConcreteTypeId};
use crate::{
    GenericArgumentId, Member, Mutability, Parameter, PatternStringLiteral, PatternStruct,
    Signature,
};

/// Expression with its id.
#[derive(Debug, Clone)]
pub struct ExprAndId {
    pub expr: Expr,
    pub id: ExprId,
}
impl Deref for ExprAndId {
    type Target = Expr;

    fn deref(&self) -> &Self::Target {
        &self.expr
    }
}

#[derive(Debug, Clone)]
pub struct PatternAndId {
    pub pattern: Pattern,
    pub id: PatternId,
}
impl Deref for PatternAndId {
    type Target = Pattern;

    fn deref(&self) -> &Self::Target {
        &self.pattern
    }
}

/// Named argument in a function call.
#[derive(Debug, Clone)]
pub struct NamedArg(ExprAndId, Option<ast::TerminalIdentifier>, Mutability);

/// Context for computing the semantic model of expression trees.
pub struct ComputationContext<'ctx> {
    pub db: &'ctx dyn SemanticGroup,
    pub diagnostics: &'ctx mut SemanticDiagnostics,
    function: Option<FunctionWithBodyId>,
    pub resolver: Resolver<'ctx>,
    signature: Option<&'ctx Signature>,
    environment: Box<Environment>,
    pub exprs: Arena<semantic::Expr>,
    pub patterns: Arena<semantic::Pattern>,
    pub statements: Arena<semantic::Statement>,
    /// Definitions of semantic variables.
    pub semantic_defs: UnorderedHashMap<semantic::VarId, semantic::Variable>,
    loop_flow_merge: Option<FlowMergeTypeHelper>,
}
impl<'ctx> ComputationContext<'ctx> {
    pub fn new(
        db: &'ctx dyn SemanticGroup,
        diagnostics: &'ctx mut SemanticDiagnostics,
        function: Option<FunctionWithBodyId>,
        resolver: Resolver<'ctx>,
        signature: Option<&'ctx Signature>,
        environment: Environment,
    ) -> Self {
        let semantic_defs =
            environment.variables.values().by_ref().map(|var| (var.id(), var.clone())).collect();
        Self {
            db,
            diagnostics,
            function,
            resolver,
            signature,
            environment: Box::new(environment),
            exprs: Arena::default(),
            patterns: Arena::default(),
            statements: Arena::default(),
            semantic_defs,
            loop_flow_merge: None,
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
        let new_environment = Box::<Environment>::default();
        let old_environment = std::mem::replace(&mut self.environment, new_environment);
        self.environment.parent = Some(old_environment);

        let res = f(self);

        // Pop the environment from the stack.
        let parent = self.environment.parent.take();
        for (var_name, var) in std::mem::take(&mut self.environment.variables) {
            self.add_unused_variable_warning(&var_name, &var);
        }
        self.environment = parent.unwrap();
        res
    }

    /// Adds warning for unused variables if required.
    fn add_unused_variable_warning(&mut self, var_name: &str, var: &Variable) {
        if !self.environment.used_variables.contains(&var.id()) && !var_name.starts_with('_') {
            self.diagnostics.report_by_ptr(var.stable_ptr(self.db.upcast()), UnusedVariable);
        }
    }

    /// Returns [Self::signature] if it exists. Otherwise, reports a diagnostic and returns `Err`.
    fn get_signature(
        &mut self,
        stable_ptr: SyntaxStablePtrId,
        feature_name: UnsupportedOutsideOfFunctionFeatureName,
    ) -> Maybe<&'ctx Signature> {
        if let Some(signature) = self.signature {
            return Ok(signature);
        }

        Err(self
            .diagnostics
            .report_by_ptr(stable_ptr, UnsupportedOutsideOfFunction { feature_name }))
    }

    fn reduce_ty(&mut self, ty: TypeId) -> TypeId {
        // TODO(spapini): Propagate error to diagnostics.
        self.resolver.inference().rewrite(ty).unwrap()
    }
}

// TODO(ilya): Change value to VarId.
pub type EnvVariables = OrderedHashMap<SmolStr, Variable>;

// TODO(spapini): Consider using identifiers instead of SmolStr everywhere in the code.
/// A state which contains all the variables defined at the current resolver until now, and a
/// pointer to the parent environment.
#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct Environment {
    parent: Option<Box<Environment>>,
    variables: EnvVariables,
    used_variables: UnorderedHashSet<semantic::VarId>,
}
impl Environment {
    /// Adds a parameter to the environment.
    pub fn add_param(
        &mut self,
        diagnostics: &mut SemanticDiagnostics,
        semantic_param: Parameter,
        ast_param: &ast::Param,
        function_title_id: FunctionTitleId,
    ) -> Maybe<()> {
        if let Entry::Vacant(entry) = self.variables.entry(semantic_param.name.clone()) {
            entry.insert(Variable::Param(semantic_param));
            Ok(())
        } else {
            Err(diagnostics.report(
                ast_param,
                ParamNameRedefinition { function_title_id, param_name: semantic_param.name },
            ))
        }
    }
}

/// Computes the semantic model of an expression.
/// Note that this expr will always be "registered" in the arena, so it can be looked up in the
/// language server.
pub fn compute_expr_semantic(ctx: &mut ComputationContext<'_>, syntax: &ast::Expr) -> ExprAndId {
    let expr = maybe_compute_expr_semantic(ctx, syntax);
    let expr = wrap_maybe_with_missing(ctx, expr, syntax.stable_ptr());
    let id = ctx.exprs.alloc(expr.clone());
    ExprAndId { expr, id }
}

/// Converts `Maybe<Expr>` to a possibly [missing](ExprMissing) [Expr].
fn wrap_maybe_with_missing(
    ctx: &mut ComputationContext<'_>,
    expr: Maybe<Expr>,
    stable_ptr: ast::ExprPtr,
) -> Expr {
    expr.unwrap_or_else(|diag_added| {
        Expr::Missing(ExprMissing {
            ty: TypeId::missing(ctx.db, diag_added),
            stable_ptr,
            diag_added,
        })
    })
}

/// Computes the semantic model of an expression, or returns a SemanticDiagnosticKind on error.
pub fn maybe_compute_expr_semantic(
    ctx: &mut ComputationContext<'_>,
    syntax: &ast::Expr,
) -> Maybe<Expr> {
    let db = ctx.db;
    let syntax_db = db.upcast();
    // TODO(spapini): When Expr holds the syntax pointer, add it here as well.
    match syntax {
        ast::Expr::Path(path) => resolve_expr_path(ctx, path),
        ast::Expr::Literal(literal_syntax) => {
            Ok(Expr::Literal(literal_to_semantic(ctx, literal_syntax)?))
        }
        ast::Expr::ShortString(literal_syntax) => {
            Ok(Expr::Literal(short_string_to_semantic(ctx, literal_syntax)?))
        }
        ast::Expr::String(literal_syntax) => {
            Ok(Expr::StringLiteral(string_literal_to_semantic(ctx, literal_syntax)?))
        }
        ast::Expr::False(syntax) => Ok(false_literal_expr(ctx, syntax.stable_ptr().into())),
        ast::Expr::True(syntax) => Ok(true_literal_expr(ctx, syntax.stable_ptr().into())),
        ast::Expr::Parenthesized(paren_syntax) => {
            maybe_compute_expr_semantic(ctx, &paren_syntax.expr(syntax_db))
        }
        ast::Expr::Unary(syntax) => compute_expr_unary_semantic(ctx, syntax),
        ast::Expr::Binary(binary_op_syntax) => compute_expr_binary_semantic(ctx, binary_op_syntax),
        ast::Expr::Tuple(tuple_syntax) => compute_expr_tuple_semantic(ctx, tuple_syntax),
        ast::Expr::FunctionCall(call_syntax) => {
            compute_expr_function_call_semantic(ctx, call_syntax)
        }
        ast::Expr::StructCtorCall(ctor_syntax) => struct_ctor_expr(ctx, ctor_syntax),
        ast::Expr::Block(block_syntax) => compute_expr_block_semantic(ctx, block_syntax),
        ast::Expr::Match(expr_match) => compute_expr_match_semantic(ctx, expr_match),
        ast::Expr::If(expr_if) => compute_expr_if_semantic(ctx, expr_if),
        ast::Expr::Loop(expr_loop) => compute_expr_loop_semantic(ctx, expr_loop),
        ast::Expr::ErrorPropagate(expr) => compute_expr_error_propagate_semantic(ctx, expr),
        ast::Expr::InlineMacro(expr) => compute_expr_inline_macro_semantic(ctx, expr),
        ast::Expr::Missing(_) | ast::Expr::FieldInitShorthand(_) => {
            Err(ctx.diagnostics.report(syntax, Unsupported))
        }
        ast::Expr::Indexed(expr) => compute_expr_indexed_semantic(ctx, expr),
    }
}

fn compute_expr_inline_macro_semantic(
    ctx: &mut ComputationContext<'_>,
    syntax: &ast::ExprInlineMacro,
) -> Maybe<Expr> {
    let syntax_db = ctx.db.upcast();

    let macro_name = syntax.path(syntax_db).as_syntax_node().get_text_without_trivia(syntax_db);
    let Some(macro_plugin) = ctx.db.inline_macro_plugins().get(&macro_name).cloned() else {
        return Err(ctx
            .diagnostics
            .report(syntax, InlineMacroNotFound { macro_name: macro_name.into() }));
    };

    let result = macro_plugin.generate_code(syntax_db, syntax);
    let mut diag_added = None;
    for diagnostic in result.diagnostics {
        diag_added = Some(
            ctx.diagnostics.report_by_ptr(diagnostic.stable_ptr, PluginDiagnostic(diagnostic)),
        );
    }

    let Some(code) = result.code else {
        return Err(diag_added.unwrap_or_else(|| {
            ctx.diagnostics.report(syntax, InlineMacroFailed { macro_name: macro_name.into() })
        }));
    };

    // Create a file
    let new_file = ctx.db.intern_file(FileLongId::Virtual(VirtualFile {
        parent: Some(ctx.diagnostics.file_id),
        name: code.name,
        content: Arc::new(code.content),
        code_mappings: Arc::new(code.code_mappings),
        kind: FileKind::Expr,
    }));
    let expr_syntax = ctx.db.file_expr_syntax(new_file)?;
    let old_file = std::mem::replace(&mut ctx.diagnostics.file_id, new_file);
    let expr = compute_expr_semantic(ctx, &expr_syntax);
    ctx.diagnostics.file_id = old_file;
    Ok(expr.expr)
}

fn compute_expr_unary_semantic(
    ctx: &mut ComputationContext<'_>,
    syntax: &ast::ExprUnary,
) -> Maybe<Expr> {
    let syntax_db = ctx.db.upcast();

    let unary_op = syntax.op(syntax_db);
    let expr = compute_expr_semantic(ctx, &syntax.expr(syntax_db));

    let expr_ty = ctx.reduce_ty(expr.ty());
    if let UnaryOperator::At(_) = unary_op {
        let ty = ctx.db.intern_type(TypeLongId::Snapshot(expr_ty));
        return Ok(Expr::Snapshot(ExprSnapshot {
            inner: expr.id,
            ty,
            stable_ptr: syntax.stable_ptr().into(),
        }));
    }
    if let UnaryOperator::Desnap(_) = unary_op {
        let desnapped_ty = match ctx.db.lookup_intern_type(expr_ty) {
            TypeLongId::Var(_) => {
                let desnapped_var =
                    ctx.resolver.inference().new_type_var(Some(syntax.stable_ptr().untyped()));
                let snapped_desnapped_var = ctx.db.intern_type(TypeLongId::Snapshot(desnapped_var));
                if ctx.resolver.inference().conform_ty(snapped_desnapped_var, expr_ty).is_err() {
                    return Err(ctx.diagnostics.report(
                        syntax,
                        WrongArgumentType {
                            expected_ty: snapped_desnapped_var,
                            actual_ty: expr_ty,
                        },
                    ));
                };
                desnapped_var
            }
            TypeLongId::Snapshot(ty) => ty,
            _ => {
                return Err(ctx.diagnostics.report(&unary_op, DesnapNonSnapshot));
            }
        };
        return Ok(Expr::Desnap(ExprDesnap {
            inner: expr.id,
            ty: desnapped_ty,
            stable_ptr: syntax.stable_ptr().into(),
        }));
    }
    let concrete_trait_function = match core_unary_operator(
        ctx.db,
        &mut ctx.resolver.inference(),
        &unary_op,
        syntax.stable_ptr().untyped(),
    )? {
        Err(err_kind) => {
            return Err(ctx.diagnostics.report(&unary_op, err_kind));
        }
        Ok(function) => function,
    };

    let impl_lookup_context = ctx.resolver.impl_lookup_context();
    let function = ctx
        .resolver
        .inference()
        .infer_trait_function(
            concrete_trait_function,
            &impl_lookup_context,
            Some(syntax.stable_ptr().untyped()),
        )
        .map_err(|err| err.report(ctx.diagnostics, syntax.stable_ptr().untyped()))?;

    expr_function_call(
        ctx,
        function,
        vec![NamedArg(expr, None, Mutability::Immutable)],
        syntax.stable_ptr().into(),
    )
}

fn compute_expr_binary_semantic(
    ctx: &mut ComputationContext<'_>,
    syntax: &ast::ExprBinary,
) -> Maybe<Expr> {
    let db = ctx.db;
    let syntax_db = db.upcast();

    let stable_ptr = syntax.stable_ptr().into();
    let binary_op = syntax.op(syntax_db);
    let lhs_syntax = &syntax.lhs(syntax_db);
    let lexpr = compute_expr_semantic(ctx, lhs_syntax);
    let rhs_syntax = syntax.rhs(syntax_db);
    if matches!(binary_op, ast::BinaryOperator::Dot(_)) {
        return dot_expr(ctx, lexpr, rhs_syntax, stable_ptr);
    }
    let rexpr = compute_expr_semantic(ctx, &rhs_syntax);
    match binary_op {
        ast::BinaryOperator::Eq(_) => {
            let member_path = match lexpr.expr {
                Expr::Var(expr) => ExprVarMemberPath::Var(expr),
                Expr::MemberAccess(ExprMemberAccess { member_path: Some(ref_arg), .. }) => ref_arg,
                _ => return Err(ctx.diagnostics.report(lhs_syntax, InvalidLhsForAssignment)),
            };

            let expected_ty = ctx.reduce_ty(member_path.ty());
            let actual_ty = ctx.reduce_ty(rexpr.ty());

            if ctx.resolver.inference().conform_ty(actual_ty, expected_ty).is_err() {
                return Err(ctx
                    .diagnostics
                    .report(&rhs_syntax, WrongArgumentType { expected_ty, actual_ty }));
            }
            // Verify the variable argument is mutable.
            if !ctx.semantic_defs[&member_path.base_var()].is_mut() {
                ctx.diagnostics.report(syntax, AssignmentToImmutableVar);
            }
            return Ok(Expr::Assignment(ExprAssignment {
                ref_arg: member_path,
                rhs: rexpr.id,
                ty: unit_ty(db),
                stable_ptr,
            }));
        }
        ast::BinaryOperator::AndAnd(_) | ast::BinaryOperator::OrOr(_) => {
            let op = match binary_op {
                ast::BinaryOperator::AndAnd(_) => LogicalOperator::AndAnd,
                ast::BinaryOperator::OrOr(_) => LogicalOperator::OrOr,
                _ => unreachable!(),
            };

            let bool_ty = core_bool_ty(db);
            if ctx.resolver.inference().conform_ty(lexpr.expr.ty(), bool_ty).is_err() {
                ctx.diagnostics.report(
                    lhs_syntax,
                    WrongType { expected_ty: bool_ty, actual_ty: lexpr.expr.ty() },
                );
            }

            if ctx.resolver.inference().conform_ty(rexpr.expr.ty(), bool_ty).is_err() {
                ctx.diagnostics.report(
                    &rhs_syntax,
                    WrongType { expected_ty: bool_ty, actual_ty: rexpr.expr.ty() },
                );
            }

            return Ok(Expr::LogicalOperator(ExprLogicalOperator {
                lhs: lexpr.id,
                op,
                rhs: rexpr.id,
                ty: bool_ty,
                stable_ptr,
            }));
        }
        _ => {}
    };
    call_core_binary_op(ctx, syntax, lexpr, rexpr)
}

/// Get the function call expression of a binary operation that is defined in the corelib.
fn call_core_binary_op(
    ctx: &mut ComputationContext<'_>,
    syntax: &ast::ExprBinary,
    mut lexpr: ExprAndId,
    mut rexpr: ExprAndId,
) -> Maybe<Expr> {
    let db = ctx.db;
    let stable_ptr = syntax.stable_ptr().into();
    let binary_op = syntax.op(db.upcast());

    ctx.reduce_ty(lexpr.ty()).check_not_missing(db)?;
    ctx.reduce_ty(rexpr.ty()).check_not_missing(db)?;
    let (concrete_trait_function, snapshot) = match core_binary_operator(
        db,
        &mut ctx.resolver.inference(),
        &binary_op,
        syntax.stable_ptr().untyped(),
    )? {
        Err(err_kind) => {
            return Err(ctx.diagnostics.report(&binary_op, err_kind));
        }
        Ok(res) => res,
    };
    if snapshot {
        let ty = ctx.db.intern_type(TypeLongId::Snapshot(lexpr.ty()));
        let expr =
            Expr::Snapshot(ExprSnapshot { inner: lexpr.id, ty, stable_ptr: lexpr.stable_ptr() });
        lexpr = ExprAndId { expr: expr.clone(), id: ctx.exprs.alloc(expr) };
        let ty = ctx.db.intern_type(TypeLongId::Snapshot(rexpr.ty()));
        let expr =
            Expr::Snapshot(ExprSnapshot { inner: rexpr.id, ty, stable_ptr: rexpr.stable_ptr() });
        rexpr = ExprAndId { expr: expr.clone(), id: ctx.exprs.alloc(expr) };
    }

    let impl_lookup_context = ctx.resolver.impl_lookup_context();
    let function = ctx
        .resolver
        .inference()
        .infer_trait_function(
            concrete_trait_function,
            &impl_lookup_context,
            Some(syntax.stable_ptr().untyped()),
        )
        .map_err(|err| err.report(ctx.diagnostics, syntax.stable_ptr().untyped()))?;

    let sig = ctx.db.concrete_function_signature(function)?;
    let first_param = sig.params.into_iter().next().unwrap();
    expr_function_call(
        ctx,
        function,
        vec![
            NamedArg(lexpr, None, first_param.mutability),
            NamedArg(rexpr, None, Mutability::Immutable),
        ],
        stable_ptr,
    )
}

fn compute_expr_tuple_semantic(
    ctx: &mut ComputationContext<'_>,
    syntax: &ast::ExprListParenthesized,
) -> Maybe<Expr> {
    let db = ctx.db;
    let syntax_db = db.upcast();

    let mut items: Vec<ExprId> = vec![];
    let mut types: Vec<TypeId> = vec![];
    for expr_syntax in syntax.expressions(syntax_db).elements(syntax_db) {
        let expr_semantic = compute_expr_semantic(ctx, &expr_syntax);
        types.push(ctx.reduce_ty(expr_semantic.ty()));
        items.push(expr_semantic.id);
    }
    Ok(Expr::Tuple(ExprTuple {
        items,
        ty: db.intern_type(TypeLongId::Tuple(types)),
        stable_ptr: syntax.stable_ptr().into(),
    }))
}

fn compute_expr_function_call_semantic(
    ctx: &mut ComputationContext<'_>,
    syntax: &ast::ExprFunctionCall,
) -> Maybe<Expr> {
    let db = ctx.db;
    let syntax_db = db.upcast();

    let path = syntax.path(syntax_db);
    let item =
        ctx.resolver.resolve_concrete_path(ctx.diagnostics, &path, NotFoundItemType::Function)?;
    let args_syntax = syntax.arguments(syntax_db);
    // TODO(Gil): Consider not invoking the TraitFunction inference below if there were errors in
    // argument semantics, in order to avoid unnecessary diagnostics.
    let named_args: Vec<_> = args_syntax
        .arguments(syntax_db)
        .elements(syntax_db)
        .into_iter()
        .map(|arg_syntax| compute_named_argument_clause(ctx, arg_syntax))
        .collect();
    match item {
        ResolvedConcreteItem::Variant(concrete_variant) => {
            if named_args.len() != 1 {
                return Err(ctx.diagnostics.report(
                    &args_syntax,
                    WrongNumberOfArguments { expected: 1, actual: named_args.len() },
                ));
            }
            let NamedArg(arg, name_terminal, mutability) = named_args[0].clone();
            if let Some(name_terminal) = name_terminal {
                ctx.diagnostics.report(&name_terminal, NamedArgumentsAreNotSupported);
            }
            if mutability != Mutability::Immutable {
                return Err(ctx.diagnostics.report(&args_syntax, VariantCtorNotImmutable));
            }
            let expected_ty = ctx.reduce_ty(concrete_variant.ty);
            let actual_ty = ctx.reduce_ty(arg.ty());
            if ctx.resolver.inference().conform_ty(actual_ty, expected_ty).is_err() {
                return Err(ctx
                    .diagnostics
                    .report(&args_syntax, WrongArgumentType { expected_ty, actual_ty }));
            }
            let concrete_enum_id = concrete_variant.concrete_enum_id;
            Ok(semantic::Expr::EnumVariantCtor(semantic::ExprEnumVariantCtor {
                variant: concrete_variant,
                value_expr: arg.id,
                ty: db.intern_type(TypeLongId::Concrete(ConcreteTypeId::Enum(concrete_enum_id))),
                stable_ptr: syntax.stable_ptr().into(),
            }))
        }
        ResolvedConcreteItem::Function(function) => {
            expr_function_call(ctx, function, named_args, syntax.stable_ptr().into())
        }
        _ => Err(ctx.diagnostics.report(
            &path,
            UnexpectedElement { expected: vec![ElementKind::Function], actual: (&item).into() },
        )),
    }
}

/// Computes the semantic model of an expression of type [ast::Arg].
///
/// Returns the value and the optional argument name.
pub fn compute_named_argument_clause(
    ctx: &mut ComputationContext<'_>,
    arg_syntax: ast::Arg,
) -> NamedArg {
    let syntax_db = ctx.db.upcast();

    let mutability = compute_mutability(
        ctx.diagnostics,
        syntax_db,
        &arg_syntax.modifiers(syntax_db).elements(syntax_db),
    );

    let arg_clause = arg_syntax.arg_clause(syntax_db);
    let (expr, arg_name_identifier) = match arg_clause {
        ast::ArgClause::Unnamed(arg_unnamed) => {
            (compute_expr_semantic(ctx, &arg_unnamed.value(syntax_db)), None)
        }
        ast::ArgClause::Named(arg_named) => (
            compute_expr_semantic(ctx, &arg_named.value(syntax_db)),
            Some(arg_named.name(syntax_db)),
        ),
        ast::ArgClause::FieldInitShorthand(arg_field_init_shorthand) => {
            let name_expr = arg_field_init_shorthand.name(syntax_db);
            let stable_ptr: ast::ExprPtr = name_expr.stable_ptr().into();
            let arg_name_identifier = name_expr.name(syntax_db);
            let maybe_expr = resolve_variable_by_name(ctx, &arg_name_identifier, stable_ptr);
            let expr = wrap_maybe_with_missing(ctx, maybe_expr, stable_ptr);
            let expr = ExprAndId { expr: expr.clone(), id: ctx.exprs.alloc(expr) };
            (expr, Some(arg_name_identifier))
        }
    };

    NamedArg(expr, arg_name_identifier, mutability)
}

pub fn compute_root_expr(
    ctx: &mut ComputationContext<'_>,
    syntax: &ast::ExprBlock,
    return_type: TypeId,
) -> Maybe<ExprId> {
    let res = compute_expr_block_semantic(ctx, syntax)?;
    let res_ty = res.ty();
    let res = ctx.exprs.alloc(res);
    if ctx.resolver.inference().conform_ty(res_ty, return_type).is_err() {
        ctx.diagnostics
            .report(syntax, WrongReturnType { expected_ty: return_type, actual_ty: res_ty });
    }

    // Check fully resolved.
    if let Some((stable_ptr, inference_err)) = ctx.resolver.inference().finalize() {
        inference_err.report(ctx.diagnostics, stable_ptr.unwrap_or(syntax.stable_ptr().untyped()));
        return Ok(res);
    }

    // Apply inference.
    infer_all(ctx).ok();

    Ok(res)
}

fn infer_all(ctx: &mut ComputationContext<'_>) -> Maybe<()> {
    for (_id, expr) in ctx.exprs.iter_mut() {
        *expr = ctx.resolver.inference().rewrite(expr.clone()).no_err();
    }
    for (_id, pattern) in ctx.patterns.iter_mut() {
        *pattern = ctx.resolver.inference().rewrite(pattern.clone()).no_err();
    }
    for (_id, stmt) in ctx.statements.iter_mut() {
        *stmt = ctx.resolver.inference().rewrite(stmt.clone()).no_err();
    }
    Ok(())
}

/// Computes the semantic model of an expression of type [ast::ExprBlock].
pub fn compute_expr_block_semantic(
    ctx: &mut ComputationContext<'_>,
    syntax: &ast::ExprBlock,
) -> Maybe<Expr> {
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
            .filter_map(|statement_syntax| {
                compute_statement_semantic(new_ctx, statement_syntax).to_option()
            })
            .collect();

        // Convert tail expression (if exists) to semantic model.
        let tail_semantic_expr = tail.map(|tail_expr| compute_expr_semantic(new_ctx, &tail_expr));
        let ty = if let Some(t) = &tail_semantic_expr {
            t.ty()
        } else if let Some(statement) = statements_semantic.last() {
            if let Statement::Return(_) | Statement::Break(_) = &new_ctx.statements[*statement] {
                never_ty(new_ctx.db)
            } else {
                unit_ty(db)
            }
        } else {
            unit_ty(db)
        };
        Ok(Expr::Block(ExprBlock {
            statements: statements_semantic,
            tail: tail_semantic_expr.map(|expr| expr.id),
            ty,
            stable_ptr: syntax.stable_ptr().into(),
        }))
    })
}

/// Helper for merging the return types of branch blocks (match or if else).
struct FlowMergeTypeHelper {
    never_type: TypeId,
    final_type: Option<TypeId>,
}
impl FlowMergeTypeHelper {
    fn new(db: &dyn SemanticGroup) -> Self {
        Self { never_type: never_ty(db), final_type: None }
    }

    /// Attempt merge a branch into the helper, on error will return the conflicting types.
    fn try_merge_types(
        &mut self,
        inference: &mut Inference<'_>,
        db: &dyn SemanticGroup,
        ty: TypeId,
    ) -> Result<(), (TypeId, TypeId)> {
        if ty != self.never_type && !ty.is_missing(db) {
            if let Some(existing) = &self.final_type {
                if inference.conform_ty(ty, *existing).is_err() {
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
) -> Maybe<Expr> {
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
                    compute_pattern_semantic(new_ctx, &syntax_arm.pattern(syntax_db), expr.ty());
                let variables = pattern.variables(&new_ctx.patterns);
                for v in variables {
                    let var_def = Variable::Local(v.var.clone());
                    // TODO(spapini): Wrap this in a function to couple with semantic_defs
                    // insertion.
                    new_ctx.environment.variables.insert(v.name.clone(), var_def.clone());
                    new_ctx.semantic_defs.insert(var_def.id(), var_def);
                }
                let arm_expr = compute_expr_semantic(new_ctx, &arm_expr_syntax);
                Ok((pattern, arm_expr))
            })
        })
        .collect();
    // Unify arm types.
    let mut helper = FlowMergeTypeHelper::new(ctx.db);
    for (_, expr) in pattern_and_expr_options.iter().flatten() {
        if let Err((match_ty, arm_ty)) =
            helper.try_merge_types(&mut ctx.resolver.inference(), ctx.db, expr.ty())
        {
            ctx.diagnostics.report_by_ptr(
                expr.stable_ptr().untyped(),
                IncompatibleMatchArms { match_ty, arm_ty },
            );
        }
    }
    // Compute semantic representation of the match arms.
    let pattern_and_exprs: Vec<_> = pattern_and_expr_options.into_iter().collect::<Maybe<_>>()?;
    let semantic_arms = pattern_and_exprs
        .into_iter()
        .map(|(pattern, arm_expr)| MatchArm { pattern: pattern.id, expression: arm_expr.id })
        .collect();
    Ok(Expr::Match(ExprMatch {
        matched_expr: expr.id,
        arms: semantic_arms,
        ty: helper.get_final_type(),
        stable_ptr: syntax.stable_ptr().into(),
    }))
}

/// Computes the semantic model of an expression of type [ast::ExprIf].
fn compute_expr_if_semantic(ctx: &mut ComputationContext<'_>, syntax: &ast::ExprIf) -> Maybe<Expr> {
    let syntax_db = ctx.db.upcast();

    let expr = compute_expr_semantic(ctx, &syntax.condition(syntax_db));
    if ctx.resolver.inference().conform_ty(expr.ty(), core_bool_ty(ctx.db)).is_err() {
        ctx.diagnostics.report_by_ptr(
            expr.stable_ptr().untyped(),
            IfConditionNotBool { condition_ty: expr.ty() },
        );
    }
    let if_block = compute_expr_block_semantic(ctx, &syntax.if_block(syntax_db))?;

    let (else_block_opt, else_block_ty) = match syntax.else_clause(syntax_db) {
        ast::OptionElseClause::Empty(_) => (None, unit_ty(ctx.db)),
        ast::OptionElseClause::ElseClause(else_clause) => {
            match else_clause.else_block_or_if(syntax_db) {
                BlockOrIf::Block(block) => {
                    let else_block = compute_expr_block_semantic(ctx, &block)?;
                    (Some(else_block.clone()), else_block.ty())
                }
                BlockOrIf::If(expr_if) => {
                    let else_if = compute_expr_if_semantic(ctx, &expr_if)?;
                    (Some(else_if.clone()), else_if.ty())
                }
            }
        }
    };

    let mut helper = FlowMergeTypeHelper::new(ctx.db);
    helper
        .try_merge_types(&mut ctx.resolver.inference(), ctx.db, if_block.ty())
        .and(helper.try_merge_types(&mut ctx.resolver.inference(), ctx.db, else_block_ty))
        .unwrap_or_else(|(block_if_ty, block_else_ty)| {
            ctx.diagnostics.report(syntax, IncompatibleIfBlockTypes { block_if_ty, block_else_ty });
        });
    Ok(Expr::If(ExprIf {
        condition: expr.id,
        if_block: ctx.exprs.alloc(if_block),
        else_block: else_block_opt.map(|else_block| ctx.exprs.alloc(else_block)),
        ty: helper.get_final_type(),
        stable_ptr: syntax.stable_ptr().into(),
    }))
}

/// Computes the semantic model of an expression of type [ast::ExprLoop].
fn compute_expr_loop_semantic(
    ctx: &mut ComputationContext<'_>,
    syntax: &ast::ExprLoop,
) -> Maybe<Expr> {
    let db = ctx.db;
    let syntax_db = db.upcast();

    let (body, new_flow_merge) = ctx.run_in_subscope(|new_ctx| {
        let old_flow_merge = new_ctx.loop_flow_merge.replace(FlowMergeTypeHelper::new(db));

        let mut statements = syntax.body(syntax_db).statements(syntax_db).elements(syntax_db);
        // Remove the typed tail expression, if exists.
        let tail = get_tail_expression(syntax_db, statements.as_slice());
        if tail.is_some() {
            statements.pop();
        }

        // Convert statements to semantic model.
        let statements_semantic: Vec<_> = statements
            .into_iter()
            .filter_map(|statement_syntax| {
                compute_statement_semantic(new_ctx, statement_syntax).to_option()
            })
            .collect();
        let tail = tail.map(|tail| compute_expr_semantic(new_ctx, &tail));
        if let Some(tail) = &tail {
            if !tail.ty().is_missing(db) && !tail.ty().is_unit(db) && tail.ty() != never_ty(db) {
                new_ctx
                    .diagnostics
                    .report_by_ptr(tail.stable_ptr().untyped(), TailExpressionNotAllowedInLoop);
            }
        }

        let new_flow_merge =
            std::mem::replace(&mut new_ctx.loop_flow_merge, old_flow_merge).unwrap();

        let body = new_ctx.exprs.alloc(Expr::Block(ExprBlock {
            statements: statements_semantic,
            tail: tail.map(|tail| tail.id),
            ty: unit_ty(db),
            stable_ptr: syntax.stable_ptr().into(),
        }));

        (body, new_flow_merge)
    });

    Ok(Expr::Loop(ExprLoop {
        body,
        ty: new_flow_merge.get_final_type(),
        stable_ptr: syntax.stable_ptr().into(),
    }))
}

/// Computes the semantic model of an expression of type [ast::ExprErrorPropagate].
fn compute_expr_error_propagate_semantic(
    ctx: &mut ComputationContext<'_>,
    syntax: &ast::ExprErrorPropagate,
) -> Maybe<Expr> {
    let syntax_db = ctx.db.upcast();
    let inner = compute_expr_semantic(ctx, &syntax.expr(syntax_db));
    let inner_ty = ctx.reduce_ty(inner.ty());
    inner_ty.check_not_missing(ctx.db)?;
    let (ok_variant, err_variant) =
        unwrap_error_propagation_type(ctx.db, inner_ty).ok_or_else(|| {
            ctx.diagnostics.report(syntax, ErrorPropagateOnNonErrorType { ty: inner_ty })
        })?;
    let func_signature = ctx.get_signature(
        syntax.stable_ptr().untyped(),
        UnsupportedOutsideOfFunctionFeatureName::ErrorPropagate,
    )?;
    // Disallow error propagation inside a loop.
    if ctx.loop_flow_merge.is_some() {
        ctx.diagnostics.report(syntax, SemanticDiagnosticKind::ErrorPropagateNotAllowedInsideALoop);
    }
    let (_, func_err_variant) = unwrap_error_propagation_type(ctx.db, func_signature.return_type)
        .ok_or_else(|| {
        ctx.diagnostics.report(
            syntax,
            IncompatibleErrorPropagateType {
                return_ty: func_signature.return_type,
                err_ty: err_variant.ty,
            },
        )
    })?;
    let conformed_err_variant_ty =
        ctx.resolver.inference().conform_ty(func_err_variant.ty, err_variant.ty);
    // If conforming the types failed, the next check will fail and a better diagnostic will be
    // added.
    let err_variant_ty = conformed_err_variant_ty.unwrap_or(err_variant.ty);
    // TODO(orizi): When auto conversion of types is added, try to convert the error type.
    if func_err_variant.ty != err_variant_ty
        || func_err_variant.concrete_enum_id.enum_id(ctx.db)
            != err_variant.concrete_enum_id.enum_id(ctx.db)
    {
        ctx.diagnostics.report(
            syntax,
            IncompatibleErrorPropagateType {
                return_ty: func_signature.return_type,
                err_ty: err_variant.ty,
            },
        );
    }
    Ok(Expr::PropagateError(ExprPropagateError {
        inner: inner.id,
        ok_variant,
        err_variant,
        func_err_variant,
        stable_ptr: syntax.stable_ptr().into(),
    }))
}

/// Computes the semantic model of an expression of type [ast::ExprIndexed].
fn compute_expr_indexed_semantic(
    ctx: &mut ComputationContext<'_>,
    syntax: &ast::ExprIndexed,
) -> Maybe<Expr> {
    let syntax_db = ctx.db.upcast();
    let expr = compute_expr_semantic(ctx, &syntax.expr(syntax_db));
    let index_expr = compute_expr_semantic(ctx, &syntax.index_expr(syntax_db));
    let candidate_traits: Vec<_> = ["Index", "IndexView"]
        .iter()
        .map(|trait_name| get_core_trait(ctx.db, (*trait_name).into()))
        .collect();
    let (function_id, fixed_expr, mutability) = compute_method_function_call_data(
        ctx,
        &candidate_traits[..],
        "index".into(),
        expr,
        syntax.stable_ptr().untyped(),
        None,
        |ty, _, inference_errors| NoImplementationOfIndexOperator { ty, inference_errors },
        |ty, _, _| MultipleImplementationOfIndexOperator(ty),
    )?;
    expr_function_call(
        ctx,
        function_id,
        vec![
            NamedArg(fixed_expr, None, mutability),
            NamedArg(index_expr, None, Mutability::Immutable),
        ],
        syntax.stable_ptr().into(),
    )
}

/// Computes the data needed for a method function call, and similar exprs (index operator). Method
/// call and Index operator differs in the diagnostics they emit. The function returns the
/// function_id to call, the self argument, with snapshots added if needed, and the mutability of
/// the self argument.
#[allow(clippy::too_many_arguments)]
fn compute_method_function_call_data(
    ctx: &mut ComputationContext<'_>,
    candidate_traits: &[TraitId],
    func_name: SmolStr,
    self_expr: ExprAndId,
    method_syntax: SyntaxStablePtrId,
    generic_args_syntax: Option<Vec<ast::GenericArg>>,
    no_implementation_diagnostic: fn(
        TypeId,
        SmolStr,
        TraitInferenceErrors,
    ) -> SemanticDiagnosticKind,
    multiple_trait_diagnostic: fn(
        TypeId,
        TraitFunctionId,
        TraitFunctionId,
    ) -> SemanticDiagnosticKind,
) -> Maybe<(FunctionId, ExprAndId, Mutability)> {
    let self_ty = self_expr.ty();
    let mut inference_errors = vec![];
    let candidates = filter_candidate_traits(
        ctx,
        &mut inference_errors,
        self_ty,
        candidate_traits,
        func_name.clone(),
        self_expr.stable_ptr().untyped(),
    );
    let trait_function_id = match candidates[..] {
        [] => {
            return Err(ctx.diagnostics.report_by_ptr(
                method_syntax,
                no_implementation_diagnostic(
                    self_ty,
                    func_name,
                    TraitInferenceErrors { traits_and_errors: inference_errors },
                ),
            ));
        }
        [trait_function_id] => trait_function_id,
        [trait_function_id0, trait_function_id1, ..] => {
            return Err(ctx.diagnostics.report_by_ptr(
                method_syntax,
                multiple_trait_diagnostic(self_ty, trait_function_id0, trait_function_id1),
            ));
        }
    };
    let (function_id, n_snapshots) =
        infer_impl_by_self(ctx, trait_function_id, self_ty, method_syntax, generic_args_syntax)
            .unwrap();

    let signature = ctx.db.trait_function_signature(trait_function_id).unwrap();
    let first_param = signature.params.into_iter().next().unwrap();
    let mut fixed_expr = self_expr.clone();
    for _ in 0..n_snapshots {
        let ty = ctx.db.intern_type(TypeLongId::Snapshot(fixed_expr.ty()));
        let expr = Expr::Snapshot(ExprSnapshot {
            inner: fixed_expr.id,
            ty,
            stable_ptr: self_expr.stable_ptr(),
        });
        fixed_expr = ExprAndId { expr: expr.clone(), id: ctx.exprs.alloc(expr) };
    }
    Ok((function_id, fixed_expr, first_param.mutability))
}

/// Computes the semantic model of a pattern.
/// Note that this pattern will always be "registered" in the arena, so it can be looked up in the
/// language server.
pub fn compute_pattern_semantic(
    ctx: &mut ComputationContext<'_>,
    syntax: &ast::Pattern,
    ty: TypeId,
) -> PatternAndId {
    let pat = maybe_compute_pattern_semantic(ctx, syntax, ty);
    let pat = pat.unwrap_or_else(|diag_added| {
        Pattern::Missing(PatternMissing {
            ty: TypeId::missing(ctx.db, diag_added),
            stable_ptr: syntax.stable_ptr(),
            diag_added,
        })
    });
    let id = ctx.patterns.alloc(pat.clone());
    PatternAndId { pattern: pat, id }
}

/// Computes the semantic model of a pattern, or None if invalid.
fn maybe_compute_pattern_semantic(
    ctx: &mut ComputationContext<'_>,
    pattern_syntax: &ast::Pattern,
    ty: TypeId,
) -> Maybe<Pattern> {
    // TODO(spapini): Check for missing type, and don't reemit an error.
    let syntax_db = ctx.db.upcast();
    let ty = ctx.reduce_ty(ty);
    let stable_ptr = pattern_syntax.stable_ptr().untyped();
    let pattern = match pattern_syntax {
        ast::Pattern::Underscore(otherwise_pattern) => {
            Pattern::Otherwise(PatternOtherwise { ty, stable_ptr: otherwise_pattern.stable_ptr() })
        }
        ast::Pattern::Literal(literal_pattern) => {
            let literal = literal_to_semantic(ctx, literal_pattern)?;
            Pattern::Literal(PatternLiteral {
                literal,
                stable_ptr: literal_pattern.stable_ptr().into(),
            })
        }
        ast::Pattern::ShortString(short_string_pattern) => {
            let literal = short_string_to_semantic(ctx, short_string_pattern)?;
            Pattern::Literal(PatternLiteral {
                literal,
                stable_ptr: short_string_pattern.stable_ptr().into(),
            })
        }
        ast::Pattern::String(string_pattern) => {
            let string_literal = string_literal_to_semantic(ctx, string_pattern)?;
            Pattern::StringLiteral(PatternStringLiteral {
                string_literal,
                stable_ptr: string_pattern.stable_ptr().into(),
            })
        }
        ast::Pattern::Enum(enum_pattern) => {
            // Peel all snapshot wrappers.
            let (n_snapshots, long_ty) = peel_snapshots(ctx.db, ty);

            // Check that type is an enum, and get the concrete enum from it.
            let concrete_enum = try_extract_matches!(long_ty, TypeLongId::Concrete)
                .and_then(|c| try_extract_matches!(c, ConcreteTypeId::Enum))
                .ok_or(())
                .or_else(|_| {
                    // Don't add a diagnostic if the type is missing.
                    // A diagnostic should've already been added.
                    ty.check_not_missing(ctx.db)?;
                    Err(ctx.diagnostics.report(enum_pattern, UnexpectedEnumPattern { ty }))
                })?;

            // Extract the enum variant from the path syntax.
            let path = enum_pattern.path(syntax_db);
            let item = ctx.resolver.resolve_generic_path(
                ctx.diagnostics,
                &path,
                NotFoundItemType::Identifier,
            )?;
            let generic_variant = try_extract_matches!(item, ResolvedGenericItem::Variant)
                .ok_or_else(|| ctx.diagnostics.report(&path, NotAVariant))?;

            // Check that these are the same enums.
            if generic_variant.enum_id != concrete_enum.enum_id(ctx.db) {
                return Err(ctx.diagnostics.report(
                    &path,
                    WrongEnum {
                        expected_enum: concrete_enum.enum_id(ctx.db),
                        actual_enum: generic_variant.enum_id,
                    },
                ));
            }
            // TODO(lior): Should we report a diagnostic here?
            let concrete_variant = ctx
                .db
                .concrete_enum_variant(concrete_enum, &generic_variant)
                .map_err(|_| ctx.diagnostics.report(&path, UnknownEnum))?;

            // Compute inner pattern.
            let inner_ty = wrap_in_snapshots(ctx.db, concrete_variant.ty, n_snapshots);

            let inner_pattern = match enum_pattern.pattern(syntax_db) {
                ast::OptionPatternEnumInnerPattern::Empty(_) => None,
                ast::OptionPatternEnumInnerPattern::PatternEnumInnerPattern(p) => {
                    let pattern = compute_pattern_semantic(ctx, &p.pattern(syntax_db), inner_ty);
                    Some(pattern.id)
                }
            };

            Pattern::EnumVariant(PatternEnumVariant {
                variant: concrete_variant,
                inner_pattern,
                ty,
                stable_ptr: enum_pattern.stable_ptr(),
            })
        }
        ast::Pattern::Path(path) => {
            // A path of length 1 is an identifier, which will result in a variable pattern.
            // Currently, other paths are not supported (and not clear if ever will be).
            if path.elements(syntax_db).len() > 1 {
                return Err(ctx.diagnostics.report(path, Unsupported));
            }
            // TODO(spapini): Make sure this is a simple identifier. In particular, no generics.
            let identifier = path.elements(syntax_db)[0].identifier_ast(syntax_db);
            create_variable_pattern(ctx, identifier, &[], ty, path.stable_ptr().into())
        }
        ast::Pattern::Identifier(identifier) => create_variable_pattern(
            ctx,
            identifier.name(syntax_db),
            &identifier.modifiers(syntax_db).elements(syntax_db),
            ty,
            identifier.stable_ptr().into(),
        ),
        ast::Pattern::Struct(pattern_struct) => {
            let pattern_ty = try_extract_matches!(
                ctx.resolver.resolve_concrete_path(
                    ctx.diagnostics,
                    &pattern_struct.path(syntax_db),
                    NotFoundItemType::Type
                )?,
                ResolvedConcreteItem::Type
            )
            .ok_or_else(|| ctx.diagnostics.report(&pattern_struct.path(syntax_db), NotAType))?;
            ctx.resolver
                .inference()
                .conform_ty(pattern_ty, ctx.db.intern_type(peel_snapshots(ctx.db, ty).1))
                .map_err(|err| err.report(ctx.diagnostics, stable_ptr))?;
            let ty = ctx.reduce_ty(ty);
            // Peel all snapshot wrappers.
            let (n_snapshots, long_ty) = peel_snapshots(ctx.db, ty);

            // Check that type is an struct, and get the concrete struct from it.
            let concrete_struct_id = try_extract_matches!(long_ty, TypeLongId::Concrete)
                .and_then(|c| try_extract_matches!(c, ConcreteTypeId::Struct))
                .ok_or(())
                .or_else(|_| {
                    // Don't add a diagnostic if the type is missing.
                    // A diagnostic should've already been added.
                    ty.check_not_missing(ctx.db)?;
                    Err(ctx.diagnostics.report(pattern_struct, UnexpectedStructPattern { ty }))
                })?;
            let pattern_param_asts = pattern_struct.params(syntax_db).elements(syntax_db);
            let struct_id = concrete_struct_id.struct_id(ctx.db);
            let mut members = ctx.db.concrete_struct_members(concrete_struct_id)?;
            let mut used_members = UnorderedHashSet::default();
            let mut get_member = |ctx: &mut ComputationContext<'_>,
                                  member_name: SmolStr,
                                  stable_ptr: SyntaxStablePtrId| {
                let member = members.swap_remove(&member_name).on_none(|| {
                    ctx.diagnostics.report_by_ptr(
                        stable_ptr,
                        if used_members.contains(&member_name) {
                            StructMemberRedefinition { struct_id, member_name: member_name.clone() }
                        } else {
                            NoSuchMember { struct_id, member_name: member_name.clone() }
                        },
                    );
                })?;
                check_struct_member_is_visible(ctx, &member, stable_ptr, &member_name);
                used_members.insert(member_name);
                Some(member)
            };
            let mut field_patterns = vec![];
            let mut has_tail = false;
            for pattern_param_ast in pattern_param_asts {
                match pattern_param_ast {
                    PatternStructParam::Single(single) => {
                        let name = single.name(syntax_db);
                        let Some(member) =
                            get_member(ctx, name.text(syntax_db), name.stable_ptr().untyped())
                        else {
                            continue;
                        };
                        let ty = wrap_in_snapshots(ctx.db, member.ty, n_snapshots);
                        let pattern = create_variable_pattern(
                            ctx,
                            name,
                            &single.modifiers(syntax_db).elements(syntax_db),
                            ty,
                            single.stable_ptr().into(),
                        );
                        field_patterns.push((member, ctx.patterns.alloc(pattern)));
                    }
                    PatternStructParam::WithExpr(with_expr) => {
                        let name = with_expr.name(syntax_db);
                        let Some(member) =
                            get_member(ctx, name.text(syntax_db), name.stable_ptr().untyped())
                        else {
                            continue;
                        };
                        let ty = wrap_in_snapshots(ctx.db, member.ty, n_snapshots);
                        let pattern =
                            compute_pattern_semantic(ctx, &with_expr.pattern(syntax_db), ty);
                        field_patterns.push((member, pattern.id));
                    }
                    PatternStructParam::Tail(_) => {
                        has_tail = true;
                    }
                }
            }
            if !has_tail {
                for (member_name, _) in members {
                    ctx.diagnostics.report(pattern_struct, MissingMember { member_name });
                }
            }
            Pattern::Struct(PatternStruct {
                concrete_struct_id,
                field_patterns,
                ty,
                n_snapshots,
                stable_ptr: pattern_struct.stable_ptr(),
            })
        }
        ast::Pattern::Tuple(pattern_tuple) => {
            // Peel all snapshot wrappers.
            let (n_snapshots, long_ty) = peel_snapshots(ctx.db, ty);

            let tys = try_extract_matches!(long_ty, TypeLongId::Tuple).ok_or_else(|| {
                ctx.diagnostics.report(pattern_tuple, UnexpectedTuplePattern { ty })
            })?;

            let patterns_ast = pattern_tuple.patterns(syntax_db).elements(syntax_db);
            if tys.len() != patterns_ast.len() {
                return Err(ctx.diagnostics.report(
                    pattern_tuple,
                    WrongNumberOfTupleElements { expected: tys.len(), actual: patterns_ast.len() },
                ));
            }
            // Iterator of Option<Pattern?, for each field.
            let pattern_options = zip_eq(patterns_ast, tys).map(|(pattern_ast, ty)| {
                let ty = wrap_in_snapshots(ctx.db, ty, n_snapshots);
                let pattern = compute_pattern_semantic(ctx, &pattern_ast, ty);
                Ok(pattern.id)
            });
            // If all are Some, collect into a Vec.
            let field_patterns: Vec<_> = pattern_options.collect::<Maybe<_>>()?;

            Pattern::Tuple(PatternTuple {
                field_patterns,
                ty,
                stable_ptr: pattern_tuple.stable_ptr(),
            })
        }
    };
    ctx.resolver
        .inference()
        .conform_ty(pattern.ty(), ty)
        .map_err(|err| err.report(ctx.diagnostics, stable_ptr))?;
    Ok(pattern)
}

/// Creates a local variable pattern.
fn create_variable_pattern(
    ctx: &mut ComputationContext<'_>,
    identifier: ast::TerminalIdentifier,
    modifier_list: &[ast::Modifier],
    ty: TypeId,
    stable_ptr: ast::PatternPtr,
) -> Pattern {
    let syntax_db = ctx.db.upcast();
    let var_id = ctx
        .db
        .intern_local_var(LocalVarLongId(ctx.resolver.module_file_id, identifier.stable_ptr()));

    let is_mut = match compute_mutability(ctx.diagnostics, syntax_db, modifier_list) {
        Mutability::Immutable => false,
        Mutability::Mutable => true,
        Mutability::Reference => {
            ctx.diagnostics.report(&identifier, ReferenceLocalVariable);
            false
        }
    };
    Pattern::Variable(PatternVariable {
        name: identifier.text(syntax_db),
        var: LocalVariable { id: var_id, ty, is_mut },
        stable_ptr,
    })
}

/// Creates a struct constructor semantic expression from its AST.
fn struct_ctor_expr(
    ctx: &mut ComputationContext<'_>,
    ctor_syntax: &ast::ExprStructCtorCall,
) -> Maybe<Expr> {
    let db = ctx.db;
    let syntax_db = db.upcast();
    let path = ctor_syntax.path(syntax_db);

    // Extract struct.
    let ty = resolve_type(db, ctx.diagnostics, &mut ctx.resolver, &ast::Expr::Path(path.clone()));
    ty.check_not_missing(db)?;

    let concrete_struct_id =
        try_extract_matches!(ctx.db.lookup_intern_type(ty), TypeLongId::Concrete)
            .and_then(|c| try_extract_matches!(c, ConcreteTypeId::Struct))
            .ok_or_else(|| ctx.diagnostics.report(&path, NotAStruct))?;

    let members = db.concrete_struct_members(concrete_struct_id)?;
    let mut member_exprs: OrderedHashMap<MemberId, Option<ExprId>> = OrderedHashMap::default();
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
        check_struct_member_is_visible(
            ctx,
            member,
            arg_identifier.stable_ptr().untyped(),
            &arg_name,
        );

        // Extract expression.
        let arg_expr = match arg.arg_expr(syntax_db) {
            ast::OptionStructArgExpr::Empty(_) => {
                let Ok(expr) =
                    resolve_variable_by_name(ctx, &arg_identifier, path.stable_ptr().into())
                else {
                    // Insert only the member id, for correct duplicate member reporting.
                    if member_exprs.insert(member.id, None).is_some() {
                        ctx.diagnostics.report(&arg_identifier, MemberSpecifiedMoreThanOnce);
                    }
                    continue;
                };
                ExprAndId { expr: expr.clone(), id: ctx.exprs.alloc(expr) }
            }
            ast::OptionStructArgExpr::StructArgExpr(arg_expr) => {
                compute_expr_semantic(ctx, &arg_expr.expr(syntax_db))
            }
        };

        // Insert and check for duplicates.
        if member_exprs.insert(member.id, Some(arg_expr.id)).is_some() {
            ctx.diagnostics.report(&arg_identifier, MemberSpecifiedMoreThanOnce);
        }

        // Check types.
        let expected_ty = ctx.reduce_ty(member.ty);
        let actual_ty = ctx.reduce_ty(arg_expr.ty());
        if ctx.resolver.inference().conform_ty(actual_ty, expected_ty).is_err() {
            if !member.ty.is_missing(db) {
                ctx.diagnostics
                    .report(&arg_identifier, WrongArgumentType { expected_ty, actual_ty });
            }
            continue;
        }
    }

    // Report errors for missing members.
    for (member_name, member) in members.iter() {
        if !member_exprs.contains_key(&member.id) {
            ctx.diagnostics.report(ctor_syntax, MissingMember { member_name: member_name.clone() });
        }
    }

    Ok(Expr::StructCtor(ExprStructCtor {
        concrete_struct_id,
        members: member_exprs.into_iter().filter_map(|(x, y)| Some((x, y?))).collect(),
        ty: db.intern_type(TypeLongId::Concrete(ConcreteTypeId::Struct(concrete_struct_id))),
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
    try_extract_matches!(statement_expr.semicolon(syntax_db), ast::OptionTerminalSemicolon::Empty)?;
    Some(statement_expr.expr(syntax_db))
}

/// Creates a new numeric literal expression.
fn new_literal_expr(
    ctx: &mut ComputationContext<'_>,
    ty: Option<&str>,
    value: BigInt,
    stable_ptr: ExprPtr,
) -> Maybe<ExprLiteral> {
    let ty = if let Some(ty_str) = ty {
        try_get_core_ty_by_name(ctx.db, ty_str.into(), vec![])
            .map_err(|err| ctx.diagnostics.report_by_ptr(stable_ptr.untyped(), err))?
    } else {
        ctx.resolver.inference().new_type_var(Some(stable_ptr.untyped()))
    };

    // Numeric trait.
    let trait_id = get_core_trait(ctx.db, "NumericLiteral".into());
    let generic_args = vec![GenericArgumentId::Type(ty)];
    let concrete_trait_id =
        ctx.db.intern_concrete_trait(semantic::ConcreteTraitLongId { trait_id, generic_args });
    let lookup_context = ctx.resolver.impl_lookup_context();
    ctx.resolver
        .inference()
        .new_impl_var(concrete_trait_id, Some(stable_ptr.untyped()), lookup_context)
        .map_err(|err| err.report(ctx.diagnostics, stable_ptr.untyped()))?;

    Ok(ExprLiteral { value, ty, stable_ptr })
}

/// Creates the semantic model of a literal expression from its AST.
fn literal_to_semantic(
    ctx: &mut ComputationContext<'_>,
    literal_syntax: &ast::TerminalLiteralNumber,
) -> Maybe<ExprLiteral> {
    let db = ctx.db;
    let syntax_db = db.upcast();

    let (value, ty) = literal_syntax.numeric_value_and_suffix(syntax_db).unwrap_or_default();
    let ty = ty.as_ref().map(SmolStr::as_str);

    new_literal_expr(ctx, ty, value, literal_syntax.stable_ptr().into())
}

/// Creates the semantic model of a short string from its AST.
fn short_string_to_semantic(
    ctx: &mut ComputationContext<'_>,
    short_string_syntax: &ast::TerminalShortString,
) -> Maybe<ExprLiteral> {
    let db = ctx.db;
    let syntax_db = db.upcast();

    let value = short_string_syntax.numeric_value(syntax_db).unwrap_or_default();

    let suffix = short_string_syntax.suffix(syntax_db);
    let suffix = suffix.as_ref().map(SmolStr::as_str);

    new_literal_expr(ctx, suffix, value, short_string_syntax.stable_ptr().into())
}

/// Creates a new string literal expression.
fn new_string_literal_expr(
    ctx: &mut ComputationContext<'_>,
    value: String,
    stable_ptr: ExprPtr,
) -> Maybe<ExprStringLiteral> {
    let ty = ctx.resolver.inference().new_type_var(Some(stable_ptr.untyped()));

    // String trait.
    let trait_id = get_core_trait(ctx.db, "StringLiteral".into());
    let generic_args = vec![GenericArgumentId::Type(ty)];
    let concrete_trait_id =
        ctx.db.intern_concrete_trait(semantic::ConcreteTraitLongId { trait_id, generic_args });
    let lookup_context = ctx.resolver.impl_lookup_context();
    ctx.resolver
        .inference()
        .new_impl_var(concrete_trait_id, Some(stable_ptr.untyped()), lookup_context)
        .map_err(|err| err.report(ctx.diagnostics, stable_ptr.untyped()))?;

    Ok(ExprStringLiteral { value, ty, stable_ptr })
}

/// Creates the semantic model of a string literal from its AST.
fn string_literal_to_semantic(
    ctx: &mut ComputationContext<'_>,
    string_syntax: &ast::TerminalString,
) -> Maybe<ExprStringLiteral> {
    let db = ctx.db;
    let syntax_db = db.upcast();
    let stable_ptr = string_syntax.stable_ptr();

    let value = string_syntax.string_value(syntax_db).unwrap_or_default();
    // TODO(yuval): support prefixes/suffixes for explicit types?

    new_string_literal_expr(ctx, value, stable_ptr.into())
}

/// Given an expression syntax, if it's an identifier, returns it. Otherwise, returns the proper
/// error.
fn expr_as_identifier(
    ctx: &mut ComputationContext<'_>,
    path: &ast::ExprPath,
    syntax_db: &dyn SyntaxGroup,
) -> Maybe<SmolStr> {
    let segments = path.elements(syntax_db);
    if segments.len() == 1 {
        return Ok(segments[0].identifier(syntax_db));
    }
    Err(ctx.diagnostics.report(path, InvalidMemberExpression))
}

// TODO(spapini): Consider moving some checks here to the responsibility of the parser.
/// Computes the semantic expression for a dot expression.
fn dot_expr(
    ctx: &mut ComputationContext<'_>,
    lexpr: ExprAndId,
    rhs_syntax: ast::Expr,
    stable_ptr: ast::ExprPtr,
) -> Maybe<Expr> {
    // Find MemberId.
    match rhs_syntax {
        ast::Expr::Path(expr) => member_access_expr(ctx, lexpr, expr, stable_ptr),
        ast::Expr::FunctionCall(expr) => method_call_expr(ctx, lexpr, expr, stable_ptr),
        _ => Err(ctx.diagnostics.report(&rhs_syntax, InvalidMemberExpression)),
    }
}

/// Finds all the trait ids usable in the current context.
fn traits_in_context(ctx: &mut ComputationContext<'_>) -> Maybe<OrderedHashSet<TraitId>> {
    let mut traits = ctx.db.module_usable_trait_ids(ctx.resolver.module_file_id.0)?.deref().clone();
    traits
        .extend(ctx.db.module_usable_trait_ids(ctx.resolver.prelude_submodule())?.iter().copied());
    Ok(traits)
}

/// Computes the semantic model of a method call expression (e.g. "expr.method(..)").
/// Finds all traits with at least one candidate impl with a matching `self` param.
/// If more/less than 1 such trait exists, fails.
fn method_call_expr(
    ctx: &mut ComputationContext<'_>,
    lexpr: ExprAndId,
    expr: ast::ExprFunctionCall,
    stable_ptr: ast::ExprPtr,
) -> Maybe<Expr> {
    // TODO(spapini): Add ctx.module_id.
    // TODO(spapini): Look also in uses.
    let syntax_db = ctx.db.upcast();
    let path = expr.path(syntax_db);
    let Ok([segment]): Result<[_; 1], _> = path.elements(syntax_db).try_into() else {
        return Err(ctx.diagnostics.report(&expr, InvalidMemberExpression));
    };
    let func_name = segment.identifier(syntax_db);
    let generic_args_syntax = segment.generic_args(syntax_db);
    // Save some work.
    ctx.resolver.inference().solve().ok();

    let mut candidate_traits = traits_in_context(ctx)?;

    // Add traits from impl generic args in the context.
    for generic_param in &ctx.resolver.data.generic_params {
        if generic_param.kind(ctx.db.upcast()) == GenericKind::Impl {
            let Ok(trait_id) = ctx.db.generic_impl_param_trait(*generic_param) else {
                continue;
            };
            candidate_traits.insert(trait_id);
        }
    }

    let (function_id, fixed_lexpr, mutability) = compute_method_function_call_data(
        ctx,
        Vec::from_iter(candidate_traits).as_slice(),
        func_name,
        lexpr,
        path.stable_ptr().untyped(),
        generic_args_syntax,
        |ty, method_name, inference_errors| CannotCallMethod { ty, method_name, inference_errors },
        |_, trait_function_id0, trait_function_id1| AmbiguousTrait {
            trait_function_id0,
            trait_function_id1,
        },
    )?;
    ctx.resolver.data.resolved_items.mark_concrete(
        ctx.db,
        &segment,
        ResolvedConcreteItem::Function(function_id),
    );

    let named_args: Vec<_> = chain!(
        [NamedArg(fixed_lexpr, None, mutability)],
        expr.arguments(syntax_db)
            .arguments(syntax_db)
            .elements(syntax_db)
            .into_iter()
            .map(|arg_syntax| compute_named_argument_clause(ctx, arg_syntax))
    )
    .collect();

    expr_function_call(ctx, function_id, named_args, stable_ptr)
}

/// Computes the semantic model of a member access expression (e.g. "expr.member").
fn member_access_expr(
    ctx: &mut ComputationContext<'_>,
    lexpr: ExprAndId,
    rhs_syntax: ast::ExprPath,
    stable_ptr: ast::ExprPtr,
) -> Maybe<Expr> {
    let syntax_db = ctx.db.upcast();

    // Find MemberId.
    let member_name = expr_as_identifier(ctx, &rhs_syntax, syntax_db)?;
    let ty = ctx.reduce_ty(lexpr.ty());
    let (n_snapshots, long_ty) = peel_snapshots(ctx.db, ty);
    match long_ty {
        TypeLongId::Concrete(concrete) => match concrete {
            ConcreteTypeId::Struct(concrete_struct_id) => {
                // TODO(lior): Add a diagnostic test when accessing a member of a missing type.
                let members = ctx.db.concrete_struct_members(concrete_struct_id)?;
                let Some(member) = members.get(&member_name) else {
                    return Err(ctx.diagnostics.report(
                        &rhs_syntax,
                        NoSuchMember {
                            struct_id: concrete_struct_id.struct_id(ctx.db),
                            member_name,
                        },
                    ));
                };
                check_struct_member_is_visible(
                    ctx,
                    member,
                    rhs_syntax.stable_ptr().untyped(),
                    &member_name,
                );
                let member_path = if n_snapshots == 0 {
                    lexpr.as_member_path().map(|parent| ExprVarMemberPath::Member {
                        parent: Box::new(parent),
                        member_id: member.id,
                        stable_ptr,
                        concrete_struct_id,
                        ty: member.ty,
                    })
                } else {
                    None
                };
                let lexpr_id = lexpr.id;

                let ty = wrap_in_snapshots(ctx.db, member.ty, n_snapshots);
                Ok(Expr::MemberAccess(ExprMemberAccess {
                    expr: lexpr_id,
                    concrete_struct_id,
                    member: member.id,
                    ty,
                    member_path,
                    n_snapshots,
                    stable_ptr,
                }))
            }
            _ => Err(ctx.diagnostics.report(&rhs_syntax, TypeHasNoMembers { ty, member_name })),
        },
        TypeLongId::Tuple(_) => {
            // TODO(spapini): Handle .0, .1, etc. .
            Err(ctx.diagnostics.report(&rhs_syntax, Unsupported))
        }
        TypeLongId::Snapshot(_) => {
            // TODO(spapini): Handle snapshot members.
            Err(ctx.diagnostics.report(&rhs_syntax, Unsupported))
        }
        TypeLongId::GenericParameter(_) => {
            Err(ctx.diagnostics.report(&rhs_syntax, TypeHasNoMembers { ty, member_name }))
        }
        TypeLongId::Var(_) => Err(ctx
            .diagnostics
            .report(&rhs_syntax, InternalInferenceError(InferenceError::TypeNotInferred { ty }))),
        TypeLongId::Missing(diag_added) => Err(diag_added),
    }
}

/// Resolves a variable or a constant given a context and a path expression.
fn resolve_expr_path(ctx: &mut ComputationContext<'_>, path: &ast::ExprPath) -> Maybe<Expr> {
    let db = ctx.db;
    let syntax_db = db.upcast();
    let segments = path.elements(syntax_db);
    if segments.is_empty() {
        return Err(ctx.diagnostics.report(path, Unsupported));
    }

    // Check if this is a variable.
    if let [PathSegment::Simple(ident_segment)] = &segments[..] {
        let identifier = ident_segment.ident(syntax_db);
        let variable_name = identifier.text(ctx.db.upcast());
        if let Some(res) = get_variable_by_name(ctx, &variable_name, path.stable_ptr().into()) {
            let var = extract_matches!(res.clone(), Expr::Var);
            ctx.resolver.data.resolved_items.generic.insert(
                identifier.stable_ptr(),
                ResolvedGenericItem::Variable(ctx.function.unwrap(), var.var),
            );
            return Ok(res);
        }
    }

    let resolved_item =
        ctx.resolver.resolve_concrete_path(ctx.diagnostics, path, NotFoundItemType::Identifier)?;

    match resolved_item {
        ResolvedConcreteItem::Constant(constant_id) => Ok(Expr::Constant(ExprConstant {
            constant_id,
            ty: db.constant_semantic_data(constant_id)?.value.ty(),
            stable_ptr: path.stable_ptr().into(),
        })),
        ResolvedConcreteItem::Variant(variant) if variant.ty == unit_ty(db) => {
            let stable_ptr = path.stable_ptr().into();
            let concrete_enum_id = variant.concrete_enum_id;
            Ok(semantic::Expr::EnumVariantCtor(semantic::ExprEnumVariantCtor {
                variant,
                value_expr: unit_expr(ctx, stable_ptr),
                ty: db.intern_type(TypeLongId::Concrete(ConcreteTypeId::Enum(concrete_enum_id))),
                stable_ptr,
            }))
        }
        resolved_item => Err(ctx.diagnostics.report(
            path,
            UnexpectedElement {
                expected: vec![ElementKind::Variable, ElementKind::Constant],
                actual: (&resolved_item).into(),
            },
        )),
    }
}

/// Resolves a variable given a context and a simple name.
///
/// Reports a diagnostic if the variable was not found.
pub fn resolve_variable_by_name(
    ctx: &mut ComputationContext<'_>,
    identifier: &ast::TerminalIdentifier,
    stable_ptr: ast::ExprPtr,
) -> Maybe<Expr> {
    let variable_name = identifier.text(ctx.db.upcast());
    let res = get_variable_by_name(ctx, &variable_name, stable_ptr).ok_or_else(|| {
        ctx.diagnostics.report(identifier, VariableNotFound { name: variable_name })
    })?;
    let var = extract_matches!(res.clone(), Expr::Var);

    ctx.resolver.data.resolved_items.generic.insert(
        identifier.stable_ptr(),
        ResolvedGenericItem::Variable(ctx.function.unwrap(), var.var),
    );
    Ok(res)
}

/// Returns the requested variable from the environment if it exists. Returns None otherwise.
pub fn get_variable_by_name(
    ctx: &mut ComputationContext<'_>,
    variable_name: &SmolStr,
    stable_ptr: ast::ExprPtr,
) -> Option<Expr> {
    let mut maybe_env = Some(&mut *ctx.environment);
    while let Some(env) = maybe_env {
        if let Some(var) = env.variables.get(variable_name) {
            env.used_variables.insert(var.id());
            return Some(Expr::Var(ExprVar { var: var.id(), ty: var.ty(), stable_ptr }));
        }
        maybe_env = env.parent.as_deref_mut();
    }
    None
}

/// Typechecks a function call.
fn expr_function_call(
    ctx: &mut ComputationContext<'_>,
    function_id: FunctionId,
    named_args: Vec<NamedArg>,
    stable_ptr: ast::ExprPtr,
) -> Maybe<Expr> {
    // TODO(spapini): Better location for these diagnostics after the refactor for generics resolve.
    // TODO(lior): Check whether concrete_function_signature should be `Option` instead of `Maybe`.
    let signature = ctx.db.concrete_function_signature(function_id)?;

    if named_args.len() != signature.params.len() {
        return Err(ctx.diagnostics.report_by_ptr(
            stable_ptr.untyped(),
            WrongNumberOfArguments { expected: signature.params.len(), actual: named_args.len() },
        ));
    }

    // Check argument names and types.
    check_named_arguments(&named_args, &signature, ctx)?;

    let mut args = Vec::new();
    for (NamedArg(arg, _name, mutability), param) in
        named_args.into_iter().zip(signature.params.iter())
    {
        let arg_typ = arg.ty();
        let param_typ = param.ty;
        // Don't add diagnostic if the type is missing (a diagnostic should have already been
        // added).
        // TODO(lior): Add a test to missing type once possible.
        let expected_ty = ctx.reduce_ty(param_typ);
        let actual_ty = ctx.reduce_ty(arg_typ);
        if !arg_typ.is_missing(ctx.db)
            && ctx.resolver.inference().conform_ty(actual_ty, expected_ty).is_err()
        {
            ctx.diagnostics.report_by_ptr(
                arg.stable_ptr().untyped(),
                WrongArgumentType { expected_ty, actual_ty },
            );
        }

        args.push(if param.mutability == Mutability::Reference {
            // Verify the argument is a variable.
            let Some(ref_arg) = arg.as_member_path() else {
                return Err(ctx
                    .diagnostics
                    .report_by_ptr(arg.stable_ptr().untyped(), RefArgNotAVariable));
            };
            // Verify the variable argument is mutable.
            if !ctx.semantic_defs[&ref_arg.base_var()].is_mut() {
                ctx.diagnostics.report_by_ptr(arg.stable_ptr().untyped(), RefArgNotMutable);
            }
            // Verify that it is passed explicitly as 'ref'.
            if mutability != Mutability::Reference {
                ctx.diagnostics.report_by_ptr(arg.stable_ptr().untyped(), RefArgNotExplicit);
            }
            ExprFunctionCallArg::Reference(ref_arg)
        } else {
            // Verify that it is passed without modifiers.
            if mutability != Mutability::Immutable {
                ctx.diagnostics
                    .report_by_ptr(arg.stable_ptr().untyped(), ImmutableArgWithModifiers);
            }
            ExprFunctionCallArg::Value(arg.id)
        });
    }

    let expr_function_call =
        ExprFunctionCall { function: function_id, args, ty: signature.return_type, stable_ptr };
    // Check panicable.
    if signature.panicable && has_panic_incompatibility(ctx, &expr_function_call)? {
        // TODO(spapini): Delay this check until after inference, to allow resolving specific
        //   impls first.
        return Err(ctx.diagnostics.report_by_ptr(stable_ptr.untyped(), PanicableFromNonPanicable));
    }
    Ok(Expr::FunctionCall(expr_function_call))
}

/// Checks if a panicable function is called from a disallowed context.
fn has_panic_incompatibility(
    ctx: &mut ComputationContext<'_>,
    expr_function_call: &ExprFunctionCall,
) -> Maybe<bool> {
    // If this is not an actual function call, but actually a minus literal (e.g. -1), then this is
    // the same as nopanic.
    if try_extract_minus_literal(ctx.db, &ctx.exprs, expr_function_call).is_some() {
        return Ok(false);
    }
    // If this is not from within a context of a function - e.g. a const item, we will exit with an
    // error here, as this is a call with bad context.
    let caller_signature = ctx.get_signature(
        expr_function_call.stable_ptr.untyped(),
        UnsupportedOutsideOfFunctionFeatureName::FunctionCall,
    )?;
    // If the caller is nopanic, then this is a panic incompatibility.
    Ok(!caller_signature.panicable)
}

/// Checks the correctness of the named arguments, and outputs diagnostics on errors.
fn check_named_arguments(
    named_args: &[NamedArg],
    signature: &Signature,
    ctx: &mut ComputationContext<'_>,
) -> Maybe<()> {
    let mut res: Maybe<()> = Ok(());

    // Indicates whether we saw a named argument. Used to report a diagnostic if an unnamed argument
    // will follow it.
    let mut seen_named_arguments: bool = false;
    // Indicates whether a [UnnamedArgumentFollowsNamed] diagnostic was reported. Used to prevent
    // multiple similar diagnostics.
    let mut reported_unnamed_argument_follows_named: bool = false;
    for (NamedArg(arg, name_opt, _mutability), param) in
        named_args.iter().zip(signature.params.iter())
    {
        // Check name.
        if let Some(name_terminal) = name_opt {
            seen_named_arguments = true;
            let name = name_terminal.text(ctx.db.upcast());
            if param.name != name.clone() {
                res = Err(ctx.diagnostics.report_by_ptr(
                    name_terminal.stable_ptr().untyped(),
                    NamedArgumentMismatch { expected: param.name.clone(), found: name },
                ));
            }
        } else if seen_named_arguments && !reported_unnamed_argument_follows_named {
            reported_unnamed_argument_follows_named = true;
            res = Err(ctx
                .diagnostics
                .report_by_ptr(arg.stable_ptr().untyped(), UnnamedArgumentFollowsNamed));
        }
    }
    res
}

/// Computes the semantic model of a statement (excluding tail-expression).
pub fn compute_statement_semantic(
    ctx: &mut ComputationContext<'_>,
    syntax: ast::Statement,
) -> Maybe<StatementId> {
    let db = ctx.db;
    let syntax_db = db.upcast();
    // As for now, statement attributes does not have any semantic affect, so we only validate they
    // are allowed.
    validate_statement_attributes(ctx, &syntax);
    let statement = match &syntax {
        ast::Statement::Let(let_syntax) => {
            let expr = compute_expr_semantic(ctx, &let_syntax.rhs(syntax_db));
            let inferred_type = expr.ty();
            let rhs_expr_id = expr.id;

            let ty = match let_syntax.type_clause(syntax_db) {
                ast::OptionTypeClause::Empty(_) => inferred_type,
                ast::OptionTypeClause::TypeClause(type_clause) => {
                    let var_type_path = type_clause.ty(syntax_db);
                    let explicit_type =
                        resolve_type(db, ctx.diagnostics, &mut ctx.resolver, &var_type_path);
                    let explicit_type = ctx.reduce_ty(explicit_type);
                    let inferred_type = ctx.reduce_ty(inferred_type);
                    if !inferred_type.is_missing(db)
                        && ctx
                            .resolver
                            .inference()
                            .conform_ty(inferred_type, explicit_type)
                            .is_err()
                    {
                        ctx.diagnostics.report(
                            &let_syntax.rhs(syntax_db),
                            WrongArgumentType {
                                expected_ty: explicit_type,
                                actual_ty: inferred_type,
                            },
                        );
                    }
                    explicit_type
                }
            };

            let pattern = compute_pattern_semantic(ctx, &let_syntax.pattern(syntax_db), ty);
            let variables = pattern.variables(&ctx.patterns);
            // TODO(yuval): allow unnamed variables. Add them here to
            // ctx.environment.unnamed_variables
            for v in variables {
                let var_def = Variable::Local(v.var.clone());
                if let Some(old_var) =
                    ctx.environment.variables.insert(v.name.clone(), var_def.clone())
                {
                    ctx.add_unused_variable_warning(&v.name, &old_var);
                }
                ctx.semantic_defs.insert(var_def.id(), var_def);
            }
            semantic::Statement::Let(semantic::StatementLet {
                pattern: pattern.id,
                expr: rhs_expr_id,
                stable_ptr: syntax.stable_ptr(),
            })
        }
        ast::Statement::Expr(stmt_expr_syntax) => {
            let expr_syntax = stmt_expr_syntax.expr(syntax_db);
            let expr = compute_expr_semantic(ctx, &expr_syntax);
            if matches!(
                stmt_expr_syntax.semicolon(syntax_db),
                ast::OptionTerminalSemicolon::Empty(_)
            ) && !matches!(
                expr_syntax,
                ast::Expr::Block(_) | ast::Expr::If(_) | ast::Expr::Match(_)
            ) {
                // Point to after the expression, where the semicolon is missing.
                ctx.diagnostics.report_after(&expr_syntax, MissingSemicolon);
            }
            let ty: TypeId = expr.ty();
            if let TypeLongId::Concrete(concrete) = db.lookup_intern_type(ty) {
                if match concrete {
                    ConcreteTypeId::Struct(id) => id.has_attr(db, MUST_USE_ATTR)?,
                    ConcreteTypeId::Enum(id) => id.has_attr(db, MUST_USE_ATTR)?,
                    ConcreteTypeId::Extern(_) => false,
                } {
                    ctx.diagnostics.report(&expr_syntax, UnhandledMustUseType { ty });
                }
            }
            if let Expr::FunctionCall(expr_function_call) = &expr.expr {
                let generic_function_id = db
                    .lookup_intern_function(expr_function_call.function)
                    .function
                    .generic_function;
                if match generic_function_id {
                    crate::items::functions::GenericFunctionId::Free(id) => {
                        id.has_attr(db, MUST_USE_ATTR)?
                    }
                    crate::items::functions::GenericFunctionId::Impl(id) => {
                        id.function.has_attr(db, MUST_USE_ATTR)?
                    }
                    crate::items::functions::GenericFunctionId::Extern(_) => false,
                } {
                    ctx.diagnostics.report(&expr_syntax, UnhandledMustUseFunction);
                }
            }
            semantic::Statement::Expr(semantic::StatementExpr {
                expr: expr.id,
                stable_ptr: syntax.stable_ptr(),
            })
        }
        ast::Statement::Continue(continue_syntax) => {
            if ctx.loop_flow_merge.is_none() {
                return Err(ctx
                    .diagnostics
                    .report(continue_syntax, ContinueOnlyAllowedInsideALoop));
            }
            semantic::Statement::Continue(semantic::StatementContinue {
                stable_ptr: syntax.stable_ptr(),
            })
        }
        ast::Statement::Return(return_syntax) => {
            if ctx.loop_flow_merge.is_some() {
                return Err(ctx.diagnostics.report(return_syntax, ReturnNotAllowedInsideALoop));
            }

            let (expr_option, expr_ty, stable_ptr) = match return_syntax.expr_clause(syntax_db) {
                ast::OptionExprClause::Empty(empty_clause) => {
                    (None, unit_ty(db), empty_clause.stable_ptr().untyped())
                }
                ast::OptionExprClause::ExprClause(expr_clause) => {
                    let expr_syntax = expr_clause.expr(syntax_db);
                    let expr = compute_expr_semantic(ctx, &expr_syntax);
                    (Some(expr.id), expr.ty(), expr_syntax.stable_ptr().untyped())
                }
            };
            let expected_ty = ctx
                .get_signature(
                    return_syntax.stable_ptr().untyped(),
                    UnsupportedOutsideOfFunctionFeatureName::ReturnStatement,
                )?
                .return_type;
            if !expected_ty.is_missing(db)
                && !expr_ty.is_missing(db)
                && ctx.resolver.inference().conform_ty(expr_ty, expected_ty).is_err()
            {
                ctx.diagnostics
                    .report_by_ptr(stable_ptr, WrongReturnType { expected_ty, actual_ty: expr_ty });
            }
            semantic::Statement::Return(semantic::StatementReturn {
                expr_option,
                stable_ptr: syntax.stable_ptr(),
            })
        }
        ast::Statement::Break(break_syntax) => {
            let (expr_option, ty, stable_ptr) = match break_syntax.expr_clause(syntax_db) {
                ast::OptionExprClause::Empty(expr_empty) => {
                    (None, unit_ty(db), expr_empty.stable_ptr().untyped())
                }
                ast::OptionExprClause::ExprClause(expr_clause) => {
                    let expr_syntax = expr_clause.expr(syntax_db);
                    let expr = compute_expr_semantic(ctx, &expr_syntax);
                    (Some(expr.id), expr.ty(), expr.stable_ptr().untyped())
                }
            };
            let Some(flow_merge) = ctx.loop_flow_merge.as_mut() else {
                return Err(ctx.diagnostics.report(break_syntax, BreakOnlyAllowedInsideALoop));
            };
            if let Err((current_ty, break_ty)) =
                flow_merge.try_merge_types(&mut ctx.resolver.inference(), ctx.db, ty)
            {
                ctx.diagnostics
                    .report_by_ptr(stable_ptr, IncompatibleLoopBreakTypes { current_ty, break_ty });
            };
            semantic::Statement::Break(semantic::StatementBreak {
                expr_option,
                stable_ptr: syntax.stable_ptr(),
            })
        }
        ast::Statement::Missing(_) => todo!(),
    };
    Ok(ctx.statements.alloc(statement))
}

/// Validates a struct member is visible and otherwise adds a diagnostic.
fn check_struct_member_is_visible(
    ctx: &mut ComputationContext<'_>,
    member: &Member,
    stable_ptr: SyntaxStablePtrId,
    member_name: &SmolStr,
) {
    let db = ctx.db.upcast();
    let containing_module_id = member.id.parent_module(db);
    if ctx.resolver.ignore_visibility_checks(containing_module_id) {
        return;
    }
    let user_module_id = ctx.resolver.module_file_id.0;
    if !visibility::peek_visible_in(db, member.visibility, containing_module_id, user_module_id) {
        ctx.diagnostics
            .report_by_ptr(stable_ptr, MemberNotVisible { member_name: member_name.clone() });
    }
}

/// Verifies that the statement attributes are valid statements attributes, if not a diagnostic is
/// reported.
fn validate_statement_attributes(ctx: &mut ComputationContext<'_>, syntax: &ast::Statement) {
    let allowed_attributes = ctx.db.allowed_statement_attributes();
    let module_file_id = ctx.resolver.module_file_id;
    let mut diagnostics = vec![];
    validate_attributes_flat(
        ctx.db.upcast(),
        &allowed_attributes,
        module_file_id,
        syntax,
        &mut diagnostics,
    );
    // Translate the plugin diagnostics to semantic diagnostics.
    for (_, diagnostic) in diagnostics {
        ctx.diagnostics.report_by_ptr(
            diagnostic.stable_ptr,
            SemanticDiagnosticKind::UnknownStatementAttribute,
        );
    }
}
