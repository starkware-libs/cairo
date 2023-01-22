//! This module is responsible of computing the semantic model of expressions and statements in
//! the code, while type checking.
//! It is invoked by queries for function bodies and other code blocks.

use std::collections::HashMap;

use ast::{BinaryOperator, PathSegment};
use cairo_lang_defs::ids::{FunctionSignatureId, LocalVarLongId, MemberId, TraitId};
use cairo_lang_diagnostics::{skip_diagnostic, Maybe, ToMaybe, ToOption};
use cairo_lang_syntax::node::ast::{BlockOrIf, PatternStructParam};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::{GetIdentifier, PathSegmentEx};
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_syntax::node::{ast, Terminal, TypedSyntaxNode};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use cairo_lang_utils::unordered_hash_set::UnorderedHashSet;
use cairo_lang_utils::{try_extract_matches, OptionHelper};
use id_arena::Arena;
use itertools::{chain, zip_eq};
use num_bigint::{BigInt, Sign};
use smol_str::SmolStr;
use unescaper::unescape;

use super::inference::Inference;
use super::objects::*;
use super::pattern::{
    Pattern, PatternEnumVariant, PatternLiteral, PatternOtherwise, PatternTuple, PatternVariable,
};
use crate::corelib::{
    core_binary_operator, core_felt_ty, core_unary_operator, false_literal_expr, never_ty,
    true_literal_expr, try_get_const_libfunc_name_by_type, try_get_core_ty_by_name, unit_ty,
    unwrap_error_propagation_type,
};
use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnosticKind::*;
use crate::diagnostic::{
    ElementKind, NotFoundItemType, SemanticDiagnostics, UnsupportedOutsideOfFunctionFeatureName,
};
use crate::items::enm::SemanticEnumEx;
use crate::items::functions::{ConcreteImplGenericFunctionId, GenericFunctionId};
use crate::items::imp::find_impls_at_context;
use crate::items::modifiers::compute_mutability;
use crate::items::strct::SemanticStructEx;
use crate::items::trt::{ConcreteTraitFunctionId, ConcreteTraitFunctionLongId};
use crate::literals::LiteralLongId;
use crate::resolve_path::{ResolvedConcreteItem, ResolvedGenericItem, Resolver};
use crate::semantic::{self, FunctionId, LocalVariable, TypeId, TypeLongId, Variable};
use crate::types::{resolve_type_with_inference, ConcreteTypeId};
use crate::{ConcreteFunction, FunctionLongId, Mutability, Parameter, PatternStruct, Signature};

/// Context for computing the semantic model of expression trees.
pub struct ComputationContext<'ctx> {
    pub db: &'ctx dyn SemanticGroup,
    pub diagnostics: &'ctx mut SemanticDiagnostics,
    pub resolver: Resolver<'ctx>,
    signature: Option<&'ctx Signature>,
    environment: Box<Environment>,
    pub exprs: Arena<semantic::Expr>,
    pub statements: Arena<semantic::Statement>,
    /// Definitions of semantic variables.
    pub semantic_defs: UnorderedHashMap<semantic::VarId, semantic::Variable>,
    pub inference: Inference<'ctx>,
}
impl<'ctx> ComputationContext<'ctx> {
    pub fn new(
        db: &'ctx dyn SemanticGroup,
        diagnostics: &'ctx mut SemanticDiagnostics,
        resolver: Resolver<'ctx>,
        signature: Option<&'ctx Signature>,
        environment: Environment,
    ) -> Self {
        let semantic_defs =
            environment.variables.values().by_ref().map(|var| (var.id(), var.clone())).collect();
        Self {
            db,
            diagnostics,
            resolver,
            signature,
            environment: Box::new(environment),
            exprs: Arena::default(),
            statements: Arena::default(),
            semantic_defs,
            inference: Inference::new(db),
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
        self.environment = parent.unwrap();
        res
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
        self.inference.reduce_ty(ty)
    }
}

// TODO(ilya): Change value to VarId.
pub type EnvVariables = HashMap<SmolStr, Variable>;

// TODO(spapini): Consider using identifiers instead of SmolStr everywhere in the code.
/// A state which contains all the variables defined at the current resolver until now, and a
/// pointer to the parent environment.
#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct Environment {
    parent: Option<Box<Environment>>,
    variables: EnvVariables,
}
impl Environment {
    /// Adds a parameter to the environment.
    pub fn add_param(
        &mut self,
        diagnostics: &mut SemanticDiagnostics,
        semantic_param: Parameter,
        ast_param: &ast::Param,
        function_signature_id: FunctionSignatureId,
    ) -> Maybe<()> {
        let name = &semantic_param.name;
        match self.variables.entry(name.clone()) {
            std::collections::hash_map::Entry::Occupied(_) => Err(diagnostics.report(
                ast_param,
                ParamNameRedefinition { function_signature_id, param_name: name.clone() },
            )),
            std::collections::hash_map::Entry::Vacant(entry) => {
                entry.insert(Variable::Param(semantic_param));
                Ok(())
            }
        }
    }
}

/// Computes the semantic model of an expression.
pub fn compute_expr_semantic(ctx: &mut ComputationContext<'_>, syntax: &ast::Expr) -> Expr {
    let expr = maybe_compute_expr_semantic(ctx, syntax);
    wrap_maybe_with_missing(ctx, expr, syntax.stable_ptr())
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

/// Computes the semantic model of an expression, or returns a SemanticDiagnosticKind on error,
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
        ast::Expr::ErrorPropagate(expr) => compute_expr_error_propagate_semantic(ctx, expr),
        ast::Expr::Missing(_) | ast::Expr::FieldInitShorthand(_) => {
            Err(ctx.diagnostics.report(syntax, Unsupported))
        }
    }
}
fn compute_expr_unary_semantic(
    ctx: &mut ComputationContext<'_>,
    syntax: &ast::ExprUnary,
) -> Maybe<Expr> {
    let syntax_db = ctx.db.upcast();

    let unary_op = syntax.op(syntax_db);
    let expr = compute_expr_semantic(ctx, &syntax.expr(syntax_db));

    let expr_ty = ctx.reduce_ty(expr.ty());
    let function = match core_unary_operator(ctx.db, &unary_op, expr_ty) {
        Err(err_kind) => {
            return Err(ctx.diagnostics.report(&unary_op, err_kind));
        }
        Ok(function) => function,
    };
    expr_function_call(
        ctx,
        function,
        vec![(expr, None, Mutability::Immutable)],
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
    if matches!(binary_op, BinaryOperator::Dot(_)) {
        return dot_expr(ctx, lexpr, rhs_syntax, stable_ptr);
    }
    let rexpr = compute_expr_semantic(ctx, &rhs_syntax);
    if matches!(binary_op, BinaryOperator::Eq(_)) {
        return match lexpr {
            Expr::Var(ExprVar { var, .. }) => {
                // The semantic_var must be valid as 'lexpr' is a result of compute_expr_semantic.
                let semantic_var = ctx.semantic_defs.get(&var).unwrap();

                if ctx.inference.conform_ty(semantic_var.ty(), rexpr.ty()).is_err() {
                    return Err(ctx.diagnostics.report(
                        &rhs_syntax,
                        WrongArgumentType { expected_ty: semantic_var.ty(), actual_ty: rexpr.ty() },
                    ));
                }
                if !semantic_var.is_mut() {
                    ctx.diagnostics.report(syntax, AssignmentToImmutableVar);
                }
                Ok(Expr::Assignment(ExprAssignment {
                    var,
                    rhs: ctx.exprs.alloc(rexpr),
                    ty: unit_ty(db),
                    stable_ptr,
                }))
            }
            _ => Err(ctx.diagnostics.report(lhs_syntax, InvalidLhsForAssignment)),
        };
    }
    // TODO(spapini): Use a trait here.
    let function = match core_binary_operator(
        db,
        &binary_op,
        ctx.reduce_ty(lexpr.ty()),
        ctx.reduce_ty(rexpr.ty()),
    )? {
        Err(err_kind) => {
            return Err(ctx.diagnostics.report(&binary_op, err_kind));
        }
        Ok(function) => function,
    };
    expr_function_call(
        ctx,
        function,
        vec![(lexpr, None, Mutability::Immutable), (rexpr, None, Mutability::Immutable)],
        stable_ptr,
    )
}

fn compute_expr_tuple_semantic(
    ctx: &mut ComputationContext<'_>,
    syntax: &ast::ExprTuple,
) -> Maybe<Expr> {
    let db = ctx.db;
    let syntax_db = db.upcast();

    let mut items: Vec<ExprId> = vec![];
    let mut types: Vec<TypeId> = vec![];
    for expr_syntax in syntax.expressions(syntax_db).elements(syntax_db) {
        let expr_semantic = compute_expr_semantic(ctx, &expr_syntax);
        types.push(ctx.reduce_ty(expr_semantic.ty()));
        items.push(ctx.exprs.alloc(expr_semantic));
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
    let item = ctx.resolver.resolve_concrete_path(
        ctx.diagnostics,
        &mut ctx.inference,
        &path,
        NotFoundItemType::Function,
    )?;
    let args_syntax = syntax.arguments(syntax_db);
    let named_args: Vec<_> = args_syntax
        .args(syntax_db)
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
            let (arg, name_terminal, mutability) = named_args[0].clone();
            if let Some(name_terminal) = name_terminal {
                ctx.diagnostics.report(&name_terminal, NamedArgumentsAreNotSupported);
            }
            if mutability != Mutability::Immutable {
                return Err(ctx.diagnostics.report(&args_syntax, VariantCtorNotImmutable));
            }
            if ctx.inference.conform_ty(concrete_variant.ty, arg.ty()).is_err() {
                return Err(ctx.diagnostics.report(
                    &args_syntax,
                    WrongArgumentType { expected_ty: concrete_variant.ty, actual_ty: arg.ty() },
                ));
            }
            let concrete_enum_id = concrete_variant.concrete_enum_id;
            Ok(semantic::Expr::EnumVariantCtor(semantic::ExprEnumVariantCtor {
                variant: concrete_variant,
                value_expr: ctx.exprs.alloc(arg),
                ty: db.intern_type(TypeLongId::Concrete(ConcreteTypeId::Enum(concrete_enum_id))),
                stable_ptr: syntax.stable_ptr().into(),
            }))
        }
        ResolvedConcreteItem::Function(function) => {
            expr_function_call(ctx, function, named_args, syntax.stable_ptr().into())
        }
        ResolvedConcreteItem::TraitFunction(trait_function) => {
            let function = db.intern_function(FunctionLongId {
                function: ConcreteFunction {
                    generic_function: GenericFunctionId::Trait(trait_function),
                    generic_args: vec![],
                },
            });
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
) -> (Expr, Option<ast::TerminalIdentifier>, Mutability) {
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
            (expr, Some(arg_name_identifier))
        }
    };

    (expr, arg_name_identifier, mutability)
}

/// Resolves a trait function to an impl function.
pub fn resolve_trait_function(
    db: &dyn SemanticGroup,
    diagnostics: &mut SemanticDiagnostics,
    inference: &mut Inference<'_>,
    resolver: &mut Resolver<'_>,
    concrete_trait_function: ConcreteTraitFunctionId,
    stable_ptr: SyntaxStablePtrId,
) -> Maybe<ConcreteImplGenericFunctionId> {
    // Resolve impl.
    let concrete_impl =
        resolver.resolve_trait(diagnostics, inference, concrete_trait_function, stable_ptr)?;
    let impl_id = db.lookup_intern_concrete_impl(concrete_impl).impl_id;
    let impl_function = db
        .impl_function_by_trait_function(impl_id, concrete_trait_function.function_id(db))?
        .unwrap();
    Ok(ConcreteImplGenericFunctionId { concrete_impl, function: impl_function })
}

pub fn compute_root_expr(
    ctx: &mut ComputationContext<'_>,
    syntax: &ast::ExprBlock,
    return_type: TypeId,
) -> Maybe<Expr> {
    let res = compute_expr_block_semantic(ctx, syntax)?;
    if ctx.inference.conform_ty(res.ty(), return_type).is_err() {
        ctx.diagnostics
            .report(syntax, WrongReturnType { expected_ty: return_type, actual_ty: res.ty() });
        return Ok(res);
    }

    // Apply inference.
    for (_id, expr) in ctx.exprs.iter_mut() {
        match expr {
            Expr::Constant(expr) => expr.ty = ctx.inference.reduce_ty(expr.ty),
            Expr::Tuple(expr) => expr.ty = ctx.inference.reduce_ty(expr.ty),
            Expr::Assignment(expr) => expr.ty = ctx.inference.reduce_ty(expr.ty),
            Expr::Block(expr) => expr.ty = ctx.inference.reduce_ty(expr.ty),
            Expr::FunctionCall(call_expr) => {
                call_expr.ty = ctx.inference.reduce_ty(call_expr.ty);
                let mut long_function_id = ctx.db.lookup_intern_function(call_expr.function);
                long_function_id.function.generic_args =
                    ctx.inference.reduce_generic_args(&long_function_id.function.generic_args);
                if let GenericFunctionId::Trait(concrete_trait_function) =
                    long_function_id.function.generic_function
                {
                    let concrete_impl_function = match resolve_trait_function(
                        ctx.db,
                        ctx.diagnostics,
                        &mut ctx.inference,
                        &mut ctx.resolver,
                        concrete_trait_function,
                        call_expr.stable_ptr.untyped(),
                    ) {
                        Ok(concrete_impl_function) => concrete_impl_function,
                        Err(diag_added) => {
                            *expr = Expr::Missing(ExprMissing {
                                ty: TypeId::missing(ctx.db, diag_added),
                                stable_ptr: call_expr.stable_ptr,
                                diag_added,
                            });
                            continue;
                        }
                    };
                    long_function_id.function.generic_function =
                        GenericFunctionId::Impl(concrete_impl_function);
                }
                call_expr.function = ctx.db.intern_function(long_function_id)
            }
            Expr::Match(expr) => {
                expr.ty = ctx.inference.reduce_ty(expr.ty);
                for arm in expr.arms.iter_mut() {
                    ctx.inference.reduce_pattern(&mut arm.pattern);
                }
            }
            Expr::If(expr) => expr.ty = ctx.inference.reduce_ty(expr.ty),
            Expr::Var(expr) => expr.ty = ctx.inference.reduce_ty(expr.ty),
            Expr::Literal(_) => {
                // TODO(spapini): Support literal inference. Perhaps using Numeric trait.
            }
            Expr::MemberAccess(expr) => expr.ty = ctx.inference.reduce_ty(expr.ty),
            Expr::StructCtor(expr) => {
                expr.ty = ctx.inference.reduce_ty(expr.ty);
            }
            Expr::EnumVariantCtor(expr) => {
                expr.ty = ctx.inference.reduce_ty(expr.ty);
                ctx.inference.reduce_concrete_variant(&mut expr.variant);
            }
            Expr::PropagateError(expr) => {
                ctx.inference.reduce_concrete_variant(&mut expr.err_variant);
                ctx.inference.reduce_concrete_variant(&mut expr.ok_variant);
                ctx.inference.reduce_concrete_variant(&mut expr.func_err_variant);
            }
            Expr::Missing(_) => {}
        };
    }
    for (_id, stmt) in ctx.statements.iter_mut() {
        match stmt {
            Statement::Let(stmt) => ctx.inference.reduce_pattern(&mut stmt.pattern),
            Statement::Expr(_) | Statement::Return(_) => {}
        }
    }

    // Check fully resolved.
    if let Some(stable_ptr) = ctx.inference.first_undetermined_variable() {
        ctx.diagnostics.report_by_ptr(stable_ptr, TypeYetUnknown);
        return Ok(res);
    }

    Ok(res)
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
            if let Statement::Return(_) = &new_ctx.statements[*statement] {
                never_ty(new_ctx.db)
            } else {
                unit_ty(db)
            }
        } else {
            unit_ty(db)
        };
        Ok(Expr::Block(ExprBlock {
            statements: statements_semantic,
            tail: tail_semantic_expr.map(|expr| new_ctx.exprs.alloc(expr)),
            ty,
            stable_ptr: syntax.stable_ptr().into(),
        }))
    })
}

/// Helper for merging the return types of branch blocks (match or if else).
struct FlowMergeTypeHelper<'a, 'ctx> {
    inference: &'a mut Inference<'ctx>,
    never_type: TypeId,
    missing_type: TypeId,
    final_type: Option<TypeId>,
}
impl<'a, 'ctx> FlowMergeTypeHelper<'a, 'ctx> {
    fn new(db: &dyn SemanticGroup, inference: &'a mut Inference<'ctx>) -> Self {
        Self {
            inference,
            never_type: never_ty(db),
            missing_type: TypeId::missing(db, skip_diagnostic()),
            final_type: None,
        }
    }

    /// Attempt merge a branch into the helper, on error will return the conflicting types.
    fn try_merge_types(&mut self, ty: TypeId) -> Result<(), (TypeId, TypeId)> {
        if ty != self.never_type && ty != self.missing_type {
            if let Some(existing) = &self.final_type {
                if self.inference.conform_ty(ty, *existing).is_err() {
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
                    compute_pattern_semantic(new_ctx, syntax_arm.pattern(syntax_db), expr.ty())?;
                let variables = pattern.variables();
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
    let mut helper = FlowMergeTypeHelper::new(ctx.db, &mut ctx.inference);
    for (_, expr) in pattern_and_expr_options.iter().flatten() {
        if let Err((match_ty, arm_ty)) = helper.try_merge_types(expr.ty()) {
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
        .map(|(pattern, arm_expr)| MatchArm { pattern, expression: ctx.exprs.alloc(arm_expr) })
        .collect();
    Ok(Expr::Match(ExprMatch {
        matched_expr: ctx.exprs.alloc(expr),
        arms: semantic_arms,
        ty: helper.get_final_type(),
        stable_ptr: syntax.stable_ptr().into(),
    }))
}

/// Computes the semantic model of an expression of type [ast::ExprIf].
fn compute_expr_if_semantic(ctx: &mut ComputationContext<'_>, syntax: &ast::ExprIf) -> Maybe<Expr> {
    let syntax_db = ctx.db.upcast();

    let expr = compute_expr_semantic(ctx, &syntax.condition(syntax_db));
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

    let mut helper = FlowMergeTypeHelper::new(ctx.db, &mut ctx.inference);
    helper
        .try_merge_types(if_block.ty())
        .and(helper.try_merge_types(else_block_ty))
        .unwrap_or_else(|(block_if_ty, block_else_ty)| {
            ctx.diagnostics.report(syntax, IncompatibleIfBlockTypes { block_if_ty, block_else_ty });
        });
    Ok(Expr::If(ExprIf {
        condition: ctx.exprs.alloc(expr),
        if_block: ctx.exprs.alloc(if_block),
        else_block: else_block_opt.map(|else_block| ctx.exprs.alloc(else_block)),
        ty: helper.get_final_type(),
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
    // TODO(orizi): When auto conversion of types is added, try to convert the error type.
    if func_err_variant.ty != err_variant.ty
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
        inner: ctx.exprs.alloc(inner),
        ok_variant,
        err_variant,
        func_err_variant,
        stable_ptr: syntax.stable_ptr().into(),
    }))
}

/// Computes the semantic model of a pattern, or None if invalid.
fn compute_pattern_semantic(
    ctx: &mut ComputationContext<'_>,
    pattern_syntax: ast::Pattern,
    ty: TypeId,
) -> Maybe<Pattern> {
    // TODO(spapini): Check for missing type, and don't reemit an error.
    let syntax_db = ctx.db.upcast();
    let ty = ctx.inference.reduce_ty(ty);
    Ok(match pattern_syntax {
        ast::Pattern::Underscore(otherwise_pattern) => {
            Pattern::Otherwise(PatternOtherwise { ty, stable_ptr: otherwise_pattern.stable_ptr() })
        }
        ast::Pattern::Literal(literal_pattern) => {
            let literal = literal_to_semantic(ctx, &literal_pattern)?;
            if ctx.inference.conform_ty(ty, core_felt_ty(ctx.db)).is_err() {
                return Err(ctx
                    .diagnostics
                    .report(&literal_pattern, UnexpectedLiteralPattern { ty }));
            }
            Pattern::Literal(PatternLiteral {
                literal,
                ty,
                stable_ptr: literal_pattern.stable_ptr().into(),
            })
        }
        ast::Pattern::ShortString(short_string_pattern) => {
            let literal = short_string_to_semantic(ctx, &short_string_pattern)?;
            if ctx.inference.conform_ty(ty, core_felt_ty(ctx.db)).is_err() {
                return Err(ctx
                    .diagnostics
                    .report(&short_string_pattern, UnexpectedLiteralPattern { ty }));
            }
            Pattern::Literal(PatternLiteral {
                literal,
                ty,
                stable_ptr: short_string_pattern.stable_ptr().into(),
            })
        }
        ast::Pattern::Enum(enum_pattern) => {
            // Check that type is an enum, and get the concrete enum from it.
            let concrete_enum =
                try_extract_matches!(ctx.db.lookup_intern_type(ty), TypeLongId::Concrete)
                    .and_then(|c| try_extract_matches!(c, ConcreteTypeId::Enum))
                    .ok_or_else(|| {
                        // Don't add a diagnostic if the type is missing.
                        // A diagnostic should've already been added.
                        if !ty.is_missing(ctx.db) {
                            ctx.diagnostics.report(&enum_pattern, UnexpectedEnumPattern { ty })
                        } else {
                            // TODO(lior): Take the DiagnosticAdded from the missing type.
                            skip_diagnostic()
                        }
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
            let ty = concrete_variant.ty;
            let inner_pattern =
                compute_pattern_semantic(ctx, enum_pattern.pattern(syntax_db), ty)?.into();
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
                return Err(ctx.diagnostics.report(&path, Unsupported));
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
            // Check that type is an struct, and get the concrete struct from it.
            let concrete_struct =
                try_extract_matches!(ctx.db.lookup_intern_type(ty), TypeLongId::Concrete)
                    .and_then(|c| try_extract_matches!(c, ConcreteTypeId::Struct))
                    .ok_or_else(|| {
                        // Don't add a diagnostic if the type is missing.
                        // A diagnostic should've already been added.
                        if !ty.is_missing(ctx.db) {
                            ctx.diagnostics.report(&pattern_struct, UnexpectedEnumPattern { ty })
                        } else {
                            // TODO(lior): Take the DiagnosticAdded from the missing type.
                            skip_diagnostic()
                        }
                    })?;
            let pattern_param_asts = pattern_struct.params(syntax_db).elements(syntax_db);
            let struct_id = concrete_struct.struct_id(ctx.db);
            let mut members = ctx.db.struct_members(struct_id).expect("This is a valid struct.");
            let mut used_members = UnorderedHashSet::default();
            let mut get_member = |ctx: &mut ComputationContext<'_>, member_name: SmolStr| {
                let member = members.swap_remove(&member_name).on_none(|| {
                    ctx.diagnostics.report(
                        &pattern_struct,
                        if used_members.contains(&member_name) {
                            StructMemberRedefinition { struct_id, member_name: member_name.clone() }
                        } else {
                            NoSuchMember { struct_id, member_name: member_name.clone() }
                        },
                    );
                })?;
                used_members.insert(member_name);
                Some(member)
            };
            let mut field_patterns = vec![];
            let mut has_tail = false;
            for pattern_param_ast in pattern_param_asts {
                match pattern_param_ast {
                    PatternStructParam::Single(single) => {
                        let name = single.name(syntax_db);
                        let member = get_member(ctx, name.text(syntax_db)).to_maybe()?;
                        let pattern = create_variable_pattern(
                            ctx,
                            name,
                            &single.modifiers(syntax_db).elements(syntax_db),
                            member.ty,
                            single.stable_ptr().into(),
                        );
                        field_patterns.push((member, Box::new(pattern)));
                    }
                    PatternStructParam::WithExpr(with_expr) => {
                        let member = get_member(ctx, with_expr.name(syntax_db).text(syntax_db))
                            .to_maybe()?;
                        let pattern =
                            compute_pattern_semantic(ctx, with_expr.pattern(syntax_db), member.ty)?;
                        field_patterns.push((member, Box::new(pattern)));
                    }
                    PatternStructParam::Tail(_) => {
                        has_tail = true;
                    }
                }
            }
            if !has_tail {
                for (member_name, _) in members {
                    ctx.diagnostics.report(&pattern_struct, MissingMember { member_name });
                }
            }
            Pattern::Struct(PatternStruct {
                id: struct_id,
                field_patterns,
                ty,
                stable_ptr: pattern_struct.stable_ptr(),
            })
        }
        ast::Pattern::Tuple(pattern_tuple) => {
            let tys = try_extract_matches!(ctx.db.lookup_intern_type(ty), TypeLongId::Tuple)
                .ok_or_else(|| {
                    ctx.diagnostics.report(&pattern_tuple, UnexpectedTuplePattern { ty })
                })?;

            let patterns_ast = pattern_tuple.patterns(syntax_db).elements(syntax_db);
            if tys.len() != patterns_ast.len() {
                return Err(ctx.diagnostics.report(
                    &pattern_tuple,
                    WrongNumberOfGenericArguments {
                        expected: tys.len(),
                        actual: patterns_ast.len(),
                    },
                ));
            }
            // Iterator of Option<Pattern?, for each field.
            let pattern_options = zip_eq(patterns_ast.into_iter(), tys).map(|(pattern_ast, ty)| {
                Ok(Box::new(compute_pattern_semantic(ctx, pattern_ast, ty)?))
            });
            // If all are Some, collect into a Vec.
            let field_patterns: Vec<_> = pattern_options.collect::<Maybe<_>>()?;

            Pattern::Tuple(PatternTuple {
                field_patterns,
                ty,
                stable_ptr: pattern_tuple.stable_ptr(),
            })
        }
    })
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
    let ty = resolve_type_with_inference(
        db,
        ctx.diagnostics,
        &mut ctx.inference,
        &mut ctx.resolver,
        &ast::Expr::Path(path.clone()),
    );
    ty.check_not_missing(db)?;

    let concrete_struct = try_extract_matches!(ctx.db.lookup_intern_type(ty), TypeLongId::Concrete)
        .and_then(|c| try_extract_matches!(c, ConcreteTypeId::Struct))
        .ok_or_else(|| ctx.diagnostics.report(&path, NotAStruct))?;

    let members = db.concrete_struct_members(concrete_struct)?;
    let mut member_exprs: OrderedHashMap<MemberId, ExprId> = OrderedHashMap::default();
    // A set of struct members for which a diagnostic has been reported.
    let mut skipped_members: UnorderedHashSet<MemberId> = UnorderedHashSet::default();
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
                resolve_variable_by_name(ctx, &arg_identifier, path.stable_ptr().into())?
            }
            ast::OptionStructArgExpr::StructArgExpr(arg_expr) => {
                compute_expr_semantic(ctx, &arg_expr.expr(syntax_db))
            }
        };

        // Check types.
        if ctx.inference.conform_ty(arg_expr.ty(), member.ty).is_err() {
            if !member.ty.is_missing(db) {
                ctx.diagnostics.report(
                    &arg_identifier,
                    WrongArgumentType { expected_ty: member.ty, actual_ty: arg_expr.ty() },
                );
            }
            skipped_members.insert(member.id);
            continue;
        }
        // Insert and check for duplicates.
        if member_exprs.insert(member.id, ctx.exprs.alloc(arg_expr)).is_some() {
            ctx.diagnostics.report(&arg_identifier, MemberSpecifiedMoreThanOnce);
        }
    }

    // Report errors for missing members.
    for (member_name, member) in members.iter() {
        if !member_exprs.contains_key(&member.id) && !skipped_members.contains(&member.id) {
            ctx.diagnostics.report(ctor_syntax, MissingMember { member_name: member_name.clone() });
        }
    }

    Ok(Expr::StructCtor(ExprStructCtor {
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
    try_extract_matches!(statement_expr.semicolon(syntax_db), ast::OptionTerminalSemicolon::Empty)?;
    Some(statement_expr.expr(syntax_db))
}

/// Creates the semantic model of a literal expression from its AST.
fn literal_to_semantic(
    ctx: &mut ComputationContext<'_>,
    literal_syntax: &ast::TerminalLiteralNumber,
) -> Maybe<ExprLiteral> {
    let db = ctx.db;
    let syntax_db = db.upcast();
    let text = literal_syntax.text(syntax_db);

    let (literal_text, ty) = if let Some((literal, ty)) = text.split_once('_') {
        (literal.into(), Some(ty))
    } else {
        (text.clone(), None)
    };
    let value = LiteralLongId::try_from(literal_text)
        .map_err(|_| ctx.diagnostics.report(literal_syntax, UnknownLiteral))?
        .value;

    let ty = if let Some(ty_str) = ty {
        try_get_core_ty_by_name(db, ty_str.into(), vec![])
            .map_err(|err| ctx.diagnostics.report(literal_syntax, err))?
    } else {
        db.core_felt_ty()
    };
    try_get_const_libfunc_name_by_type(db, ty)
        .map_err(|err| ctx.diagnostics.report(literal_syntax, err))?;
    Ok(ExprLiteral { value, ty, stable_ptr: literal_syntax.stable_ptr().into() })
}

/// Creates the semantic model of a short string from its AST.
fn short_string_to_semantic(
    ctx: &mut ComputationContext<'_>,
    short_string_syntax: &ast::TerminalShortString,
) -> Maybe<ExprLiteral> {
    let db = ctx.db;
    let syntax_db = db.upcast();
    let text = short_string_syntax.text(syntax_db);

    if let Some((literal, suffix)) = text[1..].rsplit_once('\'') {
        let ty = if !suffix.is_empty() {
            try_get_core_ty_by_name(db, suffix[1..].into(), vec![])
                .map_err(|err| ctx.diagnostics.report(short_string_syntax, err))?
        } else {
            db.core_felt_ty()
        };
        let unescaped_literal = unescape(literal).map_err(|err| {
            ctx.diagnostics.report(short_string_syntax, IllegalStringEscaping(format!("{}", err)))
        })?;
        if unescaped_literal.is_ascii() {
            Ok(ExprLiteral {
                value: BigInt::from_bytes_be(Sign::Plus, unescaped_literal.as_bytes()),
                ty,
                stable_ptr: short_string_syntax.stable_ptr().into(),
            })
        } else {
            Err(ctx.diagnostics.report(short_string_syntax, ShortStringMustBeAscii))
        }
    } else {
        unreachable!();
    }
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
    lexpr: Expr,
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
fn all_module_trait_ids(ctx: &mut ComputationContext<'_>) -> Maybe<Vec<TraitId>> {
    let mut module_traits = ctx.db.module_traits_ids(ctx.resolver.module_file_id.0)?;
    for use_id in ctx.db.module_uses_ids(ctx.resolver.module_file_id.0)? {
        if let Ok(ResolvedGenericItem::Trait(trait_id)) = ctx.db.use_resolved_item(use_id) {
            module_traits.push(trait_id);
        }
    }
    Ok(module_traits)
}

/// Computes the semantic model of a method call expression (e.g. "expr.method(..)").
/// Finds all traits with at least one candidate impl with a matching `self` param.
/// If more/less than 1 such trait exists, fails.
fn method_call_expr(
    ctx: &mut ComputationContext<'_>,
    lexpr: Expr,
    expr: ast::ExprFunctionCall,
    stable_ptr: ast::ExprPtr,
) -> Maybe<Expr> {
    // TODO(spapini): Add ctx.module_id.
    // TODO(spapini): Look also in uses.
    let syntax_db = ctx.db.upcast();
    let path = expr.path(syntax_db);
    let func_name = expr_as_identifier(ctx, &path, syntax_db)?;
    let mut candidates = vec![];
    for trait_id in all_module_trait_ids(ctx)? {
        for (name, trait_function) in ctx.db.trait_functions(trait_id)? {
            if name != func_name {
                continue;
            }

            // Check if trait function signature's first param can fit our expr type.
            let mut inference = ctx.inference.clone();
            let Some(concrete_trait_id) = inference.infer_concrete_trait_by_self(
                trait_function, lexpr.ty()
            ) else {continue};

            // Find impls for it.
            let lookup_context = ctx.resolver.impl_lookup_context(trait_id);
            let Ok(impls) = find_impls_at_context(
                ctx.db, &inference, &lookup_context, concrete_trait_id) else { continue; };

            if impls.is_empty() {
                continue;
            }

            candidates.push(trait_function);
        }
    }

    let trait_function = match candidates[..] {
        [] => {
            return Err(ctx.diagnostics.report_by_ptr(stable_ptr.untyped(), UnknownFunction));
        }
        [trait_function] => trait_function,
        [trait_function_id0, trait_function_id1, ..] => {
            return Err(ctx.diagnostics.report_by_ptr(
                stable_ptr.untyped(),
                AmbiguousTrait { trait_function_id0, trait_function_id1 },
            ));
        }
    };

    let concrete_trait_id =
        ctx.inference.infer_concrete_trait_by_self(trait_function, lexpr.ty()).unwrap();
    let signature = ctx.db.trait_function_signature(trait_function).unwrap();
    let first_param = signature.params.into_iter().next().unwrap();
    let concrete_trait_function_id = ctx.db.intern_concrete_trait_function(
        ConcreteTraitFunctionLongId::new(ctx.db, concrete_trait_id, trait_function),
    );

    let function_id = ctx.db.intern_function(FunctionLongId {
        function: ConcreteFunction {
            generic_function: GenericFunctionId::Trait(concrete_trait_function_id),
            generic_args: vec![],
        },
    });

    let named_args: Vec<_> = chain!(
        [(lexpr, None, first_param.mutability)],
        expr.arguments(syntax_db)
            .args(syntax_db)
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
    lexpr: Expr,
    rhs_syntax: ast::ExprPath,
    stable_ptr: ast::ExprPtr,
) -> Maybe<Expr> {
    let syntax_db = ctx.db.upcast();

    // Find MemberId.
    let member_name = expr_as_identifier(ctx, &rhs_syntax, syntax_db)?;
    match ctx.db.lookup_intern_type(lexpr.ty()) {
        TypeLongId::Concrete(concrete) => match concrete {
            ConcreteTypeId::Struct(concrete_struct_id) => {
                // TODO(lior): Add a diagnostic test when accessing a member of a missing type.
                let members = ctx.db.concrete_struct_members(concrete_struct_id)?;
                let member = members.get(&member_name).ok_or_else(|| {
                    ctx.diagnostics.report(
                        &rhs_syntax,
                        NoSuchMember {
                            struct_id: concrete_struct_id.struct_id(ctx.db),
                            member_name,
                        },
                    )
                })?;
                let lexpr_id = ctx.exprs.alloc(lexpr);
                Ok(Expr::MemberAccess(ExprMemberAccess {
                    expr: lexpr_id,
                    struct_id: concrete_struct_id.struct_id(ctx.db),
                    member: member.id,
                    ty: member.ty,
                    stable_ptr,
                }))
            }
            _ => Err(ctx
                .diagnostics
                .report(&rhs_syntax, TypeHasNoMembers { ty: lexpr.ty(), member_name })),
        },
        TypeLongId::Tuple(_) => {
            // TODO(spapini): Handle .0, .1, ...;
            Err(ctx.diagnostics.report(&rhs_syntax, Unsupported))
        }
        TypeLongId::GenericParameter(_) => Err(ctx
            .diagnostics
            .report(&rhs_syntax, TypeHasNoMembers { ty: lexpr.ty(), member_name })),
        TypeLongId::Var(_) => Err(ctx.diagnostics.report(&rhs_syntax, TypeYetUnknown)),
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
            return Ok(res);
        }
    }

    // Check if this is a constant.
    let resolved_item = ctx.resolver.resolve_concrete_path(
        ctx.diagnostics,
        &mut ctx.inference,
        path,
        NotFoundItemType::Identifier,
    )?;
    let ResolvedConcreteItem::Constant(constant_id) = resolved_item else {
        return Err(
            ctx.diagnostics.report(path, UnexpectedElement{
                expected:vec![ElementKind::Variable, ElementKind::Constant],
                actual: (&resolved_item).into(),
            })
        );
    };

    let ty = db.constant_semantic_data(constant_id)?.value.ty();
    Ok(Expr::Constant(ExprConstant { constant_id, ty, stable_ptr: path.stable_ptr().into() }))
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
    get_variable_by_name(ctx, &variable_name, stable_ptr)
        .ok_or_else(|| ctx.diagnostics.report(identifier, VariableNotFound { name: variable_name }))
}

/// Returns the requested variable from the environment if it exists. Returns None otherwise.
pub fn get_variable_by_name(
    ctx: &mut ComputationContext<'_>,
    variable_name: &SmolStr,
    stable_ptr: ast::ExprPtr,
) -> Option<Expr> {
    let mut maybe_env = Some(&*ctx.environment);
    while let Some(env) = maybe_env {
        if let Some(var) = env.variables.get(variable_name) {
            return Some(Expr::Var(ExprVar { var: var.id(), ty: var.ty(), stable_ptr }));
        }
        maybe_env = env.parent.as_deref();
    }
    None
}

/// Typechecks a function call.
fn expr_function_call(
    ctx: &mut ComputationContext<'_>,
    function_id: FunctionId,
    named_args: Vec<(Expr, Option<ast::TerminalIdentifier>, Mutability)>,
    stable_ptr: ast::ExprPtr,
) -> Maybe<Expr> {
    // TODO(spapini): Better location for these diagnostics after the refactor for generics resolve.
    // TODO(lior): Check whether concrete_function_signature should be `Option` instead of `Maybe`.
    let signature = ctx
        .db
        .concrete_function_signature(function_id)
        .map_err(|_| ctx.diagnostics.report_by_ptr(stable_ptr.untyped(), UnknownFunction))?;

    if named_args.len() != signature.params.len() {
        return Err(ctx.diagnostics.report_by_ptr(
            stable_ptr.untyped(),
            WrongNumberOfArguments { expected: signature.params.len(), actual: named_args.len() },
        ));
    }

    // Check panicable.
    if signature.panicable
        && !ctx
            .get_signature(
                stable_ptr.untyped(),
                UnsupportedOutsideOfFunctionFeatureName::FunctionCall,
            )?
            .panicable
    {
        return Err(ctx.diagnostics.report_by_ptr(stable_ptr.untyped(), PanicableFromNonPanicable));
    }

    // Check argument names and types.
    check_named_arguments(&named_args, &signature, ctx)?;

    let mut ref_args = Vec::new();
    let mut args = Vec::new();
    for ((arg, _name, mutability), param) in named_args.into_iter().zip(signature.params.iter()) {
        let arg_typ = arg.ty();
        let param_typ = param.ty;
        // Don't add diagnostic if the type is missing (a diagnostic should have already been
        // added).
        // TODO(lior): Add a test to missing type once possible.
        if !arg_typ.is_missing(ctx.db) && ctx.inference.conform_ty(arg_typ, param_typ).is_err() {
            ctx.diagnostics.report_by_ptr(
                arg.stable_ptr().untyped(),
                WrongArgumentType { expected_ty: param_typ, actual_ty: arg_typ },
            );
        }

        if param.mutability == Mutability::Reference {
            // Verify the argument is a variable.
            let expr_var = try_extract_matches!(&arg, Expr::Var).ok_or_else(|| {
                ctx.diagnostics.report_by_ptr(arg.stable_ptr().untyped(), RefArgNotAVariable)
            })?;
            // Verify the variable argument is mutable.
            if !ctx.semantic_defs[expr_var.var].is_mut() {
                ctx.diagnostics.report_by_ptr(arg.stable_ptr().untyped(), RefArgNotMutable);
            }
            // Verify that it is passed explicitly as 'ref'.
            if mutability != Mutability::Reference {
                ctx.diagnostics.report_by_ptr(arg.stable_ptr().untyped(), RefArgNotExplicit);
            }
            ref_args.push(expr_var.var);
        } else {
            // Verify that it is passed explicitly as 'ref'.
            if mutability != Mutability::Immutable {
                ctx.diagnostics
                    .report_by_ptr(arg.stable_ptr().untyped(), ImmutableArgWithModifiers);
            }
            args.push(ctx.exprs.alloc(arg));
        }
    }
    Ok(Expr::FunctionCall(ExprFunctionCall {
        function: function_id,
        ref_args,
        args,
        ty: signature.return_type,
        stable_ptr,
    }))
}

/// Checks the correctness of the named arguments, and outputs diagnostics on errors.
fn check_named_arguments(
    named_args: &[(Expr, Option<ast::TerminalIdentifier>, Mutability)],
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
    for ((arg, name_opt, _mutability), param) in named_args.iter().zip(signature.params.iter()) {
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

/// Computes the semantic model of a statement.
pub fn compute_statement_semantic(
    ctx: &mut ComputationContext<'_>,
    syntax: ast::Statement,
) -> Maybe<StatementId> {
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
                    let explicit_type = resolve_type_with_inference(
                        db,
                        ctx.diagnostics,
                        &mut ctx.inference,
                        &mut ctx.resolver,
                        &var_type_path,
                    );
                    if !inferred_type.is_missing(db)
                        && ctx.inference.conform_ty(explicit_type, inferred_type).is_err()
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

            let pattern = compute_pattern_semantic(ctx, let_syntax.pattern(syntax_db), ty)?;
            let variables = pattern.variables();
            // TODO(yuval): allow unnamed variables. Add them here to
            // ctx.environment.unnamed_variables
            for v in variables {
                let var_def = Variable::Local(v.var.clone());
                ctx.environment.variables.insert(v.name.clone(), var_def.clone());
                ctx.semantic_defs.insert(var_def.id(), var_def);
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
            let expr_ty = expr.ty();
            let expected_ty = ctx
                .get_signature(
                    return_syntax.stable_ptr().untyped(),
                    UnsupportedOutsideOfFunctionFeatureName::ReturnStatement,
                )?
                .return_type;
            if expr_ty != expected_ty && !expected_ty.is_missing(db) && !expr_ty.is_missing(db) {
                ctx.diagnostics
                    .report(&expr_syntax, WrongReturnType { expected_ty, actual_ty: expr_ty });
            }
            semantic::Statement::Return(semantic::StatementReturn {
                expr: ctx.exprs.alloc(expr),
                stable_ptr: syntax.stable_ptr(),
            })
        }
        ast::Statement::Missing(_) => todo!(),
    };
    Ok(ctx.statements.alloc(statement))
}
