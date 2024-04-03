//! This module is responsible of computing the semantic model of expressions and statements in
//! the code, while type checking.
//! It is invoked by queries for function bodies and other code blocks.

use std::ops::Deref;
use std::sync::Arc;

use ast::PathSegment;
use cairo_lang_defs::db::validate_attributes_flat;
use cairo_lang_defs::ids::{
    EnumId, FunctionTitleId, FunctionWithBodyId, GenericKind, LanguageElementId, LocalVarLongId,
    LookupItemId, MemberId, ModuleId, TraitFunctionId, TraitId,
};
use cairo_lang_diagnostics::{DiagnosticAdded, Maybe, ToOption};
use cairo_lang_filesystem::ids::{FileKind, FileLongId, VirtualFile};
use cairo_lang_syntax::attribute::consts::FEATURE_ATTR;
use cairo_lang_syntax::attribute::structured::{
    Attribute, AttributeArg, AttributeArgVariant, AttributeStructurize,
};
use cairo_lang_syntax::node::ast::{
    BlockOrIf, ExprPtr, PatternListOr, PatternStructParam, UnaryOperator,
};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::{GetIdentifier, PathSegmentEx, QueryAttrs};
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_syntax::node::{ast, Terminal, TypedStablePtr, TypedSyntaxNode};
use cairo_lang_utils as utils;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use cairo_lang_utils::unordered_hash_set::UnorderedHashSet;
use cairo_lang_utils::{extract_matches, try_extract_matches, OptionHelper};
use id_arena::Arena;
use itertools::{chain, zip_eq};
use num_bigint::BigInt;
use num_traits::ToPrimitive;
use smol_str::SmolStr;

use super::inference::canonic::ResultNoErrEx;
use super::inference::conform::InferenceConform;
use super::inference::infers::InferenceEmbeddings;
use super::inference::{Inference, InferenceError};
use super::objects::*;
use super::pattern::{
    Pattern, PatternEnumVariant, PatternFixedSizeArray, PatternLiteral, PatternMissing,
    PatternOtherwise, PatternTuple, PatternVariable,
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
use crate::items::constant::ConstValue;
use crate::items::enm::SemanticEnumEx;
use crate::items::imp::{filter_candidate_traits, infer_impl_by_self};
use crate::items::modifiers::compute_mutability;
use crate::items::structure::SemanticStructEx;
use crate::items::visibility;
use crate::literals::try_extract_minus_literal;
use crate::resolve::{ResolvedConcreteItem, ResolvedGenericItem, Resolver};
use crate::semantic::{self, FunctionId, LocalVariable, TypeId, TypeLongId, Variable};
use crate::substitution::SemanticRewriter;
use crate::types::{
    are_coupons_enabled, extract_fixed_size_array_size, peel_snapshots, resolve_type,
    verify_fixed_size_array_size, wrap_in_snapshots, ConcreteTypeId,
};
use crate::{
    ConcreteEnumId, GenericArgumentId, GenericParam, Member, Mutability, Parameter,
    PatternStringLiteral, PatternStruct, Signature,
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

/// Context inside loops.
#[derive(Debug, Clone)]
enum LoopContext {
    /// Context inside a `loop`
    Loop(FlowMergeTypeHelper),
    /// Context inside a `while` loop
    While,
}

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
    loop_ctx: Option<LoopContext>,
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
            loop_ctx: None,
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
        let new_environment = Box::new(Environment::empty());
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
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Environment {
    parent: Option<Box<Environment>>,
    variables: EnvVariables,
    used_variables: UnorderedHashSet<semantic::VarId>,
    allowed_features: UnorderedHashSet<SmolStr>,
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
        if let utils::ordered_hash_map::Entry::Vacant(entry) =
            self.variables.entry(semantic_param.name.clone())
        {
            entry.insert(Variable::Param(semantic_param));
            Ok(())
        } else {
            Err(diagnostics.report(
                ast_param,
                ParamNameRedefinition { function_title_id, param_name: semantic_param.name },
            ))
        }
    }

    pub fn empty() -> Self {
        Self {
            parent: None,
            variables: Default::default(),
            used_variables: Default::default(),
            allowed_features: Default::default(),
        }
    }

    pub fn from_lookup_item_id(
        db: &dyn SemanticGroup,
        lookup_item_id: LookupItemId,
        diagnostics: &mut SemanticDiagnostics,
    ) -> Self {
        let defs_db = db.upcast();
        let semantic_db = db.upcast();
        let allowed_features = match lookup_item_id {
            LookupItemId::ModuleItem(id) => extract_allowed_features(
                semantic_db,
                &id.stable_location(defs_db).syntax_node(defs_db),
                diagnostics,
            ),
            LookupItemId::TraitItem(id) => extract_allowed_features(
                semantic_db,
                &id.stable_location(defs_db).syntax_node(defs_db),
                diagnostics,
            ),
            LookupItemId::ImplItem(id) => extract_allowed_features(
                semantic_db,
                &id.stable_location(defs_db).syntax_node(defs_db),
                diagnostics,
            ),
        };

        Self::from_element_id(db, lookup_item_id, allowed_features.into_iter().collect())
    }

    fn from_element_id(
        db: &dyn SemanticGroup,
        element_id: impl LanguageElementId,
        mut allowed_features: UnorderedHashSet<SmolStr>,
    ) -> Environment {
        let defs_db = db.upcast();
        let syntax_db = db.upcast();
        let ignored_diagnostics = &mut SemanticDiagnostics::new(
            element_id.module_file_id(defs_db).file_id(defs_db).unwrap(),
        );
        let mut curr_module_id = element_id.parent_module(defs_db);
        loop {
            let submodule_id = match curr_module_id {
                ModuleId::CrateRoot(_) => break,
                ModuleId::Submodule(id) => id,
            };
            let parent = submodule_id.parent_module(defs_db);
            let module = &defs_db.module_submodules(parent).unwrap()[&submodule_id];
            // TODO(orizi): Add parent module diagnostics.
            for allowed_feature in extract_allowed_features(syntax_db, module, ignored_diagnostics)
            {
                allowed_features.insert(allowed_feature);
            }
            curr_module_id = parent;
        }
        Self {
            parent: None,
            variables: Default::default(),
            used_variables: Default::default(),
            allowed_features,
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
    if let TypeLongId::Concrete(concrete) = ctx.db.lookup_intern_type(expr.ty()) {
        if let Ok(Some(attr)) = concrete.unstable_attr(ctx.db.upcast()) {
            validate_unstable_feature_usage(ctx, attr, syntax.stable_ptr());
        }
    }
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
        ast::Expr::While(expr_while) => compute_expr_while_semantic(ctx, expr_while),
        ast::Expr::ErrorPropagate(expr) => compute_expr_error_propagate_semantic(ctx, expr),
        ast::Expr::InlineMacro(expr) => compute_expr_inline_macro_semantic(ctx, expr),
        ast::Expr::Missing(_) | ast::Expr::FieldInitShorthand(_) => {
            Err(ctx.diagnostics.report(syntax, Unsupported))
        }
        ast::Expr::Indexed(expr) => compute_expr_indexed_semantic(ctx, expr),
        ast::Expr::FixedSizeArray(expr) => compute_expr_fixed_size_array_semantic(ctx, expr),
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
                let inference = &mut ctx.resolver.inference();
                let desnapped_var = inference.new_type_var(Some(syntax.stable_ptr().untyped()));
                let snapped_desnapped_var = ctx.db.intern_type(TypeLongId::Snapshot(desnapped_var));
                if let Err(err_set) = inference.conform_ty(snapped_desnapped_var, expr_ty) {
                    let diag_added = ctx.diagnostics.report(
                        syntax,
                        WrongArgumentType {
                            expected_ty: snapped_desnapped_var,
                            actual_ty: expr_ty,
                        },
                    );
                    inference.consume_reported_error(err_set, diag_added);
                    return Err(diag_added);
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
    let inference = &mut ctx.resolver.inference();
    let function = inference
        .infer_trait_function(
            concrete_trait_function,
            &impl_lookup_context,
            Some(syntax.stable_ptr().untyped()),
        )
        .map_err(|err_set| {
            inference.report_on_pending_error(
                err_set,
                ctx.diagnostics,
                syntax.stable_ptr().untyped(),
            )
        })?;

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

            let inference = &mut ctx.resolver.inference();
            if let Err(err_set) = inference.conform_ty(actual_ty, expected_ty) {
                let diag_added = ctx
                    .diagnostics
                    .report(&rhs_syntax, WrongArgumentType { expected_ty, actual_ty });
                inference.consume_reported_error(err_set, diag_added);
                return Err(diag_added);
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

            let inference = &mut ctx.resolver.inference();
            let bool_ty = core_bool_ty(db);
            if let Err(err_set) = inference.conform_ty(lexpr.expr.ty(), bool_ty) {
                let diag_added = ctx.diagnostics.report(
                    lhs_syntax,
                    WrongType { expected_ty: bool_ty, actual_ty: lexpr.expr.ty() },
                );
                inference.consume_reported_error(err_set, diag_added);
            }

            if let Err(err_set) = inference.conform_ty(rexpr.expr.ty(), bool_ty) {
                let diag_added = ctx.diagnostics.report(
                    &rhs_syntax,
                    WrongType { expected_ty: bool_ty, actual_ty: rexpr.expr.ty() },
                );
                inference.consume_reported_error(err_set, diag_added);
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
    let inference = &mut ctx.resolver.inference();
    let function = inference
        .infer_trait_function(
            concrete_trait_function,
            &impl_lookup_context,
            Some(syntax.stable_ptr().untyped()),
        )
        .map_err(|err_set| {
            inference.report_on_pending_error(
                err_set,
                ctx.diagnostics,
                syntax.stable_ptr().untyped(),
            )
        })?;

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
/// Computes the semantic model of an expression of type [ast::ExprFixedSizeArray].
fn compute_expr_fixed_size_array_semantic(
    ctx: &mut ComputationContext<'_>,
    syntax: &ast::ExprFixedSizeArray,
) -> Maybe<Expr> {
    let db = ctx.db;
    let syntax_db = db.upcast();
    let exprs = syntax.exprs(syntax_db).elements(syntax_db);
    let (first_expr, tail_exprs) = exprs
        .split_first()
        .ok_or_else(|| ctx.diagnostics.report(syntax, FixedSizeArrayEmptyElements))?;
    let first_expr_semantic = compute_expr_semantic(ctx, first_expr);

    let items = if let Some(size_const_id) =
        extract_fixed_size_array_size(db, ctx.diagnostics, syntax, &ctx.resolver)?
    {
        // Fixed size array with a defined size must have exactly one element.
        if !tail_exprs.is_empty() {
            return Err(ctx.diagnostics.report(syntax, FixedSizeArrayNonSingleValue));
        }
        let size =
            try_extract_matches!(db.lookup_intern_const_value(size_const_id), ConstValue::Int)
                .ok_or_else(|| ctx.diagnostics.report(syntax, FixedSizeArrayNonNumericSize))?
                .to_usize()
                .unwrap();
        verify_fixed_size_array_size(ctx.diagnostics, &size.into(), syntax)?;
        FixedSizeArrayItems::ValueAndSize(first_expr_semantic.id, size_const_id)
    } else {
        let mut items: Vec<ExprId> = vec![];
        items.push(first_expr_semantic.id);
        // The type of the first expression is the type of the array. All other expressions must
        // have the same type.
        let first_expr_ty = ctx.reduce_ty(first_expr_semantic.ty());
        for expr_syntax in tail_exprs {
            let expr_semantic = compute_expr_semantic(ctx, expr_syntax);
            let expr_ty = ctx.reduce_ty(expr_semantic.ty());
            let inference = &mut ctx.resolver.inference();
            if let Err(err_set) = inference.conform_ty(expr_ty, first_expr_ty) {
                let diag_added = ctx.diagnostics.report(
                    expr_syntax,
                    WrongArgumentType { expected_ty: first_expr_ty, actual_ty: expr_ty },
                );
                inference.consume_reported_error(err_set, diag_added);
                return Err(diag_added);
            }
            items.push(expr_semantic.id);
        }
        FixedSizeArrayItems::Items(items)
    };
    let size = match &items {
        FixedSizeArrayItems::ValueAndSize(_, size_const_id) => *size_const_id,
        FixedSizeArrayItems::Items(items) => {
            let size = BigInt::from(items.len());
            verify_fixed_size_array_size(ctx.diagnostics, &size, syntax)?;
            db.intern_const_value(ConstValue::Int(size))
        }
    };
    Ok(Expr::FixedSizeArray(ExprFixedSizeArray {
        items,
        ty: db.intern_type(TypeLongId::FixedSizeArray {
            type_id: ctx.reduce_ty(first_expr_semantic.ty()),
            size,
        }),
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
            let inference = &mut ctx.resolver.inference();
            if let Err(err_set) = inference.conform_ty(actual_ty, expected_ty) {
                let diag_added = ctx
                    .diagnostics
                    .report(&args_syntax, WrongArgumentType { expected_ty, actual_ty });
                inference.consume_reported_error(err_set, diag_added);
                return Err(diag_added);
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
    let res_ty = ctx.reduce_ty(res.ty());
    let res = ctx.exprs.alloc(res);
    let return_type = ctx.reduce_ty(return_type);
    let inference = &mut ctx.resolver.inference();
    if let Err(err_set) = inference.conform_ty(res_ty, return_type) {
        let diag_added = ctx
            .diagnostics
            .report(syntax, WrongReturnType { expected_ty: return_type, actual_ty: res_ty });
        inference.consume_reported_error(err_set, diag_added);
    }

    // Check fully resolved.
    inference.finalize(ctx.diagnostics, syntax.stable_ptr().untyped());

    // Apply inference.
    infer_all(ctx).ok();

    Ok(res)
}

fn infer_all(ctx: &mut ComputationContext<'_>) -> Maybe<()> {
    for (_id, expr) in ctx.exprs.iter_mut() {
        ctx.resolver.inference().internal_rewrite(expr).no_err();
    }
    for (_id, pattern) in ctx.patterns.iter_mut() {
        ctx.resolver.inference().internal_rewrite(pattern).no_err();
    }
    for (_id, stmt) in ctx.statements.iter_mut() {
        ctx.resolver.inference().internal_rewrite(stmt).no_err();
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
#[derive(Debug, Clone)]
struct FlowMergeTypeHelper {
    never_type: TypeId,
    final_type: Option<TypeId>,
}
impl FlowMergeTypeHelper {
    fn new(db: &dyn SemanticGroup) -> Self {
        Self { never_type: never_ty(db), final_type: None }
    }

    /// Attempt merge a branch into the helper, on error will return the conflicting types.
    fn try_merge_types<F>(
        &mut self,
        inference: &mut Inference<'_>,
        db: &dyn SemanticGroup,
        ty: TypeId,
        mut report_type_mismatch: F,
    ) -> bool
    where
        F: FnMut(TypeId, TypeId) -> DiagnosticAdded,
    {
        if ty != self.never_type && !ty.is_missing(db) {
            if let Some(existing) = &self.final_type {
                if let Err(err_set) = inference.conform_ty(ty, *existing) {
                    let diag_added = report_type_mismatch(*existing, ty);
                    inference.consume_reported_error(err_set, diag_added);
                    return false;
                }
            } else {
                self.final_type = Some(ty);
            }
        }
        true
    }

    /// Returns the merged type.
    fn get_final_type(self) -> TypeId {
        self.final_type.unwrap_or(self.never_type)
    }
}

/// computes the semnatic of a match arm pattern and the block expression.
fn compute_arm_semantic(
    ctx: &mut ComputationContext<'_>,
    expr: &Expr,
    arm_expr_syntax: ast::Expr,
    patterns_syntax: &PatternListOr,
    is_loop_arm: bool,
) -> (Vec<PatternAndId>, ExprAndId) {
    let db = ctx.db;
    let syntax_db = db.upcast();
    ctx.run_in_subscope(|new_ctx| {
        // Typecheck the arms's patterns, and introduce the new variables to the subscope.
        // Note that if the arm expr is a block, there will be *another* subscope
        // for it.
        let mut arm_patterns_variables: UnorderedHashMap<SmolStr, LocalVariable> =
            UnorderedHashMap::default();
        let patterns: Vec<_> = patterns_syntax
            .elements(syntax_db)
            .iter()
            .map(|pattern_syntax| {
                let pattern: PatternAndId = compute_pattern_semantic(
                    new_ctx,
                    pattern_syntax,
                    expr.ty(),
                    &mut arm_patterns_variables,
                );
                let variables = pattern.variables(&new_ctx.patterns);
                for variable in variables {
                    match arm_patterns_variables.entry(variable.name.clone()) {
                        std::collections::hash_map::Entry::Occupied(entry) => {
                            let get_location = || variable.stable_ptr.lookup(db.upcast());
                            let var = entry.get();

                            let expected_ty = new_ctx.reduce_ty(var.ty);
                            let actual_ty = new_ctx.reduce_ty(variable.var.ty);

                            let mut has_inference_error = false;
                            if !variable.var.ty.is_missing(new_ctx.db) {
                                let inference = &mut new_ctx.resolver.inference();
                                if let Err(err_set) = inference.conform_ty(actual_ty, expected_ty) {
                                    let diag_added = new_ctx.diagnostics.report(
                                        &get_location(),
                                        WrongType { expected_ty, actual_ty: variable.var.ty },
                                    );
                                    inference.consume_reported_error(err_set, diag_added);
                                    has_inference_error = true;
                                }
                            };
                            if !has_inference_error && var.is_mut != variable.var.is_mut {
                                new_ctx.diagnostics.report(&get_location(), InconsistentBinding);
                            }
                        }
                        std::collections::hash_map::Entry::Vacant(entry) => {
                            entry.insert(variable.var.clone());
                        }
                    }
                }
                pattern
            })
            .collect();

        for (pattern_syntax, pattern) in
            patterns_syntax.elements(syntax_db).iter().zip(patterns.iter())
        {
            let variables = pattern.variables(&new_ctx.patterns);

            if variables.len() != arm_patterns_variables.len() {
                new_ctx.diagnostics.report(pattern_syntax, MissingVariableInPattern);
            }

            for v in variables {
                let var_def = Variable::Local(v.var.clone());
                // TODO(spapini): Wrap this in a function to couple with semantic_defs
                // insertion.
                new_ctx.environment.variables.insert(v.name.clone(), var_def.clone());
                new_ctx.semantic_defs.insert(var_def.id(), var_def);
            }
        }
        let arm_expr = if is_loop_arm {
            let ast::Expr::Block(arm_expr_syntax) = arm_expr_syntax else {
                unreachable!("Expected a block expression for a loop arm.");
            };
            let (id, _) = compute_loop_body_semantic(new_ctx, arm_expr_syntax, LoopContext::While);
            let expr = new_ctx.exprs[id].clone();
            ExprAndId { expr, id }
        } else {
            compute_expr_semantic(new_ctx, &arm_expr_syntax)
        };
        (patterns, arm_expr)
    })
}

/// Computes the semantic model of an expression of type [ast::ExprMatch].
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
    let patterns_and_exprs: Vec<_> = syntax_arms
        .iter()
        .map(|syntax_arm| {
            compute_arm_semantic(
                ctx,
                &expr,
                syntax_arm.expression(syntax_db),
                &syntax_arm.patterns(syntax_db),
                false,
            )
        })
        .collect();
    // Unify arm types.
    let mut helper = FlowMergeTypeHelper::new(ctx.db);
    for (_, expr) in patterns_and_exprs.iter() {
        let expr_ty = ctx.reduce_ty(expr.ty());
        helper.try_merge_types(
            &mut ctx.resolver.inference(),
            ctx.db,
            expr_ty,
            |match_ty, arm_ty| {
                ctx.diagnostics.report_by_ptr(
                    expr.stable_ptr().untyped(),
                    IncompatibleMatchArms { match_ty, arm_ty },
                )
            },
        );
    }
    // Compute semantic representation of the match arms.
    let semantic_arms = patterns_and_exprs
        .into_iter()
        .map(|(patterns, arm_expr)| MatchArm {
            patterns: patterns.iter().map(|pattern| pattern.id).collect(),
            expression: arm_expr.id,
        })
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
    let (condition, if_block) = match &syntax.condition(syntax_db) {
        ast::Condition::Let(condition) => {
            let expr = compute_expr_semantic(ctx, &condition.expr(syntax_db));
            if let Expr::LogicalOperator(_) = expr.expr {
                ctx.diagnostics
                    .report(&condition.expr(syntax_db), LogicalOperatorNotAllowedInIfLet);
            }

            let (patterns, if_block) = compute_arm_semantic(
                ctx,
                &expr,
                ast::Expr::Block(syntax.if_block(syntax_db)),
                &condition.patterns(syntax_db),
                false,
            );
            (Condition::Let(expr.id, patterns.iter().map(|pattern| pattern.id).collect()), if_block)
        }
        ast::Condition::Expr(expr) => {
            let if_block = compute_expr_block_semantic(ctx, &syntax.if_block(syntax_db))?;
            (
                Condition::BoolExpr(compute_bool_condition_semantic(ctx, &expr.expr(syntax_db)).id),
                ExprAndId { expr: if_block.clone(), id: ctx.exprs.alloc(if_block) },
            )
        }
    };

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
    let if_block_ty = ctx.reduce_ty(if_block.ty());
    let else_block_ty = ctx.reduce_ty(else_block_ty);
    let mut report_type_mismatch = |block_if_ty, block_else_ty| {
        ctx.diagnostics.report(syntax, IncompatibleIfBlockTypes { block_if_ty, block_else_ty })
    };
    let inference = &mut ctx.resolver.inference();
    let _ = helper.try_merge_types(inference, ctx.db, if_block_ty, &mut report_type_mismatch)
        && helper.try_merge_types(inference, ctx.db, else_block_ty, &mut report_type_mismatch);
    Ok(Expr::If(ExprIf {
        condition,
        if_block: if_block.id,
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

    let (body, loop_ctx) = compute_loop_body_semantic(
        ctx,
        syntax.body(syntax_db),
        LoopContext::Loop(FlowMergeTypeHelper::new(db)),
    );
    Ok(Expr::Loop(ExprLoop {
        body,
        ty: extract_matches!(loop_ctx, LoopContext::Loop).get_final_type(),
        stable_ptr: syntax.stable_ptr().into(),
    }))
}

/// Computes the semantic model of an expression of type [ast::ExprWhile].
fn compute_expr_while_semantic(
    ctx: &mut ComputationContext<'_>,
    syntax: &ast::ExprWhile,
) -> Maybe<Expr> {
    let db = ctx.db;
    let syntax_db = db.upcast();

    let (condition, body) = match &syntax.condition(syntax_db) {
        ast::Condition::Let(condition) => {
            let expr = compute_expr_semantic(ctx, &condition.expr(syntax_db));
            if let Expr::LogicalOperator(_) = expr.expr {
                ctx.diagnostics
                    .report(&condition.expr(syntax_db), LogicalOperatorNotAllowedInWhileLet);
            }

            let (patterns, body) = compute_arm_semantic(
                ctx,
                &expr,
                ast::Expr::Block(syntax.body(syntax_db)),
                &condition.patterns(syntax_db),
                true,
            );
            (Condition::Let(expr.id, patterns.iter().map(|pattern| pattern.id).collect()), body.id)
        }
        ast::Condition::Expr(expr) => {
            let (body, _loop_ctx) =
                compute_loop_body_semantic(ctx, syntax.body(syntax_db), LoopContext::While);
            (
                Condition::BoolExpr(compute_bool_condition_semantic(ctx, &expr.expr(syntax_db)).id),
                body,
            )
        }
    };

    Ok(Expr::While(ExprWhile {
        condition,
        body,
        ty: unit_ty(ctx.db),
        stable_ptr: syntax.stable_ptr().into(),
    }))
}

/// Computes the semantic model for a body of a loop.
fn compute_loop_body_semantic(
    ctx: &mut ComputationContext<'_>,
    syntax: ast::ExprBlock,
    loop_ctx: LoopContext,
) -> (ExprId, LoopContext) {
    let db = ctx.db;
    let syntax_db = db.upcast();

    ctx.run_in_subscope(|new_ctx| {
        let old_loop_ctx = std::mem::replace(&mut new_ctx.loop_ctx, Some(loop_ctx));

        let mut statements = syntax.statements(syntax_db).elements(syntax_db);
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

        let loop_ctx = std::mem::replace(&mut new_ctx.loop_ctx, old_loop_ctx).unwrap();
        let body = new_ctx.exprs.alloc(Expr::Block(ExprBlock {
            statements: statements_semantic,
            tail: tail.map(|tail| tail.id),
            ty: unit_ty(db),
            stable_ptr: syntax.stable_ptr().into(),
        }));

        (body, loop_ctx)
    })
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
    if ctx.loop_ctx.is_some() {
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
    let err_variant_ty = match conformed_err_variant_ty {
        Ok(ty) => ty,
        Err(err_set) => {
            ctx.resolver.inference().consume_error_without_reporting(err_set);
            err_variant.ty
        }
    };
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
    let self_ty = ctx.reduce_ty(self_expr.ty());
    // Inference errors found when looking for candidates. Only relevant in the case of 0 candidates
    // found. If >0 candidates are found these are ignored as they may describe, e.g., "errors"
    // indicating certain traits/impls/functions don't match, which is OK as we only look for one.
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
    or_pattern_variables_map: &mut UnorderedHashMap<SmolStr, LocalVariable>,
) -> PatternAndId {
    let pat = maybe_compute_pattern_semantic(ctx, syntax, ty, or_pattern_variables_map);
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
    or_pattern_variables_map: &mut UnorderedHashMap<SmolStr, LocalVariable>,
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
            let path = enum_pattern.path(syntax_db);
            let item = ctx.resolver.resolve_generic_path(
                ctx.diagnostics,
                &path,
                NotFoundItemType::Identifier,
            )?;
            let generic_variant = try_extract_matches!(item, ResolvedGenericItem::Variant)
                .ok_or_else(|| ctx.diagnostics.report(&path, NotAVariant))?;

            let (concrete_enum, n_snapshots) = extract_concrete_enum_from_pattern_and_validate(
                ctx,
                pattern_syntax,
                ty,
                generic_variant.enum_id,
            )?;

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
                    let pattern = compute_pattern_semantic(
                        ctx,
                        &p.pattern(syntax_db),
                        inner_ty,
                        or_pattern_variables_map,
                    );
                    Some(pattern.id)
                }
            };

            Pattern::EnumVariant(PatternEnumVariant {
                variant: concrete_variant,
                inner_pattern,
                ty,
                stable_ptr: enum_pattern.stable_ptr().into(),
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
            create_variable_pattern(
                ctx,
                identifier,
                &[],
                ty,
                path.stable_ptr().into(),
                or_pattern_variables_map,
            )
        }
        ast::Pattern::Identifier(identifier) => create_variable_pattern(
            ctx,
            identifier.name(syntax_db),
            &identifier.modifiers(syntax_db).elements(syntax_db),
            ty,
            identifier.stable_ptr().into(),
            or_pattern_variables_map,
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
            let inference = &mut ctx.resolver.inference();
            inference
                .conform_ty(pattern_ty, ctx.db.intern_type(peel_snapshots(ctx.db, ty).1))
                .map_err(|err_set| {
                    inference.report_on_pending_error(err_set, ctx.diagnostics, stable_ptr)
                })?;
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
            let mut used_members = UnorderedHashSet::<_>::default();
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
                            or_pattern_variables_map,
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
                        let pattern = compute_pattern_semantic(
                            ctx,
                            &with_expr.pattern(syntax_db),
                            ty,
                            or_pattern_variables_map,
                        );
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
        ast::Pattern::Tuple(_) => maybe_compute_tuple_like_pattern_semantic(
            ctx,
            pattern_syntax,
            ty,
            or_pattern_variables_map,
            |ty: TypeId| UnexpectedTuplePattern { ty },
            |expected, actual| WrongNumberOfTupleElements { expected, actual },
        )?,
        ast::Pattern::FixedSizeArray(_) => maybe_compute_tuple_like_pattern_semantic(
            ctx,
            pattern_syntax,
            ty,
            or_pattern_variables_map,
            |ty: TypeId| UnexpectedFixedSizeArrayPattern { ty },
            |expected, actual| WrongNumberOfFixedSizeArrayElements { expected, actual },
        )?,
        ast::Pattern::False(pattern_false) => {
            let enum_expr = extract_matches!(
                false_literal_expr(ctx, pattern_false.stable_ptr().into()),
                Expr::EnumVariantCtor
            );

            extract_concrete_enum_from_pattern_and_validate(
                ctx,
                pattern_syntax,
                ty,
                enum_expr.variant.concrete_enum_id.enum_id(ctx.db),
            )?;

            Pattern::EnumVariant(PatternEnumVariant {
                variant: enum_expr.variant,
                stable_ptr: pattern_false.stable_ptr().into(),
                ty,
                inner_pattern: None,
            })
        }
        ast::Pattern::True(pattern_true) => {
            let enum_expr = extract_matches!(
                true_literal_expr(ctx, pattern_true.stable_ptr().into()),
                Expr::EnumVariantCtor
            );
            extract_concrete_enum_from_pattern_and_validate(
                ctx,
                pattern_syntax,
                ty,
                enum_expr.variant.concrete_enum_id.enum_id(ctx.db),
            )?;

            Pattern::EnumVariant(PatternEnumVariant {
                variant: enum_expr.variant,
                stable_ptr: pattern_true.stable_ptr().into(),
                ty,
                inner_pattern: None,
            })
        }
    };
    let inference = &mut ctx.resolver.inference();
    inference.conform_ty(pattern.ty(), ty).map_err(|err_set| {
        inference.report_on_pending_error(err_set, ctx.diagnostics, stable_ptr)
    })?;
    Ok(pattern)
}

/// Computes the semantic model of a pattern of a tuple or a fixed size array. Assumes that the
/// pattern is one of these types.
fn maybe_compute_tuple_like_pattern_semantic(
    ctx: &mut ComputationContext<'_>,
    pattern_syntax: &ast::Pattern,
    ty: TypeId,
    or_pattern_variables_map: &mut UnorderedHashMap<SmolStr, LocalVariable>,
    unexpected_pattern: fn(TypeId) -> SemanticDiagnosticKind,
    wrong_number_of_elements: fn(usize, usize) -> SemanticDiagnosticKind,
) -> Maybe<Pattern> {
    let (n_snapshots, long_ty) = peel_snapshots(ctx.db, ty);
    // Assert that the pattern is of the same type as the expr.
    match (pattern_syntax, &long_ty) {
        (ast::Pattern::Tuple(_), TypeLongId::Tuple(_))
        | (ast::Pattern::FixedSizeArray(_), TypeLongId::FixedSizeArray { .. }) => {}
        _ => {
            return Err(ctx.diagnostics.report(pattern_syntax, unexpected_pattern(ty)));
        }
    };
    let inner_tys = match long_ty {
        TypeLongId::Tuple(inner_tys) => inner_tys,
        TypeLongId::FixedSizeArray { type_id: inner_ty, size } => {
            let size = extract_matches!(ctx.db.lookup_intern_const_value(size), ConstValue::Int)
                .to_usize()
                .unwrap();
            [inner_ty].repeat(size)
        }
        _ => unreachable!(),
    };
    let patterns_syntax = match pattern_syntax {
        ast::Pattern::Tuple(pattern_tuple) => {
            pattern_tuple.patterns(ctx.db.upcast()).elements(ctx.db.upcast())
        }
        ast::Pattern::FixedSizeArray(pattern_fixed_size_array) => {
            pattern_fixed_size_array.patterns(ctx.db.upcast()).elements(ctx.db.upcast())
        }
        _ => unreachable!(),
    };
    let size = inner_tys.len();
    if size != patterns_syntax.len() {
        return Err(ctx
            .diagnostics
            .report(pattern_syntax, wrong_number_of_elements(size, patterns_syntax.len())));
    }
    let pattern_options = zip_eq(patterns_syntax, inner_tys).map(|(pattern_ast, ty)| {
        let ty = wrap_in_snapshots(ctx.db, ty, n_snapshots);
        let pattern = compute_pattern_semantic(ctx, &pattern_ast, ty, or_pattern_variables_map);
        Ok(pattern.id)
    });
    // If all are Some, collect into a Vec.
    let field_patterns: Vec<_> = pattern_options.collect::<Maybe<_>>()?;
    Ok(match pattern_syntax {
        ast::Pattern::Tuple(syntax) => {
            Pattern::Tuple(PatternTuple { field_patterns, ty, stable_ptr: syntax.stable_ptr() })
        }
        ast::Pattern::FixedSizeArray(syntax) => Pattern::FixedSizeArray(PatternFixedSizeArray {
            elements_patterns: field_patterns,
            ty,
            stable_ptr: syntax.stable_ptr(),
        }),
        _ => unreachable!(),
    })
}

/// Validates that the semantic type of an enum pattern is an enum, and returns the concrete enum.
fn extract_concrete_enum_from_pattern_and_validate(
    ctx: &mut ComputationContext<'_>,
    pattern: &ast::Pattern,
    ty: TypeId,
    enum_id: EnumId,
) -> Maybe<(ConcreteEnumId, usize)> {
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
            Err(ctx.diagnostics.report(pattern, UnexpectedEnumPattern { ty }))
        })?;
    // Check that these are the same enums.
    if enum_id != concrete_enum.enum_id(ctx.db) {
        return Err(ctx.diagnostics.report(
            pattern,
            WrongEnum { expected_enum: concrete_enum.enum_id(ctx.db), actual_enum: enum_id },
        ));
    }
    Ok((concrete_enum, n_snapshots))
}

/// Creates a local variable pattern.
fn create_variable_pattern(
    ctx: &mut ComputationContext<'_>,
    identifier: ast::TerminalIdentifier,
    modifier_list: &[ast::Modifier],
    ty: TypeId,
    stable_ptr: ast::PatternPtr,
    or_pattern_variables_map: &mut UnorderedHashMap<SmolStr, LocalVariable>,
) -> Pattern {
    let syntax_db = ctx.db.upcast();

    let var_id = match or_pattern_variables_map.get(&identifier.text(syntax_db)) {
        Some(var) => var.id,
        None => ctx
            .db
            .intern_local_var(LocalVarLongId(ctx.resolver.module_file_id, identifier.stable_ptr())),
    };
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
    let mut base_struct = None;
    for (index, arg) in ctor_syntax
        .arguments(syntax_db)
        .arguments(syntax_db)
        .elements(syntax_db)
        .into_iter()
        .enumerate()
    {
        // TODO: Extract to a function for results.
        match arg {
            ast::StructArg::StructArgSingle(arg) => {
                let arg_identifier = arg.identifier(syntax_db);
                let arg_name = arg_identifier.text(syntax_db);

                // Find struct member by name.
                let Some(member) = members.get(&arg_name) else {
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
                        let Ok(expr) = resolve_variable_by_name(
                            ctx,
                            &arg_identifier,
                            path.stable_ptr().into(),
                        ) else {
                            // Insert only the member id, for correct duplicate member reporting.
                            if member_exprs.insert(member.id, None).is_some() {
                                ctx.diagnostics
                                    .report(&arg_identifier, MemberSpecifiedMoreThanOnce);
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
                let inference = &mut ctx.resolver.inference();
                if let Err(err_set) = inference.conform_ty(actual_ty, expected_ty) {
                    if !member.ty.is_missing(db) {
                        let diag_added = ctx
                            .diagnostics
                            .report(&arg_identifier, WrongArgumentType { expected_ty, actual_ty });
                        inference.consume_reported_error(err_set, diag_added);
                    }
                    continue;
                }
            }
            ast::StructArg::StructArgTail(base_struct_syntax) => {
                // TODO(TomerStarkware): remove tail expression from argument list.
                if index
                    != ctor_syntax
                        .arguments(syntax_db)
                        .arguments(syntax_db)
                        .elements(syntax_db)
                        .len()
                        - 1
                {
                    ctx.diagnostics.report(&base_struct_syntax, StructBaseStructExpressionNotLast);
                    continue;
                }
                let base_struct_expr =
                    compute_expr_semantic(ctx, &base_struct_syntax.expression(syntax_db));
                let base_struct_ty = ctx.reduce_ty(base_struct_expr.ty());
                let inference = &mut ctx.resolver.inference();
                if let Err(err_set) = inference.conform_ty(base_struct_ty, ty) {
                    let diag_added = ctx.diagnostics.report(
                        &base_struct_syntax,
                        WrongArgumentType { expected_ty: ty, actual_ty: base_struct_ty },
                    );
                    inference.consume_reported_error(err_set, diag_added);
                    continue;
                }

                base_struct = Some((base_struct_expr.id, base_struct_syntax));
            }
        };
    }

    // Report errors for missing members.
    for (member_name, member) in members.iter() {
        if !member_exprs.contains_key(&member.id) {
            if base_struct.is_some() {
                check_struct_member_is_visible(
                    ctx,
                    member,
                    base_struct.clone().unwrap().1.stable_ptr().untyped(),
                    member_name,
                );
            } else {
                ctx.diagnostics
                    .report(ctor_syntax, MissingMember { member_name: member_name.clone() });
            }
        }
    }
    if members.len() == member_exprs.len() {
        if let Some((_, base_struct_syntax)) = base_struct {
            return Err(ctx
                .diagnostics
                .report(&base_struct_syntax, StructBaseStructExpressionNoEffect));
        }
    }
    Ok(Expr::StructCtor(ExprStructCtor {
        concrete_struct_id,
        members: member_exprs.into_iter().filter_map(|(x, y)| Some((x, y?))).collect(),
        base_struct: base_struct.map(|(x, _)| x),
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
        // Requires specific blocking as `NonZero` now has NumericLiteral support.
        if ty_str == "NonZero" {
            return Err(ctx.diagnostics.report_by_ptr(
                stable_ptr.untyped(),
                SemanticDiagnosticKind::WrongNumberOfArguments { expected: 1, actual: 0 },
            ));
        }
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
    let inference = &mut ctx.resolver.inference();
    inference.new_impl_var(concrete_trait_id, Some(stable_ptr.untyped()), lookup_context).map_err(
        |err_set| inference.report_on_pending_error(err_set, ctx.diagnostics, stable_ptr.untyped()),
    )?;

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
    let inference = &mut ctx.resolver.inference();
    inference.new_impl_var(concrete_trait_id, Some(stable_ptr.untyped()), lookup_context).map_err(
        |err_set| inference.report_on_pending_error(err_set, ctx.diagnostics, stable_ptr.untyped()),
    )?;

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
    // Save some work. ignore the result. The error, if any, will be reported later.
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
        TypeLongId::Coupon(_) => {
            Err(ctx.diagnostics.report(&rhs_syntax, TypeHasNoMembers { ty, member_name }))
        }
        TypeLongId::Missing(diag_added) => Err(diag_added),
        TypeLongId::FixedSizeArray { .. } => {
            Err(ctx.diagnostics.report(&rhs_syntax, TypeHasNoMembers { ty, member_name }))
        }
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
            ty: db.constant_semantic_data(constant_id)?.ty(),
            stable_ptr: path.stable_ptr().into(),
        })),
        ResolvedConcreteItem::ConstGenericParameter(generic_param_id) => {
            Ok(Expr::ParamConstant(ExprParamConstant {
                const_value_id: db.intern_const_value(ConstValue::Generic(generic_param_id)),
                ty: extract_matches!(
                    db.generic_param_semantic(generic_param_id)?,
                    GenericParam::Const
                )
                .ty,
                stable_ptr: path.stable_ptr().into(),
            }))
        }
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
    mut named_args: Vec<NamedArg>,
    stable_ptr: ast::ExprPtr,
) -> Maybe<Expr> {
    if let Ok(Some(attr)) = ctx
        .db
        .lookup_intern_function(function_id)
        .function
        .generic_function
        .unstable_feature(ctx.db.upcast())
    {
        validate_unstable_feature_usage(ctx, attr, stable_ptr);
    }
    // TODO(spapini): Better location for these diagnostics after the refactor for generics resolve.
    // TODO(lior): Check whether concrete_function_signature should be `Option` instead of `Maybe`.
    let signature = ctx.db.concrete_function_signature(function_id)?;

    // Check if the last item in `named_args`, has the argument name `__coupon__`, and remove it
    // if so.
    let mut coupon_arg: Option<ExprId> = None;
    if let Some(NamedArg(arg, Some(name_terminal), mutability)) = named_args.last() {
        let coupons_enabled = are_coupons_enabled(ctx.db, ctx.resolver.module_file_id);
        if name_terminal.text(ctx.db.upcast()) == "__coupon__" && coupons_enabled {
            // Check that the argument type is correct.
            let expected_ty = ctx.db.intern_type(TypeLongId::Coupon(function_id));
            let arg_typ = arg.ty();
            let actual_ty = ctx.reduce_ty(arg_typ);
            if !arg_typ.is_missing(ctx.db) {
                let inference = &mut ctx.resolver.inference();
                if let Err(err_set) = inference.conform_ty(actual_ty, expected_ty) {
                    let diag_added = ctx.diagnostics.report_by_ptr(
                        arg.stable_ptr().untyped(),
                        WrongArgumentType { expected_ty, actual_ty },
                    );
                    inference.consume_reported_error(err_set, diag_added);
                }
            }

            // Check that the argument is not mutable/reference.
            if *mutability != Mutability::Immutable {
                ctx.diagnostics
                    .report_by_ptr(arg.stable_ptr().untyped(), CouponArgumentNoModifiers);
            }

            coupon_arg = Some(arg.id);

            // Remove the __coupon__ argument from the argument list.
            named_args.pop();
        }
    }

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
        if !arg_typ.is_missing(ctx.db) {
            let inference = &mut ctx.resolver.inference();
            if let Err(err_set) = inference.conform_ty(actual_ty, expected_ty) {
                let diag_added = ctx.diagnostics.report_by_ptr(
                    arg.stable_ptr().untyped(),
                    WrongArgumentType { expected_ty, actual_ty },
                );
                inference.consume_reported_error(err_set, diag_added);
            }
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

    let expr_function_call = ExprFunctionCall {
        function: function_id,
        args,
        coupon_arg,
        ty: signature.return_type,
        stable_ptr,
    };
    // Check panicable.
    if signature.panicable && has_panic_incompatibility(ctx, &expr_function_call) {
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
) -> bool {
    // If this is not an actual function call, but actually a minus literal (e.g. -1), then this is
    // the same as nopanic.
    if try_extract_minus_literal(ctx.db, &ctx.exprs, expr_function_call).is_some() {
        return false;
    }
    if let Some(signature) = ctx.signature {
        // If the caller is nopanic, then this is a panic incompatibility.
        !signature.panicable
    } else {
        false
    }
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
    let mut features_to_remove = vec![];
    for feature_name in extract_allowed_features(syntax_db, &syntax, ctx.diagnostics) {
        if ctx.environment.allowed_features.insert(feature_name.clone()) {
            features_to_remove.push(feature_name);
        }
    }
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
                    if !inferred_type.is_missing(db) {
                        let inference = &mut ctx.resolver.inference();
                        if let Err(err_set) = inference.conform_ty(inferred_type, explicit_type) {
                            let diag_added = ctx.diagnostics.report(
                                &let_syntax.rhs(syntax_db),
                                WrongArgumentType {
                                    expected_ty: explicit_type,
                                    actual_ty: inferred_type,
                                },
                            );
                            inference.consume_reported_error(err_set, diag_added);
                        }
                    }
                    explicit_type
                }
            };

            let pattern = compute_pattern_semantic(
                ctx,
                &let_syntax.pattern(syntax_db),
                ty,
                &mut UnorderedHashMap::default(),
            );
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
                if concrete.is_must_use(db)? {
                    ctx.diagnostics.report(&expr_syntax, UnhandledMustUseType { ty });
                }
            }
            if let Expr::FunctionCall(expr_function_call) = &expr.expr {
                let generic_function_id = db
                    .lookup_intern_function(expr_function_call.function)
                    .function
                    .generic_function;
                if generic_function_id.is_must_use(db)? {
                    ctx.diagnostics.report(&expr_syntax, UnhandledMustUseFunction);
                }
            }
            semantic::Statement::Expr(semantic::StatementExpr {
                expr: expr.id,
                stable_ptr: syntax.stable_ptr(),
            })
        }
        ast::Statement::Continue(continue_syntax) => {
            if ctx.loop_ctx.is_none() {
                return Err(ctx
                    .diagnostics
                    .report(continue_syntax, ContinueOnlyAllowedInsideALoop));
            }
            semantic::Statement::Continue(semantic::StatementContinue {
                stable_ptr: syntax.stable_ptr(),
            })
        }
        ast::Statement::Return(return_syntax) => {
            if ctx.loop_ctx.is_some() {
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
            if !expected_ty.is_missing(db) && !expr_ty.is_missing(db) {
                let inference = &mut ctx.resolver.inference();
                if let Err(err_set) = inference.conform_ty(expr_ty, expected_ty) {
                    let diag_added = ctx.diagnostics.report_by_ptr(
                        stable_ptr,
                        WrongReturnType { expected_ty, actual_ty: expr_ty },
                    );
                    inference.consume_reported_error(err_set, diag_added);
                }
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
            let ty = ctx.reduce_ty(ty);
            match &mut ctx.loop_ctx {
                None => {
                    return Err(ctx.diagnostics.report(break_syntax, BreakOnlyAllowedInsideALoop));
                }
                Some(LoopContext::Loop(flow_merge)) => {
                    flow_merge.try_merge_types(
                        &mut ctx.resolver.inference(),
                        ctx.db,
                        ty,
                        |current_ty, break_ty| {
                            ctx.diagnostics.report_by_ptr(
                                stable_ptr,
                                IncompatibleLoopBreakTypes { current_ty, break_ty },
                            )
                        },
                    );
                }
                Some(LoopContext::While) => {
                    if expr_option.is_some() {
                        ctx.diagnostics.report(break_syntax, BreakWithValueOnlyAllowedInsideALoop);
                    };
                }
            };
            semantic::Statement::Break(semantic::StatementBreak {
                expr_option,
                stable_ptr: syntax.stable_ptr(),
            })
        }
        ast::Statement::Missing(_) => todo!(),
    };
    for feature_name in features_to_remove {
        ctx.environment.allowed_features.remove(&feature_name);
    }
    Ok(ctx.statements.alloc(statement))
}

/// Returns the allowed features of an object which supports attributes.
fn extract_allowed_features(
    db: &dyn SyntaxGroup,
    syntax: &impl QueryAttrs,
    diagnostics: &mut SemanticDiagnostics,
) -> Vec<SmolStr> {
    let mut features = vec![];
    for attr_syntax in syntax.query_attr(db, FEATURE_ATTR) {
        let attr = attr_syntax.structurize(db);
        let feature_name = match &attr.args[..] {
            [
                AttributeArg {
                    variant: AttributeArgVariant::Unnamed { value: ast::Expr::String(value), .. },
                    ..
                },
            ] => value.text(db),
            _ => {
                diagnostics
                    .report_by_ptr(attr.args_stable_ptr.untyped(), UnsupportedFeatureAttrArguments);
                continue;
            }
        };
        features.push(feature_name);
    }
    features
}

/// Computes the semantic model of an expression and reports diagnostics if the expression does not
/// evaluate to a boolean value.
fn compute_bool_condition_semantic(
    ctx: &mut ComputationContext<'_>,
    condition_syntax: &ast::Expr,
) -> ExprAndId {
    let condition = compute_expr_semantic(ctx, condition_syntax);
    let inference = &mut ctx.resolver.inference();
    if let Err(err_set) = inference.conform_ty(condition.ty(), core_bool_ty(ctx.db)) {
        let diag_added = ctx.diagnostics.report_by_ptr(
            condition.stable_ptr().untyped(),
            ConditionNotBool { condition_ty: condition.ty() },
        );
        inference.consume_reported_error(err_set, diag_added);
    }
    condition
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

/// Adds diagnostics if an expression using an unstable feature is not explicitly allowed to use the
/// feature.
fn validate_unstable_feature_usage(
    ctx: &mut ComputationContext<'_>,
    attr: Attribute,
    stable_ptr: ExprPtr,
) {
    let Some(feature_name) = attr.args.iter().find_map(|arg| match &arg.variant {
        AttributeArgVariant::Named { value: ast::Expr::String(value), name, .. }
            if name == "feature" =>
        {
            Some(value.text(ctx.db.upcast()))
        }
        // TODO(orizi): Creates diagnostics for this case.
        _ => None,
    }) else {
        return;
    };
    let mut env = &ctx.environment;
    loop {
        if env.allowed_features.contains(feature_name.as_str()) {
            // The feature is allowed.
            return;
        }
        if let Some(parent) = env.parent.as_ref() {
            // Continue checking if the feature was allowed up the tree.
            env = parent;
        } else {
            ctx.diagnostics.report_by_ptr(stable_ptr.untyped(), UnstableFeature { feature_name });
            return;
        }
    }
}
