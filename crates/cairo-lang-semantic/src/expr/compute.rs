//! This module is responsible of computing the semantic model of expressions and statements in
//! the code, while type checking.
//! It is invoked by queries for function bodies and other code blocks.

use core::panic;
use std::ops::Deref;
use std::sync::Arc;

use ast::PathSegment;
use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::db::{get_all_path_leaves, validate_attributes_flat};
use cairo_lang_defs::diagnostic_utils::StableLocation;
use cairo_lang_defs::ids::{
    EnumId, FunctionTitleId, GenericKind, LanguageElementId, LocalVarLongId, LookupItemId,
    MemberId, ModuleFileId, ModuleItemId, NamedLanguageElementId, StatementConstLongId,
    StatementItemId, StatementUseLongId, TraitFunctionId, TraitId, VarId,
};
use cairo_lang_defs::plugin::{InlineMacroExprPlugin, MacroPluginMetadata};
use cairo_lang_diagnostics::{Maybe, ToOption, skip_diagnostic};
use cairo_lang_filesystem::cfg::CfgSet;
use cairo_lang_filesystem::ids::{FileKind, FileLongId, VirtualFile};
use cairo_lang_proc_macros::DebugWithDb;
use cairo_lang_syntax::node::ast::{
    BinaryOperator, BlockOrIf, ClosureParamWrapper, ExprPtr, OptionReturnTypeClause, PatternListOr,
    PatternStructParam, UnaryOperator,
};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::{GetIdentifier, PathSegmentEx};
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{Terminal, TypedStablePtr, TypedSyntaxNode, ast};
use cairo_lang_utils as utils;
use cairo_lang_utils::ordered_hash_map::{Entry, OrderedHashMap};
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use cairo_lang_utils::unordered_hash_set::UnorderedHashSet;
use cairo_lang_utils::{Intern, LookupIntern, OptionHelper, extract_matches, try_extract_matches};
use itertools::{Itertools, chain, zip_eq};
use num_bigint::BigInt;
use num_traits::ToPrimitive;
use smol_str::SmolStr;

use super::inference::canonic::ResultNoErrEx;
use super::inference::conform::InferenceConform;
use super::inference::infers::InferenceEmbeddings;
use super::inference::{Inference, InferenceData, InferenceError};
use super::objects::*;
use super::pattern::{
    Pattern, PatternEnumVariant, PatternFixedSizeArray, PatternLiteral, PatternMissing,
    PatternOtherwise, PatternTuple, PatternVariable,
};
use crate::corelib::{
    core_binary_operator, core_bool_ty, core_unary_operator, false_literal_expr, get_usize_ty,
    never_ty, true_literal_expr, try_get_core_ty_by_name, unit_expr, unit_ty,
    unwrap_error_propagation_type, validate_literal,
};
use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnosticKind::{self, *};
use crate::diagnostic::{
    ElementKind, MultiArmExprKind, NotFoundItemType, SemanticDiagnostics,
    SemanticDiagnosticsBuilder, TraitInferenceErrors, UnsupportedOutsideOfFunctionFeatureName,
};
use crate::expr::inference::solver::SolutionSet;
use crate::expr::inference::{ImplVarTraitItemMappings, InferenceId};
use crate::items::constant::{ConstValue, resolve_const_expr_and_evaluate, validate_const_expr};
use crate::items::enm::SemanticEnumEx;
use crate::items::feature_kind::extract_item_feature_config;
use crate::items::functions::{concrete_function_closure_params, function_signature_params};
use crate::items::imp::{ImplLookupContext, filter_candidate_traits, infer_impl_by_self};
use crate::items::modifiers::compute_mutability;
use crate::items::us::get_use_path_segments;
use crate::items::visibility;
use crate::resolve::{
    EnrichedMembers, EnrichedTypeMemberAccess, ResolvedConcreteItem, ResolvedGenericItem, Resolver,
};
use crate::semantic::{self, Binding, FunctionId, LocalVariable, TypeId, TypeLongId};
use crate::substitution::SemanticRewriter;
use crate::types::{
    ClosureTypeLongId, ConcreteTypeId, add_type_based_diagnostics, are_coupons_enabled,
    extract_fixed_size_array_size, peel_snapshots, peel_snapshots_ex,
    resolve_type_with_environment, verify_fixed_size_array_size, wrap_in_snapshots,
};
use crate::usage::Usages;
use crate::{
    ConcreteEnumId, GenericArgumentId, GenericParam, LocalItem, Member, Mutability, Parameter,
    PatternStringLiteral, PatternStruct, Signature, StatementItemKind,
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

pub enum ContextFunction {
    Global,
    Function(Maybe<FunctionId>),
}

/// Context inside loops or closures.
#[derive(Debug, Clone)]
enum InnerContext {
    /// Context inside a `loop`
    Loop { type_merger: FlowMergeTypeHelper },
    /// Context inside a `while` loop
    While,
    /// Context inside a `for` loop
    For,
    /// Context inside a `closure`
    Closure { return_type: TypeId },
}

/// Context for computing the semantic model of expression trees.
pub struct ComputationContext<'ctx> {
    pub db: &'ctx dyn SemanticGroup,
    pub diagnostics: &'ctx mut SemanticDiagnostics,
    pub resolver: Resolver<'ctx>,
    signature: Option<&'ctx Signature>,
    environment: Box<Environment>,
    /// Arenas of semantic objects.
    pub arenas: Arenas,
    function_id: ContextFunction,
    /// Definitions of semantic variables.
    pub semantic_defs: UnorderedHashMap<semantic::VarId, semantic::Binding>,
    inner_ctx: Option<InnerContext>,
    cfg_set: Arc<CfgSet>,
    /// whether to look for closures when calling variables.
    /// TODO(TomerStarkware): Remove this once we disallow calling shadowed functions.
    are_closures_in_context: bool,
}
impl<'ctx> ComputationContext<'ctx> {
    pub fn new(
        db: &'ctx dyn SemanticGroup,
        diagnostics: &'ctx mut SemanticDiagnostics,
        resolver: Resolver<'ctx>,
        signature: Option<&'ctx Signature>,
        environment: Environment,
        function_id: ContextFunction,
    ) -> Self {
        let semantic_defs =
            environment.variables.values().by_ref().map(|var| (var.id(), var.clone())).collect();
        let cfg_set =
            resolver.settings.cfg_set.clone().map(Arc::new).unwrap_or_else(|| db.cfg_set());
        Self {
            db,
            diagnostics,
            resolver,
            signature,
            environment: Box::new(environment),
            arenas: Default::default(),
            function_id,
            semantic_defs,
            inner_ctx: None,
            cfg_set,
            are_closures_in_context: false,
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
            self.add_unused_binding_warning(&var_name, &var);
        }
        // Adds warning for unused items if required.
        for (ty_name, statement_ty) in std::mem::take(&mut self.environment.use_items) {
            if !self.environment.used_use_items.contains(&ty_name) && !ty_name.starts_with('_') {
                self.diagnostics.report(statement_ty.stable_ptr, UnusedUse);
            }
        }
        self.environment = parent.unwrap();
        res
    }

    /// Adds warning for unused bindings if required.
    fn add_unused_binding_warning(&mut self, var_name: &str, var: &Binding) {
        if !self.environment.used_variables.contains(&var.id()) && !var_name.starts_with('_') {
            match var {
                Binding::LocalItem(local_item) => match local_item.id {
                    StatementItemId::Constant(_) => {
                        self.diagnostics.report(var.stable_ptr(self.db.upcast()), UnusedConstant);
                    }
                    StatementItemId::Use(_) => {
                        self.diagnostics.report(var.stable_ptr(self.db.upcast()), UnusedUse);
                    }
                },
                Binding::LocalVar(_) | Binding::Param(_) => {
                    self.diagnostics.report(var.stable_ptr(self.db.upcast()), UnusedVariable);
                }
            }
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

        Err(self.diagnostics.report(stable_ptr, UnsupportedOutsideOfFunction(feature_name)))
    }

    fn reduce_ty(&mut self, ty: TypeId) -> TypeId {
        self.resolver.inference().rewrite(ty).no_err()
    }

    /// Applies inference rewriter to all the expressions in the computation context, and adds
    /// errors on types from the final expressions.
    pub fn apply_inference_rewriter_to_exprs(&mut self) {
        let mut analyzed_types = UnorderedHashSet::<_>::default();
        for (_id, expr) in self.arenas.exprs.iter_mut() {
            self.resolver.inference().internal_rewrite(expr).no_err();
            // Adding an error only once per type.
            if analyzed_types.insert(expr.ty()) {
                add_type_based_diagnostics(self.db, self.diagnostics, expr.ty(), &*expr);
            }
        }
    }

    /// Applies inference rewriter to all the rewritable things in the computation context.
    fn apply_inference_rewriter(&mut self) {
        self.apply_inference_rewriter_to_exprs();
        for (_id, pattern) in self.arenas.patterns.iter_mut() {
            self.resolver.inference().internal_rewrite(pattern).no_err();
        }
        for (_id, stmt) in self.arenas.statements.iter_mut() {
            self.resolver.inference().internal_rewrite(stmt).no_err();
        }
    }
    /// Returns whether the current context is inside a loop.
    fn is_inside_loop(&self) -> bool {
        match self.inner_ctx {
            None | Some(InnerContext::Closure { .. }) => false,
            Some(InnerContext::Loop { .. } | InnerContext::While | InnerContext::For) => true,
        }
    }
}

// TODO(ilya): Change value to VarId.
pub type EnvVariables = OrderedHashMap<SmolStr, Binding>;

type EnvItems = OrderedHashMap<SmolStr, StatementGenericItemData>;

/// Struct that holds the resolved generic type of a statement item.
#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
struct StatementGenericItemData {
    resolved_generic_item: ResolvedGenericItem,
    stable_ptr: SyntaxStablePtrId,
}

// TODO(spapini): Consider using identifiers instead of SmolStr everywhere in the code.
/// A state which contains all the variables defined at the current resolver until now, and a
/// pointer to the parent environment.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Environment {
    parent: Option<Box<Environment>>,
    variables: EnvVariables,
    used_variables: UnorderedHashSet<semantic::VarId>,
    use_items: EnvItems,
    used_use_items: UnorderedHashSet<SmolStr>,
}
impl Environment {
    /// Adds a parameter to the environment.
    pub fn add_param(
        &mut self,
        diagnostics: &mut SemanticDiagnostics,
        semantic_param: Parameter,
        ast_param: &ast::Param,
        function_title_id: Option<FunctionTitleId>,
    ) -> Maybe<()> {
        if let utils::ordered_hash_map::Entry::Vacant(entry) =
            self.variables.entry(semantic_param.name.clone())
        {
            entry.insert(Binding::Param(semantic_param));
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
            use_items: Default::default(),
            used_use_items: Default::default(),
        }
    }
}

/// Returns the requested item from the environment if it exists. Returns None otherwise.
pub fn get_statement_item_by_name(
    env: &mut Environment,
    item_name: &SmolStr,
) -> Option<ResolvedGenericItem> {
    let mut maybe_env = Some(&mut *env);
    while let Some(curr_env) = maybe_env {
        if let Some(var) = curr_env.use_items.get(item_name) {
            curr_env.used_use_items.insert(item_name.clone());
            return Some(var.resolved_generic_item.clone());
        }
        maybe_env = curr_env.parent.as_deref_mut();
    }
    None
}

/// Computes the semantic model of an expression.
/// Note that this expr will always be "registered" in the arena, so it can be looked up in the
/// language server.
pub fn compute_expr_semantic(ctx: &mut ComputationContext<'_>, syntax: &ast::Expr) -> ExprAndId {
    let expr = maybe_compute_expr_semantic(ctx, syntax);
    let expr = wrap_maybe_with_missing(ctx, expr, syntax.stable_ptr());
    let id = ctx.arenas.exprs.alloc(expr.clone());
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
        ast::Expr::For(expr) => compute_expr_for_semantic(ctx, expr),
        ast::Expr::Closure(expr) => compute_expr_closure_semantic(ctx, expr, None),
    }
}

fn compute_expr_inline_macro_semantic(
    ctx: &mut ComputationContext<'_>,
    syntax: &ast::ExprInlineMacro,
) -> Maybe<Expr> {
    let syntax_db = ctx.db.upcast();

    let crate_id = ctx.resolver.owning_crate_id;

    let macro_name = syntax.path(syntax_db).as_syntax_node().get_text_without_trivia(syntax_db);
    let Some(macro_plugin_id) =
        ctx.db.crate_inline_macro_plugins(crate_id).get(&macro_name).cloned()
    else {
        return Err(ctx.diagnostics.report(syntax, InlineMacroNotFound(macro_name.into())));
    };
    let macro_plugin = ctx.db.lookup_intern_inline_macro_plugin(macro_plugin_id);

    // Skipping expanding an inline macro if it had a parser error.
    if syntax.as_syntax_node().descendants(syntax_db).any(|node| {
        matches!(
            node.kind(syntax_db),
            SyntaxKind::ExprMissing
                | SyntaxKind::WrappedArgListMissing
                | SyntaxKind::StatementMissing
                | SyntaxKind::ModuleItemMissing
                | SyntaxKind::TraitItemMissing
                | SyntaxKind::ImplItemMissing
                | SyntaxKind::TokenMissing
                | SyntaxKind::TokenSkipped
        )
    }) {
        return Err(skip_diagnostic());
    }

    let result = macro_plugin.generate_code(
        syntax_db,
        syntax,
        &MacroPluginMetadata {
            cfg_set: &ctx.cfg_set,
            declared_derives: &ctx.db.declared_derives(crate_id),
            allowed_features: &ctx.resolver.data.feature_config.allowed_features,
            edition: ctx.resolver.settings.edition,
        },
    );
    let mut diag_added = None;
    for diagnostic in result.diagnostics {
        diag_added =
            Some(ctx.diagnostics.report(diagnostic.stable_ptr, PluginDiagnostic(diagnostic)));
    }

    let Some(code) = result.code else {
        return Err(diag_added.unwrap_or_else(|| {
            ctx.diagnostics.report(syntax, InlineMacroFailed(macro_name.into()))
        }));
    };

    // Create a file
    let new_file = FileLongId::Virtual(VirtualFile {
        parent: Some(syntax.stable_ptr().untyped().file_id(ctx.db.upcast())),
        name: code.name,
        content: code.content.into(),
        code_mappings: code.code_mappings.into(),
        kind: FileKind::Expr,
    })
    .intern(ctx.db);
    let expr_syntax = ctx.db.file_expr_syntax(new_file)?;
    let expr = compute_expr_semantic(ctx, &expr_syntax);
    Ok(expr.expr)
}

fn compute_expr_unary_semantic(
    ctx: &mut ComputationContext<'_>,
    syntax: &ast::ExprUnary,
) -> Maybe<Expr> {
    let syntax_db = ctx.db.upcast();
    let unary_op = syntax.op(syntax_db);
    let inner = syntax.expr(syntax_db);
    match (&unary_op, &inner) {
        // If this is not an actual function call, but actually a minus literal (e.g. -1).
        (UnaryOperator::Minus(_), ast::Expr::Literal(literal)) => {
            let (value, ty) = literal.numeric_value_and_suffix(syntax_db).unwrap_or_default();
            let ty = ty.as_ref().map(SmolStr::as_str);

            Ok(Expr::Literal(new_literal_expr(ctx, ty, -value, syntax.stable_ptr().into())?))
        }
        (UnaryOperator::At(_), inner) => {
            let expr = compute_expr_semantic(ctx, inner);

            let ty = TypeLongId::Snapshot(expr.ty()).intern(ctx.db);
            Ok(Expr::Snapshot(ExprSnapshot {
                inner: expr.id,
                ty,
                stable_ptr: syntax.stable_ptr().into(),
            }))
        }
        (UnaryOperator::Desnap(_), inner) => {
            let (desnapped_expr, desnapped_ty) = {
                // The expr the desnap acts on. E.g. `x` in `*x`.
                let desnapped_expr = compute_expr_semantic(ctx, inner);
                let desnapped_expr_type = ctx.reduce_ty(desnapped_expr.ty());

                let desnapped_ty = match desnapped_expr_type.lookup_intern(ctx.db) {
                    TypeLongId::Var(_) | TypeLongId::ImplType(_) => {
                        let inference = &mut ctx.resolver.inference();
                        // The type of the full desnap expr. E.g. the type of `*x` for `*x`.
                        let desnap_expr_type =
                            inference.new_type_var(Some(inner.stable_ptr().untyped()));
                        let desnapped_expr_type_var =
                            TypeLongId::Snapshot(desnap_expr_type).intern(ctx.db);
                        if let Err(err_set) =
                            inference.conform_ty(desnapped_expr_type_var, desnapped_expr_type)
                        {
                            let diag_added = ctx.diagnostics.report(
                                syntax,
                                WrongArgumentType {
                                    expected_ty: desnapped_expr_type_var,
                                    actual_ty: desnapped_expr_type,
                                },
                            );
                            inference.consume_reported_error(err_set, diag_added);
                            return Err(diag_added);
                        };
                        ctx.reduce_ty(desnap_expr_type)
                    }
                    TypeLongId::Snapshot(ty) => ty,
                    _ => {
                        return Err(ctx.diagnostics.report(&unary_op, DesnapNonSnapshot));
                    }
                };
                (desnapped_expr, desnapped_ty)
            };

            Ok(Expr::Desnap(ExprDesnap {
                inner: desnapped_expr.id,
                ty: desnapped_ty,
                stable_ptr: syntax.stable_ptr().into(),
            }))
        }
        (_, inner) => {
            let expr = compute_expr_semantic(ctx, inner);

            let concrete_trait_function = match core_unary_operator(
                ctx.db,
                &mut ctx.resolver.inference(),
                &unary_op,
                syntax.into(),
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
                    Some(syntax.into()),
                )
                .map_err(|err_set| {
                    inference.report_on_pending_error(err_set, ctx.diagnostics, syntax.into())
                })?;

            expr_function_call(
                ctx,
                function,
                vec![NamedArg(expr, None, Mutability::Immutable)],
                syntax,
                syntax.stable_ptr().into(),
            )
        }
    }
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
    let rhs_syntax = syntax.rhs(syntax_db);

    match binary_op {
        ast::BinaryOperator::Dot(_) => {
            let lexpr = compute_expr_semantic(ctx, lhs_syntax);
            dot_expr(ctx, lexpr, rhs_syntax, stable_ptr)
        }
        ast::BinaryOperator::Eq(_) => {
            let lexpr = compute_expr_semantic(ctx, lhs_syntax);
            let rexpr = compute_expr_semantic(ctx, &rhs_syntax);

            let member_path = match lexpr.expr {
                Expr::Var(expr) => ExprVarMemberPath::Var(expr),
                Expr::MemberAccess(ExprMemberAccess { member_path: Some(ref_arg), .. }) => ref_arg,
                _ => return Err(ctx.diagnostics.report(lhs_syntax, InvalidLhsForAssignment)),
            };

            let inference = &mut ctx.resolver.inference();
            inference.conform_ty_for_diag(
                rexpr.ty(),
                member_path.ty(),
                ctx.diagnostics,
                || rhs_syntax.stable_ptr().untyped(),
                |actual_ty, expected_ty| WrongArgumentType { expected_ty, actual_ty },
            )?;
            // Verify the variable argument is mutable.
            if !ctx.semantic_defs[&member_path.base_var()].is_mut() {
                ctx.diagnostics.report(syntax, AssignmentToImmutableVar);
            }
            Ok(Expr::Assignment(ExprAssignment {
                ref_arg: member_path,
                rhs: rexpr.id,
                ty: unit_ty(db),
                stable_ptr,
            }))
        }
        ast::BinaryOperator::AndAnd(_) | ast::BinaryOperator::OrOr(_) => {
            let lexpr = compute_expr_semantic(ctx, lhs_syntax);
            let rexpr = compute_expr_semantic(ctx, &rhs_syntax);

            let op = match binary_op {
                ast::BinaryOperator::AndAnd(_) => LogicalOperator::AndAnd,
                ast::BinaryOperator::OrOr(_) => LogicalOperator::OrOr,
                _ => unreachable!(),
            };

            let inference = &mut ctx.resolver.inference();
            let bool_ty = core_bool_ty(db);
            let _ = inference.conform_ty_for_diag(
                lexpr.expr.ty(),
                bool_ty,
                ctx.diagnostics,
                || lhs_syntax.stable_ptr().untyped(),
                |actual_ty, expected_ty| WrongType { expected_ty, actual_ty },
            );
            let _ = inference.conform_ty_for_diag(
                rexpr.expr.ty(),
                bool_ty,
                ctx.diagnostics,
                || rhs_syntax.stable_ptr().untyped(),
                |actual_ty, expected_ty| WrongType { expected_ty, actual_ty },
            );

            Ok(Expr::LogicalOperator(ExprLogicalOperator {
                lhs: lexpr.id,
                op,
                rhs: rexpr.id,
                ty: bool_ty,
                stable_ptr,
            }))
        }
        _ => call_core_binary_op(ctx, syntax, lhs_syntax, &rhs_syntax),
    }
}

/// Get the function call expression of a binary operation that is defined in the corelib.
fn call_core_binary_op(
    ctx: &mut ComputationContext<'_>,
    syntax: &ast::ExprBinary,
    lhs_syntax: &ast::Expr,
    rhs_syntax: &ast::Expr,
) -> Maybe<Expr> {
    let db = ctx.db;
    let stable_ptr = syntax.stable_ptr().into();
    let binary_op = syntax.op(db.upcast());

    let (concrete_trait_function, snapshot) =
        match core_binary_operator(db, &mut ctx.resolver.inference(), &binary_op, syntax.into())? {
            Err(err_kind) => {
                return Err(ctx.diagnostics.report(&binary_op, err_kind));
            }
            Ok(res) => res,
        };

    let impl_lookup_context = ctx.resolver.impl_lookup_context();
    let inference = &mut ctx.resolver.inference();
    let function = inference
        .infer_trait_function(concrete_trait_function, &impl_lookup_context, Some(syntax.into()))
        .map_err(|err_set| {
            inference.report_on_pending_error(err_set, ctx.diagnostics, syntax.into())
        })?;

    let mut lexpr = compute_expr_semantic(ctx, lhs_syntax);

    if let (Expr::Missing(_), BinaryOperator::LT(_)) = (&lexpr.expr, &binary_op) {
        return Err(ctx
            .diagnostics
            .report(binary_op.stable_ptr(), SemanticDiagnosticKind::MaybeMissingColonColon));
    }

    let mut rexpr = compute_expr_semantic(ctx, rhs_syntax);

    ctx.reduce_ty(lexpr.ty()).check_not_missing(db)?;
    ctx.reduce_ty(rexpr.ty()).check_not_missing(db)?;

    if snapshot {
        let ty = TypeLongId::Snapshot(lexpr.ty()).intern(ctx.db);
        let expr =
            Expr::Snapshot(ExprSnapshot { inner: lexpr.id, ty, stable_ptr: lexpr.stable_ptr() });
        lexpr = ExprAndId { expr: expr.clone(), id: ctx.arenas.exprs.alloc(expr) };
        let ty = TypeLongId::Snapshot(rexpr.ty()).intern(ctx.db);
        let expr =
            Expr::Snapshot(ExprSnapshot { inner: rexpr.id, ty, stable_ptr: rexpr.stable_ptr() });
        rexpr = ExprAndId { expr: expr.clone(), id: ctx.arenas.exprs.alloc(expr) };
    }

    let sig = ctx.db.concrete_function_signature(function)?;
    let first_param = sig.params.into_iter().next().unwrap();

    expr_function_call(
        ctx,
        function,
        vec![
            NamedArg(lexpr, None, first_param.mutability),
            NamedArg(rexpr, None, Mutability::Immutable),
        ],
        syntax,
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
    let expressions_syntax = &syntax.expressions(syntax_db).elements(syntax_db);
    for expr_syntax in expressions_syntax {
        let expr_semantic = compute_expr_semantic(ctx, expr_syntax);
        types.push(ctx.reduce_ty(expr_semantic.ty()));
        items.push(expr_semantic.id);
    }
    Ok(Expr::Tuple(ExprTuple {
        items,
        ty: TypeLongId::Tuple(types).intern(db),
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
    let size_ty = get_usize_ty(db);
    let (items, type_id, size) = if let Some(size_const_id) =
        extract_fixed_size_array_size(db, ctx.diagnostics, syntax, &ctx.resolver)?
    {
        // Fixed size array with a defined size must have exactly one element.
        let [expr] = exprs.as_slice() else {
            return Err(ctx.diagnostics.report(syntax, FixedSizeArrayNonSingleValue));
        };
        let expr_semantic = compute_expr_semantic(ctx, expr);
        let size = size_const_id
            .lookup_intern(db)
            .into_int()
            .ok_or_else(|| ctx.diagnostics.report(syntax, FixedSizeArrayNonNumericSize))?
            .to_usize()
            .unwrap();
        verify_fixed_size_array_size(ctx.diagnostics, &size.into(), syntax)?;
        (
            FixedSizeArrayItems::ValueAndSize(expr_semantic.id, size_const_id),
            expr_semantic.ty(),
            size_const_id,
        )
    } else if let Some((first_expr, tail_exprs)) = exprs.split_first() {
        let size = ConstValue::Int((tail_exprs.len() + 1).into(), size_ty).intern(db);
        let first_expr_semantic = compute_expr_semantic(ctx, first_expr);
        let mut items: Vec<ExprId> = vec![first_expr_semantic.id];
        // The type of the first expression is the type of the array. All other expressions must
        // have the same type.
        let first_expr_ty = ctx.reduce_ty(first_expr_semantic.ty());
        for expr_syntax in tail_exprs {
            let expr_semantic = compute_expr_semantic(ctx, expr_syntax);
            let inference = &mut ctx.resolver.inference();
            inference.conform_ty_for_diag(
                expr_semantic.ty(),
                first_expr_ty,
                ctx.diagnostics,
                || expr_syntax.into(),
                |actual_ty, expected_ty| WrongArgumentType { expected_ty, actual_ty },
            )?;
            items.push(expr_semantic.id);
        }
        (FixedSizeArrayItems::Items(items), first_expr_ty, size)
    } else {
        (
            FixedSizeArrayItems::Items(vec![]),
            ctx.resolver.inference().new_type_var(Some(syntax.into())),
            ConstValue::Int(0.into(), size_ty).intern(db),
        )
    };
    Ok(Expr::FixedSizeArray(ExprFixedSizeArray {
        items,
        ty: TypeLongId::FixedSizeArray { type_id, size }.intern(db),
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
    let args_syntax = syntax.arguments(syntax_db).arguments(syntax_db);
    // Check if this is a variable.
    let segments = path.elements(syntax_db);
    let mut is_shadowed_by_variable = false;
    if let [PathSegment::Simple(ident_segment)] = &segments[..] {
        let identifier = ident_segment.ident(syntax_db);
        let variable_name = identifier.text(ctx.db.upcast());
        if let Some(var) = get_binded_expr_by_name(ctx, &variable_name, path.stable_ptr().into()) {
            is_shadowed_by_variable = true;
            // if closures are not in context, we want to call the function instead of the variable.
            if ctx.are_closures_in_context {
                let info = db.core_info();
                // TODO(TomerStarkware): find the correct trait based on captured variables.
                let fn_once_trait = info.fn_once_trt;
                let fn_trait = info.fn_trt;
                let self_expr = ExprAndId { expr: var.clone(), id: ctx.arenas.exprs.alloc(var) };
                let mut closure_call_data = |call_trait| {
                    compute_method_function_call_data(
                        ctx,
                        &[call_trait],
                        "call".into(),
                        self_expr.clone(),
                        syntax.into(),
                        None,
                        |ty, _, inference_errors| {
                            if call_trait == fn_once_trait {
                                Some(CallExpressionRequiresFunction { ty, inference_errors })
                            } else {
                                None
                            }
                        },
                        |_, _, _| {
                            unreachable!(
                                "There is one explicit trait, FnOnce trait. No implementations of \
                                 the trait, caused by both lack of implementation or multiple \
                                 implementations of the trait, are handled in \
                                 NoImplementationOfTrait function."
                            )
                        },
                    )
                };
                let (call_function_id, _, fixed_closure, closure_mutability) =
                    closure_call_data(fn_trait).or_else(|_| closure_call_data(fn_once_trait))?;

                let args_iter = args_syntax.elements(syntax_db).into_iter();
                // Normal parameters
                let mut args = vec![];
                let mut arg_types = vec![];
                for arg_syntax in args_iter {
                    let stable_ptr = arg_syntax.stable_ptr();
                    let arg = compute_named_argument_clause(ctx, arg_syntax, None);
                    if arg.2 != Mutability::Immutable {
                        return Err(ctx.diagnostics.report(stable_ptr, RefClosureArgument));
                    }
                    args.push(arg.0.id);
                    arg_types.push(arg.0.ty());
                }
                let args_expr = Expr::Tuple(ExprTuple {
                    items: args,
                    ty: TypeLongId::Tuple(arg_types).intern(db),
                    stable_ptr: syntax.stable_ptr().into(),
                });
                let args_expr =
                    ExprAndId { expr: args_expr.clone(), id: ctx.arenas.exprs.alloc(args_expr) };
                return expr_function_call(
                    ctx,
                    call_function_id,
                    vec![
                        NamedArg(fixed_closure, None, closure_mutability),
                        NamedArg(args_expr, None, Mutability::Immutable),
                    ],
                    syntax,
                    syntax.stable_ptr().into(),
                );
            }
        }
    }

    let item = ctx.resolver.resolve_concrete_path_ex(
        ctx.diagnostics,
        &path,
        NotFoundItemType::Function,
        Some(&mut ctx.environment),
    )?;

    match item {
        ResolvedConcreteItem::Variant(variant) => {
            let concrete_enum_type =
                TypeLongId::Concrete(ConcreteTypeId::Enum(variant.concrete_enum_id)).intern(db);
            if concrete_enum_type.is_phantom(db) {
                ctx.diagnostics.report(syntax, CannotCreateInstancesOfPhantomTypes);
            }

            // TODO(Gil): Consider not invoking the TraitFunction inference below if there were
            // errors in argument semantics, in order to avoid unnecessary diagnostics.
            let named_args: Vec<_> = args_syntax
                .elements(syntax_db)
                .into_iter()
                .map(|arg_syntax| compute_named_argument_clause(ctx, arg_syntax, None))
                .collect();
            if named_args.len() != 1 {
                return Err(ctx.diagnostics.report(
                    syntax,
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
            let inference = &mut ctx.resolver.inference();
            inference.conform_ty_for_diag(
                arg.ty(),
                variant.ty,
                ctx.diagnostics,
                || args_syntax.stable_ptr().untyped(),
                |actual_ty, expected_ty| WrongArgumentType { expected_ty, actual_ty },
            )?;
            Ok(semantic::Expr::EnumVariantCtor(semantic::ExprEnumVariantCtor {
                variant,
                value_expr: arg.id,
                ty: concrete_enum_type,
                stable_ptr: syntax.stable_ptr().into(),
            }))
        }
        ResolvedConcreteItem::Function(function) => {
            if is_shadowed_by_variable {
                return Err(ctx.diagnostics.report(
                    &path,
                    CallingShadowedFunction {
                        shadowed_function_name: path
                            .elements(syntax_db)
                            .first()
                            .unwrap()
                            .identifier(syntax_db),
                    },
                ));
            }
            // TODO(Gil): Consider not invoking the TraitFunction inference below if there were
            // errors in argument semantics, in order to avoid unnecessary diagnostics.

            // Note there may be n+1 arguments for n parameters, if the last one is a coupon.
            let mut args_iter = args_syntax.elements(syntax_db).into_iter();
            // Normal parameters
            let mut named_args = vec![];
            let closure_params = concrete_function_closure_params(db, function)?;
            for ty in function_parameter_types(ctx, function)? {
                let Some(arg_syntax) = args_iter.next() else {
                    continue;
                };
                named_args.push(compute_named_argument_clause(
                    ctx,
                    arg_syntax,
                    closure_params.get(&ty).copied(),
                ));
            }

            // Maybe coupon
            if let Some(arg_syntax) = args_iter.next() {
                named_args.push(compute_named_argument_clause(ctx, arg_syntax, None));
            }

            expr_function_call(ctx, function, named_args, syntax, syntax.stable_ptr().into())
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
    closure_params_tuple_ty: Option<TypeId>,
) -> NamedArg {
    let syntax_db = ctx.db.upcast();

    let mutability = compute_mutability(
        ctx.diagnostics,
        syntax_db,
        &arg_syntax.modifiers(syntax_db).elements(syntax_db),
    );

    let arg_clause = arg_syntax.arg_clause(syntax_db);
    let (expr, arg_name_identifier) = match arg_clause {
        ast::ArgClause::Unnamed(arg_unnamed) => (
            handle_possible_closure_expr(
                ctx,
                &arg_unnamed.value(syntax_db),
                closure_params_tuple_ty,
            ),
            None,
        ),
        ast::ArgClause::Named(arg_named) => (
            handle_possible_closure_expr(ctx, &arg_named.value(syntax_db), closure_params_tuple_ty),
            Some(arg_named.name(syntax_db)),
        ),
        ast::ArgClause::FieldInitShorthand(arg_field_init_shorthand) => {
            let name_expr = arg_field_init_shorthand.name(syntax_db);
            let stable_ptr: ast::ExprPtr = name_expr.stable_ptr().into();
            let arg_name_identifier = name_expr.name(syntax_db);
            let maybe_expr = resolve_variable_by_name(ctx, &arg_name_identifier, stable_ptr);
            let expr = wrap_maybe_with_missing(ctx, maybe_expr, stable_ptr);
            let expr = ExprAndId { expr: expr.clone(), id: ctx.arenas.exprs.alloc(expr) };
            (expr, Some(arg_name_identifier))
        }
    };
    NamedArg(expr, arg_name_identifier, mutability)
}

/// Handles the semantic computation of a closure expression.
/// It processes a closure expression, computes its semantic model,
/// allocates it in the expression arena, and ensures that the closure's
/// parameter types are conformed if provided.
fn handle_possible_closure_expr(
    ctx: &mut ComputationContext<'_>,
    expr: &ast::Expr,
    closure_param_types: Option<TypeId>,
) -> ExprAndId {
    if let ast::Expr::Closure(expr_closure) = expr {
        let expr = compute_expr_closure_semantic(ctx, expr_closure, closure_param_types);
        let expr = wrap_maybe_with_missing(ctx, expr, expr_closure.stable_ptr().into());
        let id = ctx.arenas.exprs.alloc(expr.clone());
        ExprAndId { expr, id }
    } else {
        compute_expr_semantic(ctx, expr)
    }
}

pub fn compute_root_expr(
    ctx: &mut ComputationContext<'_>,
    syntax: &ast::ExprBlock,
    return_type: TypeId,
) -> Maybe<ExprId> {
    // Conform TypeEqual constraints for Associated type bounds.
    let inference = &mut ctx.resolver.data.inference_data.inference(ctx.db);
    for param in &ctx.resolver.data.generic_params {
        let Ok(GenericParam::Impl(imp)) = ctx.db.generic_param_semantic(*param) else {
            continue;
        };
        let Ok(concrete_trait_id) = imp.concrete_trait else {
            continue;
        };
        if crate::corelib::fn_traits(ctx.db).contains(&concrete_trait_id.trait_id(ctx.db)) {
            ctx.are_closures_in_context = true;
        }
    }
    let constrains =
        ctx.db.generic_params_type_constraints(ctx.resolver.data.generic_params.clone());
    inference.conform_generic_params_type_constraints(&constrains);

    let return_type = ctx.reduce_ty(return_type);
    let res = compute_expr_block_semantic(ctx, syntax)?;
    let res_ty = ctx.reduce_ty(res.ty());
    let res = ctx.arenas.exprs.alloc(res);
    let inference = &mut ctx.resolver.inference();
    let _ = inference.conform_ty_for_diag(
        res_ty,
        return_type,
        ctx.diagnostics,
        || {
            ctx.signature
                .map(|s| match s.stable_ptr.lookup(ctx.db.upcast()).ret_ty(ctx.db.upcast()) {
                    OptionReturnTypeClause::Empty(_) => syntax.stable_ptr().untyped(),
                    OptionReturnTypeClause::ReturnTypeClause(return_type_clause) => {
                        return_type_clause.ty(ctx.db.upcast()).stable_ptr().untyped()
                    }
                })
                .unwrap_or_else(|| syntax.stable_ptr().untyped())
        },
        |actual_ty, expected_ty| WrongReturnType { expected_ty, actual_ty },
    );

    // Check fully resolved.
    inference.finalize(ctx.diagnostics, syntax.into());

    ctx.apply_inference_rewriter();
    if ctx.signature.map(|s| s.is_const) == Some(true) {
        validate_const_expr(ctx, res);
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
            if let Statement::Return(_) | Statement::Break(_) =
                &new_ctx.arenas.statements[*statement]
            {
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
    multi_arm_expr_kind: MultiArmExprKind,
    never_type: TypeId,
    final_type: Option<TypeId>,
    /// Whether or not the Helper had a previous type merge error.
    had_merge_error: bool,
}
impl FlowMergeTypeHelper {
    fn new(db: &dyn SemanticGroup, multi_arm_expr_kind: MultiArmExprKind) -> Self {
        Self {
            multi_arm_expr_kind,
            never_type: never_ty(db),
            final_type: None,
            had_merge_error: false,
        }
    }

    /// Merge a type into the helper. Returns false on error or if had a previous error.
    /// May conform the type to the self.expected_type, if set. Mostly, it is called after
    /// the types have already been conformed earlier, in which case it has no external effect.
    fn try_merge_types(
        &mut self,
        db: &dyn SemanticGroup,
        diagnostics: &mut SemanticDiagnostics,
        inference: &mut Inference<'_>,
        ty: TypeId,
        stable_ptr: SyntaxStablePtrId,
    ) -> bool {
        if self.had_merge_error {
            return false;
        }

        if ty != self.never_type && !ty.is_missing(db) {
            if let Some(pending) = &self.final_type {
                if let Err(err_set) = inference.conform_ty(ty, *pending) {
                    let diag_added = diagnostics.report(
                        stable_ptr,
                        IncompatibleArms {
                            multi_arm_expr_kind: self.multi_arm_expr_kind,
                            pending_ty: *pending,
                            different_ty: ty,
                        },
                    );
                    inference.consume_reported_error(err_set, diag_added);
                    self.had_merge_error = true;
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

/// computes the semantic of a match arm pattern and the block expression.
fn compute_arm_semantic(
    ctx: &mut ComputationContext<'_>,
    expr: &Expr,
    arm_expr_syntax: ast::Expr,
    patterns_syntax: &PatternListOr,
    // Whether the arm is a while let arm. This case is handled a little differently.
    is_while_let_arm: bool,
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
                let variables = pattern.variables(&new_ctx.arenas.patterns);
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
                                if inference
                                    .conform_ty_for_diag(
                                        actual_ty,
                                        expected_ty,
                                        new_ctx.diagnostics,
                                        || get_location().stable_ptr().untyped(),
                                        |actual_ty, expected_ty| WrongType {
                                            expected_ty,
                                            actual_ty,
                                        },
                                    )
                                    .is_err()
                                {
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
            let variables = pattern.variables(&new_ctx.arenas.patterns);

            if variables.len() != arm_patterns_variables.len() {
                new_ctx.diagnostics.report(pattern_syntax, MissingVariableInPattern);
            }

            for v in variables {
                let var_def = Binding::LocalVar(v.var.clone());
                // TODO(spapini): Wrap this in a function to couple with semantic_defs
                // insertion.
                new_ctx.environment.variables.insert(v.name.clone(), var_def.clone());
                new_ctx.semantic_defs.insert(var_def.id(), var_def);
            }
        }
        let arm_expr = if is_while_let_arm {
            let ast::Expr::Block(arm_expr_syntax) = arm_expr_syntax else {
                unreachable!("Expected a block expression for a loop arm.");
            };
            let (id, _) = compute_loop_body_semantic(new_ctx, arm_expr_syntax, InnerContext::While);
            let expr = new_ctx.arenas.exprs[id].clone();
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
    let mut helper = FlowMergeTypeHelper::new(ctx.db, MultiArmExprKind::Match);
    for (_, expr) in patterns_and_exprs.iter() {
        let expr_ty = ctx.reduce_ty(expr.ty());
        if !helper.try_merge_types(
            ctx.db,
            ctx.diagnostics,
            &mut ctx.resolver.inference(),
            expr_ty,
            expr.stable_ptr().untyped(),
        ) {
            break;
        };
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
                ExprAndId { expr: if_block.clone(), id: ctx.arenas.exprs.alloc(if_block) },
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

    let mut helper = FlowMergeTypeHelper::new(ctx.db, MultiArmExprKind::If);
    let if_block_ty = ctx.reduce_ty(if_block.ty());
    let else_block_ty = ctx.reduce_ty(else_block_ty);
    let inference = &mut ctx.resolver.inference();
    let _ = helper.try_merge_types(ctx.db, ctx.diagnostics, inference, if_block_ty, syntax.into())
        && helper.try_merge_types(ctx.db, ctx.diagnostics, inference, else_block_ty, syntax.into());
    Ok(Expr::If(ExprIf {
        condition,
        if_block: if_block.id,
        else_block: else_block_opt.map(|else_block| ctx.arenas.exprs.alloc(else_block)),
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

    let (body, inner_ctx) = compute_loop_body_semantic(
        ctx,
        syntax.body(syntax_db),
        InnerContext::Loop { type_merger: FlowMergeTypeHelper::new(db, MultiArmExprKind::Loop) },
    );
    Ok(Expr::Loop(ExprLoop {
        body,
        ty: match inner_ctx {
            InnerContext::Loop { type_merger, .. } => type_merger.get_final_type(),
            _ => unreachable!("Expected loop context"),
        },
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
            let (body, _inner_ctx) =
                compute_loop_body_semantic(ctx, syntax.body(syntax_db), InnerContext::While);
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

/// Computes the semantic model of an expression of type [ast::ExprFor].
fn compute_expr_for_semantic(
    ctx: &mut ComputationContext<'_>,
    syntax: &ast::ExprFor,
) -> Maybe<Expr> {
    let db = ctx.db;
    let syntax_db = db.upcast();
    let expr_ptr = syntax.expr(syntax_db).stable_ptr();

    let expr = compute_expr_semantic(ctx, &syntax.expr(syntax_db));
    let expr_id = expr.id;

    let into_iterator_trait = ctx.db.core_info().into_iterator_trt;

    let (into_iterator_function_id, _, fixed_into_iter_var, into_iter_mutability) =
        compute_method_function_call_data(
            ctx,
            &[into_iterator_trait],
            "into_iter".into(),
            expr,
            expr_ptr.into(),
            None,
            |ty, _, inference_errors| {
                Some(NoImplementationOfTrait {
                    ty,
                    inference_errors,
                    trait_name: "IntoIterator".into(),
                })
            },
            |_, _, _| {
                unreachable!(
                    "There is one explicit trait, IntoIterator trait. No implementations of the \
                     trait, caused by both lack of implementation or multiple implementations of \
                     the trait, are handled in NoImplementationOfTrait function."
                )
            },
        )?;
    let into_iter_call = expr_function_call(
        ctx,
        into_iterator_function_id,
        vec![NamedArg(fixed_into_iter_var, None, into_iter_mutability)],
        expr_ptr,
        expr_ptr,
    )?;

    let into_iter_variable =
        LocalVarLongId(ctx.resolver.module_file_id, syntax.identifier(syntax_db).stable_ptr())
            .intern(ctx.db);

    let into_iter_expr = Expr::Var(ExprVar {
        var: VarId::Local(into_iter_variable),
        ty: into_iter_call.ty(),
        stable_ptr: into_iter_call.stable_ptr(),
    });
    let into_iter_member_path = ExprVarMemberPath::Var(ExprVar {
        var: VarId::Local(into_iter_variable),
        ty: into_iter_call.ty(),
        stable_ptr: into_iter_call.stable_ptr(),
    });
    let into_iter_expr_id = ctx.arenas.exprs.alloc(into_iter_expr.clone());

    let iterator_trait = ctx.db.core_info().iterator_trt;

    let (next_function_id, _, _, _) = compute_method_function_call_data(
        ctx,
        &[iterator_trait],
        "next".into(),
        ExprAndId { expr: into_iter_expr, id: into_iter_expr_id },
        expr_ptr.into(),
        None,
        |ty, _, inference_errors| {
            Some(NoImplementationOfTrait { ty, inference_errors, trait_name: "Iterator".into() })
        },
        |_, _, _| {
            unreachable!(
                "There is one explicit trait, Iterator trait. No implementations of the trait, \
                 caused by both lack of implementation or multiple implementations of the trait, \
                 are handled in NoImplementationOfTrait function."
            )
        },
    )?;

    let next_success_variant =
        match db.concrete_function_signature(next_function_id)?.return_type.lookup_intern(db) {
            TypeLongId::Concrete(semantic::ConcreteTypeId::Enum(enm)) => {
                assert_eq!(enm.enum_id(db.upcast()).name(db.upcast()), "Option");
                db.concrete_enum_variants(enm).unwrap().into_iter().next().unwrap()
            }
            _ => unreachable!(),
        };
    let (body_id, pattern) = ctx.run_in_subscope(|new_ctx| {
        let inner_pattern = compute_pattern_semantic(
            new_ctx,
            &syntax.pattern(syntax_db),
            next_success_variant.ty,
            &mut UnorderedHashMap::default(),
        );
        let variables = inner_pattern.variables(&new_ctx.arenas.patterns);
        for v in variables {
            let var_def = Binding::LocalVar(v.var.clone());
            new_ctx.environment.variables.insert(v.name.clone(), var_def.clone());
            new_ctx.semantic_defs.insert(var_def.id(), var_def);
        }
        let (body, _inner_ctx) =
            compute_loop_body_semantic(new_ctx, syntax.body(syntax_db), InnerContext::For);
        (body, new_ctx.arenas.patterns.alloc(inner_pattern.pattern))
    });
    Ok(Expr::For(ExprFor {
        into_iter: into_iterator_function_id,
        into_iter_member_path,
        next_function_id,
        expr_id,
        pattern,
        body: body_id,
        ty: unit_ty(ctx.db),
        stable_ptr: syntax.stable_ptr().into(),
    }))
}

/// Computes the semantic model for a body of a loop.
fn compute_loop_body_semantic(
    ctx: &mut ComputationContext<'_>,
    syntax: ast::ExprBlock,
    inner_ctx: InnerContext,
) -> (ExprId, InnerContext) {
    let db = ctx.db;
    let syntax_db = db.upcast();

    ctx.run_in_subscope(|new_ctx| {
        let old_inner_ctx = std::mem::replace(&mut new_ctx.inner_ctx, Some(inner_ctx));

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
                new_ctx.diagnostics.report(tail.deref(), TailExpressionNotAllowedInLoop);
            }
        }

        let inner_ctx = std::mem::replace(&mut new_ctx.inner_ctx, old_inner_ctx).unwrap();
        let body = new_ctx.arenas.exprs.alloc(Expr::Block(ExprBlock {
            statements: statements_semantic,
            tail: tail.map(|tail| tail.id),
            ty: unit_ty(db),
            stable_ptr: syntax.stable_ptr().into(),
        }));

        (body, inner_ctx)
    })
}

/// Computes the semantic model of an expression of type [ast::ExprClosure].
fn compute_expr_closure_semantic(
    ctx: &mut ComputationContext<'_>,
    syntax: &ast::ExprClosure,
    params_tuple_ty: Option<TypeId>,
) -> Maybe<Expr> {
    ctx.are_closures_in_context = true;
    let syntax_db = ctx.db.upcast();
    let (params, ret_ty, body) = ctx.run_in_subscope(|new_ctx| {
        let params = if let ClosureParamWrapper::NAry(params) = syntax.wrapper(syntax_db) {
            function_signature_params(
                new_ctx.diagnostics,
                new_ctx.db,
                &mut new_ctx.resolver,
                &params.params(syntax_db).elements(syntax_db),
                None,
                &mut new_ctx.environment,
            )
            .into_iter()
            .collect()
        } else {
            vec![]
        };
        let closure_type =
            TypeLongId::Tuple(params.iter().map(|param| param.ty).collect()).intern(new_ctx.db);
        if let Some(param_types) = params_tuple_ty {
            if let Err(err_set) = new_ctx.resolver.inference().conform_ty(closure_type, param_types)
            {
                new_ctx.resolver.inference().report_on_pending_error(
                    err_set,
                    new_ctx.diagnostics,
                    syntax.stable_ptr().untyped(),
                );
            }
        }

        params.iter().filter(|param| param.mutability == Mutability::Reference).for_each(|param| {
            new_ctx.diagnostics.report(param.stable_ptr(ctx.db.upcast()), RefClosureParam);
        });

        new_ctx
            .semantic_defs
            .extend(new_ctx.environment.variables.iter().map(|(_, var)| (var.id(), var.clone())));

        let return_type = match syntax.ret_ty(syntax_db) {
            OptionReturnTypeClause::ReturnTypeClause(ty_syntax) => resolve_type_with_environment(
                new_ctx.db,
                new_ctx.diagnostics,
                &mut new_ctx.resolver,
                &ty_syntax.ty(syntax_db),
                Some(&mut new_ctx.environment),
            ),
            OptionReturnTypeClause::Empty(missing) => {
                new_ctx.resolver.inference().new_type_var(Some(missing.stable_ptr().untyped()))
            }
        };
        let old_inner_ctx =
            std::mem::replace(&mut new_ctx.inner_ctx, Some(InnerContext::Closure { return_type }));
        let body = match syntax.expr(syntax_db) {
            ast::Expr::Block(syntax) => compute_closure_body_semantic(new_ctx, syntax),
            _ => compute_expr_semantic(new_ctx, &syntax.expr(syntax_db)).id,
        };
        std::mem::replace(&mut new_ctx.inner_ctx, old_inner_ctx).unwrap();
        let mut inference = new_ctx.resolver.inference();
        let _ = inference.conform_ty_for_diag(
            new_ctx.arenas.exprs[body].ty(),
            return_type,
            new_ctx.diagnostics,
            || match syntax.ret_ty(ctx.db.upcast()).stable_ptr().lookup(ctx.db.upcast()) {
                OptionReturnTypeClause::Empty(_) => syntax.expr(syntax_db).stable_ptr().untyped(),
                OptionReturnTypeClause::ReturnTypeClause(return_type_clause) => {
                    return_type_clause.ty(ctx.db.upcast()).stable_ptr().untyped()
                }
            },
            |actual_ty, expected_ty| WrongReturnType { expected_ty, actual_ty },
        );
        (params, return_type, body)
    });
    let parent_function = match ctx.function_id {
        ContextFunction::Global => Maybe::Err(ctx.diagnostics.report(syntax, ClosureInGlobalScope)),
        ContextFunction::Function(function_id) => function_id,
    };
    if matches!(ctx.function_id, ContextFunction::Global) {
        ctx.diagnostics.report(syntax, ClosureInGlobalScope);
    }

    let mut usages = Usages { usages: Default::default() };
    let usage = usages.handle_closure(&ctx.arenas, &params, body);
    let mut reported = UnorderedHashSet::<_>::default();
    // TODO(TomerStarkware): Add support for capturing mutable variables when then we have borrow.
    for (captured_var, expr) in
        chain!(usage.usage.iter(), usage.snap_usage.iter(), usage.changes.iter())
    {
        let Some(var) = ctx.semantic_defs.get(&captured_var.base_var()) else {
            // if the variable is not found in the semantic defs, it is closure parameter.
            continue;
        };

        if var.is_mut() && reported.insert(expr.stable_ptr()) {
            ctx.diagnostics.report(expr.stable_ptr(), MutableCapturedVariable);
        }
    }

    let captured_types = chain!(
        chain!(usage.usage.values(), usage.changes.values()).map(|item| item.ty()),
        usage.snap_usage.values().map(|item| wrap_in_snapshots(ctx.db, item.ty(), 1)),
    )
    .collect_vec();

    let ty = TypeLongId::Closure(ClosureTypeLongId {
        param_tys: params.iter().map(|param| param.ty).collect(),
        ret_ty,
        captured_types,
        parent_function,
        wrapper_location: StableLocation::new(syntax.wrapper(syntax_db).stable_ptr().into()),
    })
    .intern(ctx.db);

    Ok(Expr::ExprClosure(ExprClosure { body, params, stable_ptr: syntax.stable_ptr().into(), ty }))
}

/// Computes the semantic model for a body of a closure.
fn compute_closure_body_semantic(
    ctx: &mut ComputationContext<'_>,
    syntax: ast::ExprBlock,
) -> ExprId {
    let syntax_db = ctx.db.upcast();

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
            compute_statement_semantic(ctx, statement_syntax).to_option()
        })
        .collect();
    // Convert tail expression (if exists) to semantic model.
    let tail_semantic_expr = tail.map(|tail_expr| compute_expr_semantic(ctx, &tail_expr));
    let ty = if let Some(t) = &tail_semantic_expr {
        t.ty()
    } else if let Some(statement) = statements_semantic.last() {
        if let Statement::Return(_) | Statement::Break(_) = &ctx.arenas.statements[*statement] {
            never_ty(ctx.db)
        } else {
            unit_ty(ctx.db)
        }
    } else {
        unit_ty(ctx.db)
    };
    ctx.arenas.exprs.alloc(Expr::Block(ExprBlock {
        statements: statements_semantic,
        tail: tail_semantic_expr.map(|expr| expr.id),
        ty,
        stable_ptr: syntax.stable_ptr().into(),
    }))
}

/// Computes the semantic model of an expression of type [ast::ExprErrorPropagate].
fn compute_expr_error_propagate_semantic(
    ctx: &mut ComputationContext<'_>,
    syntax: &ast::ExprErrorPropagate,
) -> Maybe<Expr> {
    let syntax_db = ctx.db.upcast();

    let return_type = match ctx.inner_ctx {
        Some(InnerContext::Closure { return_type }) => return_type,
        None | Some(InnerContext::Loop { .. } | InnerContext::While | InnerContext::For) => {
            ctx.get_signature(
                syntax.into(),
                UnsupportedOutsideOfFunctionFeatureName::ErrorPropagate,
            )?
            .return_type
        }
    };

    let func_err_prop_ty = unwrap_error_propagation_type(ctx.db, return_type)
        .ok_or_else(|| ctx.diagnostics.report(syntax, ReturnTypeNotErrorPropagateType))?;

    // `inner_expr` is the expr inside the `?`.
    let inner_expr = match &func_err_prop_ty {
        crate::corelib::ErrorPropagationType::Option { .. } => {
            compute_expr_semantic(ctx, &syntax.expr(syntax_db))
        }
        crate::corelib::ErrorPropagationType::Result { .. } => {
            compute_expr_semantic(ctx, &syntax.expr(syntax_db))
        }
    };
    let func_err_variant = func_err_prop_ty.err_variant();

    let inner_expr_ty = ctx.reduce_ty(inner_expr.ty());
    inner_expr_ty.check_not_missing(ctx.db)?;
    let inner_expr_err_prop_ty =
        unwrap_error_propagation_type(ctx.db, inner_expr_ty).ok_or_else(|| {
            ctx.diagnostics.report(syntax, ErrorPropagateOnNonErrorType(inner_expr_ty))
        })?;
    let inner_expr_err_variant = inner_expr_err_prop_ty.err_variant();

    // Disallow error propagation inside a loop.
    if ctx.is_inside_loop() {
        ctx.diagnostics.report(syntax, SemanticDiagnosticKind::ErrorPropagateNotAllowedInsideALoop);
    }

    let conformed_err_variant_ty =
        ctx.resolver.inference().conform_ty(func_err_variant.ty, inner_expr_err_variant.ty);
    // If conforming the types failed, the next check will fail and a better diagnostic will be
    // added.
    let err_variant_ty = match conformed_err_variant_ty {
        Ok(ty) => ty,
        Err(err_set) => {
            ctx.resolver.inference().consume_error_without_reporting(err_set);
            inner_expr_err_variant.ty
        }
    };
    // TODO(orizi): When auto conversion of types is added, try to convert the error type.
    if func_err_variant.ty != err_variant_ty
        || func_err_variant.concrete_enum_id.enum_id(ctx.db)
            != inner_expr_err_variant.concrete_enum_id.enum_id(ctx.db)
    {
        ctx.diagnostics.report(
            syntax,
            IncompatibleErrorPropagateType {
                return_ty: return_type,
                err_ty: inner_expr_err_variant.ty,
            },
        );
    }
    Ok(Expr::PropagateError(ExprPropagateError {
        inner: inner_expr.id,
        ok_variant: inner_expr_err_prop_ty.ok_variant().clone(),
        err_variant: inner_expr_err_variant.clone(),
        func_err_variant: func_err_variant.clone(),
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
    let index_expr_syntax = &syntax.index_expr(syntax_db);
    let index_expr = compute_expr_semantic(ctx, index_expr_syntax);
    // Make sure the maximal amount of types is known when trying to access. Ignoring the returned
    // value, as any errors will be reported later.
    ctx.resolver.inference().solve().ok();
    let info = ctx.db.core_info();
    let candidate_traits = [info.index_trt, info.index_view_trt];
    let (function_id, _, fixed_expr, mutability) = compute_method_function_call_data(
        ctx,
        &candidate_traits[..],
        "index".into(),
        expr,
        syntax.into(),
        None,
        |ty, _, inference_errors| Some(NoImplementationOfIndexOperator { ty, inference_errors }),
        |ty, _, _| Some(MultipleImplementationOfIndexOperator(ty)),
    )?;

    expr_function_call(
        ctx,
        function_id,
        vec![
            NamedArg(fixed_expr, None, mutability),
            NamedArg(index_expr, None, Mutability::Immutable),
        ],
        syntax,
        index_expr_syntax.stable_ptr(),
    )
}

/// Computes the data needed for a method function call, and similar exprs (index operator). Method
/// call and Index operator differs in the diagnostics they emit. The function returns the
/// function_id to call, the trait containing the function, the self argument, with snapshots added
/// if needed, and the mutability of the self argument.
#[expect(clippy::too_many_arguments)]
fn compute_method_function_call_data(
    ctx: &mut ComputationContext<'_>,
    candidate_traits: &[TraitId],
    func_name: SmolStr,
    self_expr: ExprAndId,
    method_syntax: SyntaxStablePtrId,
    generic_args_syntax: Option<Vec<ast::GenericArg>>,
    no_implementation_diagnostic: impl Fn(
        TypeId,
        SmolStr,
        TraitInferenceErrors,
    ) -> Option<SemanticDiagnosticKind>,
    multiple_trait_diagnostic: fn(
        TypeId,
        TraitFunctionId,
        TraitFunctionId,
    ) -> Option<SemanticDiagnosticKind>,
) -> Maybe<(FunctionId, TraitId, ExprAndId, Mutability)> {
    let expr_ptr = self_expr.stable_ptr();
    let self_ty = ctx.reduce_ty(self_expr.ty());
    // Inference errors found when looking for candidates. Only relevant in the case of 0 candidates
    // found. If >0 candidates are found these are ignored as they may describe, e.g., "errors"
    // indicating certain traits/impls/functions don't match, which is OK as we only look for one.
    let mut inference_errors = vec![];
    let (candidates, mut fixed_expr, fixed_ty) = get_method_function_candidates(
        ctx,
        candidate_traits,
        &func_name,
        self_expr,
        method_syntax,
        expr_ptr,
        self_ty,
        &mut inference_errors,
    )?;

    let trait_function_id = match candidates[..] {
        [] => {
            return Err(no_implementation_diagnostic(
                self_ty,
                func_name,
                TraitInferenceErrors { traits_and_errors: inference_errors },
            )
            .map(|diag| ctx.diagnostics.report(method_syntax, diag))
            .unwrap_or_else(skip_diagnostic));
        }
        [trait_function_id] => trait_function_id,
        [trait_function_id0, trait_function_id1, ..] => {
            return Err(multiple_trait_diagnostic(
                fixed_ty,
                trait_function_id0,
                trait_function_id1,
            )
            .map(|diag| ctx.diagnostics.report(method_syntax, diag))
            .unwrap_or_else(skip_diagnostic));
        }
    };
    let (function_id, n_snapshots) =
        infer_impl_by_self(ctx, trait_function_id, fixed_ty, method_syntax, generic_args_syntax)?;

    let signature = ctx.db.trait_function_signature(trait_function_id).unwrap();
    let first_param = signature.params.into_iter().next().unwrap();
    for _ in 0..n_snapshots {
        let ty = TypeLongId::Snapshot(fixed_expr.ty()).intern(ctx.db);
        let expr = Expr::Snapshot(ExprSnapshot { inner: fixed_expr.id, ty, stable_ptr: expr_ptr });
        fixed_expr = ExprAndId { expr: expr.clone(), id: ctx.arenas.exprs.alloc(expr) };
    }

    Ok((
        function_id,
        trait_function_id.trait_id(ctx.db.upcast()),
        fixed_expr,
        first_param.mutability,
    ))
}

/// Return candidates for method functions that match the given arguments.
/// Also returns the expression to be used as self for the method call and its type.
#[expect(clippy::too_many_arguments)]
fn get_method_function_candidates(
    ctx: &mut ComputationContext<'_>,
    candidate_traits: &[TraitId],
    func_name: &SmolStr,
    self_expr: ExprAndId,
    method_syntax: SyntaxStablePtrId,
    expr_ptr: ExprPtr,
    self_ty: TypeId,
    inference_errors: &mut Vec<(TraitFunctionId, InferenceError)>,
) -> Result<(Vec<TraitFunctionId>, ExprAndId, TypeId), cairo_lang_diagnostics::DiagnosticAdded> {
    let mut candidates = filter_candidate_traits(
        ctx,
        inference_errors,
        self_ty,
        candidate_traits,
        func_name.clone(),
        method_syntax,
    );
    if !candidates.is_empty() {
        return Ok((candidates, self_expr, self_ty));
    }

    let mut fixed_expr = self_expr;
    let mut fixed_ty = self_ty;

    let base_var = match &fixed_expr.expr {
        Expr::Var(expr_var) => Some(expr_var.var),
        Expr::MemberAccess(ExprMemberAccess { member_path: Some(member_path), .. }) => {
            Some(member_path.base_var())
        }
        _ => None,
    };
    let is_mut_var = base_var
        .filter(|var_id| matches!(ctx.semantic_defs.get(var_id), Some(var) if var.is_mut()))
        .is_some();

    let deref_chain = ctx.db.deref_chain(self_ty, is_mut_var)?;

    for deref_info in deref_chain.derefs.iter() {
        let derefed_expr = expr_function_call(
            ctx,
            deref_info.function_id,
            vec![NamedArg(fixed_expr, None, deref_info.self_mutability)],
            method_syntax,
            expr_ptr,
        )?;

        fixed_expr =
            ExprAndId { expr: derefed_expr.clone(), id: ctx.arenas.exprs.alloc(derefed_expr) };

        candidates = filter_candidate_traits(
            ctx,
            inference_errors,
            deref_info.target_ty,
            candidate_traits,
            func_name.clone(),
            method_syntax,
        );
        if !candidates.is_empty() {
            fixed_ty = deref_info.target_ty;
            break;
        }
    }

    Ok((candidates, fixed_expr, fixed_ty))
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
    let id = ctx.arenas.patterns.alloc(pat.clone());
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
    let stable_ptr = pattern_syntax.into();
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
                Some(&mut ctx.environment),
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
            let item_result = ctx.resolver.resolve_generic_path(
                &mut Default::default(),
                path,
                NotFoundItemType::Identifier,
                Some(&mut ctx.environment),
            );
            if let Ok(item) = item_result {
                if let Some(generic_variant) =
                    try_extract_matches!(item, ResolvedGenericItem::Variant)
                {
                    let (concrete_enum, _n_snapshots) =
                        extract_concrete_enum_from_pattern_and_validate(
                            ctx,
                            pattern_syntax,
                            ty,
                            generic_variant.enum_id,
                        )?;
                    let concrete_variant = ctx
                        .db
                        .concrete_enum_variant(concrete_enum, &generic_variant)
                        .map_err(|_| ctx.diagnostics.report(path, UnknownEnum))?;
                    return Ok(Pattern::EnumVariant(PatternEnumVariant {
                        variant: concrete_variant,
                        inner_pattern: None,
                        ty,
                        stable_ptr: path.stable_ptr().into(),
                    }));
                }
            }

            // Paths with a single element are treated as identifiers, which will result in a
            // variable pattern if no matching enum variant is found. If a matching enum
            // variant exists, it is resolved to the corresponding concrete variant.
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
                ctx.resolver.resolve_concrete_path_ex(
                    ctx.diagnostics,
                    &pattern_struct.path(syntax_db),
                    NotFoundItemType::Type,
                    Some(&mut ctx.environment)
                )?,
                ResolvedConcreteItem::Type
            )
            .ok_or_else(|| ctx.diagnostics.report(&pattern_struct.path(syntax_db), NotAType))?;
            let inference = &mut ctx.resolver.inference();
            inference.conform_ty(pattern_ty, peel_snapshots(ctx.db, ty).1.intern(ctx.db)).map_err(
                |err_set| inference.report_on_pending_error(err_set, ctx.diagnostics, stable_ptr),
            )?;
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
                    Err(ctx.diagnostics.report(pattern_struct, UnexpectedStructPattern(ty)))
                })?;
            let pattern_param_asts = pattern_struct.params(syntax_db).elements(syntax_db);
            let struct_id = concrete_struct_id.struct_id(ctx.db);
            let mut members = ctx.db.concrete_struct_members(concrete_struct_id)?.as_ref().clone();
            let mut used_members = UnorderedHashSet::<_>::default();
            let mut get_member = |ctx: &mut ComputationContext<'_>,
                                  member_name: SmolStr,
                                  stable_ptr: SyntaxStablePtrId| {
                let member = members.swap_remove(&member_name).on_none(|| {
                    ctx.diagnostics.report(
                        stable_ptr,
                        if used_members.contains(&member_name) {
                            StructMemberRedefinition { struct_id, member_name: member_name.clone() }
                        } else {
                            NoSuchStructMember { struct_id, member_name: member_name.clone() }
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
                        field_patterns.push((member, ctx.arenas.patterns.alloc(pattern)));
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
                for (member_name, _) in members.iter() {
                    ctx.diagnostics.report(pattern_struct, MissingMember(member_name.clone()));
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
            |ty: TypeId| UnexpectedTuplePattern(ty),
            |expected, actual| WrongNumberOfTupleElements { expected, actual },
        )?,
        ast::Pattern::FixedSizeArray(_) => maybe_compute_tuple_like_pattern_semantic(
            ctx,
            pattern_syntax,
            ty,
            or_pattern_variables_map,
            |ty: TypeId| UnexpectedFixedSizeArrayPattern(ty),
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
    let (n_snapshots, long_ty) = finalized_snapshot_peeled_ty(ctx, ty, pattern_syntax)?;
    // Assert that the pattern is of the same type as the expr.
    match (pattern_syntax, &long_ty) {
        (ast::Pattern::Tuple(_), TypeLongId::Tuple(_) | TypeLongId::Var(_))
        | (
            ast::Pattern::FixedSizeArray(_),
            TypeLongId::FixedSizeArray { .. } | TypeLongId::Var(_),
        ) => {}
        _ => {
            return Err(ctx.diagnostics.report(pattern_syntax, unexpected_pattern(ty)));
        }
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
    let inner_tys = match long_ty {
        TypeLongId::Tuple(inner_tys) => inner_tys,
        TypeLongId::FixedSizeArray { type_id: inner_ty, size } => {
            let size = if let ConstValue::Int(value, _) = size.lookup_intern(ctx.db) {
                value.to_usize().expect("Fixed sized array size must always be usize.")
            } else {
                let inference = &mut ctx.resolver.inference();
                let expected_size =
                    ConstValue::Int(patterns_syntax.len().into(), get_usize_ty(ctx.db))
                        .intern(ctx.db);
                if let Err(err) = inference.conform_const(size, expected_size) {
                    let _ = inference.report_on_pending_error(
                        err,
                        ctx.diagnostics,
                        pattern_syntax.stable_ptr().untyped(),
                    );
                }
                patterns_syntax.len()
            };

            [inner_ty].repeat(size)
        }
        TypeLongId::Var(_) => {
            let inference = &mut ctx.resolver.inference();
            let (inner_tys, tuple_like_ty) = if matches!(pattern_syntax, ast::Pattern::Tuple(_)) {
                let inner_tys: Vec<_> = patterns_syntax
                    .iter()
                    .map(|e| inference.new_type_var(Some(e.stable_ptr().untyped())))
                    .collect();
                (inner_tys.clone(), TypeLongId::Tuple(inner_tys))
            } else {
                let var = inference.new_type_var(Some(pattern_syntax.stable_ptr().untyped()));
                (
                    vec![var; patterns_syntax.len()],
                    TypeLongId::FixedSizeArray {
                        type_id: var,
                        size: ConstValue::Int(patterns_syntax.len().into(), get_usize_ty(ctx.db))
                            .intern(ctx.db),
                    },
                )
            };
            match inference.conform_ty(ty, tuple_like_ty.intern(ctx.db)) {
                Ok(_) => {}
                Err(_) => unreachable!("As the type is a var, conforming should always succeed."),
            }
            inner_tys
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
    let (n_snapshots, long_ty) = finalized_snapshot_peeled_ty(ctx, ty, pattern)?;

    // Check that type is an enum, and get the concrete enum from it.
    let concrete_enum = try_extract_matches!(long_ty, TypeLongId::Concrete)
        .and_then(|c| try_extract_matches!(c, ConcreteTypeId::Enum))
        .ok_or(())
        .or_else(|_| {
            // Don't add a diagnostic if the type is missing.
            // A diagnostic should've already been added.
            ty.check_not_missing(ctx.db)?;
            Err(ctx.diagnostics.report(pattern, UnexpectedEnumPattern(ty)))
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
        None => LocalVarLongId(ctx.resolver.module_file_id, identifier.stable_ptr()).intern(ctx.db),
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
    let ty = resolve_type_with_environment(
        db,
        ctx.diagnostics,
        &mut ctx.resolver,
        &ast::Expr::Path(path.clone()),
        Some(&mut ctx.environment),
    );
    ty.check_not_missing(db)?;

    let concrete_struct_id = try_extract_matches!(ty.lookup_intern(ctx.db), TypeLongId::Concrete)
        .and_then(|c| try_extract_matches!(c, ConcreteTypeId::Struct))
        .ok_or_else(|| ctx.diagnostics.report(&path, NotAStruct))?;

    if ty.is_phantom(db) {
        ctx.diagnostics.report(ctor_syntax, CannotCreateInstancesOfPhantomTypes);
    }

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
                        ExprAndId { expr: expr.clone(), id: ctx.arenas.exprs.alloc(expr) }
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
                let inference = &mut ctx.resolver.inference();
                if inference
                    .conform_ty_for_diag(
                        arg_expr.ty(),
                        member.ty,
                        ctx.diagnostics,
                        || arg_identifier.stable_ptr().untyped(),
                        |actual_ty, expected_ty| WrongArgumentType { expected_ty, actual_ty },
                    )
                    .is_err()
                {
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
                let inference = &mut ctx.resolver.inference();
                if inference
                    .conform_ty_for_diag(
                        base_struct_expr.ty(),
                        ty,
                        ctx.diagnostics,
                        || base_struct_syntax.expression(syntax_db).stable_ptr().untyped(),
                        |actual_ty, expected_ty| WrongArgumentType { expected_ty, actual_ty },
                    )
                    .is_err()
                {
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
                ctx.diagnostics.report(ctor_syntax, MissingMember(member_name.clone()));
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
        ty: TypeLongId::Concrete(ConcreteTypeId::Struct(concrete_struct_id)).intern(db),
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
    if let Some(ty_str) = ty {
        // Requires specific blocking as `NonZero` now has NumericLiteral support.
        if ty_str == "NonZero" {
            return Err(ctx.diagnostics.report(
                stable_ptr.untyped(),
                SemanticDiagnosticKind::WrongNumberOfArguments { expected: 1, actual: 0 },
            ));
        }
        let ty = try_get_core_ty_by_name(ctx.db, ty_str.into(), vec![])
            .map_err(|err| ctx.diagnostics.report(stable_ptr.untyped(), err))?;
        if let Err(err) = validate_literal(ctx.db, ty, &value) {
            ctx.diagnostics.report(stable_ptr, SemanticDiagnosticKind::LiteralError(err));
        }
        return Ok(ExprLiteral { value, ty, stable_ptr });
    };
    let ty = ctx.resolver.inference().new_type_var(Some(stable_ptr.untyped()));

    // Numeric trait.
    let trait_id = ctx.db.core_info().numeric_literal_trt;
    let generic_args = vec![GenericArgumentId::Type(ty)];
    let concrete_trait_id = semantic::ConcreteTraitLongId { trait_id, generic_args }.intern(ctx.db);
    let lookup_context = ctx.resolver.impl_lookup_context();
    let inference = &mut ctx.resolver.inference();
    inference.new_impl_var(concrete_trait_id, Some(stable_ptr.untyped()), lookup_context);

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

    let trait_id = ctx.db.core_info().string_literal_trt;
    let generic_args = vec![GenericArgumentId::Type(ty)];
    let concrete_trait_id = semantic::ConcreteTraitLongId { trait_id, generic_args }.intern(ctx.db);
    let lookup_context = ctx.resolver.impl_lookup_context();
    let inference = &mut ctx.resolver.inference();
    inference.new_impl_var(concrete_trait_id, Some(stable_ptr.untyped()), lookup_context);

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
fn traits_in_context(
    ctx: &mut ComputationContext<'_>,
) -> Maybe<OrderedHashMap<TraitId, LookupItemId>> {
    let mut traits =
        ctx.db.module_usable_trait_ids(ctx.resolver.prelude_submodule())?.deref().clone();
    traits.extend(
        ctx.db
            .module_usable_trait_ids(ctx.resolver.module_file_id.0)?
            .iter()
            .map(|(k, v)| (*k, *v)),
    );
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
            candidate_traits
                .insert(trait_id, LookupItemId::ModuleItem(ModuleItemId::Trait(trait_id)));
        }
    }

    // Extracting the possible traits that should be imported, in order to use the method.
    let module_file_id = ctx.resolver.module_file_id;
    let lookup_context = ctx.resolver.impl_lookup_context();
    let lexpr_clone = lexpr.clone();
    let db = ctx.db;
    let (function_id, actual_trait_id, fixed_lexpr, mutability) =
        compute_method_function_call_data(
            ctx,
            candidate_traits.keys().copied().collect_vec().as_slice(),
            func_name.clone(),
            lexpr,
            path.stable_ptr().untyped(),
            generic_args_syntax,
            |ty, method_name, inference_errors| {
                let relevant_traits = if !inference_errors.is_empty() {
                    vec![]
                } else {
                    match_method_to_traits(
                        db,
                        ty,
                        &method_name,
                        lookup_context.clone(),
                        module_file_id,
                        lexpr_clone.stable_ptr().untyped(),
                    )
                };
                Some(CannotCallMethod { ty, method_name, inference_errors, relevant_traits })
            },
            |_, trait_function_id0, trait_function_id1| {
                Some(AmbiguousTrait { trait_function_id0, trait_function_id1 })
            },
        )?;

    if let Ok(trait_definition_data) = ctx.db.priv_trait_definition_data(actual_trait_id) {
        if let Some(trait_item_info) = trait_definition_data.get_trait_item_info(&func_name) {
            ctx.resolver.validate_feature_constraints(
                ctx.diagnostics,
                &segment.identifier_ast(db.upcast()),
                &trait_item_info,
            );
        }
    }
    ctx.resolver.data.used_items.insert(candidate_traits[&actual_trait_id]);
    ctx.resolver.data.resolved_items.mark_concrete(
        ctx.db,
        &segment,
        ResolvedConcreteItem::Function(function_id),
    );

    // Note there may be n+1 arguments for n parameters, if the last one is a coupon.
    let mut args_iter =
        expr.arguments(syntax_db).arguments(syntax_db).elements(syntax_db).into_iter();
    // Self argument.
    let mut named_args = vec![NamedArg(fixed_lexpr, None, mutability)];
    // Other arguments.
    let closure_params: OrderedHashMap<TypeId, TypeId> =
        concrete_function_closure_params(ctx.db, function_id)?;
    for ty in function_parameter_types(ctx, function_id)?.skip(1) {
        let Some(arg_syntax) = args_iter.next() else {
            break;
        };
        named_args.push(compute_named_argument_clause(
            ctx,
            arg_syntax,
            closure_params.get(&ty).copied(),
        ));
    }

    // Maybe coupon
    if let Some(arg_syntax) = args_iter.next() {
        named_args.push(compute_named_argument_clause(ctx, arg_syntax, None));
    }

    expr_function_call(ctx, function_id, named_args, &expr, stable_ptr)
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
    let (n_snapshots, long_ty) = finalized_snapshot_peeled_ty(ctx, lexpr.ty(), &rhs_syntax)?;

    match &long_ty {
        TypeLongId::Concrete(_) | TypeLongId::Tuple(_) | TypeLongId::FixedSizeArray { .. } => {
            let Some(EnrichedTypeMemberAccess { member, deref_functions }) =
                get_enriched_type_member_access(ctx, lexpr.clone(), stable_ptr, &member_name)?
            else {
                return Err(ctx.diagnostics.report(
                    &rhs_syntax,
                    NoSuchTypeMember { ty: long_ty.intern(ctx.db), member_name },
                ));
            };
            check_struct_member_is_visible(
                ctx,
                &member,
                rhs_syntax.stable_ptr().untyped(),
                &member_name,
            );
            let member_path = match &long_ty {
                TypeLongId::Concrete(ConcreteTypeId::Struct(concrete_struct_id))
                    if n_snapshots == 0 && deref_functions.is_empty() =>
                {
                    lexpr.as_member_path().map(|parent| ExprVarMemberPath::Member {
                        parent: Box::new(parent),
                        member_id: member.id,
                        stable_ptr,
                        concrete_struct_id: *concrete_struct_id,
                        ty: member.ty,
                    })
                }
                _ => None,
            };
            let mut derefed_expr: ExprAndId = lexpr;
            for (deref_function, mutability) in &deref_functions {
                let cur_expr = expr_function_call(
                    ctx,
                    *deref_function,
                    vec![NamedArg(derefed_expr, None, *mutability)],
                    stable_ptr,
                    stable_ptr,
                )
                .unwrap();

                derefed_expr =
                    ExprAndId { expr: cur_expr.clone(), id: ctx.arenas.exprs.alloc(cur_expr) };
            }
            let (_, long_ty) = finalized_snapshot_peeled_ty(ctx, derefed_expr.ty(), &rhs_syntax)?;
            let derefed_expr_concrete_struct_id = match long_ty {
                TypeLongId::Concrete(ConcreteTypeId::Struct(concrete_struct_id)) => {
                    concrete_struct_id
                }
                _ => unreachable!(),
            };
            let ty = if !deref_functions.is_empty() {
                member.ty
            } else {
                wrap_in_snapshots(ctx.db, member.ty, n_snapshots)
            };
            Ok(Expr::MemberAccess(ExprMemberAccess {
                expr: derefed_expr.id,
                concrete_struct_id: derefed_expr_concrete_struct_id,
                member: member.id,
                ty,
                member_path,
                n_snapshots,
                stable_ptr,
            }))
        }

        TypeLongId::Snapshot(_) => {
            // TODO(spapini): Handle snapshot members.
            Err(ctx.diagnostics.report(&rhs_syntax, Unsupported))
        }
        TypeLongId::Closure(_) => Err(ctx.diagnostics.report(&rhs_syntax, Unsupported)),
        TypeLongId::ImplType(impl_type_id) => {
            unreachable!(
                "Impl type should've been reduced {:?}.",
                impl_type_id.debug(ctx.db.elongate())
            )
        }
        TypeLongId::Var(_) => Err(ctx.diagnostics.report(
            &rhs_syntax,
            InternalInferenceError(InferenceError::TypeNotInferred(long_ty.intern(ctx.db))),
        )),
        TypeLongId::GenericParameter(_) | TypeLongId::Coupon(_) => Err(ctx
            .diagnostics
            .report(&rhs_syntax, TypeHasNoMembers { ty: long_ty.intern(ctx.db), member_name })),
        TypeLongId::Missing(diag_added) => Err(*diag_added),
    }
}

/// Returns the member and the deref operations needed for its access.
///
/// Enriched members include both direct members (in case of a struct), and members of derefed types
/// if the type implements the Deref trait into a struct.
fn get_enriched_type_member_access(
    ctx: &mut ComputationContext<'_>,
    expr: ExprAndId,
    stable_ptr: ast::ExprPtr,
    accessed_member_name: &str,
) -> Maybe<Option<EnrichedTypeMemberAccess>> {
    // Run solver to get as much info on the type as possible.
    // Ignore the result of the `solve()` call - the error, if any, will be
    // reported later.
    ctx.resolver.inference().solve().ok();
    let ty = ctx.reduce_ty(expr.ty());
    let base_var = match &expr.expr {
        Expr::Var(expr_var) => Some(expr_var.var),
        Expr::MemberAccess(ExprMemberAccess { member_path: Some(member_path), .. }) => {
            Some(member_path.base_var())
        }
        _ => None,
    };
    let is_mut_var = base_var
        .filter(|var_id| matches!(ctx.semantic_defs.get(var_id), Some(var) if var.is_mut()))
        .is_some();
    let key = (ty, is_mut_var);
    let mut enriched_members = match ctx.resolver.type_enriched_members.entry(key) {
        Entry::Occupied(entry) => {
            let e = entry.get();
            match e.get_member(accessed_member_name) {
                Some(value) => return Ok(Some(value)),
                None => {
                    if e.deref_chain.len() == e.explored_derefs {
                        // There's no further exploration to be done, and member was not found.
                        return Ok(None);
                    }
                }
            }
            // Moving out of the map to call `enrich_members` and insert back with updated value.
            entry.swap_remove()
        }
        Entry::Vacant(_) => {
            let (_, long_ty) = finalized_snapshot_peeled_ty(ctx, ty, stable_ptr)?;
            let members =
                if let TypeLongId::Concrete(ConcreteTypeId::Struct(concrete_struct_id)) = long_ty {
                    let members = ctx.db.concrete_struct_members(concrete_struct_id)?;
                    if let Some(member) = members.get(accessed_member_name) {
                        // Found direct member access - so directly returning it.
                        return Ok(Some(EnrichedTypeMemberAccess {
                            member: member.clone(),
                            deref_functions: vec![],
                        }));
                    }
                    members.iter().map(|(k, v)| (k.clone(), (v.clone(), 0))).collect()
                } else {
                    Default::default()
                };

            EnrichedMembers {
                members,
                deref_chain: ctx.db.deref_chain(ty, is_mut_var)?.derefs,
                explored_derefs: 0,
            }
        }
    };
    enrich_members(ctx, &mut enriched_members, stable_ptr, accessed_member_name)?;
    let e = ctx.resolver.type_enriched_members.entry(key).or_insert(enriched_members);
    Ok(e.get_member(accessed_member_name))
}

/// Enriches the `enriched_members` with members from "deref"s of the current type.
///
/// The function will stop enriching if it encounters a cycle in the deref chain, or if the
/// requested member is found.
fn enrich_members(
    ctx: &mut ComputationContext<'_>,
    enriched_members: &mut EnrichedMembers,
    stable_ptr: ast::ExprPtr,
    accessed_member_name: &str,
) -> Maybe<()> {
    let EnrichedMembers { members: enriched, deref_chain, explored_derefs } = enriched_members;

    // Add members of derefed types.
    for deref_info in deref_chain.iter().skip(*explored_derefs).cloned() {
        *explored_derefs += 1;
        let (_, long_ty) = finalized_snapshot_peeled_ty(ctx, deref_info.target_ty, stable_ptr)?;
        if let TypeLongId::Concrete(ConcreteTypeId::Struct(concrete_struct_id)) = long_ty {
            let members = ctx.db.concrete_struct_members(concrete_struct_id)?;
            for (member_name, member) in members.iter() {
                // Insert member if there is not already a member with the same name.
                enriched
                    .entry(member_name.clone())
                    .or_insert_with(|| (member.clone(), *explored_derefs));
            }
            // If member is contained we can stop the calculation post the lookup.
            if members.contains_key(accessed_member_name) {
                // Found member, so exploration isn't done.
                break;
            }
        }
    }
    Ok(())
}

/// Peels snapshots from a type and making sure it is fully not a variable type.
fn finalized_snapshot_peeled_ty(
    ctx: &mut ComputationContext<'_>,
    ty: TypeId,
    stable_ptr: impl Into<SyntaxStablePtrId>,
) -> Maybe<(usize, TypeLongId)> {
    let ty = ctx.reduce_ty(ty);
    let (base_snapshots, mut long_ty) = peel_snapshots(ctx.db, ty);
    if let TypeLongId::ImplType(impl_type_id) = long_ty {
        let inference = &mut ctx.resolver.inference();
        let Ok(ty) = inference.reduce_impl_ty(impl_type_id) else {
            return Err(ctx
                .diagnostics
                .report(stable_ptr, InternalInferenceError(InferenceError::TypeNotInferred(ty))));
        };
        long_ty = ty.lookup_intern(ctx.db);
    }
    if matches!(long_ty, TypeLongId::Var(_)) {
        // Save some work. ignore the result. The error, if any, will be reported later.
        ctx.resolver.inference().solve().ok();
        long_ty = ctx.resolver.inference().rewrite(long_ty).no_err();
    }
    let (additional_snapshots, long_ty) = peel_snapshots_ex(ctx.db, long_ty);
    Ok((base_snapshots + additional_snapshots, long_ty))
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
        if let Some(res) = get_binded_expr_by_name(ctx, &variable_name, path.stable_ptr().into()) {
            match res.clone() {
                Expr::Var(expr_var) => {
                    let item = ResolvedGenericItem::Variable(expr_var.var);
                    ctx.resolver.data.resolved_items.generic.insert(identifier.stable_ptr(), item);
                }
                Expr::Constant(expr_const) => {
                    let item = ResolvedConcreteItem::Constant(expr_const.const_value_id);
                    ctx.resolver.data.resolved_items.concrete.insert(identifier.stable_ptr(), item);
                }
                _ => unreachable!(
                    "get_binded_expr_by_name should only return variables or constants"
                ),
            };
            return Ok(res);
        }
    }

    let resolved_item: ResolvedConcreteItem = ctx.resolver.resolve_concrete_path_ex(
        ctx.diagnostics,
        path,
        NotFoundItemType::Identifier,
        Some(&mut ctx.environment),
    )?;

    match resolved_item {
        ResolvedConcreteItem::Constant(const_value_id) => Ok(Expr::Constant(ExprConstant {
            const_value_id,
            ty: const_value_id.ty(db)?,
            stable_ptr: path.stable_ptr().into(),
        })),

        ResolvedConcreteItem::Variant(variant) if variant.ty == unit_ty(db) => {
            let stable_ptr = path.stable_ptr().into();
            let concrete_enum_id = variant.concrete_enum_id;
            Ok(semantic::Expr::EnumVariantCtor(semantic::ExprEnumVariantCtor {
                variant,
                value_expr: unit_expr(ctx, stable_ptr),
                ty: TypeLongId::Concrete(ConcreteTypeId::Enum(concrete_enum_id)).intern(db),
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
    let res = get_binded_expr_by_name(ctx, &variable_name, stable_ptr)
        .ok_or_else(|| ctx.diagnostics.report(identifier, VariableNotFound(variable_name)))?;
    let item = ResolvedGenericItem::Variable(extract_matches!(&res, Expr::Var).var);
    ctx.resolver.data.resolved_items.generic.insert(identifier.stable_ptr(), item);
    Ok(res)
}

/// Returns the requested variable from the environment if it exists. Returns None otherwise.
pub fn get_binded_expr_by_name(
    ctx: &mut ComputationContext<'_>,
    variable_name: &SmolStr,
    stable_ptr: ast::ExprPtr,
) -> Option<Expr> {
    let mut maybe_env = Some(&mut *ctx.environment);
    while let Some(env) = maybe_env {
        if let Some(var) = env.variables.get(variable_name) {
            env.used_variables.insert(var.id());
            return match var {
                Binding::LocalItem(local_const) => match local_const.kind.clone() {
                    crate::StatementItemKind::Constant(const_value_id, ty) => {
                        Some(Expr::Constant(ExprConstant { const_value_id, ty, stable_ptr }))
                    }
                },
                Binding::LocalVar(_) | Binding::Param(_) => {
                    Some(Expr::Var(ExprVar { var: var.id(), ty: var.ty(), stable_ptr }))
                }
            };
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
    call_ptr: impl Into<SyntaxStablePtrId>,
    stable_ptr: ast::ExprPtr,
) -> Maybe<Expr> {
    let coupon_arg = maybe_pop_coupon_argument(ctx, &mut named_args, function_id);

    let signature = ctx.db.concrete_function_signature(function_id)?;
    let signature = ctx.resolver.inference().rewrite(signature).unwrap();

    // TODO(spapini): Better location for these diagnostics after the refactor for generics resolve.
    if named_args.len() != signature.params.len() {
        return Err(ctx.diagnostics.report(
            call_ptr,
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
        if !arg_typ.is_missing(ctx.db) {
            let inference = &mut ctx.resolver.inference();
            let _ = inference.conform_ty_for_diag(
                arg_typ,
                param_typ,
                ctx.diagnostics,
                || arg.stable_ptr().untyped(),
                |actual_ty, expected_ty| WrongArgumentType { expected_ty, actual_ty },
            );
        }

        args.push(if param.mutability == Mutability::Reference {
            // Verify the argument is a variable.
            let Some(ref_arg) = arg.as_member_path() else {
                return Err(ctx.diagnostics.report(arg.deref(), RefArgNotAVariable));
            };
            // Verify the variable argument is mutable.
            if !ctx.semantic_defs[&ref_arg.base_var()].is_mut() {
                ctx.diagnostics.report(arg.deref(), RefArgNotMutable);
            }
            // Verify that it is passed explicitly as 'ref'.
            if mutability != Mutability::Reference {
                ctx.diagnostics.report(arg.deref(), RefArgNotExplicit);
            }
            ExprFunctionCallArg::Reference(ref_arg)
        } else {
            // Verify that it is passed without modifiers.
            if mutability != Mutability::Immutable {
                ctx.diagnostics.report(arg.deref(), ImmutableArgWithModifiers);
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
    if signature.panicable && has_panic_incompatibility(ctx) {
        // TODO(spapini): Delay this check until after inference, to allow resolving specific
        //   impls first.
        return Err(ctx.diagnostics.report(call_ptr, PanicableFromNonPanicable));
    }
    Ok(Expr::FunctionCall(expr_function_call))
}

/// Checks if the last item in `named_args`, has the argument name `__coupon__`, and removes and
/// returns it if so.
fn maybe_pop_coupon_argument(
    ctx: &mut ComputationContext<'_>,
    named_args: &mut Vec<NamedArg>,
    function_id: FunctionId,
) -> Option<id_arena::Id<Expr>> {
    let mut coupon_arg: Option<ExprId> = None;
    if let Some(NamedArg(arg, Some(name_terminal), mutability)) = named_args.last() {
        let coupons_enabled = are_coupons_enabled(ctx.db, ctx.resolver.module_file_id);
        if name_terminal.text(ctx.db.upcast()) == "__coupon__" && coupons_enabled {
            // Check that the argument type is correct.
            let expected_ty = TypeLongId::Coupon(function_id).intern(ctx.db);
            let arg_typ = arg.ty();
            if !arg_typ.is_missing(ctx.db) {
                let inference = &mut ctx.resolver.inference();
                let _ = inference.conform_ty_for_diag(
                    arg_typ,
                    expected_ty,
                    ctx.diagnostics,
                    || arg.stable_ptr().untyped(),
                    |actual_ty, expected_ty| WrongArgumentType { expected_ty, actual_ty },
                );
            }

            // Check that the argument is not mutable/reference.
            if *mutability != Mutability::Immutable {
                ctx.diagnostics.report(arg.deref(), CouponArgumentNoModifiers);
            }

            coupon_arg = Some(arg.id);

            // Remove the __coupon__ argument from the argument list.
            named_args.pop();
        }
    }
    coupon_arg
}

/// Checks if a panicable function is called from a disallowed context.
fn has_panic_incompatibility(ctx: &mut ComputationContext<'_>) -> bool {
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
                res = Err(ctx.diagnostics.report(
                    name_terminal,
                    NamedArgumentMismatch { expected: param.name.clone(), found: name },
                ));
            }
        } else if seen_named_arguments && !reported_unnamed_argument_follows_named {
            reported_unnamed_argument_follows_named = true;
            res = Err(ctx.diagnostics.report(arg.deref(), UnnamedArgumentFollowsNamed));
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

    let crate_id = ctx.resolver.owning_crate_id;

    // As for now, statement attributes does not have any semantic affect, so we only validate they
    // are allowed.
    validate_statement_attributes(ctx, &syntax);
    let feature_restore = ctx
        .resolver
        .data
        .feature_config
        .override_with(extract_item_feature_config(db, crate_id, &syntax, ctx.diagnostics));
    let statement = match &syntax {
        ast::Statement::Let(let_syntax) => {
            let rhs_syntax = &let_syntax.rhs(syntax_db);
            let (rhs_expr, ty) = match let_syntax.type_clause(syntax_db) {
                ast::OptionTypeClause::Empty(_) => {
                    let rhs_expr = compute_expr_semantic(ctx, rhs_syntax);
                    let inferred_type = rhs_expr.ty();
                    (rhs_expr, inferred_type)
                }
                ast::OptionTypeClause::TypeClause(type_clause) => {
                    let var_type_path = type_clause.ty(syntax_db);
                    let explicit_type = resolve_type_with_environment(
                        db,
                        ctx.diagnostics,
                        &mut ctx.resolver,
                        &var_type_path,
                        Some(&mut ctx.environment),
                    );

                    let rhs_expr = compute_expr_semantic(ctx, rhs_syntax);
                    let inferred_type = ctx.reduce_ty(rhs_expr.ty());
                    if !inferred_type.is_missing(db) {
                        let inference = &mut ctx.resolver.inference();
                        let _ = inference.conform_ty_for_diag(
                            inferred_type,
                            explicit_type,
                            ctx.diagnostics,
                            || rhs_syntax.into(),
                            |actual_ty, expected_ty| WrongArgumentType { expected_ty, actual_ty },
                        );
                    }
                    (rhs_expr, explicit_type)
                }
            };
            let rhs_expr_id = rhs_expr.id;

            let pattern = compute_pattern_semantic(
                ctx,
                &let_syntax.pattern(syntax_db),
                ty,
                &mut UnorderedHashMap::default(),
            );
            let variables = pattern.variables(&ctx.arenas.patterns);
            for v in variables {
                let var_def = Binding::LocalVar(v.var.clone());
                if let Some(old_var) =
                    ctx.environment.variables.insert(v.name.clone(), var_def.clone())
                {
                    if matches!(old_var, Binding::LocalItem(_)) {
                        return Err(ctx
                            .diagnostics
                            .report(v.stable_ptr, MultipleDefinitionforBinding(v.name.clone())));
                    }
                    ctx.add_unused_binding_warning(&v.name, &old_var);
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
                ast::Expr::Block(_)
                    | ast::Expr::If(_)
                    | ast::Expr::Match(_)
                    | ast::Expr::Loop(_)
                    | ast::Expr::While(_)
                    | ast::Expr::For(_)
            ) {
                // Point to after the expression, where the semicolon is missing.
                ctx.diagnostics.report_after(&expr_syntax, MissingSemicolon);
            }
            let ty: TypeId = expr.ty();
            if let TypeLongId::Concrete(concrete) = ty.lookup_intern(db) {
                if concrete.is_must_use(db)? {
                    ctx.diagnostics.report(&expr_syntax, UnhandledMustUseType(ty));
                }
            }
            if let Expr::FunctionCall(expr_function_call) = &expr.expr {
                let generic_function_id =
                    expr_function_call.function.lookup_intern(db).function.generic_function;
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
            if !ctx.is_inside_loop() {
                return Err(ctx
                    .diagnostics
                    .report(continue_syntax, ContinueOnlyAllowedInsideALoop));
            }
            semantic::Statement::Continue(semantic::StatementContinue {
                stable_ptr: syntax.stable_ptr(),
            })
        }
        ast::Statement::Return(return_syntax) => {
            if ctx.is_inside_loop() {
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
            let expected_ty = match ctx.inner_ctx {
                None => {
                    ctx.get_signature(
                        return_syntax.into(),
                        UnsupportedOutsideOfFunctionFeatureName::ReturnStatement,
                    )?
                    .return_type
                }
                Some(InnerContext::Closure { return_type }) => return_type,
                _ => unreachable!("Return statement inside a loop"),
            };

            let expected_ty = ctx.reduce_ty(expected_ty);
            let expr_ty = ctx.reduce_ty(expr_ty);
            if !expected_ty.is_missing(db) && !expr_ty.is_missing(db) {
                let inference = &mut ctx.resolver.inference();
                let _ = inference.conform_ty_for_diag(
                    expr_ty,
                    expected_ty,
                    ctx.diagnostics,
                    || stable_ptr,
                    |actual_ty, expected_ty| WrongReturnType { expected_ty, actual_ty },
                );
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
            match &mut ctx.inner_ctx {
                None | Some(InnerContext::Closure { .. }) => {
                    return Err(ctx.diagnostics.report(break_syntax, BreakOnlyAllowedInsideALoop));
                }
                Some(InnerContext::Loop { type_merger, .. }) => {
                    type_merger.try_merge_types(
                        ctx.db,
                        ctx.diagnostics,
                        &mut ctx.resolver.inference(),
                        ty,
                        stable_ptr,
                    );
                }
                Some(InnerContext::While | InnerContext::For) => {
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
        ast::Statement::Item(stmt_item_syntax) => {
            let item_syntax = &stmt_item_syntax.item(syntax_db);
            match item_syntax {
                ast::ModuleItem::Constant(const_syntax) => {
                    let lhs = const_syntax.type_clause(db.upcast()).ty(db.upcast());
                    let rhs = const_syntax.value(db.upcast());
                    let rhs_expr = compute_expr_semantic(ctx, &rhs);
                    let explicit_type = resolve_type_with_environment(
                        db,
                        ctx.diagnostics,
                        &mut ctx.resolver,
                        &lhs,
                        Some(&mut ctx.environment),
                    );
                    let rhs_resolved_expr = resolve_const_expr_and_evaluate(
                        db,
                        ctx,
                        &rhs_expr,
                        stmt_item_syntax.stable_ptr().untyped(),
                        explicit_type,
                        false,
                    );
                    let name_syntax = const_syntax.name(syntax_db);
                    let name = name_syntax.text(db.upcast());
                    let rhs_id = StatementConstLongId(
                        ctx.resolver.module_file_id,
                        const_syntax.stable_ptr(),
                    );
                    let var_def = Binding::LocalItem(LocalItem {
                        id: StatementItemId::Constant(rhs_id.intern(db)),
                        kind: StatementItemKind::Constant(
                            db.intern_const_value(rhs_resolved_expr.clone()),
                            rhs_resolved_expr.ty(db.upcast())?,
                        ),
                    });
                    add_item_to_statement_environment(ctx, name, var_def, &name_syntax);
                }
                ast::ModuleItem::Use(use_syntax) => {
                    for leaf in get_all_path_leaves(syntax_db, use_syntax) {
                        let stable_ptr = leaf.stable_ptr();
                        let segments = get_use_path_segments(syntax_db, ast::UsePath::Leaf(leaf))?;
                        let resolved_item = ctx.resolver.resolve_generic_path(
                            ctx.diagnostics,
                            segments,
                            NotFoundItemType::Identifier,
                            Some(&mut ctx.environment),
                        )?;
                        let var_def_id = StatementItemId::Use(
                            StatementUseLongId(ctx.resolver.module_file_id, stable_ptr).intern(db),
                        );
                        let name = var_def_id.name(db.upcast());
                        match resolved_item {
                            ResolvedGenericItem::GenericConstant(const_id) => {
                                let var_def = Binding::LocalItem(LocalItem {
                                    id: var_def_id,
                                    kind: StatementItemKind::Constant(
                                        db.constant_const_value(const_id)?,
                                        db.constant_const_type(const_id)?,
                                    ),
                                });
                                add_item_to_statement_environment(ctx, name, var_def, stable_ptr);
                            }
                            ResolvedGenericItem::GenericType(generic_type_id) => {
                                add_type_to_statement_environment(
                                    ctx,
                                    name,
                                    ResolvedGenericItem::GenericType(generic_type_id),
                                    stable_ptr,
                                );
                            }
                            ResolvedGenericItem::Module(_)
                            | ResolvedGenericItem::GenericFunction(_)
                            | ResolvedGenericItem::GenericTypeAlias(_)
                            | ResolvedGenericItem::GenericImplAlias(_)
                            | ResolvedGenericItem::Variant(_)
                            | ResolvedGenericItem::Trait(_)
                            | ResolvedGenericItem::Impl(_)
                            | ResolvedGenericItem::Variable(_) => {
                                return Err(ctx
                                    .diagnostics
                                    .report(stable_ptr, UnsupportedUseItemInStatement));
                            }
                        }
                    }
                }
                ast::ModuleItem::Module(_) => {
                    unreachable!("Modules are not supported inside a function.")
                }
                ast::ModuleItem::FreeFunction(_) => {
                    unreachable!("FreeFunction type not supported.")
                }
                ast::ModuleItem::ExternFunction(_) => {
                    unreachable!("ExternFunction type not supported.")
                }
                ast::ModuleItem::ExternType(_) => unreachable!("ExternType type not supported."),
                ast::ModuleItem::Trait(_) => unreachable!("Trait type not supported."),
                ast::ModuleItem::Impl(_) => unreachable!("Impl type not supported."),
                ast::ModuleItem::ImplAlias(_) => unreachable!("ImplAlias type not supported."),
                ast::ModuleItem::Struct(_) => unreachable!("Struct type not supported."),
                ast::ModuleItem::Enum(_) => unreachable!("Enum type not supported."),
                ast::ModuleItem::TypeAlias(_) => unreachable!("TypeAlias type not supported."),
                ast::ModuleItem::InlineMacro(_) => unreachable!("InlineMacro type not supported."),
                ast::ModuleItem::HeaderDoc(_) => unreachable!("HeaderDoc type not supported."),
                ast::ModuleItem::Missing(_) => unreachable!("Missing type not supported."),
            }
            semantic::Statement::Item(semantic::StatementItem { stable_ptr: syntax.stable_ptr() })
        }
        ast::Statement::Missing(_) => todo!(),
    };
    ctx.resolver.data.feature_config.restore(feature_restore);
    Ok(ctx.arenas.statements.alloc(statement))
}

/// Adds an item to the statement environment and reports a diagnostic if the item is already
/// defined.
fn add_item_to_statement_environment(
    ctx: &mut ComputationContext<'_>,
    name: SmolStr,
    var_def: Binding,
    stable_ptr: impl Into<SyntaxStablePtrId>,
) {
    if let Some(old_var) = ctx.environment.variables.insert(name.clone(), var_def.clone()) {
        ctx.diagnostics.report(
            stable_ptr,
            match old_var {
                Binding::LocalItem(_) => MultipleConstantDefinition(name),
                Binding::LocalVar(_) | Binding::Param(_) => MultipleDefinitionforBinding(name),
            },
        );
    }
    ctx.semantic_defs.insert(var_def.id(), var_def);
}

/// Adds a type to the statement environment and reports a diagnostic if the type is already
/// defined.
fn add_type_to_statement_environment(
    ctx: &mut ComputationContext<'_>,
    name: SmolStr,
    resolved_generic_item: ResolvedGenericItem,
    stable_ptr: impl Into<SyntaxStablePtrId> + std::marker::Copy,
) {
    if ctx
        .environment
        .use_items
        .insert(
            name.clone(),
            StatementGenericItemData { resolved_generic_item, stable_ptr: stable_ptr.into() },
        )
        .is_some()
    {
        ctx.diagnostics.report(stable_ptr, MultipleGenericItemDefinition(name));
    }
}

/// Computes the semantic model of an expression and reports diagnostics if the expression does not
/// evaluate to a boolean value.
fn compute_bool_condition_semantic(
    ctx: &mut ComputationContext<'_>,
    condition_syntax: &ast::Expr,
) -> ExprAndId {
    let condition = compute_expr_semantic(ctx, condition_syntax);
    let inference = &mut ctx.resolver.inference();
    let _ = inference.conform_ty_for_diag(
        condition.ty(),
        core_bool_ty(ctx.db),
        ctx.diagnostics,
        || condition.stable_ptr().untyped(),
        |condition_ty, _expected_ty| ConditionNotBool(condition_ty),
    );
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
        ctx.diagnostics.report(stable_ptr, MemberNotVisible(member_name.clone()));
    }
}

/// Verifies that the statement attributes are valid statements attributes, if not a diagnostic is
/// reported.
fn validate_statement_attributes(ctx: &mut ComputationContext<'_>, syntax: &ast::Statement) {
    let allowed_attributes = ctx.db.allowed_statement_attributes();
    let mut diagnostics = vec![];
    validate_attributes_flat(
        ctx.db.upcast(),
        &allowed_attributes,
        &OrderedHashSet::default(),
        syntax,
        &mut diagnostics,
    );
    // Translate the plugin diagnostics to semantic diagnostics.
    for diagnostic in diagnostics {
        ctx.diagnostics
            .report(diagnostic.stable_ptr, SemanticDiagnosticKind::UnknownStatementAttribute);
    }
}

/// Gets an iterator with the types of the parameters of the given function.
fn function_parameter_types(
    ctx: &mut ComputationContext<'_>,
    function: FunctionId,
) -> Maybe<impl Iterator<Item = TypeId>> {
    let signature = ctx.db.concrete_function_signature(function)?;
    let param_types = signature.params.into_iter().map(|param| param.ty);
    Ok(param_types)
}

/// Finds traits which contain a method matching the given name and type.
/// This function checks for visible traits in the specified module file and filters
/// methods based on their association with the given type and method name.
fn match_method_to_traits(
    db: &dyn SemanticGroup,
    ty: semantic::TypeId,
    method_name: &SmolStr,
    lookup_context: ImplLookupContext,
    module_file_id: ModuleFileId,
    stable_ptr: SyntaxStablePtrId,
) -> Vec<String> {
    let visible_traits = db
        .visible_traits_from_module(module_file_id)
        .unwrap_or_else(|| Arc::new(OrderedHashMap::default()));

    visible_traits
        .iter()
        .filter_map(|(trait_id, path)| {
            let mut data = InferenceData::new(InferenceId::NoContext);
            let mut inference = data.inference(db);
            let trait_function =
                db.trait_function_by_name(*trait_id, method_name.clone()).ok()??;
            let (concrete_trait_id, _) = inference.infer_concrete_trait_by_self(
                trait_function,
                ty,
                &lookup_context,
                Some(stable_ptr),
                |_| {},
            )?;
            inference.solve().ok();
            match inference.trait_solution_set(
                concrete_trait_id,
                ImplVarTraitItemMappings::default(),
                lookup_context.clone(),
            ) {
                Ok(SolutionSet::Unique(_) | SolutionSet::Ambiguous(_)) => Some(path.clone()),
                _ => None,
            }
        })
        .collect()
}
