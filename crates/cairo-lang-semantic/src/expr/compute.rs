//! This module is responsible of computing the semantic model of expressions and statements in
//! the code, while type checking.
//! It is invoked by queries for function bodies and other code blocks.

use core::panic;
use std::ops::Deref;
use std::sync::Arc;

use ast::PathSegment;
use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::db::{DefsGroup, get_all_path_leaves, validate_attributes_flat};
use cairo_lang_defs::diagnostic_utils::StableLocation;
use cairo_lang_defs::ids::{
    FunctionTitleId, GenericKind, LanguageElementId, LocalVarLongId, LookupItemId, MemberId,
    ModuleId, ModuleItemId, NamedLanguageElementId, StatementConstLongId, StatementItemId,
    StatementUseLongId, TraitFunctionId, TraitId, VarId,
};
use cairo_lang_defs::plugin::{InlineMacroExprPlugin, MacroPluginMetadata};
use cairo_lang_diagnostics::{DiagnosticNote, Maybe, skip_diagnostic};
use cairo_lang_filesystem::cfg::CfgSet;
use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_filesystem::ids::{
    CodeMapping, CodeOrigin, FileKind, FileLongId, SmolStrId, VirtualFile,
};
use cairo_lang_filesystem::span::TextOffset;
use cairo_lang_parser::db::ParserGroup;
use cairo_lang_proc_macros::DebugWithDb;
use cairo_lang_syntax::attribute::consts::UNUSED_VARIABLES;
use cairo_lang_syntax::node::ast::{
    BinaryOperator, BlockOrIf, ClosureParamWrapper, ConditionListAnd, ExprPtr,
    OptionReturnTypeClause, PatternListOr, PatternStructParam, TerminalIdentifier, UnaryOperator,
};
use cairo_lang_syntax::node::helpers::{GetIdentifier, PathSegmentEx, QueryAttrs};
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{Terminal, TypedStablePtr, TypedSyntaxNode, ast};
use cairo_lang_utils::ordered_hash_map::{Entry, OrderedHashMap};
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use cairo_lang_utils::unordered_hash_set::UnorderedHashSet;
use cairo_lang_utils::{
    self as utils, Intern, OptionHelper, extract_matches, require, try_extract_matches,
};
use itertools::{Itertools, chain, zip_eq};
use num_bigint::BigInt;
use num_traits::ToPrimitive;
use salsa::Database;

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
    CorelibSemantic, core_binary_operator, core_bool_ty, core_unary_operator, false_literal_expr,
    get_usize_ty, never_ty, true_literal_expr, try_get_core_ty_by_name, unit_expr, unit_ty,
    unwrap_error_propagation_type, validate_literal,
};
use crate::diagnostic::SemanticDiagnosticKind::{self, *};
use crate::diagnostic::{
    ElementKind, MultiArmExprKind, NotFoundItemType, SemanticDiagnostics,
    SemanticDiagnosticsBuilder, TraitInferenceErrors, UnsupportedOutsideOfFunctionFeatureName,
};
use crate::expr::inference::solver::SolutionSet;
use crate::expr::inference::{ImplVarTraitItemMappings, InferenceId};
use crate::items::constant::{
    ConstValue, ConstantSemantic, resolve_const_expr_and_evaluate, validate_const_expr,
};
use crate::items::enm::{EnumSemantic, SemanticEnumEx};
use crate::items::feature_kind::{FeatureConfig, FeatureConfigRestore};
use crate::items::functions::{FunctionsSemantic, function_signature_params};
use crate::items::generics::GenericParamSemantic;
use crate::items::imp::{
    DerefInfo, ImplLookupContextId, ImplSemantic, filter_candidate_traits, infer_impl_by_self,
};
use crate::items::macro_declaration::{
    MacroDeclarationSemantic, MatcherContext, expand_macro_rule, is_macro_rule_match,
};
use crate::items::modifiers::compute_mutability;
use crate::items::module::ModuleSemantic;
use crate::items::structure::StructSemantic;
use crate::items::trt::TraitSemantic;
use crate::items::visibility;
use crate::keyword::MACRO_CALL_SITE;
use crate::lsp_helpers::LspHelpers;
use crate::resolve::{
    AsSegments, EnrichedMembers, EnrichedTypeMemberAccess, ResolutionContext, ResolvedConcreteItem,
    ResolvedGenericItem, Resolver, ResolverMacroData,
};
use crate::semantic::{self, Binding, FunctionId, LocalVariable, TypeId, TypeLongId};
use crate::substitution::SemanticRewriter;
use crate::types::{
    ClosureTypeLongId, ConcreteTypeId, add_type_based_diagnostics, are_coupons_enabled,
    extract_fixed_size_array_size, peel_snapshots, peel_snapshots_ex, resolve_type_ex,
    verify_fixed_size_array_size, wrap_in_snapshots,
};
use crate::usage::Usages;
use crate::{
    ConcreteEnumId, ConcreteVariant, GenericArgumentId, GenericParam, LocalItem, Member,
    Mutability, Parameter, PatternStringLiteral, PatternStruct, Signature, StatementItemKind,
};

/// The information of a macro expansion.
#[derive(Debug, Clone, PartialEq, Eq, salsa::Update)]
struct MacroExpansionInfo<'db> {
    /// The code mappings for this expansion.
    mappings: Arc<[CodeMapping]>,
    /// The kind of macro the expansion is from.
    kind: MacroKind,
    /// The variables that should be exposed to the parent scope after the macro expansion.
    /// This is used for unhygienic macros.
    vars_to_expose: Vec<(SmolStrId<'db>, Binding<'db>)>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ExpansionOffset(TextOffset);
impl ExpansionOffset {
    /// Creates a new origin offset.
    pub fn new(offset: TextOffset) -> Self {
        Self(offset)
    }
    /// Returns the origin of the position that was expanded at the given offset, if any.
    pub fn mapped(self, mappings: &[CodeMapping]) -> Option<Self> {
        let mapping = mappings
            .iter()
            .find(|mapping| mapping.span.start <= self.0 && self.0 <= mapping.span.end)?;
        Some(Self::new(match mapping.origin {
            CodeOrigin::Start(offset) => offset.add_width(self.0 - mapping.span.start),
            CodeOrigin::Span(span) | CodeOrigin::CallSite(span) => span.start,
        }))
    }
}

/// Describes the origin and hygiene behavior of a macro expansion.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MacroKind {
    /// A user-defined macro, expanded with standard hygiene.
    UserDefined,
    /// A plugin macro.
    Plugin,
    /// An unhygienic macro, whose variables are injected into the parent scope.
    Unhygienic,
}

/// Expression with its id.
#[derive(Debug, Clone)]
pub struct ExprAndId<'db> {
    pub expr: Expr<'db>,
    pub id: ExprId,
}
impl<'db> Deref for ExprAndId<'db> {
    type Target = Expr<'db>;

    fn deref(&self) -> &Self::Target {
        &self.expr
    }
}

#[derive(Debug, Clone)]
pub struct PatternAndId<'db> {
    pub pattern: Pattern<'db>,
    pub id: PatternId,
}
impl<'db> Deref for PatternAndId<'db> {
    type Target = Pattern<'db>;

    fn deref(&self) -> &Self::Target {
        &self.pattern
    }
}

/// Named argument in a function call.
#[derive(Debug, Clone)]
pub struct NamedArg<'db>(ExprAndId<'db>, Option<ast::TerminalIdentifier<'db>>, Mutability);

pub enum ContextFunction<'db> {
    Global,
    Function(Maybe<FunctionId<'db>>),
}

/// Context inside loops or closures.
#[derive(Debug, Clone)]
struct InnerContext<'db> {
    /// The return type in the current context.
    return_type: TypeId<'db>,
    /// The kind of inner context.
    kind: InnerContextKind<'db>,
}

/// Kinds of inner context.
#[derive(Debug, Clone)]
enum InnerContextKind<'db> {
    /// Context inside a `loop`
    Loop { type_merger: FlowMergeTypeHelper<'db> },
    /// Context inside a `while` loop
    While,
    /// Context inside a `for` loop
    For,
    /// Context inside a `closure`
    Closure,
}

/// The result of expanding an inline macro.
#[derive(Debug, Clone)]
struct InlineMacroExpansion<'db> {
    pub content: Arc<str>,
    pub name: String,
    pub info: MacroExpansionInfo<'db>,
}

/// Context for computing the semantic model of expression trees.
pub struct ComputationContext<'ctx, 'mt> {
    pub db: &'ctx dyn Database,
    pub diagnostics: &'mt mut SemanticDiagnostics<'ctx>,
    pub resolver: &'mt mut Resolver<'ctx>,
    /// Tracks variable definitions and their mutability state.
    pub variable_tracker: VariableTracker<'ctx>,
    signature: Option<&'mt Signature<'ctx>>,
    environment: Box<Environment<'ctx>>,
    /// Arenas of semantic objects.
    pub arenas: Arenas<'ctx>,
    function_id: ContextFunction<'ctx>,
    inner_ctx: Option<InnerContext<'ctx>>,
    cfg_set: Arc<CfgSet>,
    /// Whether to look for closures when calling variables.
    /// TODO(TomerStarkware): Remove this once we disallow calling shadowed functions.
    are_closures_in_context: bool,
    /// Whether variables defined in the current macro scope should be injected into the parent
    /// scope.
    macro_defined_var_unhygienic: bool,
}
impl<'ctx, 'mt> ComputationContext<'ctx, 'mt> {
    /// Creates a new computation context.
    pub fn new(
        db: &'ctx dyn Database,
        diagnostics: &'mt mut SemanticDiagnostics<'ctx>,
        resolver: &'mt mut Resolver<'ctx>,
        signature: Option<&'mt Signature<'ctx>>,
        environment: Environment<'ctx>,
        function_id: ContextFunction<'ctx>,
    ) -> Self {
        let cfg_set =
            Arc::new(resolver.settings.cfg_set.as_ref().unwrap_or_else(|| db.cfg_set()).clone());
        let mut variable_tracker = VariableTracker::default();
        variable_tracker.extend_from_environment(&environment);
        Self {
            db,
            diagnostics,
            resolver,
            signature,
            environment: Box::new(environment),
            arenas: Default::default(),
            function_id,
            variable_tracker,
            inner_ctx: None,
            cfg_set,
            are_closures_in_context: false,
            macro_defined_var_unhygienic: false,
        }
    }

    /// Creates a new computation context for a global scope.
    pub fn new_global(
        db: &'ctx dyn Database,
        diagnostics: &'mt mut SemanticDiagnostics<'ctx>,
        resolver: &'mt mut Resolver<'ctx>,
    ) -> Self {
        Self::new(db, diagnostics, resolver, None, Environment::empty(), ContextFunction::Global)
    }

    /// Inserts a variable into both environment and variable tracker.
    /// Returns the old value if the variable was already defined.
    pub fn insert_variable(
        &mut self,
        name: SmolStrId<'ctx>,
        var_def: Binding<'ctx>,
    ) -> Option<Binding<'ctx>> {
        // Ignore re-definitions in the variable tracker, diagnostics will be issued by the caller
        // using this method's return value.
        let _ = self.variable_tracker.insert(var_def.clone());
        self.environment.variables.insert(name, var_def)
    }

    /// Runs a function with a modified context, with a new environment for a subscope.
    /// This environment holds no variables of its own, but points to the current environment as a
    /// parent. Used for blocks of code that introduce a new scope like function bodies, if
    /// blocks, loops, etc.
    fn run_in_subscope<T, F>(&mut self, f: F) -> T
    where
        F: FnOnce(&mut Self) -> T,
    {
        self.run_in_subscope_ex(f, None)
    }

    /// Similar to run_in_subscope, but for macro expanded code. It creates a new environment
    /// that points to the current environment as a parent, and also contains the macro expansion
    /// data. When looking up variables, we will get out of the macro expansion environment if
    /// and only if the text was originated from expanding a placeholder.
    fn run_in_macro_subscope<T, F>(
        &mut self,
        operation: F,
        macro_info: MacroExpansionInfo<'ctx>,
    ) -> T
    where
        F: FnOnce(&mut Self) -> T,
    {
        let prev_macro_hygiene_kind = self.macro_defined_var_unhygienic;
        let prev_default_module_allowed = self.resolver.default_module_allowed;
        match macro_info.kind {
            MacroKind::Unhygienic => {
                self.macro_defined_var_unhygienic = true;
            }
            MacroKind::Plugin => {
                self.resolver.set_default_module_allowed(true);
            }
            MacroKind::UserDefined => {}
        }
        let result = self.run_in_subscope_ex(operation, Some(macro_info));
        self.macro_defined_var_unhygienic = prev_macro_hygiene_kind;
        self.resolver.set_default_module_allowed(prev_default_module_allowed);
        result
    }
    /// Runs a function with a modified context, with a new environment for a subscope.
    /// Shouldn't be called directly, use [Self::run_in_subscope] or [Self::run_in_macro_subscope]
    /// instead.
    fn run_in_subscope_ex<T, F>(&mut self, f: F, macro_info: Option<MacroExpansionInfo<'ctx>>) -> T
    where
        F: FnOnce(&mut Self) -> T,
    {
        // Push an environment to the stack.
        let parent = std::mem::replace(&mut self.environment, Environment::empty().into());
        self.environment.parent = Some(parent);
        self.environment.macro_info = macro_info;
        let res = f(self);

        // Pop the environment from the stack.
        let parent = self.environment.parent.take().unwrap();
        let mut closed = std::mem::replace(&mut self.environment, parent);
        let parent = &mut self.environment;
        if let Some(macro_info) = closed.macro_info {
            for (name, binding) in macro_info.vars_to_expose {
                // Exposed variables are considered moved in the closed scope, as they still may be
                // used later.
                if !closed.used_variables.insert(binding.id()) {
                    // In case a variable was already marked as used in the closed environment, it
                    // means it was actually used, and is marked as used in the environment it is
                    // moved to.
                    parent.used_variables.insert(binding.id());
                }
                if let Some(old_var) = parent.variables.insert(name, binding.clone()) {
                    add_unused_binding_warning(
                        self.diagnostics,
                        self.db,
                        &parent.used_variables,
                        name,
                        &old_var,
                        &self.resolver.data.feature_config,
                    );
                }
                if let Some(parent_macro_info) = parent.macro_info.as_mut() {
                    parent_macro_info.vars_to_expose.push((name, binding));
                }
            }
        }
        for (name, binding) in closed.variables {
            add_unused_binding_warning(
                self.diagnostics,
                self.db,
                &closed.used_variables,
                name,
                &binding,
                &self.resolver.data.feature_config,
            );
        }
        // Adds warning for unused items if required.
        for (ty_name, statement_ty) in closed.use_items {
            if !closed.used_use_items.contains(&ty_name) && !ty_name.long(self.db).starts_with('_')
            {
                self.diagnostics.report(statement_ty.stable_ptr, UnusedUse);
            }
        }
        res
    }

    /// Returns the return type in the current context if available.
    fn get_return_type(&mut self) -> Option<TypeId<'ctx>> {
        if let Some(inner_ctx) = &self.inner_ctx {
            return Some(inner_ctx.return_type);
        }

        if let Some(signature) = self.signature {
            return Some(signature.return_type);
        }

        None
    }

    fn reduce_ty(&mut self, ty: TypeId<'ctx>) -> TypeId<'ctx> {
        self.resolver.inference().rewrite(ty).no_err()
    }

    /// Applies inference rewriter to all the expressions in the computation context, and adds
    /// errors on types from the final expressions.
    pub fn apply_inference_rewriter_to_exprs(&mut self) {
        let mut analyzed_types = UnorderedHashSet::<_>::default();
        for (_id, expr) in &mut self.arenas.exprs {
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
        for (_id, pattern) in &mut self.arenas.patterns {
            self.resolver.inference().internal_rewrite(pattern).no_err();
        }
        for (_id, stmt) in &mut self.arenas.statements {
            self.resolver.inference().internal_rewrite(stmt).no_err();
        }
    }
    /// Returns whether the current context is inside a loop.
    fn is_inside_loop(&self) -> bool {
        let Some(inner_ctx) = &self.inner_ctx else {
            return false;
        };

        match inner_ctx.kind {
            InnerContextKind::Closure => false,
            InnerContextKind::Loop { .. } | InnerContextKind::While | InnerContextKind::For => true,
        }
    }

    /// Validates the features of the given item, then pushes them into the context.
    /// IMPORTANT: Don't forget to restore through `restore_features`!
    fn add_features_from_statement<Item: QueryAttrs<'ctx> + TypedSyntaxNode<'ctx>>(
        &mut self,
        item: &Item,
    ) -> FeatureConfigRestore<'ctx> {
        validate_statement_attributes(self, item);
        let crate_id = self.resolver.owning_crate_id;
        self.resolver.extend_feature_config_from_item(self.db, crate_id, self.diagnostics, item)
    }

    /// Restores the feature config to its state before [Self::add_features_from_statement],
    /// using the restoration state returned by that method.
    fn restore_features(&mut self, feature_restore: FeatureConfigRestore<'ctx>) {
        self.resolver.restore_feature_config(feature_restore);
    }
}

/// Tracks variable definitions and their mutability state.
#[derive(Debug, Default, PartialEq, Eq)]
pub struct VariableTracker<'ctx> {
    /// Definitions of semantic variables.
    semantic_defs: UnorderedHashMap<semantic::VarId<'ctx>, Binding<'ctx>>,
    /// Mutable variables that have been referenced (lost mutability).
    referenced_mut_vars: UnorderedHashMap<semantic::VarId<'ctx>, SyntaxStablePtrId<'ctx>>,
}

impl<'ctx> VariableTracker<'ctx> {
    /// Extends the variable tracker from the variables present in the environment.
    pub fn extend_from_environment(&mut self, environment: &Environment<'ctx>) {
        self.semantic_defs
            .extend(environment.variables.values().cloned().map(|var| (var.id(), var)));
    }
    /// Inserts a semantic definition for a variable.
    /// Returns the old value if the variable was already defined.
    pub fn insert(&mut self, var: Binding<'ctx>) -> Option<Binding<'ctx>> {
        self.semantic_defs.insert(var.id(), var)
    }

    /// Returns true if the variable is currently mutable.
    /// A variable is mutable only if declared mutable AND not referenced.
    pub fn is_mut(&self, var_id: &semantic::VarId<'ctx>) -> bool {
        self.semantic_defs
            .get(var_id)
            .is_some_and(|def| def.is_mut() && !self.referenced_mut_vars.contains_key(var_id))
    }

    /// Returns true if the expression is a variable or a member access of a mutable variable.
    pub fn is_mut_expr(&self, expr: &ExprAndId<'ctx>) -> bool {
        expr.as_member_path().is_some_and(|var| self.is_mut(&var.base_var()))
    }

    /// Reports a mutability error if the variable cannot be modified.
    pub fn report_var_mutability_error(
        &self,
        db: &'ctx dyn Database,
        diagnostics: &mut SemanticDiagnostics<'ctx>,
        var_id: &semantic::VarId<'ctx>,
        error_ptr: impl Into<SyntaxStablePtrId<'ctx>>,
        immutable_diagnostic: SemanticDiagnosticKind<'ctx>,
    ) {
        let Some(def) = self.semantic_defs.get(var_id) else {
            return;
        };

        if !def.is_mut() {
            diagnostics.report(error_ptr, immutable_diagnostic);
            return;
        }

        if let Some(&referenced_at) = self.referenced_mut_vars.get(var_id) {
            let note = DiagnosticNote::with_location(
                "variable pointer taken here".into(),
                StableLocation::new(referenced_at).span_in_file(db),
            );
            diagnostics.report(error_ptr, AssignmentToReprPtrVariable(vec![note]));
        }
    }

    /// Marks an expression as pointed to if it refers to a mutable variable.
    pub fn mark_referenced(
        &mut self,
        expr: &ExprAndId<'ctx>,
        reference_location: SyntaxStablePtrId<'ctx>,
    ) {
        if let Some(base_var) = expr.as_member_path().map(|path| path.base_var())
            && self.is_mut(&base_var)
        {
            // This insert happens only once, since `is_mut` returns false if already referenced.
            let _ = self.referenced_mut_vars.insert(base_var, reference_location);
        }
    }
}

/// Adds warning for unused bindings if required.
fn add_unused_binding_warning<'db>(
    diagnostics: &mut SemanticDiagnostics<'db>,
    db: &'db dyn Database,
    used_bindings: &UnorderedHashSet<VarId<'db>>,
    name: SmolStrId<'db>,
    binding: &Binding<'db>,
    ctx_feature_config: &FeatureConfig<'db>,
) {
    if !name.long(db).starts_with('_') && !used_bindings.contains(&binding.id()) {
        match binding {
            Binding::LocalItem(local_item) => match local_item.id {
                StatementItemId::Constant(_) => {
                    diagnostics.report(binding.stable_ptr(db), UnusedConstant);
                }
                StatementItemId::Use(_) => {
                    diagnostics.report(binding.stable_ptr(db), UnusedUse);
                }
            },
            Binding::LocalVar(local_var) => {
                if !local_var.allow_unused
                    && !ctx_feature_config
                        .allowed_lints
                        .contains(&SmolStrId::from(db, UNUSED_VARIABLES))
                {
                    diagnostics.report(binding.stable_ptr(db), UnusedVariable);
                }
            }
            Binding::Param(_) => {
                if !ctx_feature_config
                    .allowed_lints
                    .contains(&SmolStrId::from(db, UNUSED_VARIABLES))
                {
                    diagnostics.report(binding.stable_ptr(db), UnusedVariable);
                }
            }
        }
    }
}

// TODO(ilya): Change value to VarId.
pub type EnvVariables<'db> = OrderedHashMap<SmolStrId<'db>, Binding<'db>>;

type EnvItems<'db> = OrderedHashMap<SmolStrId<'db>, StatementGenericItemData<'db>>;

/// Struct that holds the resolved generic type of a statement item.
#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb, salsa::Update)]
#[debug_db(dyn Database)]
struct StatementGenericItemData<'db> {
    resolved_generic_item: ResolvedGenericItem<'db>,
    stable_ptr: SyntaxStablePtrId<'db>,
}

/// A state which contains all the variables defined at the current resolver until now, and a
/// pointer to the parent environment.
#[derive(Clone, Debug, PartialEq, Eq, salsa::Update)]
pub struct Environment<'db> {
    parent: Option<Box<Environment<'db>>>,
    variables: EnvVariables<'db>,
    used_variables: UnorderedHashSet<semantic::VarId<'db>>,
    use_items: EnvItems<'db>,
    used_use_items: UnorderedHashSet<SmolStrId<'db>>,
    /// Information for macro in case the current environment was create by expanding a macro.
    macro_info: Option<MacroExpansionInfo<'db>>,
}
impl<'db> Environment<'db> {
    /// Adds a parameter to the environment.
    pub fn add_param(
        &mut self,
        db: &'db dyn Database,
        diagnostics: &mut SemanticDiagnostics<'db>,
        semantic_param: Parameter<'db>,
        ast_param: &ast::Param<'db>,
        function_title_id: Option<FunctionTitleId<'db>>,
    ) -> Maybe<()> {
        if let utils::ordered_hash_map::Entry::Vacant(entry) =
            self.variables.entry(semantic_param.name)
        {
            entry.insert(Binding::Param(semantic_param));
            Ok(())
        } else {
            Err(diagnostics.report(
                ast_param.stable_ptr(db),
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
            macro_info: None,
        }
    }
}

/// Returns the requested item from the environment if it exists. Returns None otherwise.
pub fn get_statement_item_by_name<'db>(
    env: &mut Environment<'db>,
    item_name: SmolStrId<'db>,
) -> Option<ResolvedGenericItem<'db>> {
    let mut maybe_env = Some(&mut *env);
    while let Some(curr_env) = maybe_env {
        if let Some(var) = curr_env.use_items.get(&item_name) {
            curr_env.used_use_items.insert(item_name);
            return Some(var.resolved_generic_item.clone());
        }
        maybe_env = curr_env.parent.as_deref_mut();
    }
    None
}

/// Computes the semantic model of an expression.
/// Note that this expr will always be "registered" in the arena, so it can be looked up in the
/// language server.
pub fn compute_expr_semantic<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    syntax: &ast::Expr<'db>,
) -> ExprAndId<'db> {
    let expr = maybe_compute_expr_semantic(ctx, syntax);
    let expr = wrap_maybe_with_missing(ctx, expr, syntax.stable_ptr(ctx.db));
    let id = ctx.arenas.exprs.alloc(expr.clone());
    ExprAndId { expr, id }
}

/// Converts `Maybe<Expr>` to a possibly [missing](ExprMissing) [Expr].
fn wrap_maybe_with_missing<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    expr: Maybe<Expr<'db>>,
    stable_ptr: ast::ExprPtr<'db>,
) -> Expr<'db> {
    expr.unwrap_or_else(|diag_added| {
        Expr::Missing(ExprMissing {
            ty: TypeId::missing(ctx.db, diag_added),
            stable_ptr,
            diag_added,
        })
    })
}

/// Computes the semantic model of an expression, or returns a SemanticDiagnosticKind on error.
pub fn maybe_compute_expr_semantic<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    syntax: &ast::Expr<'db>,
) -> Maybe<Expr<'db>> {
    let db = ctx.db;

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
        ast::Expr::False(syntax) => Ok(false_literal_expr(ctx, syntax.stable_ptr(db).into())),
        ast::Expr::True(syntax) => Ok(true_literal_expr(ctx, syntax.stable_ptr(db).into())),
        ast::Expr::Parenthesized(paren_syntax) => {
            maybe_compute_expr_semantic(ctx, &paren_syntax.expr(db))
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
            Err(ctx.diagnostics.report(syntax.stable_ptr(db), SemanticDiagnosticKind::Unsupported))
        }
        ast::Expr::Indexed(expr) => compute_expr_indexed_semantic(ctx, expr),
        ast::Expr::FixedSizeArray(expr) => compute_expr_fixed_size_array_semantic(ctx, expr),
        ast::Expr::For(expr) => compute_expr_for_semantic(ctx, expr),
        ast::Expr::Closure(expr) => compute_expr_closure_semantic(ctx, expr, None),
    }
}

/// Expands an inline macro invocation and returns the generated code and related metadata.
fn expand_inline_macro<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    syntax: &ast::ExprInlineMacro<'db>,
) -> Maybe<InlineMacroExpansion<'db>> {
    let db = ctx.db;
    let macro_name = syntax.path(db).identifier(ctx.db);
    let crate_id = ctx.resolver.owning_crate_id;
    // Skipping expanding an inline macro if it had a parser error.
    if syntax.as_syntax_node().descendants(db).any(|node| {
        matches!(
            node.kind(db),
            SyntaxKind::ExprMissing
                | SyntaxKind::WrappedArgListMissing
                | SyntaxKind::StatementMissing
                | SyntaxKind::ModuleItemMissing
                | SyntaxKind::TraitItemMissing
                | SyntaxKind::ImplItemMissing
                | SyntaxKind::TokenMissing
                | SyntaxKind::TokenSkipped
                | SyntaxKind::WrappedTokenTreeMissing
        )
    }) {
        return Err(skip_diagnostic());
    }
    // We call the resolver with a new diagnostics, since the diagnostics should not be reported
    // if the macro was found as a plugin.
    let user_defined_macro = ctx.resolver.resolve_generic_path(
        &mut Default::default(),
        &syntax.path(db),
        NotFoundItemType::Macro,
        ResolutionContext::Statement(&mut ctx.environment),
    );
    if let Ok(ResolvedGenericItem::Macro(macro_declaration_id)) = user_defined_macro {
        let macro_rules = ctx.db.macro_declaration_rules(macro_declaration_id)?;
        let Some((rule, (captures, placeholder_to_rep_id))) = macro_rules.iter().find_map(|rule| {
            is_macro_rule_match(ctx.db, rule, &syntax.arguments(db)).map(|res| (rule, res))
        }) else {
            return Err(ctx
                .diagnostics
                .report(syntax.stable_ptr(ctx.db), InlineMacroNoMatchingRule(macro_name)));
        };
        let mut matcher_ctx =
            MatcherContext { captures, placeholder_to_rep_id, ..Default::default() };
        let expanded_code = expand_macro_rule(ctx.db, rule, &mut matcher_ctx)?;

        let macro_defsite_resolver_data =
            ctx.db.macro_declaration_resolver_data(macro_declaration_id)?;
        let inference_id = ctx.resolver.inference().inference_id;
        let callsite_resolver = ctx.resolver.data.clone_with_inference_id(ctx.db, inference_id);
        let parent_macro_call_data = ctx.resolver.macro_call_data.clone();
        let info = MacroExpansionInfo {
            mappings: expanded_code.code_mappings,
            kind: MacroKind::UserDefined,
            vars_to_expose: vec![],
        };
        ctx.resolver.macro_call_data = Some(Arc::new(ResolverMacroData {
            defsite_module_id: macro_defsite_resolver_data.module_id,
            callsite_module_id: callsite_resolver.module_id,
            expansion_mappings: info.mappings.clone(),
            parent_macro_call_data,
        }));
        Ok(InlineMacroExpansion {
            content: expanded_code.text,
            name: macro_name.to_string(db),
            info,
        })
    } else if let Some(macro_plugin_id) =
        ctx.db.crate_inline_macro_plugins(crate_id).get(&macro_name.to_string(db)).cloned()
    {
        let macro_plugin = macro_plugin_id.long(ctx.db);
        let result = macro_plugin.generate_code(
            db,
            syntax,
            &MacroPluginMetadata {
                cfg_set: &ctx.cfg_set,
                declared_derives: ctx.db.declared_derives(crate_id),
                allowed_features: &ctx.resolver.data.feature_config.allowed_features,
                edition: ctx.resolver.settings.edition,
            },
        );
        let mut diag_added = None;
        for diagnostic in result.diagnostics {
            diag_added = match diagnostic.inner_span {
                None => Some(
                    ctx.diagnostics.report(diagnostic.stable_ptr, PluginDiagnostic(diagnostic)),
                ),
                Some((offset, width)) => Some(ctx.diagnostics.report_with_inner_span(
                    diagnostic.stable_ptr,
                    (offset, width),
                    PluginDiagnostic(diagnostic),
                )),
            }
        }
        let Some(code) = result.code else {
            return Err(diag_added.unwrap_or_else(|| {
                ctx.diagnostics.report(syntax.stable_ptr(ctx.db), InlineMacroNotFound(macro_name))
            }));
        };
        Ok(InlineMacroExpansion {
            content: code.content.into(),
            name: code.name.to_string(),
            info: MacroExpansionInfo {
                mappings: code.code_mappings.into(),
                kind: if code.is_unhygienic { MacroKind::Unhygienic } else { MacroKind::Plugin },
                vars_to_expose: vec![],
            },
        })
    } else {
        let macro_name = syntax.path(db).as_syntax_node().get_text_without_trivia(db);
        Err(ctx.diagnostics.report(syntax.stable_ptr(db), InlineMacroNotFound(macro_name)))
    }
}

/// Expands and computes the semantic model of an inline macro used in expression position.
fn compute_expr_inline_macro_semantic<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    syntax: &ast::ExprInlineMacro<'db>,
) -> Maybe<Expr<'db>> {
    let prev_macro_call_data = ctx.resolver.macro_call_data.clone();
    let InlineMacroExpansion { content, name, info } = expand_inline_macro(ctx, syntax)?;
    let new_file_id = FileLongId::Virtual(VirtualFile {
        parent: Some(syntax.stable_ptr(ctx.db).untyped().span_in_file(ctx.db)),
        name: SmolStrId::from(ctx.db, name),
        content: SmolStrId::from(ctx.db, content),
        code_mappings: info.mappings.clone(),
        kind: FileKind::Expr,
        original_item_removed: true,
    })
    .intern(ctx.db);
    ctx.resolver.files.push(new_file_id);
    let expr_syntax = ctx.db.file_expr_syntax(new_file_id)?;
    let parser_diagnostics = ctx.db.file_syntax_diagnostics(new_file_id);
    if let Err(diag_added) = parser_diagnostics.check_error_free() {
        for diag in parser_diagnostics.get_diagnostics_without_duplicates(ctx.db) {
            ctx.diagnostics.report(
                syntax.stable_ptr(ctx.db),
                SemanticDiagnosticKind::MacroGeneratedCodeParserDiagnostic(diag),
            );
        }
        return Err(diag_added);
    }
    let expr = ctx.run_in_macro_subscope(|ctx| compute_expr_semantic(ctx, &expr_syntax), info);
    ctx.resolver.macro_call_data = prev_macro_call_data;
    Ok(expr.expr)
}

/// Computes the semantic model of a tail expression, handling inline macros recursively and
/// ensuring the correct tail expression is extracted from the resulting statements.
fn compute_tail_semantic<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    tail: &ast::StatementExpr<'db>,
    statements_ids: &mut Vec<StatementId>,
) -> ExprAndId<'db> {
    // Push the statement's attributes into the context, restored after the computation is resolved.
    let feature_restore = ctx.add_features_from_statement(tail);

    let db = ctx.db;
    let expr = tail.expr(db);
    let res = match &expr {
        ast::Expr::InlineMacro(inline_macro_syntax) => {
            match expand_macro_for_statement(ctx, inline_macro_syntax, true, statements_ids) {
                Ok(Some(expr_and_id)) => expr_and_id,
                Ok(None) => unreachable!("Tail expression should not be None"),
                Err(diag_added) => {
                    let expr = Expr::Missing(ExprMissing {
                        ty: TypeId::missing(db, diag_added),
                        stable_ptr: expr.stable_ptr(db),
                        diag_added,
                    });
                    ExprAndId { id: ctx.arenas.exprs.alloc(expr.clone()), expr }
                }
            }
        }
        _ => compute_expr_semantic(ctx, &expr),
    };

    // Pop the statement's attributes from the context.
    ctx.restore_features(feature_restore);
    res
}

/// Expands an inline macro used in statement position, computes its semantic model, and extends
/// `statements` with it.
fn expand_macro_for_statement<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    syntax: &ast::ExprInlineMacro<'db>,
    is_tail: bool,
    statements_ids: &mut Vec<StatementId>,
) -> Maybe<Option<ExprAndId<'db>>> {
    let prev_macro_call_data = ctx.resolver.macro_call_data.clone();
    let InlineMacroExpansion { content, name, info } = expand_inline_macro(ctx, syntax)?;
    let new_file_id = FileLongId::Virtual(VirtualFile {
        parent: Some(syntax.stable_ptr(ctx.db).untyped().span_in_file(ctx.db)),
        name: SmolStrId::from(ctx.db, name),
        content: SmolStrId::from_arcstr(ctx.db, &content),
        code_mappings: info.mappings.clone(),
        kind: FileKind::StatementList,
        original_item_removed: true,
    })
    .intern(ctx.db);
    ctx.resolver.files.push(new_file_id);
    let parser_diagnostics = ctx.db.file_syntax_diagnostics(new_file_id);
    if let Err(diag_added) = parser_diagnostics.check_error_free() {
        for diag in parser_diagnostics.get_diagnostics_without_duplicates(ctx.db) {
            ctx.diagnostics.report(
                syntax.stable_ptr(ctx.db),
                SemanticDiagnosticKind::MacroGeneratedCodeParserDiagnostic(diag),
            );
        }
        return Err(diag_added);
    }
    let statement_list = ctx.db.file_statement_list_syntax(new_file_id)?;
    let (parsed_statements, tail) = statements_and_tail(ctx.db, statement_list);
    let result = ctx.run_in_macro_subscope(
        |ctx| {
            compute_statements_semantic_and_extend(ctx, parsed_statements, statements_ids);
            if is_tail {
                if let Some(tail_expr) = tail {
                    Ok(Some(compute_tail_semantic(ctx, &tail_expr, statements_ids)))
                } else {
                    Err(ctx.diagnostics.report_after(syntax.stable_ptr(ctx.db), MissingSemicolon))
                }
            } else {
                if let Some(tail_expr) = tail {
                    let expr = compute_expr_semantic(ctx, &tail_expr.expr(ctx.db));
                    statements_ids.push(ctx.arenas.statements.alloc(semantic::Statement::Expr(
                        semantic::StatementExpr {
                            expr: expr.id,
                            stable_ptr: tail_expr.stable_ptr(ctx.db).into(),
                        },
                    )));
                }
                Ok(None)
            }
        },
        info,
    );
    ctx.resolver.macro_call_data = prev_macro_call_data;
    result
}

fn compute_expr_unary_semantic<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    syntax: &ast::ExprUnary<'db>,
) -> Maybe<Expr<'db>> {
    let db = ctx.db;
    let unary_op = syntax.op(db);
    let inner = syntax.expr(db);
    match (&unary_op, &inner) {
        // If this is not an actual function call, but actually a minus literal (e.g. -1).
        (UnaryOperator::Minus(_), ast::Expr::Literal(literal)) => {
            let (value, ty) = literal.numeric_value_and_suffix(db).unwrap_or_default();

            Ok(Expr::Literal(new_literal_expr(ctx, ty, -value, syntax.stable_ptr(db).into())?))
        }
        (UnaryOperator::At(_), inner) => {
            let expr = compute_expr_semantic(ctx, inner);

            let ty = TypeLongId::Snapshot(expr.ty()).intern(ctx.db);
            Ok(Expr::Snapshot(ExprSnapshot {
                inner: expr.id,
                ty,
                stable_ptr: syntax.stable_ptr(db).into(),
            }))
        }
        (UnaryOperator::Desnap(_), inner) => {
            // If the desnapped expr is a snapshot we `desnap` it.
            // If it is a known type we look for a deref impl and call it.
            // Otherwise we conform the desnapped type to snapshot type, and return the inner var
            // type.
            let (desnapped_expr, desnapped_ty) = {
                // The expr the desnap acts on. E.g. `x` in `*x`.
                let desnapped_expr = compute_expr_semantic(ctx, inner);
                let desnapped_expr_type = ctx.reduce_ty(desnapped_expr.ty());

                let desnapped_ty = match desnapped_expr_type.long(ctx.db) {
                    TypeLongId::Var(_) | TypeLongId::ImplType(_) => {
                        let inference = &mut ctx.resolver.inference();
                        // The type of the full desnap expr. E.g. the type of `*x` for `*x`.
                        let desnap_expr_type =
                            inference.new_type_var(Some(inner.stable_ptr(db).untyped()));
                        let desnapped_expr_type_var =
                            TypeLongId::Snapshot(desnap_expr_type).intern(ctx.db);
                        if let Err(err_set) =
                            inference.conform_ty(desnapped_expr_type_var, desnapped_expr_type)
                        {
                            let diag_added = ctx.diagnostics.report(
                                syntax.stable_ptr(db),
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
                    TypeLongId::Snapshot(ty) => *ty,
                    _ => {
                        // If the desnapped type is not a snapshot, we look for a deref impl and
                        // call it.
                        let deref_chain = ctx.db.deref_chain(
                            desnapped_expr_type,
                            ctx.resolver.owning_crate_id,
                            ctx.variable_tracker.is_mut_expr(&desnapped_expr),
                        )?;
                        let Some(DerefInfo { function_id, self_mutability, target_ty: _ }) =
                            deref_chain.derefs.first()
                        else {
                            return Err(ctx.diagnostics.report(
                                unary_op.stable_ptr(db),
                                DerefNonRef { ty: desnapped_expr_type },
                            ));
                        };
                        return expr_function_call(
                            ctx,
                            *function_id,
                            vec![NamedArg(desnapped_expr, None, *self_mutability)],
                            syntax.stable_ptr(db),
                            syntax.stable_ptr(db).into(),
                        );
                    }
                };
                (desnapped_expr, desnapped_ty)
            };

            Ok(Expr::Desnap(ExprDesnap {
                inner: desnapped_expr.id,
                ty: desnapped_ty,
                stable_ptr: syntax.stable_ptr(db).into(),
            }))
        }
        (UnaryOperator::Reference(_), inner) => {
            let stable_ptr = syntax.stable_ptr(db);
            if !crate::types::are_repr_ptrs_enabled(ctx.db, ctx.resolver.module_id) {
                return Err(ctx.diagnostics.report(stable_ptr, ReprPtrsDisabled));
            }
            let inner_expr = compute_expr_semantic(ctx, inner);

            // Disable mutability from repr ptr variable.
            ctx.variable_tracker.mark_referenced(&inner_expr, stable_ptr.untyped());

            // Snapshot inner expression.
            let inner_ty = inner_expr.ty();
            let snapshot_ty = TypeLongId::Snapshot(inner_ty).intern(ctx.db);
            let snapshot_expr = ExprSnapshot {
                inner: inner_expr.id,
                ty: snapshot_ty,
                stable_ptr: stable_ptr.into(),
            };
            let snapshot_expr_id = ctx.arenas.exprs.alloc(Expr::Snapshot(snapshot_expr.clone()));

            // Get BoxTrait::<@T>::new
            let info = ctx.db.core_info();
            let generic_args = vec![GenericArgumentId::Type(snapshot_ty)];
            let concrete_trait_id =
                crate::ConcreteTraitLongId { trait_id: info.box_trt, generic_args }.intern(ctx.db);
            let concrete_trait_function =
                crate::items::trt::ConcreteTraitGenericFunctionLongId::new(
                    ctx.db,
                    concrete_trait_id,
                    info.box_new_fn,
                )
                .intern(ctx.db);

            // Resolve which BoxTrait implementation applies to this type.
            let impl_lookup_context = ctx.resolver.impl_lookup_context();
            let mut inference = ctx.resolver.inference();
            let function = inference
                .infer_trait_function(
                    concrete_trait_function,
                    impl_lookup_context,
                    Some(stable_ptr.untyped()),
                )
                .map_err(|err_set| {
                    inference.report_on_pending_error(
                        err_set,
                        ctx.diagnostics,
                        stable_ptr.untyped(),
                    )
                })?;

            // Call BoxTrait::new(@x).
            expr_function_call(
                ctx,
                function,
                vec![NamedArg(
                    ExprAndId { expr: Expr::Snapshot(snapshot_expr), id: snapshot_expr_id },
                    None,
                    Mutability::Immutable,
                )],
                stable_ptr,
                stable_ptr.into(),
            )
        }
        (_, inner) => {
            let expr = compute_expr_semantic(ctx, inner);

            let mut inference = ctx.resolver.inference();
            let concrete_trait_function = match core_unary_operator(
                ctx.db,
                &mut inference,
                &unary_op,
                syntax.stable_ptr(db).untyped(),
            )? {
                Err(err_kind) => {
                    return Err(ctx.diagnostics.report(unary_op.stable_ptr(db), err_kind));
                }
                Ok(function) => function,
            };

            let impl_lookup_context = ctx.resolver.impl_lookup_context();
            let mut inference = ctx.resolver.inference();
            let function = inference
                .infer_trait_function(
                    concrete_trait_function,
                    impl_lookup_context,
                    Some(syntax.stable_ptr(db).untyped()),
                )
                .map_err(|err_set| {
                    inference.report_on_pending_error(
                        err_set,
                        ctx.diagnostics,
                        syntax.stable_ptr(db).untyped(),
                    )
                })?;

            expr_function_call(
                ctx,
                function,
                vec![NamedArg(expr, None, Mutability::Immutable)],
                syntax.stable_ptr(db),
                syntax.stable_ptr(db).into(),
            )
        }
    }
}

fn compute_expr_binary_semantic<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    syntax: &ast::ExprBinary<'db>,
) -> Maybe<Expr<'db>> {
    let db = ctx.db;

    let stable_ptr = syntax.stable_ptr(db).into();
    let binary_op = syntax.op(db);
    let lhs_syntax = &syntax.lhs(db);
    let rhs_syntax = syntax.rhs(db);

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
                _ => {
                    return Err(ctx
                        .diagnostics
                        .report(lhs_syntax.stable_ptr(db), InvalidLhsForAssignment));
                }
            };

            let inference = &mut ctx.resolver.inference();
            inference.conform_ty_for_diag(
                rexpr.ty(),
                member_path.ty(),
                ctx.diagnostics,
                || rhs_syntax.stable_ptr(db).untyped(),
                |actual_ty, expected_ty| WrongArgumentType { expected_ty, actual_ty },
            )?;
            ctx.variable_tracker.report_var_mutability_error(
                db,
                ctx.diagnostics,
                &member_path.base_var(),
                syntax.stable_ptr(db),
                AssignmentToImmutableVar,
            );
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
                || lhs_syntax.stable_ptr(db).untyped(),
                |actual_ty, expected_ty| WrongType { expected_ty, actual_ty },
            );
            let _ = inference.conform_ty_for_diag(
                rexpr.expr.ty(),
                bool_ty,
                ctx.diagnostics,
                || rhs_syntax.stable_ptr(db).untyped(),
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
fn call_core_binary_op<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    syntax: &ast::ExprBinary<'db>,
    lhs_syntax: &ast::Expr<'db>,
    rhs_syntax: &ast::Expr<'db>,
) -> Maybe<Expr<'db>> {
    let db = ctx.db;
    let stable_ptr = syntax.stable_ptr(db);
    let binary_op = syntax.op(db);

    let (concrete_trait_function, snapshot) = match core_binary_operator(
        db,
        &mut ctx.resolver.inference(),
        &binary_op,
        stable_ptr.untyped(),
    )? {
        Err(err_kind) => {
            return Err(ctx.diagnostics.report(binary_op.stable_ptr(db), err_kind));
        }
        Ok(res) => res,
    };

    let impl_lookup_context = ctx.resolver.impl_lookup_context();
    let inference = &mut ctx.resolver.inference();
    let function = inference
        .infer_trait_function(
            concrete_trait_function,
            impl_lookup_context,
            Some(stable_ptr.untyped()),
        )
        .map_err(|err_set| {
            inference.report_on_pending_error(err_set, ctx.diagnostics, stable_ptr.untyped())
        })?;

    let mut lexpr = compute_expr_semantic(ctx, lhs_syntax);

    if let (Expr::Missing(_), BinaryOperator::LT(_)) = (&lexpr.expr, &binary_op) {
        return Err(ctx
            .diagnostics
            .report(binary_op.stable_ptr(db), SemanticDiagnosticKind::MaybeMissingColonColon));
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
    let first_param = sig.params.first().unwrap();

    expr_function_call(
        ctx,
        function,
        vec![
            NamedArg(lexpr, None, first_param.mutability),
            NamedArg(rexpr, None, Mutability::Immutable),
        ],
        stable_ptr,
        stable_ptr.into(),
    )
}

fn compute_expr_tuple_semantic<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    syntax: &ast::ExprListParenthesized<'db>,
) -> Maybe<Expr<'db>> {
    let db = ctx.db;

    let mut items: Vec<ExprId> = vec![];
    let mut types: Vec<TypeId<'_>> = vec![];
    for expr_syntax in syntax.expressions(db).elements(db) {
        let expr_semantic = compute_expr_semantic(ctx, &expr_syntax);
        types.push(ctx.reduce_ty(expr_semantic.ty()));
        items.push(expr_semantic.id);
    }
    Ok(Expr::Tuple(ExprTuple {
        items,
        ty: TypeLongId::Tuple(types).intern(db),
        stable_ptr: syntax.stable_ptr(db).into(),
    }))
}
/// Computes the semantic model of an expression of type [ast::ExprFixedSizeArray].
fn compute_expr_fixed_size_array_semantic<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    syntax: &ast::ExprFixedSizeArray<'db>,
) -> Maybe<Expr<'db>> {
    let db = ctx.db;
    let exprs = syntax.exprs(db).elements_vec(db);
    let size_ty = get_usize_ty(db);
    let (items, type_id, size) = if let Some(size_const_id) =
        extract_fixed_size_array_size(db, ctx.diagnostics, syntax, ctx.resolver)?
    {
        // Fixed size array with a defined size must have exactly one element.
        let [expr] = exprs.as_slice() else {
            return Err(ctx
                .diagnostics
                .report(syntax.stable_ptr(db), FixedSizeArrayNonSingleValue));
        };
        let expr_semantic = compute_expr_semantic(ctx, expr);
        let size = size_const_id
            .long(db)
            .to_int()
            .ok_or_else(|| {
                ctx.diagnostics.report(syntax.stable_ptr(db), FixedSizeArrayNonNumericSize)
            })?
            .to_usize()
            .unwrap();
        verify_fixed_size_array_size(db, ctx.diagnostics, &size.into(), syntax)?;
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
                || expr_syntax.stable_ptr(db).untyped(),
                |actual_ty, expected_ty| WrongArgumentType { expected_ty, actual_ty },
            )?;
            items.push(expr_semantic.id);
        }
        (FixedSizeArrayItems::Items(items), first_expr_ty, size)
    } else {
        (
            FixedSizeArrayItems::Items(vec![]),
            ctx.resolver.inference().new_type_var(Some(syntax.stable_ptr(db).untyped())),
            ConstValue::Int(0.into(), size_ty).intern(db),
        )
    };
    Ok(Expr::FixedSizeArray(ExprFixedSizeArray {
        items,
        ty: TypeLongId::FixedSizeArray { type_id, size }.intern(db),
        stable_ptr: syntax.stable_ptr(db).into(),
    }))
}

fn compute_expr_function_call_semantic<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    syntax: &ast::ExprFunctionCall<'db>,
) -> Maybe<Expr<'db>> {
    let db = ctx.db;

    let path = syntax.path(db);
    let args_syntax = syntax.arguments(db).arguments(db);
    // Check if this is a variable.
    let mut is_shadowed_by_variable = false;
    if let Some((identifier, is_callsite_prefixed)) = try_extract_identifier_from_path(db, &path) {
        let variable_name = identifier.text(ctx.db);
        if let Some(var) = get_binded_expr_by_name(
            ctx,
            variable_name,
            is_callsite_prefixed,
            path.stable_ptr(ctx.db).into(),
        ) {
            is_shadowed_by_variable = true;
            // if closures are not in context, we want to call the function instead of the variable.
            if ctx.are_closures_in_context {
                let info = db.core_info();
                // TODO(TomerStarkware): find the correct trait based on captured variables.
                let fn_once_trait = info.fn_once_trt;
                let fn_trait = info.fn_trt;
                let self_expr = ExprAndId { expr: var.clone(), id: ctx.arenas.exprs.alloc(var) };
                let mut closure_call_data = |call_trait: TraitId<'db>| {
                    compute_method_function_call_data(
                        ctx,
                        &[call_trait],
                        SmolStrId::from(ctx.db, "call"),
                        self_expr.clone(),
                        syntax.stable_ptr(db).untyped(),
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

                let args_iter = args_syntax.elements(db);
                // Normal parameters
                let mut args = vec![];
                let mut arg_types = vec![];
                for arg_syntax in args_iter {
                    let stable_ptr = arg_syntax.stable_ptr(db);
                    let arg = compute_named_argument_clause(ctx, arg_syntax, None);
                    if arg.2 != Mutability::Immutable {
                        return Err(ctx.diagnostics.report(stable_ptr, RefClosureArgument));
                    }
                    if arg.1.is_some() {
                        return Err(ctx
                            .diagnostics
                            .report(stable_ptr, NamedArgumentsAreNotSupported));
                    }
                    args.push(arg.0.id);
                    arg_types.push(arg.0.ty());
                }
                let args_expr = Expr::Tuple(ExprTuple {
                    items: args,
                    ty: TypeLongId::Tuple(arg_types).intern(db),
                    stable_ptr: syntax.stable_ptr(db).into(),
                });
                let args_expr =
                    ExprAndId { expr: args_expr.clone(), id: ctx.arenas.exprs.alloc(args_expr) };
                let call_ptr = syntax.stable_ptr(db);
                return expr_function_call(
                    ctx,
                    call_function_id,
                    vec![
                        NamedArg(fixed_closure, None, closure_mutability),
                        NamedArg(args_expr, None, Mutability::Immutable),
                    ],
                    call_ptr,
                    call_ptr.into(),
                );
            }
        }
    }

    let item = ctx
        .resolver
        .resolve_concrete_path_ex(
            ctx.diagnostics,
            &path,
            NotFoundItemType::Function,
            ResolutionContext::Statement(&mut ctx.environment),
        )
        .inspect_err(|_| {
            // Getting better diagnostics and usage metrics for the function args.
            for arg in args_syntax.elements(db) {
                compute_named_argument_clause(ctx, arg, None);
            }
        })?;

    match item {
        ResolvedConcreteItem::Variant(variant) => {
            let concrete_enum_type =
                TypeLongId::Concrete(ConcreteTypeId::Enum(variant.concrete_enum_id)).intern(db);
            if concrete_enum_type.is_phantom(db) {
                ctx.diagnostics.report(syntax.stable_ptr(db), CannotCreateInstancesOfPhantomTypes);
            }

            let named_args: Vec<_> = args_syntax
                .elements(db)
                .map(|arg_syntax| compute_named_argument_clause(ctx, arg_syntax, None))
                .collect();
            if named_args.len() != 1 {
                return Err(ctx.diagnostics.report(
                    syntax.stable_ptr(db),
                    WrongNumberOfArguments { expected: 1, actual: named_args.len() },
                ));
            }
            let NamedArg(arg, name_terminal, mutability) = named_args[0].clone();
            if let Some(name_terminal) = name_terminal {
                ctx.diagnostics.report(name_terminal.stable_ptr(db), NamedArgumentsAreNotSupported);
            }
            if mutability != Mutability::Immutable {
                return Err(ctx
                    .diagnostics
                    .report(args_syntax.stable_ptr(db), VariantCtorNotImmutable));
            }
            let inference = &mut ctx.resolver.inference();
            inference.conform_ty_for_diag(
                arg.ty(),
                variant.ty,
                ctx.diagnostics,
                || args_syntax.stable_ptr(db).untyped(),
                |actual_ty, expected_ty| WrongArgumentType { expected_ty, actual_ty },
            )?;
            Ok(semantic::Expr::EnumVariantCtor(semantic::ExprEnumVariantCtor {
                variant,
                value_expr: arg.id,
                ty: concrete_enum_type,
                stable_ptr: syntax.stable_ptr(db).into(),
            }))
        }
        ResolvedConcreteItem::Function(function) => {
            if is_shadowed_by_variable {
                return Err(ctx.diagnostics.report(
                    path.stable_ptr(ctx.db),
                    CallingShadowedFunction {
                        shadowed_function_name: path
                            .segments(db)
                            .elements(db)
                            .next()
                            .unwrap()
                            .identifier(db),
                    },
                ));
            }

            // Note there may be n+1 arguments for n parameters, if the last one is a coupon.
            let mut args_iter = args_syntax.elements(db);
            // Normal parameters
            let mut named_args = vec![];
            let closure_params = db.concrete_function_closure_params(function)?;
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
            let call_ptr = syntax.stable_ptr(db);
            expr_function_call(ctx, function, named_args, call_ptr, call_ptr.into())
        }
        _ => Err(ctx.diagnostics.report(
            path.stable_ptr(db),
            UnexpectedElement { expected: vec![ElementKind::Function], actual: (&item).into() },
        )),
    }
}

/// Computes the semantic model of an expression of type [ast::Arg].
///
/// Returns the value and the optional argument name.
pub fn compute_named_argument_clause<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    arg_syntax: ast::Arg<'db>,
    closure_params_tuple_ty: Option<TypeId<'db>>,
) -> NamedArg<'db> {
    let db = ctx.db;

    let mutability =
        compute_mutability(ctx.diagnostics, db, &arg_syntax.modifiers(db).elements_vec(db));

    let arg_clause = arg_syntax.arg_clause(db);
    let (expr, arg_name_identifier) = match arg_clause {
        ast::ArgClause::Unnamed(arg_unnamed) => (
            handle_possible_closure_expr(ctx, &arg_unnamed.value(db), closure_params_tuple_ty),
            None,
        ),
        ast::ArgClause::Named(arg_named) => (
            handle_possible_closure_expr(ctx, &arg_named.value(db), closure_params_tuple_ty),
            Some(arg_named.name(db)),
        ),
        ast::ArgClause::FieldInitShorthand(arg_field_init_shorthand) => {
            let name_expr = arg_field_init_shorthand.name(db);
            let stable_ptr: ast::ExprPtr<'_> = name_expr.stable_ptr(db).into();
            let arg_name_identifier = name_expr.name(db);
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
fn handle_possible_closure_expr<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    expr: &ast::Expr<'db>,
    closure_param_types: Option<TypeId<'db>>,
) -> ExprAndId<'db> {
    if let ast::Expr::Closure(expr_closure) = expr {
        let expr = compute_expr_closure_semantic(ctx, expr_closure, closure_param_types);
        let expr = wrap_maybe_with_missing(ctx, expr, expr_closure.stable_ptr(ctx.db).into());
        let id = ctx.arenas.exprs.alloc(expr.clone());
        ExprAndId { expr, id }
    } else {
        compute_expr_semantic(ctx, expr)
    }
}

pub fn compute_root_expr<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    syntax: &ast::ExprBlock<'db>,
    return_type: TypeId<'db>,
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
    inference.conform_generic_params_type_constraints(constrains);

    let return_type = ctx.reduce_ty(return_type);
    let res = compute_expr_block_semantic(ctx, syntax)?;
    let res_ty = ctx.reduce_ty(res.ty());
    let res = ctx.arenas.exprs.alloc(res);
    let inference = &mut ctx.resolver.inference();
    let db = ctx.db;
    let _ = inference.conform_ty_for_diag(
        res_ty,
        return_type,
        ctx.diagnostics,
        || {
            ctx.signature
                .map(|s| match s.stable_ptr.lookup(db).ret_ty(db) {
                    OptionReturnTypeClause::Empty(_) => syntax.stable_ptr(db).untyped(),
                    OptionReturnTypeClause::ReturnTypeClause(return_type_clause) => {
                        return_type_clause.ty(db).stable_ptr(db).untyped()
                    }
                })
                .unwrap_or_else(|| syntax.stable_ptr(db).untyped())
        },
        |actual_ty, expected_ty| WrongReturnType { expected_ty, actual_ty },
    );

    // Check fully resolved.
    inference.finalize(ctx.diagnostics, syntax.stable_ptr(db).untyped());

    ctx.apply_inference_rewriter();
    if ctx.signature.map(|s| s.is_const) == Some(true) {
        validate_const_expr(ctx, res);
    }
    Ok(res)
}

/// Computes the semantic model for a list of statements, flattening the result.
pub fn compute_statements_semantic_and_extend<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    statements_syntax: impl Iterator<Item = ast::Statement<'db>>,
    statement_ids: &mut Vec<StatementId>,
) {
    for statement_syntax in statements_syntax {
        compute_and_append_statement_semantic(ctx, statement_syntax, statement_ids)
            .unwrap_or_default();
    }
}

/// Computes the semantic model of an expression of type [ast::ExprBlock].
pub fn compute_expr_block_semantic<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    syntax: &ast::ExprBlock<'db>,
) -> Maybe<Expr<'db>> {
    let db = ctx.db;
    ctx.run_in_subscope(|new_ctx| {
        let (statements, tail) = statements_and_tail(db, syntax.statements(db));
        let mut statements_semantic = vec![];
        compute_statements_semantic_and_extend(new_ctx, statements, &mut statements_semantic);
        let tail_semantic_expr =
            tail.map(|tail| compute_tail_semantic(new_ctx, &tail, &mut statements_semantic));
        let ty = block_ty(new_ctx, &statements_semantic, &tail_semantic_expr);
        Ok(Expr::Block(ExprBlock {
            statements: statements_semantic,
            tail: tail_semantic_expr.map(|expr| expr.id),
            ty,
            stable_ptr: syntax.stable_ptr(db).into(),
        }))
    })
}

/// The type returned from a block with the given statements and tail.
fn block_ty<'db>(
    ctx: &ComputationContext<'db, '_>,
    statements: &[StatementId],
    tail: &Option<ExprAndId<'db>>,
) -> TypeId<'db> {
    if let Some(tail) = tail {
        return tail.ty();
    }
    let Some(statement) = statements
        .iter()
        .rev()
        .map(|id| &ctx.arenas.statements[*id])
        .find(|s| !matches!(s, Statement::Item(_)))
    else {
        return unit_ty(ctx.db);
    };
    match statement {
        Statement::Item(_) => unreachable!("Was previously filtered out."),
        Statement::Let(_) => unit_ty(ctx.db),
        Statement::Return(_) | Statement::Break(_) | Statement::Continue(_) => never_ty(ctx.db),
        Statement::Expr(expr) => {
            let never_ty = never_ty(ctx.db);
            if ctx.arenas.exprs[expr.expr].ty() == never_ty { never_ty } else { unit_ty(ctx.db) }
        }
    }
}

/// Helper for merging the return types of branch blocks (match or if else).
#[derive(Debug, Clone)]
struct FlowMergeTypeHelper<'db> {
    multi_arm_expr_kind: MultiArmExprKind,
    never_type: TypeId<'db>,
    final_type: Option<TypeId<'db>>,
    /// Whether or not the Helper had a previous type merge error.
    had_merge_error: bool,
}
impl<'db> FlowMergeTypeHelper<'db> {
    fn new(db: &'db dyn Database, multi_arm_expr_kind: MultiArmExprKind) -> Self {
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
        db: &'db dyn Database,
        diagnostics: &mut SemanticDiagnostics<'db>,
        inference: &mut Inference<'db, '_>,
        ty: TypeId<'db>,
        stable_ptr: SyntaxStablePtrId<'db>,
    ) -> bool {
        if self.had_merge_error {
            return false;
        }

        // Merging of never type if forbidden in loops but not in other multi-arm expressions.
        if (ty == never_ty(db) && self.multi_arm_expr_kind != MultiArmExprKind::Loop)
            || ty.is_missing(db)
        {
            return true;
        }

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

        true
    }

    /// Returns the merged type.
    fn get_final_type(self) -> TypeId<'db> {
        self.final_type.unwrap_or(self.never_type)
    }
}

/// Computes the semantic of a match arm pattern and the block expression.
fn compute_arm_semantic<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    expr: &Expr<'db>,
    arm_expr_syntax: ast::Expr<'db>,
    patterns_syntax: &PatternListOr<'db>,
) -> (Vec<PatternAndId<'db>>, ExprAndId<'db>) {
    ctx.run_in_subscope(|new_ctx| {
        let patterns = compute_pattern_list_or_semantic(new_ctx, expr, patterns_syntax);
        let arm_expr = compute_expr_semantic(new_ctx, &arm_expr_syntax);
        (patterns, arm_expr)
    })
}

/// Computes the semantic of `PatternListOr` and introducing the pattern variables into the scope.
fn compute_pattern_list_or_semantic<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    expr: &Expr<'db>,
    patterns_syntax: &PatternListOr<'db>,
) -> Vec<PatternAndId<'db>> {
    let db = ctx.db;

    // Typecheck the arms's patterns, and introduce the new variables to the subscope.
    // Note that if the arm expr is a block, there will be *another* subscope
    // for it.
    let mut arm_patterns_variables: UnorderedHashMap<SmolStrId<'db>, LocalVariable<'db>> =
        UnorderedHashMap::default();

    let patterns: Vec<_> = patterns_syntax
        .elements(db)
        .map(|pattern_syntax| {
            let pattern: PatternAndId<'_> =
                compute_pattern_semantic(ctx, &pattern_syntax, expr.ty(), &arm_patterns_variables);
            let variables = pattern.variables(&ctx.arenas.patterns);
            for variable in variables {
                match arm_patterns_variables.entry(variable.name) {
                    std::collections::hash_map::Entry::Occupied(entry) => {
                        let get_location = || variable.stable_ptr.lookup(db).stable_ptr(db);
                        let var = entry.get();

                        let expected_ty = ctx.reduce_ty(var.ty);
                        let actual_ty = ctx.reduce_ty(variable.var.ty);

                        let mut has_inference_error = false;
                        if !variable.var.ty.is_missing(ctx.db) {
                            let inference = &mut ctx.resolver.inference();
                            if inference
                                .conform_ty_for_diag(
                                    actual_ty,
                                    expected_ty,
                                    ctx.diagnostics,
                                    || get_location().untyped(),
                                    |actual_ty, expected_ty| WrongType { expected_ty, actual_ty },
                                )
                                .is_err()
                            {
                                has_inference_error = true;
                            }
                        };
                        if !has_inference_error && var.is_mut != variable.var.is_mut {
                            ctx.diagnostics.report(get_location(), InconsistentBinding);
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

    for (pattern_syntax, pattern) in patterns_syntax.elements(db).zip(patterns.iter()) {
        let variables = pattern.variables(&ctx.arenas.patterns);

        let mut variable_names_in_pattern = UnorderedHashSet::<_>::default();
        for v in variables {
            if !variable_names_in_pattern.insert(v.name) {
                ctx.diagnostics.report(v.stable_ptr, VariableDefinedMultipleTimesInPattern(v.name));
            }
            let var_def = Binding::LocalVar(v.var.clone());
            let _ = ctx.insert_variable(v.name, var_def);
        }

        if variable_names_in_pattern.len() != arm_patterns_variables.len() {
            ctx.diagnostics.report(pattern_syntax.stable_ptr(db), MissingVariableInPattern);
        }
    }

    patterns
}

/// Computes the semantic model of an expression of type [ast::ExprMatch].
fn compute_expr_match_semantic<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    syntax: &ast::ExprMatch<'db>,
) -> Maybe<Expr<'db>> {
    // TODO(yuval): verify exhaustiveness.
    let db = ctx.db;
    let arms = syntax.arms(db);
    let syntax_arms = arms.elements(db);
    let expr = compute_expr_semantic(ctx, &syntax.expr(db));
    // Run compute_pattern_semantic on every arm, even if other arms failed, to get as many
    // diagnostics as possible.
    let patterns_and_exprs: Vec<_> = syntax_arms
        .map(|syntax_arm| {
            compute_arm_semantic(ctx, &expr, syntax_arm.expression(db), &syntax_arm.patterns(db))
        })
        .collect();
    // Unify arm types.
    let mut helper = FlowMergeTypeHelper::new(ctx.db, MultiArmExprKind::Match);
    for (_, expr) in &patterns_and_exprs {
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
        stable_ptr: syntax.stable_ptr(db).into(),
    }))
}

/// Computes the semantic model of an expression of type [ast::ExprIf].
fn compute_expr_if_semantic<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    syntax: &ast::ExprIf<'db>,
) -> Maybe<Expr<'db>> {
    let db = ctx.db;

    let (conditions, if_block) = compute_condition_list_semantic(
        ctx,
        &syntax.conditions(db),
        &ast::Expr::Block(syntax.if_block(db)),
    );

    let (else_block_opt, else_block_ty) = match syntax.else_clause(db) {
        ast::OptionElseClause::Empty(_) => (None, unit_ty(ctx.db)),
        ast::OptionElseClause::ElseClause(else_clause) => match else_clause.else_block_or_if(db) {
            BlockOrIf::Block(block) => {
                let else_block = compute_expr_block_semantic(ctx, &block)?;
                (Some(else_block.clone()), else_block.ty())
            }
            BlockOrIf::If(expr_if) => {
                let else_if = compute_expr_if_semantic(ctx, &expr_if)?;
                (Some(else_if.clone()), else_if.ty())
            }
        },
    };

    let mut helper = FlowMergeTypeHelper::new(ctx.db, MultiArmExprKind::If);
    let if_block_ty = ctx.reduce_ty(if_block.ty());
    let else_block_ty = ctx.reduce_ty(else_block_ty);
    let inference = &mut ctx.resolver.inference();
    let _ = helper.try_merge_types(
        ctx.db,
        ctx.diagnostics,
        inference,
        if_block_ty,
        syntax.stable_ptr(db).untyped(),
    ) && helper.try_merge_types(
        ctx.db,
        ctx.diagnostics,
        inference,
        else_block_ty,
        syntax.stable_ptr(db).untyped(),
    );
    Ok(Expr::If(ExprIf {
        conditions,
        if_block: if_block.id,
        else_block: else_block_opt.map(|else_block| ctx.arenas.exprs.alloc(else_block)),
        ty: helper.get_final_type(),
        stable_ptr: syntax.stable_ptr(db).into(),
    }))
}

/// Computes the semantic of the given condition list and the given body.
///
/// Note that pattern variables in the conditions can be used in the body.
fn compute_condition_list_semantic<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    condition_list_syntax: &ConditionListAnd<'db>,
    body_syntax: &ast::Expr<'db>,
) -> (Vec<Condition>, ExprAndId<'db>) {
    let mut conditions = Vec::new();
    let conditions_syntax = condition_list_syntax.elements(ctx.db);
    conditions.reserve(conditions_syntax.len());

    let body = compute_condition_list_semantic_helper(
        ctx,
        conditions_syntax,
        &mut conditions,
        body_syntax,
    );

    (conditions, body)
}

/// Computes the semantic of the given condition list and the given body.
///
/// Note that pattern variables in the conditions can be used in the body.
fn compute_condition_list_semantic_helper<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    mut conditions_syntax: impl Iterator<Item = ast::Condition<'db>>,
    conditions: &mut Vec<Condition>,
    body_syntax: &ast::Expr<'db>,
) -> ExprAndId<'db> {
    match conditions_syntax.next() {
        None => compute_expr_semantic(ctx, body_syntax),
        Some(ast::Condition::Let(condition)) => {
            let expr = compute_expr_semantic(ctx, &condition.expr(ctx.db));

            ctx.run_in_subscope(|new_ctx| {
                let patterns =
                    compute_pattern_list_or_semantic(new_ctx, &expr, &condition.patterns(ctx.db));
                conditions.push(Condition::Let(
                    expr.id,
                    patterns.iter().map(|pattern| pattern.id).collect(),
                ));
                compute_condition_list_semantic_helper(
                    new_ctx,
                    conditions_syntax,
                    conditions,
                    body_syntax,
                )
            })
        }
        Some(ast::Condition::Expr(expr)) => {
            conditions.push(Condition::BoolExpr(
                compute_bool_condition_semantic(ctx, &expr.expr(ctx.db)).id,
            ));

            compute_condition_list_semantic_helper(ctx, conditions_syntax, conditions, body_syntax)
        }
    }
}

/// Computes the semantic model of an expression of type [ast::ExprLoop].
fn compute_expr_loop_semantic<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    syntax: &ast::ExprLoop<'db>,
) -> Maybe<Expr<'db>> {
    let db = ctx.db;

    let (body, inner_ctx) = compute_loop_body_semantic(
        ctx,
        syntax.body(db),
        InnerContextKind::Loop {
            type_merger: FlowMergeTypeHelper::new(db, MultiArmExprKind::Loop),
        },
    );

    let InnerContext { kind: InnerContextKind::Loop { type_merger, .. }, .. } = inner_ctx else {
        unreachable!("Expected loop context");
    };
    Ok(Expr::Loop(ExprLoop {
        body,
        ty: type_merger.get_final_type(),
        stable_ptr: syntax.stable_ptr(db).into(),
    }))
}

/// Computes the semantic model of an expression of type [ast::ExprWhile].
fn compute_expr_while_semantic<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    syntax: &ast::ExprWhile<'db>,
) -> Maybe<Expr<'db>> {
    let db = ctx.db;

    let Some([condition_syntax]) = &syntax.conditions(db).elements(db).collect_array() else {
        return Err(ctx.diagnostics.report(syntax.conditions(db).stable_ptr(db), Unsupported));
    };

    let (condition, body) = match condition_syntax {
        ast::Condition::Let(condition) => {
            let expr = compute_expr_semantic(ctx, &condition.expr(db));

            let (patterns, body) = ctx.run_in_subscope(|new_ctx| {
                let patterns =
                    compute_pattern_list_or_semantic(new_ctx, &expr, &condition.patterns(db));

                let (id, _) =
                    compute_loop_body_semantic(new_ctx, syntax.body(db), InnerContextKind::While);
                let expr = new_ctx.arenas.exprs[id].clone();
                (patterns, ExprAndId { expr, id })
            });

            (Condition::Let(expr.id, patterns.iter().map(|pattern| pattern.id).collect()), body.id)
        }
        ast::Condition::Expr(expr) => {
            let (body, _inner_ctx) =
                compute_loop_body_semantic(ctx, syntax.body(db), InnerContextKind::While);
            (Condition::BoolExpr(compute_bool_condition_semantic(ctx, &expr.expr(db)).id), body)
        }
    };

    Ok(Expr::While(ExprWhile {
        condition,
        body,
        ty: unit_ty(ctx.db),
        stable_ptr: syntax.stable_ptr(db).into(),
    }))
}

/// Computes the semantic model of an expression of type [ast::ExprFor].
fn compute_expr_for_semantic<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    syntax: &ast::ExprFor<'db>,
) -> Maybe<Expr<'db>> {
    let db = ctx.db;
    let expr_ptr = syntax.expr(db).stable_ptr(db);
    let expr = compute_expr_semantic(ctx, &syntax.expr(db));
    let into_iterator_trait = ctx.db.core_info().into_iterator_trt;

    let (into_iterator_function_id, _, fixed_into_iter_var, into_iter_mutability) =
        compute_method_function_call_data(
            ctx,
            &[into_iterator_trait],
            SmolStrId::from(ctx.db, "into_iter"),
            expr,
            expr_ptr.into(),
            None,
            |ty, _, inference_errors| {
                Some(NoImplementationOfTrait {
                    ty,
                    inference_errors,
                    trait_name: SmolStrId::from(ctx.db, "IntoIterator"),
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
    let expr_id = fixed_into_iter_var.id;
    let into_iter_call = expr_function_call(
        ctx,
        into_iterator_function_id,
        vec![NamedArg(fixed_into_iter_var, None, into_iter_mutability)],
        expr_ptr,
        expr_ptr,
    )?;

    let into_iter_variable =
        LocalVarLongId(ctx.resolver.module_id, syntax.identifier(db).stable_ptr(db)).intern(ctx.db);

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
        SmolStrId::from(ctx.db, "next"),
        ExprAndId { expr: into_iter_expr, id: into_iter_expr_id },
        expr_ptr.into(),
        None,
        |ty, _, inference_errors| {
            Some(NoImplementationOfTrait {
                ty,
                inference_errors,
                trait_name: SmolStrId::from(ctx.db, "Iterator"),
            })
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
        match db.concrete_function_signature(next_function_id)?.return_type.long(db) {
            TypeLongId::Concrete(semantic::ConcreteTypeId::Enum(enm)) => {
                assert_eq!(enm.enum_id(db).name(db).long(db), "Option");
                db.concrete_enum_variants(*enm).unwrap().into_iter().next().unwrap()
            }
            _ => unreachable!(),
        };
    let (body_id, pattern) = ctx.run_in_subscope(|new_ctx| {
        let inner_pattern = compute_pattern_semantic(
            new_ctx,
            &syntax.pattern(db),
            next_success_variant.ty,
            &UnorderedHashMap::default(),
        );
        let variables = inner_pattern.variables(&new_ctx.arenas.patterns);
        let mut variable_names_in_pattern = UnorderedHashSet::<_>::default();
        for v in variables {
            if !variable_names_in_pattern.insert(v.name) {
                new_ctx
                    .diagnostics
                    .report(v.stable_ptr, VariableDefinedMultipleTimesInPattern(v.name));
            }
            let var_def = Binding::LocalVar(v.var.clone());
            let _ = new_ctx.insert_variable(v.name, var_def);
        }
        let (body, _inner_ctx) =
            compute_loop_body_semantic(new_ctx, syntax.body(db), InnerContextKind::For);
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
        stable_ptr: syntax.stable_ptr(db).into(),
    }))
}

/// Computes the semantic model for a body of a loop.
fn compute_loop_body_semantic<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    syntax: ast::ExprBlock<'db>,
    kind: InnerContextKind<'db>,
) -> (ExprId, InnerContext<'db>) {
    let db: &dyn Database = ctx.db;
    ctx.run_in_subscope(|new_ctx| {
        let return_type = new_ctx.get_return_type().unwrap();
        let old_inner_ctx = new_ctx.inner_ctx.replace(InnerContext { return_type, kind });
        let (statements, tail) = statements_and_tail(ctx.db, syntax.statements(db));
        let mut statements_semantic = vec![];
        compute_statements_semantic_and_extend(new_ctx, statements, &mut statements_semantic);
        let tail_semantic_expr =
            tail.map(|tail| compute_tail_semantic(new_ctx, &tail, &mut statements_semantic));
        if let Some(tail) = &tail_semantic_expr
            && !tail.ty().is_missing(db)
            && !tail.ty().is_unit(db)
            && tail.ty() != never_ty(db)
        {
            new_ctx.diagnostics.report(tail.deref(), TailExpressionNotAllowedInLoop);
        }
        let inner_ctx = std::mem::replace(&mut new_ctx.inner_ctx, old_inner_ctx).unwrap();
        let body = new_ctx.arenas.exprs.alloc(Expr::Block(ExprBlock {
            statements: statements_semantic,
            tail: tail_semantic_expr.map(|expr| expr.id),
            ty: unit_ty(db),
            stable_ptr: syntax.stable_ptr(db).into(),
        }));
        (body, inner_ctx)
    })
}

/// Computes the semantic model of an expression of type [ast::ExprClosure].
fn compute_expr_closure_semantic<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    syntax: &ast::ExprClosure<'db>,
    params_tuple_ty: Option<TypeId<'db>>,
) -> Maybe<Expr<'db>> {
    ctx.are_closures_in_context = true;
    let db = ctx.db;
    let (params, ret_ty, body) = ctx.run_in_subscope(|new_ctx| {
        let params = if let ClosureParamWrapper::NAry(params) = syntax.wrapper(db) {
            function_signature_params(
                new_ctx.diagnostics,
                new_ctx.db,
                new_ctx.resolver,
                params.params(db).elements(db),
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
        if let Some(param_types) = params_tuple_ty
            && let Err(err_set) = new_ctx.resolver.inference().conform_ty(closure_type, param_types)
        {
            new_ctx.resolver.inference().report_on_pending_error(
                err_set,
                new_ctx.diagnostics,
                syntax.stable_ptr(db).untyped(),
            );
        }

        params.iter().filter(|param| param.mutability == Mutability::Reference).for_each(|param| {
            new_ctx.diagnostics.report(param.stable_ptr(ctx.db), RefClosureParam);
        });

        new_ctx.variable_tracker.extend_from_environment(&new_ctx.environment);

        let return_type = match syntax.ret_ty(db) {
            OptionReturnTypeClause::ReturnTypeClause(ty_syntax) => resolve_type_ex(
                new_ctx.db,
                new_ctx.diagnostics,
                new_ctx.resolver,
                &ty_syntax.ty(db),
                ResolutionContext::Statement(&mut new_ctx.environment),
            ),
            OptionReturnTypeClause::Empty(missing) => {
                new_ctx.resolver.inference().new_type_var(Some(missing.stable_ptr(db).untyped()))
            }
        };

        let old_inner_ctx = new_ctx
            .inner_ctx
            .replace(InnerContext { return_type, kind: InnerContextKind::Closure });
        let body = match syntax.expr(db) {
            ast::Expr::Block(syntax) => compute_closure_body_semantic(new_ctx, syntax),
            _ => compute_expr_semantic(new_ctx, &syntax.expr(db)).id,
        };
        std::mem::replace(&mut new_ctx.inner_ctx, old_inner_ctx).unwrap();
        let mut inference = new_ctx.resolver.inference();
        let _ = inference.conform_ty_for_diag(
            new_ctx.arenas.exprs[body].ty(),
            return_type,
            new_ctx.diagnostics,
            || match syntax.ret_ty(ctx.db).stable_ptr(db).lookup(ctx.db) {
                OptionReturnTypeClause::Empty(_) => syntax.expr(db).stable_ptr(db).untyped(),
                OptionReturnTypeClause::ReturnTypeClause(return_type_clause) => {
                    return_type_clause.ty(ctx.db).stable_ptr(db).untyped()
                }
            },
            |actual_ty, expected_ty| WrongReturnType { expected_ty, actual_ty },
        );
        (params, return_type, body)
    });
    let parent_function = match ctx.function_id {
        ContextFunction::Global => {
            Maybe::Err(ctx.diagnostics.report(syntax.stable_ptr(db), ClosureInGlobalScope))
        }
        ContextFunction::Function(function_id) => function_id,
    };
    if matches!(ctx.function_id, ContextFunction::Global) {
        ctx.diagnostics.report(syntax.stable_ptr(db), ClosureInGlobalScope);
    }

    let mut usages = Usages { usages: Default::default() };
    let usage = usages.handle_closure(&ctx.arenas, &params, body);
    let mut reported = UnorderedHashSet::<_>::default();
    // TODO(TomerStarkware): Add support for capturing mutable variables when then we have borrow.
    for (captured_var, expr) in
        chain!(usage.usage.iter(), usage.snap_usage.iter(), usage.changes.iter())
    {
        let base_var = &captured_var.base_var();
        if ctx.variable_tracker.is_mut(base_var) && reported.insert(expr.stable_ptr()) {
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
        wrapper_location: StableLocation::new(syntax.wrapper(db).stable_ptr(db).into()),
    })
    .intern(ctx.db);

    Ok(Expr::ExprClosure(ExprClosure {
        body,
        params: params.to_vec(),
        stable_ptr: syntax.stable_ptr(db).into(),
        ty,
    }))
}

/// Computes the semantic model for a body of a closure.
fn compute_closure_body_semantic<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    syntax: ast::ExprBlock<'db>,
) -> ExprId {
    let db = ctx.db;
    let (statements, tail) = statements_and_tail(db, syntax.statements(db));
    let mut statements_semantic = vec![];
    compute_statements_semantic_and_extend(ctx, statements, &mut statements_semantic);
    let tail_semantic_expr =
        tail.map(|tail| compute_tail_semantic(ctx, &tail, &mut statements_semantic));
    let ty = block_ty(ctx, &statements_semantic, &tail_semantic_expr);
    ctx.arenas.exprs.alloc(Expr::Block(ExprBlock {
        statements: statements_semantic,
        tail: tail_semantic_expr.map(|expr| expr.id),
        ty,
        stable_ptr: syntax.stable_ptr(db).into(),
    }))
}

/// Computes the semantic model of an expression of type [ast::ExprErrorPropagate].
fn compute_expr_error_propagate_semantic<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    syntax: &ast::ExprErrorPropagate<'db>,
) -> Maybe<Expr<'db>> {
    let db = ctx.db;

    let return_type = ctx.get_return_type().ok_or_else(|| {
        ctx.diagnostics.report(
            syntax.stable_ptr(db),
            UnsupportedOutsideOfFunction(UnsupportedOutsideOfFunctionFeatureName::ErrorPropagate),
        )
    })?;

    let func_err_prop_ty = unwrap_error_propagation_type(ctx.db, return_type).ok_or_else(|| {
        ctx.diagnostics.report(syntax.stable_ptr(db), ReturnTypeNotErrorPropagateType)
    })?;

    // `inner_expr` is the expr inside the `?`.
    let inner_expr = match &func_err_prop_ty {
        crate::corelib::ErrorPropagationType::Option { .. } => {
            compute_expr_semantic(ctx, &syntax.expr(db))
        }
        crate::corelib::ErrorPropagationType::Result { .. } => {
            compute_expr_semantic(ctx, &syntax.expr(db))
        }
    };
    let func_err_variant = func_err_prop_ty.err_variant();

    // Runs solver to get as much info as possible about the return type.
    ctx.resolver.inference().solve().ok();
    let inner_expr_ty = ctx.reduce_ty(inner_expr.ty());
    inner_expr_ty.check_not_missing(ctx.db)?;
    let inner_expr_err_prop_ty =
        unwrap_error_propagation_type(ctx.db, inner_expr_ty).ok_or_else(|| {
            ctx.diagnostics
                .report(syntax.stable_ptr(db), ErrorPropagateOnNonErrorType(inner_expr_ty))
        })?;
    let inner_expr_err_variant = inner_expr_err_prop_ty.err_variant();

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
            syntax.stable_ptr(db),
            IncompatibleErrorPropagateType {
                return_ty: return_type,
                err_ty: inner_expr_err_variant.ty,
            },
        );
    }
    Ok(Expr::PropagateError(ExprPropagateError {
        inner: inner_expr.id,
        ok_variant: *inner_expr_err_prop_ty.ok_variant(),
        err_variant: *inner_expr_err_variant,
        func_err_variant: *func_err_variant,
        stable_ptr: syntax.stable_ptr(db).into(),
    }))
}

/// Computes the semantic model of an expression of type [ast::ExprIndexed].
fn compute_expr_indexed_semantic<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    syntax: &ast::ExprIndexed<'db>,
) -> Maybe<Expr<'db>> {
    let db = ctx.db;
    let expr = compute_expr_semantic(ctx, &syntax.expr(db));
    let index_expr_syntax = &syntax.index_expr(db);
    let index_expr = compute_expr_semantic(ctx, index_expr_syntax);
    if !ctx.reduce_ty(expr.ty()).is_var_free(ctx.db) {
        // Make sure the maximal amount of types is known when trying to access. Ignoring the
        // returned value, as any errors will be reported later.
        ctx.resolver.inference().solve().ok();
    }
    let info = ctx.db.core_info();
    let candidate_traits = [info.index_trt, info.index_view_trt];
    let (function_id, _, fixed_expr, mutability) = compute_method_function_call_data(
        ctx,
        &candidate_traits[..],
        SmolStrId::from(db, "index"),
        expr,
        syntax.stable_ptr(db).untyped(),
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
        syntax.stable_ptr(db),
        index_expr_syntax.stable_ptr(db),
    )
}

/// Computes the data needed for a method function call, and similar exprs (index operator). Method
/// call and Index operator differs in the diagnostics they emit. The function returns the
/// function_id to call, the trait containing the function, the self argument, with snapshots added
/// if needed, and the mutability of the self argument.
#[expect(clippy::too_many_arguments)]
fn compute_method_function_call_data<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    candidate_traits: &[TraitId<'db>],
    func_name: SmolStrId<'db>,
    self_expr: ExprAndId<'db>,
    method_syntax: SyntaxStablePtrId<'db>,
    generic_args_syntax: Option<Vec<ast::GenericArg<'db>>>,
    no_implementation_diagnostic: impl Fn(
        TypeId<'db>,
        SmolStrId<'db>,
        TraitInferenceErrors<'db>,
    ) -> Option<SemanticDiagnosticKind<'db>>,
    multiple_trait_diagnostic: fn(
        TypeId<'db>,
        TraitFunctionId<'db>,
        TraitFunctionId<'db>,
    ) -> Option<SemanticDiagnosticKind<'db>>,
) -> Maybe<(FunctionId<'db>, TraitId<'db>, ExprAndId<'db>, Mutability)> {
    let expr_ptr = self_expr.stable_ptr();
    let self_ty = ctx.reduce_ty(self_expr.ty());
    // Inference errors found when looking for candidates. Only relevant in the case of 0 candidates
    // found. If >0 candidates are found these are ignored as they may describe, e.g., "errors"
    // indicating certain traits/impls/functions don't match, which is OK as we only look for one.
    let mut inference_errors = vec![];
    let (candidates, mut fixed_expr, fixed_ty, deref_used) = get_method_function_candidates(
        ctx,
        candidate_traits,
        func_name,
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
    let first_param = signature.params.first().unwrap();

    if deref_used && first_param.mutability == Mutability::Reference {
        return Err(no_implementation_diagnostic(
            self_ty,
            func_name,
            TraitInferenceErrors { traits_and_errors: inference_errors },
        )
        .map(|diag| ctx.diagnostics.report(method_syntax, diag))
        .unwrap_or_else(skip_diagnostic));
    }

    for _ in 0..n_snapshots {
        let ty = TypeLongId::Snapshot(fixed_expr.ty()).intern(ctx.db);
        let expr = Expr::Snapshot(ExprSnapshot { inner: fixed_expr.id, ty, stable_ptr: expr_ptr });
        fixed_expr = ExprAndId { expr: expr.clone(), id: ctx.arenas.exprs.alloc(expr) };
    }

    Ok((function_id, trait_function_id.trait_id(ctx.db), fixed_expr, first_param.mutability))
}

/// Return candidates for method functions that match the given arguments.
/// Also returns the expression to be used as self for the method call, its type and whether deref
/// was used.
#[expect(clippy::too_many_arguments)]
fn get_method_function_candidates<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    candidate_traits: &[TraitId<'db>],
    func_name: SmolStrId<'db>,
    self_expr: ExprAndId<'db>,
    method_syntax: SyntaxStablePtrId<'db>,
    expr_ptr: ExprPtr<'db>,
    self_ty: TypeId<'db>,
    inference_errors: &mut Vec<(TraitFunctionId<'db>, InferenceError<'db>)>,
) -> Result<
    (Vec<TraitFunctionId<'db>>, ExprAndId<'db>, TypeId<'db>, bool),
    cairo_lang_diagnostics::DiagnosticAdded,
> {
    let mut candidates = filter_candidate_traits(
        ctx,
        inference_errors,
        self_ty,
        candidate_traits,
        func_name,
        method_syntax,
    );
    if !candidates.is_empty() {
        return Ok((candidates, self_expr, self_ty, false));
    }

    let deref_chain = ctx.db.deref_chain(
        self_ty,
        ctx.resolver.owning_crate_id,
        ctx.variable_tracker.is_mut_expr(&self_expr),
    )?;

    // Find the first deref that contains the method.
    let Some(last_deref_index) = deref_chain.derefs.iter().position(|deref_info| {
        candidates = filter_candidate_traits(
            ctx,
            inference_errors,
            deref_info.target_ty,
            candidate_traits,
            func_name,
            method_syntax,
        );
        !candidates.is_empty()
    }) else {
        // Not found in any deref.
        return Ok((candidates, self_expr, self_ty, false));
    };

    // Found in a deref - generating deref expressions.
    let mut fixed_expr = self_expr;
    for deref_info in &deref_chain.derefs[0..=last_deref_index] {
        let derefed_expr = expr_function_call(
            ctx,
            deref_info.function_id,
            vec![NamedArg(fixed_expr, None, deref_info.self_mutability)],
            method_syntax,
            expr_ptr,
        )?;

        fixed_expr =
            ExprAndId { expr: derefed_expr.clone(), id: ctx.arenas.exprs.alloc(derefed_expr) };
    }
    Ok((candidates, fixed_expr, deref_chain.derefs[last_deref_index].target_ty, true))
}

/// Computes the semantic model of a pattern.
/// Note that this pattern will always be "registered" in the arena, so it can be looked up in the
/// language server.
pub fn compute_pattern_semantic<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    syntax: &ast::Pattern<'db>,
    ty: TypeId<'db>,
    or_pattern_variables_map: &UnorderedHashMap<SmolStrId<'db>, LocalVariable<'db>>,
) -> PatternAndId<'db> {
    let pat = maybe_compute_pattern_semantic(ctx, syntax, ty, or_pattern_variables_map);
    let pat = pat.unwrap_or_else(|diag_added| {
        Pattern::Missing(PatternMissing {
            ty: TypeId::missing(ctx.db, diag_added),
            stable_ptr: syntax.stable_ptr(ctx.db),
            diag_added,
        })
    });
    let id = ctx.arenas.patterns.alloc(pat.clone());
    PatternAndId { pattern: pat, id }
}

/// Computes the semantic model of a pattern, or None if invalid.
fn maybe_compute_pattern_semantic<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    pattern_syntax: &ast::Pattern<'db>,
    ty: TypeId<'db>,
    or_pattern_variables_map: &UnorderedHashMap<SmolStrId<'db>, LocalVariable<'db>>,
) -> Maybe<Pattern<'db>> {
    // TODO(spapini): Check for missing type, and don't reemit an error.
    let db = ctx.db;
    let ty = ctx.reduce_ty(ty);
    let stable_ptr = pattern_syntax.stable_ptr(db).untyped();
    let pattern = match pattern_syntax {
        ast::Pattern::Underscore(otherwise_pattern) => Pattern::Otherwise(PatternOtherwise {
            ty,
            stable_ptr: otherwise_pattern.stable_ptr(db),
        }),
        ast::Pattern::Literal(literal_pattern) => {
            let literal = literal_to_semantic(ctx, literal_pattern)?;
            Pattern::Literal(PatternLiteral {
                literal,
                stable_ptr: literal_pattern.stable_ptr(db).into(),
            })
        }
        ast::Pattern::ShortString(short_string_pattern) => {
            let literal = short_string_to_semantic(ctx, short_string_pattern)?;
            Pattern::Literal(PatternLiteral {
                literal,
                stable_ptr: short_string_pattern.stable_ptr(db).into(),
            })
        }
        ast::Pattern::String(string_pattern) => {
            let string_literal = string_literal_to_semantic(ctx, string_pattern)?;
            Pattern::StringLiteral(PatternStringLiteral {
                string_literal,
                stable_ptr: string_pattern.stable_ptr(db).into(),
            })
        }
        ast::Pattern::Enum(enum_pattern) => {
            let path = enum_pattern.path(db);
            let item = ctx.resolver.resolve_concrete_path_ex(
                ctx.diagnostics,
                &path,
                NotFoundItemType::Identifier,
                ResolutionContext::Statement(&mut ctx.environment),
            )?;
            let concrete_variant = try_extract_matches!(item, ResolvedConcreteItem::Variant)
                .ok_or_else(|| ctx.diagnostics.report(path.stable_ptr(db), NotAVariant))?;

            let n_snapshots =
                validate_pattern_type_and_args(ctx, pattern_syntax, ty, concrete_variant)?;

            // Compute inner pattern.
            let inner_ty = wrap_in_snapshots(ctx.db, concrete_variant.ty, n_snapshots);

            let inner_pattern = match enum_pattern.pattern(db) {
                ast::OptionPatternEnumInnerPattern::Empty(_) => None,
                ast::OptionPatternEnumInnerPattern::PatternEnumInnerPattern(p) => {
                    let pattern = compute_pattern_semantic(
                        ctx,
                        &p.pattern(db),
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
                stable_ptr: enum_pattern.stable_ptr(db).into(),
            })
        }
        ast::Pattern::Path(path) => {
            let item_result = ctx.resolver.resolve_generic_path_with_args(
                &mut Default::default(),
                path,
                NotFoundItemType::Identifier,
                ResolutionContext::Statement(&mut ctx.environment),
            );
            if let Ok(ResolvedGenericItem::Variant(_)) = item_result {
                // If the path resolves to a variant, it might still be a generic param, so we
                // resolve it as a concrete path.
                // Resolveing as concrete path first might create vars which will not be inferred so
                // we use the generic path first.
                let item = ctx.resolver.resolve_concrete_path_ex(
                    &mut Default::default(),
                    path,
                    NotFoundItemType::Identifier,
                    ResolutionContext::Statement(&mut ctx.environment),
                );
                if let Ok(ResolvedConcreteItem::Variant(concrete_variant)) = item {
                    let _n_snapshots =
                        validate_pattern_type_and_args(ctx, pattern_syntax, ty, concrete_variant)?;
                    return Ok(Pattern::EnumVariant(PatternEnumVariant {
                        variant: concrete_variant,
                        inner_pattern: None,
                        ty,
                        stable_ptr: path.stable_ptr(db).into(),
                    }));
                }
            }

            // Paths with a single element are treated as identifiers, which will result in a
            // variable pattern if no matching enum variant is found. If a matching enum
            // variant exists, it is resolved to the corresponding concrete variant.
            let segments_var = path.segments(db);
            let mut segments = segments_var.elements(db);
            if segments.len() > 1 {
                return Err(ctx.diagnostics.report(path.stable_ptr(ctx.db), Unsupported));
            }
            // TODO(spapini): Make sure this is a simple identifier. In particular, no generics.
            let identifier = segments.next().unwrap().identifier_ast(db);
            create_variable_pattern(
                ctx,
                identifier,
                &[],
                ty,
                path.stable_ptr(db).into(),
                or_pattern_variables_map,
            )
        }
        ast::Pattern::Identifier(identifier) => create_variable_pattern(
            ctx,
            identifier.name(db),
            &identifier.modifiers(db).elements_vec(db),
            ty,
            identifier.stable_ptr(db).into(),
            or_pattern_variables_map,
        ),
        ast::Pattern::Struct(pattern_struct) => {
            let pattern_ty = try_extract_matches!(
                ctx.resolver.resolve_concrete_path_ex(
                    ctx.diagnostics,
                    &pattern_struct.path(db),
                    NotFoundItemType::Type,
                    ResolutionContext::Statement(&mut ctx.environment)
                )?,
                ResolvedConcreteItem::Type
            )
            .ok_or_else(|| {
                ctx.diagnostics.report(pattern_struct.path(db).stable_ptr(db), NotAType)
            })?;
            let inference = &mut ctx.resolver.inference();
            inference.conform_ty(pattern_ty, peel_snapshots(ctx.db, ty).1.intern(ctx.db)).map_err(
                |err_set| inference.report_on_pending_error(err_set, ctx.diagnostics, stable_ptr),
            )?;
            let ty = ctx.reduce_ty(ty);
            // Peel all snapshot wrappers.
            let (n_snapshots, long_ty) = peel_snapshots(ctx.db, ty);

            // Check that type is a struct, and get the concrete struct from it.
            let concrete_struct_id = try_extract_matches!(long_ty, TypeLongId::Concrete)
                .and_then(|c| try_extract_matches!(c, ConcreteTypeId::Struct))
                .ok_or(())
                .or_else(|_| {
                    // Don't add a diagnostic if the type is missing.
                    // A diagnostic should've already been added.
                    ty.check_not_missing(ctx.db)?;
                    Err(ctx
                        .diagnostics
                        .report(pattern_struct.stable_ptr(db), UnexpectedStructPattern(ty)))
                })?;
            let params = pattern_struct.params(db);
            let pattern_param_asts = params.elements(db);
            let struct_id = concrete_struct_id.struct_id(ctx.db);
            let mut members = ctx.db.concrete_struct_members(concrete_struct_id)?.clone();
            let mut used_members = UnorderedHashSet::<_>::default();
            let mut get_member =
                |ctx: &mut ComputationContext<'db, '_>,
                 member_name: SmolStrId<'db>,
                 stable_ptr: SyntaxStablePtrId<'db>| {
                    let member = members.swap_remove(&member_name).on_none(|| {
                        ctx.diagnostics.report(
                            stable_ptr,
                            if used_members.contains(&member_name) {
                                StructMemberRedefinition { struct_id, member_name }
                            } else {
                                NoSuchStructMember { struct_id, member_name }
                            },
                        );
                    })?;
                    check_struct_member_is_visible(ctx, &member, stable_ptr, member_name);
                    used_members.insert(member_name);
                    Some(member)
                };
            let mut field_patterns = vec![];
            let mut has_tail = false;
            for pattern_param_ast in pattern_param_asts {
                match pattern_param_ast {
                    PatternStructParam::Single(single) => {
                        let name = single.name(db);
                        let name_id = name.text(ctx.db);
                        let Some(member) = get_member(ctx, name_id, name.stable_ptr(db).untyped())
                        else {
                            continue;
                        };
                        let ty = wrap_in_snapshots(ctx.db, member.ty, n_snapshots);
                        let pattern = create_variable_pattern(
                            ctx,
                            name,
                            &single.modifiers(db).elements_vec(db),
                            ty,
                            single.stable_ptr(db).into(),
                            or_pattern_variables_map,
                        );
                        field_patterns.push((ctx.arenas.patterns.alloc(pattern), member));
                    }
                    PatternStructParam::WithExpr(with_expr) => {
                        let name = with_expr.name(db);
                        let name_id = name.text(ctx.db);
                        let Some(member) = get_member(ctx, name_id, name.stable_ptr(db).untyped())
                        else {
                            continue;
                        };
                        let ty = wrap_in_snapshots(ctx.db, member.ty, n_snapshots);
                        let pattern = compute_pattern_semantic(
                            ctx,
                            &with_expr.pattern(db),
                            ty,
                            or_pattern_variables_map,
                        );
                        field_patterns.push((pattern.id, member));
                    }
                    PatternStructParam::Tail(_) => {
                        has_tail = true;
                    }
                }
            }
            if !has_tail {
                for (member_name, _) in members.iter() {
                    ctx.diagnostics
                        .report(pattern_struct.stable_ptr(db), MissingMember(*member_name));
                }
            }
            Pattern::Struct(PatternStruct {
                concrete_struct_id,
                field_patterns,
                ty,
                n_snapshots,
                stable_ptr: pattern_struct.stable_ptr(db),
            })
        }
        ast::Pattern::Tuple(_) => compute_tuple_like_pattern_semantic(
            ctx,
            pattern_syntax,
            ty,
            or_pattern_variables_map,
            |ty: TypeId<'db>| UnexpectedTuplePattern(ty),
            |expected, actual| WrongNumberOfTupleElements { expected, actual },
        ),
        ast::Pattern::FixedSizeArray(_) => compute_tuple_like_pattern_semantic(
            ctx,
            pattern_syntax,
            ty,
            or_pattern_variables_map,
            |ty: TypeId<'db>| UnexpectedFixedSizeArrayPattern(ty),
            |expected, actual| WrongNumberOfFixedSizeArrayElements { expected, actual },
        ),
        ast::Pattern::False(pattern_false) => {
            let enum_expr = extract_matches!(
                false_literal_expr(ctx, pattern_false.stable_ptr(db).into()),
                Expr::EnumVariantCtor
            );

            extract_concrete_enum_from_pattern_and_validate(
                ctx,
                pattern_syntax,
                ty,
                enum_expr.variant.concrete_enum_id,
            )?;

            Pattern::EnumVariant(PatternEnumVariant {
                variant: enum_expr.variant,
                stable_ptr: pattern_false.stable_ptr(db).into(),
                ty,
                inner_pattern: None,
            })
        }
        ast::Pattern::True(pattern_true) => {
            let enum_expr = extract_matches!(
                true_literal_expr(ctx, pattern_true.stable_ptr(db).into()),
                Expr::EnumVariantCtor
            );
            extract_concrete_enum_from_pattern_and_validate(
                ctx,
                pattern_syntax,
                ty,
                enum_expr.variant.concrete_enum_id,
            )?;

            Pattern::EnumVariant(PatternEnumVariant {
                variant: enum_expr.variant,
                stable_ptr: pattern_true.stable_ptr(db).into(),
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
fn compute_tuple_like_pattern_semantic<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    pattern_syntax: &ast::Pattern<'db>,
    ty: TypeId<'db>,
    or_pattern_variables_map: &UnorderedHashMap<SmolStrId<'db>, LocalVariable<'db>>,
    unexpected_pattern: fn(TypeId<'db>) -> SemanticDiagnosticKind<'db>,
    wrong_number_of_elements: fn(usize, usize) -> SemanticDiagnosticKind<'db>,
) -> Pattern<'db> {
    let db = ctx.db;
    let (n_snapshots, mut long_ty) =
        match finalized_snapshot_peeled_ty(ctx, ty, pattern_syntax.stable_ptr(db)) {
            Ok(value) => value,
            Err(diag_added) => (0, TypeLongId::Missing(diag_added)),
        };
    // Assert that the pattern is of the same type as the expr.
    match (pattern_syntax, &long_ty) {
        (_, TypeLongId::Var(_) | TypeLongId::Missing(_))
        | (ast::Pattern::Tuple(_), TypeLongId::Tuple(_))
        | (ast::Pattern::FixedSizeArray(_), TypeLongId::FixedSizeArray { .. }) => {}
        _ => {
            long_ty = TypeLongId::Missing(
                ctx.diagnostics.report(pattern_syntax.stable_ptr(db), unexpected_pattern(ty)),
            );
        }
    };
    let patterns_syntax = match pattern_syntax {
        ast::Pattern::Tuple(pattern_tuple) => pattern_tuple.patterns(db).elements_vec(db),
        ast::Pattern::FixedSizeArray(pattern_fixed_size_array) => {
            pattern_fixed_size_array.patterns(db).elements_vec(db)
        }
        _ => unreachable!(),
    };
    let mut inner_tys = match long_ty {
        TypeLongId::Tuple(inner_tys) => inner_tys,
        TypeLongId::FixedSizeArray { type_id: inner_ty, size } => {
            let size = if let ConstValue::Int(value, _) = size.long(db) {
                value.to_usize().expect("Fixed sized array size must always be usize.")
            } else {
                let inference = &mut ctx.resolver.inference();
                let expected_size =
                    ConstValue::Int(patterns_syntax.len().into(), get_usize_ty(db)).intern(db);
                if let Err(err) = inference.conform_const(size, expected_size) {
                    let _ = inference.report_on_pending_error(
                        err,
                        ctx.diagnostics,
                        pattern_syntax.stable_ptr(db).untyped(),
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
                    .map(|e| inference.new_type_var(Some(e.stable_ptr(db).untyped())))
                    .collect();
                (inner_tys.clone(), TypeLongId::Tuple(inner_tys))
            } else {
                let var = inference.new_type_var(Some(pattern_syntax.stable_ptr(db).untyped()));
                (
                    vec![var; patterns_syntax.len()],
                    TypeLongId::FixedSizeArray {
                        type_id: var,
                        size: ConstValue::Int(patterns_syntax.len().into(), get_usize_ty(db))
                            .intern(db),
                    },
                )
            };
            match inference.conform_ty(long_ty.intern(db), tuple_like_ty.intern(db)) {
                Ok(_) => {}
                Err(_) => unreachable!("As the type is a var, conforming should always succeed."),
            }
            inner_tys
        }
        TypeLongId::Missing(diag_added) => {
            let missing = TypeId::missing(db, diag_added);
            vec![missing; patterns_syntax.len()]
        }
        _ => unreachable!(),
    };
    let size = inner_tys.len();
    if size != patterns_syntax.len() {
        let diag_added = ctx.diagnostics.report(
            pattern_syntax.stable_ptr(db),
            wrong_number_of_elements(size, patterns_syntax.len()),
        );
        let missing = TypeId::missing(db, diag_added);

        inner_tys = vec![missing; patterns_syntax.len()];
    }
    let subpatterns = zip_eq(patterns_syntax, inner_tys)
        .map(|(pattern_ast, ty)| {
            let ty = wrap_in_snapshots(db, ty, n_snapshots);
            compute_pattern_semantic(ctx, &pattern_ast, ty, or_pattern_variables_map).id
        })
        .collect();
    match pattern_syntax {
        ast::Pattern::Tuple(syntax) => Pattern::Tuple(PatternTuple {
            field_patterns: subpatterns,
            ty,
            stable_ptr: syntax.stable_ptr(db),
        }),
        ast::Pattern::FixedSizeArray(syntax) => Pattern::FixedSizeArray(PatternFixedSizeArray {
            elements_patterns: subpatterns,
            ty,
            stable_ptr: syntax.stable_ptr(db),
        }),
        _ => unreachable!(),
    }
}

/// Validates that the semantic type of an enum pattern is an enum, and returns the concrete enum.
fn extract_concrete_enum_from_pattern_and_validate<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    pattern: &ast::Pattern<'db>,
    ty: TypeId<'db>,
    concrete_enum_id: ConcreteEnumId<'db>,
) -> Maybe<(ConcreteEnumId<'db>, usize)> {
    // Peel all snapshot wrappers.
    let (n_snapshots, long_ty) = finalized_snapshot_peeled_ty(ctx, ty, pattern.stable_ptr(ctx.db))?;

    // Check that type is an enum, and get the concrete enum from it.
    let concrete_enum = try_extract_matches!(long_ty, TypeLongId::Concrete)
        .and_then(|c| try_extract_matches!(c, ConcreteTypeId::Enum))
        .ok_or(())
        .or_else(|_| {
            // Don't add a diagnostic if the type is missing.
            // A diagnostic should've already been added.
            ty.check_not_missing(ctx.db)?;
            Err(ctx.diagnostics.report(pattern.stable_ptr(ctx.db), UnexpectedEnumPattern(ty)))
        })?;
    // Check that these are the same enums.
    let pattern_enum = concrete_enum_id.enum_id(ctx.db);
    if pattern_enum != concrete_enum.enum_id(ctx.db) {
        return Err(ctx.diagnostics.report(
            pattern.stable_ptr(ctx.db),
            WrongEnum { expected_enum: concrete_enum.enum_id(ctx.db), actual_enum: pattern_enum },
        ));
    }
    Ok((concrete_enum, n_snapshots))
}

/// Validates that the semantic type of an enum pattern is an enum.
/// After that finds the concrete variant in the pattern, and verifies it has args if needed.
/// Returns the number of snapshots.
fn validate_pattern_type_and_args<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    pattern: &ast::Pattern<'db>,
    ty: TypeId<'db>,
    concrete_variant: ConcreteVariant<'db>,
) -> Maybe<usize> {
    let db = ctx.db;

    let (concrete_enum, n_snapshots) = extract_concrete_enum_from_pattern_and_validate(
        ctx,
        pattern,
        ty,
        concrete_variant.concrete_enum_id,
    )?;

    if let Err(err_set) = ctx.resolver.inference().conform_generic_args(
        &concrete_variant.concrete_enum_id.long(db).generic_args,
        &concrete_enum.long(db).generic_args,
    ) {
        let diag_added = ctx.diagnostics.report(
            pattern.stable_ptr(db),
            InternalInferenceError(InferenceError::TypeKindMismatch {
                ty0: ty,
                ty1: TypeLongId::Concrete(ConcreteTypeId::Enum(concrete_variant.concrete_enum_id))
                    .intern(db),
            }),
        );
        ctx.resolver.inference().consume_reported_error(err_set, diag_added);
    };

    let generic_variant = db
        .variant_semantic(concrete_variant.concrete_enum_id.enum_id(db), concrete_variant.id)
        .expect("concrete variant has to exist");

    let needs_args = generic_variant.ty != unit_ty(db);
    let has_args = matches!(
        pattern,
        ast::Pattern::Enum(inner)
            if matches!(
                inner.pattern(db),
                ast::OptionPatternEnumInnerPattern::PatternEnumInnerPattern(_)
            )
    );

    if needs_args && !has_args {
        let path = match pattern {
            ast::Pattern::Enum(pattern) => pattern.path(db),
            ast::Pattern::Path(p) => p.clone(),
            _ => unreachable!("Expected enum pattern in variant extraction."),
        };
        ctx.diagnostics.report(pattern.stable_ptr(db), PatternMissingArgs(path));
    }

    Ok(n_snapshots)
}

/// Creates a local variable pattern.
fn create_variable_pattern<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    identifier: ast::TerminalIdentifier<'db>,
    modifier_list: &[ast::Modifier<'db>],
    ty: TypeId<'db>,
    stable_ptr: ast::PatternPtr<'db>,
    or_pattern_variables_map: &UnorderedHashMap<SmolStrId<'db>, LocalVariable<'db>>,
) -> Pattern<'db> {
    let db = ctx.db;

    let var_id = match or_pattern_variables_map.get(&identifier.text(db)) {
        Some(var) => var.id,
        None => LocalVarLongId(ctx.resolver.module_id, identifier.stable_ptr(db)).intern(ctx.db),
    };
    let is_mut = match compute_mutability(ctx.diagnostics, db, modifier_list) {
        Mutability::Immutable => false,
        Mutability::Mutable => true,
        Mutability::Reference => {
            ctx.diagnostics.report(identifier.stable_ptr(db), ReferenceLocalVariable);
            false
        }
    };
    let allow_unused = ctx
        .resolver
        .data
        .feature_config
        .allowed_lints
        .contains(&SmolStrId::from(db, UNUSED_VARIABLES));
    Pattern::Variable(PatternVariable {
        name: identifier.text(db),
        var: LocalVariable { id: var_id, ty, is_mut, allow_unused },
        stable_ptr,
    })
}

/// Creates a struct constructor semantic expression from its AST.
fn struct_ctor_expr<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    ctor_syntax: &ast::ExprStructCtorCall<'db>,
) -> Maybe<Expr<'db>> {
    let db = ctx.db;
    let path = ctor_syntax.path(db);

    // Extract struct.
    let ty = resolve_type_ex(
        db,
        ctx.diagnostics,
        ctx.resolver,
        &ast::Expr::Path(path.clone()),
        ResolutionContext::Statement(&mut ctx.environment),
    );
    ty.check_not_missing(db)?;

    let concrete_struct_id = *try_extract_matches!(ty.long(ctx.db), TypeLongId::Concrete)
        .and_then(|c| try_extract_matches!(c, ConcreteTypeId::Struct))
        .ok_or_else(|| ctx.diagnostics.report(path.stable_ptr(db), NotAStruct))?;

    if ty.is_phantom(db) {
        ctx.diagnostics.report(ctor_syntax.stable_ptr(db), CannotCreateInstancesOfPhantomTypes);
    }

    let members = db.concrete_struct_members(concrete_struct_id)?;
    let mut member_exprs: OrderedHashMap<MemberId<'_>, Option<ExprId>> = OrderedHashMap::default();
    let mut base_struct = None;

    for (index, arg) in ctor_syntax.arguments(db).arguments(db).elements(db).enumerate() {
        // TODO: Extract to a function for results.
        match arg {
            ast::StructArg::StructArgSingle(arg) => {
                let arg_identifier = arg.identifier(db);
                let arg_name = arg_identifier.text(db);

                // Find struct member by name.
                let Some(member) = members.get(&arg_name) else {
                    ctx.diagnostics.report(arg_identifier.stable_ptr(db), UnknownMember);
                    continue;
                };
                check_struct_member_is_visible(
                    ctx,
                    member,
                    arg_identifier.stable_ptr(db).untyped(),
                    arg_name,
                );

                // Extract expression.
                let arg_expr = match arg.arg_expr(db) {
                    ast::OptionStructArgExpr::Empty(_) => {
                        let Ok(expr) = resolve_variable_by_name(
                            ctx,
                            &arg_identifier,
                            path.stable_ptr(db).into(),
                        ) else {
                            // Insert only the member id, for correct duplicate member reporting.
                            if member_exprs.insert(member.id, None).is_some() {
                                ctx.diagnostics.report(
                                    arg_identifier.stable_ptr(db),
                                    MemberSpecifiedMoreThanOnce,
                                );
                            }
                            continue;
                        };
                        ExprAndId { expr: expr.clone(), id: ctx.arenas.exprs.alloc(expr) }
                    }
                    ast::OptionStructArgExpr::StructArgExpr(arg_expr) => {
                        compute_expr_semantic(ctx, &arg_expr.expr(db))
                    }
                };

                // Insert and check for duplicates.
                if member_exprs.insert(member.id, Some(arg_expr.id)).is_some() {
                    ctx.diagnostics
                        .report(arg_identifier.stable_ptr(db), MemberSpecifiedMoreThanOnce);
                }

                // Check types.
                let inference = &mut ctx.resolver.inference();
                if inference
                    .conform_ty_for_diag(
                        arg_expr.ty(),
                        member.ty,
                        ctx.diagnostics,
                        || arg_expr.stable_ptr().untyped(),
                        |actual_ty, expected_ty| WrongArgumentType { expected_ty, actual_ty },
                    )
                    .is_err()
                {
                    continue;
                }
            }
            ast::StructArg::StructArgTail(base_struct_syntax) => {
                // TODO(TomerStarkware): remove tail expression from argument list.
                if index != ctor_syntax.arguments(db).arguments(db).elements(db).len() - 1 {
                    ctx.diagnostics.report(
                        base_struct_syntax.stable_ptr(db),
                        StructBaseStructExpressionNotLast,
                    );
                    continue;
                }
                let base_struct_expr =
                    compute_expr_semantic(ctx, &base_struct_syntax.expression(db));
                let inference = &mut ctx.resolver.inference();
                if inference
                    .conform_ty_for_diag(
                        base_struct_expr.ty(),
                        ty,
                        ctx.diagnostics,
                        || base_struct_syntax.expression(db).stable_ptr(db).untyped(),
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
    let missing_members: Vec<_> = members
        .iter()
        .filter(|(_, member)| !member_exprs.contains_key(&member.id))
        .map(|(member_name, member)| (*member_name, member))
        .collect();
    for (member_name, member) in missing_members {
        if base_struct.is_some() {
            check_struct_member_is_visible(
                ctx,
                member,
                base_struct.clone().unwrap().1.stable_ptr(db).untyped(),
                member_name,
            );
        } else {
            ctx.diagnostics.report(ctor_syntax.stable_ptr(db), MissingMember(member_name));
        }
    }
    if members.len() == member_exprs.len()
        && let Some((_, base_struct_syntax)) = base_struct
    {
        return Err(ctx
            .diagnostics
            .report(base_struct_syntax.stable_ptr(db), StructBaseStructExpressionNoEffect));
    }
    Ok(Expr::StructCtor(ExprStructCtor {
        concrete_struct_id,
        members: member_exprs.into_iter().filter_map(|(x, y)| Some((y?, x))).collect(),
        base_struct: base_struct.map(|(x, _)| x),
        ty: TypeLongId::Concrete(ConcreteTypeId::Struct(concrete_struct_id)).intern(db),
        stable_ptr: ctor_syntax.stable_ptr(db).into(),
    }))
}

/// Splits the statements into a tail expression (if exists) and the rest of the statements.
/// A tail expression is the last statement in the list, if it is an expression and
/// it does not end with a semicolon.
fn statements_and_tail<'a>(
    db: &'a dyn Database,
    syntax: ast::StatementList<'a>,
) -> (impl Iterator<Item = ast::Statement<'a>> + 'a, Option<ast::StatementExpr<'a>>) {
    let mut statements = syntax.elements(db);
    let last = statements.next_back();
    if let Some(ast::Statement::Expr(expr)) = &last {
        // If the last statement is an expression, check if it is a tail expression.
        if matches!(expr.semicolon(db), ast::OptionTerminalSemicolon::Empty(_)) {
            return (chain!(statements, None), Some(expr.clone()));
        }
    }
    (chain!(statements, last), None)
}

/// Creates a new numeric literal expression.
fn new_literal_expr<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    ty: Option<SmolStrId<'db>>,
    value: BigInt,
    stable_ptr: ExprPtr<'db>,
) -> Maybe<ExprLiteral<'db>> {
    if let Some(ty_str) = ty {
        // Requires specific blocking as `NonZero` now has NumericLiteral support.
        if ty_str.long(ctx.db) == "NonZero" {
            return Err(ctx.diagnostics.report(
                stable_ptr.untyped(),
                SemanticDiagnosticKind::WrongNumberOfArguments { expected: 1, actual: 0 },
            ));
        }
        let ty = try_get_core_ty_by_name(ctx.db, ty_str, vec![])
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
fn literal_to_semantic<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    literal_syntax: &ast::TerminalLiteralNumber<'db>,
) -> Maybe<ExprLiteral<'db>> {
    let db = ctx.db;

    let (value, ty) = literal_syntax.numeric_value_and_suffix(db).unwrap_or_default();

    new_literal_expr(ctx, ty, value, literal_syntax.stable_ptr(db).into())
}

/// Creates the semantic model of a short string from its AST.
fn short_string_to_semantic<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    short_string_syntax: &ast::TerminalShortString<'db>,
) -> Maybe<ExprLiteral<'db>> {
    let db = ctx.db;

    let value = short_string_syntax.numeric_value(db).unwrap_or_default();
    let suffix = short_string_syntax.suffix(db);
    new_literal_expr(
        ctx,
        suffix.map(|s| SmolStrId::from(db, s)),
        value,
        short_string_syntax.stable_ptr(db).into(),
    )
}

/// Creates a new string literal expression.
fn new_string_literal_expr<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    value: String,
    stable_ptr: ExprPtr<'db>,
) -> Maybe<ExprStringLiteral<'db>> {
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
fn string_literal_to_semantic<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    string_syntax: &ast::TerminalString<'db>,
) -> Maybe<ExprStringLiteral<'db>> {
    let db = ctx.db;
    let stable_ptr = string_syntax.stable_ptr(db);

    let value = string_syntax.string_value(db).unwrap_or_default();
    // TODO(yuval): support prefixes/suffixes for explicit types?

    new_string_literal_expr(ctx, value, stable_ptr.into())
}
/// Given path, if it's a single segment or a $callsite-prefixed segment,
/// returns a tuple of (identifier, is_callsite_prefixed).
/// Otherwise, returns None.
fn try_extract_identifier_from_path<'a>(
    db: &'a dyn Database,
    path: &ast::ExprPath<'a>,
) -> Option<(TerminalIdentifier<'a>, bool)> {
    let segments_var = path.segments(db);
    let mut segments = segments_var.elements(db);
    require(segments.len() <= 2)?;
    let Some(PathSegment::Simple(first)) = segments.next() else {
        return None;
    };
    let Some(second) = segments.next() else {
        return Some((first.ident(db), false));
    };
    let second = try_extract_matches!(second, PathSegment::Simple)?;
    if first.identifier(db).long(db) == MACRO_CALL_SITE && path.placeholder_marker(db).is_some() {
        Some((second.ident(db), true))
    } else {
        None
    }
}

/// Given an expression syntax, if it's an identifier, returns it. Otherwise, returns the proper
/// error.
fn expr_as_identifier<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    path: &ast::ExprPath<'db>,
    db: &'db dyn Database,
) -> Maybe<SmolStrId<'db>> {
    let segments_var = path.segments(db);
    let mut segments = segments_var.elements(db);
    if segments.len() == 1 {
        Ok(segments.next().unwrap().identifier(db))
    } else {
        Err(ctx.diagnostics.report(path.stable_ptr(db), InvalidMemberExpression))
    }
}

// TODO(spapini): Consider moving some checks here to the responsibility of the parser.
/// Computes the semantic expression for a dot expression.
fn dot_expr<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    lexpr: ExprAndId<'db>,
    rhs_syntax: ast::Expr<'db>,
    stable_ptr: ast::ExprPtr<'db>,
) -> Maybe<Expr<'db>> {
    // Find MemberId.
    match rhs_syntax {
        ast::Expr::Path(expr) => member_access_expr(ctx, lexpr, expr, stable_ptr),
        ast::Expr::FunctionCall(expr) => method_call_expr(ctx, lexpr, expr, stable_ptr),
        _ => Err(ctx.diagnostics.report(rhs_syntax.stable_ptr(ctx.db), InvalidMemberExpression)),
    }
}

/// Finds all the trait ids usable in the current context.
fn traits_in_context<'db>(
    ctx: &mut ComputationContext<'db, '_>,
) -> OrderedHashMap<TraitId<'db>, LookupItemId<'db>> {
    let mut traits = ctx.db.module_usable_trait_ids(ctx.resolver.prelude_submodule()).clone();
    traits.extend(
        ctx.db.module_usable_trait_ids(ctx.resolver.module_id).iter().map(|(k, v)| (*k, *v)),
    );
    traits
}

/// Computes the semantic model of a method call expression (e.g. "expr.method(..)").
/// Finds all traits with at least one candidate impl with a matching `self` param.
/// If more/less than 1 such trait exists, fails.
fn method_call_expr<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    lexpr: ExprAndId<'db>,
    expr: ast::ExprFunctionCall<'db>,
    stable_ptr: ast::ExprPtr<'db>,
) -> Maybe<Expr<'db>> {
    // TODO(spapini): Add ctx.module_id.
    // TODO(spapini): Look also in uses.
    let db = ctx.db;
    let path = expr.path(db);
    let Some([segment]) = path.segments(db).elements(db).collect_array() else {
        return Err(ctx.diagnostics.report(expr.stable_ptr(ctx.db), InvalidMemberExpression));
    };
    let func_name = segment.identifier(db);
    let generic_args_syntax = segment.generic_args(db);

    if !ctx.reduce_ty(lexpr.ty()).is_var_free(ctx.db) {
        // Run solver to get as much info on the type as possible.
        // Ignore the result of the `solve()` call - the error, if any, will be
        // reported later.
        ctx.resolver.inference().solve().ok();
    }

    let mut candidate_traits = traits_in_context(ctx);

    // Add traits from impl generic args in the context.
    for generic_param in &ctx.resolver.data.generic_params {
        if generic_param.kind(ctx.db) == GenericKind::Impl {
            let Ok(trait_id) = ctx.db.generic_impl_param_trait(*generic_param) else {
                continue;
            };
            candidate_traits
                .insert(trait_id, LookupItemId::ModuleItem(ModuleItemId::Trait(trait_id)));
        }
    }

    // Extracting the possible traits that should be imported, in order to use the method.
    let module_id = ctx.resolver.module_id;
    let lookup_context = ctx.resolver.impl_lookup_context();
    let lexpr_stable_ptr = lexpr.stable_ptr().untyped();
    let db = ctx.db;
    let (function_id, actual_trait_id, fixed_lexpr, mutability) =
        compute_method_function_call_data(
            ctx,
            candidate_traits.keys().copied().collect_vec().as_slice(),
            func_name,
            lexpr,
            path.stable_ptr(db).untyped(),
            generic_args_syntax,
            |ty, method_name, inference_errors| {
                let relevant_traits = if !inference_errors.is_empty() {
                    vec![]
                } else {
                    match_method_to_traits(
                        db,
                        ty,
                        method_name,
                        lookup_context,
                        module_id,
                        lexpr_stable_ptr,
                    )
                };
                Some(CannotCallMethod { ty, method_name, inference_errors, relevant_traits })
            },
            |_, trait_function_id0, trait_function_id1| {
                Some(AmbiguousTrait { trait_function_id0, trait_function_id1 })
            },
        )
        .inspect_err(|_| {
            // Getting better diagnostics and usage metrics for the function args.
            for arg in expr.arguments(db).arguments(db).elements(db) {
                compute_named_argument_clause(ctx, arg, None);
            }
        })?;

    if let Ok(Some(trait_item_info)) = ctx.db.trait_item_info_by_name(actual_trait_id, func_name) {
        ctx.resolver.validate_feature_constraints(
            ctx.diagnostics,
            &segment.identifier_ast(db),
            &trait_item_info,
        );
    }
    if let LookupItemId::ModuleItem(item_id) = candidate_traits[&actual_trait_id] {
        ctx.resolver.insert_used_use(item_id);
    }
    ctx.resolver.data.resolved_items.mark_concrete(
        ctx.db,
        &segment,
        ResolvedConcreteItem::Function(function_id),
    );

    // Note there may be n+1 arguments for n parameters, if the last one is a coupon.
    let arguments_var = expr.arguments(db).arguments(db);
    let mut args_iter = arguments_var.elements(db);
    // Self argument.
    let mut named_args = vec![NamedArg(fixed_lexpr, None, mutability)];
    // Other arguments.
    let closure_params: OrderedHashMap<TypeId<'db>, TypeId<'_>> =
        ctx.db.concrete_function_closure_params(function_id)?;
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

    expr_function_call(ctx, function_id, named_args, expr.stable_ptr(db), stable_ptr)
}

/// Computes the semantic model of a member access expression (e.g. "expr.member").
fn member_access_expr<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    lexpr: ExprAndId<'db>,
    rhs_syntax: ast::ExprPath<'db>,
    stable_ptr: ast::ExprPtr<'db>,
) -> Maybe<Expr<'db>> {
    let db = ctx.db;

    // Find MemberId.
    let member_name = expr_as_identifier(ctx, &rhs_syntax, db)?;
    let (n_snapshots, long_ty) =
        finalized_snapshot_peeled_ty(ctx, lexpr.ty(), rhs_syntax.stable_ptr(db))?;

    match &long_ty {
        TypeLongId::Concrete(_) | TypeLongId::Tuple(_) | TypeLongId::FixedSizeArray { .. } => {
            let Some(EnrichedTypeMemberAccess { member, deref_functions }) =
                get_enriched_type_member_access(ctx, lexpr.clone(), stable_ptr, member_name)?
            else {
                return Err(ctx.diagnostics.report(
                    rhs_syntax.stable_ptr(db),
                    NoSuchTypeMember { ty: long_ty.intern(ctx.db), member_name },
                ));
            };
            check_struct_member_is_visible(
                ctx,
                &member,
                rhs_syntax.stable_ptr(db).untyped(),
                member_name,
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
            let mut derefed_expr: ExprAndId<'_> = lexpr;
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
            let (_, long_ty) =
                finalized_snapshot_peeled_ty(ctx, derefed_expr.ty(), rhs_syntax.stable_ptr(db))?;
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
            Err(ctx.diagnostics.report(rhs_syntax.stable_ptr(db), Unsupported))
        }
        TypeLongId::Closure(_) => {
            Err(ctx.diagnostics.report(rhs_syntax.stable_ptr(db), Unsupported))
        }
        TypeLongId::ImplType(impl_type_id) => {
            unreachable!("Impl type should've been reduced {:?}.", impl_type_id.debug(ctx.db))
        }
        TypeLongId::Var(_) => Err(ctx.diagnostics.report(
            rhs_syntax.stable_ptr(db),
            InternalInferenceError(InferenceError::TypeNotInferred(long_ty.intern(ctx.db))),
        )),
        TypeLongId::GenericParameter(_) | TypeLongId::Coupon(_) => Err(ctx.diagnostics.report(
            rhs_syntax.stable_ptr(db),
            TypeHasNoMembers { ty: long_ty.intern(ctx.db), member_name },
        )),
        TypeLongId::Missing(diag_added) => Err(*diag_added),
    }
}

/// Returns the member and the deref operations needed for its access.
///
/// Enriched members include both direct members (in case of a struct), and members of derefed types
/// if the type implements the Deref trait into a struct.
fn get_enriched_type_member_access<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    expr: ExprAndId<'db>,
    stable_ptr: ast::ExprPtr<'db>,
    accessed_member_name: SmolStrId<'db>,
) -> Maybe<Option<EnrichedTypeMemberAccess<'db>>> {
    let mut ty = ctx.reduce_ty(expr.ty());
    if !ty.is_var_free(ctx.db) {
        // Run solver to get as much info on the type as possible.
        // Ignore the result of the `solve()` call - the error, if any, will be
        // reported later.
        ctx.resolver.inference().solve().ok();
        ty = ctx.reduce_ty(ty);
    }

    let is_mut_var = ctx.variable_tracker.is_mut_expr(&expr);
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
                    if let Some(member) = members.get(&accessed_member_name) {
                        // Found direct member access - so directly returning it.
                        return Ok(Some(EnrichedTypeMemberAccess {
                            member: member.clone(),
                            deref_functions: vec![],
                        }));
                    }
                    members.iter().map(|(k, v)| (*k, (v.clone(), 0))).collect()
                } else {
                    Default::default()
                };

            EnrichedMembers {
                members,
                deref_chain: ctx
                    .db
                    .deref_chain(ty, ctx.resolver.owning_crate_id, is_mut_var)?
                    .derefs
                    .clone(),
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
fn enrich_members<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    enriched_members: &mut EnrichedMembers<'db>,
    stable_ptr: ast::ExprPtr<'db>,
    accessed_member_name: SmolStrId<'db>,
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
                enriched.entry(*member_name).or_insert_with(|| (member.clone(), *explored_derefs));
            }
            // If member is contained we can stop the calculation post the lookup.
            if members.contains_key(&accessed_member_name) {
                // Found member, so exploration isn't done.
                break;
            }
        }
    }
    Ok::<(), cairo_lang_diagnostics::DiagnosticAdded>(())
}

/// Peels snapshots from a type and making sure it is fully not a variable type.
fn finalized_snapshot_peeled_ty<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    ty: TypeId<'db>,
    stable_ptr: impl Into<SyntaxStablePtrId<'db>>,
) -> Maybe<(usize, TypeLongId<'db>)> {
    let ty = ctx.reduce_ty(ty);
    let (base_snapshots, mut long_ty) = peel_snapshots(ctx.db, ty);
    if let TypeLongId::ImplType(impl_type_id) = long_ty {
        let inference = &mut ctx.resolver.inference();
        let Ok(ty) = inference.reduce_impl_ty(impl_type_id) else {
            return Err(ctx
                .diagnostics
                .report(stable_ptr, InternalInferenceError(InferenceError::TypeNotInferred(ty))));
        };
        long_ty = ty.long(ctx.db).clone();
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
fn resolve_expr_path<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    path: &ast::ExprPath<'db>,
) -> Maybe<Expr<'db>> {
    let db = ctx.db;
    if path.segments(db).elements(db).len() == 0 {
        return Err(ctx.diagnostics.report(path.stable_ptr(db), Unsupported));
    }

    // Check if this is a variable.
    if let Some((identifier, is_callsite_prefixed)) = try_extract_identifier_from_path(db, path) {
        let variable_name = identifier.text(ctx.db);
        if let Some(res) = get_binded_expr_by_name(
            ctx,
            variable_name,
            is_callsite_prefixed,
            path.stable_ptr(ctx.db).into(),
        ) {
            match res.clone() {
                Expr::Var(expr_var) => {
                    let item = ResolvedGenericItem::Variable(expr_var.var);
                    ctx.resolver
                        .data
                        .resolved_items
                        .generic
                        .insert(identifier.stable_ptr(db), item);
                }
                Expr::Constant(expr_const) => {
                    let item = ResolvedConcreteItem::Constant(expr_const.const_value_id);
                    ctx.resolver
                        .data
                        .resolved_items
                        .concrete
                        .insert(identifier.stable_ptr(db), item);
                }
                _ => unreachable!(
                    "get_binded_expr_by_name should only return variables or constants"
                ),
            };
            return Ok(res);
        }
    }

    let resolved_item: ResolvedConcreteItem<'_> = ctx.resolver.resolve_concrete_path_ex(
        ctx.diagnostics,
        path,
        NotFoundItemType::Identifier,
        ResolutionContext::Statement(&mut ctx.environment),
    )?;

    match resolved_item {
        ResolvedConcreteItem::Constant(const_value_id) => Ok(Expr::Constant(ExprConstant {
            const_value_id,
            ty: const_value_id.ty(db)?,
            stable_ptr: path.stable_ptr(db).into(),
        })),

        ResolvedConcreteItem::Variant(variant) if variant.ty == unit_ty(db) => {
            let stable_ptr = path.stable_ptr(db).into();
            let concrete_enum_id = variant.concrete_enum_id;
            Ok(semantic::Expr::EnumVariantCtor(semantic::ExprEnumVariantCtor {
                variant,
                value_expr: unit_expr(ctx, stable_ptr),
                ty: TypeLongId::Concrete(ConcreteTypeId::Enum(concrete_enum_id)).intern(db),
                stable_ptr,
            }))
        }
        resolved_item => Err(ctx.diagnostics.report(
            path.stable_ptr(db),
            UnexpectedElement {
                expected: vec![ElementKind::Variable, ElementKind::Constant],
                actual: (&resolved_item).into(),
            },
        )),
    }
}

/// Resolves a variable given a context and a simple name.
/// It is used where resolving a variable where only a single identifier is allowed, specifically
/// named function call and struct constructor arguments.
///
/// Reports a diagnostic if the variable was not found.
pub fn resolve_variable_by_name<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    identifier: &ast::TerminalIdentifier<'db>,
    stable_ptr: ast::ExprPtr<'db>,
) -> Maybe<Expr<'db>> {
    let variable_name = identifier.text(ctx.db);
    let res = get_binded_expr_by_name(ctx, variable_name, false, stable_ptr).ok_or_else(|| {
        ctx.diagnostics.report(identifier.stable_ptr(ctx.db), VariableNotFound(variable_name))
    })?;
    let item = ResolvedGenericItem::Variable(extract_matches!(&res, Expr::Var).var);
    ctx.resolver.data.resolved_items.generic.insert(identifier.stable_ptr(ctx.db), item);
    Ok(res)
}

/// Returns the requested variable from the environment if it exists. Returns None otherwise.
pub fn get_binded_expr_by_name<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    variable_name: SmolStrId<'db>,
    is_callsite_prefixed: bool,
    stable_ptr: ast::ExprPtr<'db>,
) -> Option<Expr<'db>> {
    let mut maybe_env = Some(&mut *ctx.environment);
    let mut cur_offset =
        ExpansionOffset::new(stable_ptr.lookup(ctx.db).as_syntax_node().offset(ctx.db));
    let mut found_callsite_scope = false;
    while let Some(env) = maybe_env {
        // If a variable is from an expanded macro placeholder, we need to look for it in the parent
        // env.
        if let Some(macro_info) = &env.macro_info
            && let Some(new_offset) = cur_offset.mapped(&macro_info.mappings)
        {
            maybe_env = env.parent.as_deref_mut();
            cur_offset = new_offset;
            continue;
        }
        if (!is_callsite_prefixed || found_callsite_scope)
            && let Some(var) = env.variables.get(&variable_name)
        {
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

        // Don't look inside a callsite environment unless explicitly stated.
        if env.macro_info.is_some() {
            if is_callsite_prefixed && !found_callsite_scope {
                found_callsite_scope = true;
            } else {
                break;
            }
        }
        maybe_env = env.parent.as_deref_mut();
    }
    None
}

/// Typechecks a function call.
fn expr_function_call<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    function_id: FunctionId<'db>,
    mut named_args: Vec<NamedArg<'db>>,
    call_ptr: impl Into<SyntaxStablePtrId<'db>>,
    stable_ptr: ast::ExprPtr<'db>,
) -> Maybe<Expr<'db>> {
    let coupon_arg = maybe_pop_coupon_argument(ctx, &mut named_args, function_id);

    let signature = ctx.db.concrete_function_signature(function_id)?;

    // TODO(spapini): Better location for these diagnostics after the refactor for generics resolve.
    if named_args.len() != signature.params.len() {
        return Err(ctx.diagnostics.report(
            call_ptr,
            WrongNumberOfArguments { expected: signature.params.len(), actual: named_args.len() },
        ));
    }

    // Check argument names and types.
    check_named_arguments(&named_args, signature, ctx)?;

    let inference = &mut ctx.resolver.inference();
    let mut args = Vec::new();
    for (NamedArg(arg, _name, mutability), param) in
        named_args.into_iter().zip(signature.params.iter())
    {
        let arg_ty = arg.ty();
        let param_ty = inference.rewrite(param.ty).no_err();
        // Don't add diagnostic if the type is missing (a diagnostic should have already been
        // added).
        // TODO(lior): Add a test to missing type once possible.
        if !arg_ty.is_missing(ctx.db) {
            let _ = inference.conform_ty_for_diag(
                arg_ty,
                param_ty,
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
            // Verify the variable is mutable and hasn't been referenced.
            ctx.variable_tracker.report_var_mutability_error(
                ctx.db,
                ctx.diagnostics,
                &ref_arg.base_var(),
                arg.deref(),
                RefArgNotMutable,
            );
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
        ty: inference.rewrite(signature.return_type).no_err(),
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
fn maybe_pop_coupon_argument<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    named_args: &mut Vec<NamedArg<'db>>,
    function_id: FunctionId<'db>,
) -> Option<ExprId> {
    let mut coupon_arg: Option<ExprId> = None;
    if let Some(NamedArg(arg, Some(name_terminal), mutability)) = named_args.last() {
        let coupons_enabled = are_coupons_enabled(ctx.db, ctx.resolver.module_id);
        if name_terminal.text(ctx.db).long(ctx.db) == "__coupon__" && coupons_enabled {
            // Check that the argument type is correct.
            let expected_ty = TypeLongId::Coupon(function_id).intern(ctx.db);
            let arg_ty = arg.ty();
            if !arg_ty.is_missing(ctx.db) {
                let inference = &mut ctx.resolver.inference();
                let _ = inference.conform_ty_for_diag(
                    arg_ty,
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
fn has_panic_incompatibility(ctx: &mut ComputationContext<'_, '_>) -> bool {
    if let Some(signature) = ctx.signature {
        // If the caller is nopanic, then this is a panic incompatibility.
        !signature.panicable
    } else {
        false
    }
}

/// Checks the correctness of the named arguments, and outputs diagnostics on errors.
fn check_named_arguments<'db>(
    named_args: &[NamedArg<'db>],
    signature: &Signature<'db>,
    ctx: &mut ComputationContext<'db, '_>,
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
            let name = name_terminal.text(ctx.db);
            if param.name != name {
                res = Err(ctx.diagnostics.report(
                    name_terminal.stable_ptr(ctx.db),
                    NamedArgumentMismatch { expected: param.name, found: name },
                ));
            }
        } else if seen_named_arguments && !reported_unnamed_argument_follows_named {
            reported_unnamed_argument_follows_named = true;
            res = Err(ctx.diagnostics.report(arg.deref(), UnnamedArgumentFollowsNamed));
        }
    }
    res
}

/// Computes the semantic model for a statement and appends the resulting statement IDs to the
/// provided vector.
pub fn compute_and_append_statement_semantic<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    syntax: ast::Statement<'db>,
    statements: &mut Vec<StatementId>,
) -> Maybe<()> {
    // Push the statement's attributes into the context, restored after the computation is resolved.
    let feature_restore = ctx.add_features_from_statement(&syntax);

    let db = ctx.db;
    let _ = match &syntax {
        ast::Statement::Let(let_syntax) => {
            let rhs_syntax = &let_syntax.rhs(db);
            let (rhs_expr, ty) = match let_syntax.type_clause(db) {
                ast::OptionTypeClause::Empty(_) => {
                    let rhs_expr = compute_expr_semantic(ctx, rhs_syntax);
                    let inferred_type = rhs_expr.ty();
                    (rhs_expr, inferred_type)
                }
                ast::OptionTypeClause::TypeClause(type_clause) => {
                    let var_type_path = type_clause.ty(db);
                    let explicit_type = resolve_type_ex(
                        db,
                        ctx.diagnostics,
                        ctx.resolver,
                        &var_type_path,
                        ResolutionContext::Statement(&mut ctx.environment),
                    );

                    let rhs_expr = compute_expr_semantic(ctx, rhs_syntax);
                    let inferred_type = ctx.reduce_ty(rhs_expr.ty());
                    if !inferred_type.is_missing(db) {
                        let inference = &mut ctx.resolver.inference();
                        let _ = inference.conform_ty_for_diag(
                            inferred_type,
                            explicit_type,
                            ctx.diagnostics,
                            || rhs_syntax.stable_ptr(db).untyped(),
                            |actual_ty, expected_ty| WrongArgumentType { expected_ty, actual_ty },
                        );
                    }
                    (rhs_expr, explicit_type)
                }
            };
            let rhs_expr_id = rhs_expr.id;

            let else_clause = match let_syntax.let_else_clause(db) {
                ast::OptionLetElseClause::Empty(_) => None,
                ast::OptionLetElseClause::LetElseClause(else_clause) => {
                    let else_block_syntax = else_clause.else_block(db);
                    let else_block_stable_ptr = else_block_syntax.stable_ptr(db);

                    let else_block =
                        compute_expr_semantic(ctx, &ast::Expr::Block(else_block_syntax));

                    if else_block.ty() != never_ty(db) {
                        // Report the error, but continue processing.
                        ctx.diagnostics.report(else_block_stable_ptr, NonNeverLetElseType);
                    }

                    Some(else_block.id)
                }
            };

            let pattern = compute_pattern_semantic(
                ctx,
                &let_syntax.pattern(db),
                ty,
                &UnorderedHashMap::default(),
            );
            let variables = pattern.variables(&ctx.arenas.patterns);
            let mut variable_names_in_pattern = UnorderedHashSet::<_>::default();
            for v in variables {
                if !variable_names_in_pattern.insert(v.name) {
                    ctx.diagnostics
                        .report(v.stable_ptr, VariableDefinedMultipleTimesInPattern(v.name));
                }
                let var_def = Binding::LocalVar(v.var.clone());
                if let Some(old_var) = ctx.environment.variables.insert(v.name, var_def.clone()) {
                    if matches!(old_var, Binding::LocalItem(_)) {
                        return Err(ctx
                            .diagnostics
                            .report(v.stable_ptr, MultipleDefinitionforBinding(v.name)));
                    }
                    add_unused_binding_warning(
                        ctx.diagnostics,
                        ctx.db,
                        &ctx.environment.used_variables,
                        v.name,
                        &old_var,
                        &ctx.resolver.data.feature_config,
                    );
                }
                if ctx.macro_defined_var_unhygienic
                    && let Some(macro_info) = &mut ctx.environment.macro_info
                {
                    macro_info.vars_to_expose.push((v.name, var_def.clone()));
                }
                let _ = ctx.variable_tracker.insert(var_def);
            }
            statements.push(ctx.arenas.statements.alloc(semantic::Statement::Let(
                semantic::StatementLet {
                    pattern: pattern.id,
                    expr: rhs_expr_id,
                    else_clause,
                    stable_ptr: syntax.stable_ptr(db),
                },
            )));
            Ok(()) as Maybe<()>
        }
        ast::Statement::Expr(stmt_expr_syntax) => {
            let expr_syntax = stmt_expr_syntax.expr(db);
            if let ast::Expr::InlineMacro(inline_macro_syntax) = &expr_syntax {
                expand_macro_for_statement(ctx, inline_macro_syntax, false, statements)?;
            } else {
                let expr = compute_expr_semantic(ctx, &expr_syntax);
                if matches!(stmt_expr_syntax.semicolon(db), ast::OptionTerminalSemicolon::Empty(_))
                    && !matches!(
                        expr_syntax,
                        ast::Expr::Block(_)
                            | ast::Expr::If(_)
                            | ast::Expr::Match(_)
                            | ast::Expr::Loop(_)
                            | ast::Expr::While(_)
                            | ast::Expr::For(_)
                    )
                {
                    ctx.diagnostics.report_after(expr_syntax.stable_ptr(db), MissingSemicolon);
                }
                let ty: TypeId<'_> = expr.ty();
                if let TypeLongId::Concrete(concrete) = ty.long(db)
                    && concrete.is_must_use(db)?
                {
                    ctx.diagnostics.report(expr_syntax.stable_ptr(db), UnhandledMustUseType(ty));
                }
                if let Expr::FunctionCall(expr_function_call) = &expr.expr {
                    let generic_function_id =
                        expr_function_call.function.long(db).function.generic_function;
                    if generic_function_id.is_must_use(db)? {
                        ctx.diagnostics
                            .report(expr_syntax.stable_ptr(db), UnhandledMustUseFunction);
                    }
                }
                statements.push(ctx.arenas.statements.alloc(semantic::Statement::Expr(
                    semantic::StatementExpr { expr: expr.id, stable_ptr: syntax.stable_ptr(db) },
                )));
            }
            Ok(())
        }
        ast::Statement::Continue(continue_syntax) => {
            if !ctx.is_inside_loop() {
                return Err(ctx
                    .diagnostics
                    .report(continue_syntax.stable_ptr(db), ContinueOnlyAllowedInsideALoop));
            }
            statements.push(ctx.arenas.statements.alloc(semantic::Statement::Continue(
                semantic::StatementContinue { stable_ptr: syntax.stable_ptr(db) },
            )));
            Ok(())
        }
        ast::Statement::Return(return_syntax) => {
            let (expr_option, expr_ty, stable_ptr) = match return_syntax.expr_clause(db) {
                ast::OptionExprClause::Empty(empty_clause) => {
                    (None, unit_ty(db), empty_clause.stable_ptr(db).untyped())
                }
                ast::OptionExprClause::ExprClause(expr_clause) => {
                    let expr_syntax = expr_clause.expr(db);
                    let expr = compute_expr_semantic(ctx, &expr_syntax);
                    (Some(expr.id), expr.ty(), expr_syntax.stable_ptr(db).untyped())
                }
            };
            let expected_ty = match &ctx.inner_ctx {
                None => ctx.get_return_type().ok_or_else(|| {
                    ctx.diagnostics.report(
                        return_syntax.stable_ptr(db),
                        UnsupportedOutsideOfFunction(
                            UnsupportedOutsideOfFunctionFeatureName::ReturnStatement,
                        ),
                    )
                })?,
                Some(ctx) => ctx.return_type,
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
            statements.push(ctx.arenas.statements.alloc(semantic::Statement::Return(
                semantic::StatementReturn { expr_option, stable_ptr: syntax.stable_ptr(db) },
            )));
            Ok(())
        }
        ast::Statement::Break(break_syntax) => {
            let (expr_option, ty, stable_ptr) = match break_syntax.expr_clause(db) {
                ast::OptionExprClause::Empty(expr_empty) => {
                    (None, unit_ty(db), expr_empty.stable_ptr(db).untyped())
                }
                ast::OptionExprClause::ExprClause(expr_clause) => {
                    let expr_syntax = expr_clause.expr(db);
                    let expr = compute_expr_semantic(ctx, &expr_syntax);

                    (Some(expr.id), expr.ty(), expr.stable_ptr().untyped())
                }
            };
            let ty = ctx.reduce_ty(ty);

            if !ctx.is_inside_loop() {
                return Err(ctx
                    .diagnostics
                    .report(break_syntax.stable_ptr(db), BreakOnlyAllowedInsideALoop));
            }

            if let Some(inner_ctx) = &mut ctx.inner_ctx {
                match &mut inner_ctx.kind {
                    InnerContextKind::Loop { type_merger, .. } => {
                        type_merger.try_merge_types(
                            ctx.db,
                            ctx.diagnostics,
                            &mut ctx.resolver.inference(),
                            ty,
                            stable_ptr,
                        );
                    }
                    InnerContextKind::While | InnerContextKind::For => {
                        if expr_option.is_some() {
                            ctx.diagnostics.report(
                                break_syntax.stable_ptr(db),
                                BreakWithValueOnlyAllowedInsideALoop,
                            );
                        };
                    }
                    InnerContextKind::Closure => unreachable!("Not inside a loop."),
                }
            }

            statements.push(ctx.arenas.statements.alloc(semantic::Statement::Break(
                semantic::StatementBreak { expr_option, stable_ptr: syntax.stable_ptr(db) },
            )));
            Ok(())
        }
        ast::Statement::Item(stmt_item_syntax) => {
            let item_syntax = &stmt_item_syntax.item(db);
            match item_syntax {
                ast::ModuleItem::Constant(const_syntax) => {
                    let lhs = const_syntax.type_clause(db).ty(db);
                    let rhs = const_syntax.value(db);
                    let rhs_expr = compute_expr_semantic(ctx, &rhs);
                    let explicit_type = resolve_type_ex(
                        db,
                        ctx.diagnostics,
                        ctx.resolver,
                        &lhs,
                        ResolutionContext::Statement(&mut ctx.environment),
                    );
                    let rhs_resolved_expr = resolve_const_expr_and_evaluate(
                        db,
                        ctx,
                        &rhs_expr,
                        stmt_item_syntax.stable_ptr(db).untyped(),
                        explicit_type,
                        false,
                    );
                    let name_syntax = const_syntax.name(db);
                    let name = name_syntax.text(db);
                    let rhs_id =
                        StatementConstLongId(ctx.resolver.module_id, const_syntax.stable_ptr(db));
                    let var_def = Binding::LocalItem(LocalItem {
                        id: StatementItemId::Constant(rhs_id.intern(db)),
                        kind: StatementItemKind::Constant(
                            rhs_resolved_expr,
                            rhs_resolved_expr.ty(db)?,
                        ),
                    });
                    add_value_to_statement_environment(
                        ctx,
                        name,
                        var_def,
                        name_syntax.stable_ptr(db),
                    );
                }
                ast::ModuleItem::Use(use_syntax) => {
                    for leaf in get_all_path_leaves(db, use_syntax) {
                        let stable_ptr = leaf.stable_ptr(db);
                        let resolved_item = ctx.resolver.resolve_use_path(
                            ctx.diagnostics,
                            ast::UsePath::Leaf(leaf),
                            ResolutionContext::Statement(&mut ctx.environment),
                        )?;
                        let var_def_id = StatementItemId::Use(
                            StatementUseLongId(ctx.resolver.module_id, stable_ptr).intern(db),
                        );
                        let name = var_def_id.name(db);
                        match resolved_item {
                            ResolvedGenericItem::GenericConstant(const_id) => {
                                let const_value_id = db.constant_const_value(const_id)?;
                                let var_def = Binding::LocalItem(LocalItem {
                                    id: var_def_id,
                                    kind: StatementItemKind::Constant(
                                        const_value_id,
                                        const_value_id.ty(db)?,
                                    ),
                                });
                                add_value_to_statement_environment(ctx, name, var_def, stable_ptr);
                            }
                            item @ (ResolvedGenericItem::GenericType(_)
                            | ResolvedGenericItem::Module(_)) => {
                                add_item_to_statement_environment(ctx, name, item, stable_ptr);
                            }
                            ResolvedGenericItem::GenericFunction(_)
                            | ResolvedGenericItem::GenericTypeAlias(_)
                            | ResolvedGenericItem::GenericImplAlias(_)
                            | ResolvedGenericItem::Variant(_)
                            | ResolvedGenericItem::Trait(_)
                            | ResolvedGenericItem::Impl(_)
                            | ResolvedGenericItem::Variable(_)
                            | ResolvedGenericItem::TraitItem(_)
                            | ResolvedGenericItem::Macro(_) => {
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
                ast::ModuleItem::MacroDeclaration(_) => {
                    unreachable!("MacroDeclaration type not supported.")
                }
                ast::ModuleItem::Missing(_) => unreachable!("Missing type not supported."),
            }
            statements.push(ctx.arenas.statements.alloc(semantic::Statement::Item(
                semantic::StatementItem { stable_ptr: syntax.stable_ptr(db) },
            )));
            Ok(())
        }
        // Dianogstics reported on syntax level already.
        ast::Statement::Missing(_) => return Err(skip_diagnostic()),
    };
    ctx.restore_features(feature_restore);
    Ok(())
}
/// Adds an item to the statement environment and reports a diagnostic if the item is already
/// defined.
fn add_value_to_statement_environment<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    name: SmolStrId<'db>,
    var_def: Binding<'db>,
    stable_ptr: impl Into<SyntaxStablePtrId<'db>>,
) {
    if let Some(old_var) = ctx.insert_variable(name, var_def) {
        ctx.diagnostics.report(
            stable_ptr,
            match old_var {
                Binding::LocalItem(_) => MultipleConstantDefinition(name),
                Binding::LocalVar(_) | Binding::Param(_) => MultipleDefinitionforBinding(name),
            },
        );
    }
}

/// Adds an item to the statement environment and reports a diagnostic if the type is already
/// defined.
fn add_item_to_statement_environment<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    name: SmolStrId<'db>,
    resolved_generic_item: ResolvedGenericItem<'db>,
    stable_ptr: impl Into<SyntaxStablePtrId<'db>> + std::marker::Copy,
) {
    if ctx
        .environment
        .use_items
        .insert(
            name,
            StatementGenericItemData { resolved_generic_item, stable_ptr: stable_ptr.into() },
        )
        .is_some()
    {
        ctx.diagnostics.report(stable_ptr, MultipleGenericItemDefinition(name));
    }
}

/// Computes the semantic model of an expression and reports diagnostics if the expression does not
/// evaluate to a boolean value.
fn compute_bool_condition_semantic<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    condition_syntax: &ast::Expr<'db>,
) -> ExprAndId<'db> {
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
fn check_struct_member_is_visible<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    member: &Member<'db>,
    stable_ptr: SyntaxStablePtrId<'db>,
    member_name: SmolStrId<'db>,
) {
    let db = ctx.db;
    let containing_module_id = member.id.parent_module(db);
    if ctx.resolver.ignore_visibility_checks(containing_module_id) {
        return;
    }
    let user_module_id = ctx.resolver.module_id;
    if !visibility::peek_visible_in(db, member.visibility, containing_module_id, user_module_id) {
        ctx.diagnostics.report(stable_ptr, MemberNotVisible(member_name));
    }
}

/// Verifies that the statement attributes are valid statements attributes, if not a diagnostic is
/// reported.
fn validate_statement_attributes<'db, Item: QueryAttrs<'db> + TypedSyntaxNode<'db>>(
    ctx: &mut ComputationContext<'db, '_>,
    item: &Item,
) {
    let allowed_attributes = ctx.db.allowed_statement_attributes();
    let mut diagnostics = vec![];
    validate_attributes_flat(
        ctx.db,
        allowed_attributes,
        &OrderedHashSet::default(),
        item,
        &mut diagnostics,
    );
    // Translate the plugin diagnostics to semantic diagnostics.
    for diagnostic in diagnostics {
        ctx.diagnostics
            .report(diagnostic.stable_ptr, SemanticDiagnosticKind::UnknownStatementAttribute);
    }
}

/// Gets an iterator with the types of the parameters of the given function.
fn function_parameter_types<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    function: FunctionId<'db>,
) -> Maybe<impl Iterator<Item = TypeId<'db>> + use<'db>> {
    let signature = ctx.db.concrete_function_signature(function)?;
    let param_types = signature.params.iter().map(|param| param.ty);
    Ok(param_types)
}

/// Finds traits which contain a method matching the given name and type.
/// This function checks for visible traits in the specified module file and filters
/// methods based on their association with the given type and method name.
fn match_method_to_traits<'db>(
    db: &dyn Database,
    ty: semantic::TypeId<'db>,
    method_name: SmolStrId<'db>,
    lookup_context: ImplLookupContextId<'db>,
    module_id: ModuleId<'db>,
    stable_ptr: SyntaxStablePtrId<'db>,
) -> Vec<String> {
    let visible_traits = db
        .visible_traits_from_module(module_id)
        .unwrap_or_else(|| Arc::new(OrderedHashMap::default()));

    visible_traits
        .iter()
        .filter_map(|(trait_id, path)| {
            let mut data = InferenceData::new(InferenceId::NoContext);
            let mut inference = data.inference(db);
            let trait_function = db.trait_function_by_name(*trait_id, method_name).ok()??;
            let (concrete_trait_id, _) = inference.infer_concrete_trait_by_self_without_errors(
                trait_function,
                ty,
                lookup_context,
                Some(stable_ptr),
            )?;
            inference.solve().ok();
            match inference.trait_solution_set(
                concrete_trait_id,
                ImplVarTraitItemMappings::default(),
                lookup_context,
            ) {
                Ok(SolutionSet::Unique(_) | SolutionSet::Ambiguous(_)) => Some(path.clone()),
                _ => None,
            }
        })
        .collect()
}
