use std::iter::zip;
use std::sync::Arc;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::{
    ConstantId, ExternFunctionId, GenericParamId, LanguageElementId, LookupItemId, ModuleItemId,
    NamedLanguageElementId, TopLevelLanguageElementId, TraitConstantId, TraitId, VarId,
};
use cairo_lang_diagnostics::{
    DiagnosticAdded, DiagnosticEntry, DiagnosticNote, Diagnostics, Maybe, ToMaybe, skip_diagnostic,
};
use cairo_lang_proc_macros::{DebugWithDb, SemanticObject};
use cairo_lang_syntax::node::ast::ItemConstant;
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_syntax::node::{TypedStablePtr, TypedSyntaxNode};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::unordered_hash_set::UnorderedHashSet;
use cairo_lang_utils::{
    Intern, LookupIntern, define_short_id, extract_matches, require, try_extract_matches,
};
use itertools::Itertools;
use num_bigint::BigInt;
use num_traits::{Num, ToPrimitive, Zero};
use smol_str::SmolStr;

use super::functions::{GenericFunctionId, GenericFunctionWithBodyId};
use super::imp::{ImplId, ImplLongId};
use crate::corelib::{
    CoreInfo, LiteralError, core_box_ty, core_nonzero_ty, false_variant, get_core_ty_by_name,
    true_variant, try_extract_nz_wrapped_type, unit_ty, validate_literal,
};
use crate::db::SemanticGroup;
use crate::diagnostic::{SemanticDiagnosticKind, SemanticDiagnostics, SemanticDiagnosticsBuilder};
use crate::expr::compute::{
    ComputationContext, ContextFunction, Environment, ExprAndId, compute_expr_semantic,
};
use crate::expr::inference::conform::InferenceConform;
use crate::expr::inference::{ConstVar, InferenceId};
use crate::helper::ModuleHelper;
use crate::items::enm::SemanticEnumEx;
use crate::resolve::{Resolver, ResolverData};
use crate::substitution::{GenericSubstitution, SemanticRewriter};
use crate::types::resolve_type;
use crate::{
    Arenas, ConcreteFunction, ConcreteTypeId, ConcreteVariant, Condition, Expr, ExprBlock,
    ExprConstant, ExprFunctionCall, ExprFunctionCallArg, ExprId, ExprMemberAccess, ExprStructCtor,
    FunctionId, GenericParam, LogicalOperator, Pattern, PatternId, SemanticDiagnostic, Statement,
    TypeId, TypeLongId, semantic_object_for_id,
};

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct Constant {
    /// The actual id of the const expression value.
    pub value: ExprId,
    /// The arena of all the expressions for the const calculation.
    pub arenas: Arc<Arenas>,
}

impl Constant {
    pub fn ty(&self) -> TypeId {
        self.arenas.exprs[self.value].ty()
    }
}

/// Information about a constant definition.
///
/// Helper struct for the data returned by [SemanticGroup::priv_constant_semantic_data].
#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct ConstantData {
    pub diagnostics: Diagnostics<SemanticDiagnostic>,
    pub constant: Maybe<Constant>,
    pub const_value: ConstValueId,
    pub resolver_data: Arc<ResolverData>,
}

define_short_id!(
    ConstValueId,
    ConstValue,
    SemanticGroup,
    lookup_intern_const_value,
    intern_const_value
);
semantic_object_for_id!(ConstValueId, lookup_intern_const_value, intern_const_value, ConstValue);
impl ConstValueId {
    pub fn format(&self, db: &dyn SemanticGroup) -> String {
        format!("{:?}", self.lookup_intern(db).debug(db.elongate()))
    }

    /// Returns true if the const does not depend on any generics.
    pub fn is_fully_concrete(&self, db: &dyn SemanticGroup) -> bool {
        self.lookup_intern(db).is_fully_concrete(db)
    }

    /// Returns true if the const does not contain any inference variables.
    pub fn is_var_free(&self, db: &dyn SemanticGroup) -> bool {
        self.lookup_intern(db).is_var_free(db)
    }

    /// Returns the type of the const.
    pub fn ty(&self, db: &dyn SemanticGroup) -> Maybe<TypeId> {
        self.lookup_intern(db).ty(db)
    }
}

/// A constant value.
#[derive(Clone, Debug, Hash, PartialEq, Eq, SemanticObject)]
pub enum ConstValue {
    Int(#[dont_rewrite] BigInt, TypeId),
    Struct(Vec<ConstValue>, TypeId),
    Enum(ConcreteVariant, Box<ConstValue>),
    NonZero(Box<ConstValue>),
    Boxed(Box<ConstValue>),
    Generic(#[dont_rewrite] GenericParamId),
    ImplConstant(ImplConstantId),
    Var(ConstVar, TypeId),
    /// A missing value, used in cases where the value is not known due to diagnostics.
    Missing(#[dont_rewrite] DiagnosticAdded),
}
impl ConstValue {
    /// Returns true if the const does not depend on any generics.
    pub fn is_fully_concrete(&self, db: &dyn SemanticGroup) -> bool {
        self.ty(db).unwrap().is_fully_concrete(db)
            && match self {
                ConstValue::Int(_, _) => true,
                ConstValue::Struct(members, _) => {
                    members.iter().all(|member: &ConstValue| member.is_fully_concrete(db))
                }
                ConstValue::Enum(_, value)
                | ConstValue::NonZero(value)
                | ConstValue::Boxed(value) => value.is_fully_concrete(db),
                ConstValue::Generic(_)
                | ConstValue::Var(_, _)
                | ConstValue::Missing(_)
                | ConstValue::ImplConstant(_) => false,
            }
    }

    /// Returns true if the const does not contain any inference variables.
    pub fn is_var_free(&self, db: &dyn SemanticGroup) -> bool {
        self.ty(db).unwrap().is_var_free(db)
            && match self {
                ConstValue::Int(_, _) | ConstValue::Generic(_) | ConstValue::Missing(_) => true,
                ConstValue::Struct(members, _) => {
                    members.iter().all(|member| member.is_var_free(db))
                }
                ConstValue::Enum(_, value)
                | ConstValue::NonZero(value)
                | ConstValue::Boxed(value) => value.is_var_free(db),
                ConstValue::Var(_, _) => false,
                ConstValue::ImplConstant(impl_constant) => impl_constant.impl_id().is_var_free(db),
            }
    }

    /// Returns the type of the const.
    pub fn ty(&self, db: &dyn SemanticGroup) -> Maybe<TypeId> {
        Ok(match self {
            ConstValue::Int(_, ty) => *ty,
            ConstValue::Struct(_, ty) => *ty,
            ConstValue::Enum(variant, _) => {
                TypeLongId::Concrete(ConcreteTypeId::Enum(variant.concrete_enum_id)).intern(db)
            }
            ConstValue::NonZero(value) => core_nonzero_ty(db, value.ty(db)?),
            ConstValue::Boxed(value) => core_box_ty(db, value.ty(db)?),
            ConstValue::Generic(param) => {
                extract_matches!(db.generic_param_semantic(*param)?, GenericParam::Const).ty
            }
            ConstValue::Var(_, ty) => *ty,
            ConstValue::Missing(_) => TypeId::missing(db, skip_diagnostic()),
            ConstValue::ImplConstant(impl_constant_id) => {
                db.impl_constant_concrete_implized_type(*impl_constant_id)?
            }
        })
    }

    /// Returns the value of an int const as a BigInt.
    pub fn into_int(self) -> Option<BigInt> {
        match self {
            ConstValue::Int(value, _) => Some(value.clone()),
            _ => None,
        }
    }
}

/// An impl item of kind const.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, SemanticObject)]
pub struct ImplConstantId {
    /// The impl the item const is in.
    impl_id: ImplId,
    /// The trait const this impl const "implements".
    trait_constant_id: TraitConstantId,
}

impl ImplConstantId {
    /// Creates a new impl constant id. For an impl constant of a concrete impl, asserts that the
    /// trait constant belongs to the same trait that the impl implements (panics if not).
    pub fn new(
        impl_id: ImplId,
        trait_constant_id: TraitConstantId,
        db: &dyn SemanticGroup,
    ) -> Self {
        if let ImplLongId::Concrete(concrete_impl) = impl_id.lookup_intern(db) {
            let impl_def_id = concrete_impl.impl_def_id(db);
            assert_eq!(Ok(trait_constant_id.trait_id(db.upcast())), db.impl_def_trait(impl_def_id));
        }

        ImplConstantId { impl_id, trait_constant_id }
    }
    pub fn impl_id(&self) -> ImplId {
        self.impl_id
    }
    pub fn trait_constant_id(&self) -> TraitConstantId {
        self.trait_constant_id
    }

    pub fn format(&self, db: &dyn SemanticGroup) -> SmolStr {
        format!("{}::{}", self.impl_id.name(db.upcast()), self.trait_constant_id.name(db.upcast()))
            .into()
    }
}
impl DebugWithDb<dyn SemanticGroup> for ImplConstantId {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        db: &(dyn SemanticGroup + 'static),
    ) -> std::fmt::Result {
        write!(f, "{}", self.format(db))
    }
}

/// Query implementation of [SemanticGroup::priv_constant_semantic_data].
pub fn priv_constant_semantic_data(
    db: &dyn SemanticGroup,
    const_id: ConstantId,
    in_cycle: bool,
) -> Maybe<ConstantData> {
    let lookup_item_id = LookupItemId::ModuleItem(ModuleItemId::Constant(const_id));
    if in_cycle {
        constant_semantic_data_cycle_helper(
            db,
            &db.module_constant_by_id(const_id)?.to_maybe()?,
            lookup_item_id,
            None,
            &const_id,
        )
    } else {
        constant_semantic_data_helper(
            db,
            &db.module_constant_by_id(const_id)?.to_maybe()?,
            lookup_item_id,
            None,
            &const_id,
        )
    }
}

/// Cycle handling for [SemanticGroup::priv_constant_semantic_data].
pub fn priv_constant_semantic_data_cycle(
    db: &dyn SemanticGroup,
    _cycle: &salsa::Cycle,
    const_id: &ConstantId,
    _in_cycle: &bool,
) -> Maybe<ConstantData> {
    priv_constant_semantic_data(db, *const_id, true)
}

/// Returns constant semantic data for the given ItemConstant.
pub fn constant_semantic_data_helper(
    db: &dyn SemanticGroup,
    constant_ast: &ItemConstant,
    lookup_item_id: LookupItemId,
    parent_resolver_data: Option<Arc<ResolverData>>,
    element_id: &impl LanguageElementId,
) -> Maybe<ConstantData> {
    let mut diagnostics: SemanticDiagnostics = SemanticDiagnostics::default();
    // TODO(spapini): when code changes in a file, all the AST items change (as they contain a path
    // to the green root that changes. Once ASTs are rooted on items, use a selector that picks only
    // the item instead of all the module data.
    let syntax_db = db.upcast();

    let inference_id = InferenceId::LookupItemDeclaration(lookup_item_id);

    let mut resolver = match parent_resolver_data {
        Some(parent_resolver_data) => {
            Resolver::with_data(db, parent_resolver_data.clone_with_inference_id(db, inference_id))
        }
        None => Resolver::new(db, element_id.module_file_id(db.upcast()), inference_id),
    };
    resolver.set_feature_config(element_id, constant_ast, &mut diagnostics);

    let constant_type = resolve_type(
        db,
        &mut diagnostics,
        &mut resolver,
        &constant_ast.type_clause(syntax_db).ty(syntax_db),
    );

    let environment = Environment::empty();
    let mut ctx = ComputationContext::new(
        db,
        &mut diagnostics,
        resolver,
        None,
        environment,
        ContextFunction::Global,
    );

    let value = compute_expr_semantic(&mut ctx, &constant_ast.value(syntax_db));
    let const_value = resolve_const_expr_and_evaluate(
        db,
        &mut ctx,
        &value,
        constant_ast.stable_ptr().untyped(),
        constant_type,
        true,
    )
    .intern(db);

    let const_value = ctx
        .resolver
        .inference()
        .rewrite(const_value)
        .unwrap_or_else(|_| ConstValue::Missing(skip_diagnostic()).intern(db));
    let resolver_data = Arc::new(ctx.resolver.data);
    let constant = Constant { value: value.id, arenas: Arc::new(ctx.arenas) };
    Ok(ConstantData {
        diagnostics: diagnostics.build(),
        const_value,
        constant: Ok(constant),
        resolver_data,
    })
}

/// Helper for cycle handling of constants.
pub fn constant_semantic_data_cycle_helper(
    db: &dyn SemanticGroup,
    constant_ast: &ItemConstant,
    lookup_item_id: LookupItemId,
    parent_resolver_data: Option<Arc<ResolverData>>,
    element_id: &impl LanguageElementId,
) -> Maybe<ConstantData> {
    let mut diagnostics: SemanticDiagnostics = SemanticDiagnostics::default();

    let inference_id = InferenceId::LookupItemDeclaration(lookup_item_id);

    let resolver = match parent_resolver_data {
        Some(parent_resolver_data) => {
            Resolver::with_data(db, parent_resolver_data.clone_with_inference_id(db, inference_id))
        }
        None => Resolver::new(db, element_id.module_file_id(db.upcast()), inference_id),
    };

    let resolver_data = Arc::new(resolver.data);

    let diagnostic_added = diagnostics.report(constant_ast, SemanticDiagnosticKind::ConstCycle);
    Ok(ConstantData {
        constant: Err(diagnostic_added),
        const_value: ConstValue::Missing(diagnostic_added).intern(db),
        diagnostics: diagnostics.build(),
        resolver_data,
    })
}

/// Checks if the given expression only involved constant calculations.
pub fn validate_const_expr(ctx: &mut ComputationContext<'_>, expr_id: ExprId) {
    let info = ctx.db.const_calc_info();
    let mut eval_ctx = ConstantEvaluateContext {
        db: ctx.db,
        info: info.as_ref(),
        arenas: &ctx.arenas,
        vars: Default::default(),
        generic_substitution: Default::default(),
        depth: 0,
        diagnostics: ctx.diagnostics,
    };
    eval_ctx.validate(expr_id);
}

/// Resolves the given const expression and evaluates its value.
pub fn resolve_const_expr_and_evaluate(
    db: &dyn SemanticGroup,
    ctx: &mut ComputationContext<'_>,
    value: &ExprAndId,
    const_stable_ptr: SyntaxStablePtrId,
    target_type: TypeId,
    finalize: bool,
) -> ConstValue {
    let prev_err_count = ctx.diagnostics.error_count;
    let inference = &mut ctx.resolver.inference();
    if let Err(err_set) = inference.conform_ty(value.ty(), target_type) {
        inference.report_on_pending_error(err_set, ctx.diagnostics, const_stable_ptr);
    }

    if finalize {
        // Check fully resolved.
        inference.finalize(ctx.diagnostics, const_stable_ptr);
    } else if let Err(err_set) = inference.solve() {
        inference.report_on_pending_error(err_set, ctx.diagnostics, const_stable_ptr);
    }

    // TODO(orizi): Consider moving this to be called only upon creating const values, other callees
    // don't necessarily need it.
    ctx.apply_inference_rewriter_to_exprs();

    match &value.expr {
        Expr::Constant(ExprConstant { const_value_id, .. }) => const_value_id.lookup_intern(db),
        // Check that the expression is a valid constant.
        _ if ctx.diagnostics.error_count > prev_err_count => ConstValue::Missing(skip_diagnostic()),
        _ => {
            let info = db.const_calc_info();
            let mut eval_ctx = ConstantEvaluateContext {
                db,
                info: info.as_ref(),
                arenas: &ctx.arenas,
                vars: Default::default(),
                generic_substitution: Default::default(),
                depth: 0,
                diagnostics: ctx.diagnostics,
            };
            eval_ctx.validate(value.id);
            if eval_ctx.diagnostics.error_count > prev_err_count {
                ConstValue::Missing(skip_diagnostic())
            } else {
                eval_ctx.evaluate(value.id)
            }
        }
    }
}

/// creates a [ConstValue] from a [BigInt] value.
pub fn value_as_const_value(
    db: &dyn SemanticGroup,
    ty: TypeId,
    value: &BigInt,
) -> Result<ConstValue, LiteralError> {
    validate_literal(db.upcast(), ty, value)?;
    let get_basic_const_value = |ty| {
        let u256_ty = get_core_ty_by_name(db.upcast(), "u256".into(), vec![]);

        if ty != u256_ty {
            ConstValue::Int(value.clone(), ty)
        } else {
            let u128_ty = get_core_ty_by_name(db.upcast(), "u128".into(), vec![]);
            let mask128 = BigInt::from(u128::MAX);
            let low = value & mask128;
            let high = value >> 128;
            ConstValue::Struct(
                vec![(ConstValue::Int(low, u128_ty)), (ConstValue::Int(high, u128_ty))],
                ty,
            )
        }
    };

    if let Some(inner) = try_extract_nz_wrapped_type(db.upcast(), ty) {
        Ok(ConstValue::NonZero(Box::new(get_basic_const_value(inner))))
    } else {
        Ok(get_basic_const_value(ty))
    }
}

/// A context for evaluating constant expressions.
struct ConstantEvaluateContext<'a> {
    db: &'a dyn SemanticGroup,
    info: &'a ConstCalcInfo,
    arenas: &'a Arenas,
    vars: OrderedHashMap<VarId, ConstValue>,
    generic_substitution: GenericSubstitution,
    depth: usize,
    diagnostics: &'a mut SemanticDiagnostics,
}
impl ConstantEvaluateContext<'_> {
    /// Validate the given expression can be used as constant.
    fn validate(&mut self, expr_id: ExprId) {
        match &self.arenas.exprs[expr_id] {
            Expr::Var(_) | Expr::Constant(_) | Expr::Missing(_) => {}
            Expr::Block(ExprBlock { statements, tail: Some(inner), .. }) => {
                for statement_id in statements {
                    match &self.arenas.statements[*statement_id] {
                        Statement::Let(statement) => {
                            self.validate(statement.expr);
                        }
                        Statement::Expr(expr) => {
                            self.validate(expr.expr);
                        }
                        other => {
                            self.diagnostics.report(
                                other.stable_ptr(),
                                SemanticDiagnosticKind::UnsupportedConstant,
                            );
                        }
                    }
                }
                self.validate(*inner);
            }
            Expr::FunctionCall(expr) => {
                for arg in &expr.args {
                    match arg {
                        ExprFunctionCallArg::Value(arg) => self.validate(*arg),
                        ExprFunctionCallArg::Reference(var) => {
                            self.diagnostics.report(
                                var.stable_ptr(),
                                SemanticDiagnosticKind::UnsupportedConstant,
                            );
                        }
                    }
                    if let ExprFunctionCallArg::Value(arg) = arg {
                        self.validate(*arg);
                    }
                }
                if !self.is_function_const(expr.function) {
                    self.diagnostics.report(
                        expr.stable_ptr.untyped(),
                        SemanticDiagnosticKind::UnsupportedConstant,
                    );
                }
            }
            Expr::Literal(expr) => {
                if let Err(err) = validate_literal(self.db, expr.ty, &expr.value) {
                    self.diagnostics.report(
                        expr.stable_ptr.untyped(),
                        SemanticDiagnosticKind::LiteralError(err),
                    );
                }
            }
            Expr::Tuple(expr) => {
                for item in &expr.items {
                    self.validate(*item);
                }
            }
            Expr::StructCtor(ExprStructCtor { members, base_struct: None, .. }) => {
                for (_, expr_id) in members {
                    self.validate(*expr_id);
                }
            }
            Expr::EnumVariantCtor(expr) => self.validate(expr.value_expr),
            Expr::MemberAccess(expr) => self.validate(expr.expr),
            Expr::FixedSizeArray(expr) => match &expr.items {
                crate::FixedSizeArrayItems::Items(items) => {
                    for item in items {
                        self.validate(*item);
                    }
                }
                crate::FixedSizeArrayItems::ValueAndSize(value, _) => {
                    self.validate(*value);
                }
            },
            Expr::Snapshot(expr) => self.validate(expr.inner),
            Expr::Desnap(expr) => self.validate(expr.inner),
            Expr::LogicalOperator(expr) => {
                self.validate(expr.lhs);
                self.validate(expr.rhs);
            }
            Expr::Match(expr) => {
                self.validate(expr.matched_expr);
                for arm in &expr.arms {
                    self.validate(arm.expression);
                }
            }
            Expr::If(expr) => {
                self.validate(match &expr.condition {
                    Condition::BoolExpr(id) | Condition::Let(id, _) => *id,
                });
                self.validate(expr.if_block);
                if let Some(else_block) = expr.else_block {
                    self.validate(else_block);
                }
            }
            other => {
                self.diagnostics.report(
                    other.stable_ptr().untyped(),
                    SemanticDiagnosticKind::UnsupportedConstant,
                );
            }
        }
    }

    /// Returns true if the given function is allowed to be called in constant context.
    fn is_function_const(&self, function_id: FunctionId) -> bool {
        if function_id == self.panic_with_felt252 {
            return true;
        }
        let db = self.db;
        let concrete_function = function_id.get_concrete(db);
        let signature = (|| match concrete_function.generic_function {
            GenericFunctionId::Free(id) => db.free_function_signature(id),
            GenericFunctionId::Extern(id) => db.extern_function_signature(id),
            GenericFunctionId::Impl(id) => {
                if let ImplLongId::Concrete(impl_id) = id.impl_id.lookup_intern(db) {
                    if let Ok(Some(impl_function_id)) = impl_id.get_impl_function(db, id.function) {
                        return self.db.impl_function_signature(impl_function_id);
                    }
                }
                self.db.trait_function_signature(id.function)
            }
        })();
        if signature.map(|s| s.is_const) == Ok(true) {
            return true;
        }
        let Ok(Some(body)) = concrete_function.body(db) else { return false };
        let GenericFunctionWithBodyId::Impl(imp) = body.generic_function(db) else {
            return false;
        };
        let impl_def = imp.concrete_impl_id.impl_def_id(db);
        if impl_def.parent_module(db.upcast()).owning_crate(db.upcast()) != db.core_crate() {
            return false;
        }
        let Ok(trait_id) = db.impl_def_trait(impl_def) else {
            return false;
        };
        self.const_traits.contains(&trait_id)
    }

    /// Evaluate the given const expression value.
    fn evaluate(&mut self, expr_id: ExprId) -> ConstValue {
        let expr = &self.arenas.exprs[expr_id];
        let db = self.db;
        match expr {
            Expr::Var(expr) => self
                .vars
                .get(&expr.var)
                .cloned()
                .unwrap_or_else(|| ConstValue::Missing(skip_diagnostic())),
            Expr::Constant(expr) => self
                .generic_substitution
                .substitute(self.db, expr.const_value_id.lookup_intern(db))
                .unwrap_or_else(ConstValue::Missing),
            Expr::Block(ExprBlock { statements, tail: Some(inner), .. }) => {
                for statement_id in statements {
                    match &self.arenas.statements[*statement_id] {
                        Statement::Let(statement) => {
                            let value = self.evaluate(statement.expr);
                            self.destructure_pattern(statement.pattern, value);
                        }
                        Statement::Expr(expr) => {
                            self.evaluate(expr.expr);
                        }
                        other => {
                            self.diagnostics.report(
                                other.stable_ptr(),
                                SemanticDiagnosticKind::UnsupportedConstant,
                            );
                        }
                    }
                }
                self.evaluate(*inner)
            }
            Expr::FunctionCall(expr) => self.evaluate_function_call(expr),
            Expr::Literal(expr) => value_as_const_value(db, expr.ty, &expr.value)
                .expect("LiteralError should have been caught on `validate`"),
            Expr::Tuple(expr) => ConstValue::Struct(
                expr.items.iter().map(|expr_id| self.evaluate(*expr_id)).collect(),
                expr.ty,
            ),
            Expr::StructCtor(ExprStructCtor {
                members,
                base_struct: None,
                ty,
                concrete_struct_id,
                ..
            }) => {
                let member_order = match db.concrete_struct_members(*concrete_struct_id) {
                    Ok(member_order) => member_order,
                    Err(diag_add) => return ConstValue::Missing(diag_add),
                };
                ConstValue::Struct(
                    member_order
                        .values()
                        .map(|m| {
                            members
                                .iter()
                                .find(|(member_id, _)| m.id == *member_id)
                                .map(|(_, expr_id)| self.evaluate(*expr_id))
                                .expect("Should have been caught by semantic validation")
                        })
                        .collect(),
                    *ty,
                )
            }
            Expr::EnumVariantCtor(expr) => {
                ConstValue::Enum(expr.variant.clone(), Box::new(self.evaluate(expr.value_expr)))
            }
            Expr::MemberAccess(expr) => {
                self.evaluate_member_access(expr).unwrap_or_else(ConstValue::Missing)
            }
            Expr::FixedSizeArray(expr) => ConstValue::Struct(
                match &expr.items {
                    crate::FixedSizeArrayItems::Items(items) => {
                        items.iter().map(|expr_id| self.evaluate(*expr_id)).collect()
                    }
                    crate::FixedSizeArrayItems::ValueAndSize(value, count) => {
                        let value = self.evaluate(*value);
                        let count = count.lookup_intern(db);
                        if let Some(count) = count.into_int() {
                            (0..count.to_usize().unwrap()).map(|_| value.clone()).collect()
                        } else {
                            self.diagnostics.report(
                                expr.stable_ptr.untyped(),
                                SemanticDiagnosticKind::UnsupportedConstant,
                            );
                            vec![]
                        }
                    }
                },
                expr.ty,
            ),
            Expr::Snapshot(expr) => self.evaluate(expr.inner),
            Expr::Desnap(expr) => self.evaluate(expr.inner),
            Expr::LogicalOperator(expr) => {
                let lhs = self.evaluate(expr.lhs);
                if let ConstValue::Enum(v, _) = &lhs {
                    let early_return_variant = match expr.op {
                        LogicalOperator::AndAnd => false_variant(self.db),
                        LogicalOperator::OrOr => true_variant(self.db),
                    };
                    if *v == early_return_variant { lhs } else { self.evaluate(expr.lhs) }
                } else {
                    ConstValue::Missing(skip_diagnostic())
                }
            }
            Expr::Match(expr) => {
                let value = self.evaluate(expr.matched_expr);
                let ConstValue::Enum(variant, value) = value else {
                    return ConstValue::Missing(skip_diagnostic());
                };
                for arm in &expr.arms {
                    for pattern_id in &arm.patterns {
                        let pattern = &self.arenas.patterns[*pattern_id];
                        if matches!(pattern, Pattern::Otherwise(_)) {
                            return self.evaluate(arm.expression);
                        }
                        let Pattern::EnumVariant(pattern) = pattern else {
                            continue;
                        };
                        if pattern.variant.idx != variant.idx {
                            continue;
                        }
                        if let Some(inner_pattern) = pattern.inner_pattern {
                            self.destructure_pattern(inner_pattern, *value);
                        }
                        return self.evaluate(arm.expression);
                    }
                }
                ConstValue::Missing(
                    self.diagnostics.report(
                        expr.stable_ptr.untyped(),
                        SemanticDiagnosticKind::UnsupportedConstant,
                    ),
                )
            }
            Expr::If(expr) => match &expr.condition {
                crate::Condition::BoolExpr(id) => {
                    let condition = self.evaluate(*id);
                    let ConstValue::Enum(variant, _) = condition else {
                        return ConstValue::Missing(skip_diagnostic());
                    };
                    if variant == true_variant(self.db) {
                        self.evaluate(expr.if_block)
                    } else if let Some(else_block) = expr.else_block {
                        self.evaluate(else_block)
                    } else {
                        self.unit_const.clone()
                    }
                }
                crate::Condition::Let(id, patterns) => {
                    let value = self.evaluate(*id);
                    let ConstValue::Enum(variant, value) = value else {
                        return ConstValue::Missing(skip_diagnostic());
                    };
                    for pattern_id in patterns {
                        let Pattern::EnumVariant(pattern) = &self.arenas.patterns[*pattern_id]
                        else {
                            continue;
                        };
                        if pattern.variant != variant {
                            continue;
                        }
                        if let Some(inner_pattern) = pattern.inner_pattern {
                            self.destructure_pattern(inner_pattern, *value);
                        }
                        return self.evaluate(expr.if_block);
                    }
                    if let Some(else_block) = expr.else_block {
                        self.evaluate(else_block)
                    } else {
                        self.unit_const.clone()
                    }
                }
            },
            _ => ConstValue::Missing(skip_diagnostic()),
        }
    }

    /// Attempts to evaluate constants from a const function call.
    fn evaluate_function_call(&mut self, expr: &ExprFunctionCall) -> ConstValue {
        let db = self.db;
        let args = expr
            .args
            .iter()
            .filter_map(|arg| try_extract_matches!(arg, ExprFunctionCallArg::Value))
            .map(|arg| self.evaluate(*arg))
            .collect_vec();
        if expr.function == self.panic_with_felt252 {
            return ConstValue::Missing(self.diagnostics.report(
                expr.stable_ptr.untyped(),
                SemanticDiagnosticKind::FailedConstantCalculation,
            ));
        }
        let concrete_function =
            match self.generic_substitution.substitute(db, expr.function.get_concrete(db)) {
                Ok(v) => v,
                Err(err) => return ConstValue::Missing(err),
            };
        if let Some(calc_result) =
            self.evaluate_const_function_call(&concrete_function, &args, expr)
        {
            return calc_result;
        }

        let imp = extract_matches!(concrete_function.generic_function, GenericFunctionId::Impl);
        let bool_value = |condition: bool| {
            if condition { self.true_const.clone() } else { self.false_const.clone() }
        };

        if imp.function == self.eq_fn {
            return bool_value(args[0] == args[1]);
        } else if imp.function == self.ne_fn {
            return bool_value(args[0] != args[1]);
        } else if imp.function == self.not_fn {
            return bool_value(args[0] == self.false_const);
        }

        let args = match args
            .into_iter()
            .map(|arg| NumericArg::try_new(db, arg))
            .collect::<Option<Vec<_>>>()
        {
            Some(args) => args,
            // Diagnostic can be skipped as we would either have a semantic error for a bad arg for
            // the function, or the arg itself couldn't have been calculated.
            None => return ConstValue::Missing(skip_diagnostic()),
        };
        let mut value = match imp.function {
            id if id == self.neg_fn => -&args[0].v,
            id if id == self.add_fn => &args[0].v + &args[1].v,
            id if id == self.sub_fn => &args[0].v - &args[1].v,
            id if id == self.mul_fn => &args[0].v * &args[1].v,
            id if (id == self.div_fn || id == self.rem_fn) && args[1].v.is_zero() => {
                return ConstValue::Missing(
                    self.diagnostics
                        .report(expr.stable_ptr.untyped(), SemanticDiagnosticKind::DivisionByZero),
                );
            }
            id if id == self.div_fn => &args[0].v / &args[1].v,
            id if id == self.rem_fn => &args[0].v % &args[1].v,
            id if id == self.bitand_fn => &args[0].v & &args[1].v,
            id if id == self.bitor_fn => &args[0].v | &args[1].v,
            id if id == self.bitxor_fn => &args[0].v ^ &args[1].v,
            id if id == self.lt_fn => return bool_value(args[0].v < args[1].v),
            id if id == self.le_fn => return bool_value(args[0].v <= args[1].v),
            id if id == self.gt_fn => return bool_value(args[0].v > args[1].v),
            id if id == self.ge_fn => return bool_value(args[0].v >= args[1].v),
            id if id == self.div_rem_fn => {
                // No need for non-zero check as this is type checked to begin with.
                // Also results are always in the range of the input type, so `unwrap`s are ok.
                return ConstValue::Struct(
                    vec![
                        value_as_const_value(db, args[0].ty, &(&args[0].v / &args[1].v)).unwrap(),
                        value_as_const_value(db, args[0].ty, &(&args[0].v % &args[1].v)).unwrap(),
                    ],
                    expr.ty,
                );
            }
            _ => {
                unreachable!("Unexpected function call in constant lowering: {:?}", expr)
            }
        };
        if expr.ty == db.core_info().felt252 {
            // Specifically handling felt252s since their evaluation is more complex.
            value %= BigInt::from_str_radix(
                "800000000000011000000000000000000000000000000000000000000000001",
                16,
            )
            .unwrap();
        }
        value_as_const_value(db, expr.ty, &value)
            .map_err(|err| {
                self.diagnostics
                    .report(expr.stable_ptr.untyped(), SemanticDiagnosticKind::LiteralError(err))
            })
            .unwrap_or_else(ConstValue::Missing)
    }

    /// Attempts to evaluate a constant function call.
    fn evaluate_const_function_call(
        &mut self,
        concrete_function: &ConcreteFunction,
        args: &[ConstValue],
        expr: &ExprFunctionCall,
    ) -> Option<ConstValue> {
        let db = self.db;
        if let GenericFunctionId::Extern(extern_fn) = concrete_function.generic_function {
            let expr_ty = self.generic_substitution.substitute(db, expr.ty).ok()?;
            if self.upcast_fns.contains(&extern_fn) {
                let [ConstValue::Int(value, _)] = args else { return None };
                return Some(ConstValue::Int(value.clone(), expr_ty));
            } else if self.downcast_fns.contains(&extern_fn) {
                let [ConstValue::Int(value, _)] = args else { return None };
                let TypeLongId::Concrete(ConcreteTypeId::Enum(enm)) = expr_ty.lookup_intern(db)
                else {
                    return None;
                };
                let (some, none) =
                    db.concrete_enum_variants(enm).ok()?.into_iter().collect_tuple()?;
                let success_ty = some.ty;
                return Some(match validate_literal(db, success_ty, value) {
                    Ok(()) => {
                        ConstValue::Enum(some, ConstValue::Int(value.clone(), success_ty).into())
                    }
                    Err(LiteralError::OutOfRange(_)) => {
                        ConstValue::Enum(none, self.unit_const.clone().into())
                    }
                    Err(LiteralError::InvalidTypeForLiteral(_)) => unreachable!(
                        "`downcast` is only allowed into types that can be literals. Got `{}`.",
                        success_ty.format(db)
                    ),
                });
            } else {
                unreachable!(
                    "Unexpected extern function in constant lowering: `{}`",
                    extern_fn.full_path(db.upcast())
                );
            }
        }
        let body_id = concrete_function.body(db).ok()??;
        let concrete_body_id = body_id.function_with_body_id(db);
        let signature = db.function_with_body_signature(concrete_body_id).ok()?;
        require(signature.is_const)?;
        let generic_substitution = body_id.substitution(db).ok()?;
        let body = db.function_body(concrete_body_id).ok()?;
        const MAX_CONST_EVAL_DEPTH: usize = 100;
        if self.depth > MAX_CONST_EVAL_DEPTH {
            return Some(ConstValue::Missing(self.diagnostics.report(
                expr.stable_ptr,
                SemanticDiagnosticKind::ConstantCalculationDepthExceeded,
            )));
        }
        let mut diagnostics = SemanticDiagnostics::default();
        let mut inner = ConstantEvaluateContext {
            db,
            info: self.info,
            arenas: &body.arenas,
            vars: signature
                .params
                .into_iter()
                .map(|p| VarId::Param(p.id))
                .zip(args.iter().cloned())
                .collect(),
            generic_substitution,
            depth: self.depth + 1,
            diagnostics: &mut diagnostics,
        };
        let value = inner.evaluate(body.body_expr);
        for diagnostic in diagnostics.build().get_all() {
            let location = diagnostic.location(db.elongate());
            let (inner_diag, mut notes) = match diagnostic.kind {
                SemanticDiagnosticKind::ConstantCalculationDepthExceeded => {
                    self.diagnostics.report(
                        expr.stable_ptr,
                        SemanticDiagnosticKind::ConstantCalculationDepthExceeded,
                    );
                    continue;
                }
                SemanticDiagnosticKind::InnerFailedConstantCalculation(inner_diag, notes) => {
                    (inner_diag, notes)
                }
                _ => (diagnostic.into(), vec![]),
            };
            notes.push(DiagnosticNote::with_location(
                format!("In `{}`", concrete_function.full_path(db)),
                location,
            ));
            self.diagnostics.report(
                expr.stable_ptr,
                SemanticDiagnosticKind::InnerFailedConstantCalculation(inner_diag, notes),
            );
        }
        Some(value)
    }

    /// Extract const member access from a const value.
    fn evaluate_member_access(&mut self, expr: &ExprMemberAccess) -> Maybe<ConstValue> {
        let full_struct = self.evaluate(expr.expr);
        let ConstValue::Struct(mut values, _) = full_struct else {
            // A semantic diagnostic should have been reported.
            return Err(skip_diagnostic());
        };
        let members = self.db.concrete_struct_members(expr.concrete_struct_id)?;
        let Some(member_idx) = members.iter().position(|(_, member)| member.id == expr.member)
        else {
            // A semantic diagnostic should have been reported.
            return Err(skip_diagnostic());
        };
        Ok(values.swap_remove(member_idx))
    }

    /// Destructures the pattern into the const value of the variables in scope.
    fn destructure_pattern(&mut self, pattern_id: PatternId, value: ConstValue) {
        let pattern = &self.arenas.patterns[pattern_id];
        match pattern {
            Pattern::Literal(_)
            | Pattern::StringLiteral(_)
            | Pattern::Otherwise(_)
            | Pattern::Missing(_) => {}
            Pattern::Variable(pattern) => {
                self.vars.insert(VarId::Local(pattern.var.id), value);
            }
            Pattern::Struct(pattern) => {
                if let ConstValue::Struct(inner_values, _) = value {
                    let member_order =
                        match self.db.concrete_struct_members(pattern.concrete_struct_id) {
                            Ok(member_order) => member_order,
                            Err(_) => return,
                        };
                    for (member, inner_value) in zip(member_order.values(), inner_values) {
                        if let Some((_, inner_pattern)) =
                            pattern.field_patterns.iter().find(|(field, _)| member.id == field.id)
                        {
                            self.destructure_pattern(*inner_pattern, inner_value);
                        }
                    }
                }
            }
            Pattern::Tuple(pattern) => {
                if let ConstValue::Struct(inner_values, _) = value {
                    for (inner_pattern, inner_value) in zip(&pattern.field_patterns, inner_values) {
                        self.destructure_pattern(*inner_pattern, inner_value);
                    }
                }
            }
            Pattern::FixedSizeArray(pattern) => {
                if let ConstValue::Struct(inner_values, _) = value {
                    for (inner_pattern, inner_value) in
                        zip(&pattern.elements_patterns, inner_values)
                    {
                        self.destructure_pattern(*inner_pattern, inner_value);
                    }
                }
            }
            Pattern::EnumVariant(pattern) => {
                if let ConstValue::Enum(variant, inner_value) = value {
                    if pattern.variant == variant {
                        if let Some(inner_pattern) = pattern.inner_pattern {
                            self.destructure_pattern(inner_pattern, *inner_value);
                        }
                    }
                }
            }
        }
    }
}

impl std::ops::Deref for ConstantEvaluateContext<'_> {
    type Target = ConstCalcInfo;
    fn deref(&self) -> &Self::Target {
        self.info
    }
}

/// Helper for the arguments info.
struct NumericArg {
    /// The arg's integer value.
    v: BigInt,
    /// The arg's type.
    ty: TypeId,
}
impl NumericArg {
    fn try_new(db: &dyn SemanticGroup, arg: ConstValue) -> Option<Self> {
        Some(Self { ty: arg.ty(db).ok()?, v: numeric_arg_value(arg)? })
    }
}

/// Helper for creating a `NumericArg` value.
/// This includes unwrapping of `NonZero` values and struct of 2 values as a `u256`.
fn numeric_arg_value(value: ConstValue) -> Option<BigInt> {
    match value {
        ConstValue::Int(value, _) => Some(value),
        ConstValue::Struct(v, _) => {
            if let [ConstValue::Int(low, _), ConstValue::Int(high, _)] = &v[..] {
                Some(low + (high << 128))
            } else {
                None
            }
        }
        ConstValue::NonZero(const_value) => numeric_arg_value(*const_value),
        _ => None,
    }
}

/// Query implementation of [SemanticGroup::constant_semantic_diagnostics].
pub fn constant_semantic_diagnostics(
    db: &dyn SemanticGroup,
    const_id: ConstantId,
) -> Diagnostics<SemanticDiagnostic> {
    db.priv_constant_semantic_data(const_id, false).map(|data| data.diagnostics).unwrap_or_default()
}

/// Query implementation of [SemanticGroup::constant_semantic_data].
pub fn constant_semantic_data(db: &dyn SemanticGroup, const_id: ConstantId) -> Maybe<Constant> {
    db.priv_constant_semantic_data(const_id, false)?.constant
}

/// Cycle handling for [SemanticGroup::constant_semantic_data].
pub fn constant_semantic_data_cycle(
    db: &dyn SemanticGroup,
    _cycle: &salsa::Cycle,
    const_id: &ConstantId,
) -> Maybe<Constant> {
    // Forwarding cycle handling to `priv_constant_semantic_data` handler.
    db.priv_constant_semantic_data(*const_id, true)?.constant
}

/// Query implementation of [crate::db::SemanticGroup::constant_resolver_data].
pub fn constant_resolver_data(
    db: &dyn SemanticGroup,
    const_id: ConstantId,
) -> Maybe<Arc<ResolverData>> {
    Ok(db.priv_constant_semantic_data(const_id, false)?.resolver_data)
}

/// Cycle handling for [crate::db::SemanticGroup::constant_resolver_data].
pub fn constant_resolver_data_cycle(
    db: &dyn SemanticGroup,
    _cycle: &salsa::Cycle,
    const_id: &ConstantId,
) -> Maybe<Arc<ResolverData>> {
    Ok(db.priv_constant_semantic_data(*const_id, true)?.resolver_data)
}

/// Query implementation of [crate::db::SemanticGroup::constant_const_value].
pub fn constant_const_value(db: &dyn SemanticGroup, const_id: ConstantId) -> Maybe<ConstValueId> {
    Ok(db.priv_constant_semantic_data(const_id, false)?.const_value)
}

/// Cycle handling for [crate::db::SemanticGroup::constant_const_value].
pub fn constant_const_value_cycle(
    db: &dyn SemanticGroup,
    _cycle: &salsa::Cycle,
    const_id: &ConstantId,
) -> Maybe<ConstValueId> {
    // Forwarding cycle handling to `priv_constant_semantic_data` handler.
    Ok(db.priv_constant_semantic_data(*const_id, true)?.const_value)
}

/// Query implementation of [crate::db::SemanticGroup::constant_const_type].
pub fn constant_const_type(db: &dyn SemanticGroup, const_id: ConstantId) -> Maybe<TypeId> {
    db.priv_constant_semantic_data(const_id, false)?.const_value.ty(db)
}

/// Cycle handling for [crate::db::SemanticGroup::constant_const_type].
pub fn constant_const_type_cycle(
    db: &dyn SemanticGroup,
    _cycle: &salsa::Cycle,
    const_id: &ConstantId,
) -> Maybe<TypeId> {
    // Forwarding cycle handling to `priv_constant_semantic_data` handler.
    db.priv_constant_semantic_data(*const_id, true)?.const_value.ty(db)
}

/// Query implementation of [crate::db::SemanticGroup::const_calc_info].
pub fn const_calc_info(db: &dyn SemanticGroup) -> Arc<ConstCalcInfo> {
    Arc::new(ConstCalcInfo::new(db))
}

/// Holds static information about extern functions required for const calculations.
#[derive(Debug, PartialEq, Eq)]
pub struct ConstCalcInfo {
    /// Traits that are allowed for consts if their impls is in the corelib.
    const_traits: UnorderedHashSet<TraitId>,
    /// The const value for the unit type `()`.
    unit_const: ConstValue,
    /// The const value for `true`.
    true_const: ConstValue,
    /// The const value for `false`.
    false_const: ConstValue,
    /// The function for panicking with a felt252.
    panic_with_felt252: FunctionId,
    /// The integer `upcast` function.
    upcast_fns: UnorderedHashSet<ExternFunctionId>,
    /// The integer `downcast` function.
    downcast_fns: UnorderedHashSet<ExternFunctionId>,

    core_info: Arc<CoreInfo>,
}

impl std::ops::Deref for ConstCalcInfo {
    type Target = CoreInfo;
    fn deref(&self) -> &CoreInfo {
        &self.core_info
    }
}

impl ConstCalcInfo {
    /// Creates a new ConstCalcInfo.
    fn new(db: &dyn SemanticGroup) -> Self {
        let core_info = db.core_info();
        let unit_const = ConstValue::Struct(vec![], unit_ty(db));
        let core = ModuleHelper::core(db);
        let integer = core.submodule("integer");
        let starknet = core.submodule("starknet");
        let class_hash_module = starknet.submodule("class_hash");
        let contract_address_module = starknet.submodule("contract_address");
        Self {
            const_traits: FromIterator::from_iter([
                core_info.neg_trt,
                core_info.add_trt,
                core_info.sub_trt,
                core_info.mul_trt,
                core_info.div_trt,
                core_info.rem_trt,
                core_info.div_rem_trt,
                core_info.bitand_trt,
                core_info.bitor_trt,
                core_info.bitxor_trt,
                core_info.partialeq_trt,
                core_info.partialord_trt,
                core_info.not_trt,
            ]),
            true_const: ConstValue::Enum(true_variant(db), unit_const.clone().into()),
            false_const: ConstValue::Enum(false_variant(db), unit_const.clone().into()),
            unit_const,
            panic_with_felt252: core.function_id("panic_with_felt252", vec![]),
            upcast_fns: FromIterator::from_iter([
                integer.extern_function_id("upcast"),
                integer.extern_function_id("u8_to_felt252"),
                integer.extern_function_id("u16_to_felt252"),
                integer.extern_function_id("u32_to_felt252"),
                integer.extern_function_id("u64_to_felt252"),
                integer.extern_function_id("u128_to_felt252"),
                integer.extern_function_id("i8_to_felt252"),
                integer.extern_function_id("i16_to_felt252"),
                integer.extern_function_id("i32_to_felt252"),
                integer.extern_function_id("i64_to_felt252"),
                integer.extern_function_id("i128_to_felt252"),
                class_hash_module.extern_function_id("class_hash_to_felt252"),
                contract_address_module.extern_function_id("contract_address_to_felt252"),
            ]),
            downcast_fns: FromIterator::from_iter([
                integer.extern_function_id("downcast"),
                integer.extern_function_id("u8_try_from_felt252"),
                integer.extern_function_id("u16_try_from_felt252"),
                integer.extern_function_id("u32_try_from_felt252"),
                integer.extern_function_id("u64_try_from_felt252"),
                integer.extern_function_id("i8_try_from_felt252"),
                integer.extern_function_id("i16_try_from_felt252"),
                integer.extern_function_id("i32_try_from_felt252"),
                integer.extern_function_id("i64_try_from_felt252"),
                integer.extern_function_id("i128_try_from_felt252"),
                class_hash_module.extern_function_id("class_hash_try_from_felt252"),
                contract_address_module.extern_function_id("contract_address_try_from_felt252"),
            ]),
            core_info,
        }
    }
}
