use std::sync::Arc;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::{
    ConstantId, GenericParamId, LanguageElementId, LookupItemId, ModuleItemId,
    NamedLanguageElementId, TraitConstantId,
};
use cairo_lang_diagnostics::{DiagnosticAdded, Diagnostics, Maybe, ToMaybe, skip_diagnostic};
use cairo_lang_proc_macros::{DebugWithDb, SemanticObject};
use cairo_lang_syntax::node::ast::ItemConstant;
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_syntax::node::{TypedStablePtr, TypedSyntaxNode};
use cairo_lang_utils::{
    Intern, LookupIntern, define_short_id, extract_matches, try_extract_matches,
};
use itertools::Itertools;
use num_bigint::BigInt;
use num_traits::{Num, ToPrimitive, Zero};
use smol_str::SmolStr;

use super::functions::{GenericFunctionId, GenericFunctionWithBodyId};
use super::imp::ImplId;
use crate::corelib::{
    CoreTraitContext, LiteralError, core_box_ty, core_nonzero_ty, get_core_trait,
    get_core_ty_by_name, try_extract_nz_wrapped_type, validate_literal,
};
use crate::db::SemanticGroup;
use crate::diagnostic::{SemanticDiagnosticKind, SemanticDiagnostics, SemanticDiagnosticsBuilder};
use crate::expr::compute::{
    ComputationContext, ContextFunction, Environment, ExprAndId, compute_expr_semantic,
};
use crate::expr::inference::conform::InferenceConform;
use crate::expr::inference::{ConstVar, InferenceId};
use crate::literals::try_extract_minus_literal;
use crate::resolve::{Resolver, ResolverData};
use crate::substitution::SemanticRewriter;
use crate::types::resolve_type;
use crate::{
    Arenas, ConcreteTypeId, ConcreteVariant, Expr, ExprBlock, ExprConstant, ExprFunctionCall,
    ExprFunctionCallArg, ExprId, ExprMemberAccess, ExprStructCtor, FunctionId, GenericParam,
    SemanticDiagnostic, TypeId, TypeLongId, semantic_object_for_id,
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
    TraitConstant(TraitConstantId),
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
                | ConstValue::ImplConstant(_)
                | ConstValue::TraitConstant(_) => false,
            }
    }

    /// Returns true if the const does not contain any inference variables.
    pub fn is_var_free(&self, db: &dyn SemanticGroup) -> bool {
        self.ty(db).unwrap().is_var_free(db)
            && match self {
                ConstValue::Int(_, _)
                | ConstValue::Generic(_)
                | ConstValue::Missing(_)
                | ConstValue::TraitConstant(_) => true,
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
            ConstValue::TraitConstant(trait_constant) => db.trait_constant_type(*trait_constant)?,
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
        if let crate::items::imp::ImplLongId::Concrete(concrete_impl) = impl_id.lookup_intern(db) {
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
            let mut eval_ctx =
                ConstantEvaluateContext { db, arenas: &ctx.arenas, diagnostics: ctx.diagnostics };
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
    validate_literal(db.upcast(), ty, value.clone())?;
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
    arenas: &'a Arenas,
    diagnostics: &'a mut SemanticDiagnostics,
}
impl ConstantEvaluateContext<'_> {
    /// Validate the given expression can be used as constant.
    fn validate(&mut self, expr_id: ExprId) {
        match &self.arenas.exprs[expr_id] {
            Expr::Var(_) | Expr::Constant(_) | Expr::Missing(_) => {}
            Expr::Block(ExprBlock { statements, tail: Some(inner), .. })
                if statements.is_empty() =>
            {
                self.validate(*inner);
            }
            Expr::FunctionCall(expr) => {
                if let Some(value) = try_extract_minus_literal(self.db, &self.arenas.exprs, expr) {
                    if let Err(err) = validate_literal(self.db, expr.ty, value) {
                        self.diagnostics.report(
                            expr.stable_ptr.untyped(),
                            SemanticDiagnosticKind::LiteralError(err),
                        );
                    }
                    return;
                }
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
                if let Err(err) = validate_literal(self.db, expr.ty, expr.value.clone()) {
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
        let db = self.db;
        let concrete_function = function_id.get_concrete(db);
        let Ok(Some(body)) = concrete_function.body(db) else { return false };
        let GenericFunctionWithBodyId::Impl(imp) = body.generic_function(db) else { return false };
        let impl_def = imp.concrete_impl_id.impl_def_id(db);
        if impl_def.parent_module(db.upcast()).owning_crate(db.upcast()) != db.core_crate() {
            return false;
        }
        let Ok(trait_id) = db.impl_def_trait(impl_def) else {
            return false;
        };
        let expected_trait_name = match imp.function_body.name(db).as_str() {
            "neg" => "Neg",
            "add" => "Add",
            "sub" => "Sub",
            "mul" => "Mul",
            "div" => "Div",
            "rem" => "Rem",
            "bitand" => "BitAnd",
            "bitor" => "BitOr",
            "bitxor" => "BitXor",
            _ => return false,
        };
        trait_id == get_core_trait(db, CoreTraitContext::TopLevel, expected_trait_name.into())
    }

    /// Evaluate the given const expression value.
    fn evaluate(&mut self, expr_id: ExprId) -> ConstValue {
        let expr = &self.arenas.exprs[expr_id];
        let db = self.db;
        match expr {
            Expr::Constant(expr) => expr.const_value_id.lookup_intern(db),
            Expr::Block(ExprBlock { statements, tail: Some(inner), .. })
                if statements.is_empty() =>
            {
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
            _ => ConstValue::Missing(skip_diagnostic()),
        }
    }

    /// Attempts to evaluate constants from a const function call.
    fn evaluate_function_call(&mut self, expr: &ExprFunctionCall) -> ConstValue {
        let db = self.db;
        if let Some(value) = try_extract_minus_literal(db.upcast(), &self.arenas.exprs, expr) {
            return value_as_const_value(db, expr.ty, &value)
                .expect("LiteralError should have been caught on `validate`");
        }
        let args = match expr
            .args
            .iter()
            .filter_map(|arg| try_extract_matches!(arg, ExprFunctionCallArg::Value))
            .map(|arg| match self.evaluate(*arg) {
                ConstValue::Int(v, _ty) => Ok(v),
                // Handling u256 constants to enable const evaluation of them.
                ConstValue::Struct(v, _) => {
                    if let [ConstValue::Int(low, _), ConstValue::Int(high, _)] = &v[..] {
                        Ok(low + (high << 128))
                    } else {
                        Err(self.diagnostics.report(
                            self.arenas.exprs[*arg].stable_ptr().untyped(),
                            SemanticDiagnosticKind::UnsupportedConstant,
                        ))
                    }
                }
                ConstValue::Missing(err) => Err(err),
                // Dignostic can be skipped as we would either have a semantic error for a bad arg
                // for the function, or the arg itself could'nt have been calculated.
                _ => Err(skip_diagnostic()),
            })
            .collect_vec()
            .into_iter()
            .collect::<Result<Vec<_>, _>>()
        {
            Ok(args) => args,
            Err(err) => return ConstValue::Missing(err),
        };

        let imp = extract_matches!(
            expr.function.get_concrete(db.upcast()).generic_function,
            GenericFunctionId::Impl
        );
        let is_felt252_ty = expr.ty == db.core_felt252_ty();
        let mut value = match imp.function.name(db.upcast()).as_str() {
            "neg" => -&args[0],
            "add" => &args[0] + &args[1],
            "sub" => &args[0] - &args[1],
            "mul" => &args[0] * &args[1],
            "div" | "rem" if args[1].is_zero() => {
                return ConstValue::Missing(
                    self.diagnostics
                        .report(expr.stable_ptr.untyped(), SemanticDiagnosticKind::DivisionByZero),
                );
            }
            "div" if !is_felt252_ty => &args[0] / &args[1],
            "rem" if !is_felt252_ty => &args[0] % &args[1],
            "bitand" if !is_felt252_ty => &args[0] & &args[1],
            "bitor" if !is_felt252_ty => &args[0] | &args[1],
            "bitxor" if !is_felt252_ty => &args[0] ^ &args[1],
            _ => unreachable!("Unexpected function call in constant lowering: {:?}", expr),
        };
        if is_felt252_ty {
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
