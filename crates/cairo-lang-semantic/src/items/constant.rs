use std::sync::Arc;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::{
    ConstantId, GenericParamId, LanguageElementId, LookupItemId, ModuleItemId,
    NamedLanguageElementId, TraitConstantId,
};
use cairo_lang_diagnostics::{skip_diagnostic, DiagnosticAdded, Diagnostics, Maybe, ToMaybe};
use cairo_lang_proc_macros::{DebugWithDb, SemanticObject};
use cairo_lang_syntax::node::ast::ItemConstant;
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_syntax::node::{TypedStablePtr, TypedSyntaxNode};
use cairo_lang_utils::{
    define_short_id, extract_matches, try_extract_matches, Intern, LookupIntern,
};
use id_arena::Arena;
use itertools::Itertools;
use num_bigint::BigInt;
use num_traits::{Num, ToPrimitive, Zero};
use smol_str::SmolStr;

use super::feature_kind::extract_allowed_features;
use super::functions::{GenericFunctionId, GenericFunctionWithBodyId};
use super::imp::ImplId;
use super::structure::SemanticStructEx;
use crate::corelib::{
    core_felt252_ty, get_core_trait, get_core_ty_by_name, try_extract_nz_wrapped_type,
    validate_literal, LiteralError,
};
use crate::db::SemanticGroup;
use crate::diagnostic::{SemanticDiagnosticKind, SemanticDiagnostics, SemanticDiagnosticsBuilder};
use crate::expr::compute::{compute_expr_semantic, ComputationContext, Environment, ExprAndId};
use crate::expr::inference::conform::InferenceConform;
use crate::expr::inference::{ConstVar, InferenceId};
use crate::literals::try_extract_minus_literal;
use crate::resolve::{Resolver, ResolverData};
use crate::substitution::SemanticRewriter;
use crate::types::resolve_type;
use crate::{
    semantic_object_for_id, ConcreteVariant, Expr, ExprBlock, ExprConstant, ExprFunctionCall,
    ExprFunctionCallArg, ExprId, ExprMemberAccess, ExprStructCtor, FunctionId, SemanticDiagnostic,
    TypeId,
};

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct Constant {
    /// The actual id of the const expression value.
    pub value: ExprId,
    /// The arena of all the expressions for the const calculation.
    pub exprs: Arc<Arena<Expr>>,
}
impl Constant {
    pub fn ty(&self) -> TypeId {
        self.exprs[self.value].ty()
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
    pub const_value: ConstValue,
    pub ty: TypeId,
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
        self.lookup_intern(db).is_fully_concrete()
    }

    /// Returns true if the const does not contain any inference variables.
    pub fn is_var_free(&self, db: &dyn SemanticGroup) -> bool {
        self.lookup_intern(db).is_var_free()
    }
}

/// A constant value.
#[derive(Clone, Debug, Hash, PartialEq, Eq, SemanticObject)]
pub enum ConstValue {
    Int(#[dont_rewrite] BigInt),
    Struct(Vec<(TypeId, ConstValue)>),
    Enum(ConcreteVariant, Box<ConstValue>),
    NonZero(TypeId, Box<ConstValue>),
    Boxed(TypeId, Box<ConstValue>),
    Generic(#[dont_rewrite] GenericParamId),
    ImplConstant(ImplConstantId),
    Var(ConstVar),
    /// A missing value, used in cases where the value is not known due to diagnostics.
    Missing(#[dont_rewrite] DiagnosticAdded),
}
impl ConstValue {
    /// Returns true if the const does not depend on any generics.
    pub fn is_fully_concrete(&self) -> bool {
        match self {
            ConstValue::Int(_) => true,
            ConstValue::Struct(members) => {
                members.iter().all(|(_, member)| member.is_fully_concrete())
            }
            ConstValue::Enum(_, value)
            | ConstValue::NonZero(_, value)
            | ConstValue::Boxed(_, value) => value.is_fully_concrete(),
            ConstValue::Generic(_)
            | ConstValue::Var(_)
            | ConstValue::Missing(_)
            | ConstValue::ImplConstant(_) => false,
        }
    }

    /// Returns true if the const does not contain any inference variables.
    pub fn is_var_free(&self) -> bool {
        match self {
            ConstValue::Int(_)
            | ConstValue::Generic(_)
            | ConstValue::ImplConstant(_)
            | ConstValue::Missing(_) => true,
            ConstValue::Struct(members) => members.iter().all(|(_, member)| member.is_var_free()),
            ConstValue::Enum(_, value)
            | ConstValue::NonZero(_, value)
            | ConstValue::Boxed(_, value) => value.is_var_free(),
            ConstValue::Var(_) => false,
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
    /// Creates a new impl constand id. For an impl constamt of a concrete impl, asserts that the
    /// trait constant belongs to the same trait that the impl implements (panics if not).
    pub fn new(
        impl_id: ImplId,
        trait_constant_id: TraitConstantId,
        db: &dyn SemanticGroup,
    ) -> Self {
        if let crate::items::imp::ImplId::Concrete(concrete_impl) = impl_id {
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

pub fn reduce_impl_constant_id(
    db: &dyn SemanticGroup,
    impl_constant_id: ImplConstantId,
    resolver: &mut Resolver<'_>,
) -> Maybe<(ConstValueId, TypeId)> {
    let impl_id = resolver.inference().rewrite(impl_constant_id.impl_id()).unwrap();
    let impl_constant_id = ImplConstantId::new(impl_id, impl_constant_id.trait_constant_id, db);
    // Try to implize the impl constant if its impl is concrete.
    if let Some(constant) = db.impl_constant_concrete_implized(impl_constant_id)? {
        return Ok(constant);
    }

    // Try to implize by the impl context, if given. E.g. for `Self::MyConst`
    // inside an impl.
    if let Some(impl_def_id) = resolver.trait_or_impl_ctx.impl_context() {
        if let Some(constant) =
            db.impl_constant_implized_by_context(impl_constant_id, impl_def_id)?
        {
            return Ok(constant);
        }
    }
    let trait_constant = impl_constant_id.trait_constant_id();
    let ty = db.trait_constant_type(trait_constant)?;
    Ok((ConstValue::ImplConstant(impl_constant_id).intern(db), ty))
}

/// Query implementation of [SemanticGroup::priv_constant_semantic_data].
pub fn priv_constant_semantic_data(
    db: &dyn SemanticGroup,
    const_id: ConstantId,
) -> Maybe<ConstantData> {
    let lookup_item_id = LookupItemId::ModuleItem(ModuleItemId::Constant(const_id));
    constant_semantic_data_helper(
        db,
        &db.module_constant_by_id(const_id)?.to_maybe()?,
        lookup_item_id,
        None,
        &const_id,
    )
}

/// Cycle handling for [SemanticGroup::priv_constant_semantic_data].
pub fn priv_constant_semantic_data_cycle(
    db: &dyn SemanticGroup,
    _cycle: &[String],
    const_id: &ConstantId,
) -> Maybe<ConstantData> {
    let lookup_item_id = LookupItemId::ModuleItem(ModuleItemId::Constant(*const_id));
    constant_semantic_data_cycle_helper(
        db,
        &db.module_constant_by_id(*const_id)?.to_maybe()?,
        lookup_item_id,
        None,
        const_id,
    )
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

    // TODO(TomerStarkware): check if we should clone the allowed features instead of overiting
    // them.
    resolver.data.allowed_features =
        extract_allowed_features(db.upcast(), element_id, constant_ast, &mut diagnostics);

    let constant_type = resolve_type(
        db,
        &mut diagnostics,
        &mut resolver,
        &constant_ast.type_clause(syntax_db).ty(syntax_db),
    );

    let environment = Environment::empty();
    let mut ctx = ComputationContext::new(db, &mut diagnostics, None, resolver, None, environment);

    let value = compute_expr_semantic(&mut ctx, &constant_ast.value(syntax_db), None);
    let (ty, const_value) = resolve_const_expr_and_evaluate(
        db,
        &mut ctx,
        &value,
        constant_ast.stable_ptr().untyped(),
        constant_type,
    );

    let resolver_data = Arc::new(ctx.resolver.data);
    let constant = Constant { value: value.id, exprs: Arc::new(ctx.exprs) };
    Ok(ConstantData {
        diagnostics: diagnostics.build(),
        const_value,
        ty,
        constant: Ok(constant),
        resolver_data,
    })
}

// cycle handling for constant_semantic_data_helper.
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

    let diagnostic_add = diagnostics.report(constant_ast, SemanticDiagnosticKind::ConstCycle);
    Ok(ConstantData {
        constant: Err(diagnostic_add),
        const_value: ConstValue::Missing(diagnostic_add),
        ty: TypeId::missing(db, diagnostic_add),
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
) -> (TypeId, ConstValue) {
    let inference = &mut ctx.resolver.inference();
    if let Err(err_set) = inference.conform_ty(value.ty(), target_type) {
        inference.report_on_pending_error(err_set, ctx.diagnostics, const_stable_ptr);
    }
    // Check fully resolved.
    inference.finalize(ctx.diagnostics, const_stable_ptr);
    ctx.apply_inference_rewriter_to_exprs();

    match &value.expr {
        Expr::Constant(ExprConstant { const_value_id, ty, .. }) => {
            (*ty, const_value_id.lookup_intern(db))
        }
        // Check that the expression is a valid constant.
        _ => evaluate_constant_expr(db, &ctx.exprs, value.id, ctx.diagnostics),
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
            ConstValue::Int(value.clone())
        } else {
            let u128_ty = get_core_ty_by_name(db.upcast(), "u128".into(), vec![]);
            let mask128 = BigInt::from(u128::MAX);
            let low = value & mask128;
            let high = value >> 128;
            ConstValue::Struct(vec![
                (u128_ty, ConstValue::Int(low)),
                (u128_ty, ConstValue::Int(high)),
            ])
        }
    };

    if let Some(inner) = try_extract_nz_wrapped_type(db.upcast(), ty) {
        Ok(ConstValue::NonZero(inner, Box::new(get_basic_const_value(inner))))
    } else {
        Ok(get_basic_const_value(ty))
    }
}

/// evaluate the given const expression value.
pub fn evaluate_constant_expr(
    db: &dyn SemanticGroup,
    exprs: &Arena<Expr>,
    expr_id: ExprId,
    diagnostics: &mut SemanticDiagnostics,
) -> (TypeId, ConstValue) {
    let expr = &exprs[expr_id];
    (
        expr.ty(),
        match expr {
            Expr::Constant(expr) => expr.const_value_id.lookup_intern(db),
            Expr::Block(ExprBlock { statements, tail: Some(inner), .. })
                if statements.is_empty() =>
            {
                evaluate_constant_expr(db, exprs, *inner, diagnostics).1
            }
            Expr::FunctionCall(expr) => evaluate_const_function_call(db, exprs, expr, diagnostics)
                .map(|value| {
                    value_as_const_value(db, expr.ty, &value)
                        .map_err(|err| {
                            diagnostics.report_by_ptr(
                                expr.stable_ptr.untyped(),
                                SemanticDiagnosticKind::LiteralError(err),
                            )
                        })
                        .unwrap_or_else(ConstValue::Missing)
                })
                .unwrap_or_else(ConstValue::Missing),
            Expr::Literal(expr) => value_as_const_value(db, expr.ty, &expr.value)
                .map_err(|err| {
                    diagnostics.report_by_ptr(
                        expr.stable_ptr.untyped(),
                        SemanticDiagnosticKind::LiteralError(err),
                    )
                })
                .unwrap_or_else(ConstValue::Missing),
            Expr::Tuple(expr) => ConstValue::Struct(
                expr.items
                    .iter()
                    .map(|expr_id| evaluate_constant_expr(db, exprs, *expr_id, diagnostics))
                    .collect(),
            ),
            Expr::StructCtor(ExprStructCtor { members, base_struct: None, .. }) => {
                ConstValue::Struct(
                    members
                        .iter()
                        .map(|(_, expr_id)| {
                            evaluate_constant_expr(db, exprs, *expr_id, diagnostics)
                        })
                        .collect(),
                )
            }
            Expr::EnumVariantCtor(expr) => ConstValue::Enum(
                expr.variant.clone(),
                Box::new(evaluate_constant_expr(db, exprs, expr.value_expr, diagnostics).1),
            ),
            Expr::MemberAccess(expr) => extract_const_member_access(db, exprs, expr, diagnostics)
                .unwrap_or_else(ConstValue::Missing),
            Expr::FixedSizeArray(expr) => ConstValue::Struct(match &expr.items {
                crate::FixedSizeArrayItems::Items(items) => items
                    .iter()
                    .map(|expr_id| evaluate_constant_expr(db, exprs, *expr_id, diagnostics))
                    .collect(),
                crate::FixedSizeArrayItems::ValueAndSize(value, count) => {
                    let value = evaluate_constant_expr(db, exprs, *value, diagnostics).1;
                    let count = count.lookup_intern(db);
                    if let ConstValue::Int(count) = count {
                        (0..count.to_usize().unwrap())
                            .map(|_| value.clone())
                            .map(|value| (expr.ty, value))
                            .collect()
                    } else {
                        diagnostics.report_by_ptr(
                            expr.stable_ptr.untyped(),
                            SemanticDiagnosticKind::UnsupportedConstant,
                        );
                        vec![]
                    }
                }
            }),
            _ if diagnostics.error_count == 0 => ConstValue::Missing(diagnostics.report_by_ptr(
                expr.stable_ptr().untyped(),
                SemanticDiagnosticKind::UnsupportedConstant,
            )),
            _ => ConstValue::Missing(skip_diagnostic()),
        },
    )
}

/// Returns true if the given function is allowed to be called in constant context.
fn is_function_const(db: &dyn SemanticGroup, function_id: FunctionId) -> bool {
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
    let expected_trait_name = match imp.function.name(db.upcast()).as_str() {
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
    trait_id == get_core_trait(db, expected_trait_name.into())
}

/// Attempts to evaluate constants from a function call.
fn evaluate_const_function_call(
    db: &dyn SemanticGroup,
    exprs: &Arena<Expr>,
    expr: &ExprFunctionCall,
    diagnostics: &mut SemanticDiagnostics,
) -> Maybe<BigInt> {
    if let Some(value) = try_extract_minus_literal(db.upcast(), exprs, expr) {
        return Ok(value);
    }
    let args = expr
        .args
        .iter()
        .filter_map(|arg| try_extract_matches!(arg, ExprFunctionCallArg::Value))
        .map(|arg| {
            match evaluate_constant_expr(db, exprs, *arg, diagnostics).1 {
                ConstValue::Int(v) => Ok(v),
                // Handling u256 constants to enable const evaluation of them.
                ConstValue::Struct(v) => {
                    if let [(_, ConstValue::Int(low)), (_, ConstValue::Int(high))] = &v[..] {
                        Ok(low + (high << 128))
                    } else {
                        Err(diagnostics.report_by_ptr(
                            exprs[*arg].stable_ptr().untyped(),
                            SemanticDiagnosticKind::UnsupportedConstant,
                        ))
                    }
                }
                ConstValue::Missing(err) => Err(err),
                _ => Err(diagnostics.report_by_ptr(
                    exprs[*arg].stable_ptr().untyped(),
                    SemanticDiagnosticKind::UnsupportedConstant,
                )),
            }
        })
        .collect_vec()
        .into_iter()
        .collect::<Result<Vec<_>, _>>()?;

    if !is_function_const(db, expr.function) {
        return Err(diagnostics.report_by_ptr(
            expr.stable_ptr.untyped(),
            SemanticDiagnosticKind::UnsupportedConstant,
        ));
    }

    let imp = extract_matches!(
        expr.function.get_concrete(db.upcast()).generic_function,
        GenericFunctionId::Impl
    );
    let is_felt252_ty = expr.ty == core_felt252_ty(db.upcast());
    let mut value = match imp.function.name(db.upcast()).as_str() {
        "neg" => -&args[0],
        "add" => &args[0] + &args[1],
        "sub" => &args[0] - &args[1],
        "mul" => &args[0] * &args[1],
        "div" | "rem" if args[1].is_zero() => {
            return Err(diagnostics
                .report_by_ptr(expr.stable_ptr.untyped(), SemanticDiagnosticKind::DivisionByZero));
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
    Ok(value)
}

/// Extract const member access from a const value.
fn extract_const_member_access(
    db: &dyn SemanticGroup,
    exprs: &Arena<Expr>,
    expr: &ExprMemberAccess,
    diagnostics: &mut SemanticDiagnostics,
) -> Maybe<ConstValue> {
    let full_struct = evaluate_constant_expr(db, exprs, expr.expr, diagnostics).1;
    let ConstValue::Struct(mut values) = full_struct else {
        return Err(diagnostics.report_by_ptr(
            exprs[expr.expr].stable_ptr().untyped(),
            SemanticDiagnosticKind::UnsupportedConstant,
        ));
    };
    let members = db.concrete_struct_members(expr.concrete_struct_id)?;
    let Some(member_idx) = members.iter().position(|(_, member)| member.id == expr.member) else {
        return Err(diagnostics.report_by_ptr(
            exprs[expr.expr].stable_ptr().untyped(),
            SemanticDiagnosticKind::UnsupportedConstant,
        ));
    };
    Ok(values.swap_remove(member_idx).1)
}

/// Query implementation of [SemanticGroup::constant_semantic_diagnostics].
pub fn constant_semantic_diagnostics(
    db: &dyn SemanticGroup,
    const_id: ConstantId,
) -> Diagnostics<SemanticDiagnostic> {
    db.priv_constant_semantic_data(const_id).map(|data| data.diagnostics).unwrap_or_default()
}

/// Query implementation of [SemanticGroup::constant_semantic_data].
pub fn constant_semantic_data(db: &dyn SemanticGroup, const_id: ConstantId) -> Maybe<Constant> {
    db.priv_constant_semantic_data(const_id)?.constant
}

/// Cycle handling for [SemanticGroup::constant_semantic_data].
pub fn constant_semantic_data_cycle(
    db: &dyn SemanticGroup,
    _cycle: &[String],
    const_id: &ConstantId,
) -> Maybe<Constant> {
    // Forwarding cycle handling to `priv_constant_semantic_data` handler.
    constant_semantic_data(db, *const_id)
}

/// Query implementation of [crate::db::SemanticGroup::constant_resolver_data].
pub fn constant_resolver_data(
    db: &dyn SemanticGroup,
    const_id: ConstantId,
) -> Maybe<Arc<ResolverData>> {
    Ok(db.priv_constant_semantic_data(const_id)?.resolver_data)
}

/// Cycle handling for [crate::db::SemanticGroup::constant_resolver_data].
pub fn constant_resolver_data_cycle(
    db: &dyn SemanticGroup,
    _cycle: &[String],
    const_id: &ConstantId,
) -> Maybe<Arc<ResolverData>> {
    constant_resolver_data(db, *const_id)
}

/// Query implementation of [crate::db::SemanticGroup::constant_const_value].
pub fn constant_const_value(db: &dyn SemanticGroup, const_id: ConstantId) -> Maybe<ConstValue> {
    Ok(db.priv_constant_semantic_data(const_id)?.const_value)
}

/// Cycle handling for [crate::db::SemanticGroup::constant_const_value].
pub fn constant_const_value_cycle(
    db: &dyn SemanticGroup,
    _cycle: &[String],
    const_id: &ConstantId,
) -> Maybe<ConstValue> {
    // Forwarding cycle handling to `priv_constant_semantic_data` handler.
    Ok(db.priv_constant_semantic_data(*const_id)?.const_value)
}

/// Query implementation of [crate::db::SemanticGroup::constant_const_type].
pub fn constant_const_type(db: &dyn SemanticGroup, const_id: ConstantId) -> Maybe<TypeId> {
    Ok(db.priv_constant_semantic_data(const_id)?.ty)
}

/// Cycle handling for [crate::db::SemanticGroup::constant_const_type].
pub fn constant_const_type_cycle(
    db: &dyn SemanticGroup,
    _cycle: &[String],
    const_id: &ConstantId,
) -> Maybe<TypeId> {
    // Forwarding cycle handling to `priv_constant_semantic_data` handler.
    Ok(db.priv_constant_semantic_data(*const_id)?.ty)
}
