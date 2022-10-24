use defs::ids::{EnumId, GenericFunctionId, GenericTypeId, ModuleId, ModuleItemId};
use filesystem::ids::CrateLongId;
use smol_str::SmolStr;
use syntax::node::ast::{self, BinaryOperator};
use utils::{extract_matches, OptionFrom};

use crate::db::SemanticGroup;
use crate::expr::compute::ComputationContext;
use crate::items::enm::SemanticEnumEx;
use crate::types::ConcreteEnumLongId;
use crate::{
    semantic, ConcreteEnumId, ConcreteFunction, ConcreteVariant, Expr, ExprId, ExprTuple,
    FunctionId, FunctionLongId, GenericArgumentId, TypeId, TypeLongId,
};

pub fn core_module(db: &dyn SemanticGroup) -> ModuleId {
    let core_crate = db.intern_crate(CrateLongId("core".into()));
    ModuleId::CrateRoot(core_crate)
}

pub fn core_felt_ty(db: &dyn SemanticGroup) -> TypeId {
    get_core_ty_by_name(db, "felt".into(), vec![])
}

pub fn core_nonzero_ty(db: &dyn SemanticGroup, inner_type: TypeId) -> TypeId {
    get_core_ty_by_name(db, "NonZero".into(), vec![GenericArgumentId::Type(inner_type)])
}

fn get_core_ty_by_name(
    db: &dyn SemanticGroup,
    name: SmolStr,
    generic_args: Vec<GenericArgumentId>,
) -> TypeId {
    let core_module = db.core_module();
    // This should not fail if the corelib is present.
    let generic_type = db
        .module_item_by_name(core_module, name.clone())
        .and_then(GenericTypeId::option_from)
        .unwrap_or_else(|| panic!("Type '{name}' was not found in core lib."));
    db.intern_type(semantic::TypeLongId::Concrete(semantic::ConcreteTypeId::new(
        db,
        generic_type,
        generic_args,
    )))
}

pub fn core_bool_ty(db: &dyn SemanticGroup) -> TypeId {
    let core_module = db.core_module();
    // This should not fail if the corelib is present.
    let generic_type = db
        .module_item_by_name(core_module, "bool".into())
        .and_then(GenericTypeId::option_from)
        .expect("Type bool was not found in core lib.");
    db.intern_type(semantic::TypeLongId::Concrete(semantic::ConcreteTypeId::new(
        db,
        generic_type,
        vec![],
    )))
}

// TODO(spapini): Consider making all these queries for better caching.
/// Generates a ConcreteEnumId instance for `bool`.
pub fn core_bool_enum(db: &dyn SemanticGroup) -> ConcreteEnumId {
    let core_module = db.core_module();
    // This should not fail if the corelib is present.
    let enum_id = db
        .module_item_by_name(core_module, "bool".into())
        .and_then(EnumId::option_from)
        .expect("Type bool was not found in core lib.");
    db.intern_concrete_enum(ConcreteEnumLongId { enum_id, generic_args: vec![] })
}

/// Generates a ConcreteVariant instance for `false`.
pub fn false_variant(db: &dyn SemanticGroup) -> ConcreteVariant {
    get_enum_concrete_variant(db, core_module(db), "bool", "False")
}

/// Generates a ConcreteVariant instance for `true`.
pub fn true_variant(db: &dyn SemanticGroup) -> ConcreteVariant {
    get_enum_concrete_variant(db, core_module(db), "bool", "True")
}

/// Gets a semantic expression of the literal `false`. Uses the given `stable_ptr` in the returned
/// semantic expression.
pub fn false_literal_expr(
    ctx: &mut ComputationContext<'_>,
    stable_ptr: ast::ExprPtr,
) -> semantic::Expr {
    get_bool_variant_expr(ctx, core_module(ctx.db), "bool", "False", stable_ptr)
}

/// Gets a semantic expression of the literal `true`. Uses the given `stable_ptr` in the returned
/// semantic expression.
pub fn true_literal_expr(
    ctx: &mut ComputationContext<'_>,
    stable_ptr: ast::ExprPtr,
) -> semantic::Expr {
    get_bool_variant_expr(ctx, core_module(ctx.db), "bool", "True", stable_ptr)
}

/// Gets a semantic expression of the specified bool enum variant. Uses the given `stable_ptr` in
/// the returned semantic expression.
fn get_bool_variant_expr(
    ctx: &mut ComputationContext<'_>,
    module_id: ModuleId,
    enum_name: &str,
    variant_name: &str,
    stable_ptr: ast::ExprPtr,
) -> semantic::Expr {
    let concrete_variant = get_enum_concrete_variant(ctx.db, module_id, enum_name, variant_name);
    semantic::Expr::EnumVariantCtor(semantic::ExprEnumVariantCtor {
        variant: concrete_variant,
        value_expr: unit_expr(ctx, stable_ptr),
        ty: core_bool_ty(ctx.db),
        stable_ptr,
    })
}

/// Gets a [ConcreteVariant] instance for an enum variant, by name.
/// Assumes the variant exists.
fn get_enum_concrete_variant(
    db: &dyn SemanticGroup,
    module_id: ModuleId,
    enum_name: &str,
    variant_name: &str,
) -> ConcreteVariant {
    let enum_item = db.module_item_by_name(module_id, enum_name.into()).unwrap();
    let enum_id = extract_matches!(enum_item, ModuleItemId::Enum);
    let concrete_enum_id =
        db.intern_concrete_enum(ConcreteEnumLongId { enum_id, generic_args: vec![] });
    let variant_id = db.enum_variants(enum_id).unwrap()[variant_name];
    let variant = db.variant_semantic(enum_id, variant_id).unwrap();
    db.concrete_enum_variant(concrete_enum_id, &variant).unwrap()
}

/// Gets the unit type ().
pub fn unit_ty(db: &dyn SemanticGroup) -> TypeId {
    db.intern_type(semantic::TypeLongId::Tuple(vec![]))
}

/// builds a semantic unit expression. This is not necessarily located in the AST, so it is received
/// as a param.
pub fn unit_expr(ctx: &mut ComputationContext<'_>, stable_ptr: ast::ExprPtr) -> ExprId {
    ctx.exprs.alloc(Expr::Tuple(ExprTuple {
        items: Vec::new(),
        ty: ctx.db.intern_type(TypeLongId::Tuple(Vec::new())),
        stable_ptr,
    }))
}

pub fn core_binary_operator(
    db: &dyn SemanticGroup,
    binary_op: &BinaryOperator,
) -> Option<FunctionId> {
    let function_name = match binary_op {
        BinaryOperator::Plus(_) => "felt_add",
        BinaryOperator::Minus(_) => "felt_sub",
        BinaryOperator::Mul(_) => "felt_mul",
        BinaryOperator::Div(_) => "felt_div",
        BinaryOperator::EqEq(_) => "felt_eq",
        BinaryOperator::AndAnd(_) => "bool_and",
        BinaryOperator::OrOr(_) => "bool_or",
        BinaryOperator::LE(_) => "felt_le",
        BinaryOperator::GE(_) => "felt_ge",
        BinaryOperator::LT(_) => "felt_lt",
        BinaryOperator::GT(_) => "felt_gt",
        _ => return None,
    };
    Some(get_core_function_id(db, function_name.into(), vec![]))
}

pub fn felt_eq(db: &dyn SemanticGroup) -> FunctionId {
    get_core_function_id(db, "felt_eq".into(), vec![])
}

pub fn core_jump_nz_func(db: &dyn SemanticGroup) -> FunctionId {
    get_core_function_id(db, "felt_jump_nz".into(), vec![])
}

/// Given a core library function name and its generic arguments, returns [FunctionId].
fn get_core_function_id(
    db: &dyn SemanticGroup,
    name: SmolStr,
    generic_args: Vec<GenericArgumentId>,
) -> FunctionId {
    let core_module = db.core_module();
    let generic_function = db
        .module_item_by_name(core_module, name.clone())
        .and_then(GenericFunctionId::option_from)
        .unwrap_or_else(|| panic!("Function '{name}' was not found in core lib."));
    db.intern_function(FunctionLongId::Concrete(ConcreteFunction {
        generic_function,
        generic_args,
    }))
}
