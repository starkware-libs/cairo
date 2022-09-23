use defs::ids::{GenericFunctionId, GenericTypeId, ModuleId, ModuleItemId};
use filesystem::ids::CrateLongId;
use syntax::node::ast::BinaryOperator;
use syntax::node::ids::SyntaxStablePtrId;
use syntax::node::TypedSyntaxNode;
use utils::{extract_matches, OptionFrom};

use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnostics;
use crate::resolve_item::specialize_function;
use crate::{semantic, Expr, ExprId, ExprTuple, FunctionId, TypeId, TypeLongId};

pub fn core_module(db: &dyn SemanticGroup) -> ModuleId {
    let core_crate = db.intern_crate(CrateLongId("core".into()));
    ModuleId::CrateRoot(core_crate)
}

pub fn core_felt_ty(db: &dyn SemanticGroup) -> TypeId {
    let core_module = db.core_module();
    // This should not fail if the corelib is present.
    let generic_type = db
        .module_item_by_name(core_module, "felt".into())
        .and_then(GenericTypeId::option_from)
        .unwrap();
    db.intern_type(semantic::TypeLongId::Concrete(semantic::ConcreteType {
        generic_type,
        generic_args: vec![],
    }))
}

pub fn core_bool_ty(db: &dyn SemanticGroup) -> TypeId {
    let core_module = db.core_module();
    // This should not fail if the corelib is present.
    let generic_type = db
        .module_item_by_name(core_module, "bool".into())
        .and_then(GenericTypeId::option_from)
        .unwrap();
    db.intern_type(semantic::TypeLongId::Concrete(semantic::ConcreteType {
        generic_type,
        generic_args: vec![],
    }))
}

/// Gets a semantic expression of the literal `false`. Uses the given `stable_ptr` in the returned
/// semantic expression.
pub fn false_literal_expr(db: &dyn SemanticGroup, stable_ptr: SyntaxStablePtrId) -> semantic::Expr {
    get_enum_variant(db, core_module(db), "bool", "False", stable_ptr)
}

/// Gets a semantic expression of the literal `true`. Uses the given `stable_ptr` in the returned
/// semantic expression.
pub fn true_literal_expr(db: &dyn SemanticGroup, stable_ptr: SyntaxStablePtrId) -> semantic::Expr {
    get_enum_variant(db, core_module(db), "bool", "True", stable_ptr)
}

/// Gets a semantic expression of the specified enum variant. Uses the given `stable_ptr` in the
/// returned semantic expression.
fn get_enum_variant(
    db: &dyn SemanticGroup,
    module_id: ModuleId,
    enum_name: &str,
    variant_name: &str,
    stable_ptr: SyntaxStablePtrId,
) -> semantic::Expr {
    let bool_enum = db.module_item_by_name(module_id, enum_name.into()).unwrap();
    let bool_enum_id = extract_matches!(bool_enum, ModuleItemId::Enum);
    let variant_id = db.enum_variants(bool_enum_id).unwrap()[variant_name].id;
    semantic::Expr::ExprEnumVariantCtor(semantic::ExprEnumVariantCtor {
        enum_variant_id: variant_id,
        value_expr: unit_expr(db, stable_ptr),
        ty: core_bool_ty(db),
        stable_ptr,
    })
}

pub fn unit_ty(db: &dyn SemanticGroup) -> TypeId {
    db.intern_type(semantic::TypeLongId::Tuple(vec![]))
}

/// builds a semantic unit expression. This is not necessarily located in the AST, so it is received
/// as a param.
pub fn unit_expr(db: &dyn SemanticGroup, stable_ptr: SyntaxStablePtrId) -> ExprId {
    db.intern_expr(Expr::ExprTuple(ExprTuple {
        items: Vec::new(),
        ty: db.intern_type(TypeLongId::Tuple(Vec::new())),
        stable_ptr,
    }))
}

pub fn core_binary_operator(
    db: &dyn SemanticGroup,
    diagnostics: &mut SemanticDiagnostics,
    binary_op: &BinaryOperator,
) -> Option<FunctionId> {
    let core_module = db.core_module();
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
    let generic_function = db
        .module_item_by_name(core_module, function_name.into())
        .and_then(GenericFunctionId::option_from)
        .expect("Operator function not found in core lib.");
    specialize_function(db, diagnostics, binary_op.stable_ptr().untyped(), generic_function, vec![])
}
