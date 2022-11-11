use defs::ids::{EnumId, GenericFunctionId, GenericTypeId, ModuleId, ModuleItemId, SubmoduleId};
use filesystem::ids::CrateLongId;
use smol_str::SmolStr;
use syntax::node::ast::{self, BinaryOperator};
use utils::{extract_matches, OptionFrom};

use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnosticKind;
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
    get_core_ty_by_name(db, db.core_module(), "felt".into(), vec![])
}

pub fn core_uint128_ty(db: &dyn SemanticGroup) -> TypeId {
    get_core_ty_by_name(db, get_core_submodule(db, "integer"), "uint128".into(), vec![])
}

pub fn core_nonzero_ty(db: &dyn SemanticGroup, inner_type: TypeId) -> TypeId {
    get_core_ty_by_name(
        db,
        db.core_module(),
        "NonZero".into(),
        vec![GenericArgumentId::Type(inner_type)],
    )
}

fn get_core_ty_by_name(
    db: &dyn SemanticGroup,
    module: ModuleId,
    name: SmolStr,
    generic_args: Vec<GenericArgumentId>,
) -> TypeId {
    // This should not fail if the corelib is present.
    let generic_type = db
        .module_item_by_name(module, name.clone())
        .and_then(GenericTypeId::option_from)
        .unwrap_or_else(|| panic!("Type '{name}' was not found in core lib."));
    db.intern_type(semantic::TypeLongId::Concrete(semantic::ConcreteTypeId::new(
        db,
        generic_type,
        generic_args,
    )))
}

fn get_core_submodule(db: &dyn SemanticGroup, submodule: &str) -> ModuleId {
    let core_module = db.core_module();
    let submodule_id = db
        .module_item_by_name(core_module, submodule.into())
        .and_then(SubmoduleId::option_from)
        .unwrap_or_else(|| panic!("Submodule 'core::{submodule}' was not found."));
    ModuleId::Submodule(submodule_id)
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
    type1: TypeId,
    type2: TypeId,
) -> Result<FunctionId, SemanticDiagnosticKind> {
    // TODO(lior): Replace current hard-coded implementation with an implementation that is based on
    //   traits.
    let felt = core_felt_ty(db);
    let uint128 = core_uint128_ty(db);
    let (module, function_name) = match binary_op {
        BinaryOperator::Plus(_) => {
            if type1 == felt && type2 == felt {
                (db.core_module(), "felt_add")
            } else if type1 == uint128 && type2 == uint128 {
                (get_core_submodule(db, "integer"), "uint128_add")
            } else {
                return Err(SemanticDiagnosticKind::UnsupportedBinaryOperator {
                    op: "+".into(),
                    type1,
                    type2,
                });
            }
        }
        BinaryOperator::Minus(_) => {
            if type1 == felt && type2 == felt {
                (db.core_module(), "felt_sub")
            } else if type1 == uint128 && type2 == uint128 {
                (get_core_submodule(db, "integer"), "uint128_sub")
            } else {
                return Err(SemanticDiagnosticKind::UnsupportedBinaryOperator {
                    op: "-".into(),
                    type1,
                    type2,
                });
            }
        }
        BinaryOperator::Mul(_) => (db.core_module(), "felt_mul"),
        BinaryOperator::Div(_) => (db.core_module(), "felt_div"),
        BinaryOperator::EqEq(_) => (db.core_module(), "felt_eq"),
        BinaryOperator::AndAnd(_) => (db.core_module(), "bool_and"),
        BinaryOperator::OrOr(_) => (db.core_module(), "bool_or"),
        BinaryOperator::LE(_) => (db.core_module(), "felt_le"),
        BinaryOperator::GE(_) => (db.core_module(), "felt_ge"),
        BinaryOperator::LT(_) => (db.core_module(), "felt_lt"),
        BinaryOperator::GT(_) => (db.core_module(), "felt_gt"),
        _ => return Err(SemanticDiagnosticKind::UnknownBinaryOperator),
    };
    Ok(get_core_function_id(db, module, function_name.into(), vec![]))
}

pub fn felt_eq(db: &dyn SemanticGroup) -> FunctionId {
    get_core_function_id(db, db.core_module(), "felt_eq".into(), vec![])
}

pub fn felt_sub(db: &dyn SemanticGroup) -> FunctionId {
    get_core_function_id(db, db.core_module(), "felt_sub".into(), vec![])
}

pub fn core_jump_nz_func(db: &dyn SemanticGroup) -> FunctionId {
    get_core_function_id(db, db.core_module(), "felt_jump_nz".into(), vec![])
}

/// Given a core library function name and its generic arguments, returns [FunctionId].
fn get_core_function_id(
    db: &dyn SemanticGroup,
    module: ModuleId,
    name: SmolStr,
    generic_args: Vec<GenericArgumentId>,
) -> FunctionId {
    let generic_function = db
        .module_item_by_name(module, name.clone())
        .and_then(GenericFunctionId::option_from)
        .unwrap_or_else(|| panic!("Function '{name}' was not found in core lib."));
    db.intern_function(FunctionLongId {
        function: ConcreteFunction { generic_function, generic_args },
    })
}
