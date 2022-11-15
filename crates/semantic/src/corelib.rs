use defs::ids::{EnumId, GenericFunctionId, GenericTypeId, ModuleId, ModuleItemId, TraitId};
use filesystem::ids::CrateLongId;
use smol_str::SmolStr;
use syntax::node::ast::{self, BinaryOperator};
use utils::{extract_matches, try_extract_matches, OptionFrom};

use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnosticKind;
use crate::expr::compute::ComputationContext;
use crate::items::enm::SemanticEnumEx;
use crate::items::trt::ConcreteTraitId;
use crate::resolve_path::ResolvedGenericItem;
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

pub fn core_uint128_ty(db: &dyn SemanticGroup) -> TypeId {
    get_core_ty_by_name(db, "uint128".into(), vec![])
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
    let module_item_id = db
        .module_item_by_name(core_module, name.clone())
        .unwrap_or_else(|| panic!("Type '{name}' was not found in core lib."));
    let generic_type = match module_item_id {
        ModuleItemId::Use(use_id) => {
            db.use_resolved_item(use_id).and_then(|resolved_generic_item| {
                try_extract_matches!(resolved_generic_item, ResolvedGenericItem::GenericType)
            })
        }
        _ => GenericTypeId::option_from(module_item_id),
    }
    .unwrap_or_else(|| panic!("{name} is not a type."));

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
    type1: TypeId,
    type2: TypeId,
) -> Result<FunctionId, SemanticDiagnosticKind> {
    // TODO(lior): Replace current hard-coded implementation with an implementation that is based on
    //   traits.
    // TODO(lior): Add type checking for all of the operators.
    let felt = core_felt_ty(db);
    let uint128 = core_uint128_ty(db);
    let function_name = match binary_op {
        BinaryOperator::Plus(_) => {
            if type1 == felt && type2 == felt {
                "felt_add"
            } else if type1 == uint128 && type2 == uint128 {
                "uint128_add"
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
                "felt_sub"
            } else if type1 == uint128 && type2 == uint128 {
                "uint128_sub"
            } else {
                return Err(SemanticDiagnosticKind::UnsupportedBinaryOperator {
                    op: "-".into(),
                    type1,
                    type2,
                });
            }
        }
        BinaryOperator::Mul(_) => "felt_mul",
        BinaryOperator::Div(_) => "felt_div",
        BinaryOperator::EqEq(_) => "felt_eq",
        BinaryOperator::AndAnd(_) => "bool_and",
        BinaryOperator::OrOr(_) => "bool_or",
        BinaryOperator::LE(_) => "felt_le",
        BinaryOperator::GE(_) => "felt_ge",
        BinaryOperator::LT(_) => "felt_lt",
        BinaryOperator::GT(_) => "felt_gt",
        _ => return Err(SemanticDiagnosticKind::UnknownBinaryOperator),
    };
    Ok(get_core_function_id(db, function_name.into(), vec![]))
}

pub fn felt_eq(db: &dyn SemanticGroup) -> FunctionId {
    get_core_function_id(db, "felt_eq".into(), vec![])
}

pub fn felt_sub(db: &dyn SemanticGroup) -> FunctionId {
    get_core_function_id(db, "felt_sub".into(), vec![])
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
    let module_item_id = db
        .module_item_by_name(core_module, name.clone())
        .unwrap_or_else(|| panic!("Function '{name}' was not found in core lib."));
    let generic_function = match module_item_id {
        ModuleItemId::Use(use_id) => {
            db.use_resolved_item(use_id).and_then(|resolved_generic_item| {
                try_extract_matches!(resolved_generic_item, ResolvedGenericItem::GenericFunction)
            })
        }
        _ => GenericFunctionId::option_from(module_item_id),
    }
    .unwrap_or_else(|| panic!("{name} is not a function."));

    db.intern_function(FunctionLongId {
        function: ConcreteFunction { generic_function, generic_args },
    })
}

pub fn concrete_copy_trait(db: &dyn SemanticGroup, ty: TypeId) -> ConcreteTraitId {
    get_core_concrete_trait(db, "Copy".into(), vec![GenericArgumentId::Type(ty)])
}

pub fn concrete_drop_trait(db: &dyn SemanticGroup, ty: TypeId) -> ConcreteTraitId {
    get_core_concrete_trait(db, "Drop".into(), vec![GenericArgumentId::Type(ty)])
}

pub fn copy_trait(db: &dyn SemanticGroup) -> TraitId {
    get_core_trait(db, "Copy".into())
}

pub fn drop_trait(db: &dyn SemanticGroup) -> TraitId {
    get_core_trait(db, "Drop".into())
}

/// Given a core library trait name and its generic arguments, returns [ConcreteTraitId].
fn get_core_concrete_trait(
    db: &dyn SemanticGroup,
    name: SmolStr,
    generic_args: Vec<GenericArgumentId>,
) -> ConcreteTraitId {
    let trait_id = get_core_trait(db, name);
    db.intern_concrete_trait(semantic::ConcreteTraitLongId { trait_id, generic_args })
}

/// Given a core library trait name, returns [TraitId].
fn get_core_trait(db: &dyn SemanticGroup, name: SmolStr) -> TraitId {
    let core_module = db.core_module();
    // This should not fail if the corelib is present.
    let use_id =
        extract_matches!(db.module_item_by_name(core_module, name).unwrap(), ModuleItemId::Use);
    let trait_id =
        extract_matches!(db.use_resolved_item(use_id).unwrap(), ResolvedGenericItem::Trait);
    trait_id
}
