use defs::ids::{EnumId, GenericFunctionId, GenericTypeId, ModuleId, ModuleItemId, TraitId};
use diagnostics::{Maybe, ToOption};
use filesystem::ids::CrateLongId;
use smol_str::SmolStr;
use syntax::node::ast::{self, BinaryOperator, UnaryOperator};
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

pub fn core_nonzero_ty(db: &dyn SemanticGroup, inner_type: TypeId) -> TypeId {
    get_core_ty_by_name(db, "NonZero".into(), vec![GenericArgumentId::Type(inner_type)])
}

pub fn try_get_core_ty_by_name(
    db: &dyn SemanticGroup,
    name: SmolStr,
    generic_args: Vec<GenericArgumentId>,
) -> Result<TypeId, SemanticDiagnosticKind> {
    let core_module = db.core_module();
    // This should not fail if the corelib is present.
    let module_item_id = db
        .module_item_by_name(core_module, name.clone())
        .map_err(|_| SemanticDiagnosticKind::UnknownType)?
        .ok_or(SemanticDiagnosticKind::UnknownType)?;
    let generic_type = match module_item_id {
        ModuleItemId::Use(use_id) => {
            db.use_resolved_item(use_id).to_option().and_then(|resolved_generic_item| {
                try_extract_matches!(resolved_generic_item, ResolvedGenericItem::GenericType)
            })
        }
        _ => GenericTypeId::option_from(module_item_id),
    }
    .unwrap_or_else(|| panic!("{name} is not a type."));

    Ok(db.intern_type(semantic::TypeLongId::Concrete(semantic::ConcreteTypeId::new(
        db,
        generic_type,
        generic_args,
    ))))
}

pub fn get_core_ty_by_name(
    db: &dyn SemanticGroup,
    name: SmolStr,
    generic_args: Vec<GenericArgumentId>,
) -> TypeId {
    try_get_core_ty_by_name(db, name, generic_args).unwrap()
}

pub fn core_bool_ty(db: &dyn SemanticGroup) -> TypeId {
    let core_module = db.core_module();
    // This should not fail if the corelib is present.
    let generic_type = db
        .module_item_by_name(core_module, "bool".into())
        .expect("Failed to load core lib.")
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
        .expect("Failed to load core lib.")
        .and_then(EnumId::option_from)
        .expect("Type bool was not found in core lib.");
    db.intern_concrete_enum(ConcreteEnumLongId { enum_id, generic_args: vec![] })
}

/// Generates a ConcreteVariant instance for `false`.
pub fn false_variant(db: &dyn SemanticGroup) -> ConcreteVariant {
    get_enum_concrete_variant(db, "bool", vec![], "False")
}

/// Generates a ConcreteVariant instance for `true`.
pub fn true_variant(db: &dyn SemanticGroup) -> ConcreteVariant {
    get_enum_concrete_variant(db, "bool", vec![], "True")
}

/// Generates a ConcreteVariant instance for `JumpNzResult::<felt>::Zero`.
pub fn jump_nz_zero_variant(db: &dyn SemanticGroup) -> ConcreteVariant {
    get_enum_concrete_variant(
        db,
        "JumpNzResult",
        vec![GenericArgumentId::Type(core_felt_ty(db))],
        "Zero",
    )
}

/// Generates a ConcreteVariant instance for `JumpNzResult::<felt>::NonZero`.
pub fn jump_nz_nonzero_variant(db: &dyn SemanticGroup) -> ConcreteVariant {
    get_enum_concrete_variant(
        db,
        "JumpNzResult",
        vec![GenericArgumentId::Type(core_felt_ty(db))],
        "NonZero",
    )
}

/// Gets a semantic expression of the literal `false`. Uses the given `stable_ptr` in the returned
/// semantic expression.
pub fn false_literal_expr(
    ctx: &mut ComputationContext<'_>,
    stable_ptr: ast::ExprPtr,
) -> semantic::Expr {
    get_bool_variant_expr(ctx, "bool", "False", stable_ptr)
}

/// Gets a semantic expression of the literal `true`. Uses the given `stable_ptr` in the returned
/// semantic expression.
pub fn true_literal_expr(
    ctx: &mut ComputationContext<'_>,
    stable_ptr: ast::ExprPtr,
) -> semantic::Expr {
    get_bool_variant_expr(ctx, "bool", "True", stable_ptr)
}

/// Gets a semantic expression of the specified bool enum variant. Uses the given `stable_ptr` in
/// the returned semantic expression.
fn get_bool_variant_expr(
    ctx: &mut ComputationContext<'_>,
    enum_name: &str,
    variant_name: &str,
    stable_ptr: ast::ExprPtr,
) -> semantic::Expr {
    let concrete_variant = get_enum_concrete_variant(ctx.db, enum_name, vec![], variant_name);
    semantic::Expr::EnumVariantCtor(semantic::ExprEnumVariantCtor {
        variant: concrete_variant,
        value_expr: unit_expr(ctx, stable_ptr),
        ty: core_bool_ty(ctx.db),
        stable_ptr,
    })
}

/// Gets a [ConcreteVariant] instance for an enum variant, by name.
/// Assumes the variant exists.
pub fn get_enum_concrete_variant(
    db: &dyn SemanticGroup,
    enum_name: &str,
    generic_args: Vec<GenericArgumentId>,
    variant_name: &str,
) -> ConcreteVariant {
    let module_id = core_module(db);
    let enum_item = db.module_item_by_name(module_id, enum_name.into()).unwrap().unwrap();
    let enum_id = extract_matches!(enum_item, ModuleItemId::Enum);
    let concrete_enum_id = db.intern_concrete_enum(ConcreteEnumLongId { enum_id, generic_args });
    let variant_id = db.enum_variants(enum_id).unwrap()[variant_name];
    let variant = db.variant_semantic(enum_id, variant_id).unwrap();
    db.concrete_enum_variant(concrete_enum_id, &variant).unwrap()
}

/// Gets the unit type ().
pub fn unit_ty(db: &dyn SemanticGroup) -> TypeId {
    db.intern_type(semantic::TypeLongId::Tuple(vec![]))
}

/// Gets the never type ().
pub fn never_ty(db: &dyn SemanticGroup) -> TypeId {
    let core_module = db.core_module();
    // This should not fail if the corelib is present.
    let generic_type = db
        .module_item_by_name(core_module, "never".into())
        .expect("Failed to load core lib.")
        .and_then(GenericTypeId::option_from)
        .expect("Type bool was not found in core lib.");
    db.intern_type(semantic::TypeLongId::Concrete(semantic::ConcreteTypeId::new(
        db,
        generic_type,
        vec![],
    )))
}

/// Attempts to unwrap error propagation types (Option, Result).
/// Returns None if not one of these types.
pub fn unwrap_error_propagation_type(
    db: &dyn SemanticGroup,
    ty: TypeId,
) -> Option<(ConcreteVariant, ConcreteVariant)> {
    match db.lookup_intern_type(ty) {
        // Only enums may be `Result` and `Option` types.
        TypeLongId::Concrete(semantic::ConcreteTypeId::Enum(enm)) => {
            let name = enm.enum_id(db.upcast()).name(db.upcast());
            if name == "Option" || name == "Result" {
                if let [ok_variant, err_variant] =
                    db.concrete_enum_variants(enm).to_option()?.as_slice()
                {
                    Some((ok_variant.clone(), err_variant.clone()))
                } else {
                    None
                }
            } else {
                None
            }
        }
        TypeLongId::GenericParameter(_) => todo!(
            "When generic types are supported, if type is of matching type, allow unwrapping it \
             to type."
        ),
        TypeLongId::Concrete(
            semantic::ConcreteTypeId::Struct(_) | semantic::ConcreteTypeId::Extern(_),
        )
        | TypeLongId::Tuple(_)
        | TypeLongId::Missing(_) => None,
    }
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

pub fn core_unary_operator(
    db: &dyn SemanticGroup,
    unary_op: &UnaryOperator,
    ty: TypeId,
) -> Result<FunctionId, SemanticDiagnosticKind> {
    let felt_ty = core_felt_ty(db);
    let bool_ty = core_bool_ty(db);
    let unsupported_operator =
        |op: &str| Err(SemanticDiagnosticKind::UnsupportedUnaryOperator { op: op.into(), ty });

    let function_name = match unary_op {
        UnaryOperator::Minus(_) if ty == felt_ty => "felt_neg",
        UnaryOperator::Minus(_) => return unsupported_operator("-"),

        UnaryOperator::Not(_) if ty == bool_ty => "bool_not",
        UnaryOperator::Not(_) => return unsupported_operator("!"),
    };
    Ok(get_core_function_id(db, function_name.into(), vec![]))
}

pub fn core_binary_operator(
    db: &dyn SemanticGroup,
    binary_op: &BinaryOperator,
    type1: TypeId,
    type2: TypeId,
) -> Maybe<Result<FunctionId, SemanticDiagnosticKind>> {
    // TODO(lior): Replace current hard-coded implementation with an implementation that is based on
    //   traits.
    type1.check_not_missing(db)?;
    type2.check_not_missing(db)?;

    let felt_ty = core_felt_ty(db);
    let u128_ty = get_core_ty_by_name(db, "u128".into(), vec![]);
    let u256_ty = get_core_ty_by_name(db, "u256".into(), vec![]);
    let bool_ty = core_bool_ty(db);
    let unsupported_operator = |op: &str| {
        Ok(Err(SemanticDiagnosticKind::UnsupportedBinaryOperator { op: op.into(), type1, type2 }))
    };
    let function_name = match binary_op {
        BinaryOperator::Plus(_) if [type1, type2] == [felt_ty, felt_ty] => "felt_add",
        BinaryOperator::Plus(_) if [type1, type2] == [u128_ty, u128_ty] => "u128_add",
        BinaryOperator::Plus(_) if [type1, type2] == [u256_ty, u256_ty] => "u256_add",
        BinaryOperator::Plus(_) => return unsupported_operator("+"),
        BinaryOperator::Minus(_) if [type1, type2] == [felt_ty, felt_ty] => "felt_sub",
        BinaryOperator::Minus(_) if [type1, type2] == [u128_ty, u128_ty] => "u128_sub",
        BinaryOperator::Minus(_) if [type1, type2] == [u256_ty, u256_ty] => "u256_sub",
        BinaryOperator::Minus(_) => return unsupported_operator("-"),
        BinaryOperator::Mul(_) if [type1, type2] == [felt_ty, felt_ty] => "felt_mul",
        BinaryOperator::Mul(_) if [type1, type2] == [u128_ty, u128_ty] => "u128_mul",
        BinaryOperator::Mul(_) if [type1, type2] == [u256_ty, u256_ty] => "u256_mul",
        BinaryOperator::Mul(_) => return unsupported_operator("*"),
        BinaryOperator::Div(_) if [type1, type2] == [felt_ty, felt_ty] => "felt_div",
        BinaryOperator::Div(_) if [type1, type2] == [u128_ty, u128_ty] => "u128_div",
        BinaryOperator::Div(_) => return unsupported_operator("/"),
        BinaryOperator::Mod(_) if [type1, type2] == [u128_ty, u128_ty] => "u128_mod",
        BinaryOperator::Mod(_) => return unsupported_operator("%"),
        BinaryOperator::EqEq(_) if [type1, type2] == [felt_ty, felt_ty] => "felt_eq",
        BinaryOperator::EqEq(_) if [type1, type2] == [bool_ty, bool_ty] => "bool_eq",
        BinaryOperator::EqEq(_) if [type1, type2] == [u128_ty, u128_ty] => "u128_eq",
        BinaryOperator::EqEq(_) if [type1, type2] == [u256_ty, u256_ty] => "u256_eq",
        BinaryOperator::EqEq(_) => return unsupported_operator("=="),
        BinaryOperator::Neq(_) if [type1, type2] == [felt_ty, felt_ty] => "felt_ne",
        BinaryOperator::Neq(_) if [type1, type2] == [bool_ty, bool_ty] => "bool_ne",
        BinaryOperator::Neq(_) if [type1, type2] == [u128_ty, u128_ty] => "u128_ne",
        BinaryOperator::Neq(_) if [type1, type2] == [u256_ty, u256_ty] => "u256_ne",
        BinaryOperator::Neq(_) => return unsupported_operator("!="),
        BinaryOperator::And(_) if [type1, type2] == [bool_ty, bool_ty] => "bool_and",
        BinaryOperator::And(_) => return unsupported_operator("&"),
        BinaryOperator::Or(_) if [type1, type2] == [bool_ty, bool_ty] => "bool_or",
        BinaryOperator::Or(_) => return unsupported_operator("|"),
        BinaryOperator::LE(_) if [type1, type2] == [felt_ty, felt_ty] => "felt_le",
        BinaryOperator::LE(_) if [type1, type2] == [u128_ty, u128_ty] => "u128_le",
        BinaryOperator::LE(_) => return unsupported_operator("<="),
        BinaryOperator::GE(_) if [type1, type2] == [felt_ty, felt_ty] => "felt_ge",
        BinaryOperator::GE(_) if [type1, type2] == [u128_ty, u128_ty] => "u128_ge",
        BinaryOperator::GE(_) => return unsupported_operator(">="),
        BinaryOperator::LT(_) if [type1, type2] == [felt_ty, felt_ty] => "felt_lt",
        BinaryOperator::LT(_) if [type1, type2] == [u128_ty, u128_ty] => "u128_lt",
        BinaryOperator::LT(_) => return unsupported_operator("<"),
        BinaryOperator::GT(_) if [type1, type2] == [felt_ty, felt_ty] => "felt_gt",
        BinaryOperator::GT(_) if [type1, type2] == [u128_ty, u128_ty] => "u128_gt",
        BinaryOperator::GT(_) => return unsupported_operator(">"),
        _ => return Ok(Err(SemanticDiagnosticKind::UnknownBinaryOperator)),
    };
    Ok(Ok(get_core_function_id(db, function_name.into(), vec![])))
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
pub fn get_core_function_id(
    db: &dyn SemanticGroup,
    name: SmolStr,
    generic_args: Vec<GenericArgumentId>,
) -> FunctionId {
    let generic_function = get_core_generic_function_id(db, name);

    db.intern_function(FunctionLongId {
        function: ConcreteFunction { generic_function, generic_args },
    })
}

/// Given a core library function name, returns [GenericFunctionId].
pub fn get_core_generic_function_id(db: &dyn SemanticGroup, name: SmolStr) -> GenericFunctionId {
    let core_module = db.core_module();
    let module_item_id = db
        .module_item_by_name(core_module, name.clone())
        .expect("Failed to load core lib.")
        .unwrap_or_else(|| panic!("Function '{name}' was not found in core lib."));
    match module_item_id {
        ModuleItemId::Use(use_id) => {
            db.use_resolved_item(use_id).to_option().and_then(|resolved_generic_item| {
                try_extract_matches!(resolved_generic_item, ResolvedGenericItem::GenericFunction)
            })
        }
        _ => GenericFunctionId::option_from(module_item_id),
    }
    .unwrap_or_else(|| panic!("{name} is not a function."))
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
    let use_id = extract_matches!(
        db.module_item_by_name(core_module, name).unwrap().unwrap(),
        ModuleItemId::Use
    );
    let trait_id =
        extract_matches!(db.use_resolved_item(use_id).unwrap(), ResolvedGenericItem::Trait);
    trait_id
}

pub fn get_panic_ty(db: &dyn SemanticGroup, inner_ty: TypeId) -> TypeId {
    get_core_ty_by_name(db.upcast(), "PanicResult".into(), vec![GenericArgumentId::Type(inner_ty)])
}

/// Returns the name of the libfunc that creates a constant of type `ty`.
pub fn try_get_const_libfunc_name_by_type(
    db: &dyn SemanticGroup,
    ty: TypeId,
) -> Result<String, SemanticDiagnosticKind> {
    let felt_ty = core_felt_ty(db);
    let u128_ty = get_core_ty_by_name(db, "u128".into(), vec![]);
    if ty == felt_ty {
        Ok("felt_const".into())
    } else if ty == u128_ty {
        Ok("u128_const".into())
    } else {
        Err(SemanticDiagnosticKind::NoLiteralFunctionFound)
    }
}

pub fn get_const_libfunc_name_by_type(db: &dyn SemanticGroup, ty: TypeId) -> String {
    try_get_const_libfunc_name_by_type(db, ty).unwrap()
}
