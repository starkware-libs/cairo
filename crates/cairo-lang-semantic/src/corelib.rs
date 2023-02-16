use cairo_lang_defs::ids::{EnumId, GenericTypeId, ImplDefId, ModuleId, ModuleItemId, TraitId};
use cairo_lang_diagnostics::{Maybe, ToOption};
use cairo_lang_filesystem::ids::{CrateId, CrateLongId};
use cairo_lang_syntax::node::ast::{self, BinaryOperator, UnaryOperator};
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_utils::{extract_matches, try_extract_matches, OptionFrom};
use num_bigint::BigInt;
use num_traits::{Num, Signed};
use smol_str::SmolStr;

use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnosticKind;
use crate::expr::compute::ComputationContext;
use crate::expr::inference::Inference;
use crate::items::enm::SemanticEnumEx;
use crate::items::functions::{ConcreteImplGenericFunctionId, GenericFunctionId};
use crate::items::trt::{ConcreteTraitGenericFunctionLongId, ConcreteTraitId};
use crate::items::us::SemanticUseEx;
use crate::resolve_path::ResolvedGenericItem;
use crate::types::ConcreteEnumLongId;
use crate::{
    semantic, ConcreteEnumId, ConcreteFunction, ConcreteImplLongId, ConcreteVariant, Expr, ExprId,
    ExprTuple, FunctionId, FunctionLongId, GenericArgumentId, TypeId, TypeLongId,
};

pub fn core_module(db: &dyn SemanticGroup) -> ModuleId {
    let core_crate = core_crate(db);
    ModuleId::CrateRoot(core_crate)
}

pub fn core_crate(db: &dyn SemanticGroup) -> CrateId {
    db.intern_crate(CrateLongId("core".into()))
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
        ModuleItemId::TypeAlias(type_alias_id) => {
            let ty =
                db.type_alias_resolved_type(type_alias_id).expect("Could not find type alias.");
            assert!(
                db.type_alias_generic_params(type_alias_id).unwrap().is_empty(),
                "Cannot get type aliases with params from corelib."
            );
            return Ok(ty);
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

/// Generates a ConcreteVariant instance for `IsZeroResult::<felt>::Zero`.
pub fn jump_nz_zero_variant(db: &dyn SemanticGroup) -> ConcreteVariant {
    get_enum_concrete_variant(
        db,
        "IsZeroResult",
        vec![GenericArgumentId::Type(core_felt_ty(db))],
        "Zero",
    )
}

/// Generates a ConcreteVariant instance for `IsZeroResult::<felt>::NonZero`.
pub fn jump_nz_nonzero_variant(db: &dyn SemanticGroup) -> ConcreteVariant {
    get_enum_concrete_variant(
        db,
        "IsZeroResult",
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
        | TypeLongId::Snapshot(_)
        | TypeLongId::Var(_)
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
    inference: &mut Inference<'_>,
    unary_op: &UnaryOperator,
    stable_ptr: SyntaxStablePtrId,
) -> Maybe<Result<FunctionId, SemanticDiagnosticKind>> {
    let (trait_name, function_name) = match unary_op {
        UnaryOperator::Minus(_) => ("Neg", "neg"),
        UnaryOperator::Not(_) => ("Not", "not"),
        UnaryOperator::At(_) => unreachable!("@ is not an unary operator."),
    };
    Ok(Ok(get_core_trait_function_infer(
        db,
        inference,
        trait_name.into(),
        function_name.into(),
        stable_ptr,
    )))
}

pub fn core_binary_operator(
    db: &dyn SemanticGroup,
    inference: &mut Inference<'_>,
    binary_op: &BinaryOperator,
    stable_ptr: SyntaxStablePtrId,
) -> Maybe<Result<FunctionId, SemanticDiagnosticKind>> {
    let (trait_name, function_name) = match binary_op {
        BinaryOperator::Plus(_) => ("Add", "add"),
        BinaryOperator::PlusEq(_) => ("AddEq", "add_eq"),
        BinaryOperator::Minus(_) => ("Sub", "sub"),
        BinaryOperator::MinusEq(_) => ("SubEq", "sub_eq"),
        BinaryOperator::Mul(_) => ("Mul", "mul"),
        BinaryOperator::MulEq(_) => ("MulEq", "mul_eq"),
        BinaryOperator::Div(_) => ("Div", "div"),
        BinaryOperator::DivEq(_) => ("DivEq", "div_eq"),
        BinaryOperator::Mod(_) => ("Rem", "rem"),
        BinaryOperator::ModEq(_) => ("RemEq", "rem_eq"),
        BinaryOperator::EqEq(_) => ("PartialEq", "eq"),
        BinaryOperator::Neq(_) => ("PartialEq", "ne"),
        BinaryOperator::LE(_) => ("PartialOrd", "le"),
        BinaryOperator::GE(_) => ("PartialOrd", "ge"),
        BinaryOperator::LT(_) => ("PartialOrd", "lt"),
        BinaryOperator::GT(_) => ("PartialOrd", "gt"),
        BinaryOperator::And(_) => ("BitAnd", "bitand"),
        BinaryOperator::Or(_) => ("BitOr", "bitor"),
        BinaryOperator::Xor(_) => ("BitXor", "bitxor"),
        _ => return Ok(Err(SemanticDiagnosticKind::UnknownBinaryOperator)),
    };
    Ok(Ok(get_core_trait_function_infer(
        db,
        inference,
        trait_name.into(),
        function_name.into(),
        stable_ptr,
    )))
}

pub fn felt_eq(db: &dyn SemanticGroup) -> FunctionId {
    get_core_function_impl_method(db, "FeltPartialEq".into(), "eq".into())
}

pub fn felt_sub(db: &dyn SemanticGroup) -> FunctionId {
    get_core_function_impl_method(db, "FeltSub".into(), "sub".into())
}

/// Given a core library impl name and a method name, returns [FunctionId].
fn get_core_function_impl_method(
    db: &dyn SemanticGroup,
    impl_name: SmolStr,
    method_name: SmolStr,
) -> FunctionId {
    let core_module = db.core_module();
    let module_item_id = db
        .module_item_by_name(core_module, impl_name.clone())
        .expect("Failed to load core lib.")
        .unwrap_or_else(|| panic!("Impl '{impl_name}' was not found in core lib."));
    let impl_def_id = match module_item_id {
        ModuleItemId::Use(use_id) => {
            db.use_resolved_item(use_id).to_option().and_then(|resolved_generic_item| {
                try_extract_matches!(resolved_generic_item, ResolvedGenericItem::Impl)
            })
        }
        _ => ImplDefId::option_from(module_item_id),
    }
    .unwrap_or_else(|| panic!("{impl_name} is not an impl."));
    let function = db
        .impl_functions(impl_def_id)
        .ok()
        .and_then(|functions| functions.get(&method_name).cloned())
        .unwrap_or_else(|| panic!("no {method_name} in {impl_name}."));
    let concrete_impl =
        db.intern_concrete_impl(ConcreteImplLongId { impl_def_id, generic_args: vec![] });
    db.intern_function(FunctionLongId {
        function: ConcreteFunction {
            generic_function: GenericFunctionId::Impl(ConcreteImplGenericFunctionId {
                concrete_impl_id: concrete_impl,
                function,
            }),
            generic_args: vec![],
        },
    })
}

pub fn core_felt_is_zero(db: &dyn SemanticGroup) -> FunctionId {
    get_core_function_id(db, "felt_is_zero".into(), vec![])
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

/// Retrieves a trait function from the core library with type variables as generic arguments, to
/// be inferred later.
fn get_core_trait_function_infer(
    db: &dyn SemanticGroup,
    inference: &mut Inference<'_>,
    trait_name: SmolStr,
    function_name: SmolStr,
    stable_ptr: SyntaxStablePtrId,
) -> FunctionId {
    let trait_id = get_core_trait(db, trait_name);
    let generic_params = db.trait_generic_params(trait_id);
    let generic_args = generic_params
        .iter()
        .map(|_| GenericArgumentId::Type(inference.new_var(stable_ptr)))
        .collect();
    let concrete_trait_id =
        db.intern_concrete_trait(semantic::ConcreteTraitLongId { trait_id, generic_args });
    let trait_function = db.trait_function_by_name(trait_id, function_name).unwrap().unwrap();
    let concrete_trait_function = db.intern_concrete_trait_function(
        ConcreteTraitGenericFunctionLongId::new(db, concrete_trait_id, trait_function),
    );
    db.intern_function(FunctionLongId {
        function: ConcreteFunction {
            generic_function: GenericFunctionId::Trait(concrete_trait_function),
            generic_args: vec![],
        },
    })
}

pub fn get_panic_ty(db: &dyn SemanticGroup, inner_ty: TypeId) -> TypeId {
    get_core_ty_by_name(db.upcast(), "PanicResult".into(), vec![GenericArgumentId::Type(inner_ty)])
}

/// Returns the name of the libfunc that creates a constant of type `ty`;
pub fn get_const_libfunc_name_by_type(db: &dyn SemanticGroup, ty: TypeId) -> String {
    if ty == core_felt_ty(db) {
        "felt_const".into()
    } else if ty == get_core_ty_by_name(db, "u8".into(), vec![]) {
        "u8_const".into()
    } else if ty == get_core_ty_by_name(db, "u16".into(), vec![]) {
        "u16_const".into()
    } else if ty == get_core_ty_by_name(db, "u32".into(), vec![]) {
        "u32_const".into()
    } else if ty == get_core_ty_by_name(db, "u64".into(), vec![]) {
        "u64_const".into()
    } else if ty == get_core_ty_by_name(db, "u128".into(), vec![]) {
        "u128_const".into()
    } else {
        panic!("No const libfunc for type {}.", ty.format(db))
    }
}

/// Validates that a given type is valid for a literal and that the value fits the range of the
/// specific type.
pub fn validate_literal(
    db: &dyn SemanticGroup,
    ty: TypeId,
    value: BigInt,
) -> Result<(), SemanticDiagnosticKind> {
    let is_out_of_range = if ty == core_felt_ty(db) {
        value.is_negative()
            || value
                > BigInt::from_str_radix(
                    "800000000000011000000000000000000000000000000000000000000000000",
                    16,
                )
                .unwrap()
    } else if ty == get_core_ty_by_name(db, "u8".into(), vec![]) {
        value.is_negative() || value.bits() > 8
    } else if ty == get_core_ty_by_name(db, "u16".into(), vec![]) {
        value.is_negative() || value.bits() > 16
    } else if ty == get_core_ty_by_name(db, "u32".into(), vec![]) {
        value.is_negative() || value.bits() > 32
    } else if ty == get_core_ty_by_name(db, "u64".into(), vec![]) {
        value.is_negative() || value.bits() > 64
    } else if ty == get_core_ty_by_name(db, "u128".into(), vec![]) {
        value.is_negative() || value.bits() > 128
    } else {
        return Err(SemanticDiagnosticKind::NoLiteralFunctionFound);
    };
    if is_out_of_range { Err(SemanticDiagnosticKind::LiteralOutOfRange { ty }) } else { Ok(()) }
}
