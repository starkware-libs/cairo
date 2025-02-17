use std::sync::Arc;

use cairo_lang_defs::ids::{
    EnumId, GenericTypeId, ImplDefId, ModuleId, ModuleItemId, NamedLanguageElementId,
    TraitFunctionId, TraitId,
};
use cairo_lang_diagnostics::{Maybe, ToOption};
use cairo_lang_filesystem::ids::CrateId;
use cairo_lang_syntax::node::Terminal;
use cairo_lang_syntax::node::ast::{self, BinaryOperator, UnaryOperator};
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_utils::{
    Intern, LookupIntern, OptionFrom, extract_matches, require, try_extract_matches,
};
use num_bigint::BigInt;
use num_traits::{Num, Signed, ToPrimitive, Zero};
use smol_str::SmolStr;

use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnosticKind;
use crate::expr::compute::ComputationContext;
use crate::expr::inference::Inference;
use crate::helper::ModuleHelper;
use crate::items::constant::ConstValue;
use crate::items::enm::SemanticEnumEx;
use crate::items::functions::{GenericFunctionId, ImplGenericFunctionId};
use crate::items::imp::ImplLongId;
use crate::items::trt::{
    ConcreteTraitGenericFunctionId, ConcreteTraitGenericFunctionLongId, ConcreteTraitId,
};
use crate::items::us::SemanticUseEx;
use crate::resolve::ResolvedGenericItem;
use crate::types::{ConcreteEnumLongId, ConcreteExternTypeLongId};
use crate::{
    ConcreteEnumId, ConcreteFunction, ConcreteImplLongId, ConcreteTypeId, ConcreteVariant, Expr,
    ExprId, ExprTuple, FunctionId, FunctionLongId, GenericArgumentId, TypeId, TypeLongId, semantic,
};

/// Query implementation of [SemanticGroup::core_module].
pub fn core_module(db: &dyn SemanticGroup) -> ModuleId {
    let core_crate = db.core_crate();
    ModuleId::CrateRoot(core_crate)
}

/// Returns the submodule of `base_module`, named `submodule_name`, if exists.
pub fn get_submodule(
    db: &dyn SemanticGroup,
    base_module: ModuleId,
    submodule_name: &str,
) -> Option<ModuleId> {
    let submodules = db.module_submodules(base_module).ok()?;
    let syntax_db = db.upcast();
    for (submodule_id, submodule) in submodules.iter() {
        if submodule.name(syntax_db).text(syntax_db) == submodule_name {
            return Some(ModuleId::Submodule(*submodule_id));
        }
    }
    None
}

/// Returns a submodule of the corelib named `submodule_name`.
/// If no such submodule exists, panics.
pub fn core_submodule(db: &dyn SemanticGroup, submodule_name: &str) -> ModuleId {
    get_submodule(db, core_module(db), submodule_name)
        .unwrap_or_else(|| panic!("`{submodule_name}` is not a core submodule."))
}

/// Query implementation of [SemanticGroup::core_crate].
pub fn core_crate(db: &dyn SemanticGroup) -> CrateId {
    CrateId::core(db)
}

/// Returns the concrete type of a bounded int type with a given min and max.
pub fn bounded_int_ty(db: &dyn SemanticGroup, min: BigInt, max: BigInt) -> TypeId {
    let internal = core_submodule(db, "internal");
    let bounded_int = get_submodule(db, internal, "bounded_int")
        .expect("Could not find bounded_int submodule in corelib.");
    let felt252_ty = db.core_info().felt252;
    let lower_id = ConstValue::Int(min, felt252_ty).intern(db);
    let upper_id = ConstValue::Int(max, felt252_ty).intern(db);
    try_get_ty_by_name(
        db,
        bounded_int,
        "BoundedInt".into(),
        vec![GenericArgumentId::Constant(lower_id), GenericArgumentId::Constant(upper_id)],
    )
    .expect("could not find")
}

pub fn core_nonzero_ty(db: &dyn SemanticGroup, inner_type: TypeId) -> TypeId {
    get_ty_by_name(
        db,
        core_submodule(db, "zeroable"),
        "NonZero".into(),
        vec![GenericArgumentId::Type(inner_type)],
    )
}

pub fn core_result_ty(db: &dyn SemanticGroup, ok_type: TypeId, err_type: TypeId) -> TypeId {
    get_ty_by_name(
        db,
        core_submodule(db, "result"),
        "Result".into(),
        vec![GenericArgumentId::Type(ok_type), GenericArgumentId::Type(err_type)],
    )
}

pub fn core_option_ty(db: &dyn SemanticGroup, some_type: TypeId) -> TypeId {
    get_ty_by_name(
        db,
        core_submodule(db, "option"),
        "Option".into(),
        vec![GenericArgumentId::Type(some_type)],
    )
}

pub fn core_box_ty(db: &dyn SemanticGroup, inner_type: TypeId) -> TypeId {
    get_ty_by_name(
        db,
        core_submodule(db, "box"),
        "Box".into(),
        vec![GenericArgumentId::Type(inner_type)],
    )
}

pub fn core_array_felt252_ty(db: &dyn SemanticGroup) -> TypeId {
    get_core_ty_by_name(db, "Array".into(), vec![GenericArgumentId::Type(db.core_info().felt252)])
}

pub fn try_get_core_ty_by_name(
    db: &dyn SemanticGroup,
    name: SmolStr,
    generic_args: Vec<GenericArgumentId>,
) -> Result<TypeId, SemanticDiagnosticKind> {
    try_get_ty_by_name(db, db.core_module(), name, generic_args)
}

pub fn try_get_ty_by_name(
    db: &dyn SemanticGroup,
    module: ModuleId,
    name: SmolStr,
    generic_args: Vec<GenericArgumentId>,
) -> Result<TypeId, SemanticDiagnosticKind> {
    // This should not fail if the corelib is present.
    let module_item_id = db
        .module_item_by_name(module, name.clone())
        .map_err(|_| SemanticDiagnosticKind::UnknownType)?
        .ok_or(SemanticDiagnosticKind::UnknownType)?;
    let generic_type = match module_item_id {
        ModuleItemId::Use(use_id) => {
            db.use_resolved_item(use_id).to_option().and_then(|resolved_generic_item| {
                try_extract_matches!(resolved_generic_item, ResolvedGenericItem::GenericType)
            })
        }
        ModuleItemId::TypeAlias(module_type_alias_id) => {
            let ty = db
                .module_type_alias_resolved_type(module_type_alias_id)
                .expect("Could not find type alias.");
            assert!(
                db.module_type_alias_generic_params(module_type_alias_id).unwrap().is_empty(),
                "Cannot get type aliases with params from corelib."
            );
            return Ok(ty);
        }
        _ => GenericTypeId::option_from(module_item_id),
    }
    .unwrap_or_else(|| panic!("{name} is not a type."));

    Ok(semantic::TypeLongId::Concrete(semantic::ConcreteTypeId::new(
        db,
        generic_type,
        generic_args,
    ))
    .intern(db))
}

pub fn get_core_ty_by_name(
    db: &dyn SemanticGroup,
    name: SmolStr,
    generic_args: Vec<GenericArgumentId>,
) -> TypeId {
    try_get_core_ty_by_name(db, name, generic_args).unwrap()
}

pub fn get_ty_by_name(
    db: &dyn SemanticGroup,
    module: ModuleId,
    name: SmolStr,
    generic_args: Vec<GenericArgumentId>,
) -> TypeId {
    try_get_ty_by_name(db, module, name, generic_args).unwrap()
}

pub fn core_bool_ty(db: &dyn SemanticGroup) -> TypeId {
    let core_module = db.core_module();
    // This should not fail if the corelib is present.
    let generic_type = db
        .module_item_by_name(core_module, "bool".into())
        .expect("Failed to load core lib.")
        .and_then(GenericTypeId::option_from)
        .expect("Type bool was not found in core lib.");
    semantic::TypeLongId::Concrete(semantic::ConcreteTypeId::new(db, generic_type, vec![]))
        .intern(db)
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
    ConcreteEnumLongId { enum_id, generic_args: vec![] }.intern(db)
}

/// Generates a ConcreteVariant instance for `false`.
pub fn false_variant(db: &dyn SemanticGroup) -> ConcreteVariant {
    get_core_enum_concrete_variant(db, "bool", vec![], "False")
}

/// Generates a ConcreteVariant instance for `true`.
pub fn true_variant(db: &dyn SemanticGroup) -> ConcreteVariant {
    get_core_enum_concrete_variant(db, "bool", vec![], "True")
}

/// Generates a ConcreteVariant instance for `IsZeroResult::<felt252>::Zero`.
pub fn jump_nz_zero_variant(db: &dyn SemanticGroup, ty: TypeId) -> ConcreteVariant {
    get_enum_concrete_variant(
        db,
        core_submodule(db, "zeroable"),
        "IsZeroResult",
        vec![GenericArgumentId::Type(ty)],
        "Zero",
    )
}

/// Generates a ConcreteVariant instance for `IsZeroResult::<felt252>::NonZero`.
pub fn jump_nz_nonzero_variant(db: &dyn SemanticGroup, ty: TypeId) -> ConcreteVariant {
    get_enum_concrete_variant(
        db,
        core_submodule(db, "zeroable"),
        "IsZeroResult",
        vec![GenericArgumentId::Type(ty)],
        "NonZero",
    )
}

/// Generates a ConcreteVariant instance for `Option::Some`.
pub fn option_some_variant(db: &dyn SemanticGroup, ty: TypeId) -> ConcreteVariant {
    get_enum_concrete_variant(
        db,
        core_submodule(db, "option"),
        "Option",
        vec![GenericArgumentId::Type(ty)],
        "Some",
    )
}

/// Generates a ConcreteVariant instance for `Option::None`.
pub fn option_none_variant(db: &dyn SemanticGroup, ty: TypeId) -> ConcreteVariant {
    get_enum_concrete_variant(
        db,
        core_submodule(db, "option"),
        "Option",
        vec![GenericArgumentId::Type(ty)],
        "None",
    )
}

/// Generates a ConcreteVariant instance for `Result::Ok`.
pub fn result_ok_variant(db: &dyn SemanticGroup, ok_ty: TypeId, err_ty: TypeId) -> ConcreteVariant {
    get_enum_concrete_variant(
        db,
        core_submodule(db, "result"),
        "Result",
        vec![GenericArgumentId::Type(ok_ty), GenericArgumentId::Type(err_ty)],
        "Ok",
    )
}

/// Generates a ConcreteVariant instance for `Result::Err`.
pub fn result_err_variant(
    db: &dyn SemanticGroup,
    ok_ty: TypeId,
    err_ty: TypeId,
) -> ConcreteVariant {
    get_enum_concrete_variant(
        db,
        core_submodule(db, "result"),
        "Result",
        vec![GenericArgumentId::Type(ok_ty), GenericArgumentId::Type(err_ty)],
        "Err",
    )
}

/// Generates a ConcreteVariant instance for `SignedIntegerResult::InRange`.
pub fn signed_int_result_in_range_variant(db: &dyn SemanticGroup, ty: TypeId) -> ConcreteVariant {
    get_enum_concrete_variant(
        db,
        core_submodule(db, "integer"),
        "SignedIntegerResult",
        vec![GenericArgumentId::Type(ty)],
        "InRange",
    )
}
/// Generates a ConcreteVariant instance for `SignedIntegerResult::Underflow`.
pub fn signed_int_result_underflow_variant(db: &dyn SemanticGroup, ty: TypeId) -> ConcreteVariant {
    get_enum_concrete_variant(
        db,
        core_submodule(db, "integer"),
        "SignedIntegerResult",
        vec![GenericArgumentId::Type(ty)],
        "Underflow",
    )
}
/// Generates a ConcreteVariant instance for `SignedIntegerResult::Overflow`.
pub fn signed_int_result_overflow_variant(db: &dyn SemanticGroup, ty: TypeId) -> ConcreteVariant {
    get_enum_concrete_variant(
        db,
        core_submodule(db, "integer"),
        "SignedIntegerResult",
        vec![GenericArgumentId::Type(ty)],
        "Overflow",
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
    let concrete_variant = get_core_enum_concrete_variant(ctx.db, enum_name, vec![], variant_name);
    semantic::Expr::EnumVariantCtor(semantic::ExprEnumVariantCtor {
        variant: concrete_variant,
        value_expr: unit_expr(ctx, stable_ptr),
        ty: core_bool_ty(ctx.db),
        stable_ptr,
    })
}

/// Gets a [ConcreteVariant] instance for an enum variant, by module and name.
/// Assumes the variant exists.
pub fn get_enum_concrete_variant(
    db: &dyn SemanticGroup,
    module_id: ModuleId,
    enum_name: &str,
    generic_args: Vec<GenericArgumentId>,
    variant_name: &str,
) -> ConcreteVariant {
    let ty = get_ty_by_name(db, module_id, enum_name.into(), generic_args);
    let concrete_ty = extract_matches!(ty.lookup_intern(db), TypeLongId::Concrete);
    let concrete_enum_id = extract_matches!(concrete_ty, ConcreteTypeId::Enum);
    let enum_id = concrete_enum_id.enum_id(db);
    let variant_id = db.enum_variants(enum_id).unwrap()[variant_name];
    let variant = db.variant_semantic(enum_id, variant_id).unwrap();
    db.concrete_enum_variant(concrete_enum_id, &variant).unwrap()
}

/// Gets a [ConcreteVariant] instance for an enum variant from the core module, by name.
/// Assumes the variant exists.
pub fn get_core_enum_concrete_variant(
    db: &dyn SemanticGroup,
    enum_name: &str,
    generic_args: Vec<GenericArgumentId>,
    variant_name: &str,
) -> ConcreteVariant {
    get_enum_concrete_variant(db, core_module(db), enum_name, generic_args, variant_name)
}

/// Gets the unit type ().
pub fn unit_ty(db: &dyn SemanticGroup) -> TypeId {
    semantic::TypeLongId::Tuple(vec![]).intern(db)
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
    semantic::TypeLongId::Concrete(semantic::ConcreteTypeId::new(db, generic_type, vec![]))
        .intern(db)
}

pub enum ErrorPropagationType {
    Option { some_variant: ConcreteVariant, none_variant: ConcreteVariant },
    Result { ok_variant: ConcreteVariant, err_variant: ConcreteVariant },
}
impl ErrorPropagationType {
    pub fn ok_variant(&self) -> &ConcreteVariant {
        match self {
            ErrorPropagationType::Option { some_variant, .. } => some_variant,
            ErrorPropagationType::Result { ok_variant, .. } => ok_variant,
        }
    }
    pub fn err_variant(&self) -> &ConcreteVariant {
        match self {
            ErrorPropagationType::Option { none_variant, .. } => none_variant,
            ErrorPropagationType::Result { err_variant, .. } => err_variant,
        }
    }
}

/// Attempts to unwrap error propagation types (Option, Result).
/// Returns None if not one of these types.
pub fn unwrap_error_propagation_type(
    db: &dyn SemanticGroup,
    ty: TypeId,
) -> Option<ErrorPropagationType> {
    match ty.lookup_intern(db) {
        // Only enums may be `Result` and `Option` types.
        TypeLongId::Concrete(semantic::ConcreteTypeId::Enum(enm)) => {
            if let [ok_variant, err_variant] =
                db.concrete_enum_variants(enm).to_option()?.as_slice()
            {
                let name = enm.enum_id(db.upcast()).name(db.upcast());
                if name == "Option" {
                    return Some(ErrorPropagationType::Option {
                        some_variant: ok_variant.clone(),
                        none_variant: err_variant.clone(),
                    });
                } else if name == "Result" {
                    return Some(ErrorPropagationType::Result {
                        ok_variant: ok_variant.clone(),
                        err_variant: err_variant.clone(),
                    });
                }
            }
            None
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
        | TypeLongId::Coupon(_)
        | TypeLongId::ImplType(_)
        | TypeLongId::Missing(_)
        | TypeLongId::FixedSizeArray { .. }
        | TypeLongId::Closure(_) => None,
    }
}

/// builds a semantic unit expression. This is not necessarily located in the AST, so it is received
/// as a param.
pub fn unit_expr(ctx: &mut ComputationContext<'_>, stable_ptr: ast::ExprPtr) -> ExprId {
    ctx.arenas.exprs.alloc(Expr::Tuple(ExprTuple {
        items: Vec::new(),
        ty: TypeLongId::Tuple(Vec::new()).intern(ctx.db),
        stable_ptr,
    }))
}

pub fn core_unary_operator(
    db: &dyn SemanticGroup,
    inference: &mut Inference<'_>,
    unary_op: &UnaryOperator,
    stable_ptr: SyntaxStablePtrId,
) -> Maybe<Result<ConcreteTraitGenericFunctionId, SemanticDiagnosticKind>> {
    let info = db.core_info();
    let (trait_id, trait_fn) = match unary_op {
        UnaryOperator::Minus(_) => (info.neg_trt, info.neg_fn),
        UnaryOperator::Not(_) => (info.not_trt, info.not_fn),
        UnaryOperator::BitNot(_) => (info.bitnot_trt, info.bitnot_fn),
        UnaryOperator::At(_) => unreachable!("@ is not an unary operator."),
        UnaryOperator::Desnap(_) => unreachable!("* is not an unary operator."),
    };
    Ok(Ok(get_core_trait_function_infer(db, inference, trait_id, trait_fn, stable_ptr)))
}

pub fn core_binary_operator(
    db: &dyn SemanticGroup,
    inference: &mut Inference<'_>,
    binary_op: &BinaryOperator,
    stable_ptr: SyntaxStablePtrId,
) -> Maybe<Result<(ConcreteTraitGenericFunctionId, bool), SemanticDiagnosticKind>> {
    let info = db.core_info();
    let (trait_id, trait_fn, snapshot) = match binary_op {
        BinaryOperator::Plus(_) => (info.add_trt, info.add_fn, false),
        BinaryOperator::PlusEq(_) => (info.add_assign_trt, info.add_assign_fn, false),
        BinaryOperator::Minus(_) => (info.sub_trt, info.sub_fn, false),
        BinaryOperator::MinusEq(_) => (info.sub_assign_trt, info.sub_assign_fn, false),
        BinaryOperator::Mul(_) => (info.mul_trt, info.mul_fn, false),
        BinaryOperator::MulEq(_) => (info.mul_assign_trt, info.mul_assign_fn, false),
        BinaryOperator::Div(_) => (info.div_trt, info.div_fn, false),
        BinaryOperator::DivEq(_) => (info.div_assign_trt, info.div_assign_fn, false),
        BinaryOperator::Mod(_) => (info.rem_trt, info.rem_fn, false),
        BinaryOperator::ModEq(_) => (info.rem_assign_trt, info.rem_assign_fn, false),
        BinaryOperator::EqEq(_) => (info.partialeq_trt, info.eq_fn, true),
        BinaryOperator::Neq(_) => (info.partialeq_trt, info.ne_fn, true),
        BinaryOperator::LE(_) => (info.partialord_trt, info.le_fn, false),
        BinaryOperator::GE(_) => (info.partialord_trt, info.ge_fn, false),
        BinaryOperator::LT(_) => (info.partialord_trt, info.lt_fn, false),
        BinaryOperator::GT(_) => (info.partialord_trt, info.gt_fn, false),
        BinaryOperator::And(_) => (info.bitand_trt, info.bitand_fn, false),
        BinaryOperator::Or(_) => (info.bitor_trt, info.bitor_fn, false),
        BinaryOperator::Xor(_) => (info.bitxor_trt, info.bitxor_fn, false),
        BinaryOperator::DotDot(_) => (info.range_op_trt, info.range_fn, false),
        BinaryOperator::DotDotEq(_) => {
            (info.range_inclusive_op_trt, info.range_inclusive_fn, false)
        }
        _ => return Ok(Err(SemanticDiagnosticKind::UnknownBinaryOperator)),
    };
    Ok(Ok((get_core_trait_function_infer(db, inference, trait_id, trait_fn, stable_ptr), snapshot)))
}

pub fn felt252_sub(db: &dyn SemanticGroup) -> FunctionId {
    get_core_function_impl_method(db, "Felt252Sub".into(), "sub".into())
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
    let impl_id =
        ImplLongId::Concrete(ConcreteImplLongId { impl_def_id, generic_args: vec![] }.intern(db))
            .intern(db);
    let concrete_trait_id = db.impl_concrete_trait(impl_id).unwrap();
    let function = db
        .trait_functions(concrete_trait_id.trait_id(db))
        .ok()
        .and_then(|functions| functions.get(&method_name).cloned())
        .unwrap_or_else(|| {
            panic!("no {method_name} in {}.", concrete_trait_id.trait_id(db).name(db.upcast()))
        });
    FunctionLongId {
        function: ConcreteFunction {
            generic_function: GenericFunctionId::Impl(ImplGenericFunctionId { impl_id, function }),
            generic_args: vec![],
        },
    }
    .intern(db)
}

pub fn core_felt252_is_zero(db: &dyn SemanticGroup) -> FunctionId {
    get_core_function_id(db, "felt252_is_zero".into(), vec![])
}

/// The gas withdrawal functions from the `gas` submodule.
pub fn core_withdraw_gas_fns(db: &dyn SemanticGroup) -> [FunctionId; 2] {
    let gas = core_submodule(db, "gas");
    [
        get_function_id(db, gas, "withdraw_gas".into(), vec![]),
        get_function_id(db, gas, "withdraw_gas_all".into(), vec![]),
    ]
}

pub fn internal_require_implicit(db: &dyn SemanticGroup) -> GenericFunctionId {
    get_generic_function_id(db, core_submodule(db, "internal"), "require_implicit".into())
}
/// The function `downcast` from the `integer` submodule.
pub fn core_downcast(db: &dyn SemanticGroup, input: TypeId, output: TypeId) -> FunctionId {
    let internal = core_submodule(db, "integer");

    get_function_id(
        db,
        internal,
        "downcast".into(),
        vec![GenericArgumentId::Type(input), GenericArgumentId::Type(output)],
    )
}
/// Given a core library function name and its generic arguments, returns [FunctionId].
pub fn get_core_function_id(
    db: &dyn SemanticGroup,
    name: SmolStr,
    generic_args: Vec<GenericArgumentId>,
) -> FunctionId {
    get_function_id(db, db.core_module(), name, generic_args)
}

/// Given a module, a library function name and its generic arguments, returns [FunctionId].
pub fn get_function_id(
    db: &dyn SemanticGroup,
    module: ModuleId,
    name: SmolStr,
    generic_args: Vec<GenericArgumentId>,
) -> FunctionId {
    let generic_function = get_generic_function_id(db, module, name);

    FunctionLongId { function: ConcreteFunction { generic_function, generic_args } }.intern(db)
}

/// Given a core library function name, returns [GenericFunctionId].
pub fn get_core_generic_function_id(db: &dyn SemanticGroup, name: SmolStr) -> GenericFunctionId {
    get_generic_function_id(db, db.core_module(), name)
}

/// Given a module and a library function name, returns [GenericFunctionId].
pub fn get_generic_function_id(
    db: &dyn SemanticGroup,
    module: ModuleId,
    name: SmolStr,
) -> GenericFunctionId {
    let module_item_id = db
        .module_item_by_name(module, name.clone())
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
    concrete_trait(db, db.core_info().copy_trt, vec![GenericArgumentId::Type(ty)])
}

pub fn concrete_drop_trait(db: &dyn SemanticGroup, ty: TypeId) -> ConcreteTraitId {
    concrete_trait(db, db.core_info().drop_trt, vec![GenericArgumentId::Type(ty)])
}

pub fn concrete_destruct_trait(db: &dyn SemanticGroup, ty: TypeId) -> ConcreteTraitId {
    concrete_trait(db, db.core_info().destruct_trt, vec![GenericArgumentId::Type(ty)])
}

pub fn concrete_panic_destruct_trait(db: &dyn SemanticGroup, ty: TypeId) -> ConcreteTraitId {
    concrete_trait(db, db.core_info().panic_destruct_trt, vec![GenericArgumentId::Type(ty)])
}

pub fn concrete_iterator_trait(db: &dyn SemanticGroup, ty: TypeId) -> ConcreteTraitId {
    concrete_trait(db, db.core_info().iterator_trt, vec![GenericArgumentId::Type(ty)])
}

pub fn fn_traits(db: &dyn SemanticGroup) -> [TraitId; 2] {
    let info = db.core_info();
    [info.fn_trt, info.fn_once_trt]
}

/// Given a core library generic trait and its generic arguments, returns [ConcreteTraitId].
fn concrete_trait(
    db: &dyn SemanticGroup,
    trait_id: TraitId,
    generic_args: Vec<GenericArgumentId>,
) -> ConcreteTraitId {
    semantic::ConcreteTraitLongId { trait_id, generic_args }.intern(db)
}

/// Retrieves a trait function from the core library with type variables as generic arguments, to
/// be inferred later.
fn get_core_trait_function_infer(
    db: &dyn SemanticGroup,
    inference: &mut Inference<'_>,
    trait_id: TraitId,
    trait_function: TraitFunctionId,
    stable_ptr: SyntaxStablePtrId,
) -> ConcreteTraitGenericFunctionId {
    let generic_params = db.trait_generic_params(trait_id).unwrap();
    let generic_args = generic_params
        .iter()
        .map(|_| GenericArgumentId::Type(inference.new_type_var(Some(stable_ptr))))
        .collect();
    let concrete_trait_id = semantic::ConcreteTraitLongId { trait_id, generic_args }.intern(db);
    ConcreteTraitGenericFunctionLongId::new(db, concrete_trait_id, trait_function).intern(db)
}

pub fn get_panic_ty(db: &dyn SemanticGroup, inner_ty: TypeId) -> TypeId {
    get_core_ty_by_name(db.upcast(), "PanicResult".into(), vec![GenericArgumentId::Type(inner_ty)])
}

pub fn get_usize_ty(db: &dyn SemanticGroup) -> TypeId {
    get_core_ty_by_name(db, "usize".into(), vec![])
}

/// Returns [FunctionId] of the libfunc that converts type of `ty` to felt252.
pub fn get_convert_to_felt252_libfunc_name_by_type(
    db: &dyn SemanticGroup,
    ty: TypeId,
) -> Option<FunctionId> {
    let info = db.core_info();
    if ty == info.u8 {
        Some(get_function_id(db, core_submodule(db, "integer"), "u8_to_felt252".into(), vec![]))
    } else if ty == info.u16 {
        Some(get_function_id(db, core_submodule(db, "integer"), "u16_to_felt252".into(), vec![]))
    } else if ty == info.u32 {
        Some(get_function_id(db, core_submodule(db, "integer"), "u32_to_felt252".into(), vec![]))
    } else if ty == info.u64 {
        Some(get_function_id(db, core_submodule(db, "integer"), "u64_to_felt252".into(), vec![]))
    } else if ty == info.u128 {
        Some(get_function_id(db, core_submodule(db, "integer"), "u128_to_felt252".into(), vec![]))
    } else if ty == info.i8 {
        Some(get_function_id(db, core_submodule(db, "integer"), "i8_to_felt252".into(), vec![]))
    } else if ty == info.i16 {
        Some(get_function_id(db, core_submodule(db, "integer"), "i16_to_felt252".into(), vec![]))
    } else if ty == info.i32 {
        Some(get_function_id(db, core_submodule(db, "integer"), "i32_to_felt252".into(), vec![]))
    } else if ty == info.i64 {
        Some(get_function_id(db, core_submodule(db, "integer"), "i64_to_felt252".into(), vec![]))
    } else if ty == info.i128 {
        Some(get_function_id(db, core_submodule(db, "integer"), "i128_to_felt252".into(), vec![]))
    } else {
        None
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum LiteralError {
    InvalidTypeForLiteral(TypeId),
    OutOfRange(TypeId),
}
impl LiteralError {
    pub fn format(&self, db: &dyn SemanticGroup) -> String {
        match self {
            Self::OutOfRange(ty) => format!(
                "The value does not fit within the range of type {}.",
                ty.format(db.upcast())
            ),
            Self::InvalidTypeForLiteral(ty) => {
                format!("A numeric literal of type {} cannot be created.", ty.format(db.upcast()))
            }
        }
    }
}

/// Validates that a given type is valid for a literal and that the value fits the range of the
/// specific type.
pub fn validate_literal(
    db: &dyn SemanticGroup,
    ty: TypeId,
    value: &BigInt,
) -> Result<(), LiteralError> {
    let info = db.core_info();
    let validate_out_of_range = |is_out_of_range: bool| {
        if is_out_of_range { Err(LiteralError::OutOfRange(ty)) } else { Ok(()) }
    };
    if ty == info.felt252 {
        validate_out_of_range(
            value.abs()
                > BigInt::from_str_radix(
                    "800000000000011000000000000000000000000000000000000000000000000",
                    16,
                )
                .unwrap(),
        )
    } else if ty == info.u8 {
        validate_out_of_range(value.to_u8().is_none())
    } else if ty == info.u16 {
        validate_out_of_range(value.to_u16().is_none())
    } else if ty == info.u32 {
        validate_out_of_range(value.to_u32().is_none())
    } else if ty == info.u64 {
        validate_out_of_range(value.to_u64().is_none())
    } else if ty == info.u128 {
        validate_out_of_range(value.to_u128().is_none())
    } else if ty == info.i8 {
        validate_out_of_range(value.to_i8().is_none())
    } else if ty == info.i16 {
        validate_out_of_range(value.to_i16().is_none())
    } else if ty == info.i32 {
        validate_out_of_range(value.to_i32().is_none())
    } else if ty == info.i64 {
        validate_out_of_range(value.to_i64().is_none())
    } else if ty == info.i128 {
        validate_out_of_range(value.to_i128().is_none())
    } else if ty == info.u256 {
        validate_out_of_range(value.is_negative() || value.bits() > 256)
    } else if ty == info.class_hash || ty == info.contract_address {
        validate_out_of_range(value.is_negative() || value.bits() > 251)
    } else if let Some(nz_wrapped_ty) = try_extract_nz_wrapped_type(db, ty) {
        if value.is_zero() {
            Err(LiteralError::OutOfRange(ty))
        } else {
            validate_literal(db, nz_wrapped_ty, value)
        }
    } else if let Some((min, max)) = try_extract_bounded_int_type_ranges(db, ty) {
        validate_out_of_range(*value < min || *value > max)
    } else {
        Err(LiteralError::InvalidTypeForLiteral(ty))
    }
}

/// Returns the type if the inner value of a `NonZero` type, if it is wrapped in one.
pub fn try_extract_nz_wrapped_type(db: &dyn SemanticGroup, ty: TypeId) -> Option<TypeId> {
    let concrete_ty = try_extract_matches!(ty.lookup_intern(db), TypeLongId::Concrete)?;
    let extern_ty = try_extract_matches!(concrete_ty, ConcreteTypeId::Extern)?;
    let ConcreteExternTypeLongId { extern_type_id, generic_args } = extern_ty.lookup_intern(db);
    let [GenericArgumentId::Type(inner)] = generic_args[..] else { return None };
    (extern_type_id.name(db.upcast()) == "NonZero").then_some(inner)
}

/// Returns the ranges of a BoundedInt if it is a BoundedInt type.
fn try_extract_bounded_int_type_ranges(
    db: &dyn SemanticGroup,
    ty: TypeId,
) -> Option<(BigInt, BigInt)> {
    let concrete_ty = try_extract_matches!(db.lookup_intern_type(ty), TypeLongId::Concrete)?;
    let extern_ty = try_extract_matches!(concrete_ty, ConcreteTypeId::Extern)?;
    let ConcreteExternTypeLongId { extern_type_id, generic_args } =
        db.lookup_intern_concrete_extern_type(extern_ty);
    require(extern_type_id.name(db.upcast()) == "BoundedInt")?;
    let [GenericArgumentId::Constant(min), GenericArgumentId::Constant(max)] = generic_args[..]
    else {
        return None;
    };
    let to_int = |id| db.lookup_intern_const_value(id).into_int();

    Some((to_int(min)?, to_int(max)?))
}

/// Information about various core types and traits.
#[derive(Debug, Eq, PartialEq, Hash)]
pub struct CoreInfo {
    // Types.
    pub felt252: TypeId,
    pub u8: TypeId,
    pub u16: TypeId,
    pub u32: TypeId,
    pub u64: TypeId,
    pub u128: TypeId,
    pub u256: TypeId,
    pub i8: TypeId,
    pub i16: TypeId,
    pub i32: TypeId,
    pub i64: TypeId,
    pub i128: TypeId,
    pub class_hash: TypeId,
    pub contract_address: TypeId,
    // Traits.
    pub numeric_literal_trt: TraitId,
    pub string_literal_trt: TraitId,
    pub deref_trt: TraitId,
    pub deref_mut_trt: TraitId,
    pub index_trt: TraitId,
    pub index_view_trt: TraitId,
    pub copy_trt: TraitId,
    pub drop_trt: TraitId,
    pub destruct_trt: TraitId,
    pub panic_destruct_trt: TraitId,
    pub add_trt: TraitId,
    pub sub_trt: TraitId,
    pub mul_trt: TraitId,
    pub div_trt: TraitId,
    pub rem_trt: TraitId,
    pub div_rem_trt: TraitId,
    pub neg_trt: TraitId,
    pub add_assign_trt: TraitId,
    pub sub_assign_trt: TraitId,
    pub mul_assign_trt: TraitId,
    pub div_assign_trt: TraitId,
    pub rem_assign_trt: TraitId,
    pub bitnot_trt: TraitId,
    pub bitand_trt: TraitId,
    pub bitor_trt: TraitId,
    pub bitxor_trt: TraitId,
    pub not_trt: TraitId,
    pub partialeq_trt: TraitId,
    pub partialord_trt: TraitId,
    pub range_op_trt: TraitId,
    pub range_inclusive_op_trt: TraitId,
    pub into_iterator_trt: TraitId,
    pub iterator_trt: TraitId,
    pub fn_trt: TraitId,
    pub fn_once_trt: TraitId,
    pub type_eq_trt: TraitId,
    pub felt252_dict_value_trt: TraitId,
    // Trait fns.
    pub deref_fn: TraitFunctionId,
    pub deref_mut_fn: TraitFunctionId,
    pub destruct_fn: TraitFunctionId,
    pub panic_destruct_fn: TraitFunctionId,
    pub add_fn: TraitFunctionId,
    pub sub_fn: TraitFunctionId,
    pub mul_fn: TraitFunctionId,
    pub div_fn: TraitFunctionId,
    pub rem_fn: TraitFunctionId,
    pub div_rem_fn: TraitFunctionId,
    pub neg_fn: TraitFunctionId,
    pub add_assign_fn: TraitFunctionId,
    pub sub_assign_fn: TraitFunctionId,
    pub mul_assign_fn: TraitFunctionId,
    pub div_assign_fn: TraitFunctionId,
    pub rem_assign_fn: TraitFunctionId,
    pub bitnot_fn: TraitFunctionId,
    pub bitand_fn: TraitFunctionId,
    pub bitor_fn: TraitFunctionId,
    pub bitxor_fn: TraitFunctionId,
    pub not_fn: TraitFunctionId,
    pub eq_fn: TraitFunctionId,
    pub ne_fn: TraitFunctionId,
    pub lt_fn: TraitFunctionId,
    pub gt_fn: TraitFunctionId,
    pub le_fn: TraitFunctionId,
    pub ge_fn: TraitFunctionId,
    pub range_fn: TraitFunctionId,
    pub range_inclusive_fn: TraitFunctionId,
    pub into_iter_fn: TraitFunctionId,
    pub next_fn: TraitFunctionId,
    pub call_fn: TraitFunctionId,
    pub call_once_fn: TraitFunctionId,
}
impl CoreInfo {
    fn new(db: &dyn SemanticGroup) -> Self {
        let core = ModuleHelper::core(db);
        let integer = core.submodule("integer");
        let traits = core.submodule("traits");
        let ops = core.submodule("ops");
        let deref_module = ops.submodule("deref");
        let deref_trt = deref_module.trait_id("Deref");
        let deref_mut_trt = deref_module.trait_id("DerefMut");
        let destruct_trt = traits.trait_id("Destruct");
        let panic_destruct_trt = traits.trait_id("PanicDestruct");
        let add_trt = traits.trait_id("Add");
        let sub_trt = traits.trait_id("Sub");
        let mul_trt = traits.trait_id("Mul");
        let div_trt = traits.trait_id("Div");
        let rem_trt = traits.trait_id("Rem");
        let div_rem_trt = traits.trait_id("DivRem");
        let neg_trt = traits.trait_id("Neg");
        let arith_module = ops.submodule("arith");
        let add_assign_trt = arith_module.trait_id("AddAssign");
        let sub_assign_trt = arith_module.trait_id("SubAssign");
        let mul_assign_trt = arith_module.trait_id("MulAssign");
        let div_assign_trt = arith_module.trait_id("DivAssign");
        let rem_assign_trt = arith_module.trait_id("RemAssign");
        let bitnot_trt = traits.trait_id("BitNot");
        let bitand_trt = traits.trait_id("BitAnd");
        let bitor_trt = traits.trait_id("BitOr");
        let bitxor_trt = traits.trait_id("BitXor");
        let not_trt = traits.trait_id("Not");
        let partialeq_trt = traits.trait_id("PartialEq");
        let partialord_trt = traits.trait_id("PartialOrd");
        let range_module = ops.submodule("range");
        let range_op_trt = range_module.trait_id("RangeOp");
        let range_inclusive_op_trt = range_module.trait_id("RangeInclusiveOp");
        let iter_traits = core.submodule("iter").submodule("traits");
        let into_iterator_trt = iter_traits.submodule("collect").trait_id("IntoIterator");
        let iterator_trt = iter_traits.submodule("iterator").trait_id("Iterator");
        let fn_module = ops.submodule("function");
        let fn_trt = fn_module.trait_id("Fn");
        let fn_once_trt = fn_module.trait_id("FnOnce");
        let index_module = ops.submodule("index");
        let starknet = core.submodule("starknet");
        let trait_fn = |trait_id: TraitId, name: &str| {
            db.trait_function_by_name(trait_id, name.into()).unwrap().unwrap()
        };
        Self {
            felt252: core.ty("felt252", vec![]),
            u8: integer.ty("u8", vec![]),
            u16: integer.ty("u16", vec![]),
            u32: integer.ty("u32", vec![]),
            u64: integer.ty("u64", vec![]),
            u128: integer.ty("u128", vec![]),
            u256: integer.ty("u256", vec![]),
            i8: integer.ty("i8", vec![]),
            i16: integer.ty("i16", vec![]),
            i32: integer.ty("i32", vec![]),
            i64: integer.ty("i64", vec![]),
            i128: integer.ty("i128", vec![]),
            class_hash: starknet.submodule("class_hash").ty("ClassHash", vec![]),
            contract_address: starknet.submodule("contract_address").ty("ContractAddress", vec![]),
            numeric_literal_trt: integer.trait_id("NumericLiteral"),
            string_literal_trt: core.submodule("string").trait_id("StringLiteral"),
            index_trt: index_module.trait_id("Index"),
            index_view_trt: index_module.trait_id("IndexView"),
            deref_trt,
            deref_mut_trt,
            copy_trt: traits.trait_id("Copy"),
            drop_trt: traits.trait_id("Drop"),
            destruct_trt,
            panic_destruct_trt,
            add_trt,
            sub_trt,
            mul_trt,
            div_trt,
            rem_trt,
            div_rem_trt,
            neg_trt,
            add_assign_trt,
            sub_assign_trt,
            mul_assign_trt,
            div_assign_trt,
            rem_assign_trt,
            bitnot_trt,
            bitand_trt,
            bitor_trt,
            bitxor_trt,
            not_trt,
            partialeq_trt,
            partialord_trt,
            range_op_trt,
            range_inclusive_op_trt,
            into_iterator_trt,
            iterator_trt,
            fn_trt,
            fn_once_trt,
            type_eq_trt: core.submodule("metaprogramming").trait_id("TypeEqual"),
            felt252_dict_value_trt: traits.trait_id("Felt252DictValue"),
            deref_fn: trait_fn(deref_trt, "deref"),
            deref_mut_fn: trait_fn(deref_mut_trt, "deref_mut"),
            destruct_fn: trait_fn(destruct_trt, "destruct"),
            panic_destruct_fn: trait_fn(panic_destruct_trt, "panic_destruct"),
            add_fn: trait_fn(add_trt, "add"),
            sub_fn: trait_fn(sub_trt, "sub"),
            mul_fn: trait_fn(mul_trt, "mul"),
            div_fn: trait_fn(div_trt, "div"),
            rem_fn: trait_fn(rem_trt, "rem"),
            div_rem_fn: trait_fn(div_rem_trt, "div_rem"),
            neg_fn: trait_fn(neg_trt, "neg"),
            add_assign_fn: trait_fn(add_assign_trt, "add_assign"),
            sub_assign_fn: trait_fn(sub_assign_trt, "sub_assign"),
            mul_assign_fn: trait_fn(mul_assign_trt, "mul_assign"),
            div_assign_fn: trait_fn(div_assign_trt, "div_assign"),
            rem_assign_fn: trait_fn(rem_assign_trt, "rem_assign"),
            bitnot_fn: trait_fn(bitnot_trt, "bitnot"),
            bitand_fn: trait_fn(bitand_trt, "bitand"),
            bitor_fn: trait_fn(bitor_trt, "bitor"),
            bitxor_fn: trait_fn(bitxor_trt, "bitxor"),
            not_fn: trait_fn(not_trt, "not"),
            eq_fn: trait_fn(partialeq_trt, "eq"),
            ne_fn: trait_fn(partialeq_trt, "ne"),
            lt_fn: trait_fn(partialord_trt, "lt"),
            gt_fn: trait_fn(partialord_trt, "gt"),
            le_fn: trait_fn(partialord_trt, "le"),
            ge_fn: trait_fn(partialord_trt, "ge"),
            range_fn: trait_fn(range_op_trt, "range"),
            range_inclusive_fn: trait_fn(range_inclusive_op_trt, "range_inclusive"),
            into_iter_fn: trait_fn(into_iterator_trt, "into_iter"),
            next_fn: trait_fn(iterator_trt, "next"),
            call_fn: trait_fn(fn_trt, "call"),
            call_once_fn: trait_fn(fn_once_trt, "call"),
        }
    }
}

/// Query implementation of [SemanticGroup::core_info].
pub fn core_info(db: &dyn SemanticGroup) -> Arc<CoreInfo> {
    CoreInfo::new(db).into()
}
