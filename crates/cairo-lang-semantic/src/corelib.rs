use std::sync::Arc;

use cairo_lang_defs::ids::{
    EnumId, GenericTypeId, ImplDefId, ModuleId, ModuleItemId, NamedLanguageElementId,
    TraitFunctionId, TraitId,
};
use cairo_lang_diagnostics::{Maybe, ToOption};
use cairo_lang_filesystem::ids::{CrateId, SmolStrId};
use cairo_lang_syntax::node::ast::{self, BinaryOperator, UnaryOperator};
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_utils::{Intern, OptionFrom, extract_matches, require, try_extract_matches};
use num_bigint::BigInt;
use num_traits::{Num, Signed, ToPrimitive, Zero};
use salsa::Database;

use crate::diagnostic::SemanticDiagnosticKind;
use crate::expr::compute::ComputationContext;
use crate::expr::inference::Inference;
use crate::helper::ModuleHelper;
use crate::items::constant::{ConstValue, ConstValueId};
use crate::items::enm::{EnumSemantic, SemanticEnumEx};
use crate::items::functions::{GenericFunctionId, ImplGenericFunctionId};
use crate::items::imp::{ImplLongId, ImplSemantic};
use crate::items::module::ModuleSemantic;
use crate::items::module_type_alias::ModuleTypeAliasSemantic;
use crate::items::trt::{
    ConcreteTraitGenericFunctionId, ConcreteTraitGenericFunctionLongId, ConcreteTraitId,
    TraitSemantic,
};
use crate::items::us::SemanticUseEx;
use crate::resolve::ResolvedGenericItem;
use crate::types::{ConcreteEnumLongId, ConcreteExternTypeLongId};
use crate::{
    ConcreteEnumId, ConcreteFunction, ConcreteImplLongId, ConcreteTypeId, ConcreteVariant, Expr,
    ExprId, ExprTuple, FunctionId, FunctionLongId, GenericArgumentId, TypeId, TypeLongId, semantic,
};

/// Implementation of [CorelibSemantic::core_module].
pub fn core_module(db: &dyn Database) -> ModuleId<'_> {
    let core_crate = db.core_crate();
    ModuleId::CrateRoot(core_crate)
}

/// Query implementation of [CorelibSemantic::core_module].
#[salsa::tracked]
pub fn core_module_tracked(db: &dyn Database) -> ModuleId<'_> {
    core_module(db)
}

/// Returns the submodule of `base_module`, named `submodule_name`, if exists.
pub fn get_submodule<'db>(
    db: &'db dyn Database,
    base_module: ModuleId<'db>,
    submodule_name: SmolStrId<'db>,
) -> Option<ModuleId<'db>> {
    let module_item_id = db.module_item_by_name(base_module, submodule_name).ok()??;
    if let ModuleItemId::Submodule(id) = module_item_id {
        Some(ModuleId::Submodule(id))
    } else {
        None
    }
}

/// Returns a submodule of the corelib named `submodule_name`.
/// If no such submodule exists, panics.
pub fn core_submodule<'db>(db: &'db dyn Database, submodule_name: SmolStrId<'db>) -> ModuleId<'db> {
    get_submodule(db, core_module(db), submodule_name)
        .unwrap_or_else(|| panic!("`{}` is not a core submodule.", submodule_name.long(db)))
}

/// Implementation of [CorelibSemantic::core_crate].
pub fn core_crate(db: &dyn Database) -> CrateId<'_> {
    CrateId::core(db)
}

/// Query implementation of [CorelibSemantic::core_crate].
#[salsa::tracked]
pub fn core_crate_tracked(db: &dyn Database) -> CrateId<'_> {
    core_crate(db)
}

/// Returns the concrete type of a bounded int type with a given min and max.
pub fn bounded_int_ty<'db>(db: &'db dyn Database, min: BigInt, max: BigInt) -> TypeId<'db> {
    let internal = core_submodule(db, SmolStrId::from(db, "internal"));
    let bounded_int = get_submodule(db, internal, SmolStrId::from(db, "bounded_int"))
        .expect("Could not find bounded_int submodule in corelib.");
    let felt252_ty = db.core_info().felt252;
    let lower_id = ConstValue::Int(min, felt252_ty).intern(db);
    let upper_id = ConstValue::Int(max, felt252_ty).intern(db);
    try_get_ty_by_name(
        db,
        bounded_int,
        SmolStrId::from(db, "BoundedInt"),
        vec![GenericArgumentId::Constant(lower_id), GenericArgumentId::Constant(upper_id)],
    )
    .expect("could not find")
}

pub fn core_nonzero_ty<'db>(db: &'db dyn Database, inner_type: TypeId<'db>) -> TypeId<'db> {
    get_ty_by_name(
        db,
        core_submodule(db, SmolStrId::from(db, "zeroable")),
        SmolStrId::from(db, "NonZero"),
        vec![GenericArgumentId::Type(inner_type)],
    )
}

pub fn core_result_ty<'db>(
    db: &'db dyn Database,
    ok_type: TypeId<'db>,
    err_type: TypeId<'db>,
) -> TypeId<'db> {
    get_ty_by_name(
        db,
        core_submodule(db, SmolStrId::from(db, "result")),
        SmolStrId::from(db, "Result"),
        vec![GenericArgumentId::Type(ok_type), GenericArgumentId::Type(err_type)],
    )
}

pub fn core_option_ty<'db>(db: &'db dyn Database, some_type: TypeId<'db>) -> TypeId<'db> {
    get_ty_by_name(
        db,
        core_submodule(db, SmolStrId::from(db, "option")),
        SmolStrId::from(db, "Option"),
        vec![GenericArgumentId::Type(some_type)],
    )
}

pub fn core_box_ty<'db>(db: &'db dyn Database, inner_type: TypeId<'db>) -> TypeId<'db> {
    get_ty_by_name(
        db,
        core_submodule(db, SmolStrId::from(db, "box")),
        SmolStrId::from(db, "Box"),
        vec![GenericArgumentId::Type(inner_type)],
    )
}

pub fn core_array_felt252_ty<'db>(db: &'db dyn Database) -> TypeId<'db> {
    get_core_ty_by_name(
        db,
        SmolStrId::from(db, "Array"),
        vec![GenericArgumentId::Type(db.core_info().felt252)],
    )
}

pub fn try_get_core_ty_by_name<'db>(
    db: &'db dyn Database,
    name: SmolStrId<'db>,
    generic_args: Vec<GenericArgumentId<'db>>,
) -> Result<TypeId<'db>, SemanticDiagnosticKind<'db>> {
    try_get_ty_by_name(db, db.core_module(), name, generic_args)
}

pub fn try_get_ty_by_name<'db>(
    db: &'db dyn Database,
    module: ModuleId<'db>,
    name: SmolStrId<'db>,
    generic_args: Vec<GenericArgumentId<'db>>,
) -> Result<TypeId<'db>, SemanticDiagnosticKind<'db>> {
    // This should not fail if the corelib is present.
    let module_item_id = db
        .module_item_by_name(module, name)
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
    .unwrap_or_else(|| panic!("{} is not a type.", name.long(db)));

    Ok(semantic::TypeLongId::Concrete(semantic::ConcreteTypeId::new(
        db,
        generic_type,
        generic_args,
    ))
    .intern(db))
}

pub fn get_core_ty_by_name<'db>(
    db: &'db dyn Database,
    name: SmolStrId<'db>,
    generic_args: Vec<GenericArgumentId<'db>>,
) -> TypeId<'db> {
    try_get_core_ty_by_name(db, name, generic_args).unwrap()
}

pub fn get_ty_by_name<'db>(
    db: &'db dyn Database,
    module: ModuleId<'db>,
    name: SmolStrId<'db>,
    generic_args: Vec<GenericArgumentId<'db>>,
) -> TypeId<'db> {
    try_get_ty_by_name(db, module, name, generic_args).unwrap()
}

pub fn core_bool_ty<'db>(db: &'db dyn Database) -> TypeId<'db> {
    let core_module = db.core_module();
    // This should not fail if the corelib is present.
    let generic_type = db
        .module_item_by_name(core_module, SmolStrId::from(db, "bool"))
        .expect("Failed to load core lib.")
        .and_then(GenericTypeId::option_from)
        .expect("Type bool was not found in core lib.");
    semantic::TypeLongId::Concrete(semantic::ConcreteTypeId::new(db, generic_type, vec![]))
        .intern(db)
}

// TODO(spapini): Consider making all these queries for better caching.
/// Generates a ConcreteEnumId instance for `bool`.
pub fn core_bool_enum<'db>(db: &'db dyn Database) -> ConcreteEnumId<'db> {
    let core_module = db.core_module();
    // This should not fail if the corelib is present.
    let enum_id = db
        .module_item_by_name(core_module, SmolStrId::from(db, "bool"))
        .expect("Failed to load core lib.")
        .and_then(EnumId::option_from)
        .expect("Type bool was not found in core lib.");
    ConcreteEnumLongId { enum_id, generic_args: vec![] }.intern(db)
}

/// Generates a ConcreteVariant instance for `false`.
pub fn false_variant(db: &dyn Database) -> ConcreteVariant<'_> {
    get_core_enum_concrete_variant(
        db,
        SmolStrId::from(db, "bool"),
        vec![],
        SmolStrId::from(db, "False"),
    )
}

/// Generates a ConcreteVariant instance for `true`.
pub fn true_variant(db: &dyn Database) -> ConcreteVariant<'_> {
    get_core_enum_concrete_variant(
        db,
        SmolStrId::from(db, "bool"),
        vec![],
        SmolStrId::from(db, "True"),
    )
}

/// Generates a ConcreteVariant instance for `IsZeroResult::<felt252>::Zero`.
pub fn jump_nz_zero_variant<'db>(db: &'db dyn Database, ty: TypeId<'db>) -> ConcreteVariant<'db> {
    get_enum_concrete_variant(
        db,
        core_submodule(db, SmolStrId::from(db, "zeroable")),
        SmolStrId::from(db, "IsZeroResult"),
        vec![GenericArgumentId::Type(ty)],
        SmolStrId::from(db, "Zero"),
    )
}

/// Generates a ConcreteVariant instance for `IsZeroResult::<felt252>::NonZero`.
pub fn jump_nz_nonzero_variant<'db>(
    db: &'db dyn Database,
    ty: TypeId<'db>,
) -> ConcreteVariant<'db> {
    get_enum_concrete_variant(
        db,
        core_submodule(db, SmolStrId::from(db, "zeroable")),
        SmolStrId::from(db, "IsZeroResult"),
        vec![GenericArgumentId::Type(ty)],
        SmolStrId::from(db, "NonZero"),
    )
}

/// Generates a ConcreteVariant instance for `Option::Some`.
pub fn option_some_variant<'db>(db: &'db dyn Database, ty: TypeId<'db>) -> ConcreteVariant<'db> {
    get_enum_concrete_variant(
        db,
        core_submodule(db, SmolStrId::from(db, "option")),
        SmolStrId::from(db, "Option"),
        vec![GenericArgumentId::Type(ty)],
        SmolStrId::from(db, "Some"),
    )
}

/// Generates a ConcreteVariant instance for `Option::None`.
pub fn option_none_variant<'db>(db: &'db dyn Database, ty: TypeId<'db>) -> ConcreteVariant<'db> {
    get_enum_concrete_variant(
        db,
        core_submodule(db, SmolStrId::from(db, "option")),
        SmolStrId::from(db, "Option"),
        vec![GenericArgumentId::Type(ty)],
        SmolStrId::from(db, "None"),
    )
}

/// Generates a ConcreteVariant instance for `Result::Ok`.
pub fn result_ok_variant<'db>(
    db: &'db dyn Database,
    ok_ty: TypeId<'db>,
    err_ty: TypeId<'db>,
) -> ConcreteVariant<'db> {
    get_enum_concrete_variant(
        db,
        core_submodule(db, SmolStrId::from(db, "result")),
        SmolStrId::from(db, "Result"),
        vec![GenericArgumentId::Type(ok_ty), GenericArgumentId::Type(err_ty)],
        SmolStrId::from(db, "Ok"),
    )
}

/// Generates a ConcreteVariant instance for `Result::Err`.
pub fn result_err_variant<'db>(
    db: &'db dyn Database,
    ok_ty: TypeId<'db>,
    err_ty: TypeId<'db>,
) -> ConcreteVariant<'db> {
    get_enum_concrete_variant(
        db,
        core_submodule(db, SmolStrId::from(db, "result")),
        SmolStrId::from(db, "Result"),
        vec![GenericArgumentId::Type(ok_ty), GenericArgumentId::Type(err_ty)],
        SmolStrId::from(db, "Err"),
    )
}

/// Generates a ConcreteVariant instance for `SignedIntegerResult::InRange`.
pub fn signed_int_result_in_range_variant<'db>(
    db: &'db dyn Database,
    ty: TypeId<'db>,
) -> ConcreteVariant<'db> {
    get_enum_concrete_variant(
        db,
        core_submodule(db, SmolStrId::from(db, "integer")),
        SmolStrId::from(db, "SignedIntegerResult"),
        vec![GenericArgumentId::Type(ty)],
        SmolStrId::from(db, "InRange"),
    )
}
/// Generates a ConcreteVariant instance for `SignedIntegerResult::Underflow`.
pub fn signed_int_result_underflow_variant<'db>(
    db: &'db dyn Database,
    ty: TypeId<'db>,
) -> ConcreteVariant<'db> {
    get_enum_concrete_variant(
        db,
        core_submodule(db, SmolStrId::from(db, "integer")),
        SmolStrId::from(db, "SignedIntegerResult"),
        vec![GenericArgumentId::Type(ty)],
        SmolStrId::from(db, "Underflow"),
    )
}
/// Generates a ConcreteVariant instance for `SignedIntegerResult::Overflow`.
pub fn signed_int_result_overflow_variant<'db>(
    db: &'db dyn Database,
    ty: TypeId<'db>,
) -> ConcreteVariant<'db> {
    get_enum_concrete_variant(
        db,
        core_submodule(db, SmolStrId::from(db, "integer")),
        SmolStrId::from(db, "SignedIntegerResult"),
        vec![GenericArgumentId::Type(ty)],
        SmolStrId::from(db, "Overflow"),
    )
}

/// Gets a semantic expression of the literal `false`. Uses the given `stable_ptr` in the returned
/// semantic expression.
pub fn false_literal_expr<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    stable_ptr: ast::ExprPtr<'db>,
) -> semantic::Expr<'db> {
    get_bool_variant_expr(
        ctx,
        SmolStrId::from(ctx.db, "bool"),
        SmolStrId::from(ctx.db, "False"),
        stable_ptr,
    )
}

/// Gets a semantic expression of the literal `true`. Uses the given `stable_ptr` in the returned
/// semantic expression.
pub fn true_literal_expr<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    stable_ptr: ast::ExprPtr<'db>,
) -> semantic::Expr<'db> {
    get_bool_variant_expr(
        ctx,
        SmolStrId::from(ctx.db, "bool"),
        SmolStrId::from(ctx.db, "True"),
        stable_ptr,
    )
}

/// Gets a semantic expression of the specified bool enum variant. Uses the given `stable_ptr` in
/// the returned semantic expression.
fn get_bool_variant_expr<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    enum_name: SmolStrId<'db>,
    variant_name: SmolStrId<'db>,
    stable_ptr: ast::ExprPtr<'db>,
) -> semantic::Expr<'db> {
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
pub fn get_enum_concrete_variant<'db>(
    db: &'db dyn Database,
    module_id: ModuleId<'db>,
    enum_name: SmolStrId<'db>,
    generic_args: Vec<GenericArgumentId<'db>>,
    variant_name: SmolStrId<'db>,
) -> ConcreteVariant<'db> {
    let ty = get_ty_by_name(db, module_id, enum_name, generic_args);
    let concrete_ty = extract_matches!(ty.long(db), TypeLongId::Concrete);
    let concrete_enum_id = extract_matches!(concrete_ty, ConcreteTypeId::Enum);
    let enum_id = concrete_enum_id.enum_id(db);
    let variant_id = db.enum_variants(enum_id).unwrap()[&variant_name];
    let variant = db.variant_semantic(enum_id, variant_id).unwrap();
    db.concrete_enum_variant(*concrete_enum_id, &variant).unwrap()
}

/// Gets a [ConcreteVariant] instance for an enum variant from the core module, by name.
/// Assumes the variant exists.
pub fn get_core_enum_concrete_variant<'db>(
    db: &'db dyn Database,
    enum_name: SmolStrId<'db>,
    generic_args: Vec<GenericArgumentId<'db>>,
    variant_name: SmolStrId<'db>,
) -> ConcreteVariant<'db> {
    get_enum_concrete_variant(db, core_module(db), enum_name, generic_args, variant_name)
}

/// Gets the unit type ().
pub fn unit_ty<'db>(db: &'db dyn Database) -> TypeId<'db> {
    semantic::TypeLongId::Tuple(vec![]).intern(db)
}

/// Gets the never type ().
pub fn never_ty<'db>(db: &'db dyn Database) -> TypeId<'db> {
    let core_module = db.core_module();
    // This should not fail if the corelib is present.
    let generic_type = db
        .module_item_by_name(core_module, SmolStrId::from(db, "never"))
        .expect("Failed to load core lib.")
        .and_then(GenericTypeId::option_from)
        .expect("Type never was not found in core lib.");
    semantic::TypeLongId::Concrete(semantic::ConcreteTypeId::new(db, generic_type, vec![]))
        .intern(db)
}

pub enum ErrorPropagationType<'db> {
    Option { some_variant: ConcreteVariant<'db>, none_variant: ConcreteVariant<'db> },
    Result { ok_variant: ConcreteVariant<'db>, err_variant: ConcreteVariant<'db> },
}
impl<'db> ErrorPropagationType<'db> {
    pub fn ok_variant(&self) -> &ConcreteVariant<'db> {
        match self {
            ErrorPropagationType::Option { some_variant, .. } => some_variant,
            ErrorPropagationType::Result { ok_variant, .. } => ok_variant,
        }
    }
    pub fn err_variant(&self) -> &ConcreteVariant<'db> {
        match self {
            ErrorPropagationType::Option { none_variant, .. } => none_variant,
            ErrorPropagationType::Result { err_variant, .. } => err_variant,
        }
    }
}

/// Attempts to unwrap error propagation types (Option, Result).
/// Returns None if not one of these types.
pub fn unwrap_error_propagation_type<'db>(
    db: &'db dyn Database,
    ty: TypeId<'db>,
) -> Option<ErrorPropagationType<'db>> {
    match ty.long(db) {
        // Only enums may be `Result` and `Option` types.
        TypeLongId::Concrete(semantic::ConcreteTypeId::Enum(enm)) => {
            if let [ok_variant, err_variant] =
                db.concrete_enum_variants(*enm).to_option()?.as_slice()
            {
                let name = enm.enum_id(db).name(db);
                if name.long(db) == "Option" {
                    return Some(ErrorPropagationType::Option {
                        some_variant: *ok_variant,
                        none_variant: *err_variant,
                    });
                } else if name.long(db) == "Result" {
                    return Some(ErrorPropagationType::Result {
                        ok_variant: *ok_variant,
                        err_variant: *err_variant,
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
pub fn unit_expr<'db>(
    ctx: &mut ComputationContext<'db, '_>,
    stable_ptr: ast::ExprPtr<'db>,
) -> ExprId {
    ctx.arenas.exprs.alloc(Expr::Tuple(ExprTuple {
        items: Vec::new(),
        ty: TypeLongId::Tuple(Vec::new()).intern(ctx.db),
        stable_ptr,
    }))
}

pub fn core_unary_operator<'db>(
    db: &'db dyn Database,
    inference: &mut Inference<'db, '_>,
    unary_op: &UnaryOperator<'db>,
    stable_ptr: SyntaxStablePtrId<'db>,
) -> Maybe<Result<ConcreteTraitGenericFunctionId<'db>, SemanticDiagnosticKind<'db>>> {
    let info = db.core_info();
    let (trait_id, trait_fn) = match unary_op {
        UnaryOperator::Minus(_) => (info.neg_trt, info.neg_fn),
        UnaryOperator::Not(_) => (info.not_trt, info.not_fn),
        UnaryOperator::BitNot(_) => (info.bitnot_trt, info.bitnot_fn),
        UnaryOperator::At(_) => unreachable!("@ is not an unary operator."),
        UnaryOperator::Desnap(_) => unreachable!("* is not an unary operator."),
        UnaryOperator::Reference(_) => unreachable!("& is handled before reaching here."),
    };
    Ok(Ok(get_core_trait_function_infer(db, inference, trait_id, trait_fn, stable_ptr)))
}

pub fn core_binary_operator<'db>(
    db: &'db dyn Database,
    inference: &mut Inference<'db, '_>,
    binary_op: &BinaryOperator<'db>,
    stable_ptr: SyntaxStablePtrId<'db>,
) -> Maybe<Result<(ConcreteTraitGenericFunctionId<'db>, bool), SemanticDiagnosticKind<'db>>> {
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

pub fn felt252_sub<'db>(db: &'db dyn Database) -> FunctionId<'db> {
    get_core_function_impl_method(db, SmolStrId::from(db, "Felt252Sub"), SmolStrId::from(db, "sub"))
}

/// Given a core library impl name and a method name, returns [FunctionId].
fn get_core_function_impl_method<'db>(
    db: &'db dyn Database,
    impl_name: SmolStrId<'db>,
    method_name: SmolStrId<'db>,
) -> FunctionId<'db> {
    let core_module = db.core_module();
    let module_item_id = db
        .module_item_by_name(core_module, impl_name)
        .expect("Failed to load core lib.")
        .unwrap_or_else(|| panic!("Impl '{}' was not found in core lib.", impl_name.long(db)));
    let impl_def_id = match module_item_id {
        ModuleItemId::Use(use_id) => {
            db.use_resolved_item(use_id).to_option().and_then(|resolved_generic_item| {
                try_extract_matches!(resolved_generic_item, ResolvedGenericItem::Impl)
            })
        }
        _ => ImplDefId::option_from(module_item_id),
    }
    .unwrap_or_else(|| panic!("{} is not an impl.", impl_name.long(db)));
    let impl_id =
        ImplLongId::Concrete(ConcreteImplLongId { impl_def_id, generic_args: vec![] }.intern(db))
            .intern(db);
    let concrete_trait_id = db.impl_concrete_trait(impl_id).unwrap();
    let function = db
        .trait_functions(concrete_trait_id.trait_id(db))
        .ok()
        .and_then(|functions| functions.get(&method_name).cloned())
        .unwrap_or_else(|| {
            panic!(
                "no {} in {}.",
                method_name.long(db),
                concrete_trait_id.trait_id(db).name(db).long(db)
            )
        });
    FunctionLongId {
        function: ConcreteFunction {
            generic_function: GenericFunctionId::Impl(ImplGenericFunctionId { impl_id, function }),
            generic_args: vec![],
        },
    }
    .intern(db)
}

pub fn core_felt252_is_zero<'db>(db: &'db dyn Database) -> FunctionId<'db> {
    get_core_function_id(db, SmolStrId::from(db, "felt252_is_zero"), vec![])
}

/// The gas withdrawal functions from the `gas` submodule.
pub fn core_withdraw_gas_fns<'db>(db: &'db dyn Database) -> [FunctionId<'db>; 2] {
    let gas = core_submodule(db, SmolStrId::from(db, "gas"));
    [
        get_function_id(db, gas, SmolStrId::from(db, "withdraw_gas"), vec![]),
        get_function_id(db, gas, SmolStrId::from(db, "withdraw_gas_all"), vec![]),
    ]
}

pub fn internal_require_implicit(db: &dyn Database) -> GenericFunctionId<'_> {
    get_generic_function_id(
        db,
        core_submodule(db, SmolStrId::from(db, "internal")),
        SmolStrId::from(db, "require_implicit"),
    )
}
/// Given a core library function name and its generic arguments, returns [FunctionId].
pub fn get_core_function_id<'db>(
    db: &'db dyn Database,
    name: SmolStrId<'db>,
    generic_args: Vec<GenericArgumentId<'db>>,
) -> FunctionId<'db> {
    get_function_id(db, db.core_module(), name, generic_args)
}

/// Given a module, a library function name and its generic arguments, returns [FunctionId].
pub fn get_function_id<'db>(
    db: &'db dyn Database,
    module: ModuleId<'db>,
    name: SmolStrId<'db>,
    generic_args: Vec<GenericArgumentId<'db>>,
) -> FunctionId<'db> {
    get_generic_function_id(db, module, name).concretize(db, generic_args)
}

/// Given a core library function name, returns [GenericFunctionId].
pub fn get_core_generic_function_id<'db>(
    db: &'db dyn Database,
    name: SmolStrId<'db>,
) -> GenericFunctionId<'db> {
    get_generic_function_id(db, db.core_module(), name)
}

/// Given a module and a library function name, returns [GenericFunctionId].
pub fn get_generic_function_id<'db>(
    db: &'db dyn Database,
    module: ModuleId<'db>,
    name: SmolStrId<'db>,
) -> GenericFunctionId<'db> {
    let module_item_id = db
        .module_item_by_name(module, name)
        .expect("Failed to load core lib.")
        .unwrap_or_else(|| panic!("Function '{}' was not found in core lib.", name.long(db)));
    match module_item_id {
        ModuleItemId::Use(use_id) => {
            db.use_resolved_item(use_id).to_option().and_then(|resolved_generic_item| {
                try_extract_matches!(resolved_generic_item, ResolvedGenericItem::GenericFunction)
            })
        }
        _ => GenericFunctionId::option_from(module_item_id),
    }
    .unwrap_or_else(|| panic!("{} is not a function.", name.long(db)))
}

pub fn concrete_copy_trait<'db>(db: &'db dyn Database, ty: TypeId<'db>) -> ConcreteTraitId<'db> {
    concrete_trait(db, db.core_info().copy_trt, vec![GenericArgumentId::Type(ty)])
}

pub fn concrete_drop_trait<'db>(db: &'db dyn Database, ty: TypeId<'db>) -> ConcreteTraitId<'db> {
    concrete_trait(db, db.core_info().drop_trt, vec![GenericArgumentId::Type(ty)])
}

pub fn concrete_destruct_trait<'db>(
    db: &'db dyn Database,
    ty: TypeId<'db>,
) -> ConcreteTraitId<'db> {
    concrete_trait(db, db.core_info().destruct_trt, vec![GenericArgumentId::Type(ty)])
}

pub fn concrete_panic_destruct_trait<'db>(
    db: &'db dyn Database,
    ty: TypeId<'db>,
) -> ConcreteTraitId<'db> {
    concrete_trait(db, db.core_info().panic_destruct_trt, vec![GenericArgumentId::Type(ty)])
}

pub fn concrete_iterator_trait<'db>(
    db: &'db dyn Database,
    ty: TypeId<'db>,
) -> ConcreteTraitId<'db> {
    concrete_trait(db, db.core_info().iterator_trt, vec![GenericArgumentId::Type(ty)])
}

pub fn fn_traits<'db>(db: &'db dyn Database) -> [TraitId<'db>; 2] {
    let info = db.core_info();
    [info.fn_trt, info.fn_once_trt]
}

/// Given a core library generic trait and its generic arguments, returns [ConcreteTraitId].
fn concrete_trait<'db>(
    db: &'db dyn Database,
    trait_id: TraitId<'db>,
    generic_args: Vec<GenericArgumentId<'db>>,
) -> ConcreteTraitId<'db> {
    semantic::ConcreteTraitLongId { trait_id, generic_args }.intern(db)
}

/// Retrieves a trait function from the core library with type variables as generic arguments, to
/// be inferred later.
fn get_core_trait_function_infer<'db>(
    db: &'db dyn Database,
    inference: &mut Inference<'db, '_>,
    trait_id: TraitId<'db>,
    trait_function: TraitFunctionId<'db>,
    stable_ptr: SyntaxStablePtrId<'db>,
) -> ConcreteTraitGenericFunctionId<'db> {
    let generic_params = db.trait_generic_params(trait_id).unwrap();
    let generic_args = generic_params
        .iter()
        .map(|_| GenericArgumentId::Type(inference.new_type_var(Some(stable_ptr))))
        .collect();
    let concrete_trait_id = semantic::ConcreteTraitLongId { trait_id, generic_args }.intern(db);
    ConcreteTraitGenericFunctionLongId::new(db, concrete_trait_id, trait_function).intern(db)
}

pub fn get_panic_ty<'db>(db: &'db dyn Database, inner_ty: TypeId<'db>) -> TypeId<'db> {
    get_core_ty_by_name(
        db,
        SmolStrId::from(db, "PanicResult"),
        vec![GenericArgumentId::Type(inner_ty)],
    )
}

pub fn get_usize_ty<'db>(db: &'db dyn Database) -> TypeId<'db> {
    get_core_ty_by_name(db, SmolStrId::from(db, "usize"), vec![])
}

/// Returns if `ty` is a numeric type upcastable to felt252.
pub fn numeric_upcastable_to_felt252(db: &dyn Database, ty: TypeId<'_>) -> bool {
    let info = db.core_info();
    ty == info.felt252
        || ty == info.u8
        || ty == info.u16
        || ty == info.u32
        || ty == info.u64
        || ty == info.u128
        || ty == info.i8
        || ty == info.i16
        || ty == info.i32
        || ty == info.i64
        || ty == info.i128
        || try_extract_bounded_int_type_ranges(db, ty).is_some()
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, salsa::Update)]
pub enum LiteralError<'db> {
    InvalidTypeForLiteral(TypeId<'db>),
    OutOfRange(TypeId<'db>),
}
impl<'db> LiteralError<'db> {
    pub fn format(&self, db: &dyn Database) -> String {
        match self {
            Self::OutOfRange(ty) => {
                format!("The value does not fit within the range of type {}.", ty.format(db))
            }
            Self::InvalidTypeForLiteral(ty) => {
                format!("A numeric literal of type {} cannot be created.", ty.format(db))
            }
        }
    }
}

/// Validates that a given type is valid for a literal and that the value fits the range of the
/// specific type.
pub fn validate_literal<'db>(
    db: &'db dyn Database,
    ty: TypeId<'db>,
    value: &BigInt,
) -> Result<(), LiteralError<'db>> {
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
pub fn try_extract_nz_wrapped_type<'db>(
    db: &'db dyn Database,
    ty: TypeId<'db>,
) -> Option<TypeId<'db>> {
    let concrete_ty = try_extract_matches!(ty.long(db), TypeLongId::Concrete)?;
    let extern_ty = try_extract_matches!(concrete_ty, ConcreteTypeId::Extern)?;
    let ConcreteExternTypeLongId { extern_type_id, generic_args } = extern_ty.long(db);
    let [GenericArgumentId::Type(inner)] = generic_args[..] else { return None };
    (extern_type_id.name(db).long(db) == "NonZero").then_some(inner)
}

/// Returns the ranges of a BoundedInt if it is a BoundedInt type.
pub fn try_extract_bounded_int_type_ranges<'db>(
    db: &'db dyn Database,
    ty: TypeId<'db>,
) -> Option<(BigInt, BigInt)> {
    let concrete_ty = try_extract_matches!(ty.long(db), TypeLongId::Concrete)?;
    let extern_ty = try_extract_matches!(concrete_ty, ConcreteTypeId::Extern)?;
    let ConcreteExternTypeLongId { extern_type_id, generic_args } = extern_ty.long(db);
    require(extern_type_id.name(db).long(db) == "BoundedInt")?;
    let [GenericArgumentId::Constant(min), GenericArgumentId::Constant(max)] = generic_args[..]
    else {
        return None;
    };
    let to_int = |id: ConstValueId<'db>| id.long(db).to_int().cloned();

    Some((to_int(min)?, to_int(max)?))
}

/// Information about various core types and traits.
#[derive(Debug, Eq, PartialEq, Hash, salsa::Update)]
pub struct CoreInfo<'db> {
    // Types.
    pub felt252: TypeId<'db>,
    pub u8: TypeId<'db>,
    pub u16: TypeId<'db>,
    pub u32: TypeId<'db>,
    pub u64: TypeId<'db>,
    pub u128: TypeId<'db>,
    pub u256: TypeId<'db>,
    pub i8: TypeId<'db>,
    pub i16: TypeId<'db>,
    pub i32: TypeId<'db>,
    pub i64: TypeId<'db>,
    pub i128: TypeId<'db>,
    pub class_hash: TypeId<'db>,
    pub contract_address: TypeId<'db>,
    // Traits.
    pub numeric_literal_trt: TraitId<'db>,
    pub string_literal_trt: TraitId<'db>,
    pub deref_trt: TraitId<'db>,
    pub deref_mut_trt: TraitId<'db>,
    pub index_trt: TraitId<'db>,
    pub index_view_trt: TraitId<'db>,
    pub copy_trt: TraitId<'db>,
    pub drop_trt: TraitId<'db>,
    pub destruct_trt: TraitId<'db>,
    pub panic_destruct_trt: TraitId<'db>,
    pub add_trt: TraitId<'db>,
    pub sub_trt: TraitId<'db>,
    pub mul_trt: TraitId<'db>,
    pub div_trt: TraitId<'db>,
    pub rem_trt: TraitId<'db>,
    pub div_rem_trt: TraitId<'db>,
    pub neg_trt: TraitId<'db>,
    pub add_assign_trt: TraitId<'db>,
    pub sub_assign_trt: TraitId<'db>,
    pub mul_assign_trt: TraitId<'db>,
    pub div_assign_trt: TraitId<'db>,
    pub rem_assign_trt: TraitId<'db>,
    pub bitnot_trt: TraitId<'db>,
    pub bitand_trt: TraitId<'db>,
    pub bitor_trt: TraitId<'db>,
    pub bitxor_trt: TraitId<'db>,
    pub not_trt: TraitId<'db>,
    pub partialeq_trt: TraitId<'db>,
    pub partialord_trt: TraitId<'db>,
    pub range_op_trt: TraitId<'db>,
    pub range_inclusive_op_trt: TraitId<'db>,
    pub into_iterator_trt: TraitId<'db>,
    pub iterator_trt: TraitId<'db>,
    pub fn_trt: TraitId<'db>,
    pub fn_once_trt: TraitId<'db>,
    pub type_eq_trt: TraitId<'db>,
    pub felt252_dict_value_trt: TraitId<'db>,
    pub box_trt: TraitId<'db>,
    // Trait fns.
    pub deref_fn: TraitFunctionId<'db>,
    pub deref_mut_fn: TraitFunctionId<'db>,
    pub destruct_fn: TraitFunctionId<'db>,
    pub panic_destruct_fn: TraitFunctionId<'db>,
    pub add_fn: TraitFunctionId<'db>,
    pub sub_fn: TraitFunctionId<'db>,
    pub mul_fn: TraitFunctionId<'db>,
    pub div_fn: TraitFunctionId<'db>,
    pub rem_fn: TraitFunctionId<'db>,
    pub div_rem_fn: TraitFunctionId<'db>,
    pub neg_fn: TraitFunctionId<'db>,
    pub add_assign_fn: TraitFunctionId<'db>,
    pub sub_assign_fn: TraitFunctionId<'db>,
    pub mul_assign_fn: TraitFunctionId<'db>,
    pub div_assign_fn: TraitFunctionId<'db>,
    pub rem_assign_fn: TraitFunctionId<'db>,
    pub bitnot_fn: TraitFunctionId<'db>,
    pub bitand_fn: TraitFunctionId<'db>,
    pub bitor_fn: TraitFunctionId<'db>,
    pub bitxor_fn: TraitFunctionId<'db>,
    pub not_fn: TraitFunctionId<'db>,
    pub eq_fn: TraitFunctionId<'db>,
    pub ne_fn: TraitFunctionId<'db>,
    pub lt_fn: TraitFunctionId<'db>,
    pub gt_fn: TraitFunctionId<'db>,
    pub le_fn: TraitFunctionId<'db>,
    pub ge_fn: TraitFunctionId<'db>,
    pub range_fn: TraitFunctionId<'db>,
    pub range_inclusive_fn: TraitFunctionId<'db>,
    pub into_iter_fn: TraitFunctionId<'db>,
    pub next_fn: TraitFunctionId<'db>,
    pub call_fn: TraitFunctionId<'db>,
    pub call_once_fn: TraitFunctionId<'db>,
    pub box_new_fn: TraitFunctionId<'db>,
    pub upcast_fn: GenericFunctionId<'db>,
    pub downcast_fn: GenericFunctionId<'db>,
    pub tuple_submodule: ModuleId<'db>,
    pub fixed_size_array_submodule: ModuleId<'db>,
    pub keyword_docs_submodule: ModuleId<'db>,
}
impl<'db> CoreInfo<'db> {
    fn new(db: &'db dyn Database) -> Self {
        let core = ModuleHelper::core(db);
        let integer: ModuleHelper<'db> = core.submodule("integer");
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
        let into_iterator_trt: TraitId<'db> =
            iter_traits.submodule("collect").trait_id("IntoIterator");
        let iterator_trt = iter_traits.submodule("iterator").trait_id("Iterator");
        let fn_module = ops.submodule("function");
        let fn_trt = fn_module.trait_id("Fn");
        let fn_once_trt = fn_module.trait_id("FnOnce");
        let box_module = core.submodule("box");
        let box_trt = box_module.trait_id("BoxTrait");
        let index_module = ops.submodule("index");
        let starknet = core.submodule("starknet");
        let bounded_int = core.submodule("internal").submodule("bounded_int");
        let trait_fn = |trait_id: TraitId<'db>, name: &'static str| {
            // TODO(eytan-starkware): Change &str to SmolStrId in trait_fn.
            db.trait_function_by_name(trait_id, SmolStrId::from(db, name)).unwrap().unwrap()
        };
        let tuple_submodule = core.submodule("tuple").id;
        let fixed_size_array_submodule = core.submodule("fixed_size_array").id;
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
            box_trt,
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
            box_new_fn: trait_fn(box_trt, "new"),
            upcast_fn: bounded_int.generic_function_id("upcast"),
            downcast_fn: bounded_int.generic_function_id("downcast"),
            tuple_submodule,
            fixed_size_array_submodule,
            keyword_docs_submodule: core.submodule("keyword_docs").id,
        }
    }
}

/// Implementation of [CorelibSemantic::core_info].
pub fn core_info(db: &dyn Database) -> Arc<CoreInfo<'_>> {
    CoreInfo::new(db).into()
}

/// Query implementation of [CorelibSemantic::core_info].
#[salsa::tracked]
pub fn core_info_tracked(db: &dyn Database) -> Arc<CoreInfo<'_>> {
    core_info(db)
}

/// Trait for corelib-related semantic queries.
pub trait CorelibSemantic<'db>: Database {
    fn core_crate(&'db self) -> CrateId<'db> {
        core_crate_tracked(self.as_dyn_database())
    }
    fn core_module(&'db self) -> ModuleId<'db> {
        core_module_tracked(self.as_dyn_database())
    }
    fn core_info(&'db self) -> Arc<CoreInfo<'db>> {
        core_info_tracked(self.as_dyn_database())
    }
}
impl<'db, T: Database + ?Sized> CorelibSemantic<'db> for T {}
