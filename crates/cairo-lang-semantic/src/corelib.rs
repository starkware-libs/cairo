use cairo_lang_defs::ids::{
    EnumId, GenericTypeId, ImplDefId, ModuleId, ModuleItemId, NamedLanguageElementId,
    TraitFunctionId, TraitId,
};
use cairo_lang_diagnostics::{Maybe, ToOption};
use cairo_lang_filesystem::ids::{CrateId, CrateLongId};
use cairo_lang_syntax::node::ast::{self, BinaryOperator, UnaryOperator};
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_syntax::node::Terminal;
use cairo_lang_utils::{
    extract_matches, require, try_extract_matches, Intern, LookupIntern, OptionFrom,
};
use num_bigint::BigInt;
use num_traits::{Num, Signed, ToPrimitive, Zero};
use smol_str::SmolStr;

use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnosticKind;
use crate::expr::compute::ComputationContext;
use crate::expr::inference::Inference;
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
    semantic, ConcreteEnumId, ConcreteFunction, ConcreteImplLongId, ConcreteTypeId,
    ConcreteVariant, Expr, ExprId, ExprTuple, FunctionId, FunctionLongId, GenericArgumentId,
    TypeId, TypeLongId,
};

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

pub fn core_crate(db: &dyn SemanticGroup) -> CrateId {
    CrateLongId::Real("core".into()).intern(db)
}

pub fn core_felt252_ty(db: &dyn SemanticGroup) -> TypeId {
    get_core_ty_by_name(db, "felt252".into(), vec![])
}

/// Returns the concrete type of a bounded int type with a given min and max.
pub fn bounded_int_ty(db: &dyn SemanticGroup, min: BigInt, max: BigInt) -> TypeId {
    let internal = core_submodule(db, "internal");
    let bounded_int = get_submodule(db, internal, "bounded_int")
        .expect("Could not find bounded_int submodule in corelib.");
    let size_ty = core_felt252_ty(db);
    let lower_id = ConstValue::Int(min, size_ty).intern(db);
    let upper_id = ConstValue::Int(max, size_ty).intern(db);
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
    get_core_ty_by_name(db, "Array".into(), vec![GenericArgumentId::Type(core_felt252_ty(db))])
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
pub fn jump_nz_zero_variant(db: &dyn SemanticGroup) -> ConcreteVariant {
    get_enum_concrete_variant(
        db,
        core_submodule(db, "zeroable"),
        "IsZeroResult",
        vec![GenericArgumentId::Type(core_felt252_ty(db))],
        "Zero",
    )
}

/// Generates a ConcreteVariant instance for `IsZeroResult::<felt252>::NonZero`.
pub fn jump_nz_nonzero_variant(db: &dyn SemanticGroup) -> ConcreteVariant {
    get_enum_concrete_variant(
        db,
        core_submodule(db, "zeroable"),
        "IsZeroResult",
        vec![GenericArgumentId::Type(core_felt252_ty(db))],
        "NonZero",
    )
}

/// Generates a ConcreteVariant instance for `Option::Some`.
pub fn option_some_variant(
    db: &dyn SemanticGroup,
    generic_arg: GenericArgumentId,
) -> ConcreteVariant {
    get_enum_concrete_variant(db, core_submodule(db, "option"), "Option", vec![generic_arg], "Some")
}

/// Generates a ConcreteVariant instance for `Option::None`.
pub fn option_none_variant(
    db: &dyn SemanticGroup,
    generic_arg: GenericArgumentId,
) -> ConcreteVariant {
    get_enum_concrete_variant(db, core_submodule(db, "option"), "Option", vec![generic_arg], "None")
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
        // TODO(yuval): for trait function default implementation, this may need to change.
        TypeLongId::TraitType(_) => {
            panic!("Trait types should only appear in traits, where there are no function bodies.")
        }
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
    let (trait_name, function_name) = match unary_op {
        UnaryOperator::Minus(_) => ("Neg", "neg"),
        UnaryOperator::Not(_) => ("Not", "not"),
        UnaryOperator::BitNot(_) => ("BitNot", "bitnot"),
        UnaryOperator::At(_) => unreachable!("@ is not an unary operator."),
        UnaryOperator::Desnap(_) => unreachable!("* is not an unary operator."),
    };
    Ok(Ok(get_core_trait_function_infer(
        db,
        inference,
        CoreTraitContext::TopLevel,
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
) -> Maybe<Result<(ConcreteTraitGenericFunctionId, bool), SemanticDiagnosticKind>> {
    let (trait_name, function_name, snapshot, context) = match binary_op {
        BinaryOperator::Plus(_) => ("Add", "add", false, CoreTraitContext::TopLevel),
        BinaryOperator::PlusEq(_) => ("AddAssign", "add_assign", false, CoreTraitContext::Ops),
        BinaryOperator::Minus(_) => ("Sub", "sub", false, CoreTraitContext::TopLevel),
        BinaryOperator::MinusEq(_) => ("SubAssign", "sub_assign", false, CoreTraitContext::Ops),
        BinaryOperator::Mul(_) => ("Mul", "mul", false, CoreTraitContext::TopLevel),
        BinaryOperator::MulEq(_) => ("MulAssign", "mul_assign", false, CoreTraitContext::Ops),
        BinaryOperator::Div(_) => ("Div", "div", false, CoreTraitContext::TopLevel),
        BinaryOperator::DivEq(_) => ("DivAssign", "div_assign", false, CoreTraitContext::Ops),
        BinaryOperator::Mod(_) => ("Rem", "rem", false, CoreTraitContext::TopLevel),
        BinaryOperator::ModEq(_) => ("RemAssign", "rem_assign", false, CoreTraitContext::Ops),
        BinaryOperator::EqEq(_) => ("PartialEq", "eq", true, CoreTraitContext::TopLevel),
        BinaryOperator::Neq(_) => ("PartialEq", "ne", true, CoreTraitContext::TopLevel),
        BinaryOperator::LE(_) => ("PartialOrd", "le", false, CoreTraitContext::TopLevel),
        BinaryOperator::GE(_) => ("PartialOrd", "ge", false, CoreTraitContext::TopLevel),
        BinaryOperator::LT(_) => ("PartialOrd", "lt", false, CoreTraitContext::TopLevel),
        BinaryOperator::GT(_) => ("PartialOrd", "gt", false, CoreTraitContext::TopLevel),
        BinaryOperator::And(_) => ("BitAnd", "bitand", false, CoreTraitContext::TopLevel),
        BinaryOperator::Or(_) => ("BitOr", "bitor", false, CoreTraitContext::TopLevel),
        BinaryOperator::Xor(_) => ("BitXor", "bitxor", false, CoreTraitContext::TopLevel),
        BinaryOperator::DotDot(_) => ("RangeOp", "range", false, CoreTraitContext::Ops),
        _ => return Ok(Err(SemanticDiagnosticKind::UnknownBinaryOperator)),
    };
    Ok(Ok((
        get_core_trait_function_infer(
            db,
            inference,
            context,
            trait_name.into(),
            function_name.into(),
            stable_ptr,
        ),
        snapshot,
    )))
}

pub fn felt252_eq(db: &dyn SemanticGroup) -> FunctionId {
    get_core_function_impl_method(db, "Felt252PartialEq".into(), "eq".into())
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
    get_core_concrete_trait(db, "Copy".into(), vec![GenericArgumentId::Type(ty)])
}

pub fn concrete_drop_trait(db: &dyn SemanticGroup, ty: TypeId) -> ConcreteTraitId {
    get_core_concrete_trait(db, "Drop".into(), vec![GenericArgumentId::Type(ty)])
}

pub fn concrete_destruct_trait(db: &dyn SemanticGroup, ty: TypeId) -> ConcreteTraitId {
    get_core_concrete_trait(db, "Destruct".into(), vec![GenericArgumentId::Type(ty)])
}

pub fn concrete_panic_destruct_trait(db: &dyn SemanticGroup, ty: TypeId) -> ConcreteTraitId {
    get_core_concrete_trait(db, "PanicDestruct".into(), vec![GenericArgumentId::Type(ty)])
}

pub fn concrete_iterator_trait(db: &dyn SemanticGroup, ty: TypeId) -> ConcreteTraitId {
    let trait_id = get_core_trait(db, CoreTraitContext::Iterator, "Iterator".into());
    semantic::ConcreteTraitLongId { trait_id, generic_args: vec![GenericArgumentId::Type(ty)] }
        .intern(db)
}

pub fn fn_once_trait(db: &dyn SemanticGroup) -> TraitId {
    get_core_trait(db, CoreTraitContext::Ops, "FnOnce".into())
}

pub fn copy_trait(db: &dyn SemanticGroup) -> TraitId {
    get_core_trait(db, CoreTraitContext::TopLevel, "Copy".into())
}

pub fn drop_trait(db: &dyn SemanticGroup) -> TraitId {
    get_core_trait(db, CoreTraitContext::TopLevel, "Drop".into())
}

pub fn deref_trait(db: &dyn SemanticGroup) -> TraitId {
    get_core_trait(db, CoreTraitContext::Ops, "Deref".into())
}

pub fn deref_mut_trait(db: &dyn SemanticGroup) -> TraitId {
    get_core_trait(db, CoreTraitContext::Ops, "DerefMut".into())
}

pub fn destruct_trait_fn(db: &dyn SemanticGroup) -> TraitFunctionId {
    get_core_trait_fn(db, CoreTraitContext::TopLevel, "Destruct".into(), "destruct".into())
}

pub fn panic_destruct_trait_fn(db: &dyn SemanticGroup) -> TraitFunctionId {
    get_core_trait_fn(
        db,
        CoreTraitContext::TopLevel,
        "PanicDestruct".into(),
        "panic_destruct".into(),
    )
}

pub fn into_iterator_trait(db: &dyn SemanticGroup) -> TraitId {
    get_core_trait(db, CoreTraitContext::Iterator, "IntoIterator".into())
}

pub fn numeric_literal_trait(db: &dyn SemanticGroup) -> TraitId {
    get_core_trait(db, CoreTraitContext::TopLevel, "NumericLiteral".into())
}

/// Given a core library trait name and its generic arguments, returns [ConcreteTraitId].
fn get_core_concrete_trait(
    db: &dyn SemanticGroup,
    name: SmolStr,
    generic_args: Vec<GenericArgumentId>,
) -> ConcreteTraitId {
    let trait_id = get_core_trait(db, CoreTraitContext::TopLevel, name);
    semantic::ConcreteTraitLongId { trait_id, generic_args }.intern(db)
}

/// The context for a core library trait.
pub enum CoreTraitContext {
    /// The top level core library context.
    TopLevel,
    /// The ops core library context.
    Ops,
    /// The iterator core library context.
    Iterator,
    /// The meta programming core library context.
    MetaProgramming,
}

/// Given a core library context and trait name, returns [TraitId].
pub fn get_core_trait(db: &dyn SemanticGroup, context: CoreTraitContext, name: SmolStr) -> TraitId {
    let base_module = match context {
        CoreTraitContext::TopLevel => db.core_module(),
        CoreTraitContext::Ops => core_submodule(db, "ops"),
        CoreTraitContext::Iterator => core_submodule(db, "iter"),
        CoreTraitContext::MetaProgramming => core_submodule(db, "metaprogramming"),
    };
    // This should not fail if the corelib is present.
    let item_id = db
        .module_item_by_name(base_module, name.clone())
        .unwrap_or_else(|_| {
            panic!(
                "Core module `{module}` failed to compile.",
                module = base_module.full_path(db.upcast())
            )
        })
        .unwrap_or_else(|| {
            panic!(
                "Core module `{module}` is missing an use item for trait `{name}`.",
                module = base_module.full_path(db.upcast()),
            )
        });
    match item_id {
        ModuleItemId::Trait(id) => id,
        ModuleItemId::Use(use_id) => {
            extract_matches!(
                db.use_resolved_item(use_id).unwrap_or_else(|_| panic!(
                    "Could not resolve core trait `{module}::{name}`.",
                    module = base_module.full_path(db.upcast()),
                )),
                ResolvedGenericItem::Trait
            )
        }
        _ => panic!("Expecting only traits, or uses pointing to traits."),
    }
}

/// Given a core library context, trait name and fn name, returns [TraitFunctionId].
fn get_core_trait_fn(
    db: &dyn SemanticGroup,
    context: CoreTraitContext,
    trait_name: SmolStr,
    fn_name: SmolStr,
) -> TraitFunctionId {
    db.trait_function_by_name(get_core_trait(db, context, trait_name), fn_name).unwrap().unwrap()
}

/// Retrieves a trait function from the core library with type variables as generic arguments, to
/// be inferred later.
fn get_core_trait_function_infer(
    db: &dyn SemanticGroup,
    inference: &mut Inference<'_>,
    context: CoreTraitContext,
    trait_name: SmolStr,
    function_name: SmolStr,
    stable_ptr: SyntaxStablePtrId,
) -> ConcreteTraitGenericFunctionId {
    let trait_id = get_core_trait(db, context, trait_name.clone());
    let generic_params = db.trait_generic_params(trait_id).unwrap();
    let generic_args = generic_params
        .iter()
        .map(|_| GenericArgumentId::Type(inference.new_type_var(Some(stable_ptr))))
        .collect();
    let concrete_trait_id = semantic::ConcreteTraitLongId { trait_id, generic_args }.intern(db);
    let trait_function = db
        .trait_function_by_name(trait_id, function_name.clone())
        .unwrap()
        .unwrap_or_else(move || panic!("Missing function '{function_name}' in '{trait_name}'."));
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
    if ty == get_core_ty_by_name(db, "u8".into(), vec![]) {
        Some(get_function_id(db, core_submodule(db, "integer"), "u8_to_felt252".into(), vec![]))
    } else if ty == get_core_ty_by_name(db, "u16".into(), vec![]) {
        Some(get_function_id(db, core_submodule(db, "integer"), "u16_to_felt252".into(), vec![]))
    } else if ty == get_core_ty_by_name(db, "u32".into(), vec![]) {
        Some(get_function_id(db, core_submodule(db, "integer"), "u32_to_felt252".into(), vec![]))
    } else if ty == get_core_ty_by_name(db, "u64".into(), vec![]) {
        Some(get_function_id(db, core_submodule(db, "integer"), "u64_to_felt252".into(), vec![]))
    } else if ty == get_core_ty_by_name(db, "u128".into(), vec![]) {
        Some(get_function_id(db, core_submodule(db, "integer"), "u128_to_felt252".into(), vec![]))
    } else if ty == get_core_ty_by_name(db, "i8".into(), vec![]) {
        Some(get_function_id(db, core_submodule(db, "integer"), "i8_to_felt252".into(), vec![]))
    } else if ty == get_core_ty_by_name(db, "i16".into(), vec![]) {
        Some(get_function_id(db, core_submodule(db, "integer"), "i16_to_felt252".into(), vec![]))
    } else if ty == get_core_ty_by_name(db, "i32".into(), vec![]) {
        Some(get_function_id(db, core_submodule(db, "integer"), "i32_to_felt252".into(), vec![]))
    } else if ty == get_core_ty_by_name(db, "i64".into(), vec![]) {
        Some(get_function_id(db, core_submodule(db, "integer"), "i64_to_felt252".into(), vec![]))
    } else if ty == get_core_ty_by_name(db, "i128".into(), vec![]) {
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
    value: BigInt,
) -> Result<(), LiteralError> {
    if let Some(nz_wrapped_ty) = try_extract_nz_wrapped_type(db, ty) {
        return if value.is_zero() {
            Err(LiteralError::OutOfRange(ty))
        } else {
            validate_literal(db, nz_wrapped_ty, value)
        };
    }
    let is_out_of_range = if let Some((min, max)) = try_extract_bounded_int_type_ranges(db, ty) {
        value < min || value > max
    } else if ty == core_felt252_ty(db) {
        value.abs()
            > BigInt::from_str_radix(
                "800000000000011000000000000000000000000000000000000000000000000",
                16,
            )
            .unwrap()
    } else if ty == get_core_ty_by_name(db, "u8".into(), vec![]) {
        value.to_u8().is_none()
    } else if ty == get_core_ty_by_name(db, "u16".into(), vec![]) {
        value.to_u16().is_none()
    } else if ty == get_core_ty_by_name(db, "u32".into(), vec![]) {
        value.to_u32().is_none()
    } else if ty == get_core_ty_by_name(db, "u64".into(), vec![]) {
        value.to_u64().is_none()
    } else if ty == get_core_ty_by_name(db, "u128".into(), vec![]) {
        value.to_u128().is_none()
    } else if ty == get_core_ty_by_name(db, "u256".into(), vec![]) {
        value.is_negative() || value.bits() > 256
    } else if ty == get_core_ty_by_name(db, "i8".into(), vec![]) {
        value.to_i8().is_none()
    } else if ty == get_core_ty_by_name(db, "i16".into(), vec![]) {
        value.to_i16().is_none()
    } else if ty == get_core_ty_by_name(db, "i32".into(), vec![]) {
        value.to_i32().is_none()
    } else if ty == get_core_ty_by_name(db, "i64".into(), vec![]) {
        value.to_i64().is_none()
    } else if ty == get_core_ty_by_name(db, "i128".into(), vec![]) {
        value.to_i128().is_none()
    } else {
        return Err(LiteralError::InvalidTypeForLiteral(ty));
    };
    if is_out_of_range { Err(LiteralError::OutOfRange(ty)) } else { Ok(()) }
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
