use cairo_lang_defs::ids::{
    EnumId, GenericTypeId, ImplDefId, ModuleId, ModuleItemId, NamedLanguageElementId, TraitId,
};
use cairo_lang_diagnostics::{Maybe, ToOption};
use cairo_lang_filesystem::ids::{CrateId, CrateLongId};
use cairo_lang_syntax::node::ast::{self, BinaryOperator, UnaryOperator};
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_syntax::node::Terminal;
use cairo_lang_utils::{extract_matches, try_extract_matches, OptionFrom};
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
use crate::items::imp::ImplId;
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
    db.intern_crate(CrateLongId::Real("core".into()))
}

pub fn core_felt252_ty(db: &dyn SemanticGroup) -> TypeId {
    get_core_ty_by_name(db, "felt252".into(), vec![])
}

/// Returns the concrete type of a bounded int type with a given min and max.
pub fn bounded_int_ty(db: &dyn SemanticGroup, min: BigInt, max: BigInt) -> TypeId {
    let internal = core_submodule(db, "internal");
    let lower_id = db.intern_const_value(ConstValue::Int(min));
    let upper_id = db.intern_const_value(ConstValue::Int(max));
    try_get_ty_by_name(
        db,
        internal,
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
    let concrete_ty = extract_matches!(db.lookup_intern_type(ty), TypeLongId::Concrete);
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
        | TypeLongId::Coupon(_)
        | TypeLongId::Missing(_)
        | TypeLongId::FixedSizeArray { .. } => None,
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
    let (trait_name, function_name, snapshot) = match binary_op {
        BinaryOperator::Plus(_) => ("Add", "add", false),
        BinaryOperator::PlusEq(_) => ("AddEq", "add_eq", false),
        BinaryOperator::Minus(_) => ("Sub", "sub", false),
        BinaryOperator::MinusEq(_) => ("SubEq", "sub_eq", false),
        BinaryOperator::Mul(_) => ("Mul", "mul", false),
        BinaryOperator::MulEq(_) => ("MulEq", "mul_eq", false),
        BinaryOperator::Div(_) => ("Div", "div", false),
        BinaryOperator::DivEq(_) => ("DivEq", "div_eq", false),
        BinaryOperator::Mod(_) => ("Rem", "rem", false),
        BinaryOperator::ModEq(_) => ("RemEq", "rem_eq", false),
        BinaryOperator::EqEq(_) => ("PartialEq", "eq", true),
        BinaryOperator::Neq(_) => ("PartialEq", "ne", true),
        BinaryOperator::LE(_) => ("PartialOrd", "le", false),
        BinaryOperator::GE(_) => ("PartialOrd", "ge", false),
        BinaryOperator::LT(_) => ("PartialOrd", "lt", false),
        BinaryOperator::GT(_) => ("PartialOrd", "gt", false),
        BinaryOperator::And(_) => ("BitAnd", "bitand", false),
        BinaryOperator::Or(_) => ("BitOr", "bitor", false),
        BinaryOperator::Xor(_) => ("BitXor", "bitxor", false),
        _ => return Ok(Err(SemanticDiagnosticKind::UnknownBinaryOperator)),
    };
    Ok(Ok((
        get_core_trait_function_infer(
            db,
            inference,
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
    let impl_id = ImplId::Concrete(
        db.intern_concrete_impl(ConcreteImplLongId { impl_def_id, generic_args: vec![] }),
    );
    let concrete_trait_id = db.impl_concrete_trait(impl_id).unwrap();
    let function = db
        .trait_functions(concrete_trait_id.trait_id(db))
        .ok()
        .and_then(|functions| functions.get(&method_name).cloned())
        .unwrap_or_else(|| {
            panic!("no {method_name} in {}.", concrete_trait_id.trait_id(db).name(db.upcast()))
        });
    db.intern_function(FunctionLongId {
        function: ConcreteFunction {
            generic_function: GenericFunctionId::Impl(ImplGenericFunctionId { impl_id, function }),
            generic_args: vec![],
        },
    })
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

    db.intern_function(FunctionLongId {
        function: ConcreteFunction { generic_function, generic_args },
    })
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

pub fn copy_trait(db: &dyn SemanticGroup) -> TraitId {
    get_core_trait(db, "Copy".into())
}

pub fn drop_trait(db: &dyn SemanticGroup) -> TraitId {
    get_core_trait(db, "Drop".into())
}

pub fn destruct_trait(db: &dyn SemanticGroup) -> TraitId {
    get_core_trait(db, "Destruct".into())
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
pub fn get_core_trait(db: &dyn SemanticGroup, name: SmolStr) -> TraitId {
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
) -> ConcreteTraitGenericFunctionId {
    let trait_id = get_core_trait(db, trait_name);
    let generic_params = db.trait_generic_params(trait_id).unwrap();
    let generic_args = generic_params
        .iter()
        .map(|_| GenericArgumentId::Type(inference.new_type_var(Some(stable_ptr))))
        .collect();
    let concrete_trait_id =
        db.intern_concrete_trait(semantic::ConcreteTraitLongId { trait_id, generic_args });
    let trait_function = db.trait_function_by_name(trait_id, function_name).unwrap().unwrap();
    db.intern_concrete_trait_function(ConcreteTraitGenericFunctionLongId::new(
        db,
        concrete_trait_id,
        trait_function,
    ))
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
    let is_out_of_range = if ty == core_felt252_ty(db) {
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
    let concrete_ty = try_extract_matches!(db.lookup_intern_type(ty), TypeLongId::Concrete)?;
    let extern_ty = try_extract_matches!(concrete_ty, ConcreteTypeId::Extern)?;
    let ConcreteExternTypeLongId { extern_type_id, generic_args } =
        db.lookup_intern_concrete_extern_type(extern_ty);
    let [GenericArgumentId::Type(inner)] = generic_args[..] else { return None };
    (extern_type_id.name(db.upcast()) == "NonZero").then_some(inner)
}
