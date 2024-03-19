use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::NamedLanguageElementId;
use cairo_lang_diagnostics::Maybe;
use cairo_lang_sierra::extensions::const_type::{
    ConstAsBoxLibfunc, ConstAsImmediateLibfunc, ConstType,
};
use cairo_lang_sierra::extensions::core::CoreLibfunc;
use cairo_lang_sierra::extensions::lib_func::LibfuncSignature;
use cairo_lang_sierra::extensions::snapshot::SnapshotType;
use cairo_lang_sierra::extensions::{
    ExtensionError, GenericLibfuncEx, NamedLibfunc, NamedType, SpecializationError,
};
use cairo_lang_sierra::ids::{ConcreteLibfuncId, GenericLibfuncId};
use cairo_lang_sierra::program::{self, GenericArg};
use cairo_lang_utils::extract_matches;
use semantic::items::constant::ConstValue;
use semantic::items::functions::GenericFunctionId;
use smol_str::SmolStr;
use {cairo_lang_defs as defs, cairo_lang_lowering as lowering, cairo_lang_semantic as semantic};

use crate::db::{SierraGenGroup, SierraGeneratorTypeLongId};
use crate::pre_sierra;
use crate::replace_ids::{DebugReplacer, SierraIdReplacer};
use crate::specialization_context::SierraSignatureSpecializationContext;

pub fn simple_basic_statement(
    libfunc_id: ConcreteLibfuncId,
    args: &[cairo_lang_sierra::ids::VarId],
    results: &[cairo_lang_sierra::ids::VarId],
) -> pre_sierra::Statement {
    pre_sierra::Statement::Sierra(program::GenStatement::Invocation(program::GenInvocation {
        libfunc_id,
        args: args.into(),
        branches: vec![program::GenBranchInfo {
            target: program::GenBranchTarget::Fallthrough,
            results: results.into(),
        }],
    }))
}

pub fn simple_statement(
    libfunc_id: ConcreteLibfuncId,
    args: &[cairo_lang_sierra::ids::VarId],
    results: &[cairo_lang_sierra::ids::VarId],
) -> pre_sierra::StatementWithLocation {
    simple_basic_statement(libfunc_id, args, results).into_statement_without_location()
}

pub fn jump_statement(
    jump: ConcreteLibfuncId,
    label: pre_sierra::LabelId,
) -> pre_sierra::Statement {
    pre_sierra::Statement::Sierra(program::GenStatement::Invocation(program::GenInvocation {
        libfunc_id: jump,
        args: vec![],
        branches: vec![program::GenBranchInfo {
            target: program::GenBranchTarget::Statement(label),
            results: vec![],
        }],
    }))
}

pub fn return_statement(res: Vec<cairo_lang_sierra::ids::VarId>) -> pre_sierra::Statement {
    pre_sierra::Statement::Sierra(program::GenStatement::Return(res))
}

pub fn get_libfunc_id_with_generic_arg(
    db: &dyn SierraGenGroup,
    name: impl Into<SmolStr>,
    ty: cairo_lang_sierra::ids::ConcreteTypeId,
) -> cairo_lang_sierra::ids::ConcreteLibfuncId {
    db.intern_concrete_lib_func(cairo_lang_sierra::program::ConcreteLibfuncLongId {
        generic_id: cairo_lang_sierra::ids::GenericLibfuncId::from_string(name),
        generic_args: vec![GenericArg::Type(ty)],
    })
}

/// Returns the [cairo_lang_sierra::program::ConcreteLibfuncLongId] associated with `store_temp`.
pub fn store_temp_libfunc_id(
    db: &dyn SierraGenGroup,
    ty: cairo_lang_sierra::ids::ConcreteTypeId,
) -> cairo_lang_sierra::ids::ConcreteLibfuncId {
    get_libfunc_id_with_generic_arg(db, "store_temp", ty)
}

/// Returns the [cairo_lang_sierra::program::ConcreteLibfuncLongId] associated with `store_local`.
pub fn store_local_libfunc_id(
    db: &dyn SierraGenGroup,
    ty: cairo_lang_sierra::ids::ConcreteTypeId,
) -> cairo_lang_sierra::ids::ConcreteLibfuncId {
    get_libfunc_id_with_generic_arg(db, "store_local", ty)
}

pub fn struct_construct_libfunc_id(
    db: &dyn SierraGenGroup,
    ty: cairo_lang_sierra::ids::ConcreteTypeId,
) -> cairo_lang_sierra::ids::ConcreteLibfuncId {
    get_libfunc_id_with_generic_arg(db, "struct_construct", ty)
}

pub fn struct_deconstruct_libfunc_id(
    db: &dyn SierraGenGroup,
    ty: cairo_lang_sierra::ids::ConcreteTypeId,
) -> Maybe<cairo_lang_sierra::ids::ConcreteLibfuncId> {
    let long_id = &db.get_type_info(ty.clone())?.long_id;
    let is_snapshot = long_id.generic_id == SnapshotType::id();
    Ok(if is_snapshot {
        let concrete_enum_type =
            extract_matches!(&long_id.generic_args[0], GenericArg::Type).clone();
        get_libfunc_id_with_generic_arg(db, "struct_snapshot_deconstruct", concrete_enum_type)
    } else {
        get_libfunc_id_with_generic_arg(db, "struct_deconstruct", ty)
    })
}

pub fn enum_init_libfunc_id(
    db: &dyn SierraGenGroup,
    ty: cairo_lang_sierra::ids::ConcreteTypeId,
    variant_idx: usize,
) -> cairo_lang_sierra::ids::ConcreteLibfuncId {
    db.intern_concrete_lib_func(cairo_lang_sierra::program::ConcreteLibfuncLongId {
        generic_id: cairo_lang_sierra::ids::GenericLibfuncId::from_string("enum_init"),
        generic_args: vec![GenericArg::Type(ty), GenericArg::Value(variant_idx.into())],
    })
}

/// Returns the [cairo_lang_sierra::program::ConcreteLibfuncLongId] associated with `snapshot_take`.
pub fn snapshot_take_libfunc_id(
    db: &dyn SierraGenGroup,
    ty: cairo_lang_sierra::ids::ConcreteTypeId,
) -> cairo_lang_sierra::ids::ConcreteLibfuncId {
    db.intern_concrete_lib_func(cairo_lang_sierra::program::ConcreteLibfuncLongId {
        generic_id: cairo_lang_sierra::ids::GenericLibfuncId::from_string("snapshot_take"),
        generic_args: vec![GenericArg::Type(ty)],
    })
}

/// Returns the [cairo_lang_sierra::program::ConcreteLibfuncLongId] associated with `rename`.
pub fn rename_libfunc_id(
    db: &dyn SierraGenGroup,
    ty: cairo_lang_sierra::ids::ConcreteTypeId,
) -> cairo_lang_sierra::ids::ConcreteLibfuncId {
    db.intern_concrete_lib_func(cairo_lang_sierra::program::ConcreteLibfuncLongId {
        generic_id: cairo_lang_sierra::ids::GenericLibfuncId::from_string("rename"),
        generic_args: vec![GenericArg::Type(ty)],
    })
}

fn get_libfunc_id_without_generics(
    db: &dyn SierraGenGroup,
    name: impl Into<SmolStr>,
) -> cairo_lang_sierra::ids::ConcreteLibfuncId {
    db.intern_concrete_lib_func(cairo_lang_sierra::program::ConcreteLibfuncLongId {
        generic_id: cairo_lang_sierra::ids::GenericLibfuncId::from_string(name),
        generic_args: vec![],
    })
}

pub fn const_libfunc_id_by_type(
    db: &dyn SierraGenGroup,
    ty: semantic::TypeId,
    value: &ConstValue,
) -> cairo_lang_sierra::ids::ConcreteLibfuncId {
    if let ConstValue::Boxed(ty, inner_value) = value {
        db.intern_concrete_lib_func(cairo_lang_sierra::program::ConcreteLibfuncLongId {
            generic_id: cairo_lang_sierra::ids::GenericLibfuncId::from_string(
                ConstAsBoxLibfunc::STR_ID,
            ),
            generic_args: vec![
                GenericArg::Type(const_type_id(db, *ty, inner_value)),
                GenericArg::Value(0.into()),
            ],
        })
    } else {
        db.intern_concrete_lib_func(cairo_lang_sierra::program::ConcreteLibfuncLongId {
            generic_id: cairo_lang_sierra::ids::GenericLibfuncId::from_string(
                ConstAsImmediateLibfunc::STR_ID,
            ),
            generic_args: vec![GenericArg::Type(const_type_id(db, ty, value))],
        })
    }
}

/// Returns the [cairo_lang_sierra::ids::ConcreteTypeId] for the given `ty` and `value`.
fn const_type_id(
    db: &dyn SierraGenGroup,
    ty: semantic::TypeId,
    value: &ConstValue,
) -> cairo_lang_sierra::ids::ConcreteTypeId {
    let first_arg = GenericArg::Type(db.get_concrete_type_id(ty).unwrap());
    db.intern_concrete_type(SierraGeneratorTypeLongId::Regular(
        cairo_lang_sierra::program::ConcreteTypeLongId {
            generic_id: ConstType::ID,
            generic_args: match value {
                ConstValue::Int(v) => vec![first_arg, GenericArg::Value(v.clone())],
                ConstValue::Struct(tys) => {
                    let mut args = vec![first_arg];
                    for (ty, value) in tys {
                        args.push(GenericArg::Type(const_type_id(db, *ty, value)));
                    }
                    args
                }
                ConstValue::Enum(variant, inner) => {
                    vec![
                        first_arg,
                        GenericArg::Value(variant.idx.into()),
                        GenericArg::Type(const_type_id(db, variant.ty, inner)),
                    ]
                }
                ConstValue::NonZero(ty, value) => {
                    vec![first_arg, GenericArg::Type(const_type_id(db, *ty, value))]
                }
                ConstValue::Boxed(_, _) => {
                    unreachable!("Should be handled by `const_libfunc_id_by_type`.")
                }
                ConstValue::Generic(_) | ConstValue::Var(_) | ConstValue::Missing(_) => {
                    unreachable!("Should be caught by the lowering.")
                }
            },
        }
        .into(),
    ))
}

pub fn match_enum_libfunc_id(
    db: &dyn SierraGenGroup,
    ty: cairo_lang_sierra::ids::ConcreteTypeId,
) -> Maybe<cairo_lang_sierra::ids::ConcreteLibfuncId> {
    let long_id = &db.get_type_info(ty.clone())?.long_id;
    let is_snapshot = long_id.generic_id == SnapshotType::id();
    Ok(if is_snapshot {
        let concrete_enum_type =
            extract_matches!(&long_id.generic_args[0], GenericArg::Type).clone();
        get_libfunc_id_with_generic_arg(db, "enum_snapshot_match", concrete_enum_type)
    } else {
        get_libfunc_id_with_generic_arg(db, "enum_match", ty)
    })
}

pub fn enum_from_bounded_int_libfunc_id(
    db: &dyn SierraGenGroup,
    ty: cairo_lang_sierra::ids::ConcreteTypeId,
) -> cairo_lang_sierra::ids::ConcreteLibfuncId {
    get_libfunc_id_with_generic_arg(db, "enum_from_bounded_int", ty)
}

pub fn drop_libfunc_id(
    db: &dyn SierraGenGroup,
    ty: cairo_lang_sierra::ids::ConcreteTypeId,
) -> cairo_lang_sierra::ids::ConcreteLibfuncId {
    get_libfunc_id_with_generic_arg(db, "drop", ty)
}

pub fn dup_libfunc_id(
    db: &dyn SierraGenGroup,
    ty: cairo_lang_sierra::ids::ConcreteTypeId,
) -> cairo_lang_sierra::ids::ConcreteLibfuncId {
    get_libfunc_id_with_generic_arg(db, "dup", ty)
}

pub fn branch_align_libfunc_id(
    db: &dyn SierraGenGroup,
) -> cairo_lang_sierra::ids::ConcreteLibfuncId {
    get_libfunc_id_without_generics(db, "branch_align")
}

pub fn jump_libfunc_id(db: &dyn SierraGenGroup) -> cairo_lang_sierra::ids::ConcreteLibfuncId {
    get_libfunc_id_without_generics(db, "jump")
}

pub fn revoke_ap_tracking_libfunc_id(
    db: &dyn SierraGenGroup,
) -> cairo_lang_sierra::ids::ConcreteLibfuncId {
    get_libfunc_id_without_generics(db, "revoke_ap_tracking")
}

pub fn enable_ap_tracking_libfunc_id(
    db: &dyn SierraGenGroup,
) -> cairo_lang_sierra::ids::ConcreteLibfuncId {
    get_libfunc_id_without_generics(db, "enable_ap_tracking")
}

pub fn disable_ap_tracking_libfunc_id(
    db: &dyn SierraGenGroup,
) -> cairo_lang_sierra::ids::ConcreteLibfuncId {
    get_libfunc_id_without_generics(db, "disable_ap_tracking")
}

pub fn alloc_local_libfunc_id(
    db: &dyn SierraGenGroup,
    ty: cairo_lang_sierra::ids::ConcreteTypeId,
) -> cairo_lang_sierra::ids::ConcreteLibfuncId {
    get_libfunc_id_with_generic_arg(db, "alloc_local", ty)
}

pub fn finalize_locals_libfunc_id(
    db: &dyn SierraGenGroup,
) -> cairo_lang_sierra::ids::ConcreteLibfuncId {
    get_libfunc_id_without_generics(db, "finalize_locals")
}

/// Returns the [LibfuncSignature] of the given function.
pub fn get_libfunc_signature(
    db: &dyn SierraGenGroup,
    concrete_lib_func_id: ConcreteLibfuncId,
) -> LibfuncSignature {
    let libfunc_long_id = db.lookup_intern_concrete_lib_func(concrete_lib_func_id.clone());
    CoreLibfunc::specialize_signature_by_id(
        &SierraSignatureSpecializationContext(db),
        &libfunc_long_id.generic_id,
        &libfunc_long_id.generic_args,
    )
    .unwrap_or_else(|err| {
        if let ExtensionError::LibfuncSpecialization {
            error: SpecializationError::MissingFunction(function),
            ..
        } = err
        {
            let function = db.lookup_intern_sierra_function(function);
            panic!("Missing function {:?}", function.debug(db));
        }
        // If panic happens here, make sure the specified libfunc name is in one of the STR_IDs of
        // the libfuncs in the [`CoreLibfunc`] structured enum.
        panic!(
            "Failed to specialize: `{}`. Error: {err}",
            DebugReplacer { db }.replace_libfunc_id(&concrete_lib_func_id)
        )
    })
}

/// Returns the [ConcreteLibfuncId] for calling a user-defined function.
pub fn function_call_libfunc_id(
    db: &dyn SierraGenGroup,
    func: lowering::ids::FunctionId,
) -> ConcreteLibfuncId {
    db.intern_concrete_lib_func(cairo_lang_sierra::program::ConcreteLibfuncLongId {
        generic_id: GenericLibfuncId::from_string("function_call"),
        generic_args: vec![GenericArg::UserFunc(db.intern_sierra_function(func))],
    })
}

/// Returns the [ConcreteLibfuncId] for calling a user-defined function, given a coupon for that
/// function.
pub fn coupon_call_libfunc_id(
    db: &dyn SierraGenGroup,
    func: lowering::ids::FunctionId,
) -> ConcreteLibfuncId {
    db.intern_concrete_lib_func(cairo_lang_sierra::program::ConcreteLibfuncLongId {
        generic_id: GenericLibfuncId::from_string("coupon_call"),
        generic_args: vec![GenericArg::UserFunc(db.intern_sierra_function(func))],
    })
}

/// Returns the [ConcreteLibfuncId] used for calling a libfunc.
pub fn generic_libfunc_id(
    db: &dyn SierraGenGroup,
    extern_id: defs::ids::ExternFunctionId,
    generic_args: Vec<GenericArg>,
) -> ConcreteLibfuncId {
    db.intern_concrete_lib_func(cairo_lang_sierra::program::ConcreteLibfuncLongId {
        generic_id: GenericLibfuncId::from_string(extern_id.name(db.upcast())),
        generic_args,
    })
}

/// Returns the [ConcreteLibfuncId] used for calling a function (either user-defined or libfunc).
pub fn get_concrete_libfunc_id(
    db: &dyn SierraGenGroup,
    function: lowering::ids::FunctionId,
    with_coupon: bool,
) -> (Option<lowering::ids::ConcreteFunctionWithBodyId>, ConcreteLibfuncId) {
    // Check if this is a user-defined function or a libfunc.
    if let Some(body) = function.body(db.upcast()).expect("No diagnostics at this stage.") {
        if with_coupon {
            return (Some(body), coupon_call_libfunc_id(db, function));
        } else {
            return (Some(body), function_call_libfunc_id(db, function));
        }
    }

    assert!(!with_coupon, "Coupon cannot be used with extern functions.");

    let semantic =
        extract_matches!(function.lookup(db.upcast()), lowering::ids::FunctionLongId::Semantic);
    let concrete_function = db.lookup_intern_function(semantic).function;
    let extern_id = extract_matches!(concrete_function.generic_function, GenericFunctionId::Extern);

    let mut generic_args = vec![];
    let mut seen_impl_generic = false;
    for generic_arg in &concrete_function.generic_args {
        match generic_arg {
            semantic::GenericArgumentId::Type(ty) => {
                // TODO(lior): How should the following unwrap() be handled?
                assert!(!seen_impl_generic, "Impl generics must be last.");
                generic_args.push(GenericArg::Type(db.get_concrete_type_id(*ty).unwrap()))
            }
            semantic::GenericArgumentId::Constant(value_id) => {
                assert!(!seen_impl_generic, "Impl generics must be last.");
                generic_args.push(GenericArg::Value(extract_matches!(
                    db.lookup_intern_const_value(*value_id),
                    ConstValue::Int,
                    "Only integer constants are supported."
                )))
            }
            semantic::GenericArgumentId::Impl(_) => {
                // Impl generics are ignored, as they do not exist in Sierra and are used only in
                // high-level code.

                seen_impl_generic = true;
            }
            semantic::GenericArgumentId::NegImpl => {
                panic!("Extern function with neg impl generics are not supported.")
            }
        };
    }

    (None, generic_libfunc_id(db, extern_id, generic_args))
}
