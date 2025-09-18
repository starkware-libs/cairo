use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::NamedLanguageElementId;
use cairo_lang_diagnostics::Maybe;
use cairo_lang_filesystem::ids::Tracked;
use cairo_lang_semantic::items::constant::ConstValueId;
use cairo_lang_sierra::extensions::const_type::{
    ConstAsBoxLibfunc, ConstAsImmediateLibfunc, ConstType,
};
use cairo_lang_sierra::extensions::core::CoreLibfunc;
use cairo_lang_sierra::extensions::lib_func::LibfuncSignature;
use cairo_lang_sierra::extensions::snapshot::SnapshotType;
use cairo_lang_sierra::extensions::{
    ExtensionError, GenericLibfuncEx, NamedLibfunc, NamedType, SpecializationError,
};
use cairo_lang_sierra::ids::{ConcreteLibfuncId, ConcreteTypeId, GenericLibfuncId};
use cairo_lang_sierra::program::{self, FunctionSignature, GenericArg};
use cairo_lang_utils::extract_matches;
use salsa::Database;
use semantic::items::constant::ConstValue;
use semantic::items::functions::GenericFunctionId;
use smol_str::SmolStr;
use {cairo_lang_defs as defs, cairo_lang_lowering as lowering, cairo_lang_semantic as semantic};

use crate::db::{SierraGenGroup, SierraGeneratorTypeLongId};
use crate::pre_sierra;
use crate::replace_ids::{DebugReplacer, SierraIdReplacer};
use crate::specialization_context::SierraSignatureSpecializationContext;

pub fn simple_basic_statement<'db>(
    libfunc_id: ConcreteLibfuncId,
    args: &[cairo_lang_sierra::ids::VarId],
    results: &[cairo_lang_sierra::ids::VarId],
) -> pre_sierra::Statement<'db> {
    pre_sierra::Statement::Sierra(program::GenStatement::Invocation(program::GenInvocation {
        libfunc_id,
        args: args.into(),
        branches: vec![program::GenBranchInfo {
            target: program::GenBranchTarget::Fallthrough,
            results: results.into(),
        }],
    }))
}

pub fn simple_statement<'db>(
    libfunc_id: ConcreteLibfuncId,
    args: &[cairo_lang_sierra::ids::VarId],
    results: &[cairo_lang_sierra::ids::VarId],
) -> pre_sierra::StatementWithLocation<'db> {
    simple_basic_statement(libfunc_id, args, results).into_statement_without_location()
}

pub fn jump_statement(
    jump: ConcreteLibfuncId,
    label: pre_sierra::LabelId<'_>,
) -> pre_sierra::Statement<'_> {
    pre_sierra::Statement::Sierra(program::GenStatement::Invocation(program::GenInvocation {
        libfunc_id: jump,
        args: vec![],
        branches: vec![program::GenBranchInfo {
            target: program::GenBranchTarget::Statement(label),
            results: vec![],
        }],
    }))
}

pub fn return_statement<'db>(
    res: Vec<cairo_lang_sierra::ids::VarId>,
) -> pre_sierra::Statement<'db> {
    pre_sierra::Statement::Sierra(program::GenStatement::Return(res))
}

pub fn get_libfunc_id_with_generic_arg(
    db: &dyn Database,
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
    db: &dyn Database,
    ty: cairo_lang_sierra::ids::ConcreteTypeId,
) -> cairo_lang_sierra::ids::ConcreteLibfuncId {
    get_libfunc_id_with_generic_arg(db, "store_temp", ty)
}

/// Returns the [cairo_lang_sierra::program::ConcreteLibfuncLongId] associated with `store_local`.
pub fn store_local_libfunc_id(
    db: &dyn Database,
    ty: cairo_lang_sierra::ids::ConcreteTypeId,
) -> cairo_lang_sierra::ids::ConcreteLibfuncId {
    get_libfunc_id_with_generic_arg(db, "store_local", ty)
}

pub fn struct_construct_libfunc_id(
    db: &dyn Database,
    ty: cairo_lang_sierra::ids::ConcreteTypeId,
) -> cairo_lang_sierra::ids::ConcreteLibfuncId {
    get_libfunc_id_with_generic_arg(db, "struct_construct", ty)
}

pub fn struct_deconstruct_libfunc_id(
    db: &dyn Database,
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
    db: &dyn Database,
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
    db: &dyn Database,
    ty: cairo_lang_sierra::ids::ConcreteTypeId,
) -> cairo_lang_sierra::ids::ConcreteLibfuncId {
    db.intern_concrete_lib_func(cairo_lang_sierra::program::ConcreteLibfuncLongId {
        generic_id: cairo_lang_sierra::ids::GenericLibfuncId::from_string("snapshot_take"),
        generic_args: vec![GenericArg::Type(ty)],
    })
}

/// Returns the [cairo_lang_sierra::program::ConcreteLibfuncLongId] associated with `rename`.
pub fn rename_libfunc_id(
    db: &dyn Database,
    ty: cairo_lang_sierra::ids::ConcreteTypeId,
) -> cairo_lang_sierra::ids::ConcreteLibfuncId {
    db.intern_concrete_lib_func(cairo_lang_sierra::program::ConcreteLibfuncLongId {
        generic_id: cairo_lang_sierra::ids::GenericLibfuncId::from_string("rename"),
        generic_args: vec![GenericArg::Type(ty)],
    })
}

fn get_libfunc_id_without_generics(
    db: &dyn Database,
    name: impl Into<SmolStr>,
) -> cairo_lang_sierra::ids::ConcreteLibfuncId {
    db.intern_concrete_lib_func(cairo_lang_sierra::program::ConcreteLibfuncLongId {
        generic_id: cairo_lang_sierra::ids::GenericLibfuncId::from_string(name),
        generic_args: vec![],
    })
}

pub fn const_libfunc_id_by_type(
    db: &dyn Database,
    value: ConstValueId<'_>,
    boxed: bool,
) -> cairo_lang_sierra::ids::ConcreteLibfuncId {
    let const_ty_arg = GenericArg::Type(const_type_id(db, value));
    if boxed {
        db.intern_concrete_lib_func(cairo_lang_sierra::program::ConcreteLibfuncLongId {
            generic_id: cairo_lang_sierra::ids::GenericLibfuncId::from_string(
                ConstAsBoxLibfunc::STR_ID,
            ),
            generic_args: vec![const_ty_arg, GenericArg::Value(0.into())],
        })
    } else {
        db.intern_concrete_lib_func(cairo_lang_sierra::program::ConcreteLibfuncLongId {
            generic_id: cairo_lang_sierra::ids::GenericLibfuncId::from_string(
                ConstAsImmediateLibfunc::STR_ID,
            ),
            generic_args: vec![const_ty_arg],
        })
    }
}

/// Returns the [cairo_lang_sierra::ids::ConcreteTypeId] for the given `value`.
fn const_type_id(
    db: &dyn Database,
    value: ConstValueId<'_>,
) -> cairo_lang_sierra::ids::ConcreteTypeId {
    let ty = value.ty(db).unwrap();
    let first_arg = GenericArg::Type(db.get_concrete_type_id(ty).unwrap().clone());
    db.intern_concrete_type(SierraGeneratorTypeLongId::Regular(
        cairo_lang_sierra::program::ConcreteTypeLongId {
            generic_id: ConstType::ID,
            generic_args: match value.long(db) {
                ConstValue::Int(v, _) => vec![first_arg, GenericArg::Value(v.clone())],
                ConstValue::Struct(tys, _) => {
                    let mut args = vec![first_arg];
                    for value in tys {
                        args.push(GenericArg::Type(const_type_id(db, *value)));
                    }
                    args
                }
                ConstValue::Enum(variant, inner) => {
                    vec![
                        first_arg,
                        GenericArg::Value(variant.idx.into()),
                        GenericArg::Type(const_type_id(db, *inner)),
                    ]
                }
                ConstValue::NonZero(value) => {
                    vec![first_arg, GenericArg::Type(const_type_id(db, *value))]
                }
                ConstValue::Generic(_)
                | ConstValue::Var(_, _)
                | ConstValue::Missing(_)
                | ConstValue::ImplConstant(_) => {
                    unreachable!("Should be caught by the lowering.")
                }
            },
        }
        .into(),
    ))
}

pub fn match_enum_libfunc_id(
    db: &dyn Database,
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
    db: &dyn Database,
    ty: cairo_lang_sierra::ids::ConcreteTypeId,
) -> cairo_lang_sierra::ids::ConcreteLibfuncId {
    get_libfunc_id_with_generic_arg(db, "enum_from_bounded_int", ty)
}

pub fn drop_libfunc_id(
    db: &dyn Database,
    ty: cairo_lang_sierra::ids::ConcreteTypeId,
) -> cairo_lang_sierra::ids::ConcreteLibfuncId {
    get_libfunc_id_with_generic_arg(db, "drop", ty)
}

pub fn dup_libfunc_id(
    db: &dyn Database,
    ty: cairo_lang_sierra::ids::ConcreteTypeId,
) -> cairo_lang_sierra::ids::ConcreteLibfuncId {
    get_libfunc_id_with_generic_arg(db, "dup", ty)
}

pub fn branch_align_libfunc_id(db: &dyn Database) -> cairo_lang_sierra::ids::ConcreteLibfuncId {
    get_libfunc_id_without_generics(db, "branch_align")
}

pub fn jump_libfunc_id(db: &dyn Database) -> cairo_lang_sierra::ids::ConcreteLibfuncId {
    get_libfunc_id_without_generics(db, "jump")
}

pub fn revoke_ap_tracking_libfunc_id(
    db: &dyn Database,
) -> cairo_lang_sierra::ids::ConcreteLibfuncId {
    get_libfunc_id_without_generics(db, "revoke_ap_tracking")
}

pub fn enable_ap_tracking_libfunc_id(
    db: &dyn Database,
) -> cairo_lang_sierra::ids::ConcreteLibfuncId {
    get_libfunc_id_without_generics(db, "enable_ap_tracking")
}

pub fn disable_ap_tracking_libfunc_id(
    db: &dyn Database,
) -> cairo_lang_sierra::ids::ConcreteLibfuncId {
    get_libfunc_id_without_generics(db, "disable_ap_tracking")
}

pub fn alloc_local_libfunc_id(
    db: &dyn Database,
    ty: cairo_lang_sierra::ids::ConcreteTypeId,
) -> cairo_lang_sierra::ids::ConcreteLibfuncId {
    get_libfunc_id_with_generic_arg(db, "alloc_local", ty)
}

pub fn finalize_locals_libfunc_id(db: &dyn Database) -> cairo_lang_sierra::ids::ConcreteLibfuncId {
    get_libfunc_id_without_generics(db, "finalize_locals")
}

/// Returns the [LibfuncSignature] of the given function.
pub fn get_libfunc_signature<'db>(
    db: &'db dyn Database,
    concrete_lib_func_id: &ConcreteLibfuncId,
) -> &'db LibfuncSignature {
    get_libfunc_signature_tracked(db, (), concrete_lib_func_id.id)
}

/// Implementation of [get_libfunc_signature] that is tracked.
#[salsa::tracked(returns(ref))]
fn get_libfunc_signature_tracked(
    db: &dyn Database,
    _tracked: Tracked,
    concrete_lib_func_id: u64,
) -> LibfuncSignature {
    let libfunc_long_id = db.lookup_concrete_lib_func(&concrete_lib_func_id.into());
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
            let function = db.lookup_sierra_function(&function);
            panic!("Missing function {:?}", function.debug(db));
        }
        // If panic happens here, make sure the specified libfunc name is in one of the STR_IDs of
        // the libfuncs in the [`CoreLibfunc`] structured enum.
        panic!(
            "Failed to specialize: `{}`. Error: {err}",
            DebugReplacer { db }.replace_libfunc_id(&concrete_lib_func_id.into())
        )
    })
}

/// Returns the [ConcreteLibfuncId] for calling a user-defined function.
pub fn function_call_libfunc_id(
    db: &dyn Database,
    func: lowering::ids::FunctionId<'_>,
) -> ConcreteLibfuncId {
    db.intern_concrete_lib_func(cairo_lang_sierra::program::ConcreteLibfuncLongId {
        generic_id: GenericLibfuncId::from_string("function_call"),
        generic_args: vec![GenericArg::UserFunc(db.intern_sierra_function(func))],
    })
}

/// Returns the [ConcreteLibfuncId] for calling a user-defined function, given a coupon for that
/// function.
pub fn coupon_call_libfunc_id(
    db: &dyn Database,
    func: lowering::ids::FunctionId<'_>,
) -> ConcreteLibfuncId {
    db.intern_concrete_lib_func(cairo_lang_sierra::program::ConcreteLibfuncLongId {
        generic_id: GenericLibfuncId::from_string("coupon_call"),
        generic_args: vec![GenericArg::UserFunc(db.intern_sierra_function(func))],
    })
}

/// Returns the [ConcreteLibfuncId] used for calling a libfunc.
pub fn generic_libfunc_id(
    db: &dyn Database,
    extern_id: defs::ids::ExternFunctionId<'_>,
    generic_args: Vec<GenericArg>,
) -> ConcreteLibfuncId {
    db.intern_concrete_lib_func(cairo_lang_sierra::program::ConcreteLibfuncLongId {
        generic_id: GenericLibfuncId::from_string(extern_id.name(db).long(db).clone()),
        generic_args,
    })
}

/// Returns the [ConcreteLibfuncId] used for calling a function (either user-defined or libfunc).
pub fn get_concrete_libfunc_id<'db>(
    db: &'db dyn Database,
    function: lowering::ids::FunctionId<'db>,
    with_coupon: bool,
) -> (Option<lowering::ids::ConcreteFunctionWithBodyId<'db>>, ConcreteLibfuncId) {
    // Check if this is a user-defined function or a libfunc.
    if let Some(body) = function.body(db).expect("No diagnostics at this stage.") {
        if with_coupon {
            return (Some(body), coupon_call_libfunc_id(db, function));
        } else {
            return (Some(body), function_call_libfunc_id(db, function));
        }
    }

    assert!(!with_coupon, "Coupon cannot be used with extern functions.");

    let semantic = extract_matches!(function.long(db), lowering::ids::FunctionLongId::Semantic);
    let concrete_function = semantic.long(db).function.clone();
    let GenericFunctionId::Extern(extern_id) = concrete_function.generic_function else {
        panic!("Expected an extern function, found: {:?}", concrete_function.full_path(db));
    };

    let mut generic_args = vec![];
    for generic_arg in &concrete_function.generic_args {
        match generic_arg {
            semantic::GenericArgumentId::Type(ty) => {
                // TODO(lior): How should the following unwrap() be handled?
                generic_args.push(GenericArg::Type(
                    db.get_concrete_type_id(*ty)
                        .unwrap_or_else(|_| {
                            panic!(
                                "Failed to obtain concrete type id for generic type argument: \
                                 {ty:?}"
                            )
                        })
                        .clone(),
                ))
            }
            semantic::GenericArgumentId::Constant(value_id) => {
                generic_args.push(GenericArg::Value(
                    value_id.long(db).to_int().expect("Expected ConstValue::Int for size").clone(),
                ));
            }
            semantic::GenericArgumentId::Impl(_) | semantic::GenericArgumentId::NegImpl(_) => {
                // Everything after an impl generic is ignored as it does not exist in Sierra.
                // This may still be used in high level code for getting type information that is
                // otherwise concluded by the sierra-to-casm compiler, or addition of `where` clause
                // style blocks.
                break;
            }
        };
    }

    (None, generic_libfunc_id(db, extern_id, generic_args))
}

// Given a function id, generates a dummy function call libfunc id.
pub fn dummy_call_libfunc_id(
    db: &dyn Database,
    function_id: cairo_lang_sierra::ids::FunctionId,
    sierra_signature: &FunctionSignature,
) -> ConcreteLibfuncId {
    let ap_change = db
        .get_ap_change(db.lookup_sierra_function(&function_id).body(db).unwrap().unwrap())
        .unwrap();

    let mut gargs = vec![];

    gargs.push(GenericArg::UserFunc(function_id.clone()));

    gargs.push(GenericArg::Value(
        match ap_change {
            cairo_lang_sierra::extensions::lib_func::SierraApChange::Unknown => 1,
            cairo_lang_sierra::extensions::lib_func::SierraApChange::Known { new_vars_only: _ } => {
                0
            }
            cairo_lang_sierra::extensions::lib_func::SierraApChange::BranchAlign
            | cairo_lang_sierra::extensions::lib_func::SierraApChange::FunctionCall(_) => {
                unreachable!("should never happen")
            }
        }
        .into(),
    ));

    let as_generic_arg = |ty: &ConcreteTypeId| GenericArg::Type(ty.clone());

    gargs.push(GenericArg::Value(sierra_signature.param_types.len().into()));
    gargs.extend(sierra_signature.param_types.iter().map(as_generic_arg));

    gargs.push(GenericArg::Value(sierra_signature.ret_types.len().into()));
    gargs.extend(sierra_signature.ret_types.iter().map(as_generic_arg));

    db.intern_concrete_lib_func(cairo_lang_sierra::program::ConcreteLibfuncLongId {
        generic_id: cairo_lang_sierra::ids::GenericLibfuncId::from_string("dummy_function_call"),
        generic_args: gargs,
    })
}
