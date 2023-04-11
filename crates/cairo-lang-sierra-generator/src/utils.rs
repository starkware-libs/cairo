use cairo_lang_debug::DebugWithDb;
use cairo_lang_diagnostics::Maybe;
use cairo_lang_sierra::extensions::core::CoreLibfunc;
use cairo_lang_sierra::extensions::lib_func::LibfuncSignature;
use cairo_lang_sierra::extensions::snapshot::SnapshotType;
use cairo_lang_sierra::extensions::{
    ExtensionError, GenericLibfuncEx, NamedType, SpecializationError,
};
use cairo_lang_sierra::ids::{ConcreteLibfuncId, GenericLibfuncId};
use cairo_lang_sierra::program;
use cairo_lang_utils::extract_matches;
use num_bigint::BigInt;
use semantic::corelib::get_const_libfunc_name_by_type;
use semantic::items::functions::GenericFunctionId;
use smol_str::SmolStr;
use {cairo_lang_defs as defs, cairo_lang_lowering as lowering, cairo_lang_semantic as semantic};

use crate::db::SierraGenGroup;
use crate::pre_sierra;
use crate::replace_ids::{DebugReplacer, SierraIdReplacer};
use crate::specialization_context::SierraSignatureSpecializationContext;

pub fn simple_statement(
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
        generic_args: vec![cairo_lang_sierra::program::GenericArg::Type(ty)],
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
        let concrete_enum_type = extract_matches!(
            &long_id.generic_args[0],
            cairo_lang_sierra::program::GenericArg::Type
        )
        .clone();
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
        generic_args: vec![
            cairo_lang_sierra::program::GenericArg::Type(ty),
            cairo_lang_sierra::program::GenericArg::Value(variant_idx.into()),
        ],
    })
}

/// Returns the [cairo_lang_sierra::program::ConcreteLibfuncLongId] associated with `snapshot_take`.
pub fn snapshot_take_libfunc_id(
    db: &dyn SierraGenGroup,
    ty: cairo_lang_sierra::ids::ConcreteTypeId,
) -> cairo_lang_sierra::ids::ConcreteLibfuncId {
    db.intern_concrete_lib_func(cairo_lang_sierra::program::ConcreteLibfuncLongId {
        generic_id: cairo_lang_sierra::ids::GenericLibfuncId::from_string("snapshot_take"),
        generic_args: vec![cairo_lang_sierra::program::GenericArg::Type(ty)],
    })
}

/// Returns the [cairo_lang_sierra::program::ConcreteLibfuncLongId] associated with `rename`.
pub fn rename_libfunc_id(
    db: &dyn SierraGenGroup,
    ty: cairo_lang_sierra::ids::ConcreteTypeId,
) -> cairo_lang_sierra::ids::ConcreteLibfuncId {
    db.intern_concrete_lib_func(cairo_lang_sierra::program::ConcreteLibfuncLongId {
        generic_id: cairo_lang_sierra::ids::GenericLibfuncId::from_string("rename"),
        generic_args: vec![cairo_lang_sierra::program::GenericArg::Type(ty)],
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
    value: BigInt,
) -> cairo_lang_sierra::ids::ConcreteLibfuncId {
    db.intern_concrete_lib_func(cairo_lang_sierra::program::ConcreteLibfuncLongId {
        generic_id: cairo_lang_sierra::ids::GenericLibfuncId::from_string(
            get_const_libfunc_name_by_type(db.upcast(), ty),
        ),
        generic_args: vec![cairo_lang_sierra::program::GenericArg::Value(value)],
    })
}

pub fn match_enum_libfunc_id(
    db: &dyn SierraGenGroup,
    ty: cairo_lang_sierra::ids::ConcreteTypeId,
) -> Maybe<cairo_lang_sierra::ids::ConcreteLibfuncId> {
    let long_id = &db.get_type_info(ty.clone())?.long_id;
    let is_snapshot = long_id.generic_id == SnapshotType::id();
    Ok(if is_snapshot {
        let concrete_enum_type = extract_matches!(
            &long_id.generic_args[0],
            cairo_lang_sierra::program::GenericArg::Type
        )
        .clone();
        get_libfunc_id_with_generic_arg(db, "enum_snapshot_match", concrete_enum_type)
    } else {
        get_libfunc_id_with_generic_arg(db, "enum_match", ty)
    })
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
        generic_args: vec![cairo_lang_sierra::program::GenericArg::UserFunc(
            db.intern_sierra_function(func),
        )],
    })
}

/// Returns the [ConcreteLibfuncId] used for calling a libfunc.
pub fn generic_libfunc_id(
    db: &dyn SierraGenGroup,
    extern_id: defs::ids::ExternFunctionId,
    generic_args: Vec<cairo_lang_sierra::program::GenericArg>,
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
) -> (Option<lowering::ids::ConcreteFunctionWithBodyId>, ConcreteLibfuncId) {
    // Check if this is a user-defined function or a libfunc.
    if let Some(body) = function.body(db.upcast()).expect("No diagnostics at this stage.") {
        return (Some(body), function_call_libfunc_id(db, function));
    }
    let semantic =
        extract_matches!(function.lookup(db.upcast()), lowering::ids::FunctionLongId::Semantic);
    let concrete_function = db.lookup_intern_function(semantic).function;
    let extern_id = extract_matches!(concrete_function.generic_function, GenericFunctionId::Extern);

    let mut generic_args = vec![];
    for generic_arg in &concrete_function.generic_args {
        generic_args.push(match generic_arg {
            semantic::GenericArgumentId::Type(ty) => {
                // TODO(lior): How should the following unwrap() be handled?
                cairo_lang_sierra::program::GenericArg::Type(db.get_concrete_type_id(*ty).unwrap())
            }
            semantic::GenericArgumentId::Literal(literal_id) => {
                cairo_lang_sierra::program::GenericArg::Value(
                    db.lookup_intern_literal(*literal_id).value,
                )
            }
            semantic::GenericArgumentId::Impl(_) => {
                panic!("Extern function with impl generics are not supported.")
            }
        });
    }

    (None, generic_libfunc_id(db, extern_id, generic_args))
}
