use debug::DebugWithDb;
use sierra::program;
use utils::extract_matches;

use crate::db::SierraGenGroup;
use crate::pre_sierra::{self, PushValue};

/// Replaces `sierra::ids::{ConcreteLibFuncId, ConcreteTypeId, FunctionId}` with a dummy ids whose
/// debug string is the string representing the expanded information about the id.
/// For LibFuncs and Types - that would be recursively opening their generic arguments, for
/// functions - that would be getting their original name. For example, while the original debug
/// string may be `[6]`, the resulting debug string may be: for libfuncs: `felt_const<2>` or
/// `unbox<Box<Box<felt>>>`. for types: `felt` or `Box<Box<felt>>`.
/// for functions: `test_crate::foo`.
pub fn replace_sierra_ids(
    db: &dyn SierraGenGroup,
    statement: &pre_sierra::Statement,
) -> pre_sierra::Statement {
    match statement {
        pre_sierra::Statement::Sierra(sierra::program::GenStatement::Invocation(p)) => {
            pre_sierra::Statement::Sierra(sierra::program::GenStatement::Invocation(
                sierra::program::GenInvocation {
                    libfunc_id: replace_libfunc_id(db, &p.libfunc_id),
                    ..p.clone()
                },
            ))
        }
        pre_sierra::Statement::PushValues(values) => pre_sierra::Statement::PushValues(
            values
                .iter()
                .map(|value| PushValue { ty: replace_type_id(db, &value.ty), ..value.clone() })
                .collect(),
        ),
        _ => statement.clone(),
    }
}

/// Replaces `sierra::ids::{ConcreteLibFuncId, ConcreteTypeId, FunctionId}` with a dummy ids whose
/// debug string is the string representing the expanded information about the id.
/// For LibFuncs and Types - that would be recursively opening their generic arguments, for
/// functions - that would be getting their original name. For example, while the original debug
/// string may be `[6]`, the resulting debug string may be: for libfuncs: `felt_const<2>` or
/// `unbox<Box<Box<felt>>>`. for types: `felt` or `Box<Box<felt>>`.
/// for functions: `test_crate::foo`.
///
/// Similar to [replace_sierra_ids] except that it acts on [sierra::program::Program].
pub fn replace_sierra_ids_in_program(
    db: &dyn SierraGenGroup,
    program: &sierra::program::Program,
) -> sierra::program::Program {
    let mut program = program.clone();
    for statement in &mut program.statements {
        if let sierra::program::GenStatement::Invocation(p) = statement {
            p.libfunc_id = replace_libfunc_id(db, &p.libfunc_id);
        }
    }
    for type_declaration in &mut program.type_declarations {
        type_declaration.id = replace_type_id(db, &type_declaration.id);
        replace_generic_args(db, &mut type_declaration.long_id.generic_args);
    }
    for libfunc_declaration in &mut program.libfunc_declarations {
        libfunc_declaration.id = replace_libfunc_id(db, &libfunc_declaration.id);
        replace_generic_args(db, &mut libfunc_declaration.long_id.generic_args);
    }
    for function in &mut program.funcs {
        function.id = replace_function_id(db, &function.id);
        for param in &mut function.params {
            param.ty = replace_type_id(db, &param.ty);
        }
        for ty in &mut function.signature.ret_types {
            *ty = replace_type_id(db, ty);
        }
        for ty in &mut function.signature.param_types {
            *ty = replace_type_id(db, ty);
        }
    }
    program
}

/// Helper for [replace_sierra_ids] and [replace_sierra_ids_in_program] replacing libfunc ids.
fn replace_libfunc_id(
    db: &dyn SierraGenGroup,
    id: &sierra::ids::ConcreteLibFuncId,
) -> sierra::ids::ConcreteLibFuncId {
    let mut long_id = db.lookup_intern_concrete_lib_func(id.clone());
    replace_generic_args(db, &mut long_id.generic_args);
    long_id.to_string().into()
}

/// Helper for [replace_sierra_ids] and [replace_sierra_ids_in_program] replacing type ids.
fn replace_type_id(
    db: &dyn SierraGenGroup,
    id: &sierra::ids::ConcreteTypeId,
) -> sierra::ids::ConcreteTypeId {
    let mut long_id = db.lookup_intern_concrete_type(id.clone());
    replace_generic_args(db, &mut long_id.generic_args);
    if long_id.generic_id == "Enum".into() || long_id.generic_id == "Struct".into() {
        long_id.generic_id =
            extract_matches!(&long_id.generic_args[0], program::GenericArg::UserType)
                .to_string()
                .into();
        if long_id.generic_id == "Tuple".into() {
            long_id.generic_args = long_id.generic_args.into_iter().skip(1).collect();
            if long_id.generic_args.is_empty() {
                long_id.generic_id = "Unit".into();
            }
        } else {
            long_id.generic_args.clear();
        }
    }
    long_id.to_string().into()
}

/// Helper for [replace_sierra_ids] and [replace_sierra_ids_in_program] replacing function ids.
fn replace_function_id(
    db: &dyn SierraGenGroup,
    sierra_id: &sierra::ids::FunctionId,
) -> sierra::ids::FunctionId {
    let semantic_id = db.lookup_intern_sierra_function(sierra_id.clone());
    format!("{:?}", db.lookup_intern_function(semantic_id).debug(db.upcast())).into()
}

/// Helper for [replace_sierra_ids] and [replace_sierra_ids_in_program] replacing ids within a
/// vector of generic args.
fn replace_generic_args(db: &dyn SierraGenGroup, generic_args: &mut Vec<program::GenericArg>) {
    for arg in generic_args {
        match arg {
            program::GenericArg::Type(id) => {
                *id = replace_type_id(db, id);
            }
            program::GenericArg::UserFunc(id) => {
                *id = replace_function_id(db, id);
            }
            program::GenericArg::LibFunc(id) => {
                *id = replace_libfunc_id(db, id);
            }
            program::GenericArg::Value(_) | program::GenericArg::UserType(_) => {}
        }
    }
}
