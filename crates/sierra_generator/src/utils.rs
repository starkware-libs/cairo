use defs::ids::GenericFunctionId;
use sierra::extensions::core::CoreLibFunc;
use sierra::extensions::lib_func::LibFuncSignature;
use sierra::extensions::GenericLibFuncEx;
use sierra::ids::{ConcreteLibFuncId, ConcreteTypeId, GenericLibFuncId};
use sierra::program;

use crate::db::SierraGenGroup;
use crate::pre_sierra;
use crate::specialization_context::SierraSignatureSpecializationContext;

pub fn simple_statement(
    libfunc_id: ConcreteLibFuncId,
    args: &[sierra::ids::VarId],
    results: &[sierra::ids::VarId],
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
    jump: ConcreteLibFuncId,
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

pub fn return_statement(res: Vec<sierra::ids::VarId>) -> pre_sierra::Statement {
    pre_sierra::Statement::Sierra(program::GenStatement::Return(res))
}

/// Returns the [sierra::program::ConcreteLibFuncLongId] associated with `store_temp`.
pub fn store_temp_libfunc_id(
    db: &dyn SierraGenGroup,
    ty: ConcreteTypeId,
) -> sierra::ids::ConcreteLibFuncId {
    db.intern_concrete_lib_func(sierra::program::ConcreteLibFuncLongId {
        generic_id: sierra::ids::GenericLibFuncId::from_string("store_temp"),
        generic_args: vec![sierra::program::GenericArg::Type(ty)],
    })
}

/// Returns the [sierra::program::ConcreteLibFuncLongId] associated with `rename`.
pub fn rename_libfunc_id(
    db: &dyn SierraGenGroup,
    ty: ConcreteTypeId,
) -> sierra::ids::ConcreteLibFuncId {
    db.intern_concrete_lib_func(sierra::program::ConcreteLibFuncLongId {
        generic_id: sierra::ids::GenericLibFuncId::from_string("rename"),
        generic_args: vec![sierra::program::GenericArg::Type(ty)],
    })
}

/// Returns the [LibFuncSignature] of the given function.
pub fn get_libfunc_signature(
    db: &dyn SierraGenGroup,
    concrete_lib_func_id: ConcreteLibFuncId,
) -> LibFuncSignature {
    let libfunc_long_id = db.lookup_intern_concrete_lib_func(concrete_lib_func_id);
    // TODO(lior): replace expect() with a diagnostic (unless this can never happen).
    CoreLibFunc::specialize_signature_by_id(
        &SierraSignatureSpecializationContext(db),
        &libfunc_long_id.generic_id,
        &libfunc_long_id.generic_args,
    )
    .expect("Specialization failure.")
}

/// Returns the [ConcreteLibFuncId] for calling a user-defined function.
pub fn function_call_libfunc_id(
    db: &dyn SierraGenGroup,
    func: semantic::FunctionId,
) -> ConcreteLibFuncId {
    db.intern_concrete_lib_func(sierra::program::ConcreteLibFuncLongId {
        generic_id: GenericLibFuncId::from_string("function_call"),
        generic_args: vec![sierra::program::GenericArg::UserFunc(db.intern_sierra_function(func))],
    })
}

/// Returns the [ConcreteLibFuncId] used for calling a libfunc.
pub fn generic_libfunc_id(
    db: &dyn SierraGenGroup,
    extern_id: defs::ids::ExternFunctionId,
    generic_args: Vec<sierra::program::GenericArg>,
) -> ConcreteLibFuncId {
    db.intern_concrete_lib_func(sierra::program::ConcreteLibFuncLongId {
        generic_id: GenericLibFuncId::from_string(extern_id.name(db.upcast())),
        generic_args,
    })
}

/// Returns the [ConcreteLibFuncId] used for calling a function (either user-defined or libfunc).
pub fn get_concrete_libfunc_id(
    db: &dyn SierraGenGroup,
    function: semantic::FunctionId,
) -> (semantic::ConcreteFunction, ConcreteLibFuncId) {
    // Check if this is a user-defined function or a libfunc.
    let concrete_function = db.lookup_intern_function(function).function;
    match concrete_function.generic_function {
        GenericFunctionId::Free(_) => (concrete_function, function_call_libfunc_id(db, function)),
        GenericFunctionId::Extern(extern_id) => {
            let mut generic_args = vec![];
            for generic_arg in &concrete_function.generic_args {
                generic_args.push(match generic_arg {
                    semantic::GenericArgumentId::Type(ty) => sierra::program::GenericArg::Type(
                        // TODO(lior): How should the following unwrap() be handled?
                        db.get_concrete_type_id(*ty).unwrap(),
                    ),
                });
            }

            (concrete_function, generic_libfunc_id(db, extern_id, generic_args))
        }
        GenericFunctionId::TraitFunction(_) => {
            panic!("Trait function should be replaced with concrete functions.")
        }
        GenericFunctionId::ImplFunction(_) => todo!(),
    }
}
