use sierra::ids::{ConcreteLibFuncId, ConcreteTypeId};
use sierra::program;

use crate::db::SierraGenGroup;
use crate::pre_sierra;

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
