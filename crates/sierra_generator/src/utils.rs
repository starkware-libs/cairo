use sierra::ids::ConcreteLibFuncId;
use sierra::program;
use smol_str::SmolStr;

use crate::pre_sierra;

pub fn simple_statement(
    ext_name: impl Into<SmolStr>,
    args: &[sierra::ids::VarId],
    results: &[sierra::ids::VarId],
) -> pre_sierra::Statement {
    pre_sierra::Statement::SierraStatement(program::GenStatement::Invocation(
        program::GenInvocation {
            libfunc_id: ConcreteLibFuncId::from_string(ext_name),
            args: args.into(),
            branches: vec![program::GenBranchInfo {
                target: program::GenBranchTarget::Fallthrough,
                results: results.into(),
            }],
        },
    ))
}
