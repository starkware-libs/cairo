use sierra::ids::ConcreteExtensionId;
use sierra::program;

use crate::pre_sierra;

pub fn simple_statement(
    ext_name: impl Into<String>,
    args: &[sierra::ids::VarId],
    results: &[sierra::ids::VarId],
) -> pre_sierra::Statement {
    pre_sierra::Statement::SierraStatement(program::GenStatement::Invocation(
        program::GenInvocation {
            extension_id: ConcreteExtensionId::from_string(ext_name),
            args: args.into(),
            branches: vec![program::GenBranchInfo {
                target: program::GenBranchTarget::Fallthrough,
                results: results.into(),
            }],
        },
    ))
}
