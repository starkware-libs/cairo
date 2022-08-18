use casm::instructions::Instruction;
use sierra::extensions::{CoreConcrete, ExtensionError};

use crate::references::ReferenceValue;

/// Describes the changes to the set of reference at the branch target.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct BranchRefChanges {
    // New references defined at a given branch.
    // should correspond to BranchInfo.results.
    pub refs: Vec<ReferenceValue>,
    pub ap_change: isize,
}

/// The result from a compilation of a single invocation statement.
#[derive(Debug, Eq, PartialEq)]
pub struct CompiledInvocation {
    // A vector instructions that implement the Invocation.
    pub instruction: Vec<Instruction>,
    // A vector of BranchRefChanges, should correspond to Invocation.branches.
    pub results: Vec<BranchRefChanges>,
}

pub fn compile_invocation(_ext: &CoreConcrete) -> Result<CompiledInvocation, ExtensionError> {
    Err(ExtensionError::NotImplemented)
}
