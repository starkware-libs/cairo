use casm::ap_change::ApChange;
use casm::instructions::{AssertEqInstruction, Instruction, InstructionBody};
use casm::operand::{DerefOperand, Register, ResOperand};
use sierra::extensions::core::mem::MemConcrete;
use sierra::extensions::{CoreConcrete, ExtensionError};

use crate::references::ReferenceValue;

/// Describes the changes to the set of references at the branch target.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct BranchRefChanges {
    // New references defined at a given branch.
    // should correspond to BranchInfo.results.
    pub refs: Vec<ReferenceValue>,
    pub ap_change: ApChange,
}

/// The result from a compilation of a single invocation statement.
#[derive(Debug, Eq, PartialEq)]
pub struct CompiledInvocation {
    // A vector instructions that implement the Invocation.
    pub instruction: Vec<Instruction>,
    // A vector of BranchRefChanges, should correspond to Invocation.branches.
    pub results: Vec<BranchRefChanges>,
}

pub fn compile_invocation(
    ext: &CoreConcrete,
    refs: &[ReferenceValue],
) -> Result<CompiledInvocation, ExtensionError> {
    match ext {
        // TODO(ilya, 10/10/2022): Handle type.
        CoreConcrete::Mem(MemConcrete::StoreTemp(_)) => Ok(CompiledInvocation {
            instruction: vec![Instruction {
                body: InstructionBody::AssertEq(AssertEqInstruction {
                    a: DerefOperand { register: Register::AP, offset: 0 },
                    b: refs[0].expression.clone(),
                }),
                inc_ap: true,
            }],
            results: vec![BranchRefChanges {
                refs: vec![ReferenceValue {
                    expression: ResOperand::Deref(DerefOperand {
                        register: Register::AP,
                        offset: -1,
                    }),
                }],
                ap_change: ApChange::Known(1),
            }],
        }),
        _ => Err(ExtensionError::NotImplemented),
    }
}
