use crate::operand::*;

#[allow(dead_code)]
enum Instruction {
    JumpInstruction(JumpInstruction),
}

#[allow(dead_code)]
impl Instruction {
    fn format(&self) -> String {
        match self {
            Instruction::JumpInstruction(jmp_insn) => jmp_insn.format(),
        }
    }
}

// Represents the instruction "jmp rel/abs".
#[allow(dead_code)]
pub struct JumpInstruction {
    target: Operand,
    relative: bool,
}

#[allow(dead_code)]
impl JumpInstruction {
    fn format(&self) -> String {
        format!(
            "jmp {} {}",
            if self.relative { "rel" } else { "abs" },
            self.target.format()
        )
    }
}

#[test]
fn test_jump_format() {
    let abs_jmp_insn = JumpInstruction {
        target: Operand::ImmediateOperand(ImmediateOperand { value: 3 }),
        relative: false,
    };

    assert_eq!(abs_jmp_insn.format(), "jmp abs 3");

    let rel_jmp_insn: Instruction = Instruction::JumpInstruction(JumpInstruction {
        target: Operand::ImmediateOperand(ImmediateOperand { value: -5 }),
        relative: true,
    });

    assert_eq!(rel_jmp_insn.format(), "jmp rel -5");
}

pub struct AssertEqInstruction {
    a: Operand,
    b: Operand,
}

#[allow(dead_code)]
impl AssertEqInstruction {
    fn format(&self) -> String {
        return format!("{} = {}", self.a.format(), self.b.format());
    }
}

#[test]
fn test_assert_eq_format() {
    let op1: Operand = Operand::DerefOperand(DerefOperand {
        register: Register::AP,
        offset: 5,
    });

    let op2: Operand = Operand::ImmediateOperand(ImmediateOperand { value: 205 });

    let insn = AssertEqInstruction { a: op1, b: op2 };
    assert_eq!(insn.format(), "[ap + 5] = 205");
}
