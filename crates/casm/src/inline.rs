use crate::hints::Hint;
use crate::instructions::Instruction;

#[cfg(test)]
#[path = "inline_test.rs"]
mod test;

#[macro_export]
macro_rules! casm {
    {$($tok:tt)*} => {
        {
            let mut ctx = $crate::inline::CasmContext::default();
            $crate::casm_extend!(ctx, $($tok)*);
            ctx
        }
    }
}

#[macro_export]
macro_rules! casm_extend {
    ($ctx:ident,) => {};
    ($ctx:ident, $dst:tt = $a:tt $(+ $b0:tt)? $(* $b1:tt)? $(,$ap:ident++)? ; $($tok:tt)*) => {
        let body = $crate::instructions::InstructionBody::AssertEq(
            $crate::instructions::AssertEqInstruction {
                a: $crate::deref!($dst),
                b: $crate::res!($a $(+ $b0)? $(* $b1)?),
            }
        );
        $crate::append_instruction!($ctx, body $(,$ap++)?);
        $crate::casm_extend!($ctx, $($tok)*)
    };
    ($ctx:ident, call rel $target:tt $(,$ap:ident++)? ; $($tok:tt)*) => {
        let body = $crate::instructions::InstructionBody::Call(
            $crate::instructions::CallInstruction {
                target: $crate::deref_or_immediate!($target),
                relative: true,
            }
        );
        $crate::append_instruction!($ctx, body $(,$ap++)?);
        $crate::casm_extend!($ctx, $($tok)*)
    };
    ($ctx:ident, call abs $target:tt $(,$ap:ident++)? ; $($tok:tt)*) => {
        let body = $crate::instructions::InstructionBody::Call(
            $crate::instructions::CallInstruction {
                target: $crate::deref_or_immediate!($target),
                relative: false,
            }
        );
        $crate::append_instruction!($ctx, body $(,$ap++)?);
        $crate::casm_extend!($ctx, $($tok)*)
    };
    ($ctx:ident, jmp rel $target:expr $(,$ap:ident++)? ; $($tok:tt)*) => {
        let body = $crate::instructions::InstructionBody::Jump(
            $crate::instructions::JumpInstruction {
                target: $crate::deref_or_immediate!($target),
                relative: true,
            }
        );
        $crate::append_instruction!($ctx, body $(,$ap++)?);
        $crate::casm_extend!($ctx, $($tok)*)
    };
    ($ctx:ident, jmp abs $target:tt $(,$ap:ident++)? ; $($tok:tt)*) => {
        let body = $crate::instructions::InstructionBody::Jump(
            $crate::instructions::JumpInstruction {
                target: $crate::deref_or_immediate!($target),
                relative: false,
            }
        );
        $crate::append_instruction!($ctx, body $(,$ap++)?);
        $crate::casm_extend!($ctx, $($tok)*)
    };
    ($ctx:ident, jmp rel $target:tt if $cond:tt != 0 $(,$ap:ident++)? ; $($tok:tt)*) => {
        let body = $crate::instructions::InstructionBody::Jnz(
            $crate::instructions::JnzInstruction {
                jump_offset: $crate::deref_or_immediate!($target),
                condition: $crate::deref!($cond),
            }
        );
        $crate::append_instruction!($ctx, body $(,$ap++)?);
        $crate::casm_extend!($ctx, $($tok)*)
    };
    ($ctx:ident, jmp $target:tt if $cond:tt != 0 $(,$ap:ident++)? ; $($tok:tt)*) => {
        let body = $crate::instructions::InstructionBody::Jnz(
            $crate::instructions::JnzInstruction {
                jump_offset: $crate::deref_or_immediate!($target),
                condition: $crate::deref!($cond),
            }
        );
        $crate::append_instruction!($ctx, body $(,$ap++)?);
        $crate::casm_extend!($ctx, $($tok)*)
    };
    ($ctx:ident, ap += $operand:tt $(,$ap:ident++)? ; $($tok:tt)*) => {
        let body = $crate::instructions::InstructionBody::AddAp(
            $crate::instructions::AddApInstruction { operand: $crate::res!($operand) }
        );
        $crate::append_instruction!($ctx, body $(,$ap++)?);
        $crate::casm_extend!($ctx, $($tok)*)
    };
    ($ctx:ident, ret $(,$ap:ident++)? ; $($tok:tt)*) => {
        let body = $crate::instructions::InstructionBody::Ret(
            $crate::instructions::RetInstruction {}
        );
        $crate::append_instruction!($ctx, body $(,$ap++)?);
        $crate::casm_extend!($ctx, $($tok)*)
    };
    ($ctx:ident, %{ memory $dst:tt = segments . add ( ) %} $($tok:tt)*) => {
        $ctx.current_hints.push($crate::hints::Hint::AllocSegment{dst: $crate::deref!($dst)});
        $crate::casm_extend!($ctx, $($tok)*)
    };
    ($ctx:ident, %{ memory $dst:tt = memory $lhs:tt < memory $rhs:tt %} $($tok:tt)*) => {
        $ctx.current_hints.push($crate::hints::Hint::TestLessThan{
            lhs: $crate::deref!($lhs).into(),
            rhs: $crate::deref!($rhs).into(),
            dst: $crate::deref!($dst),
        });
        $crate::casm_extend!($ctx, $($tok)*)
    };
    ($ctx:ident, %{ memory $dst:tt = memory $lhs:tt < $rhs:tt %} $($tok:tt)*) => {
        $ctx.current_hints.push($crate::hints::Hint::TestLessThan{
            lhs: $crate::deref!($lhs).into(),
            rhs: $crate::deref_or_immediate!($rhs),
            dst: $crate::deref!($dst),
        });
        $crate::casm_extend!($ctx, $($tok)*)
    };
    ($ctx:ident, %{ memory $dst:tt = $lhs:tt < memory $rhs:tt %} $($tok:tt)*) => {
        $ctx.current_hints.push($crate::hints::Hint::TestLessThan{
            lhs: $crate::deref_or_immediate!($lhs),
            rhs: $crate::deref!($rhs).into(),
            dst: $crate::deref!($dst),
        });
        $crate::casm_extend!($ctx, $($tok)*)
    };
}

#[macro_export]
macro_rules! append_instruction {
    ($ctx:ident, $body:ident $(,$ap:ident++)?) => {
        let instr = $crate::instructions::Instruction {
            body: $body,
            inc_ap: $crate::is_inc_ap!($($ap++)?),
            hints: $ctx.current_hints,
        };
        $ctx.current_code_offset += instr.body.op_size();
        $ctx.current_hints = vec![];
        $ctx.instructions.push(instr);
    };
}

#[macro_export]
macro_rules! is_inc_ap {
    () => {
        false
    };
    (ap + +) => {
        true
    };
}

#[allow(dead_code)]
#[derive(Default)]
pub struct CasmContext {
    pub current_code_offset: usize,
    pub current_hints: Vec<Hint>,
    pub instructions: Vec<Instruction>,
    // TODO(spapini): Branches.
    // TODO(spapini): Relocations.
}

#[macro_export]
macro_rules! deref {
    ([$reg:ident + $offset:expr]) => {
        $crate::operand::CellRef { register: $crate::reg!($reg), offset: $offset }
    };
    ([$reg:ident - $offset:expr]) => {
        $crate::operand::CellRef { register: $crate::reg!($reg), offset: -$offset }
    };
    ([$reg:ident]) => {
        $crate::operand::CellRef { register: $crate::reg!($reg), offset: 0 }
    };
    ($a:expr) => {
        $a
    };
}

#[macro_export]
macro_rules! reg {
    (ap) => {
        $crate::operand::Register::AP
    };
    (fp) => {
        $crate::operand::Register::FP
    };
}

#[macro_export]
macro_rules! deref_or_immediate {
    ([$a:ident $($op:tt $offset:expr)?]) => {
        $crate::operand::DerefOrImmediate::Deref($crate::deref!([$a $($op $offset)?]))
    };
    ($a:expr) => {
        $crate::operand::DerefOrImmediate::from($a)
    };
}

#[macro_export]
macro_rules! res {
    ($a:tt + $b:tt) => {
        $crate::operand::ResOperand::BinOp($crate::operand::BinOpOperand {
            op: $crate::operand::Operation::Add,
            a: $crate::deref!($a),
            b: $crate::deref_or_immediate!($b),
        })
    };
    ($a:tt * $b:tt) => {
        $crate::operand::ResOperand::BinOp($crate::operand::BinOpOperand {
            op: $crate::operand::Operation::Mul,
            a: $crate::deref!($a),
            b: $crate::deref_or_immediate!($b),
        })
    };
    ([[$a:expr]]) => {
        $crate::operand::ResOperand::DoubleDeref($a, 0)
    };
    ([[$a:expr] + $b:expr]) => {
        $crate::operand::ResOperand::DoubleDeref($a, $b)
    };
    ($a:tt) => {
        $crate::operand::ResOperand::from($crate::deref_or_immediate!($a))
    };
}
