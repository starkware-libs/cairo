use crate::hints::Hint;
use crate::instructions::*;

#[cfg(test)]
#[path = "inline_test.rs"]
mod test;

#[macro_export]
macro_rules! casm {
    {$($tok:tt)*} => {
        {
            use $crate::instructions::*;
            use $crate::inline::CasmContext;
            let mut ctx = CasmContext::default();
            $crate::casm_extend!(ctx, $($tok)*);
            ctx
        }
    }
}

#[macro_export]
macro_rules! casm_extend {
    ($ctx:ident,) => {};
    ($ctx:ident, $dst:tt = $a:tt $(+ $b0:tt)? $(* $b1:tt)? $(,$ap:ident++)? ; $($tok:tt)*) => {
        let body = InstructionBody::AssertEq(AssertEqInstruction {
            a: $crate::deref!($dst),
            b: $crate::res!($a $(+ $b0)? $(* $b1)?),
        });
        $crate::append_instruction!($ctx, body $(,$ap++)?);
        $crate::casm_extend!($ctx, $($tok)*)
    };
    ($ctx:ident, call rel $target:tt $(,$ap:ident++)? ; $($tok:tt)*) => {
        let body = InstructionBody::Call(CallInstruction {
            target: $crate::deref_or_immediate!($target),
            relative: true,
        });
        $crate::append_instruction!($ctx, body $(,$ap++)?);
        $crate::casm_extend!($ctx, $($tok)*)
    };
    ($ctx:ident, call abs $target:tt $(,$ap:ident++)? ; $($tok:tt)*) => {
        let body = InstructionBody::Call(CallInstruction {
            target: $crate::deref_or_immediate!($target),
            relative: false,
        });
        $crate::append_instruction!($ctx, body $(,$ap++)?);
        $crate::casm_extend!($ctx, $($tok)*)
    };
    ($ctx:ident, jmp rel $target:expr $(,$ap:ident++)? ; $($tok:tt)*) => {
        let body = InstructionBody::Jump(JumpInstruction {
            target: $crate::deref_or_immediate!($target),
            relative: true,
        });
        $crate::append_instruction!($ctx, body $(,$ap++)?);
        $crate::casm_extend!($ctx, $($tok)*)
    };
    ($ctx:ident, jmp abs $target:tt $(,$ap:ident++)? ; $($tok:tt)*) => {
        let body = InstructionBody::Jump(JumpInstruction {
            target: $crate::deref_or_immediate!($target),
            relative: false,
        });
        $crate::append_instruction!($ctx, body $(,$ap++)?);
        $crate::casm_extend!($ctx, $($tok)*)
    };
    ($ctx:ident, jnz rel $target:tt if $cond:tt $(,$ap:ident++)? ; $($tok:tt)*) => {
        let body = InstructionBody::Jnz(JnzInstruction {
            jump_offset: $crate::deref_or_immediate!($target),
            condition: $crate::deref!($cond),
        });
        $crate::append_instruction!($ctx, body $(,$ap++)?);
        $crate::casm_extend!($ctx, $($tok)*)
    };
    ($ctx:ident, jnz $target:tt if $cond:tt $(,$ap:ident++)? ; $($tok:tt)*) => {
        let body = InstructionBody::Jnz(JnzInstruction {
            jump_offset: $crate::deref_or_immediate!($target),
            condition: $crate::deref!($cond),
        });
        $crate::append_instruction!($ctx, body $(,$ap++)?);
        $crate::casm_extend!($ctx, $($tok)*)
    };
    ($ctx:ident, ap += $operand:tt $(,$ap:ident++)? ; $($tok:tt)*) => {
        let body = InstructionBody::AddAp(AddApInstruction {
            operand: $crate::res!($operand),
        });
        $crate::append_instruction!($ctx, body $(,$ap++)?);
        $crate::casm_extend!($ctx, $($tok)*)
    };
    ($ctx:ident, %{ memory $dst:tt = segments . add ( ) %} $($tok:tt)*) => {
        $ctx.current_hints.push($crate::hints::Hint::AllocSegment{dst: $crate::deref!($dst)});
        $crate::casm_extend!($ctx, $($tok)*)
    };
    ($ctx:ident, %{ memory [ ap + 0 ] = memory $lhs:tt < memory $rhs:tt %} $($tok:tt)*) => {
        $ctx.current_hints.push($crate::hints::Hint::TestLessThan{
            lhs: $crate::deref!($lhs).into(),
            rhs: $crate::deref!($rhs).into(),
        });
        $crate::casm_extend!($ctx, $($tok)*)
    };
    ($ctx:ident, %{ memory [ ap + 0 ] = memory $lhs:tt < $rhs:tt %} $($tok:tt)*) => {
        $ctx.current_hints.push($crate::hints::Hint::TestLessThan{
            lhs: $crate::deref!($lhs).into(),
            rhs: $crate::operand::DerefOrImmediate::Immediate($rhs),
        });
        $crate::casm_extend!($ctx, $($tok)*)
    };
    ($ctx:ident, %{ memory [ ap + 0 ] = $lhs:tt < memory $rhs:tt %} $($tok:tt)*) => {
        $ctx.current_hints.push($crate::hints::Hint::TestLessThan{
            lhs: $crate::operand::DerefOrImmediate::Immediate($lhs),
            rhs: $crate::deref!($rhs).into(),
        });
        $crate::casm_extend!($ctx, $($tok)*)
    };
}

#[macro_export]
macro_rules! append_instruction {
    ($ctx:ident, $body:ident) => {
        let instr = Instruction { body: $body, inc_ap: false, hints: $ctx.current_hints };
        $ctx.current_code_offset += instr.body.op_size();
        $ctx.current_hints = vec![];
        $ctx.instructions.push(instr);
    };
    ($ctx:ident, $body:ident,ap + +) => {
        let instr = Instruction { body: $body, inc_ap: true, hints: $ctx.current_hints };
        $ctx.current_code_offset += instr.body.op_size();
        $ctx.current_hints = vec![];
        $ctx.instructions.push(instr);
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
        CellRef { register: $crate::reg!($reg), offset: $offset }
    };
    ([$reg:ident - $offset:expr]) => {
        CellRef { register: $crate::reg!($reg), offset: -$offset }
    };
    ([$reg:ident]) => {
        CellRef { register: $crate::reg!($reg), offset: 0 }
    };
    ($a:expr) => {
        $a
    };
}

#[macro_export]
macro_rules! reg {
    (ap) => {
        Register::AP
    };
    (fp) => {
        Register::FP
    };
}

#[macro_export]
macro_rules! deref_or_immediate {
    ($a:literal) => {
        DerefOrImmediate::Immediate($a)
    };
    ([$a:ident $($op:tt $offset:expr)?]) => {
        DerefOrImmediate::Deref($crate::deref!([$a $($op $offset)?]))
    };
    ($a:expr) => {
        DerefOrImmediate::from($a)
    };
}

#[macro_export]
macro_rules! res {
    ($a:tt + $b:tt) => {
        ResOperand::BinOp(BinOpOperand {
            op: Operation::Add,
            a: $crate::deref!($a),
            b: $crate::deref_or_immediate!($b),
        })
    };
    ($a:tt * $b:tt) => {
        ResOperand::BinOp(BinOpOperand {
            op: Operation::Mul,
            a: $crate::deref!($a),
            b: $crate::deref_or_immediate!($b),
        })
    };
    ([[$a:expr]]) => {
        ResOperand::DoubleDeref($a)
    };
    ($a:tt) => {
        ResOperand::from($crate::deref_or_immediate!($a))
    };
}
