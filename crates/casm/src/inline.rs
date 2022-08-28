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
            $crate::casm_inner!(ctx, $($tok)*);
            ctx
        }
    }
}

#[macro_export]
macro_rules! casm_inner {
    ($ctx:ident,) => {};
    ($ctx:ident, $dst:tt = $a:tt $(+ $b0:tt)? $(* $b1:tt)? $(,$ap:ident++)? ; $($tok:tt)*) => {
        let body = InstructionBody::AssertEq(AssertEqInstruction {
            a: $crate::deref!($dst),
            b: $crate::res!($a $(+ $b0)? $(* $b1)?),
        });
        $crate::append_instruction!($ctx, body $(,$ap++)?);
        $crate::casm_inner!($ctx, $($tok)*)
    };
    ($ctx:ident, call rel $target:tt $(,$ap:ident++)? ; $($tok:tt)*) => {
        let body = InstructionBody::Call(CallInstruction {
            target: $crate::deref_or_immediate!($target),
            relative: true,
        });
        $crate::append_instruction!($ctx, body $(,$ap++)?);
        $crate::casm_inner!($ctx, $($tok)*)
    };
    ($ctx:ident, call abs $target:tt $(,$ap:ident++)? ; $($tok:tt)*) => {
        let body = InstructionBody::Call(CallInstruction {
            target: $crate::deref_or_immediate!($target),
            relative: false,
        });
        $crate::append_instruction!($ctx, body $(,$ap++)?);
        $crate::casm_inner!($ctx, $($tok)*)
    };
}

#[macro_export]
macro_rules! append_instruction {
    ($ctx:ident, $body:ident) => {
        let instr = Instruction { body: $body, inc_ap: false };
        $ctx.current_code_offset += instr.body.op_size();
        $ctx.instructions.push(instr);
    };
    ($ctx:ident, $body:ident,ap + +) => {
        let instr = Instruction { body: $body, inc_ap: true };
        $ctx.current_code_offset += instr.body.op_size();
        $ctx.instructions.push(instr);
    };
}

#[allow(dead_code)]
#[derive(Default)]
struct CasmContext {
    current_code_offset: usize,
    instructions: Vec<Instruction>,
    // TODO(spapini): Branches.
}

#[macro_export]
macro_rules! deref {
    ([$reg:ident + $offset:expr]) => {
        DerefOperand { register: $crate::reg!($reg), offset: $offset }
    };
    ([$reg:ident - $offset:expr]) => {
        DerefOperand { register: $crate::reg!($reg), offset: -$offset }
    };
    ([$reg:ident]) => {
        DerefOperand { register: $crate::reg!($reg), offset: 0 }
    };
    ($a:ident) => {
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
        DerefOrImmediate::Immediate(ImmediateOperand { value: $a })
    };
    ($a:tt) => {
        DerefOrImmediate::Deref($crate::deref!($a))
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
            op: Operation::Add,
            a: $crate::deref!($a),
            b: $crate::deref_or_immediate!($b),
        })
    };
    ([$a:tt]) => {
        todo!()
    };
    ($a:ident) => {
        $a
    };
    ($a:literal) => {
        ResOperand::Immediate(ImmediateOperand { value: $a })
    };
    ($a:tt) => {
        ResOperand::Deref($crate::deref!($a))
    };
}
