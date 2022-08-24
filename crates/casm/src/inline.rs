use std::collections::HashMap;

use crate::instructions::*;

#[cfg(test)]
#[path = "inline_test.rs"]
mod test;

#[allow(dead_code)]
#[derive(Default)]
struct CasmContext {
    pc: usize,
    instructions: Vec<Instruction>,
    labels: HashMap<&'static str, usize>,
    // TODO(spapini): Branches.
}

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
        $crate::add_inst!($ctx, body $(,$ap++)?);
        $crate::casm_inner!($ctx, $($tok)*)
    };
    ($ctx:ident, call rel $target:tt $(,$ap:ident++)? ; $($tok:tt)*) => {
        let body = InstructionBody::Call(CallInstruction {
            target: $crate::label_or_deref_or_immediate!($ctx, $target),
            relative: true,
        });
        $crate::add_inst!($ctx, body $(,$ap++)?);
        $crate::casm_inner!($ctx, $($tok)*)
    };
    ($ctx:ident, call abs $target:tt $(,$ap:ident++)? ; $($tok:tt)*) => {
        let body = InstructionBody::Call(CallInstruction {
            target: $crate::deref_or_immediate!($target),
            relative: false,
        });
        $crate::add_inst!($ctx, body $(,$ap++)?);
        $crate::casm_inner!($ctx, $($tok)*)
    };
    ($ctx:ident, $label:ident : $($tok:tt)*) => {
        let name: &'static str = stringify!($label);
        if $ctx.labels.insert(name, $ctx.pc).is_some() {
            panic!("Redefinition of label {name}");
        }
        $crate::casm_inner!($ctx, $($tok)*)
    };
}

#[macro_export]
macro_rules! add_inst {
    ($ctx:ident, $body:ident) => {
        let instr = Instruction { body: $body, inc_ap: false };
        $ctx.pc += instr.body.op_size();
        $ctx.instructions.push(instr);
    };
    ($ctx:ident, $body:ident,ap + +) => {
        let instr = Instruction { body: $body, inc_ap: true };
        $ctx.pc += instr.body.op_size();
        $ctx.instructions.push(instr);
    };
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
macro_rules! label_or_deref_or_immediate {
    ($ctx:ident, $a:ident) => {{
        let target = $ctx.labels[stringify!($a)] as i128;
        let rel_jump = target - ($ctx.pc as i128);
        DerefOrImmediate::Immediate(ImmediateOperand { value: rel_jump })
    }};
    ($ctx:ident, $a:tt) => {
        $crate::deref_or_immediate!($a)
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
