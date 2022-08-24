use crate::instructions::*;

#[cfg(test)]
#[path = "inline_test.rs"]
mod test;

#[macro_export]
macro_rules! casm {
    {$($opcode:ident $args:tt $(, $ap_inc:ident ++)?;)*} => {
        {
            use $crate::instructions::*;
            use $crate::inline::CasmContext;
            let mut ctx = CasmContext::default();
            $(
                ctx.instructions.push(Instruction {
                    body: $crate::$opcode!(ctx, $args),
                    inc_ap: $crate::ap_inc!($($ap_inc)?),
                });
            )*
            ctx
        }
    }
}

#[macro_export]
macro_rules! ap_inc {
    (ap) => {
        true
    };
    () => {
        false
    };
}

#[allow(dead_code)]
#[derive(Default)]
struct CasmContext {
    instructions: Vec<Instruction>,
    // TODO(spapini): Branches.
}

#[macro_export]
macro_rules! assert {
    ($ctx:ident,($dst:tt = $res:tt)) => {
        InstructionBody::AssertEq(AssertEqInstruction {
            a: $crate::deref!($dst),
            b: $crate::res!($res),
        })
    };
}

#[macro_export]
macro_rules! call {
    ($ctx:ident,(rel $target:tt)) => {
        InstructionBody::Call(CallInstruction {
            target: $crate::deref_or_immediate!($target),
            relative: true,
        })
    };
    ($ctx:ident,(abs $target:tt)) => {
        InstructionBody::Call(CallInstruction {
            target: $crate::deref_or_immediate!($target),
            relative: false,
        })
    };
}

#[macro_export]
macro_rules! deref {
    ([$reg:ident + $offset:expr]) => {
        DerefOperand { register: Register::$reg, offset: $offset }
    };
    ([$reg:ident - $offset:expr]) => {
        DerefOperand { register: Register::$reg, offset: -$offset }
    };
    ([$reg:ident]) => {
        DerefOperand { register: Register::$reg, offset: 0 }
    };
    ($a:ident) => {
        $a
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
    (($a:tt + $b:tt)) => {
        ResOperand::BinOp(BinOpOperand {
            op: Operation::Add,
            a: $crate::deref!($a),
            b: $crate::deref_or_immediate!($b),
        })
    };
    (($a:tt * $b:tt)) => {
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
