use std::collections::hash_map::Entry;
use std::collections::HashMap;

use num_bigint::BigInt;
use utils::extract_matches;

use crate::ap_change::ApplyApChange;
use crate::deref_or_immediate;
use crate::hints::Hint;
use crate::instructions::{
    AddApInstruction, AssertEqInstruction, Instruction, InstructionBody, JnzInstruction,
    JumpInstruction,
};
use crate::operand::{BinOpOperand, CellRef, DerefOrImmediate, Operation, Register, ResOperand};

#[cfg(test)]
#[path = "builder_test.rs"]
mod test;

/// Variables for casm builder, representing a `ResOperand`.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct Var(usize);

/// The state of the variables at some line.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct State {
    /// The value per variable.
    vars: HashMap<Var, ResOperand>,
    /// The number of allocated variables from the begining of the run.
    allocated: i16,
    /// The AP change since the beginging of the run.
    pub ap_change: usize,
}
impl State {
    /// Returns the value, in relation to the initial ap value.
    fn get_value(&self, var: Var) -> ResOperand {
        self.vars[&var].clone()
    }

    /// Returns the value, in relation to the current ap value.
    pub fn get_adjusted(&self, var: Var) -> ResOperand {
        self.get_value(var).unchecked_apply_known_ap_change(self.ap_change)
    }

    /// Returns the value, assumming it is a direct cell reference.
    pub fn get_adjusted_as_cell_ref(&self, var: Var) -> CellRef {
        extract_matches!(self.get_adjusted(var), ResOperand::Deref)
    }

    /// Validates that the state is valid, as it had enough ap change.
    fn validate_finality(&self) {
        assert!(
            self.ap_change >= self.allocated as usize,
            "Not enough commands to update ap, add `add_ap` calls."
        );
    }

    /// Intersect the states of branches leading to the same label, validating that the states can
    /// intersect.
    fn intersect(&mut self, other: &Self) {
        assert_eq!(self.ap_change, other.ap_change, "Merged branches not aligned on AP change.");
        assert_eq!(
            self.allocated, other.allocated,
            "Merged branches not aligned on number of allocations."
        );
        self.vars.retain(|var, value| {
            other
                .vars
                .get(var)
                .map(|x| assert_eq!(x, value, "Var mismatch between branches."))
                .is_some()
        });
    }
}

/// A statement added to the builder.
enum Statement {
    /// A final instruction, no need for further editting.
    Final(Instruction),
    /// A jump command, requires fixing the actual target label.
    Jump(String, Instruction),
    /// A target label for jumps.
    Label(String),
}

/// The builder result.
pub struct CasmBuildResult {
    /// The actual casm code.
    pub instructions: Vec<Instruction>,
    /// The set of instructions still requiring relocations.
    pub awaiting_relocations: Vec<usize>,
    /// The state at a point of jumping into a label, per label.
    pub label_state: HashMap<String, State>,
    /// The state at the last added statement.
    pub fallthrough_state: State,
}

/// Builder to more easily write casm code without specifically thinking about ap changes and the
/// sizes of opcodes. Wrong usages of it would panic instead of returning a result, as this builder
/// assumes we are in a post validation of parameters stage.
pub struct CasmBuilder {
    /// The state at a point of jumping into a label, per label.
    label_state: HashMap<String, State>,
    /// The state at the last added statement.
    main_state: State,
    /// The added statements.
    statements: Vec<Statement>,
    /// The current set of added hints.
    current_hints: Vec<Hint>,
    /// The number of vars created. Used to not reuse var names.
    var_count: usize,
    /// Is the current state reachable.
    /// Example for unreachable state is after a unconditional jump, before any label is stated.
    reachable: bool,
}
impl CasmBuilder {
    /// Finalizes the builder.
    pub fn build(mut self) -> CasmBuildResult {
        assert!(
            self.current_hints.is_empty(),
            "Build cannot be called with hints as the last addition."
        );
        self.main_state.validate_finality();
        let label_offsets = self.compute_label_offsets();
        let mut instructions = vec![];
        let mut awaiting_relocations = vec![];
        let mut offset = 0;
        for statement in self.statements {
            match statement {
                Statement::Final(inst) => {
                    offset += inst.body.op_size();
                    instructions.push(inst);
                }
                Statement::Jump(label, mut inst) => {
                    match label_offsets.get(&label) {
                        Some(label_offset) => match &mut inst.body {
                            InstructionBody::Jnz(JnzInstruction {
                                jump_offset: DerefOrImmediate::Immediate(value),
                                ..
                            })
                            | InstructionBody::Jump(JumpInstruction {
                                target: DerefOrImmediate::Immediate(value),
                                ..
                            }) => {
                                // Updating the value, instead of assigning into it, to avoid
                                // allocating a BigInt since it is already 0.
                                *value += *label_offset;
                                *value -= offset;
                            }
                            _ => unreachable!("Only jump statements should be here."),
                        },
                        None => {
                            awaiting_relocations.push(instructions.len());
                        }
                    }
                    offset += inst.body.op_size();
                    instructions.push(inst);
                }
                Statement::Label(name) => {
                    self.label_state.remove(&name);
                }
            }
        }
        for state in self.label_state.values() {
            state.validate_finality();
        }
        CasmBuildResult {
            instructions,
            awaiting_relocations,
            label_state: self.label_state,
            fallthrough_state: self.main_state,
        }
    }

    /// Computes the code offsets of all the labels.
    fn compute_label_offsets(&self) -> HashMap<String, usize> {
        let mut label_offsets = HashMap::<String, usize>::default();
        let mut offset = 0;
        for statement in &self.statements {
            match statement {
                Statement::Final(inst) | Statement::Jump(_, inst) => {
                    offset += inst.body.op_size();
                }
                Statement::Label(name) => {
                    label_offsets.insert(name.clone(), offset);
                }
            }
        }
        label_offsets
    }

    /// Adds a variable pointing to `value`.
    pub fn add_var(&mut self, value: ResOperand) -> Var {
        let var = Var(self.var_count);
        self.var_count += 1;
        self.main_state.vars.insert(var, value);
        var
    }

    /// Allocates a new variable in memory.
    pub fn alloc_var(&mut self) -> Var {
        let var = self.add_var(ResOperand::Deref(CellRef {
            offset: self.main_state.allocated,
            register: Register::AP,
        }));
        self.main_state.allocated += 1;
        var
    }

    /// Adds a hint, generated from `inputs` which are cell refs or immediates and `outputs` which
    /// must be cell refs.
    pub fn add_hint<
        const INPUTS_COUNT: usize,
        const OUTPUTS_COUNT: usize,
        F: FnOnce([DerefOrImmediate; INPUTS_COUNT], [CellRef; OUTPUTS_COUNT]) -> Hint,
    >(
        &mut self,
        f: F,
        inputs: [Var; INPUTS_COUNT],
        outputs: [Var; OUTPUTS_COUNT],
    ) {
        self.current_hints.push(f(
            inputs.map(|v| self.as_deref_or_imm(v, true)),
            outputs.map(|v| self.as_cell_ref(v, true)),
        ));
    }

    /// Adds a hint, generated from `inputs` which are cell refs or immediates and `outputs` which
    /// must be cell refs.
    pub fn add_buffer_hint<F: FnOnce(ResOperand) -> Hint>(&mut self, f: F, buffer: Var) {
        self.current_hints.push(f(self.get_value(buffer, true)));
    }

    /// Adds an assertion that `dst = res`.
    /// `dst` must be a cell reference.
    pub fn assert_vars_eq(&mut self, dst: Var, res: Var) {
        let a = self.as_cell_ref(dst, true);
        let b = self.get_value(res, true);
        let instruction =
            self.get_instruction(InstructionBody::AssertEq(AssertEqInstruction { a, b }), true);
        self.statements.push(Statement::Final(instruction));
    }

    /// Writes and increments a buffer.
    /// Useful for RangeCheck and similar buffers.
    /// `buffer` must be a cell reference, or a cell reference with a small added constant.
    /// `value` must be a cell reference.
    pub fn buffer_write_and_inc(&mut self, buffer: Var, value: Var) {
        let (cell, offset) = self.buffer_get_and_inc(buffer);
        let location = self.add_var(ResOperand::DoubleDeref(cell, offset));
        self.assert_vars_eq(value, location);
    }

    /// Increments a buffer and allocates and returns variable pointing to its previous value.
    pub fn get_ref_and_inc(&mut self, buffer: Var) -> Var {
        let (cell, offset) = self.as_cell_ref_plus_const(buffer, 0, false);
        self.main_state.vars.insert(
            buffer,
            ResOperand::BinOp(BinOpOperand {
                op: Operation::Add,
                a: cell,
                b: deref_or_immediate!(BigInt::from(offset) + 1),
            }),
        );
        self.add_var(ResOperand::DoubleDeref(cell, offset))
    }

    /// Increments a buffer and returning the previous value it pointed to.
    /// Useful for writing, reading and referencing values.
    /// `buffer` must be a cell reference, or a cell reference with a small added constant.
    fn buffer_get_and_inc(&mut self, buffer: Var) -> (CellRef, i16) {
        let (base, offset) = match self.get_value(buffer, false) {
            ResOperand::Deref(cell) => (cell, 0),
            ResOperand::BinOp(BinOpOperand {
                op: Operation::Add,
                a,
                b: DerefOrImmediate::Immediate(imm),
            }) => (a, imm.try_into().expect("Too many buffer writes.")),
            _ => panic!("Not a valid buffer."),
        };
        self.main_state.vars.insert(
            buffer,
            ResOperand::BinOp(BinOpOperand {
                op: Operation::Add,
                a: base,
                b: deref_or_immediate!(offset + 1),
            }),
        );
        (base, offset)
    }

    /// Increases AP by `size`.
    pub fn add_ap(&mut self, size: usize) {
        let instruction = self.get_instruction(
            InstructionBody::AddAp(AddApInstruction {
                operand: ResOperand::Immediate(BigInt::from(size)),
            }),
            false,
        );
        self.statements.push(Statement::Final(instruction));
        self.main_state.ap_change += size;
    }

    /// Returns a variable that is the `op` of `lhs` and `rhs`.
    /// `lhs` must be a cell reference and `rhs` must be deref or immediate.
    pub fn bin_op(&mut self, op: Operation, lhs: Var, rhs: Var) -> Var {
        self.add_var(ResOperand::BinOp(BinOpOperand {
            op,
            a: self.as_cell_ref(lhs, false),
            b: self.as_deref_or_imm(rhs, false),
        }))
    }

    /// Returns a variable that is `[[var] + offset]`.
    /// `var` must be a cell reference, or a cell ref plus a small constant.
    pub fn double_deref(&mut self, var: Var, offset: i16) -> Var {
        let (cell, full_offset) = self.as_cell_ref_plus_const(var, offset, false);
        self.add_var(ResOperand::DoubleDeref(cell, full_offset))
    }

    /// Sets the label to have the set states, otherwise tests if the state matches the existing one
    /// by merging.
    fn set_or_test_label_state(&mut self, label: String, state: State) {
        match self.label_state.entry(label) {
            Entry::Occupied(mut e) => {
                e.get_mut().intersect(&state);
            }
            Entry::Vacant(e) => {
                e.insert(state);
            }
        }
    }

    /// Add a statement to jump to `label`.
    pub fn jump(&mut self, label: String) {
        let instruction = self.get_instruction(
            InstructionBody::Jump(JumpInstruction {
                target: deref_or_immediate!(0),
                relative: true,
            }),
            true,
        );
        self.statements.push(Statement::Jump(label.clone(), instruction));
        let mut state = State::default();
        std::mem::swap(&mut state, &mut self.main_state);
        self.label_state.insert(label, state);
        self.reachable = false;
    }

    /// Add a statement to jump to `label` if `condition != 0`.
    /// `condition` must be a cell reference.
    pub fn jump_nz(&mut self, condition: Var, label: String) {
        let cell = self.as_cell_ref(condition, true);
        let instruction = self.get_instruction(
            InstructionBody::Jnz(JnzInstruction {
                condition: cell,
                jump_offset: deref_or_immediate!(0),
            }),
            true,
        );
        self.statements.push(Statement::Jump(label.clone(), instruction));
        self.set_or_test_label_state(label, self.main_state.clone());
    }

    /// Adds a label here named `name`.
    pub fn label(&mut self, name: String) {
        if self.reachable {
            self.set_or_test_label_state(name.clone(), self.main_state.clone());
        } else {
            self.main_state = self
                .label_state
                .get(&name)
                .unwrap_or_else(|| panic!("No known value for state on reaching {name}."))
                .clone();
        }
        self.statements.push(Statement::Label(name));
        self.reachable = true;
    }

    /// Returns `var`s value, with fixed ap if `adjust_ap` is true.
    fn get_value(&self, var: Var, adjust_ap: bool) -> ResOperand {
        if adjust_ap { self.main_state.get_adjusted(var) } else { self.main_state.get_value(var) }
    }

    /// Returns `var`s value as a cell reference, with fixed ap if `adjust_ap` is true.
    fn as_cell_ref(&self, var: Var, adjust_ap: bool) -> CellRef {
        extract_matches!(self.get_value(var, adjust_ap), ResOperand::Deref)
    }

    /// Returns `var`s value as a cell reference or immediate, with fixed ap if `adjust_ap` is true.
    fn as_deref_or_imm(&self, var: Var, adjust_ap: bool) -> DerefOrImmediate {
        match self.get_value(var, adjust_ap) {
            ResOperand::Deref(cell) => DerefOrImmediate::Deref(cell),
            ResOperand::Immediate(imm) => DerefOrImmediate::Immediate(imm),
            ResOperand::DoubleDeref(_, _) | ResOperand::BinOp(_) => panic!("wrong usage."),
        }
    }

    /// Returns `var`s value as a cell reference plus a small const offset, with fixed ap if
    /// `adjust_ap` is true.
    fn as_cell_ref_plus_const(
        &self,
        var: Var,
        additional_offset: i16,
        adjust_ap: bool,
    ) -> (CellRef, i16) {
        match self.get_value(var, adjust_ap) {
            ResOperand::Deref(cell) => (cell, 0),
            ResOperand::BinOp(BinOpOperand {
                op: Operation::Add,
                a,
                b: DerefOrImmediate::Immediate(imm),
            }) => (a, (imm + additional_offset).try_into().expect("Offset too large for deref.")),
            _ => panic!("Not a valid ptr."),
        }
    }

    /// Returns an instruction wrapping the instruction body.
    /// If `inc_ap_supported` may add an `ap++` to the instruction.
    fn get_instruction(&mut self, body: InstructionBody, inc_ap_supported: bool) -> Instruction {
        let inc_ap =
            inc_ap_supported && self.main_state.allocated as usize > self.main_state.ap_change;
        if inc_ap {
            self.main_state.ap_change += 1;
        }
        let mut hints = vec![];
        std::mem::swap(&mut hints, &mut self.current_hints);
        Instruction { body, inc_ap, hints }
    }
}

impl Default for CasmBuilder {
    fn default() -> Self {
        Self {
            label_state: Default::default(),
            main_state: Default::default(),
            statements: Default::default(),
            current_hints: Default::default(),
            var_count: Default::default(),
            reachable: true,
        }
    }
}

#[macro_export]
macro_rules! casm_build_extend {
    ($builder:ident,) => {};
    ($builder:ident, tempvar $var:ident; $($tok:tt)*) => {
        let $var = $builder.alloc_var();
        $crate::casm_build_extend!($builder, $($tok)*)
    };
    ($builder:ident, ap += $value:expr; $($tok:tt)*) => {
        $builder.add_ap($value);
        $crate::casm_build_extend!($builder, $($tok)*)
    };
    ($builder:ident, assert $dst:ident = $res:ident; $($tok:tt)*) => {
        $builder.assert_vars_eq($dst, $res);
        $crate::casm_build_extend!($builder, $($tok)*)
    };
    ($builder:ident, assert $dst:ident = $a:ident + $b:ident; $($tok:tt)*) => {
        {
            let __sum = $builder.bin_op($crate::operand::Operation::Add, $a, $b);
            $builder.assert_vars_eq($dst, __sum);
        }
        $crate::casm_build_extend!($builder, $($tok)*)
    };
    ($builder:ident, assert $dst:ident = $a:ident * $b:ident; $($tok:tt)*) => {
        {
            let __product = $builder.bin_op($crate::operand::Operation::Mul, $a, $b);
            $builder.assert_vars_eq($dst, __product);
        }
        $crate::casm_build_extend!($builder, $($tok)*)
    };
    ($builder:ident, assert * ( $buffer:ident ++ ) = $value:ident; $($tok:tt)*) => {
        $builder.buffer_write_and_inc($buffer, $value);
        $crate::casm_build_extend!($builder, $($tok)*)
    };
    ($builder:ident, let $dst:ident = $a:ident + $b:ident; $($tok:tt)*) => {
        let $dst = $builder.bin_op($crate::operand::Operation::Add, $a, $b);
        $crate::casm_build_extend!($builder, $($tok)*)
    };
    ($builder:ident, let $dst:ident = $a:ident * $b:ident; $($tok:tt)*) => {
        let $dst = $builder.bin_op($crate::operand::Operation::Mul, $a, $b);
        $crate::casm_build_extend!($builder, $($tok)*)
    };
    ($builder:ident, let $dst:ident = * ( $buffer:ident ++ ); $($tok:tt)*) => {
        let $dst = $builder.get_ref_and_inc($buffer);
        $crate::casm_build_extend!($builder, $($tok)*)
    };
    ($builder:ident, let $dst:ident = $buffer:ident [ $offset:ident ] ; $($tok:tt)*) => {
        let $dst = $builder.double_deref($buffer, $offset);
        $crate::casm_build_extend!($builder, $($tok)*)
    };
    ($builder:ident, jump $target:ident; $($tok:tt)*) => {
        $builder.jump(std::stringify!($target).to_owned());
        $crate::casm_build_extend!($builder, $($tok)*)
    };
    ($builder:ident, jump $target:ident if $condition:ident != 0; $($tok:tt)*) => {
        $builder.jump_nz($condition, std::stringify!($target).to_owned());
        $crate::casm_build_extend!($builder, $($tok)*)
    };
    ($builder:ident, $label:ident: $($tok:tt)*) => {
        $builder.label(std::stringify!($label).to_owned());
        $crate::casm_build_extend!($builder, $($tok)*)
    };
    ($builder:ident, hint $hint_name:ident {
            $($input_name:ident : $input_value:ident),*
        } into {
            $($output_name:ident : $output_value:ident),*
        }; $($tok:tt)*) => {
        $builder.add_hint(
            |[$($input_name),*], [$($output_name),*]| $crate::hints::Hint::$hint_name {
                $($input_name,)* $($output_name,)*
            },
            [$($input_value,)*],
            [$($output_value,)*],
        );
        $crate::casm_build_extend!($builder, $($tok)*)
    };
    ($builder:ident, hint $hint_name:ident {
        $buffer_name:ident : $buffer_value:ident
    }; $($tok:tt)*) => {
        $builder.add_buffer_hint(
            |$buffer_name| $crate::hints::Hint::$hint_name { $buffer_name },
            $buffer_value,
        );
        $crate::casm_build_extend!($builder, $($tok)*)
    };
}
