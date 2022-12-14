use std::collections::hash_map::Entry;
use std::collections::HashMap;

use num_bigint::BigInt;
use utils::extract_matches;

use crate::ap_change::ApplyApChange;
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
    pub fn get_fixed(&self, var: Var) -> ResOperand {
        self.get_value(var).unchecked_apply_known_ap_change(self.ap_change)
    }

    /// Returns the value, assumming it is a direct cell reference.
    pub fn get_fixed_as_cell_ref(&self, var: Var) -> CellRef {
        extract_matches!(self.get_fixed(var), ResOperand::Deref)
    }

    /// Validates that the state is valid, as it had enough ap change.
    fn validate_finality(&self) {
        assert!(
            self.ap_change >= self.allocated as usize,
            "Not enough commands to update ap, add `add_ap` calls."
        );
    }

    /// Merges states, validating that the states are mergable.
    fn merge(&mut self, other: &Self) {
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
    next_hints: Vec<Hint>,
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
            self.next_hints.is_empty(),
            "Build cannot be called with hints as the last addition."
        );
        self.main_state.validate_finality();
        for state in self.label_state.values() {
            state.validate_finality();
        }
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
        let mut instructions = vec![];
        let mut awaiting_relocations = vec![];
        let mut offset = 0;
        let mut idx = 0;
        for statement in self.statements {
            match statement {
                Statement::Final(inst) => {
                    offset += inst.body.op_size();
                    idx += 1;
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
                                *value += label_offset - offset;
                            }
                            _ => unreachable!("Only jumps statements should be here."),
                        },
                        None => {
                            awaiting_relocations.push(idx);
                        }
                    }
                    offset += inst.body.op_size();
                    idx += 1;
                    instructions.push(inst);
                }
                Statement::Label(name) => {
                    self.label_state.remove(&name);
                }
            }
        }
        CasmBuildResult {
            instructions,
            awaiting_relocations,
            label_state: self.label_state,
            fallthrough_state: self.main_state,
        }
    }

    /// Adds a variable pointing to `value`.
    pub fn add_var(&mut self, value: ResOperand) -> Var {
        let var = Var(self.var_count);
        self.var_count += 1;
        self.main_state.vars.insert(var, value);
        var
    }

    /// Allocates a new varialbe in memory.
    pub fn alloc_var(&mut self) -> Var {
        let var = self.add_var(ResOperand::Deref(CellRef {
            offset: self.main_state.allocated,
            register: Register::AP,
        }));
        self.main_state.allocated += 1;
        var
    }

    /// Adds a hint for `dst = lhs < rhs`.
    pub fn add_less_than_hint(&mut self, lhs: Var, rhs: Var, dst: Var) {
        self.next_hints.push(Hint::TestLessThan {
            lhs: self.as_deref_or_imm(lhs, true),
            rhs: self.as_deref_or_imm(rhs, true),
            dst: self.as_cell_ref(dst, true),
        });
    }

    /// Adds a hint for `(quotient, remainder) = divmod(lhs, rhs)`.
    pub fn add_divmod_hint(&mut self, lhs: Var, rhs: Var, quotient: Var, remainder: Var) {
        self.next_hints.push(Hint::DivMod {
            lhs: self.as_deref_or_imm(lhs, true),
            rhs: self.as_deref_or_imm(rhs, true),
            quotient: self.as_cell_ref(quotient, true),
            remainder: self.as_cell_ref(remainder, true),
        });
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

    /// Returns a variable that is the sum of `lhs` and `rhs`.
    /// `lhs` must be a cell reference and `rhs` must be deref or immediate.
    pub fn bin_op(&mut self, op: Operation, lhs: Var, rhs: Var) -> Var {
        self.add_var(ResOperand::BinOp(BinOpOperand {
            op,
            a: self.as_cell_ref(lhs, false),
            b: self.as_deref_or_imm(rhs, false),
        }))
    }

    /// Returns a variable that is `[[var] + offset]`.
    /// `var` must be a cell reference.
    pub fn double_deref(&mut self, var: Var, offset: i16) -> Var {
        self.add_var(ResOperand::DoubleDeref(self.as_cell_ref(var, false), offset))
    }

    /// Sets the label to have the set states, otherwise tests if the state matches the existing one
    /// by merging.
    fn set_or_test_label_state(&mut self, label: String, state: State) {
        match self.label_state.entry(label) {
            Entry::Occupied(mut e) => {
                e.get_mut().merge(&state);
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
                target: DerefOrImmediate::Immediate(BigInt::from(0)),
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
                jump_offset: DerefOrImmediate::Immediate(BigInt::from(0)),
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

    /// Returns `var`s value, with fixed ap if `fix` is true.
    fn get_value(&self, var: Var, fix: bool) -> ResOperand {
        if fix { self.main_state.get_fixed(var) } else { self.main_state.get_value(var) }
    }

    /// Returns `var`s value as a cell reference, with fixed ap if `fix` is true.
    fn as_cell_ref(&self, var: Var, fix: bool) -> CellRef {
        extract_matches!(self.get_value(var, fix), ResOperand::Deref)
    }

    /// Returns `var`s value as a cell reference or immediate, with fixed ap if `fix` is true.
    fn as_deref_or_imm(&self, var: Var, fix: bool) -> DerefOrImmediate {
        match self.get_value(var, fix) {
            ResOperand::Deref(cell) => DerefOrImmediate::Deref(cell),
            ResOperand::Immediate(imm) => DerefOrImmediate::Immediate(imm),
            ResOperand::DoubleDeref(_, _) | ResOperand::BinOp(_) => panic!("wrong usage."),
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
        std::mem::swap(&mut hints, &mut self.next_hints);
        Instruction { body, inc_ap, hints }
    }
}

impl Default for CasmBuilder {
    fn default() -> Self {
        Self {
            label_state: Default::default(),
            main_state: Default::default(),
            statements: Default::default(),
            next_hints: Default::default(),
            var_count: Default::default(),
            reachable: true,
        }
    }
}
