use std::hash::Hash;
use std::{fs, iter};
use std::cmp::{min, max, Ordering};
use std::path::{Path, PathBuf};
use cairo_lang_casm::cell_expression::{CellExpression, CellOperator};
use itertools::Itertools;
use num_bigint::BigInt;
use num_traits::cast::ToPrimitive;

use std::collections::HashMap;
use std::collections::HashSet;

use cairo_lang_casm::builder::{
    CasmBuilderAuxiliaryInfo,
    RetBranchDesc,
    StatementDesc,
    VarBaseDesc,
    ExprDesc,
    VarDesc,
    AssertDesc,
    Var, JumpDesc,
};
use cairo_lang_casm::instructions::{InstructionBody, Instruction, AddApInstruction};
use cairo_lang_casm::operand::{ CellRef, DerefOrImmediate, Register, ResOperand };
use cairo_lang_sierra_to_casm::compiler::CairoProgram;
use cairo_lang_utils::bigint::BigIntAsHex;

pub fn func_name_from_test_name(test_name: &str) -> String {
    if test_name.contains('_') {
        // If the test name contains underscores, we assume that all parts of the function
        // name are connected by underscores.
        test_name.split_whitespace().next().unwrap().to_string()
    } else if let Some((prefix, suffix)) = test_name.rsplit_once(' ') {
        if suffix == "libfunc" {
            prefix.replace(" ", "_")
        } else {
            test_name.replace(" ", "_")
        }
    } else {
        test_name.into()
    }
}

pub fn lean_code_name(func_name: &str) -> String {
    String::from(func_name) + "_code"
}

pub fn lean_vm_code_name(func_name: &str) -> String {
    String::from("vm_") + func_name + "_code"
}

pub fn lean_code_file_name(func_name: &str, with_suffix: bool) -> String {
    String::from(func_name) + "_code" + if with_suffix { ".lean" } else { "" }
}

pub fn lean_soundness_spec_file_name(func_name: &str, with_suffix: bool) -> String {
    String::from(func_name) + "_soundness_spec" + if with_suffix { ".lean" } else { "" }
}

pub fn lean_soundness_file_name(func_name: &str, with_suffix: bool) -> String {
    String::from(func_name) + "_soundness" + if with_suffix { ".lean" } else { "" }
}

pub fn lean_completeness_spec_file_name(func_name: &str, with_suffix: bool) -> String {
    String::from(func_name) + "_completeness_spec" + if with_suffix { ".lean" } else { "" }
}

pub fn lean_completeness_file_name(func_name: &str, with_suffix: bool) -> String {
    String::from(func_name) + "_completeness" + if with_suffix { ".lean" } else { "" }
}

pub fn lean_verification_path(test_path: &Path) -> PathBuf {
    test_path.parent().unwrap().join("verification")
}

pub fn lean_file_path(lean_path: &Path, file_name: &str) -> PathBuf {
    lean_path.join(file_name)
}

fn make_subscript(idx: usize) -> String {
    let mut idx = idx;
    let mut res = String::new();
    if idx == 0 {
        return String::from("");
    }
    while idx != 0 {
        res.insert(0, "₀₁₂₃₄₅₆₇₈₉".chars().nth(idx % 10).unwrap());
        idx /= 10;
    }
    res
}

/// This structure stores information about a single block in the function.
/// A block must always begin at a label, except for the main block (which is
/// the whole function).
struct FuncBlock {
    start_pos: usize,
    casm_start_pos: usize,
    pc_start_pos: usize,
    // The ap offset when entering this block.
    ap_offset: usize,
    label: Option<String>,
    args: Vec<String>,
    // Reachable return blocks.
    ret_labels: HashSet<String>,
    // The first ap offset to be allocated in this block. This does not necessarily
    // need to be the same as ap_offset (the offset of the ap pointer at the beginning
    // of the block) because variables may be allocated before/after the ap pointer
    // is advances.
    start_local_ap: usize,
    // Maximal number of rc checks before reaching this block.
    start_rc: usize,
}

impl FuncBlock {
    fn block_suffix(&self) -> String {
        if let Some(label) = &self.label { format!("_{label}") } else { "".into() }
    }

    fn has_args(&self) -> bool {
        0 < self.args.len()
    }
}

struct RetArgs {
    branch_num: usize,
    arg_num: usize,
    arg_names: Vec<String>,
    branch_id_pos: Option<usize>,
    num_implicit_ret_args: usize,
}

impl RetArgs {

    fn get_arg_name_at_pos(branch_desc: &Vec<RetBranchDesc>, pos: usize) -> String {

        if branch_desc.len() == 0 {
            return String::from("ρ");
        }

        let mut arg_name: String = String::new();

        for branch in branch_desc {
            if let Some(name) = branch.get_expr_at_pos(pos) {
                if name.is_empty() {
                    return String::from("ρ");
                }
                if arg_name.is_empty() {
                    arg_name = name;
                } else {
                    if arg_name.find(&name).is_some() {
                        arg_name = name;
                    } else if name.find(&arg_name).is_some() {
                        continue;
                    } else {
                        return String::from("ρ");
                    }
                }
            } else {
                return String::from("ρ");
            };
        }

        if arg_name.is_empty() { String::from("ρ") } else { arg_name }

    }

    /// Returns the argument name at the given position from the end of the name list.
    fn get_arg_name_at_pos_from_end(branch_desc: &Vec<RetBranchDesc>, pos: usize, ignore_at_start: usize) -> String {
        if branch_desc.len() == 0 {
            return String::from("ρ");
        }

        let mut arg_name: String = String::new();

        for branch in branch_desc {
            if let Some(name) = branch.get_expr_at_pos_from_end(pos, ignore_at_start) {
                if name.is_empty() {
                    return String::from("ρ");
                }
                if arg_name.is_empty() {
                    arg_name = name;
                } else {
                    if arg_name.find(&name).is_some() {
                        arg_name = name;
                    } else if name.find(&arg_name).is_some() {
                        continue;
                    } else {
                        arg_name.push_str(&format!("_or_{name}"));
                    }
                }
            }
        }

        if arg_name.is_empty() { String::from("ρ") } else { arg_name }
    }

    fn set_arg_names(&mut self, branch_desc: &Vec<RetBranchDesc>) {
        if self.branch_num == 0 {
            return;
        }
        self.arg_names = (0..self.arg_num).map(
            |pos| {
                if let Some(branch_id_pos) = self.branch_id_pos {
                    if pos == branch_id_pos {
                        String::from("branch_id")
                    } else if pos < branch_id_pos {
                        RetArgs::get_arg_name_at_pos(branch_desc, pos)
                    } else {
                        RetArgs::get_arg_name_at_pos_from_end(
                            branch_desc,
                            self.arg_num - pos - 1,
                            branch_id_pos,
                        )
                    }
                } else {
                    RetArgs::get_arg_name_at_pos(branch_desc, pos)
                }
            }).collect();

        // Duplicate names should be assigned a subscript.
        let mut name_counts: HashMap<&String, usize> = HashMap::new();
        self.arg_names = self.arg_names.iter().map(
            |name| {
                let count = name_counts.entry(name).or_insert(0);
                *count += 1;
                name.clone() + &make_subscript(*count - 1)
            }).collect();
    }

    pub fn set_implicit_ret_arg_num(&mut self) {
        if 0 == self.branch_num {
            self.num_implicit_ret_args = 0;
            return;
        }

        if let Some(branch_id_pos) = self.branch_id_pos {
            self.num_implicit_ret_args = branch_id_pos;
            return;
        }

        // List of variable names which are assumed to be implicit arguments.
        let implicit_args = vec!["range_check"];

        if let Some(pos) = self.arg_names.iter().position(
            |name| { !implicit_args.contains(&name.as_str()) }
        ) {
            self.num_implicit_ret_args = pos;
        } else { // all arguments are implicit.
            self.num_implicit_ret_args = self.arg_names.len();
        }
    }
}

/// Identifies the statements at the end of the function which copy the return variables to the end
/// of the stack. Returns a vector of tuples where the first element of the tuple is the number of statements in
/// the branch return block, the second is the position of the branch ID return argument inside that block, and the third
/// indicates the size of the ap step (ap += <count>) the block begins with.
fn get_ret_blocks(
    aux_info: &CasmBuilderAuxiliaryInfo,
    cairo_program: &CairoProgram,
    casm_start: usize,
    casm_end: usize
) -> Vec<(usize, Option<usize>, usize)> {
    let mut count = 0;
    let mut branch_id_pos: Option<usize> = None;
    let mut branch_counts: Vec<(usize, Option<usize>, usize)> = Vec::new();

    let mut current_branch =
        if 0 < aux_info.return_branches.len() { Some(aux_info.return_branches.len() - 1) } else { None };

    let set_branch =
        |branch_counts: &mut Vec<(usize, Option<usize>, usize)>, count: &mut usize, branch_id_pos: &mut Option<usize>, ap_step: usize| -> bool {
            if *count != 0 {
                match *branch_id_pos {
                    Some(pos) => branch_counts.push((*count, Some(*count - pos - 1), ap_step)),
                    None => branch_counts.push((*count, None, ap_step)),
                };
                *count = 0;
                *branch_id_pos = None;
                true
            } else {
                false
            }
        };

    for pos in (casm_start + aux_info.core_libfunc_instr_num .. casm_end).rev() {

        // A branch return block begins with an ap += <step> or with the jmp or return at the end of the previous block.
        match &cairo_program.instructions[pos].body {
            InstructionBody::AddAp(ap_instr) => {
                let step_size = ap_step_from_instruction(ap_instr);
                match current_branch {
                    Some(branch_id) => {
                        if set_branch(&mut branch_counts, &mut count, &mut branch_id_pos, step_size) {
                            current_branch = if 0 < branch_id { Some(branch_id - 1) } else { None }
                        }
                    },
                    _ => { /* handle destructor */}
                }
            },
            InstructionBody::Ret(_) | InstructionBody::Jump(_) | InstructionBody::Call(_) => {
                match current_branch {
                    Some(branch_id) => {
                        if set_branch(&mut branch_counts, &mut count, &mut branch_id_pos, 0) {
                            current_branch = if 0 < branch_id { Some(branch_id - 1) } else { None }
                        }
                    },
                    _ => { /* handle destructor */ }
                }
            },
            InstructionBody::AssertEq(assert_eq) => {
                match &assert_eq.b {
                    ResOperand::Immediate(value) => {
                        match current_branch {
                            Some(branch_id ) => {
                                if *value == BigIntAsHex::from(branch_id) {
                                    branch_id_pos = Some(count);
                                }
                            },
                            _ => {}
                        }
                    },
                    _ => { }
                }
                count += 1;
             },
            _ => { count += 1; }
        }
    }

    match current_branch {
        Some(_) => { set_branch(&mut branch_counts, &mut count, &mut branch_id_pos, 0); },
        _ => { /* handle destructor */}
    }

    branch_counts.reverse();
    branch_counts
}

fn check_ret_blocks(ret_blocks: &Vec<(usize, Option<usize>, usize)>, branch_num: usize) {
    assert!(branch_num == ret_blocks.len(), "branch_num != ret_blocks.len()");

    if branch_num == 0 {
        return;
    }

    // check that all branches have the same number of statements
    let mut count = 0;
    for (num, _, _) in ret_blocks {
        if count == 0 {
            count = *num;
        } else {
            assert!(count == *num, "count != *num");
        }
    }
    // check that all branches have the same branch ID position.
    /*let branch_id_pos = ret_blocks[0].1;
    for (_, pos, _) in ret_blocks {
        match pos {
            Some(pos) => {
                match branch_id_pos {
                    Some(branch_id_pos) => assert!(branch_id_pos == *pos, "branch_id_pos != *pos"),
                    None => {
                        assert!(false, "only some branches with branch ID position");
                    }
                }
            },
            None => { assert!(branch_id_pos == None, "only some branches with branch ID position"); }
        }
    }*/
}

fn get_branch_id_pos(ret_blocks: &Vec<(usize, Option<usize>, usize)>) -> Option<usize> {

    if ret_blocks.len() == 0 {
        return None;
    }

    match ret_blocks[0].1 {
        Some(pos0) => {
            for (_, id_branch_pos, _) in ret_blocks {
                match id_branch_pos {
                    Some(pos) => {
                        if *pos != pos0 {
                            return None;
                        }
                    },
                    None => { return None; },
                }
            }
            Some(pos0)
        },
        _ => { None }
    }
}

/// Rebinding information for variables.
#[derive(Clone)]
struct VarRebind {
    /// The tuple stores the number of rebindings for each variable (0 for the first variable,
    /// 1 for the next one, etc.) and the cell expression for the current rebinding of
    /// the variable.
    vars: HashMap<String, (usize, CellExpression)>,
}

impl VarRebind {
    pub fn new() -> Self {
        Self { vars: HashMap::new() }
    }

    pub fn get_var_name(&self, name: &str) -> String {
        let count = if let Some(entry) = self.vars.get(name) { entry.0 } else { 0 };
        if count == 0 {
            name.to_string()
        } else {
            format!("{}{}", name, make_subscript(count))
        }
    }

    pub fn get_expr(&self, name: &str) -> Option<&CellExpression> {
        if let Some(entry) = self.vars.get(name) {
            Some(&entry.1)
        } else {
            None
        }
    }

    pub fn get_expr_str(&self, name: &str, is_vm: bool, use_ap: bool) -> String {
        if let Some(entry) = self.vars.get(name) {
            cell_expr_to_lean(&entry.1, false, is_vm, use_ap)
        } else {
            "/- unknown -/".into()
        }
    }

    pub fn get_next_name(&self, name: &str) -> String {
        let count = if let Some(entry) = self.vars.get(name) { entry.0 } else { 0 };
        format!("{}{}", name, make_subscript(count + 1))
    }

    pub fn rebind(&mut self, name: &str, cell_expr: &CellExpression) -> usize {
        self.vars.entry(name.to_string())
        .and_modify(|e| { e.0 = e.0 + 1 ; e.1 = cell_expr.clone(); })
        .or_insert((0, cell_expr.clone())).0
    }

    pub fn get_rebind_name(&mut self, name: &str, cell_expr: &CellExpression) -> String {
        self.rebind(name, cell_expr);
        self.get_var_name(name)
    }

    pub fn replace_var_names_in_expr(&self, expr: &str) -> String {
        let pat = &[' ', '+', '-', '*', '/', '(', ')'];
        expr.split_inclusive(pat)
            .map(|s| {
                if let Some(name) = s.split(pat).next() {
                    s.replace(name, &self.get_var_name(name))
                } else {
                    s.to_string()
                }
            })
            .join("")
    }

    /// Adds the given rebinds to the current rebinds as if the additional rebinds happened after
    /// the rebinds already stored in this object.
    pub fn add_rebinds(&mut self, rebind: &VarRebind) {
        for (name, (count, cell_expr)) in rebind.vars.iter() {
            self.vars.entry(name.clone())
                .and_modify(|e| { e.0 = e.0 + count ; e.1 = cell_expr.clone(); })
                .or_insert((*count, cell_expr.clone()));
        }
    }

    pub fn reset_counts(&mut self) {
        for (_, (count, _)) in &mut self.vars {
            *count = 0;
        }
    }
}

/// Expression formatting.

fn ref_to_lean(cell_ref: &CellRef, paren: bool, is_vm: bool, use_ap: bool) -> String {

    if is_vm {
        return if paren {
            format!("({})", ref_to_lean(cell_ref, false, is_vm, use_ap))
        } else {
            format!("exec {}", ref_to_lean(cell_ref, true, false, use_ap))
        };
    }

    // Since the initial FP and AP are the same, it is often easier to owrk with
    // the expressions relative to the initial AP.
    let reg = if use_ap { Register::AP } else { cell_ref.register };
    if cell_ref.offset == 0 {
        format!("σ.{reg}")
    } else if paren {
        format!("({})", ref_to_lean(cell_ref, false, false, use_ap))
    } else if cell_ref.offset < 0 {
        format!("σ.{reg} - {offset}", offset = -cell_ref.offset)
    } else {
        format!("σ.{reg} + {offset}", offset = cell_ref.offset)
    }
}

fn cell_expr_to_lean(expr: &CellExpression, paren: bool, is_vm: bool, use_ap: bool) -> String {

    if paren {
        return match expr {
            // Always adds parentheses, so avoid adding double parentheses.
            CellExpression::Immediate(_) => cell_expr_to_lean(expr, false, is_vm, use_ap),
            _ => format!("({})", cell_expr_to_lean(expr, false, is_vm, use_ap)),
        };
    }

    match expr {
        CellExpression::Deref(cell_ref) => {
            format!("mem {ref_str}", ref_str = ref_to_lean(cell_ref, true, is_vm, use_ap))
        },
        CellExpression::DoubleDeref(cell_ref, offset) => {
            if *offset == 0 {
                format!("mem (mem {ref_str})", ref_str = ref_to_lean(cell_ref, true, is_vm, use_ap))
            } else if *offset < 0 {
                format!("mem ((mem {ref_str}) - {offset})", ref_str = ref_to_lean(cell_ref, true, is_vm, use_ap), offset = -*offset)
            } else {
                format!("mem ((mem {ref_str}) + {offset})", ref_str = ref_to_lean(cell_ref, true, is_vm, use_ap), offset = *offset)
            }
        },
        CellExpression::Immediate(value) => {
            let val_type = if is_vm { "ℤ" } else { "F" };
            format!("({value} : {val_type})")
        },
        CellExpression::BinOp { op, a, b } => {
            let op_str = match op {
                CellOperator::Add => "+",
                CellOperator::Sub => "-",
                CellOperator::Mul => "*",
                CellOperator::Div => "/",
            };
            format!("({a_str} {op_str} {b_str})", a_str = ref_to_lean(a, true, is_vm, use_ap), b_str = deref_or_imm_to_lean(b, true, is_vm, use_ap))
        }
    }
}

fn deref_or_imm_to_lean(expr: &DerefOrImmediate, paren: bool, is_vm: bool, use_ap: bool) -> String {
    match expr {
        DerefOrImmediate::Deref(cell_ref) => ref_to_lean(cell_ref, paren, is_vm, use_ap),
        DerefOrImmediate::Immediate(value) => {
            let val_type = if is_vm { "ℤ" } else { "F" };
            format!("({} : {})", value.value, val_type)
        },
    }
}

/// If the expression is a deref, return the register name and offset.
fn get_ref_from_deref(expr: &CellExpression) -> Option<(String, i16)> {
    match expr {
        CellExpression::Deref(cell_ref) => {
            Some((format!("{}", cell_ref.register), cell_ref.offset))
        },
        _ => None
    }
}

/// Given a deref expression, this function returns the Lean string which represents
/// the reference inside the expression. Panics if the reference cannot be found.
fn get_lean_ref_from_deref(expr: &CellExpression, paren: bool, is_vm: bool, use_ap: bool) -> String {
    match expr {
        CellExpression::Deref(cell_ref) => {
            ref_to_lean(cell_ref, paren, is_vm, use_ap)
        },
        _ => panic!("Expected a deref expression.")
    }
}

/// Returns the amount by which the ap pointer is advanced by the given
/// instruction.
fn ap_step_from_instruction(instr: &AddApInstruction) -> usize {
    match &instr.operand {
        ResOperand::Immediate(value) =>
            value.value.to_usize().expect("ap step should be a usize value."),
        _ => panic!("Expected an immediate value in ap advance instruction")
    }
}

/// All the information needed to generate the Lean code for a single function.
struct LeanFuncInfo<'a> {
    main_func_name: String,
    func_name: String,
    aux_info: &'a CasmBuilderAuxiliaryInfo,
    casm_instructions: &'a Vec<Instruction>,
    casm_start: usize,
    casm_end: usize,
    ret_args: RetArgs,
    /// The size of the ap step (ap += <count>) at the beginning of each return block.
    ret_branch_ap_steps: Vec<usize>,
    blocks: Vec<FuncBlock>,
    max_rc_counts: HashMap<String, usize>,
    is_lean3: bool,
}

impl<'a> LeanFuncInfo<'a> {

    pub fn new(
        main_func_name: String,
        func_name: String,
        aux_info: &'a CasmBuilderAuxiliaryInfo,
        casm_start: usize,
        casm_end: usize,
        cairo_program: &'a CairoProgram,
        is_lean3: bool,
    ) -> Self {
        let (ret_args, ret_branch_ap_steps) = LeanFuncInfo::make_ret_args(&aux_info, cairo_program, casm_start, casm_end);
        let mut lean_info = Self {
            main_func_name,
            func_name,
            aux_info,
            casm_instructions: &cairo_program.instructions,
            casm_start,
            casm_end,
            ret_args,
            ret_branch_ap_steps,
            blocks: Vec::new(),
            max_rc_counts: HashMap::new(),
            is_lean3
        };

        lean_info.make_blocks();
        lean_info.max_rc_counts = lean_info.find_max_rc_checks(0, None);

        lean_info
    }

    pub fn make_ret_args(aux_info: &CasmBuilderAuxiliaryInfo, cairo_program: &CairoProgram, casm_start: usize, casm_end: usize) -> (RetArgs, Vec<usize>) {
        let ret_blocks = get_ret_blocks(aux_info, cairo_program, casm_start, casm_end);
        check_ret_blocks(&ret_blocks, aux_info.return_branches.len());

        let branch_id_pos = get_branch_id_pos(&ret_blocks);
        let mut ret_args = RetArgs {
            branch_num: ret_blocks.len(),
            arg_num: if ret_blocks.len() == 0 { 0 } else { ret_blocks[0].0 },
            arg_names: Vec::new(),
            branch_id_pos: branch_id_pos,
            num_implicit_ret_args: 0, // will be set later
        };
        ret_args.set_arg_names(&aux_info.return_branches);
        // Called functions need all their arguments to be explicit, so call this only
        // for the main function.
        if casm_start == 0 {
            ret_args.set_implicit_ret_arg_num();
        }
        (ret_args, ret_blocks.iter().map(|b| b.2).collect())
    }

    /// Goes over the labels in the function (in order) and creates their block information.
    /// Begins with the block information for the main block (the start of the function).
    fn make_blocks(&mut self) {
        self.blocks.push(
            FuncBlock {
                start_pos: 0,
                casm_start_pos: 0,
                pc_start_pos: 0,
                ap_offset: 0,
                label: None,
                args: self.get_arg_names(),
                ret_labels: self.make_ret_labels(0),
                start_local_ap: 0,
                start_rc: 0,
            }
        );

        for (i, statement) in self.aux_info.statements.iter().enumerate() {
            match statement {
                StatementDesc::Label(desc) => {
                    let casm_start_pos = self.casm_jump(0, i + 1);
                    self.blocks.push(
                        FuncBlock {
                            start_pos: i + 1,
                            casm_start_pos,
                            pc_start_pos: self.get_pc_at(casm_start_pos),
                            ap_offset: desc.ap_change,
                            label: Some(desc.label.clone()),
                            args: Vec::from_iter(self.get_sorted_block_args(i, &desc.label)),
                            ret_labels: self.make_ret_labels(i),
                            start_local_ap: self.find_max_next_ap(0, &desc.label).unwrap_or(0)
                                .to_usize().expect("ap offset expected to be non-negative"),
                            start_rc: *self.find_max_rc_checks(0, Some(&desc.label)).get(&desc.label).unwrap_or(&0),
                        }
                    )
                },
                _ => {}
            }
        }
    }

    /// Finds the first AP offset which is larger than the ap offset of all variables allocated on the stack
    /// beginning at the given start position and before the given target label. The local assignment
    /// of the block beginning at that label begins at the ap offset returned (when
    /// the start position is zero).
    fn find_max_next_ap(&self, start_pos: usize,  to_label: &str) -> Option<i16> {
        let mut max_ap: i16 = 0;

        for (i, statement) in self.aux_info.statements[start_pos..].iter().enumerate() {
            match statement {
                StatementDesc::Assert(assign) => {
                    // Check the ap offsets of all variables in the assert.
                    if let Some((_, var_offset)) = get_ref_from_deref(&assign.lhs.var_expr) {
                        max_ap = max(max_ap, var_offset + 1);
                    }
                    if let Some((_, var_offset)) = get_ref_from_deref(&assign.expr.var_a.var_expr) {
                        max_ap = max(max_ap, var_offset + 1);
                    }
                    if let Some(var_b) = &assign.expr.var_b {
                        if let Some((_, var_offset)) = get_ref_from_deref(&var_b.var_expr) {
                            max_ap = max(max_ap, var_offset + 1);
                        }
                    }
                },
                StatementDesc::TempVar(var)|StatementDesc::LocalVar(var) => {
                    let (_, var_offset) = get_ref_from_deref(&var.var_expr).expect("Failed to find var offset");
                    max_ap = max(max_ap, var_offset + 1);
                },
                StatementDesc::Jump(jump) => {
                    let branch1 = if let Some(target) = self.get_jump_target_pos(jump) {
                        self.find_max_next_ap(target, to_label)
                    } else {
                        // This is a return label, so it will never reach the target label from here.
                        None
                    };
                    let branch2 = if jump.cond_var.is_some() {
                        self.find_max_next_ap(start_pos + i + 1, to_label)
                    } else {
                        None
                    };

                    if branch1.is_none() && branch2.is_none() {
                        return None; // Label cannot be reached.
                    }

                    if let Some(branch1_ap) = branch1 {
                        max_ap = max(max_ap, branch1_ap)
                    }
                    if let Some(branch2_ap) = branch2 {
                        max_ap = max(max_ap, branch2_ap)
                    }

                    return Some(max_ap);
                },
                StatementDesc::Label(label) => {
                    if to_label == label.label {
                        return Some(max_ap);
                    }
                },
                StatementDesc::Fail => {
                    return None;
                },
                _ => {}
            }
        }

        // Did not reach the label
        None
    }

    /// Finds the maximal number of range checks performed between a given start
    /// point and a target label. If the target label is not given, performs this
    /// calculation for all return labels reachable from the start point.
    /// Returns a table with the labels as keys and the maximal rc count as value.
    fn find_max_rc_checks(&self, start_pos: usize, to_label: Option<&str>) -> HashMap<String, usize> {

        // Accumulated rc count
        let mut rc_count: usize = 0;

        for (i, statement) in self.aux_info.statements[start_pos..].iter().enumerate() {
            match statement {
                StatementDesc::Assert(assign) => {
                    if self.is_range_check(&assign.expr) {
                        rc_count += 1;
                    }
                },
                StatementDesc::Jump(jump) => {
                    let mut rc_counts = if let Some(target) = self.get_jump_target_pos(jump) {
                        self.find_max_rc_checks(target, to_label)
                    } else {
                        match to_label {
                            // This is a return label that should not be included.
                            Some(to_label) if to_label != jump.target => HashMap::new(),
                            // This is a return label which should be included.
                            _ => HashMap::from([(jump.target.clone(), 0)])
                        }
                    };
                    if jump.cond_var.is_some() {
                        // Take the maximum between the jmp branch and the continuation branch.
                        for (name, count) in self.find_max_rc_checks(start_pos + i + 1, to_label).iter() {
                            rc_counts.entry(name.clone()).and_modify(|c| if *c < *count { *c = *count; }).or_insert(*count);
                        }
                    };
                    // Add the accumulated count up to this point to all return branches.
                    for (_, count) in rc_counts.iter_mut() {
                        *count += rc_count;
                    }
                    return rc_counts;
                },
                StatementDesc::Label(label) => {
                    match to_label {
                        Some(to_label) if to_label == label.label => {
                            return HashMap::from([(to_label.into(), rc_count)]);
                        },
                        _ => {}
                    }
                },
                StatementDesc::Fail => {
                    // Empty counts. In a failed branch, there is no need for range checks.
                    return HashMap::new();
                },
                _ => {}
            }
        }

        // Fallthrough branch
        match to_label {
            Some(to_label) if to_label != "Fallthrough" => HashMap::new(),
            _ => HashMap::from([("Fallthrough".into(), rc_count)])
        }
    }

    /// Constructs the set of labels of all return blocks reachable from the given position.
    fn make_ret_labels(&self, start_pos: usize) -> HashSet<String> {

        for (i, statement) in self.aux_info.statements[start_pos..].iter().enumerate() {
            match statement {
                StatementDesc::Jump(jump) => {
                    let mut ret_labels = if let Some(target) = self.get_jump_target_pos(jump) {
                        self.make_ret_labels(target + 1)
                    } else {
                        // This is a return label.
                        HashSet::from([jump.target.clone()])
                    };
                    if jump.cond_var.is_some() {
                        for label in self.make_ret_labels(start_pos + i + 1).iter() {
                            ret_labels.insert(label.clone());
                        }
                    };
                    return ret_labels;
                },
                StatementDesc::Fail => {
                    // Empty counts. A failed branch does not reach a return block.
                    return HashSet::new();
                },
                _ => {}
            }
        }

        // Fallthrough branch
        HashSet::from(["Fallthrough".into()])
    }

    pub fn get_block_by_label(&self, label: &str) -> Option<&FuncBlock> {
        self.blocks.iter().find(|block|
            match &block.label {
                Some(name) => name == label,
                _ => false
            })
    }

    pub fn is_range_check_var(&self, name: &str) -> bool {
        name == "range_check"
    }

    pub fn is_const(&self, name: &str) -> bool {
        self.aux_info.consts.iter().any(|c| c.name == name)
    }

    pub fn has_args(&self) -> bool {
        0 < self.aux_info.args.len()
    }

    /// Returns the list of all argument names.
    pub fn get_arg_names(&self) -> Vec<String> {
        self.aux_info.args.iter().map(
            |v| -> String {
                if let Some(arg) = self.aux_info.var_names.get(&v.0) {
                    String::from(arg)
                } else {
                    String::from("")
                }
            }
        ).collect()
    }

    /// Returns all arguments with the Lean expression which defines their value.
    fn get_arg_names_and_expr(&self, is_vm: bool, use_ap: bool) -> Vec<(String, String)> {
        self.get_arg_names_and_cell_expr().iter().map(
            |(name, expr)| (name.clone(), cell_expr_to_lean(expr, false, is_vm, use_ap))
        ).collect_vec()
    }

    /// Returns all arguments with the Lean expression which defines their value.
    fn get_arg_names_and_cell_expr(&self) -> Vec<(String, &CellExpression)> {
        self.aux_info.args.iter().map(
            |v| -> (String, &CellExpression) {
                if let Some(arg) = self.aux_info.var_names.get(&v.0) {
                    (String::from(arg), &v.1)
                } else {
                    ("".into(), &v.1)
                }
            }
        ).collect()
    }

    pub fn has_range_check_arg(&self) -> bool {
        self.get_arg_names().iter().any(|a| self.is_range_check_var(a))
    }

    pub fn get_range_check_arg_name(&self) -> Option<String> {
        for (var, _) in self.aux_info.args.iter() {
            if let Some(arg) = self.aux_info.var_names.get(var) {
                if self.is_range_check_var(arg) {
                    return Some(arg.into());
                }
            }
        }
        None
    }

    /// Given a variable name, returns the VM type of that variable. This is 'val'
    /// for all variables except the rc pointer, which is 'rc'.
    pub fn get_vm_var_type(&self, name: &str) -> String {
        let base_name: String = name.strip_prefix("ρ_").unwrap_or(name).into();
        if let Some(rc_ptr) = self.get_range_check_arg_name() {
            if rc_ptr == base_name {
                "rc".into()
            } else {
                "val".into()
            }
        } else {
            "val".into()
        }
    }

    pub fn get_arg_expr(&self, name: &str, paren: bool, is_vm: bool, use_ap: bool) -> Option<String> {
        for (var, expr) in self.aux_info.args.iter() {
            if let Some(arg) = self.aux_info.var_names.get(var) {
                if arg == name {
                    return Some(cell_expr_to_lean(expr, paren, is_vm, use_ap));
                }
            }
        }
        None
    }

    pub fn ret_arg_num(&self) -> usize {
        self.ret_args.arg_names.len()
    }

    pub fn implicit_ret_arg_num(&self) -> usize {
        self.ret_args.num_implicit_ret_args
    }

    pub fn explicit_ret_arg_num(&self) -> usize {
        self.ret_args.arg_names.len() - self.ret_args.num_implicit_ret_args
    }

    /// Returns the actual number of explicit arguments returned by a specific return
    /// branch. This may be smaller than the number of explicit return arguments defined
    /// for the function, as the number of return argument of the function is the maximum
    /// for all return branches, but is not necessarily used by all branches.
    pub fn get_branch_explicit_arg_num(&self, branch_id: usize) -> usize {
        // For the explicit return arguments we use the last return arg names and the first
        // return expressions. Here, we determine the number of explicit arguments (at the end of
        // the argument list).
        min(
            self.aux_info.return_branches[branch_id].flat_exprs().len(),
            // One argument may be the branch ID argument.
            self.ret_arg_num() - if self.ret_args.branch_id_pos.is_some() { 1 } else { 0 }
        ) - self.ret_args.num_implicit_ret_args
    }

    /// Returns the list of all return argument names, including the implicit ones.
    pub fn get_all_ret_arg_names(&self) -> Vec<String> {
        self.ret_args.arg_names.iter().map(
            |s| { if s.starts_with("ρ") { s.to_string() } else { "ρ_".to_string() + s } }
        ).collect()
    }

    /// The explicit return arguments are determined in two ways:
    /// 1. If there is a branch ID return argument, it is the first explicit return argument.
    /// 2. If there is no branch ID return argument, we skip specific argument names as implicit
    ///    (such as 'range_check').
    pub fn get_explicit_ret_arg_names(&self) -> Vec<String> {
        self.ret_args.arg_names[self.ret_args.num_implicit_ret_args..].iter().map(
            |s| { if s.starts_with("ρ") { s.to_string() } else { "ρ_".to_string() + s } }
        ).collect()
    }

    /// return the offset from the final ap of the given return argument. The last return argument is
    /// at offset 1, the second to last at offset 2, etc.
    pub fn get_ret_arg_offset(&self, name: &str) -> Option<usize> {
        match self.ret_args.arg_names.iter().position(|s| s == name) {
            Some(pos) => Some(self.ret_args.arg_names.len() - pos),
            _ => None
        }
    }

    /// Calculate the position in the casm at which the code for the asserts of the given
    /// return branch begins. This does not include an initial ap step, if such a step exists
    /// (the position of the ap step is then one before the returned result).
    pub fn get_ret_block_assert_start_casm_pos(&self, lean_info: &LeanFuncInfo, branch_id: usize) -> usize {

        // The position in the casm at which the asserts for the return values start
        // for this block. Assumes that each block has one assert per return argument
        // and a jump or return at the end of the block. In addition, each block may begin
        // with an ap step.

        let num_later_ap_step = lean_info.ret_branch_ap_steps[branch_id + 1 ..].iter().filter(|x| 0 < **x).count();
        lean_info.casm_end
            - (lean_info.ret_args.branch_num - branch_id) * (lean_info.ret_arg_num() + 1)
            - num_later_ap_step
    }

    // True if this expression represents a range check: x = [<range check pointer>]
    pub fn is_range_check(&self, expr: &ExprDesc) -> bool {
        expr.op == "*()" &&
        if let Some(str) = self.aux_info.var_names.get(&expr.var_a.var_id) {
            self.is_range_check_var(str)
        } else { false }
    }

    // If this expression represents a range check, x = [<range check pointer>], returns
    // the name of the range check pointer.
    pub fn get_range_check_ptr_name(&self, expr: &ExprDesc) -> Option<&str> {
        if expr.op != "*()" {
            return None;
        }
        if let Some(str) = self.aux_info.var_names.get(&expr.var_a.var_id) {
            if self.is_range_check_var(str) { Some(str) } else { None }
        } else { None }
    }

    /// Returns the offset in *() operations.
    pub fn get_offset_at(&self, expr: &ExprDesc) -> Option<BigInt> {
        if expr.op != "*()" {
            return None;
        }

        if let CellExpression::BinOp { op: _, a: _, b } = &expr.var_a.var_expr {
            if let DerefOrImmediate::Immediate(value) = b {
                return Some(value.value.clone());
            }
        }

        Some(BigInt::from(0))
    }

    // True if this expression increments the range check pointer.
    pub fn is_range_check_inc(&self, expr: &ExprDesc) -> bool {
        expr.op == "++" &&
        if let Some(str) = self.aux_info.var_names.get(&expr.var_a.var_id) {
            self.is_range_check_var(str)
        } else { false }
    }

    /// If a block has a single maximal number of range checks (same for all
    /// return blocks reachable from the block) then this number is returned.
    /// Otherwise, None is returned.
    fn get_block_max_rc(&self, block: &FuncBlock) -> Option<usize> {

        if self.max_rc_counts.len() == 0 {
            return Some(0);
        }

        let mut rc_count: Option<usize> = None;

        for label in block.ret_labels.iter() {
            if let Some(max_count) = self.max_rc_counts.get(label) {
                if let Some(count) = rc_count {
                    if count != *max_count{
                        return None
                    }
                } else {
                    rc_count = Some(*max_count);
                }
            }
        }

        rc_count
    }


    /// Returns the smallest of the maximal number of range checks for the
    /// return branches reachable from this block.
    fn get_block_min_max_rc(&self, block: &FuncBlock) -> usize {

        if self.max_rc_counts.len() == 0 {
            return 0;
        }

        let mut rc_count: Option<usize> = None;

        for label in block.ret_labels.iter() {
            if let Some(max_count) = self.max_rc_counts.get(label) {
                if let Some(min_count) = &mut rc_count {
                    if *max_count < *min_count {
                        *min_count = *max_count;
                    }
                } else {
                    rc_count = Some(*max_count);
                }
            }
        }

        if let Some(min_count) = rc_count { min_count } else { 0 }
    }

    pub fn get_code_len(&self) -> usize {
        self.casm_instructions.iter().fold(0, |len, instr| len + instr.body.op_size())
    }

    /// Returns the pc at the given casm position, under the assumption that pc = 0 at the first casm position.
    pub fn get_pc_at(&self, casm_pos: usize) -> usize {
        self.casm_instructions[0..casm_pos].iter().fold(0, |len, instr| len + instr.body.op_size())
    }

    /*
    pub fn get_ap_at(&self, casm_pos: usize) -> BigInt {
        self.casm_instructions[0..casm_pos].iter().fold(0.into(),
            |offset: BigInt, instr: &Instruction| {
                match &instr.body {
                    InstructionBody::AddAp(body) => {
                        if let ResOperand::Immediate(step) = &body.operand {
                            offset + &step.value
                        } else {
                            // Other options not supported, need to fix manually.
                            offset
                        }
                    },
                    _ => offset + if instr.inc_ap { BigInt::from(1) } else { 0.into() }
                }
            }
        )
    }*/

    /// Returns the number of positions by which to increase casm instruction position to reach
    /// the target of the jump, where jump_pos and target are the positions of the instruction
    /// descriptions of the jump and the target, respectively.
    fn casm_jump(&self, jump_pos: usize, target: usize) -> usize {

        self.aux_info.statements[jump_pos..target].iter().fold(
            0,
            |acc, s| {
                match s {
                    // These instructions have no corresponding instruction in the casm.
                    StatementDesc::Let(_) | StatementDesc::Label(_) => acc,
                    StatementDesc::TempVar(var) | StatementDesc::LocalVar(var) =>
                        if var.expr.is_some() { acc + 1 } else { acc },
                    _ => acc + 1
                }
            }
        )
    }

    fn pc_jump(&self, casm_jump_pos: usize, casm_target: usize) -> usize {
        self.casm_instructions[casm_jump_pos..casm_target].iter().fold(
            0,
            |pc, instr| {
                pc + instr.body.op_size()
            }
        )
    }

    /// Returns the position in the list of statement descriptions of the label
    /// which is the target of the jump statement. If this fails, the label is
    /// probably a return block label.
    fn get_jump_target_pos(&self, jump: &JumpDesc) -> Option<usize> {
        self.aux_info.statements.iter().position(
            |s|
                if let StatementDesc::Label(label) = s {
                    label.label == jump.target
                } else {
                    false
                }
        )
    }

    /// Given the start position of a block, returns all variable names used in the block before
    /// being defined. These are the variables which are the input argument to the block.
    fn get_block_args(&self, block_start: usize) -> HashSet<String> {

        let mut vars = HashSet::new();
        let mut declared: HashSet<String> = HashSet::new();

        for statement in self.aux_info.statements[block_start..].iter() {
            match statement {
                StatementDesc::TempVar(var)|StatementDesc::LocalVar(var) => {
                    if let Some(expr) = &var.expr {
                        let name = &expr.var_a.name;
                        if !declared.contains(name) && !self.is_const(name) {
                            vars.insert(name.clone());
                        }
                        if let Some(expr_b) = &expr.var_b {
                            let name = &expr_b.name;
                            if !declared.contains(name) && !self.is_const(name) {
                                vars.insert(name.clone());
                            }
                        }
                    }
                    declared.insert(var.name.clone());
                },
                StatementDesc::Let(assign) => {
                    let name = &assign.expr.var_a.name;
                    if !declared.contains(name) && !self.is_const(name) {
                        vars.insert(name.clone());
                    }
                    if let Some(expr_b) = &assign.expr.var_b {
                        let name = &expr_b.name;
                        if !declared.contains(name) && !self.is_const(name) {
                            vars.insert(name.clone());
                        }
                    }
                    declared.insert(assign.lhs.name.clone());
                },
                StatementDesc::Assert(assign) => {
                    let mut names: Vec<&String> = vec![&assign.lhs.name, &assign.expr.var_a.name];
                    if let Some(expr_b) = &assign.expr.var_b {
                        names.push(&expr_b.name);
                    }
                    for name in names {
                        if !declared.contains(name) && !self.is_const(name) {
                            vars.insert(name.clone());
                        }
                    }
                },
                StatementDesc::Jump(jump) => {
                    if let Some(target) = self.get_jump_target_pos(jump) {
                        let branch_args = self.get_block_args(target + 1);
                        // Add the variables used in the branch before being declared.
                        for name in branch_args {
                            if !declared.contains(&name) {
                                vars.insert(name);
                            }
                        }
                    } else { // jump to a return label
                        self.add_ret_block_args(&jump.target, &declared, &mut vars);
                    }
                    if jump.cond_var.is_none() {
                        return vars;
                    }
                },
                StatementDesc::Fail => {
                    return vars;
                },
                StatementDesc::ApPlus(_) => {},
                StatementDesc::Label(_) => {},
            }
        }

        // Handle the Fallthrough return block.
        self.add_ret_block_args("Fallthrough", &declared, &mut vars);

        vars
    }

    fn get_sorted_block_args(&self, block_start: usize, label: &String) -> Vec<String> {
        let vars = self.get_block_args(block_start);
        let bindings = self.get_rebinds_before_label(label, 0);
        let mut vars_with_ref: Vec<(String, Option<(String, i16)>)> = Vec::from_iter(
            vars.iter().map(
                |name| {
                    if let Some(bindings) = &bindings {
                        if let Some(expr) = bindings.get_expr(name) {
                            (name.clone(), get_ref_from_deref(&expr))
                        } else {
                            (name.clone(), None)
                        }
                    } else {
                        (name.clone(), None)
                    }
                }
            )
        );

        // Sort by the reference ('fp' before 'ap' with increasing offset). If could not find the reference,
        // sort alphabetically after the variables with a reference.
        vars_with_ref.sort_by(|a, b| {
            if let Some(a_ref) = &a.1 {
                if let Some(b_ref) = &b.1 {
                    if a_ref.0 == b_ref.0 {
                        a_ref.1.cmp(&b_ref.1)
                    } else {
                        b_ref.0.cmp(&a_ref.0)
                    }
                } else {
                    Ordering::Less
                }
            } else if b.1.is_none() {
                // Reverse the comparison, as we want 'fp' before 'ap'
                a.0.cmp(&b.0)
            } else {
                Ordering::Greater
            }
        });

        vars_with_ref.iter().map(|v| v.0.clone()).collect()
    }

    fn add_ret_block_args(&self, label: &str, declared: &HashSet<String>, block_args: &mut HashSet<String>) {

        if let Some(branch) =
                self.aux_info.return_branches.iter().filter(|b| b.name == *label).next() {
            for name in branch.flat_exprs() {
                if !declared.contains(&name) {
                    block_args.insert(name);
                }
            }
        }
    }

    /// Given a label, returns a rebind structure with the rebinds beginning at the given start position and
    /// accumulated up to first reaching the label. If the label is not reached, None is returned.
    fn get_rebinds_before_label(&self, label: &String, start_pos: usize) -> Option<VarRebind> {
        let mut rebind = VarRebind::new();
        if start_pos == 0 {
            // The input arguments to the function.
            for (name, cell_expr) in self.get_arg_names_and_cell_expr().iter() {
                rebind.rebind(name, cell_expr);
            }
        }

        // Continue down all branches until we hit a jump to the branch.
        for statement in self.aux_info.statements[start_pos..].iter() {
            match statement {
                StatementDesc::TempVar(var)|StatementDesc::LocalVar(var) => {
                    rebind_var_assignment(var, &mut rebind);
                },
                StatementDesc::Let(assign) => {
                    rebind_assignment_lhs(assign, &mut rebind);
                },
                StatementDesc::Jump(jump) => {
                    if &jump.target == label {
                        return Some(rebind); // Reached the label.
                    }
                    if let Some(target) = self.get_jump_target_pos(jump) {
                        let branch_rebinds = self.get_rebinds_before_label(label, target + 1);
                        if let Some(branch_rebinds) = branch_rebinds {
                            // Branch reached the label add the rebinds to the ones already collected.
                            rebind.add_rebinds(&branch_rebinds);
                            return Some(rebind);
                        }
                    }
                    if jump.cond_var.is_none() {
                        return None; // No continuation and did not reach the label.
                    }
                },
                StatementDesc::Fail => {
                    return None; // Reached end before reaching label.
                },
                StatementDesc::Label(desc) => {
                    if label == &desc.label {
                        return Some(rebind);
                    }
                },
                _ => {}
            }
        }

        None // Did not reach the label.
    }

    /// Returns the names and cell expressions for the arguments of a block.
    fn get_block_args_and_exprs(&self, block: &FuncBlock) -> Vec<(String, CellExpression)>{
        if let Some(label) = &block.label {
            let arg_rebinds = self.get_rebinds_before_label(&label, 0).expect("Failed to reach label.");
            block.args.iter().map(
                |name| (name.clone(), arg_rebinds.get_expr(name).expect("Failed to find variable expression").clone())
            ).collect_vec()
        } else {
            self.get_arg_names_and_cell_expr().iter().map(|(name, expr)| (name.clone(), (*expr).clone())).collect_vec()
        }
    }

    /// Returns the names and Lean expressions for the arguments of a block.
    fn get_block_args_and_lean_exprs(&self, block: &FuncBlock, is_vm: bool, use_ap: bool) -> Vec<(String, String)>{
        self.get_block_args_and_exprs(block).iter().map(
            |(name, expr)| (name.clone(), cell_expr_to_lean(expr, false, is_vm, use_ap).clone())
        ).collect_vec()
    }
}

// Rebinding variables and assignments

fn rebind_var_assignment(
    var: &VarDesc,
    rebind: &mut VarRebind,
) -> (String, Option<String>) {
    // Must rewrite variable names with the rebind subscripts before rebinding the lhs.
    let expr = if let Some(expr) = &var.expr {
        Some(rebind.replace_var_names_in_expr(&expr.expr))
    } else { None };

    (rebind.get_rebind_name(&var.name, &var.var_expr), expr)
}

fn rebind_assignment(
    assignment: &AssertDesc,
    rebind: &mut VarRebind,
) -> (String, String) {
    // Must rewrite the rhs before the lhs.
    let rhs_str = rebind.replace_var_names_in_expr(&assignment.expr.expr);
    (rebind.get_rebind_name(&assignment.lhs.name, &assignment.lhs.var_expr), rhs_str)
}

fn rebind_assignment_lhs(
    assignment: &AssertDesc,
    rebind: &mut VarRebind,
) -> String {
    rebind.get_rebind_name(&assignment.lhs.name, &assignment.lhs.var_expr)
}

// Soundness generation functions

/// Generates the prelude to the spec file. Lean 3 only.
fn generate_soundness_spec_prelude(main_func_name: &str) -> Vec<String> {
    let mut prelude: Vec<String> = Vec::new();
    prelude.push(String::from("import starkware.cairo.lean.semantics.soundness.prelude"));
    prelude.push(String::from("open tactic"));
    prelude.push(String::from(""));
    prelude.push(String::from("variables {F : Type} [field F] [decidable_eq F] [prelude_hyps F]"));
    prelude.push(String::from("variable  mem : F → F"));
    prelude.push(String::from("variable  σ : register_state F"));
    prelude.push(String::from(""));

    prelude
}

/// Generates the prelude to the spec file. Lean 4 only.
fn generate_soundness_spec_prelude4(main_func_name: &str) -> Vec<String> {
    let mut prelude: Vec<String> = Vec::new();
    prelude.push(String::from("import Verification.Semantics.Soundness.Prelude"));
    prelude.push(String::from("import Verification.Libfuncs.Common"));
    prelude.push(String::from(""));
    prelude.push(String::from("open Tactic"));
    prelude.push(String::from("set_option autoImplicit false"));
    prelude.push("set_option maxRecDepth 1024".into());
    prelude.push(String::from(""));
    prelude.push(String::from("variable {F : Type} [Field F] [DecidableEq F] [PreludeHyps F]"));
    prelude.push(String::from(""));

    prelude
}

fn generate_soundness_prelude(main_func_name: &str) -> Vec<String> {
    let mut prelude: Vec<String> = Vec::new();
    prelude.push(String::from("import starkware.cairo.lean.semantics.soundness.prelude"));
    prelude.push(format!("import .{}", lean_code_file_name(main_func_name, false)));
    prelude.push(format!("import .{}", lean_soundness_spec_file_name(main_func_name, false)));
    prelude.push(String::from("open tactic"));
    prelude.push(String::from(""));
    prelude.push(String::from("variables {F : Type} [field F] [decidable_eq F] [prelude_hyps F]"));
    prelude.push(String::from("variable  mem : F → F"));
    prelude.push(String::from("variable  σ : register_state F"));
    prelude.push(String::from(""));

    prelude
}

fn generate_soundness_prelude4(main_func_name: &str) -> Vec<String> {
    let mut prelude: Vec<String> = Vec::new();
    prelude.push(String::from("import Verification.Semantics.Soundness.Prelude"));
    prelude.push(String::from("import Verification.Libfuncs.Common"));
    prelude.push(format!("import Verification.Libfuncs.XXX.{}", lean_soundness_spec_file_name(main_func_name, false)));
    prelude.push(format!("import Verification.Libfuncs.XXX.{}", lean_code_file_name(main_func_name, false)));
    prelude.push(String::from(""));
    prelude.push(String::from("open Tactic"));
    prelude.push(String::from("set_option autoImplicit false"));
    prelude.push("set_option maxRecDepth 1024".into());
    prelude.push(String::from(""));
    prelude.push(format!("open {}", lean_code_name(main_func_name)));
    prelude.push(String::from(""));
    prelude.push(String::from("variable {F : Type} [Field F] [DecidableEq F] [PreludeHyps F] (mem : F → F) (σ : RegisterState F)"));
    prelude.push(String::from(""));

    prelude
}

/// Generates the prelude to the spec file. Lean 4 only.
fn generate_completeness_spec_prelude(main_func_name: &str) -> Vec<String> {
    let mut prelude: Vec<String> = Vec::new();
    prelude.push(String::from("import Verification.Semantics.Completeness.VmHoare"));
    prelude.push(String::from("import Verification.Semantics.Soundness.Prelude"));
    prelude.push(String::from("import Verification.Libfuncs.Common"));
    prelude.push(String::from(""));
    prelude.push(String::from("open Tactic"));

    prelude.push(String::from("set_option autoImplicit false"));
    prelude.push(String::from("set_option maxRecDepth 1024"));
    prelude.push(String::from(""));

    prelude
}

fn generate_completeness_prelude(main_func_name: &str) -> Vec<String> {
    let mut prelude: Vec<String> = Vec::new();

    prelude.push(String::from("import Verification.Semantics.Soundness.Prelude"));
    prelude.push(String::from("import Verification.Libfuncs.Common"));
    prelude.push(format!("import Verification.Libfuncs.XXX.{}", lean_completeness_spec_file_name(main_func_name, false)));
    prelude.push(format!("import Verification.Libfuncs.XXX.{}", lean_code_file_name(main_func_name, false)));
    prelude.push(String::from(""));
    prelude.push(String::from("open Tactic"));
    prelude.push(String::from("open Mrel"));
    prelude.push(String::from("set_option autoImplicit false"));
    prelude.push(String::from("set_option maxRecDepth 1024"));
    prelude.push(String::from("set_option maxHeartbeats 1000000"));
    prelude.push(String::from(""));
    prelude.push(format!("open {}", lean_vm_code_name(main_func_name)));
    prelude.push(String::from(""));
    prelude.push(String::from("variable (mem : Mrel → Mrel) (σ : VmRegisterState)"));
    prelude.push(String::from(""));

    prelude
}

fn generate_consts(lean_info: &LeanFuncInfo, is_completeness: bool) -> Vec<String> {
    let mut consts: Vec<String> = Vec::new();
    let const_type = if is_completeness { "ℤ" } else { "F" };

    for const_def in &lean_info.aux_info.consts {

        // let expr = const_def.expr.replace("::", "_");
        if let CellExpression::Immediate(value) = &const_def.value {
            consts.push(format!(
                "def {const_name} : {const_type} := ({value_str} : {const_type}) -- {expr}",
                const_name = const_def.name,
                value_str = value.to_string(),
                expr = const_def.expr,
            ));
        } else {
            consts.push(format!(
                "def {const_name} : {const_type} := {expr}",
                const_name = const_def.name,
                expr = const_def.expr,
            ));
        }
    }

    consts
}

trait LeanGenerator {

    /// Return a generator for a branch.
    fn branch(&self) -> Box<dyn LeanGenerator>;
    /// Integrates the Lean code generated by a branch generator into the
    /// main Lean code generator.
    fn append_branch(&mut self, branch: &Box<dyn LeanGenerator>);

    /// Returns the Lean code generated by this generator.
    fn get_lean(&self) -> &Vec<String>;

    fn generate_statement(
        &mut self,
        main_func_name: &str,
        func_name: &str,
        lean_info: &LeanFuncInfo,
        block: &FuncBlock,
        indent: usize,
    );

    fn generate_intro(
        &mut self,
        main_func_name: &str,
        func_name: &str,
        lean_info: &LeanFuncInfo,
        block: &FuncBlock,
        indent: usize,
    );

    fn generate_temp_var(
        &mut self,
        var: &VarDesc,
        lean_info: &LeanFuncInfo,
        block: &FuncBlock,
        rebind: &mut VarRebind,
        pc: usize,
        op_size: usize,
        indent: usize,
    );

    fn generate_implicit_temp_var(
        &mut self,
        var_desc: &VarBaseDesc,
        lean_info: &LeanFuncInfo,
        block: &FuncBlock,
        rebind: &mut VarRebind,
        pc: usize,
        op_size: usize,
        indent: usize,
    );

    fn generate_assert_missing_vars(
        &mut self,
        assert: &AssertDesc,
        lean_info: &LeanFuncInfo,
        block: &FuncBlock,
        rebind: &mut VarRebind,
        pc: usize,
        op_size: usize,
        indent: usize,
    ) {
        if !rebind.vars.contains_key(&assert.lhs.name) && !lean_info.is_const(&assert.lhs.name) {
            self.generate_implicit_temp_var(&assert.lhs, lean_info, block, rebind, pc, op_size, indent);
        }

        if !rebind.vars.contains_key(&assert.expr.var_a.name) && !lean_info.is_const(&assert.expr.var_a.name) {
            self.generate_implicit_temp_var(&assert.expr.var_a, lean_info, block, rebind, pc, op_size, indent);
        }

        if let Some(var_desc) = &assert.expr.var_b {
            if !rebind.vars.contains_key(&var_desc.name)  && !lean_info.is_const(&var_desc.name) {
                self.generate_implicit_temp_var(&var_desc, lean_info, block, rebind, pc, op_size, indent);
            }
        }
    }

    fn generate_local_var(
        &mut self,
        var: &VarDesc,
        lean_info: &LeanFuncInfo,
        block: &FuncBlock,
        rebind: &mut VarRebind,
        pc: usize,
        op_size: usize,
        indent: usize,
    );

    fn generate_let(
        &mut self,
        assign: &AssertDesc,
        lean_info: &LeanFuncInfo,
        rebind: &mut VarRebind,
        indent: usize,
    );

    fn generate_assert(
        &mut self,
        assign: &AssertDesc,
        lean_info: &LeanFuncInfo,
        block: &FuncBlock,
        rebind: &mut VarRebind,
        pc: usize,
        op_size: usize,
        indent: usize,
    );

    fn generate_jmp(
        &mut self,
        pc: usize,
        op_size: usize,
        indent: usize,
    );

    fn generate_jnz(
        &mut self,
        cond_var: &str,
        label: &str,
        rebind: &VarRebind,
        pc: usize,
        op_size: usize,
        indent: usize,
    );

    fn generate_label_block(
        &mut self,
        func_name: &str,
        lean_info: &LeanFuncInfo,
        block: &FuncBlock,
        calling_block: &FuncBlock,
        rebind: &VarRebind,
        indent: usize,
    );

    /// Returns the indentation after the branch intro.
    fn generate_branch_intro(
        &mut self,
        cond_var: &(String, Var),
        is_eq: bool,
        lean_info: &LeanFuncInfo,
        rebind: &mut VarRebind,
        pc: usize,
        indent: usize,
    ) -> usize;

    /// Returns the indentation after the branch close.
    fn generate_branch_close(
        &mut self,
        is_eq: bool,
        lean_info: &LeanFuncInfo,
        rebind: &mut VarRebind,
        indent: usize,
    ) -> usize;

    fn generate_return_args(
        &mut self,
        ret_block_name: &str,
        branch_id: usize,
        ret_arg_names: &Vec<String>,
        ret_exprs: &Vec<String>,
        lean_info: &LeanFuncInfo,
        // The function block in which the return block was called.
        block: &FuncBlock,
        rebind: &VarRebind,
        indent: usize,
    );

    fn generate_advance_ap(&mut self, step: &usize, pc: usize, op_size: usize, indent: usize);

    fn generate_fail(&mut self, pc: usize, op_size: usize, indent: usize);

}

struct AutoSpecs {
    specs: Vec<String>,
    /// The spec for soundness and completeness is very similar, except for a small number
    /// of differences, so we use the same code to generate both specs. This flag indicates
    /// (where it matters) whether the soundness or the completeness specs need to be generated.
    is_completeness: bool,
}

impl AutoSpecs {
    pub fn new(is_completeness: bool) -> AutoSpecs {
        AutoSpecs {
            specs: Vec::new(),
            is_completeness: is_completeness,
        }
    }

    fn push(&mut self, indent: usize, str: &str) {
        self.specs.push(" ".repeat(indent) + str);
    }

    fn continue_specs(&mut self) {
        if let Some(last) = self.specs.last_mut() {
            if !last.ends_with("=") && !last.ends_with(',') && !last.ends_with('∨') && !last.ends_with('(') {
                last.push_str(" ∧")
            }
        }
    }

    fn push_spec(&mut self, indent: usize, str: &str) {
        if !str.starts_with(')') {
            self.continue_specs();
        }

        self.specs.push(" ".repeat(indent) + str);
    }

    fn append_spec(&mut self, more_specs: &Vec<String>) {
        self.continue_specs();
        self.specs.append(&mut more_specs.clone());
    }

    fn var_type(&self) -> &str {
        if self.is_completeness { "ℤ" } else { "F" }
    }

    fn is_range_checked(&self) -> &str {
        if self.is_completeness { "VmIsRangeChecked u128Limit" } else { "IsRangeChecked (rcBound F)" }
    }

    fn generate_var(
        &mut self,
        var: &VarDesc,
        lean_info: &LeanFuncInfo,
        rebind: &mut VarRebind,
        indent: usize,
    ) {
        let (var_name, expr) = rebind_var_assignment(var, rebind);
        self.push_spec(
            indent,
            &format!("∃ {var_name} : {var_type},{expr}",
                var_type = self.var_type(),
                expr = if let Some(expr_str) = &expr {
                    format!(" {var_name} = {expr_str}")
                } else { String::new() }));
    }
}

impl LeanGenerator for AutoSpecs {

    /// Return a generator for a branch.
    fn branch(&self) -> Box<dyn LeanGenerator> {
        let mut auto_spec = AutoSpecs::new(self.is_completeness);
        Box::new(auto_spec)
    }

    fn append_branch(&mut self, branch: &Box<dyn LeanGenerator>) {
        self.append_spec(branch.get_lean());
    }

    fn get_lean(&self) -> &Vec<String> {
        &self.specs
    }

    fn generate_statement(
        &mut self,
        main_func_name: &str,
        func_name: &str,
        lean_info: &LeanFuncInfo,
        block: &FuncBlock,
        indent: usize,
    ) {
        let args_str =
        block.args.iter().chain(
                lean_info.get_explicit_ret_arg_names().iter()
            ).join(" ");

        // Only the soundness specs may need to provide a variable for bounding the number
        // of steps (not often used in the libfuncs).
        let trace_count_str = if self.is_completeness { "" } else { " (κ : ℕ)" };

        self.push(
            indent,
            &format!(
                "def auto_spec_{func_name}{block_suffix}{trace_count_str} ({args_str} : {var_type}) : Prop :=",
                block_suffix = block.block_suffix(),
                var_type = self.var_type(),
            )
        );
    }

    fn generate_intro(
        &mut self,
        main_func_name: &str,
        func_name: &str,
        lean_info: &LeanFuncInfo,
        block: &FuncBlock,
        indent: usize,
    ) {
        // Does nothing here.
    }

    fn generate_temp_var(
        &mut self,
        var: &VarDesc,
        lean_info: &LeanFuncInfo,
        block: &FuncBlock,
        rebind: &mut VarRebind,
        pc: usize,
        op_size: usize,
        indent: usize,
    ) {
        self.generate_var(var, lean_info, rebind, indent);
    }

    fn generate_implicit_temp_var(
        &mut self,
        var_desc: &VarBaseDesc,
        lean_info: &LeanFuncInfo,
        block: &FuncBlock,
        rebind: &mut VarRebind,
        pc: usize,
        op_size: usize,
        indent: usize,
    ) {
        self.generate_var(
            &VarDesc {
                name: var_desc.name.clone(),
                var_id: var_desc.var_id,
                var_expr: var_desc.var_expr.clone(),
                expr: None,
                ap_change: 0, // not used here.
            },
            lean_info,
            rebind,
            indent
        );
    }

    fn generate_local_var(
        &mut self,
        var: &VarDesc,
        lean_info: &LeanFuncInfo,
        block: &FuncBlock,
        rebind: &mut VarRebind,
        pc: usize,
        op_size: usize,
        indent: usize,
    ) {
        self.generate_var(var, lean_info, rebind, indent);
    }

    fn generate_let(
        &mut self,
        assign: &AssertDesc,
        lean_info: &LeanFuncInfo,
        rebind: &mut VarRebind,
        indent: usize,
    ) {
        // The range check increment is not part of the specification.
        if lean_info.is_range_check_inc(&assign.expr) {
            return;
        }
        let (lhs, rhs) = rebind_assignment(assign, rebind);
        self.push_spec(indent, &format!("∃ {lhs} : {var_type}, {lhs} = {rhs}", var_type = self.var_type()));
    }

    fn generate_assert(
        &mut self,
        assert: &AssertDesc,
        lean_info: &LeanFuncInfo,
        block: &FuncBlock,
        rebind: &mut VarRebind,
        pc: usize,
        op_size: usize,
        indent: usize,
    ) {
        if lean_info.is_range_check(&assert.expr) {
            self.push_spec(
                indent,
                &format!("{is_range_checked} {var_name}",
                    is_range_checked = self.is_range_checked(),
                    var_name = rebind.get_var_name(&assert.lhs.name)));
        } else {
            self.push_spec( indent, &format!("{var_name} = {expr}",
                    var_name = rebind.get_var_name(&assert.lhs.name),
                    expr = rebind.replace_var_names_in_expr(&assert.expr.expr)));
        }
    }

    fn generate_jmp(
        &mut self,
        pc: usize,
        op_size: usize,
        indent: usize,
    ) {
        // Do nothing.
    }

    fn generate_jnz(
        &mut self,
        cond_var: &str,
        label: &str,
        rebind: &VarRebind,
        pc: usize,
        op_size: usize,
        indent: usize,
    ) {
        // Do nothing.
    }

    fn generate_branch_intro(
        &mut self,
        cond_var: &(String, Var),
        is_eq: bool,
        lean_info: &LeanFuncInfo,
        rebind: &mut VarRebind,
        pc: usize,
        indent: usize,
    ) -> usize {

        // Open the or block only if this is the first branch.
        let indent = if is_eq {
            self.push_spec(indent, "(");
            indent + 2
        } else {
            indent
        };

        let var_name = rebind.get_var_name(&cond_var.0);
        self.push_spec(indent, &format!("({var_name} {op} 0", op = if is_eq { "=" } else { "≠" }));

        indent + 2
    }

    fn generate_label_block(
        &mut self,
        func_name: &str,
        lean_info: &LeanFuncInfo,
        block: &FuncBlock,
        calling_block: &FuncBlock,
        rebind: &VarRebind,
        indent: usize,
    ) {
        let trace_count = if self.is_completeness { "" } else { "∃ (κ₁ : ℕ), " };
        let trace_count_var = if self.is_completeness { "" } else { " κ₁" };
        self.push_spec(
            indent,
            &format!(
                "{trace_count}auto_spec_{func_name}{block_suffix}{trace_count_var}",
                block_suffix = block.block_suffix())
        );

        let args_str = block.args.iter().chain(
            lean_info.get_explicit_ret_arg_names().iter()
        ).map(|arg| { rebind.get_var_name(arg) }).join(" ");

        let indent = indent + 2;
        self.push(indent, &args_str);

        // TO DO (yoavseginer): add the condition on the kappas.
    }

    fn generate_branch_close(
        &mut self,
        is_eq: bool,
        lean_info: &LeanFuncInfo,
        rebind: &mut VarRebind,
        indent: usize,
    ) -> usize {
        let indent = indent - 2;
        if is_eq {
            self.push_spec(indent, ") ∨");
            indent
        } else {
            self.push_spec(indent, ")");
            let indent = indent - 2;
            self.push_spec(indent, ")");
            indent
        }
    }

    fn generate_return_args(
        &mut self,
        ret_block_name: &str,
        branch_id: usize,
        ret_arg_names: &Vec<String>,
        ret_exprs: &Vec<String>,
        lean_info: &LeanFuncInfo,
        block: &FuncBlock,
        rebind: &VarRebind,
        indent: usize,
    ) {
        // We are only interested in the explicit arguments here.
        let ret_arg_names = &ret_arg_names[lean_info.ret_args.num_implicit_ret_args..];

        if lean_info.ret_args.branch_id_pos.is_some() {
            // The first return argument is the branch ID
            self.push_spec(indent, &format!("{ret_arg} = {branch_id}", ret_arg = &ret_arg_names[0]));
        }

        // For the explicit return arguments we use the last return arg names and the first
        // return expressions. Here, we determine the number of arg names to use.
        // The number of explicit return expressions in this return block.
        let explicit_len = lean_info.get_branch_explicit_arg_num(branch_id);

        // Use the last argument names for the explicit return argument expressions.
        for (name, expr) in ret_arg_names[ret_arg_names.len() - explicit_len..].iter().zip(
            ret_exprs[lean_info.ret_args.num_implicit_ret_args..].iter()
        ) {
            let expr = rebind.replace_var_names_in_expr(expr);
            self.push_spec(indent, &format!("{name} = {expr}"));
        }
    }

    fn generate_advance_ap(&mut self, step: &usize, pc: usize, op_size: usize, indent: usize) {
        // Do nothing.
    }

    fn generate_fail(&mut self, pc: usize, op_size: usize, indent: usize) {
        self.push_spec(indent, "False");
    }
}

struct AutoProof {
    statement: Vec<String>,
    main_proof: Vec<String>,
    final_proof: Vec<String>,
    is_lean3: bool,
}

impl AutoProof {
    pub fn new() -> AutoProof {
        AutoProof {
            statement: Vec::new(),
            main_proof: Vec::new(),
            final_proof: Vec::new(),
            is_lean3: false,
        }
    }

    fn get_full_proof(&self) -> Vec<String> {
        let mut full_proof = self.statement.clone();
        if self.is_lean3 {
            full_proof.push("begin".to_string());
        } else {
            full_proof.push("by".to_string());
        }

        full_proof.append(&mut self.main_proof.clone());

        if self.is_lean3 {
            full_proof.push("end".to_string());
        } else {
            full_proof.push("  done".to_string());
        }

        full_proof
    }

    fn push_statement(&mut self, indent: usize, str: &str) {
        self.statement.push(" ".repeat(indent) + str);
    }

    fn push_statement4(&mut self, indent: usize, str: &str, lean4str: &str) {
        if self.is_lean3 {
            self.statement.push(" ".repeat(indent) + str);
        } else {
            self.statement.push(" ".repeat(indent) + lean4str);
        }
    }

    fn push_main(&mut self, indent: usize, str: &str) {
        self.main_proof.push(" ".repeat(indent) + str);
    }

    fn push_main4(&mut self, indent: usize, str: &str, lean4str: &str) {
        if self.is_lean3 {
            self.main_proof.push(" ".repeat(indent) + str);
        } else {
            self.main_proof.push(" ".repeat(indent) + lean4str);
        }
    }

    fn push_main4_with_comma(&mut self, indent: usize, str: &str) {
       if self.is_lean3 {
            self.main_proof.push(" ".repeat(indent) + str + ",");
        } else {
            self.main_proof.push(" ".repeat(indent) + str);
        }
    }

    fn push_final4(&mut self, indent: usize, str: &str, lean4str: &str) {
        if self.is_lean3 {
            self.final_proof.push(" ".repeat(indent) + str);
        } else {
            self.final_proof.push(" ".repeat(indent) + lean4str);
        }
    }

    fn push_final4_with_comma(&mut self, indent: usize, str: &str) {
        if self.is_lean3 {
            self.final_proof.push(" ".repeat(indent) + str + ",");
        } else {
            self.final_proof.push(" ".repeat(indent) + str);
        }
    }

    fn append_proof(&mut self, more_proof: &Vec<String>) {
        self.main_proof.append(&mut more_proof.clone());
    }

    fn make_codes(&self, pc: usize, op_size: usize) -> String {
        (pc..pc + op_size).map(|i| format!("hmem{} hmem", i)).join(", ")
    }

    fn make_var_rw(&mut self, var_name: &str, lean_info: &LeanFuncInfo, rebind: &mut VarRebind) -> String {
        if lean_info.is_const(var_name) {
            var_name.into() // constants are their own rewrite
        } else {
            format!("htv_{}", rebind.get_var_name(var_name))
        }
    }

    // Generates the automatic proof for the variable assignment.
    fn make_var_assign_proof(&mut self, var: &VarDesc, lean_info: &LeanFuncInfo, rebind: &mut VarRebind) -> Vec<String> {

        let mut rws: Vec<String> = Vec::new();
        let mut op: &str = "";

        rws.push(format!("hl_{}", rebind.get_var_name(&var.name)));
        if let Some(expr) = &var.expr {
            rws.push(self.make_var_rw(&expr.var_a.name, lean_info, rebind));
            op = &expr.op;
            if let Some(expr_b) = &expr.var_b {
                rws.push(self.make_var_rw(&expr_b.name, lean_info, rebind));
            }
        }

        let tv_rw = format!("tv_{}", rebind.get_var_name(&var.name));

        if op == "/" {
            rws.push("div_eq_iff_mul_eq".into());
            vec![
                format!("rw [{all_rws}] ; exact {tv_rw}.symm", all_rws = rws.join(", ")),
                "apply PRIME.nat_cast_ne_zero (by norm_num1) ; rw [PRIME] ; norm_num1".into()
            ]
        } else {
            rws.push(tv_rw);
            if op == "-" {
                rws.push("add_sub_cancel".into())
            }
            vec![format!("rw [{all_rws}]", all_rws = rws.join(", "))]
        }
    }

    fn make_assert_proof(&mut self, assert: &AssertDesc, lean_info: &LeanFuncInfo, rebind: &mut VarRebind, pc: usize) -> Vec<String> {
        let mut rws: Vec<String> = Vec::new();
        let mut op: &str = "";

        rws.push(format!("htv_{}", rebind.get_var_name(&assert.lhs.name)));
        rws.push(self.make_var_rw(&assert.expr.var_a.name, lean_info, rebind));
        op = &assert.expr.op;
        if let Some(expr_b) = &assert.expr.var_b {
            rws.push(self.make_var_rw(&expr_b.name, lean_info, rebind));
        }

        // Don't apply the same rewrite twice.
        rws = rws.iter().unique().map(|rw| rw.into()).collect();

        let ha_rw = format!("ha{pc}");

        if op == "/" {
            rws.push("div_eq_iff_mul_eq".into());
            vec![
                format!("rw [{all_rws}] ; exact {ha_rw}", all_rws = rws.join(", ")),
                "apply PRIME.nat_cast_ne_zero (by norm_num1) ; rw [PRIME] ; norm_num1".into()
            ]
        } else {
            let exact: String = if op == "-" {
                rws.push(ha_rw);
                rws.push("add_sub_cancel".into());
                "".into()
            } else {
                format!(" ; exact {ha_rw}")
            };
            vec![format!("rw [{all_rws}]{exact}", all_rws = rws.join(", "))]
        }
    }

    fn generate_var(
        &mut self,
        var: &VarDesc,
        is_local: bool,
        lean_info: &LeanFuncInfo,
        rebind: &mut VarRebind,
        pc: usize,
        op_size: usize,
        indent: usize,
    ) {
        let (var_name, expr) = rebind_var_assignment(var, rebind);
        let has_expr = expr.is_some();
        self.push_main(indent, &format!("-- {var_type} {var_name}", var_type = if is_local { "localvar" } else { "tempvar" }));
        if has_expr {
            // Cayden TODO: How to do mostly similar strings but with different endings...
            let step_str4: String = format!("step_assert_eq {codes} with tv_{var_name}",
                codes = self.make_codes(pc, op_size));
            let str_copy = step_str4.clone();
            self.push_main4(indent, &(step_str4 + ","), &str_copy);
        }

        let mkdef_str4: String = format!("mkdef hl_{var_name} : {var_name} = {expr_str}",
        expr_str = if let Some(expr) = expr { expr } else { cell_expr_to_lean(&var.var_expr, false, false, false) });
        let mkdef_copy: String = mkdef_str4.clone();
        self.push_main4(indent, &(mkdef_str4 + ","), &mkdef_copy);

        let have_htv_str4: String = format!(
                "have htv_{var_name} : {var_name} = {cell_expr}",
                cell_expr = cell_expr_to_lean(&var.var_expr, false, false, false));
        let have_htv_copy: String = have_htv_str4.clone();
        self.push_main4(indent, &(have_htv_str4 + ","), &(have_htv_copy + " := by"));

        if has_expr {
            if self.is_lean3 {
                self.push_main(indent, "{ sorry },");
            } else {
                let indent = indent + 2;
                for line in self.make_var_assign_proof(var, lean_info, rebind) {
                    self.push_main(indent, &line);
                }
            }
            self.push_final4(0, &format!("use_only [{var_name}, hl_{var_name}],"),
                &format!("use_only {var_name}, hl_{var_name}"));
        } else {
            self.push_main4(indent, &format!("{{ exact hl_{var_name} }},"),
                &format!("  exact hl_{var_name}"));
            self.push_final4(0, &format!("use_only {var_name},"), &format!("use_only {var_name}"));
        }
    }

    /// Generates the final part of the proof.
    fn generate_final(
        &mut self,
        lean_info: &LeanFuncInfo,
        block: &FuncBlock,
        // If before returning another block is called, this is the called block object.
        return_block: Option<&FuncBlock>,
        // If before returning no block was called, this is the name of the return block.
        ret_block_name: Option<&str>,
        rc_cond_hyp: &str,
        rc_bound: Option<&str>,
        indent: usize
    ) {

        // Range check condition

        if lean_info.has_range_check_arg() {
            self.push_main(indent, "-- range check condition");
            let max_rc_count = lean_info.get_block_max_rc(block);
            // If the return block has a single max rc count, we use that.
            let ret_max_rc = if let Some(ret_block_name) = ret_block_name {
                Some(*lean_info.max_rc_counts.get(ret_block_name).expect("Count not find max rc for return block."))
            } else {
                lean_info.get_block_max_rc(return_block.expect("Return block info or name expected."))
            };

            if max_rc_count.is_none() {
                // The block may return with different rc counts.
                if let Some(ret_max_rc) = ret_max_rc {
                    self.push_main(indent, &format!("use_only {ret_max_rc}"));
                } else {
                    let suffix = return_block.expect("Return block expected.").block_suffix();
                    self.push_main(indent, &format!("use_only rc{suffix}"));
                }
            }

            self.push_main(indent, "constructor");

            if max_rc_count.is_none() && ret_max_rc.is_none() {
                // If the return block has return branches with different maximal rc counts, some extra Lean code
                // must be generated.
                let min_rc_count = if ret_max_rc.is_some() {
                    None
                } else {
                    Some(lean_info.get_block_min_max_rc(return_block.expect("Return block expected.")))
                };
                self.push_main(indent, "-- Something extra is needed here, but not yet implemented. Search for this comment in the generator.");
            }

            if let Some(rc_bound) = rc_bound {
                self.push_main(indent, &format!("apply le_trans {rc_bound} (Nat.le_add_right _ _)"));
            } else {
                self.push_main(indent, "norm_num1");
            }
            if max_rc_count.is_none() {
                // There is also a claim about the minimal value of the number of rc steps.
                self.push_main(indent, "constructor ; norm_num1");
            }
            self.push_main(indent, "constructor");
            self.push_main(indent, &format!("· arith_simps ; rw [{rc_cond_hyp}] ; try {{ norm_cast }}"));

            self.push_main(indent, "intro rc_h_range_check");
        }

        if block.label.is_none() {
            let suffices_str4: String = format!(
                "suffices auto_spec : auto_spec_{func_name} _ {args} {ret_args}",
                func_name = lean_info.func_name,
                args = lean_info.get_arg_names().join(" "),
                ret_args = iter::repeat('_').take(lean_info.explicit_ret_arg_num()).join(" ")
            );
            let suffices_copy: String = suffices_str4.clone();

            self.push_main4(indent, &(suffices_str4 + ","), &(suffices_copy + " by"));
            self.push_main4(indent, &format!(
                "{{ apply sound_{func_name}, apply auto_spec }},",
                func_name = lean_info.func_name),
            &format!(
                "  apply sound_{func_name} ; apply auto_spec",
                func_name = lean_info.func_name));
        }

        // As a last step, add the final proof to the main proof (with indentation).
        for line in self.final_proof.clone() {
            self.push_main(indent, &line);
        }
    }


}

impl LeanGenerator for AutoProof {

    /// Return a generator for a branch.
    fn branch(&self) -> Box<dyn LeanGenerator> {
        let mut branch = Box::new(AutoProof::new());
        branch.is_lean3 = self.is_lean3;
        branch.final_proof = self.final_proof.clone();
        branch
    }

    fn append_branch(&mut self, branch: &Box<dyn LeanGenerator>) {
        self.append_proof(branch.get_lean());
    }

    fn get_lean(&self) -> &Vec<String> {
        &self.main_proof
    }

    fn generate_statement(
        &mut self,
        main_func_name: &str,
        func_name: &str,
        lean_info: &LeanFuncInfo,
        block: &FuncBlock,
        indent: usize,
    ) {
        self.push_statement(
            indent,
            &format!("theorem auto_sound_{func_name}{block_suffix}", block_suffix = block.block_suffix())
        );

        let mut indent = indent + 2;

        let args_str = block.args.join(" ");

        // Arguments and hypotheses.

        if block.has_args() {
            self.push_statement(indent, "-- arguments");
            self.push_statement(indent, &format!("({args_str} : F)"));
        }

        self.push_statement(indent, "-- code is in memory at σ.pc + start offset");
        let pc_start = lean_info.get_pc_at(lean_info.casm_start);
        let pc_str = if pc_start == 0 { "σ.pc".into() } else { format!("(σ.pc + {pc_start})") };
        let lean_code_def_name = lean_code_name(main_func_name);
        self.push_statement4(indent, &format!("(hmem : mem_at mem {lean_code_def_name} {pc_str})"),
            &format!("(hmem : MemAt mem {lean_code_def_name} {pc_str})"));

        if block.has_args() {
            self.push_statement(indent, "-- input arguments on the stack");
            for (arg_name, arg_expr) in lean_info.get_block_args_and_lean_exprs(block, false, false) {
                if arg_name.len() == 0 {
                    continue;
                }
                self.push_statement(indent, &format!("(htv_{arg_name} : {arg_name} = {arg_expr})"));
            }
        }
        if block.label.is_some() {
            self.push_statement(indent, "(νbound : ℕ)");
        }

        // Conclusion
        self.push_statement(indent, "-- conclusion");
        if block.label.is_none() {
            self.push_statement4(indent, ": ensures_ret mem σ (λ κ τ,",
                ": EnsuresRet mem σ (fun κ τ =>");
        } else {
            self.push_statement4(indent, ": ensures_ret νbound mem",
                ": EnsuresbRet νbound mem");
            self.push_statement(
                indent + 2,
                &format!(
                        "{{ pc := σ.pc + {pc_offset}, ap := σ.ap + {ap_offset}, fp := σ.fp }}",
                        pc_offset = block.pc_start_pos,
                        ap_offset = block.ap_offset
                    )
                );
            self.push_statement4(indent + 2, "(λ κ τ,",
                "(fun κ τ =>");
        }

        let range_check_arg_name = lean_info.get_range_check_arg_name();
        let has_rc = range_check_arg_name.is_some();

        if let Some(rc_name) = range_check_arg_name {
            indent += 2;
            let max_rc = lean_info.get_block_max_rc(block);
            let rc_value: String = if let Some(max_rc) = max_rc {
                    max_rc.to_string()
                } else {
                    "μ".into()
                };
            let rc_bound: String = if let Some(max_rc) = max_rc {
                format!("{rc_value} ≤ κ ∧")
            } else {
                format!(
                    "∃ {rc_value} ≤ κ, {min_max_rc} ≤ {rc_value} ∧",
                    min_max_rc = lean_info.get_block_min_max_rc(block)
                )
            };
            self.push_statement4(
                indent,
                &format!(
                    "{rc_bound} rc_ensures mem (rc_bound F) {rc_value} {rc_expr} (mem (τ.ap - {rc_ret_offset}))",
                    rc_expr = lean_info.get_arg_expr(&rc_name, true, false, false).unwrap(),
                    rc_ret_offset = lean_info.get_ret_arg_offset(&rc_name).unwrap()),
                &format!(
                    "{rc_bound} RcEnsures mem (rcBound F) {rc_value} {rc_expr} (mem (τ.ap - {rc_ret_offset}))",
                    rc_expr = lean_info.get_arg_expr(&rc_name, true, false, false).unwrap(),
                    rc_ret_offset = lean_info.get_ret_arg_offset(&rc_name).unwrap()));
        }

        indent += 2;

        let spec_str: String = format!(
                "{open_par}{auto_prefix}spec_{func_name}{block_suffix} κ{sep}{args_str}{ret_arg_offsets}{close_par}) :=",
                open_par = if has_rc { "(" } else { "" },
                auto_prefix = if block.label.is_some() { "auto_" } else { "" },
                block_suffix = block.block_suffix(),
                sep = if 0 < args_str.len() { " " } else { "" },
                func_name = func_name,
                ret_arg_offsets = (0..lean_info.explicit_ret_arg_num()).rev()
                    .map(|i| format!(" (mem (τ.ap - {}))", i + 1))
                    .collect::<Vec<String>>()
                .   join(""),
                close_par = if has_rc { ")" } else { "" },);

        self.push_statement(indent, &spec_str);
    }

    fn generate_intro(
        &mut self,
        main_func_name: &str,
        func_name: &str,
        lean_info: &LeanFuncInfo,
        block: &FuncBlock,
        indent: usize,
    ) {
        if block.label.is_none() {
            // Only needed in the main function theorem.
            self.push_main4(indent, "apply ensures_of_ensuresb, intro νbound,",
                "apply ensures_of_ensuresb; intro νbound");
        }
        // Only in Lean 3 we unpack the memory at the beginning. In Lean 4 we unpack each
        // instruction as we are about to use it.
        if lean_info.is_lean3 {
            // unpack the memory
            let mut line = format!(
                "unpack_memory {code_def_name} at hmem with ⟨",
                code_def_name = lean_code_name(main_func_name)
            );
            let pc_start = lean_info.get_pc_at(lean_info.casm_start);
            let pc_end = lean_info.get_pc_at(lean_info.casm_end);
            for code_i in pc_start..pc_end {
                if 95 < line.len() {
                    if code_i != pc_start {
                        line.push(',');
                    }
                    self.push_main(indent, &line);
                    line = "".into();
                } else if code_i != pc_start {
                    line.push_str(", ");
                }
                line.push_str(&format!("hpc{code_i}"));
            }
            line.push_str("⟩");
            let lean4_line = line.clone();
            line.push_str(",");
            self.push_main4(indent, &line, &lean4_line);
        }
    }

    fn generate_temp_var(
        &mut self,
        var: &VarDesc,
        lean_info: &LeanFuncInfo,
        block: &FuncBlock,
        rebind: &mut VarRebind,
        pc: usize,
        op_size: usize,
        indent: usize,
    ) {
        self.generate_var(var, false, lean_info, rebind, pc, op_size, indent);
    }

    fn generate_implicit_temp_var(
        &mut self,
        var_desc: &VarBaseDesc,
        lean_info: &LeanFuncInfo,
        block: &FuncBlock,
        rebind: &mut VarRebind,
        pc: usize,
        op_size: usize,
        indent: usize,
    ) {
        self.generate_var(
            &VarDesc {
                name: var_desc.name.clone(),
                var_id: var_desc.var_id,
                var_expr: var_desc.var_expr.clone(),
                expr: None,
                ap_change: 0, // not used here.
            },
            false,
            lean_info,
            rebind,
            pc,
            op_size,
            indent
        );
    }

    fn generate_local_var(
        &mut self,
        var: &VarDesc,
        lean_info: &LeanFuncInfo,
        block: &FuncBlock,
        rebind: &mut VarRebind,
        pc: usize,
        op_size: usize,
        indent: usize,
    ) {
        self.generate_var(var, true, lean_info, rebind, pc, op_size, indent);
    }

    fn generate_let(
        &mut self,
        assign: &AssertDesc,
        lean_info: &LeanFuncInfo,
        rebind: &mut VarRebind,
        indent: usize,
    ) {
        self.push_main(indent, "-- let");
        let (lhs, rhs) = rebind_assignment(assign, rebind);

        let mkdef_str4: String = format!("mkdef hl_{lhs} : {lhs} = {rhs}");
        let mkdef_str_copy: String = mkdef_str4.clone();
        self.push_main4(indent, &(mkdef_str4 + ","), &mkdef_str_copy);

        // Assumes that all let expressions are 'let x = y'.
        let have_htv_str4: String = format!(
            "have htv_{lhs} : {lhs} = {cell_a}",
            cell_a = cell_expr_to_lean(&assign.expr.var_a.var_expr, false, false, false),
        );
        let have_htv_copy: String = have_htv_str4.clone();
        self.push_main4(indent, &(have_htv_str4 + ","), &(have_htv_copy + " := by"));

        // Proof
        if lean_info.is_lean3 {
            self.push_main(indent, "{ sorry },");
        } else {
            self.push_main(indent + 2, &format!("rw [hl_{lhs}, htv_{rhs}]"));
        }

        self.push_final4(0, &format!("use_only [{lhs}, hl_{lhs}],"),
            &format!("use_only {lhs}, hl_{lhs}"));
    }

    fn generate_assert(
        &mut self,
        assert: &AssertDesc,
        lean_info: &LeanFuncInfo,
        block: &FuncBlock,
        rebind: &mut VarRebind,
        pc: usize,
        op_size: usize,
        indent: usize,
    ) {
        let lhs =  rebind.get_var_name(&assert.lhs.name);
        if lean_info.is_range_check(&assert.expr) {
            self.push_main(indent, &format!("-- range check for {lhs}"));
            self.push_main4_with_comma(indent, &format!("step_assert_eq {codes} with rc_{lhs}",
                    codes = self.make_codes(pc, op_size)));

            let rc_ptr = rebind.get_var_name(lean_info.get_range_check_ptr_name(&assert.expr).unwrap());
            let rc_offset = lean_info.get_offset_at(&assert.expr).unwrap_or(0.into());

            self.push_final4(
                0,
                &format!("cases rc_h_{rc_ptr}' ({rc_offset}) (by norm_num1) with n hn, arith_simps at hn,"),
                &format!("rc_app rc_h_range_check {rc_offset} htv_{lhs} rc_{lhs}"));
            self.push_final4(
                0,
                &format!("use_only [n], {{ simp only [htv_{lhs}, rc_{lhs}], arith_simps, exact hn }},"),
                "");
        } else {
            self.push_main(indent, "-- assert");
            self.push_main4_with_comma(indent, &format!("step_assert_eq {codes} with ha{pc}",
                    codes = self.make_codes(pc, op_size)));
            let rhs = rebind.replace_var_names_in_expr(&assert.expr.expr);

            let a_str4: String = format!("have a{pc} : {lhs} = {rhs}");
            let a_copy_str: String = a_str4.clone();
            self.push_main4(indent, &(a_str4 + ","), &(a_copy_str + " := by"));
            if self.is_lean3 {
                self.push_main(indent, "{ sorry },");
            } else {
                let indent = indent + 2;
                for line in self.make_assert_proof(assert, lean_info, rebind, pc) {
                    self.push_main(indent, &line);
                }
            }

            self.push_final4(0, &format!("use_only [a{pc}],"),
                &format!("use_only a{pc}"));
        }
    }

    fn generate_jmp(
        &mut self,
        pc: usize,
        op_size: usize,
        indent: usize,
    ) {
        self.push_main4_with_comma(indent, &format!("step_jump_imm {codes}",
            codes = self.make_codes(pc, op_size)));
    }

    fn generate_jnz(
        &mut self,
        cond_var: &str,
        label: &str,
        rebind: &VarRebind,
        pc: usize,
        op_size: usize,
        indent: usize,
    ) {
        let cond_var = rebind.get_var_name(cond_var);
        self.push_main(indent, &format!("-- jump to {label} if {cond_var} != 0"));
        self.push_main4_with_comma(indent, &format!("step_jnz {codes} with hcond{pc} hcond{pc}",
            codes = self.make_codes(pc, op_size)));
    }

    fn generate_branch_intro(
        &mut self,
        cond_var: &(String, Var),
        is_eq: bool,
        lean_info: &LeanFuncInfo,
        rebind: &mut VarRebind,
        pc: usize,
        indent: usize,
    ) -> usize {
        let mut indent = indent;
        let var_name = rebind.get_var_name(&cond_var.0);
        if is_eq {
            self.push_main4(indent, "{", "·");
            indent += 2;
        }

        let op = if is_eq { "=" } else { "≠" };
        self.push_main(indent, &format!("-- {var_name} {op} 0"));
        self.push_main4(indent, &format!("have a{pc} : {var_name} {op} 0, {{ simp only [htv_{var_name}], exact hcond{pc} }},"),
            &format!("have a{pc} : {var_name} {op} 0 := by simp only [htv_{var_name}]; exact hcond{pc}"));
        self.push_final4_with_comma(0, if is_eq { "left" } else { "right" });
        self.push_final4(0, &format!("use_only [a{pc}],"),
            &format!("use_only a{pc}"));

        indent
    }

    fn generate_label_block(
        &mut self,
        func_name: &str,
        lean_info: &LeanFuncInfo,
        block: &FuncBlock,
        calling_block: &FuncBlock,
        rebind: &VarRebind,
        indent: usize,
    ) {
        // lean_info.has_range_check_arg()

        self.push_main(indent, "arith_simps");
        self.push_main(
            indent,
            &format!(
                "apply ensuresbRet_trans (auto_sound_{func_name}{block_suffix} mem σ",
                block_suffix = block.block_suffix())
        );

        let arg_strs: Vec<String> = block.args.iter().map(
            |arg| { rebind.get_var_name(arg) }
        ).collect();

        {
            let indent = indent + 2;
            self.push_main(indent, &arg_strs.join(" "));
            self.push_main(indent, "hmem");
            for arg in arg_strs {
                self.push_main(indent, &format!("htv_{arg}"));
            }
            self.push_main(indent, "νbound)");
        }

        let suffix = block.block_suffix();
        self.push_main(indent, &format!("intros κ{suffix} _ h{suffix}"));
        if lean_info.has_range_check_arg() {
            if lean_info.get_block_max_rc(block).is_none() {
                self.push_main(
                    indent,
                    &format!("rcases h{suffix} with ⟨rc{suffix}, rc_m_le{suffix}, h_rc_min{suffix}, hblk_range_check_ptr, h{suffix}⟩")
                );
            } else {
                self.push_main(
                    indent,
                    &format!("rcases h{suffix} with ⟨rc_m_le{suffix}, hblk_range_check_ptr, h{suffix}⟩")
                );
            }
            self.push_final4_with_comma(0, &format!("use_only κ{suffix}"));
            self.push_final4_with_comma(0, &format!("apply h{suffix} rc_h_range_check"));
        }

        self.generate_final(
            lean_info,
            calling_block,
            Some(block),
            None,
            "hblk_range_check_ptr",
            Some(&format!("rc_m_le{suffix}")),
            indent
        );

    }

    fn generate_branch_close(
        &mut self,
        is_eq: bool,
        lean_info: &LeanFuncInfo,
        rebind: &mut VarRebind,
        indent: usize,
    ) -> usize {
        if is_eq {
            let indent = indent - 2;
            self.push_main4(indent, "},", "  done");
            indent
        } else {
            indent
        }
    }

    fn generate_return_args(
        &mut self,
        ret_block_name: &str,
        branch_id: usize,
        ret_arg_names: &Vec<String>,
        ret_exprs: &Vec<String>,
        lean_info: &LeanFuncInfo,
        block: &FuncBlock,
        rebind: &VarRebind,
        indent: usize,
    ) {
        let rc_arg_name = lean_info.get_range_check_arg_name().unwrap_or("".into());
        let mut rc_ret_name: String = "".into();

        let mut casm_pos = lean_info.get_ret_block_assert_start_casm_pos(lean_info, branch_id);
        let mut pc = lean_info.get_pc_at(casm_pos);

        self.push_main(indent, "-- return values");

        if let Some(ap_step_size) = lean_info.ret_branch_ap_steps.get(branch_id) {
            if 0 < *ap_step_size {
                // The ap step is the first instruction in the block, before the asserts
                let casm_pos = casm_pos - 1;
                let pc = lean_info.get_pc_at(casm_pos);
                let op_size = lean_info.casm_instructions[casm_pos].body.op_size();
                self.push_main4_with_comma(indent, &format!("step_advance_ap {codes}", codes = self.make_codes(pc, op_size)));
            }
        }

        if 0 < ret_arg_names.len() {
            self.push_final4_with_comma(0, "arith_simps");
        }

        // For the explicit return arguments we use the last return arg names and the first
        // return expressions. Here, we determine the number of explicit arguments (at the end of
        // the argument list).
        let explicit_len = lean_info.get_branch_explicit_arg_num(branch_id);

        let first_explicit_name = ret_arg_names.len() - explicit_len;
        let first_explicit_expr = lean_info.ret_args.num_implicit_ret_args;
        // If there is no branch ID position, set it to be a position beyond the end of the return arguments.
        let branch_id_pos = if let Some(pos) = lean_info.ret_args.branch_id_pos { pos } else { ret_arg_names.len() };

        let max_rc_count = *lean_info.max_rc_counts.get(ret_block_name).unwrap_or(&0);

        for (pos, ret_name) in ret_arg_names.iter().enumerate() {
            let op_size = lean_info.casm_instructions[casm_pos].body.op_size();
            let mut ret_name: String = ret_name.strip_prefix("ρ_").unwrap_or(ret_name).into();
            let is_rc = 0 < max_rc_count && rc_arg_name == ret_name;
            if is_rc {
                ret_name = rebind.get_next_name(&ret_name);
                self.push_main(indent, "--   range check return value");
            }

            self.push_main4_with_comma(indent, &format!("step_assert_eq {codes} with ret_{ret_name}",
                    codes = self.make_codes(pc, op_size)));
            let is_last = pos == ret_arg_names.len() - 1;
            if is_rc {
                self.push_main4_with_comma(indent, &format!("mkdef hl_{ret_name} : {ret_name} = {rc_arg_name} + {max_rc_count}"));

                let htv_str4: String = format!("have htv_{ret_name} : {ret_name} = _");
                let htv_copy: String = htv_str4.clone();
                self.push_main4(indent, &(htv_str4 + ","), &(htv_copy + " := by"));
                self.push_main4(indent, &format!("{{ apply eq.symm, apply eq.trans ret_{ret_name},"),
                    &format!("  apply Eq.symm; apply Eq.trans ret_{ret_name}"));
                self.push_main4(indent, &format!("simp only [hl_{ret_name}, htv_{rc_arg_name}] }},"),
                    &format!("  simp only [hl_{ret_name}, htv_{rc_arg_name}]"));

                rc_ret_name = ret_name;
            } else if pos == branch_id_pos {
                if is_last ||  explicit_len == 0 {
                    self.push_final4_with_comma(0,  &format!("exact ret_{ret_name}"));
                } else {
                    self.push_final4_with_comma(0,  &format!("use_only ret_{ret_name}"));
                }
            } else if first_explicit_name <= pos {
                let ret_expr = &ret_exprs[pos - first_explicit_name + first_explicit_expr];
                if is_last {
                    self.push_final4(0, &format!("rw [htv_{ret_expr}], exact ret_{ret_name},"),
                        &format!("rw [htv_{ret_expr}] ; exact ret_{ret_name}"));
                } else {
                    self.push_final4(0, &format!("split, rw [htv_{ret_expr}], exact ret_{ret_name},"),
                        &format!("constructor ; rw [htv_{ret_expr}] ; exact ret_{ret_name}"));
                }
            }

            casm_pos += 1;
            pc += op_size;
        }

        let last_instr = &lean_info.casm_instructions[casm_pos].body;
        if let InstructionBody::Jump(instr) = last_instr {
            let op_size = last_instr.op_size();
            let jump_imm_str4: String = format!("step_jump_imm {codes}",
                    codes = self.make_codes(pc, op_size));
            let ji_copy: String = jump_imm_str4.clone();
            self.push_main4(indent, &(jump_imm_str4 + ","), &ji_copy);
        }

        // Add the final return
        pc = lean_info.get_code_len() - 1;
        self.push_main4_with_comma(indent, &format!("step_ret {codes}",
                codes = self.make_codes(pc, 1)));

        self.push_main(indent, "step_done");
        self.push_main4(indent, "use_only [rfl, rfl],", "use_only rfl, rfl");

        self.generate_final(
            lean_info, block,
            None,
            Some(ret_block_name),
            &format!("ret_{rc_ret_name}"),
            None,
            indent
        );
    }

    fn generate_advance_ap(&mut self, step: &usize, pc: usize, op_size: usize, indent: usize) {
        self.push_main4_with_comma(indent, &format!("step_advance_ap {codes}",
                codes = self.make_codes(pc, op_size)));
    }

    fn generate_fail(&mut self, pc: usize, op_size: usize, indent: usize) {
        self.push_main(indent, "-- fail");
        self.push_main(indent, &format!("step_assert_eq {codes} with ha_fail",
                codes = self.make_codes(pc, op_size)));
        self.push_main4(indent, "exfalso, apply zero_ne_one (add_left_cancel (eq.trans _ ha_fail)), rw add_zero,",
            "exfalso; apply zero_ne_one (add_left_cancel (Eq.trans _ ha_fail)); rw [add_zero]");
    }
}

struct CompletenessProof {

    /// Was this branch was created as the 'equals' branch in a conditional
    /// jump? In all other cases, false.
    is_eq_branch: bool,
    statement: Vec<String>,

    /// For each positive offset from the initial ap, the name of the variable
    /// stored at that ap offset. When a position is skipped, we use the string "0".
    ap_assignments: Vec<String>,
    /// The variables which were ranged checked at different offsets from the rc pointer.
    rc_vals: Vec<String>,

    /// Number of local variables allocated so far.
    num_local_vars: usize,
    /// The rcases statment(s) that prepare the the specs to be used for constructing
    /// the local assignment.
    spec_rcases: Vec<String>,

    /// The main proof. This proof is for a single path through the code.
    main_proof: Vec<String>,

    /// The lean code generated.
    lean_code: Vec<String>,
}

impl CompletenessProof {
    pub fn new() -> CompletenessProof {
        CompletenessProof {
            is_eq_branch: false,
            statement: Vec::new(),
            ap_assignments: Vec::new(),
            rc_vals: Vec::new(),
            num_local_vars: 0,
            spec_rcases: Vec::new(),
            main_proof: Vec::new(),
            lean_code: Vec::new(),
        }
    }

    fn push_statement(&mut self, indent: usize, str: &str) {
        self.statement.push(" ".repeat(indent) + str);
    }

    fn push_main(&mut self, indent: usize, str: &str) {
        self.main_proof.push(" ".repeat(indent) + str);
    }

    fn push_lean(&mut self, indent: usize, str: &str) {
        self.lean_code.push(" ".repeat(indent) + str);
    }

    fn get_full_proof(&self) -> Vec<String> {
        let mut full_proof = self.statement.clone();

        full_proof.append(&mut self.lean_code.clone());
        full_proof.push("  done".to_string());

        full_proof
    }

    fn make_block_theorem_name(&self, lean_info: &LeanFuncInfo, block: &FuncBlock) -> String {
        format!(
            "complete_{func_name}{block_suffix}_from_spec",
            func_name = lean_info.func_name,
            block_suffix = block.block_suffix(),
        )
    }

    fn make_codes(&self, pc: usize, op_size: usize) -> String {
        (pc..pc + op_size).map(|i| format!("hmem{} hmem", i)).join(", ")
    }

    fn make_start_local_ap_expr(&self, block: &FuncBlock) -> String {
        if block.start_local_ap == 0 { "σ.ap".into() } else { format!("(σ.ap + {})", block.start_local_ap) }
    }

    fn make_start_ap_expr(&self, block: &FuncBlock) -> String {
        if block.ap_offset == 0 { "σ.ap".into() } else { format!("(σ.ap + {})", block.ap_offset) }
    }

    fn make_start_rc_expr(&self, lean_info: &LeanFuncInfo, block: &FuncBlock, explicit_zero_offset: bool) -> String {
        let rc_name = lean_info.get_range_check_arg_name();
        if let Some(rc_name) = rc_name {
            if !explicit_zero_offset && block.start_rc == 0 {
                rc_name
            } else {
                format!("({} + {})", rc_name, block.start_rc)
            }
        } else {
            "(0 : ℤ)".into()
        }
    }

    fn get_var_ap_offset(&self, var_name: &str, rebind: &VarRebind) -> Option<i16> {
        if let Some(expr) = rebind.get_expr(var_name) {
            // get the offset from the cell expression
            if let Some((_, offset)) = get_ref_from_deref(&expr) {
                Some(offset)
            } else {
                None
            }
        } else {
            None
        }
    }

    /// Adds the arguments and hypotheses of the completeness theorem for a block
    /// to the statement of the theorem.
    fn generate_args_and_hypotheses(
        &mut self,
        lean_info: &LeanFuncInfo,
        block: &FuncBlock,
        indent: usize,
    ) {
        let args_str = block.args.join(" ");
        if block.has_args() {
            self.push_statement(indent, "-- arguments");
            self.push_statement(indent, &format!("({args_str} : ℤ)"));
        }

        self.push_statement(indent, "-- code is in memory at σ.pc + start offset");
        let pc_start = lean_info.get_pc_at(lean_info.casm_start);
        let pc_str = if pc_start == 0 { "σ.pc".into() } else { format!("(σ.pc + {pc_start})") };
        let lean_code_def_name = lean_vm_code_name(&lean_info.main_func_name);
        self.push_statement(indent, &format!("(hmem : ProgAt mem {lean_code_def_name} {pc_str})"));


        self.push_statement(indent, "-- input arguments on the stack");
        self.push_statement(indent, "(hin_fp : σ.fp = σ.ap)");
        for (arg_name, arg_expr) in lean_info.get_block_args_and_lean_exprs(block, true, true) {
            if arg_name.len() == 0 {
                continue;
            }
            let var_type = lean_info.get_vm_var_type(&arg_name);
            self.push_statement(indent, &format!("(htv_{arg_name} : {var_type} {arg_name} = {arg_expr})"));
        }

        // Add the spec assumption
        for line in generate_completeness_block_spec_hyp(lean_info, block,"(h_spec: ", ")") {
            self.push_statement(indent, &line);
        }
    }

    /// Adds the conclusion of the theorem for a block to the statement of the theorem.
    fn generate_conclusion(
        &mut self,
        lean_info: &LeanFuncInfo,
        block: &FuncBlock,
        indent: usize,
    ) {
        // Expression for the ap of the state at the beginning of the block.
        let ap_expr = self.make_start_ap_expr(block);
        // Create the start ap and rc memory offsets expressions for the local assignment of this block.
        let start_local_ap_expr = self.make_start_local_ap_expr(block);
        // When the offset is zero, and this is not the main block, we sometimes need the zero offset to appear explicitly
        // and sometimes not (as the + 0 is sometimes simplified away).
        let rc_expr_explicit_offset = self.make_start_rc_expr(lean_info, block, block.pc_start_pos != 0);
        let rc_expr = self.make_start_rc_expr(lean_info, block, false);
        let start_state = if block.pc_start_pos == 0 {
                "σ".into()
            } else {
                format!("{{ pc := σ.pc + {}, ap := {}, fp := σ.fp }}", block.pc_start_pos, ap_expr)
            };

        // Conclusion
        self.push_statement(indent, "-- conclusion");
        self.push_statement(indent, &format!(": ∃ loc : LocalAssignment {start_local_ap_expr} {rc_expr_explicit_offset},"));

        let indent = indent + 2;

        self.push_statement(indent, &format!("VmRangeChecked loc.rc_vals {rc_expr_explicit_offset} loc.rc_num u128Limit ∧"));
        self.push_statement(indent, &format!("Returns PRIME (Assign mem loc) {start_state} (fun κ τ =>"));

        let indent = indent + 2;

        if let Some(rc_name) = lean_info.get_range_check_arg_name() {
            self.push_statement(indent, &format!("τ.ap = {start_local_ap_expr} + loc.exec_num ∧"));
            self.push_statement(
                indent,
                &format!("Assign mem loc (exec (τ.ap - {num_ret})) = rc ({rc_expr} + loc.rc_num)) := by",
                    num_ret = lean_info.get_ret_arg_offset(&rc_name).expect("Failed to find rc return arg offset."),
                ),
            );
        } else {
            self.push_statement(indent, &format!("τ.ap = {start_local_ap_expr} + loc.exec_num"));
        }
    }

    /// To be used for variables allocated on the stack. Not for let variables.
    fn generate_var(
        &mut self,
        var: &VarDesc,
        is_local: bool,
        lean_info: &LeanFuncInfo,
        block: &FuncBlock,
        rebind: &mut VarRebind,
        pc: usize,
        op_size: usize,
        indent: usize,
    ) {
        // Use 0 indentation here. Indentation is added later (and is the same
        // for all statements).
        let indent = 0;

        let (var_name, expr) = rebind_var_assignment(var, rebind);
        let (_, var_offset) = get_ref_from_deref(&var.var_expr).expect("Failed to find var offset");

        // TODO: Below, should use the var_offset instead of guessing the location.
        // Moreover, local variable assignment is problematic here (because it can be before
        // the beginning of the block start ap).
        let ap_pos = if !is_local {
                // Allocate the next ap position for this variable.
                self.ap_assignments.push(format!("val {var_name}"));
                self.ap_assignments.len() - 1

            } else {
                if self.ap_assignments.len() <= self.num_local_vars {
                    self.ap_assignments.push(format!("val {var_name}"));
                } else {
                    self.ap_assignments[self.num_local_vars] = format!("val {var_name}");
                }
                self.num_local_vars += 1;
                self.num_local_vars - 1
            };

        self.spec_rcases.push(var_name.clone());
        if let Some(expr) = &var.expr {
            let assert_hyp = format!("h_{var_name}");
            self.spec_rcases.push(assert_hyp.clone());
            let (rhs_var_a, rhs_var_b, simps) =
                CompletenessProof::get_rhs_vars_and_simps(&expr, lean_info, &assert_hyp);
            self.generate_assert_main(
                lean_info,
                block,
                &var.name,
                get_ref_from_deref(&var.var_expr).expect("Failed to find variable AP offset").1,
                rhs_var_a,
                rhs_var_b,
                String::from("+-*/").find(&expr.op).is_some(),
                None,
                &assert_hyp,
                Some(&simps),
                pc,
                op_size,
                indent
            );
        }
    }

    fn generate_assert_var_simp(
        &mut self,
        block: &FuncBlock,
        var_name: &str,
        ap_offset: i16,
        indent: usize,
    ) {
        // Simplify the variables in this expression.
        if ap_offset < 0 {
            self.push_main(indent, &format!("simp only [h_ap_minus_{offset}]", offset = -ap_offset));
        } else {
            if ap_offset == 0 {
                self.push_main(indent, "try simp only [add_zero]");
            }
            self.push_main(indent, &format!("simp only [h_ap_plus_{ap_offset}]"));
        }

        if ap_offset < 0 || ap_offset.to_usize().unwrap() < block.start_local_ap {
            self.push_main(indent, &format!("simp only [←htv_{var_name}]"));
        } /*else if ap_offset == 0 {
            self.push_main(indent, "simp only [Int.sub_self]");
        } else {
            self.push_main(indent, &format!("simp only [Int.add_comm σ.ap {ap_offset}, Int.add_sub_cancel]"));
        }*/
    }

    /// Generates the description of the rhs variables of an assert or variable assignment. Also generates
    /// any special simplifications that need to be applied to the assert hypothesis for this expression.
    fn get_rhs_vars_and_simps(
        expr: &ExprDesc,
        lean_info: &LeanFuncInfo,
        assert_hyp: &str,
    ) -> (Option<(String, i16)>, Option<(String, i16)>, Vec<String>) {

        let rhs_var_a = if let Some((_, var_a_offset)) = get_ref_from_deref(&expr.var_a.var_expr) {
            Some((expr.var_a.name.clone(), var_a_offset))
        } else if expr.op == "*()" {
            match expr.var_a.var_expr {
                CellExpression::BinOp { op: _, a , b: _ } => {
                    Some((expr.var_a.name.clone(), a.offset))
                },
                CellExpression::DoubleDeref(a, _) => {
                    Some((expr.var_a.name.clone(), a.offset))
                },
                _ => None
            }
        } else {
            None
        };
        let rhs_var_b = match &expr.var_b {
                Some(var_b) =>
                    if let Some((_, var_b_offset)) = get_ref_from_deref(&var_b.var_expr) {
                        Some((var_b.name.clone(), var_b_offset))
                    } else {
                        None
                    },
                _ => None
            };

        let mut simps: Vec<String> = Vec::new();

        if expr.op == "-" {
            simps.push(format!("rw [eq_sub_iff_add_eq] at {assert_hyp}"));
        }
        if lean_info.is_const(&expr.var_a.name) {
            simps.push(format!("simp only [{const_name}] at {assert_hyp}", const_name = expr.var_a.name));
        }
        if let Some(var_b) = &expr.var_b {
            if var_b.name != expr.var_a.name && lean_info.is_const(&var_b.name) {
                simps.push(format!("simp only [{const_name}] at {assert_hyp}", const_name = var_b.name));
            }
        }

        (rhs_var_a, rhs_var_b, simps)
    }

    fn generate_assert_main(
        &mut self,
        lean_info: &LeanFuncInfo,
        block: &FuncBlock,
        lhs_name: &str,
        lhs_ap_offset: i16,
        rhs_var_a: Option<(String, i16)>,
        rhs_var_b: Option<(String, i16)>,
        rhs_binary_op: bool,
        // If this is a range check, the offset from the rc pointer.
        rc_check_offset: Option<BigInt>,
        // The spec hypothesis that applies to this assert.
        assert_hyp: &str,
        // Simplifications that need to be applied ot the assert hypothesis
        assert_simps: Option<&Vec<String>>,
        pc: usize,
        op_size: usize,
        indent: usize,
    ) {
        // Use 0 indentation (indentation is added later).
        let indent = 0;
        self.push_main(indent, &format!("vm_step_assert_eq {codes}", codes = self.make_codes(pc, op_size)));
        self.push_main(indent, "constructor");
        // Simplify numeric expressions.
        self.push_main(indent, "· try simp only [neg_clip_checked', ←Int.sub_eq_add_neg]");
        let indent = indent + 2;
        self.push_main(indent, "try norm_num1");
        // Convert fp references to ap references (if needed).
        self.push_main(indent, "try simp only [hin_fp]");

        // If the expression consists of an immediate value, simplify it.
        if 1 < op_size && (rhs_var_a.is_none() || (rhs_binary_op && rhs_var_b.is_none())) {
            self.push_main(
                indent,
                &format!(
                    "rw [assign_prog] ; rw [{code}]",
                    // An immediate value is always the second of two values of a casm instruction.
                    code = self.make_codes(pc + 1, 1),
                )
            );
        }

        // Simplify the variables in this expression.
        self.generate_assert_var_simp(block, lhs_name, lhs_ap_offset, indent);
        if let Some((var_a_name, var_a_offset)) = &rhs_var_a {
            self.generate_assert_var_simp(block, var_a_name, *var_a_offset, indent);
        }
        if let Some((var_b_name, var_b_offset)) = &rhs_var_b {
            match &rhs_var_a {
                Some((var_a_name, var_a_offset)) if var_a_name == var_b_name => {},
                _ => self.generate_assert_var_simp(block, var_b_name, *var_b_offset, indent)
            }
        }

        let is_rc_var = lean_info.get_vm_var_type(lhs_name) == "rc";

        // If this is a range check, simplify the range check expression.
        if let Some(rc_offset) = rc_check_offset {
            let offset = rc_offset.to_usize().expect("rc offset out of bounds");
            self.push_main(indent, &format!("simp only [h_rc_plus_{offset}]"));
            self.push_main(indent, "try simp only [add_sub_add_comm, add_sub_right_comm, sub_add_cancel', sub_self] ; try norm_num1");
            // self.push_main(indent, "try ring_nf");
            // self.push_main(indent, &format!("simp only [Int.add_comm _ {offset}, Int.add_sub_cancel]"));
        } else if !is_rc_var {
            self.push_main(indent, "try simp only [add_sub_add_comm, add_sub_right_comm, sub_add_cancel', sub_self] ; try norm_num1");
            // self.push_main(indent, "try ring_nf");
            if let Some(simps) = assert_simps {
                for simp in simps {
                    self.push_main(indent, simp);
                }
            }
            self.push_main(indent, &format!("simp only [{assert_hyp}]"));
        } else {
            self.push_main(indent, "try simp only [add_sub_add_comm, add_sub_right_comm, sub_add_cancel', sub_self] ; try norm_num1");
            // self.push_main(indent, "try ring_nf");
        }

        if is_rc_var {
            self.push_main(indent, "apply Mrel.Equiv.refl_rc");
        } else {
            self.push_main(indent, "apply Mrel.Equiv.refl_val");
        }
    }

    /// Add the return block variables (and the associated hypotheses) to the list
    /// of locally assigned variables and the list of spec rcases.
    fn add_return_block_vars_and_hyp(
        &mut self,
        ret_block_name: &str,
        branch_id: usize,
        lean_info: &LeanFuncInfo,
    ) {
        // If AP advanced at the beginning of the return block, assign skipped
        // positions a value of 0.
        if let Some(ap_step_size) = lean_info.ret_branch_ap_steps.get(branch_id) {
            for pos in 0..*ap_step_size {
                self.ap_assignments.push("val 0".into());
            }
        }

        // Add the implicit arguments (only the range check argument is handled properly).
        let rc_arg_name = lean_info.get_range_check_arg_name().unwrap_or("".into());
        for arg_name in lean_info.get_arg_names()[..lean_info.ret_args.num_implicit_ret_args].iter() {
            if *arg_name == rc_arg_name {
                let max_rc_count = *lean_info.max_rc_counts.get(ret_block_name).unwrap_or(&0);
                self.ap_assignments.push(format!("rc ({rc_arg_name} + {max_rc_count})"));
            } else {
                self.ap_assignments.push("val 0".into());
            }
        }

        // Add the explicit arguments.
        let ret_arg_names = &lean_info.get_all_ret_arg_names()[lean_info.ret_args.num_implicit_ret_args..];

        // Add the return variables (pushed on the stack).
        for var_name in ret_arg_names {
            self.spec_rcases.push(format!("h_{var_name}"));
            self.ap_assignments.push(format!("val {var_name}"));
        }
    }

    /// Generates the main proof steps for the return block.
    fn generate_return_block_steps(
        &mut self,
        ret_block_name: &str,
        branch_id: usize,
        ret_arg_names: &Vec<String>,
        ret_exprs: &Vec<String>,
        lean_info: &LeanFuncInfo,
        // The function block in which the return block was called.
        block: &FuncBlock,
        rebind: &VarRebind,
        indent: usize,
    ) {
        // Use 0 indentation (indentation is added later).
        let indent = 0;

        let rc_arg_name = lean_info.get_range_check_arg_name().unwrap_or("".into());
        let mut rc_ret_name: String = "".into();

        let mut casm_pos = lean_info.get_ret_block_assert_start_casm_pos(lean_info, branch_id);
        let mut pc = lean_info.get_pc_at(casm_pos);

        if let Some(ap_step_size) = lean_info.ret_branch_ap_steps.get(branch_id) {
            if 0 < *ap_step_size {
                // The ap step is the first instruction in the block, before the asserts
                let casm_pos = casm_pos - 1;
                let pc = lean_info.get_pc_at(casm_pos);
                let op_size = lean_info.casm_instructions[casm_pos].body.op_size();
                self.push_main(indent, &format!("vm_step_advance_ap {codes}", codes = self.make_codes(pc, op_size)));
            }
        }

        // For the explicit return arguments we use the last return arg names and the first
        // return expressions. Here, we determine the number of explicit arguments (at the end of
        // the argument list).
        let explicit_len = lean_info.get_branch_explicit_arg_num(branch_id);
        let first_explicit_name = ret_arg_names.len() - explicit_len;
        let first_explicit_expr = lean_info.ret_args.num_implicit_ret_args;
        // If there is no branch ID position, set it to be a position beyond the end of the return arguments.
        let branch_id_pos = if let Some(pos) = lean_info.ret_args.branch_id_pos { pos } else { ret_arg_names.len() };

        let max_rc_count = *lean_info.max_rc_counts.get(ret_block_name).unwrap_or(&0);

        // All the return positions were added at the end of the ap assignment, so we find the ap offset
        // (relative to σ.ap) based on their number.
        let first_ret_ap_offset: i16 = (self.ap_assignments.len() + block.start_local_ap - lean_info.ret_arg_num()).try_into().expect("offset out of bound");

        // Go over all return argument positions, also those that are not used by this branch.
        // TODO: this code is partially duplicated in the other generators, merge the common parts.
        for (pos, ret_name) in ret_arg_names.iter().enumerate() {
            let op_size = lean_info.casm_instructions[casm_pos].body.op_size();
            let base_ret_name: String = ret_name.strip_prefix("ρ_").unwrap_or(ret_name).into();
            let is_rc = 0 < max_rc_count && rc_arg_name == base_ret_name;
            if is_rc {
                self.push_main(indent, "--   range check return value");
            }

            // Generate the code to handle the assert.

            let rhs_var_a = if is_rc {
                    if let Some((_, offset)) = get_ref_from_deref(
                        rebind.get_expr(&base_ret_name).expect("Failed to find range check in rebind table.")
                    ) {
                        Some((rc_arg_name.clone(), offset))
                    } else {
                        None
                    }
                } else if pos == branch_id_pos {
                    None
                } else if first_explicit_name <= pos {
                    // The variable which is copied to the return position.
                    let ret_expr = &ret_exprs[pos - first_explicit_name + first_explicit_expr];
                    if let Some((_, offset)) = get_ref_from_deref(
                        rebind.get_expr(ret_expr).expect("Failed to find return expression variable.")
                    ) {
                        Some((ret_expr.clone(), offset))
                    } else {
                        None
                    }
                } else {
                    // Empty return positions (return 0).
                    None
                };

            self.generate_assert_main(
                lean_info,
                block,
                &ret_name,
                first_ret_ap_offset + pos.to_i16().expect("Too many return positions."),
                rhs_var_a,
                None,
                is_rc,
                None,
                &format!("h_{ret_name}"),
                None,
                pc,
                op_size,
                indent,
            );

            casm_pos += 1;
            pc += op_size;
        }

        // Generate the final jump to the return (if any).
        let last_instr = &lean_info.casm_instructions[casm_pos].body;
        if let InstructionBody::Jump(instr) = last_instr {
            let op_size = last_instr.op_size();
            self.push_main(indent, &format!("vm_step_jump_imm {codes}", codes = self.make_codes(pc, op_size)));
        }
    }

    /// Generates the Lean code which handles the return from the function. This includes
    /// proving the correct rc pointer is returned.
    fn generate_return_proof(&mut self, indent: usize, lean_info: &LeanFuncInfo, block: &FuncBlock, pc: usize) {
        // Use 0 indentation (indentation is added later).
        let indent = 0;

        self.push_main(indent, "apply ret_returns");
        self.push_main(indent, &format!("apply {codes}", codes = self.make_codes(pc, 1)));
        self.push_main(indent, "constructor");
        self.push_main(indent,"· vm_arith_simps");

        if let Some(rc_name) = lean_info.get_range_check_arg_name() {
            let num_ret = lean_info.get_ret_arg_offset(&rc_name).expect("Failed to find rc return arg offset.");
            // The AP offset of the rc return variable.
            let ap_offset = block.start_local_ap + self.ap_assignments.len() - num_ret;
            self.push_main(indent, "simp only [Int.ofNat_eq_coe, CharP.cast_eq_zero]");
            self.push_main(indent, "simp only [add_sub_assoc] ; norm_num1");
            if ap_offset == 0 { // Unlikely here.
                self.push_main(indent, "try simp only [add_zero]");
            }
            self.push_main(indent, &format!("simp only [h_ap_plus_{ap_offset}]"));
            // self.push_main(indent, "try simp only [add_sub_add_comm, add_sub_right_comm, sub_add_cancel', sub_self] ; try norm_num1");
            self.push_main(indent, "try ring_nf");
        }
    }

    /// Called when the end of a branch is reached, either as a result of a call to a block or
    /// a return block. This function then adds the Lean code that generates the assignments
    /// for the variables and range checks accumulated up to this point.
    fn add_assignment_construction(
        &mut self,
        indent: usize,
        lean_info: &LeanFuncInfo,
        block: &FuncBlock,
        called_block: Option<&FuncBlock>
    ) {
        self.push_lean(indent, "let exec_vals :=");
        {
            let indent = indent + 4;
            self.push_lean(indent, "fun (i : ℤ) =>");
            let indent = indent + 2;
            self.push_lean(indent, &format!("match (i - {}) with", self.make_start_local_ap_expr(block)));

            let lines: Vec<String> = self.ap_assignments.iter().enumerate().map(
                |(ap_offset, var_expr)| {
                    format!("| {ap_offset} => {var_expr}")
                }
            ).collect();
            for line in lines {
                self.push_lean(indent, &line);
            }

            self.push_lean(indent, "| _ => val 0");
        }

        self.push_lean(indent, "");

        // Same construction for range checks
        self.push_lean(indent, "let rc_vals :=");
        {
            let indent = indent + 4;
            self.push_lean(indent, "fun (i : ℤ) =>");
            let indent = indent + 2;
            self.push_lean(indent, &format!("match (i - {}) with", self.make_start_rc_expr(lean_info, block, false)));

            let lines: Vec<String> = self.rc_vals.iter().enumerate().map(
                |(rc_offset, var_name)| format!("| {rc_offset} => (↑{var_name} : ℤ)")
            ).collect();
            for line in lines {
                self.push_lean(indent, &line);
            }

            self.push_lean(indent, "| _ => (0 : ℤ)");
        }

        self.push_lean(indent, "");

        // The name of the local assignment. If there is no called block, this is the
        // full assignment. If there is a called block, this is only part of the assignment
        // and will be concatenated with the called block's assignment to create the full
        // assignment.
        let loc_name = if called_block.is_none() { "loc" } else { "loc₀" };
        self.push_lean(
            indent,
            &format!(
                "let {loc_name} := (⟨{ap_len}, exec_vals, {rc_len}, rc_vals⟩ : LocalAssignment {ap_start} {rc_start})",
                ap_len = self.ap_assignments.len(),
                rc_len = self.rc_vals.len(),
                ap_start = self.make_start_local_ap_expr(block),
                // The (range_check + 0) expression must have the '+ 0' appear explicitly in the type
                // for blocks which are not the main block.
                rc_start = self.make_start_rc_expr(lean_info, block, block.pc_start_pos != 0),
            )
        );
        self.push_lean(indent, "");
    }

    /// Proves the assumptions of the block about the positions of arguments in memory.
    fn generate_block_arg_hyp(
        &mut self,
        lean_info: &LeanFuncInfo,
        block: &FuncBlock,
        calling_block: &FuncBlock,
        indent: usize,
    ) {

        for (arg_name, expr) in lean_info.get_block_args_and_exprs(block) {
            let var_type = lean_info.get_vm_var_type(&arg_name);
            // Need to distinguish between arguments in the local assignment and outside it
            let (_, offset) = get_ref_from_deref(&expr).expect("Failed to find argument offset.");
            if 0 <= offset && calling_block.start_local_ap <= offset as usize {
                // Auxiliary construction.
                self.push_lean(
                    indent,
                    &format!(
                        "have h_{arg_name}_exec_pos := assign_exec_pos mem loc₀ {var_ref}",
                        // Here we use the non-vm expression (as the theorem takes an offset in Z).
                        var_ref = get_lean_ref_from_deref(&expr, true, false, true),
                    )
                );
                self.push_lean(
                    indent + 2,
                    &format!("(by use (Int.le_add_of_nonneg_right (by norm_num1)) ; apply Int.add_lt_add_left ; norm_num1)")
                )
            }
            self.push_lean(
                indent,
                &format!(
                    "have h_{arg_name}_ap : {var_type} ↑{arg_name} = Assign mem loc₀ {var_ref} := by",
                    var_ref = get_lean_ref_from_deref(&expr, true, true, true),
                )
            );

            if offset < 0 { // outside the local assignment.
                if calling_block.start_local_ap == 0 {
                    self.push_lean(
                        indent + 2,
                        &format!(
                            "simp only [assign_exec_of_lt mem loc₀ {var_ref} (by apply Int.sub_lt_self ; norm_num1), htv_{arg_name}]",
                            // Here we use the non-vm expression (as the theorem takes an offset in Z).
                            var_ref = get_lean_ref_from_deref(&expr, true, false, true),
                        ),
                    );
                } else {
                    self.push_lean(
                        indent + 2,
                        &format!(
                            "have := assign_exec_of_lt mem loc₀ {var_ref}",
                            // Here we use the non-vm expression (as the theorem takes an offset in Z).
                            var_ref = get_lean_ref_from_deref(&expr, true, false, true),
                        ),
                    );
                    self.push_lean(
                        indent + 4,
                        "(by apply lt_trans _ (Int.lt_add_of_pos_right σ.ap (by norm_num1)) ; apply Int.sub_lt_self ; norm_num1)",
                    );
                    self.push_lean(indent + 2, &format!("simp only [this, htv_{arg_name}]"));
                }
            } else if (offset as usize) < calling_block.start_local_ap {
                self.push_lean(
                    indent + 2,
                    &format!(
                        "simp only [assign_exec_of_lt mem loc₀ {var_ref} (by apply Int.add_lt_add_left ; norm_num1), htv_{arg_name}]",
                        // Here we use the non-vm expression (as the theorem takes an offset in Z).
                        var_ref = get_lean_ref_from_deref(&expr, true, false, true),
                    ),
                );
            } else {
                // Inside the local assignment.
                self.push_lean(indent + 2, &format!("simp only [h_{arg_name}_exec_pos] ; ring_nf"))
            }
        }
    }

    /// Add the hypotheses that the values at offsets before the start ap of the block are taken from the appropriate
    /// positions in the memory before the local assignment.

    fn generate_aux_arg_offset_hyp(
        &mut self,
        indent: usize,
        lean_info: &LeanFuncInfo,
        block: &FuncBlock,
    ) {

        for (arg_name, expr) in lean_info.get_block_args_and_exprs(block) {
            let var_type = lean_info.get_vm_var_type(&arg_name);
            // Need to distinguish between arguments in the local assignment and outside it.
            if let Some((_, offset)) = get_ref_from_deref(&expr) {
                if offset < 0 {
                    self.push_lean(
                        indent,
                        &format!(
                            "have h_ap_minus_{min_offset} := assign_exec_of_lt mem loc (σ.ap - {min_offset})",
                            min_offset = -offset
                        ),
                    );
                    self.push_lean(
                        indent + 2,
                        if 0 < block.start_local_ap {
                            "(by apply lt_trans _ (Int.lt_add_of_pos_right _ (by norm_num1)) ; apply Int.sub_lt_self ; norm_num1)"
                        } else {
                            "(by apply Int.sub_lt_self ; norm_num1)"
                        }
                    )
                } else {
                    self.push_lean(
                        indent,
                        &format!("have h_ap_plus_{offset} := assign_exec_of_lt mem loc (σ.ap + {offset})"),
                    );
                    self.push_lean(indent + 2, "(by apply Int.add_lt_add_left ; norm_num1)");
                }
            } else {
                self.push_lean(
                    indent,
                    &format!("-- Did not generate hypothesis for {arg_name} because could not find its offset."),
                )
            }
        }
    }

    /// Add the hypotheses that the values at offsets within the range of the local assignment are taken
    /// from the local assignment.
    fn generate_aux_ap_offset_hyp(
        &mut self,
        indent: usize,
        lean_info: &LeanFuncInfo,
        block: &FuncBlock,
        // Is the assignment of this block a concatenation of this block together with a called block?
        is_concat: bool,
    ) {
        // Create the auxiliary ap offset claims

        for offset in block.start_local_ap..block.start_local_ap + self.ap_assignments.len() {
            if is_concat {
                if offset == 0 {
                    self.push_lean(
                        indent,
                        &format!("have h_ap_plus_{offset} := assign_exec_concat_loc₀ mem loc₀ loc₁ h_ap_concat h_rc_concat σ.ap")
                    );
                } else {
                    self.push_lean(
                        indent,
                        &format!("have h_ap_plus_{offset} := assign_exec_concat_loc₀ mem loc₀ loc₁ h_ap_concat h_rc_concat (σ.ap + {offset})")
                    );
                }
            } else {
                if offset == 0 {
                    self.push_lean(indent, &format!("have h_ap_plus_{offset} := assign_exec_pos mem loc σ.ap"));
                } else {
                    self.push_lean(indent, &format!("have h_ap_plus_{offset} := assign_exec_pos mem loc (σ.ap + {offset})"));
                }
            }

            self.push_lean(
                indent + 2,
                if block.start_local_ap == 0 && offset == 0 {
                    "(by use le_refl _ ; apply Int.lt_add_of_pos_right ; norm_num1)"
                } else if block.start_local_ap == 0 {
                    "(by use (Int.le_add_of_nonneg_right (by norm_num1)) ; apply Int.add_lt_add_left ; norm_num1)"
                } else {
                    "(by constructor ; apply Int.add_le_add_left ; norm_num1 ; rw [add_assoc] ; apply Int.add_lt_add_left ; norm_num1)"
                }
            );
        }
    }

    /// Generates the hypotheses that the values at offsets within the range check segment are
    /// from the local assignment.
    fn generate_aux_rc_offset_hyp(
        &mut self,
        indent: usize,
        lean_info: &LeanFuncInfo,
        block: &FuncBlock,
        // Is the assignment of this block a concatenation of this block together with a called block?
        is_concat: bool,
    ) {
        let rc_ptr = lean_info.get_range_check_arg_name();

        if let Some(rc_ptr) = rc_ptr {
            for offset in block.start_rc..block.start_rc + self.rc_vals.len() {

                if is_concat {
                    self.push_lean(
                        indent,
                        &format!("have h_rc_plus_{offset} := assign_rc_concat_loc₀ mem loc₀ loc₁ h_ap_concat h_rc_concat ({rc_ptr} + {offset})")
                    );
                } else {
                    self.push_lean(
                        indent,
                        &format!("have h_rc_plus_{offset} := assign_rc_pos mem loc ({rc_ptr} + {offset})")
                    );
                }

                self.push_lean(
                    indent + 2,
                    if block.pc_start_pos == 0 {
                        "(by use (Int.le_add_of_nonneg_right (by norm_num1)) ; apply Int.add_lt_add_left ; norm_num1)"
                    } else {
                        "(by constructor ; apply Int.add_le_add_left ; norm_num1 ; rw [add_assoc] ; apply Int.add_lt_add_left ; norm_num1)"
                    }
                );
            }
        }
    }

    /// Generates the rcases for the spec up the point in the function reached so far.
    /// Returns the indentation for the following proof lines.
    fn generate_rcases(&mut self, indent: usize) -> usize {
        let prefix = if self.is_eq_branch { "· " } else { "" };
        if 0 < self.spec_rcases.len() {
            self.push_lean(
                indent,
                &format!("{prefix}rcases h_spec with ⟨{specs}⟩",
                specs = self.spec_rcases.join(", "),
                )
            );
        } else if self.is_eq_branch {
            self.push_lean(indent, prefix);
        }
        // Clear the list, as these should not be used again.
        self.spec_rcases.clear();

        if self.is_eq_branch { indent + 2 } else { indent }
    }

    fn generate_proof_intro(&mut self, indent: usize) {
        self.push_lean(indent, "use loc");
        self.push_lean(indent, "constructor");
    }

    /// Prove the VmRangeChecked claim
    fn generate_range_checked_proof(
        &mut self,
        indent: usize,
        lean_info: &LeanFuncInfo,
        block: &FuncBlock,
        called_block_rc_hyp: Option<&str>
    ) {
        let indent = if called_block_rc_hyp.is_some() {
                self.push_lean(indent, "· apply VmRangeChecked_concat");
                indent + 2
            } else {
                indent
            };

        if self.rc_vals.len() == 0 {
            self.push_lean(indent, "· apply VmRangeChecked_zero");
        } else {
            let mut rc_proof_indent = indent;
            let rc_var_names: Vec<String> = self.rc_vals.iter().rev().map(|name| name.clone()).collect();
            for var_name in rc_var_names {
                if rc_proof_indent == indent {
                    self.push_lean(rc_proof_indent, "· apply VmRangeChecked_rec");
                    rc_proof_indent = indent + 2;
                } else {
                    self.push_lean(rc_proof_indent, "apply VmRangeChecked_rec");
                }
                self.push_lean(rc_proof_indent, "· try norm_num1 ; try ring_nf");
                self.push_lean(rc_proof_indent + 2, &format!("apply h_rc_{var_name}"));
            }
            self.push_lean(rc_proof_indent, "apply VmRangeChecked_zero");
        }

        if let Some(called_block_rc_hyp) = called_block_rc_hyp {
            self.push_lean(indent, &format!("apply {called_block_rc_hyp}"));
        }
    }

    /// Append the main proof generated while going through the function to the
    /// Lean proof generated.
    fn append_main_proof(&mut self, indent: usize) {
        let mut lines: Vec<String> = self.main_proof.iter().map(
            |line| " ".repeat(indent) + line
        ).collect();
        self.lean_code.append(&mut lines);
    }
}

impl LeanGenerator for CompletenessProof {
    /// Return a generator for a branch.
    fn branch(&self) -> Box<dyn LeanGenerator> {
        let mut branch = Box::new(CompletenessProof::new());
        branch.is_eq_branch = true;
        // For information passed down the branch, need to clone that information.
        branch.ap_assignments = self.ap_assignments.clone();
        branch.rc_vals = self.rc_vals.clone();
        branch.main_proof = self.main_proof.clone();
        branch
    }

    /// Integrates the Lean code generated by a branch generator into the
    /// main Lean code generator.
    fn append_branch(&mut self, branch: &Box<dyn LeanGenerator>) {
        for line in branch.get_lean() {
            self.push_lean(0, line)
        }
    }

    /// Returns the Lean code generated by this generator.
    fn get_lean(&self) -> &Vec<String> {
        &self.lean_code
    }

    fn generate_statement(
        &mut self,
        main_func_name: &str,
        func_name: &str,
        lean_info: &LeanFuncInfo,
        block: &FuncBlock,
        indent: usize,
    ) {
        self.push_statement(
            indent,
            &format!(
                "theorem {theorem_name}",
                theorem_name = self.make_block_theorem_name(lean_info, block)
            )
        );

        let indent = indent + 2;

        // Add arguments and hypotheses.
        self.generate_args_and_hypotheses(lean_info, block, indent + 2);
        // Add the conclusion.
        self.generate_conclusion(lean_info, block, indent);
    }

    fn generate_intro(
        &mut self,
        main_func_name: &str,
        func_name: &str,
        lean_info: &LeanFuncInfo,
        block: &FuncBlock,
        indent: usize,
    ) {
        // Add the return variables to the variables in the rcases statement
        // which unpacks the spec.
        self.spec_rcases.append(lean_info.get_explicit_ret_arg_names().as_mut());
    }

    fn generate_temp_var(
        &mut self,
        var: &VarDesc,
        lean_info: &LeanFuncInfo,
        block: &FuncBlock,
        rebind: &mut VarRebind,
        pc: usize,
        op_size: usize,
        indent: usize,
    ) {
        self.generate_var(var, false, lean_info, block, rebind, pc, op_size, indent);
    }

    fn generate_implicit_temp_var(
        &mut self,
        var_desc: &VarBaseDesc,
        lean_info: &LeanFuncInfo,
        block: &FuncBlock,
        rebind: &mut VarRebind,
        pc: usize,
        op_size: usize,
        indent: usize,
    ) {
        self.generate_var(
            &VarDesc {
                name: var_desc.name.clone(),
                var_id: var_desc.var_id,
                var_expr: var_desc.var_expr.clone(),
                expr: None,
                ap_change: 0, // not used here.
            },
            false,
            lean_info,
            block,
            rebind,
            pc,
            op_size,
            indent
        );
    }

    fn generate_local_var(
        &mut self,
        var: &VarDesc,
        lean_info: &LeanFuncInfo,
        block: &FuncBlock,
        rebind: &mut VarRebind,
        pc: usize,
        op_size: usize,
        indent: usize,
    ) {
        self.generate_var(var, true, lean_info, block, rebind, pc, op_size, indent);
    }

    fn generate_let(
        &mut self,
        assign: &AssertDesc,
        lean_info: &LeanFuncInfo,
        rebind: &mut VarRebind,
        indent: usize,
    ) {
        // A let does not assign a variable to the local assignment, but does add
        // to the spec.
        let (var_name, _) = rebind_assignment(assign, rebind);

        self.spec_rcases.push(var_name.clone());
        self.spec_rcases.push(format!("h_{var_name}"));
    }

    fn generate_assert(
        &mut self,
        assign: &AssertDesc,
        lean_info: &LeanFuncInfo,
        block: &FuncBlock,
        rebind: &mut VarRebind,
        pc: usize,
        op_size: usize,
        indent: usize,
    ) {
        let lhs =  rebind.get_var_name(&assign.lhs.name);

        let (rc_offset, assert_hyp) = if lean_info.is_range_check(&assign.expr) {
            self.rc_vals.push(lhs.clone());
            (lean_info.get_offset_at(&assign.expr), format!("h_rc_{}", &lhs))
        } else {
            (None, format!("h_α{pc}"))
        };

        self.spec_rcases.push(assert_hyp.clone());

        let (rhs_var_a, rhs_var_b, simps) =
            CompletenessProof::get_rhs_vars_and_simps(&assign.expr, lean_info, &assert_hyp);
        self.generate_assert_main(
            lean_info,
            block,
            &assign.lhs.name,
            get_ref_from_deref(&assign.lhs.var_expr).expect("Failed to find variable AP offset").1,
            rhs_var_a,
            rhs_var_b,
            String::from("+-*/").find(&assign.expr.op).is_some(),
            rc_offset,
            &assert_hyp,
            Some(&simps),
            pc,
            op_size,
            indent,
        );
    }

    fn generate_jmp(
        &mut self,
        pc: usize,
        op_size: usize,
        indent: usize,
    ) {
        // Use 0 indentation (indentation is added later).
        let indent = 0;

        self.push_main(
            indent,
            &format!("vm_step_jump_imm {codes}", codes = self.make_codes(pc, op_size)),
        );
    }

    fn generate_jnz(
        &mut self,
        cond_var: &str,
        label: &str,
        rebind: &VarRebind,
        pc: usize,
        op_size: usize,
        indent: usize,
    ) {
        self.spec_rcases.push("h_spec|h_spec".into());

        // Add the rcases for the spec up to the jnz.
        self.generate_rcases(indent);

        // Use 0 indentation (indentation is added later).
        let indent = 0;

        self.push_main(
            indent,
            &format!("vm_step_jnz {codes} <;>", codes = self.make_codes(pc, op_size))
        );
        self.push_main(indent, "simp only [neg_clip_checked', ←Int.sub_eq_add_neg] <;>");
        self.push_main(indent, "norm_num1 <;>");

        // Get the ap offset of the variable.
        let var_ap_offset = self.get_var_ap_offset(cond_var, rebind)
            .expect("Failed to find variable ap offset");

        if var_ap_offset == 0 {
            self.push_main(indent, "try simp only [add_zero] <;>");
        }
        self.push_main(indent, &format!("simp only [h_ap_plus_{var_ap_offset}] <;>"));
        if var_ap_offset == 0 {
            self.push_main(indent, "simp only [Int.sub_self]");
        } else {
            self.push_main(indent, &format!("simp only [Int.add_comm σ.ap {var_ap_offset}, Int.add_sub_cancel]"));
        }
    }

    fn generate_label_block(
        &mut self,
        func_name: &str,
        lean_info: &LeanFuncInfo,
        block: &FuncBlock,
        calling_block: &FuncBlock,
        rebind: &VarRebind,
        indent: usize,
    ) {
        let block_suffix = block.block_suffix();

        let block_spec = format!("h_block{block_suffix}");
        self.spec_rcases.push(block_spec.clone());

        // Now that we reached the end of the branch, we can generate the proof.

        // First, unpack the specs. Whether this is the first or second branch of a jnz
        // determines the indentation.
        let indent = self.generate_rcases(indent);

        // Construct the assignment
        self.add_assignment_construction(indent, lean_info, calling_block, Some(block));

        // Prove the assumptions of the block about the positions of arguments in memory.
        self.generate_block_arg_hyp(lean_info, block, calling_block, indent);

        // Add the block hypothesis.

        let block_hyp = format!("h{block_suffix}");
        let block_rc_hyp = format!("h_rc{block_suffix}");

        self.push_lean(
            indent,
            &format!(
                "have {block_hyp} := {theorem_name}",
                theorem_name = self.make_block_theorem_name(lean_info, block),
            ),
        );
        let block_args_str = if block.has_args() { String::from(" ") + &block.args.join(" ") } else { "".into() };
        let arg_hyp_str = if block.has_args() {
                String::from(" ") + &block.args.iter().map(|arg| format!("h_{arg}_ap")).join(" ")
            } else {
                "".into()
            };
        let mut h_spec_parts = lean_info.get_explicit_ret_arg_names();
        h_spec_parts.push(block_spec.clone());
        self.push_lean(
            indent + 2,
            &format!(
                "(Assign mem loc₀) σ{block_args_str} hmem hin_fp{arg_hyp_str} ⟨{h_spec}⟩",
                h_spec = h_spec_parts.join(", "),
            ),
        );

        self.push_lean(indent,"");
        self.push_lean(
            indent,
            &format!(
                "have h_ap_concat : {ap1} = {ap0} + ↑loc₀.exec_num := by simp",
                ap1 = self.make_start_local_ap_expr(block),
                ap0 = self.make_start_local_ap_expr(calling_block),
            ));
        self.push_lean(
            indent,
            &format!(
                "have h_rc_concat : {rc1} = {rc0} + ↑loc₀.rc_num := by simp",
                rc1 = self.make_start_rc_expr(lean_info, block, true),
                rc0 = self.make_start_rc_expr(lean_info, calling_block, false),
            ));
        self.push_lean(
            indent,
            &format!("rcases {block_hyp} with ⟨loc₁, {block_rc_hyp}, {block_hyp}⟩"),
        );
        self.push_lean(indent,"");
        self.push_lean(indent,"let loc := ConcatAssignments loc₀ loc₁ h_ap_concat h_rc_concat");
        self.push_lean(indent,"");

        // Create the auxiliary ap offset and rc offset claims
        self.generate_aux_arg_offset_hyp(indent, lean_info, calling_block);
        self.generate_aux_ap_offset_hyp(indent, lean_info, calling_block, true);
        self.generate_aux_rc_offset_hyp(indent, lean_info, calling_block, true);
        self.push_lean(indent, "");

        // Add the proof of the block to the main proof.
        // (will be added at the end below).
        self.push_main(0, "rw [assign_concat, concat_exec_num, concat_rc_num]");
        self.push_main(0, "simp only [Nat.cast_add]");
        self.push_main(0, "try simp only [Nat.cast_zero, Int.zero_add]");
        self.push_main(0, "simp only [←add_assoc]");
        self.push_main(0, &format!("apply {block_hyp}"));

        // Begin actual proof
        self.generate_proof_intro(indent);

        // Prove the VmRangeChecked claim
        self.generate_range_checked_proof(indent, lean_info, block, Some(&block_rc_hyp));

        self.append_main_proof(indent);
    }

    fn generate_branch_intro(
        &mut self,
        cond_var: &(String, Var),
        is_eq: bool,
        lean_info: &LeanFuncInfo,
        rebind: &mut VarRebind,
        pc: usize,
        indent: usize,
    ) -> usize {

        let orig_indent = indent;
        // Use 0 indentation (indentation is added later).
        let indent = 0;

        let cond_var_name = &cond_var.0;

        // Add the condition hypothesis to the rcases.
        self.spec_rcases.push(format!("h_{cond_var_name}"));

        self.push_main(
            indent,
            &format!("-- {cond_var_name} {op} 0", op = if is_eq { "=" } else { "≠" }),
        );

        if is_eq {
            self.push_main(indent, "swap");
            self.push_main(indent, &format!("· simp only [h_{cond_var_name}]"));
            self.push_main(indent + 2, "simp only [IsEmpty.forall_iff]");
            self.push_main(indent, "intro _");
        } else {
            self.push_main(indent, "· intro h_cond");
            self.push_main(indent + 2, "exfalso");
            self.push_main(indent + 2, &format!("apply h_{cond_var_name}"));
            self.push_main(indent + 2, "injection h_cond");
            self.push_main(indent, "intro _");
            self.push_main(
                indent,
                &format!("simp only [assign_prog, hmem{imm_pc} hmem, Mrel.toInt]", imm_pc = pc + 1)
            );
            self.push_main(indent, "vm_arith_simps");
        }

        orig_indent
    }

    /// Returns the indentation after the branch close.
    fn generate_branch_close(
        &mut self,
        is_eq: bool,
        lean_info: &LeanFuncInfo,
        rebind: &mut VarRebind,
        indent: usize,
    ) -> usize {
        // Do nothing (and indentation does not change).
        indent
    }

    fn generate_return_args(
        &mut self,
        ret_block_name: &str,
        branch_id: usize,
        ret_arg_names: &Vec<String>,
        ret_exprs: &Vec<String>,
        lean_info: &LeanFuncInfo,
        // The function block in which the return block was called.
        block: &FuncBlock,
        rebind: &VarRebind,
        indent: usize,
    ) {
        // Add the return block variables (and the associated hypotheses) to the list
        // of locally assigned variables and the list of spec rcases.
        self.add_return_block_vars_and_hyp(ret_block_name, branch_id, lean_info);

        // Add the steps of the return block to the main proof.
        self.generate_return_block_steps(
            ret_block_name,
            branch_id,
            ret_arg_names,
            ret_exprs,
            lean_info,
            block,
            rebind,
            indent,
        );

        // Append the final part of the proof (which includes the return step).
        self.generate_return_proof(indent, lean_info, block, lean_info.get_pc_at(lean_info.casm_end - 1));

        // First, unpack the specs. Whether this is the first or second branch of a jnz
        // determines the indentation.
        let indent = self.generate_rcases(indent);

        self.add_assignment_construction(indent, lean_info, block, None);
        // Create the auxiliary ap offset and rc offset claims
        self.generate_aux_arg_offset_hyp(indent, lean_info, block);
        self.generate_aux_ap_offset_hyp(indent, lean_info, block, false);
        self.generate_aux_rc_offset_hyp(indent, lean_info, block, false);
        self.push_lean(indent, "");

        // Begin of actual proof.
        self.generate_proof_intro(indent);

        // Prove the VmRangeChecked claim
        self.generate_range_checked_proof(indent, lean_info, block, None);

        // Add the steps through the statements of this particular branch (which were already
        // generated). This is the place where the indentation is added (it is the same for
        // all statements).
        self.append_main_proof(indent);
    }

    fn generate_advance_ap(&mut self, step: &usize, pc: usize, op_size: usize, indent: usize) {
        // Use 0 indentation (indentation is added later).
        let indent = 0;

        // As the AP is advanced without allocating a variable, these locations remain empty.
        for count in 0..*step {
            self.ap_assignments.push("val 0".into());
        }

        self.push_main(indent, &format!("vm_step_advance_ap {codes}", codes = self.make_codes(pc, op_size)));
    }

    fn generate_fail(&mut self, pc: usize, op_size: usize, indent: usize) {
        // Use 0 indentation (indentation is added later).
        let indent = 0;

        // TODO: what should we do here?
    }

}

fn generate_soundness_user_spec(lean_info: &LeanFuncInfo) -> Vec<String> {
    let func_name = &lean_info.func_name[..];
    let args_str =
        lean_info.get_arg_names().iter().chain(lean_info.get_explicit_ret_arg_names().iter()).join(" ");

    let mut user_spec: Vec<String> = Vec::new();
    user_spec.push(format!("def spec_{func_name} (κ : ℕ) ({args_str} : F) : Prop :="));
    user_spec.push(String::from("  true"));

    user_spec
}

fn generate_soundness_auto_spec(lean_info: &LeanFuncInfo) -> Vec<String> {
    let mut auto_specs = AutoSpecs::new(false);
    for block in lean_info.blocks.iter().rev() {
        generate_auto(&mut auto_specs, lean_info, &block);
        auto_specs.push(0, "");
    }

    auto_specs.specs
}

fn generate_user_soundness_theorem(lean_info: &LeanFuncInfo) -> Vec<String> {
    let func_name = &lean_info.func_name[..];
    let args_str =
        lean_info.get_arg_names().iter().chain(lean_info.get_explicit_ret_arg_names().iter()).join(" ");

    let mut user_theorem: Vec<String> = Vec::new();
    user_theorem.push(format!("theorem sound_{func_name}"));
    user_theorem.push(String::from("    (κ : ℕ)"));
    if 0 < args_str.len() {
        user_theorem.push(format!("    ({args_str} : F)"));
    }
    let sep = if 0 < args_str.len() { " " } else { "" };
    user_theorem.push(format!("    (h_auto : auto_spec_{func_name} κ{sep}{args_str}) :"));
    user_theorem.push(format!("  spec_{func_name} κ{sep}{args_str} :="));

    if lean_info.is_lean3 {
        user_theorem.push("begin".into());
        user_theorem.push("  trivial".into());
        user_theorem.push("end".into());
    } else {
        user_theorem.push("by sorry".into());
    }

    user_theorem
}

fn generate_soundness_auto_theorem(lean_info: &LeanFuncInfo) -> Vec<String> {

    let mut auto_theorems: Vec<String> = Vec::new();

    for block in lean_info.blocks.iter().rev() {
        let mut auto_theorem = AutoProof::new();
        auto_theorem.is_lean3 = lean_info.is_lean3;
        generate_auto(&mut auto_theorem, lean_info, &block);
        auto_theorems.append(auto_theorem.get_full_proof().as_mut());
        auto_theorems.push("".into());
    }

    auto_theorems
}

fn generate_completeness_auto_spec(lean_info: &LeanFuncInfo) -> Vec<String> {
    let mut auto_specs = AutoSpecs::new(true);
    for block in lean_info.blocks.iter().rev() {
        generate_auto(&mut auto_specs, lean_info, &block);
        auto_specs.push(0, "");
    }

    auto_specs.specs
}

/// Generates the spec hypothesis for the given block. This may span more than one line. The
/// prefix is prefixed to the first line and the suffix is appended at the end of
/// the last line.
fn generate_completeness_block_spec_hyp(
    lean_info: &LeanFuncInfo,
    block: &FuncBlock,
    prefix: &str,
    suffix: &str,
) -> Vec<String> {

    let mut hyp: Vec<String> = Vec::new();

    let func_name = &lean_info.func_name[..];
    let block_suffix = block.block_suffix();
    let args_str = block.args.join(" ");
    let ret_args_str = lean_info.get_explicit_ret_arg_names().join(" ");

    let sep = if 0 < args_str.len() { " " } else { "" };

    if 0 < ret_args_str.len() {
        hyp.push(format!("{prefix}∃ ({ret_args_str} : ℤ),"));
        hyp.push(
            " ".repeat(prefix.len() + 2)
            + &format!("auto_spec_{func_name}{block_suffix}{sep}{args_str} {ret_args_str}{suffix}")
        );
    } else {
        hyp.push(format!("{prefix}auto_spec_{func_name}{sep}{args_str}{suffix}"));
    }

    hyp
}

fn generate_user_completeness_theorem(lean_info: &LeanFuncInfo) -> Vec<String> {
    let func_name = &lean_info.func_name[..];
    let args_str = lean_info.get_arg_names().join(" ");
    let ret_args_str = lean_info.get_explicit_ret_arg_names().join(" ");

    let mut user_theorem: Vec<String> = Vec::new();
    user_theorem.push(format!("theorem spec_satisfiable_{func_name}"));
    if 0 < args_str.len() {
        user_theorem.push(format!("    ({args_str} : ℤ)"));
        user_theorem.push(format!("    -- Add assumptions on the arguments here."));
    }
    user_theorem.push(format!("    :"));

    user_theorem.append(
        generate_completeness_block_spec_hyp(
            lean_info, &lean_info.blocks[0], "  ", " := by",
        ).as_mut()
    );

    user_theorem.push("  sorry".into());

    user_theorem
}

fn generate_completeness_auto_theorem(lean_info: &LeanFuncInfo) -> Vec<String> {

    let mut auto_theorems: Vec<String> = Vec::new();

    for block in lean_info.blocks.iter().rev() {
        let mut auto_theorem = CompletenessProof::new();
        generate_auto(&mut auto_theorem, lean_info, &block);
        auto_theorems.append(auto_theorem.get_full_proof().as_mut());
        auto_theorems.push("".into());
    }

    auto_theorems
}

fn generate_auto(lean_gen: &mut dyn LeanGenerator, lean_info: &LeanFuncInfo, block: &FuncBlock) {
    let mut rebind = match &block.label {
        Some(label) => {
            if let Some(mut prev_rebind) = lean_info.get_rebinds_before_label(label, 0) {
                prev_rebind.reset_counts();
                prev_rebind
            } else {
                VarRebind::new()
            }
        },
        _ => {
            let mut rebind = VarRebind::new();
            for (arg_name, arg_expr) in lean_info.get_arg_names_and_cell_expr() {
                rebind.rebind(&arg_name, arg_expr);
            }
            rebind
        }
    };

    lean_gen.generate_statement(&lean_info.main_func_name, &lean_info.func_name, lean_info, block, 0);
    lean_gen.generate_intro(&lean_info.main_func_name, &lean_info.func_name, lean_info, block, 2);
    generate_auto_block(
        lean_gen,
        lean_info,
        block,
        &mut rebind,
        block.start_pos,
        block.casm_start_pos,
        block.pc_start_pos,
        2
    );
}

fn generate_auto_block(
    lean_gen: &mut dyn LeanGenerator,
    lean_info: &LeanFuncInfo,
    block: &FuncBlock,
    rebind: &mut VarRebind,
    block_start: usize,
    // Position in the list of casm instructions (this is not the same as the pc,
    // as all instructions, whether they are compiled into one or two field
    // elements, occupy a single position in this list).
    casm_pos: usize,
    // The pc, where 0 is the beginning of the function.
    pc: usize,
    indent: usize
) {
    let mut casm_pos = casm_pos;
    let mut pc = pc;
    for (i, statement) in lean_info.aux_info.statements[block_start..].iter().enumerate() {
        let op_size = lean_info.casm_instructions[casm_pos].body.op_size();
        match statement {
            StatementDesc::TempVar(var) => {
                let op_size = if var.expr.is_some() { op_size } else { 0 };
                lean_gen.generate_temp_var(var, lean_info, block, rebind, pc, op_size, indent);
                if 0 < op_size {
                    pc += op_size;
                    casm_pos += 1;
                }
            },
            StatementDesc::LocalVar(var) => {
                let op_size = if var.expr.is_some() { op_size } else { 0 };
                lean_gen.generate_local_var(var, lean_info, block, rebind, pc, op_size, indent);
                if 0 < op_size {
                    pc += op_size;
                    casm_pos += 1;
                }
            },
            StatementDesc::Let(assign) => {
                lean_gen.generate_let(assign, lean_info, rebind, indent);
            },
            StatementDesc::Assert(assert) => {
                // If the variables used here were not yet declared, add them.
                lean_gen.generate_assert_missing_vars(assert, lean_info, block, rebind, pc, op_size, indent);
                // Generate the assert.
                lean_gen.generate_assert(assert, lean_info, block, rebind, pc, op_size, indent);
                pc += op_size;
                casm_pos += 1;
            },
            StatementDesc::Jump(jump) => {
                match &jump.cond_var {
                    Some(cond_var) => {

                        lean_gen.generate_jnz(&cond_var.0, &jump.target, rebind, pc, op_size, indent);

                        // Conditional jump, must generate both branches.

                        // First (equals zero) branch

                        let mut branch = lean_gen.branch();

                        let indent = branch.generate_branch_intro(
                                cond_var, true, lean_info, rebind, pc, indent);

                        // Clone the rebind object before entering the branch, as the second branch
                        // should be independent of the rebinding accumulated in the first branch.
                        generate_auto_block(
                                &mut *branch,
                                lean_info,
                                block,
                                &mut rebind.clone(),
                                block_start + i + 1,
                                casm_pos + 1,
                                pc + op_size,
                                indent);

                        lean_gen.append_branch(&branch);

                        let indent = lean_gen.generate_branch_close(
                            true, lean_info, rebind, indent);

                        // Second (not equals zero) branch

                        let indent = lean_gen.generate_branch_intro(
                            cond_var, false, lean_info, rebind, pc, indent);

                        generate_jump_to_label(
                            lean_gen, lean_info, block, rebind, jump, block_start + i, casm_pos, pc, indent);

                        lean_gen.generate_branch_close(
                            false, lean_info, rebind, indent);
                    },
                    _ => {

                        lean_gen.generate_jmp(pc, op_size, indent);

                        generate_jump_to_label(
                            lean_gen, lean_info, block, rebind, jump, block_start + i, casm_pos, pc, indent);
                    }
                };
                return; // Remaining code handled already in the branches.
            },
            StatementDesc::Fail => {
                lean_gen.generate_fail(pc, op_size, indent);
                return; // Remaining code not reachable.
            },
            StatementDesc::ApPlus(step) => {
                lean_gen.generate_advance_ap(step, pc, op_size, indent);
                casm_pos += 1;
                pc += op_size;
            }, // Do nothing
            StatementDesc::Label(desc) => {
                // Fallthrough to next block.
                if let Some(target_block) = lean_info.get_block_by_label(&desc.label) {
                    lean_gen.generate_label_block(
                        &lean_info.func_name,
                        lean_info,
                        target_block,
                        block,
                        rebind,
                        indent,
                    );
                    return;
                }
            },
        }
    }

    generate_auto_ret_block(lean_gen, lean_info, block, rebind, "Fallthrough", indent);
}

fn generate_jump_to_label(
    lean_gen: &mut dyn LeanGenerator,
    lean_info: &LeanFuncInfo,
    block: &FuncBlock,
    rebind: &mut VarRebind,
    jump: &JumpDesc, // The jump instruction.
    instr_pos: usize, // position of the Jump instruction in the aux_info instruction list
    casm_pos: usize,
    pc: usize,
    indent: usize
) {
    let pos = lean_info.get_jump_target_pos(jump);

    // The jump target may be a return branch name.
    if pos.is_none() {
        generate_auto_ret_block(
            lean_gen, lean_info, block, rebind, &jump.target, indent);
        return;
    }

    let target = pos.unwrap();
    assert!(instr_pos < target, "No backward jumps supported.");

    let casm_jump = lean_info.casm_jump(instr_pos, target);
    let pc_jump = lean_info.pc_jump(casm_pos, casm_pos + casm_jump);

    if let Some(target_block) = lean_info.get_block_by_label(&jump.target) {
        lean_gen.generate_label_block(
            &lean_info.func_name,
            lean_info,
            target_block,
            block,
            rebind,
            indent,
        );
    } else {
        generate_auto_block(
            lean_gen,
            lean_info,
            block,
            rebind,
            target + 1,
            casm_pos + casm_jump,
            pc + pc_jump,
            indent);
    }
}

fn generate_auto_ret_block(
    lean_gen: &mut dyn LeanGenerator,
    lean_info: &LeanFuncInfo,
    block: &FuncBlock,
    rebind: &VarRebind,
    ret_block_name: &str,
    indent: usize
) {
    let branch_id = lean_info.aux_info.return_branches.iter().position(
        |b| b.name == ret_block_name
    ).expect("Could not find return block.");

    let ret_arg_names = lean_info.get_all_ret_arg_names();
    let flat_exprs = lean_info.aux_info.return_branches[branch_id].flat_exprs();

    lean_gen.generate_return_args(
        ret_block_name,
         branch_id,
         &ret_arg_names,
         &flat_exprs,
         lean_info,
         block,
         rebind,
         indent
    );
}

/*fn check_supported(test_name: &str, aux_info: &CasmBuilderAuxiliaryInfo, cairo_program: &CairoProgram) -> bool {

    if !test_name.ends_with("libfunc") {
        return false;
    }

    !cairo_program.instructions.iter().any(
        |instr| match instr.body {
            InstructionBody::Call(_) => true,
            _ => false
         }
    )
}*/

/// Find the start offset (in the casm) of the different functions.
fn find_func_offsets(cairo_program: &CairoProgram) -> Vec<(usize, usize)> {
    let mut offsets: Vec<(usize, usize)> = Vec::new();

    let mut start_pos: usize = 0;
    let mut pos: usize = 0;

    for instr in &cairo_program.instructions {
        pos += 1;
        if match instr.body { InstructionBody::Ret(_) => true, _ => false } {
            offsets.push((start_pos, pos));
            start_pos = pos;
        }
    }
    offsets
}

/// Returns a tuple whose first string is the spec file for the function and the second string the automatic
/// soundness proofs for the function.
///
pub fn generate_lean_soundness(test_name: &str, cairo_program: &CairoProgram, is_lean3: bool) -> (String, String) {

    let main_func_name: String = func_name_from_test_name(test_name);

    if cairo_program.aux_infos.len() == 0 {
        return ("-- No spec generated for {main_func_name}".into(), "-- No proof generated for {main_func_name}".into());
    }

    // Start and end offsets of each of the functions.
    let offsets = find_func_offsets(cairo_program);
    assert!(offsets.len() == cairo_program.aux_infos.len(), "Mismatch between number of functions and start offsets.");


    let mut soundness_spec: Vec<String> = Vec::new();
    let mut soundness: Vec<String> = Vec::new();

    // Generate the prelude
    if is_lean3 {
        soundness_spec.append(generate_soundness_spec_prelude(&main_func_name).as_mut());
        soundness.append(generate_soundness_prelude(&main_func_name).as_mut());
    } else {
        soundness_spec.append(generate_soundness_spec_prelude4(&main_func_name).as_mut());
        soundness.append(generate_soundness_prelude4(&main_func_name).as_mut());
    }

    for (aux_info, func_offsets) in cairo_program.aux_infos.iter().zip(offsets.iter()) {
        let casm_start = (*func_offsets).0;
        let casm_end = (*func_offsets).1;
        let func_name_suffix = if casm_start == 0 { "".to_string() } else { "_destr".to_string() + &casm_start.to_string() };
        let lean_info = LeanFuncInfo::new(
            main_func_name.clone(),
            format!("{main_func_name}{func_name_suffix}"),
            aux_info,
            casm_start,
            casm_end,
            cairo_program,
            is_lean3,
        );
        soundness_spec.append(generate_func_lean_soundness_spec(&lean_info).as_mut());
        soundness_spec.push("".into());

        soundness.append(generate_func_lean_soundness(&lean_info).as_mut());
        soundness.push("".into());
    }

    (soundness_spec.join("\n"), soundness.join("\n"))
}

fn generate_func_lean_soundness_spec(lean_info: &LeanFuncInfo) -> Vec<String> {

    let mut soundness_spec: Vec<String> = Vec::new();

    // Generate the constant definitions
    soundness_spec.append(generate_consts(&lean_info, false).as_mut());
    soundness_spec.push(String::from(""));

    // Generate the user spec
    soundness_spec.append(generate_soundness_user_spec(&lean_info).as_mut());
    soundness_spec.push(String::from(""));
    // Generate the auto spec
    soundness_spec.append(generate_soundness_auto_spec(&lean_info).as_mut());

    // Generate the (placeholder) user soundness proof.
    soundness_spec.push(String::from(""));
    soundness_spec.append(generate_user_soundness_theorem(&lean_info).as_mut());

    soundness_spec
}

fn generate_func_lean_soundness(lean_info: &LeanFuncInfo) -> Vec<String> {

    let mut soundness: Vec<String> = Vec::new();

    // Gneerate the soundness proof
    soundness.push(String::from(""));
    soundness.append(generate_soundness_auto_theorem(&lean_info).as_mut());

    soundness
}

//
// Completeness
//

/// Returns a tuple whose first string is the completeness spec file for the function and the second string the automatic
/// completeness proofs for the function.
///
pub fn generate_lean_completeness(test_name: &str, cairo_program: &CairoProgram) -> (String, String) {

    let main_func_name: String = func_name_from_test_name(test_name);

    if cairo_program.aux_infos.len() == 0 {
        return ("-- No spec generated for {main_func_name}".into(), "-- No proof generated for {main_func_name}".into());
    }

    // Start and end offsets of each of the functions.
    let offsets = find_func_offsets(cairo_program);
    assert!(offsets.len() == cairo_program.aux_infos.len(), "Mismatch between number of functions and start offsets.");


    let mut completeness_spec: Vec<String> = Vec::new();
    let mut completeness: Vec<String> = Vec::new();

    // Generate the prelude
    completeness_spec.append(generate_completeness_spec_prelude(&main_func_name).as_mut());
    completeness.append(generate_completeness_prelude(&main_func_name).as_mut());

    for (aux_info, func_offsets) in cairo_program.aux_infos.iter().zip(offsets.iter()) {
        let casm_start = (*func_offsets).0;
        let casm_end = (*func_offsets).1;
        let func_name_suffix = if casm_start == 0 { "".to_string() } else { "_destr".to_string() + &casm_start.to_string() };
        let lean_info = LeanFuncInfo::new(
            main_func_name.clone(),
            format!("{main_func_name}{func_name_suffix}"),
            aux_info,
            casm_start,
            casm_end,
            cairo_program,
            false,
        );
        completeness_spec.append(generate_func_lean_completeness_spec(&lean_info).as_mut());
        completeness_spec.push("".into());

        completeness.append(generate_func_lean_completeness(&lean_info).as_mut());
        completeness.push("".into());
    }

    (completeness_spec.join("\n"), completeness.join("\n"))
}

fn generate_func_lean_completeness_spec(lean_info: &LeanFuncInfo) -> Vec<String> {

    let mut completeness_spec: Vec<String> = Vec::new();

    // Generate the constant definitions
    completeness_spec.append(generate_consts(&lean_info, true).as_mut());
    completeness_spec.push(String::from(""));

    // Generate the user spec
    completeness_spec.push(String::from(""));
    // Generate the completeness spec (which is 'automatic')
    completeness_spec.append(generate_completeness_auto_spec(&lean_info).as_mut());

    // Generate the (placeholder) proof that the spec can be satisfied
    completeness_spec.push(String::from(""));
    completeness_spec.append(generate_user_completeness_theorem(&lean_info).as_mut());

    completeness_spec
}

fn generate_func_lean_completeness(lean_info: &LeanFuncInfo) -> Vec<String> {

    let mut completeness: Vec<String> = Vec::new();

    // Gneerate the soundness proof
    completeness.push(String::from(""));
    completeness.append(generate_completeness_auto_theorem(&lean_info).as_mut());

    completeness
}


//
// Lean code file generation
//

fn make_casm_lines(cairo_program: &CairoProgram) -> Vec<String> {
    let casm = cairo_program.to_string();
    let orig_casm_lines = casm.split("\n");
    let mut casm_lines: Vec<String> = Vec::new();

    // Comment out the hints (and indent)
    let mut in_hint = false;
    for line in orig_casm_lines {
        if line.is_empty() {
            continue;
        }
        in_hint = in_hint || line.contains("%{");
        casm_lines.push(if in_hint { format!("  -- {line}") } else { format!("  {line}") });
        in_hint = !(!in_hint || line.contains("%}"));
    }

    casm_lines
}

pub fn generate_lean_code(test_name: &str, cairo_program: &CairoProgram, is_lean3: bool) -> String {
    if is_lean3 {
        // Not supported for Lean 3.
        return "".into();
    }

    let mut lean_code: Vec<String> = Vec::new();
    let func_name = func_name_from_test_name(test_name);
    let casm_lines = make_casm_lines(cairo_program);

    lean_code.push("import Verification.Semantics.Assembly".into());
    lean_code.push("import Verification.Semantics.Completeness.VmAssembly".into());
    lean_code.push("".into());
    lean_code.push("set_option maxRecDepth 1024".into());
    lean_code.push("".into());

    // Soundness (field based) code.
    lean_code.push("open Casm in".into());
    lean_code.push(format!("casm_code_def {code_def_name} := {{", code_def_name = lean_code_name(&func_name)));
    for line in &casm_lines {
        lean_code.push(line.clone());
    }
    lean_code.push("}".into());
    lean_code.push("".into());

    // Completeness (VM based) code.
    // let mut casm_lines = make_casm_lines(cairo_program).as_mut();
    lean_code.push("open Casm in".into());
    lean_code.push(format!("vm_casm_code_def {code_def_name} := {{", code_def_name = lean_vm_code_name(&func_name)));
    for line in &casm_lines {
        lean_code.push(line.clone());
    }
    lean_code.push("}".into());
    lean_code.push("".into());

    lean_code.join("\n")
}

//
// Lean Output Functions.
//

pub fn write_lean_soundness_spec_file(test_path: &Path, test_name: &str, spec: Option<&String>) -> Result<(), std::io::Error> {
    let spec_str = match spec { Some(content) => { content }, _ => { return Ok(()); }};
    let func_name = func_name_from_test_name(test_name);
    let lean_path = lean_verification_path(test_path);
    let soundness_file_path = lean_file_path(
        &lean_path, &lean_soundness_spec_file_name(&func_name, true));
    fs::create_dir_all(lean_path)?;
    fs::write(soundness_file_path, spec_str)
}

pub fn write_lean_soundness_file(test_path: &Path, test_name: &str, soundness: Option<&String>) -> Result<(), std::io::Error> {
    let soundness_str = match soundness { Some(content) => { content }, _ => { return Ok(()); }};
    let func_name = func_name_from_test_name(test_name);
    let lean_path = lean_verification_path(test_path);
    let soundness_file_path = lean_file_path(
        &lean_path, &lean_soundness_file_name(&func_name, true));
    fs::create_dir_all(lean_path)?;
    fs::write(soundness_file_path, soundness_str)
}

pub fn write_lean_completeness_spec_file(test_path: &Path, test_name: &str, spec: Option<&String>) -> Result<(), std::io::Error> {
    let spec_str = match spec { Some(content) => { content }, _ => { return Ok(()); }};
    let func_name = func_name_from_test_name(test_name);
    let lean_path = lean_verification_path(test_path);
    let completeness_file_path = lean_file_path(
        &lean_path, &lean_completeness_spec_file_name(&func_name, true));
    fs::create_dir_all(lean_path)?;
    fs::write(completeness_file_path, spec_str)
}

pub fn write_lean_completeness_file(test_path: &Path, test_name: &str, completeness: Option<&String>) -> Result<(), std::io::Error> {
    let completeness_str = match completeness { Some(content) => { content }, _ => { return Ok(()); }};
    let func_name = func_name_from_test_name(test_name);
    let lean_path = lean_verification_path(test_path);
    let completeness_file_path = lean_file_path(
        &lean_path, &lean_completeness_file_name(&func_name, true));
    fs::create_dir_all(lean_path)?;
    fs::write(completeness_file_path, completeness_str)
}

pub fn write_lean_code_file(
    test_path: &Path, test_name: &str, lean_code: Option<&String>, is_lean3: bool
) -> Result<(), std::io::Error> {

    if is_lean3 {
        return Ok(()); // Not supported in Lean 3.
    }

    let code_str = match lean_code { Some(content) => { content }, _ => { return Ok(()); }};
    let func_name = func_name_from_test_name(test_name);
    let lean_path = lean_verification_path(test_path);
    let code_file_path = lean_file_path(
        &lean_path, &lean_code_file_name(&func_name, true));
    fs::create_dir_all(lean_path)?;
    fs::write(code_file_path, code_str)
}