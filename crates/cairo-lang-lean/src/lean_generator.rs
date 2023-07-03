use std::fs;
use std::path::{Path, PathBuf};
use cairo_lang_casm::cell_expression::CellExpression;
use itertools::Itertools;

use std::collections::HashMap;

use cairo_lang_casm::builder::{CasmBuilderAuxiliaryInfo, RetBranchDesc, StatementDesc, State, ExprDesc};
use cairo_lang_casm::instructions::InstructionBody;
use cairo_lang_casm::operand::ResOperand;
use cairo_lang_sierra_to_casm::compiler::CairoProgram;
use cairo_lang_utils::bigint::BigIntAsHex;

pub fn lean_code_file_name(func_name: &str) -> String {
    String::from(func_name) + "_code.lean"
}

pub fn lean_soundness_file_name(func_name: &str) -> String {
    String::from(func_name) + "_soundness.lean"
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

struct RetArgs {
    branch_info: Vec<(usize, Option<usize>)>,
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

    fn set_arg_names(&mut self, branch_desc: &Vec<RetBranchDesc>) {
        if self.branch_info.len() == 0 {
            return;
        }
        let arg_num = self.branch_info[0].0;
        self.arg_names = (0..arg_num).map(
            |pos| {
                if let Some(branch_id_pos) = self.branch_id_pos {
                    if pos == branch_id_pos {
                        String::from("branch_id")
                    } else if pos < branch_id_pos {
                        RetArgs::get_arg_name_at_pos(branch_desc, pos)
                    } else {
                        RetArgs::get_arg_name_at_pos(branch_desc, pos - 1)
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
        if 0 == self.branch_info.len() {
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
        } else {
            self.num_implicit_ret_args = 0;
        }
    }
}

/// Identifies the statements at the end of the function which copy the return variables to the end
/// of the stack. Returns a vector of tuples where the first element of the tuple is the number of statements in
/// the branch return block and the second is the position of the branch ID return argument inside that block.
fn get_ret_blocks(aux_info: &CasmBuilderAuxiliaryInfo, cairo_program: &CairoProgram) -> Vec<(usize, Option<usize>)> {
    let mut count = 0;
    let mut branch_id_pos: Option<usize> = None;
    let mut branch_counts: Vec<(usize, Option<usize>)> = Vec::new();

    let set_branch =
        |branch_counts: &mut Vec<(usize, Option<usize>)>, count: &mut usize, branch_id_pos: &mut Option<usize>| {
            if *count != 0 {
                match *branch_id_pos {
                    Some(pos) => branch_counts.push((*count, Some(*count - pos - 1))),
                    None => branch_counts.push((*count, None)),
                };
                *count = 0;
                *branch_id_pos = None;
            }
        };

    for pos in (aux_info.core_libfunc_instr_num..cairo_program.instructions.len()).rev() {

        let branch_num = aux_info.return_args.len() - 1 - branch_counts.len();

        // A branch return block ends with a jmp or return.
        match &cairo_program.instructions[pos].body {
            InstructionBody::Ret(_) | InstructionBody::Jump(_) | InstructionBody::AddAp(_) => {
                set_branch(&mut branch_counts, &mut count, &mut branch_id_pos);
            },
            InstructionBody::AssertEq(assert_eq) => {
                match &assert_eq.b {
                    ResOperand::Immediate(value) => {
                        if *value == BigIntAsHex::from(branch_num) {
                            branch_id_pos = Some(count);
                        }
                    },
                    _ => { }
                }
                count += 1;
             },
            _ => { count += 1; }
        }
    }

    set_branch(&mut branch_counts, &mut count, &mut branch_id_pos);

    branch_counts.reverse();
    branch_counts
}

fn check_ret_blocks(ret_blocks: &Vec<(usize, Option<usize>)>, branch_num: usize) {
    assert!(branch_num == ret_blocks.len(), "branch_num != ret_blocks.len()");

    if branch_num == 0 {
        return;
    }

    // check that all branches have the same number of statements
    let mut count = 0;
    for (num, _) in ret_blocks {
        if count == 0 {
            count = *num;
        } else {
            assert!(count == *num, "count != *num");
        }
    }
    // check that all branches have the same branch ID position.
    let branch_id_pos = ret_blocks[0].1;
    for (_, pos) in ret_blocks {
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
    }
}

/// Returns the offset from the final ap of the return argument that holds
/// the number of the branch from which the function returned.
/// The returned number is positive (so returns 1 for the last return argument,
/// 2 for the one but last, etc.).
/// Returns None if could not determine the offset.
fn find_branch_num_ret_arg_offset(
    aux_info: &CasmBuilderAuxiliaryInfo,
    cairo_program: &CairoProgram,
) -> Option<usize> {
    cairo_program.instructions.iter().rev().position(
        |inst| -> bool {
            match &inst.body {
                InstructionBody::AssertEq(assert_eq) => {
                    match &assert_eq.b {
                        ResOperand::Immediate(value) => {
                            if *value == BigIntAsHex::from(aux_info.return_args.len() - 1) {
                                return true;
                            }
                            false
                        },
                        _ => { false }
                    }
                 },
                _ => false
            }
        }
    )
}

fn make_ret_args(
    aux_info: &CasmBuilderAuxiliaryInfo,
    cairo_program: &CairoProgram,
) -> Vec<String> {
    // No branch ID and can use the names of the only branch.
    if aux_info.return_args.len() == 1 {
        return aux_info.return_args[0].exprs.iter().flat_map(
            |expr| expr.names.iter()
        ).map(|name| format!("ρ_{name}")).collect();
    }

    let mut ret_args = Vec::new();
    let branch_id_pos = find_branch_num_ret_arg_offset(aux_info, cairo_program);
    let num_args = aux_info.return_args[0].exprs.iter().flat_map(|expr| expr.names.iter()).count();
    if let Some(pos) = branch_id_pos {
        for (i, name) in
            aux_info.return_args[0].exprs.iter().flat_map(|expr| expr.names.iter()).enumerate() {
            if i < num_args + 1 - pos {
                ret_args.push(format!("ρ_{name}"));
            } else {
                if i == num_args + 1 - pos {
                    ret_args.push(String::from("ρ_branch_id"));
                }
                ret_args.push(format!("ρ{}", make_subscript(i + pos - num_args - 1)));
            }
        }
    } else {
        for i in 0..num_args {
            ret_args.push(format!("ρ{}", make_subscript(i)));
        }
    }
    ret_args
}

/// Rebinding information for variables.
#[derive(Clone)]
struct VarRebind {
    /// Number of rebindings for each variable. 0 is for the first variable,
    /// 1 for the bnext one, etc.
    vars: HashMap<String, usize>,
}

impl VarRebind {
    pub fn new() -> Self {
        Self { vars: HashMap::new() }
    }

    pub fn get_var_name(&self, name: &str) -> String {
        let count = self.vars.get(name).unwrap_or(&0);
        if *count == 0 {
            name.to_string()
        } else {
            format!("{}{}", name, make_subscript(*count))
        }
    }

    pub fn rebind(&mut self, name: &str) -> usize {
        *self.vars.entry(name.to_string())
        .and_modify(|c| *c += 1)
        .or_insert(0)
    }

    pub fn get_rebind_name(&mut self, name: &str) -> String {
        self.rebind(name);
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
}

/// All the information needed to generate the Lean code for a single function.
struct LeanFuncInfo<'a> {
    func_name: String,
    aux_info: &'a CasmBuilderAuxiliaryInfo,
    ret_args: RetArgs,
}

impl<'a> LeanFuncInfo<'a> {

        pub fn new(
            func_name: String,
            aux_info: &'a CasmBuilderAuxiliaryInfo,
            cairo_program: &CairoProgram
        ) -> Self {
            let ret_args = LeanFuncInfo::make_ret_args(&aux_info, cairo_program);
            Self { func_name, aux_info, ret_args }
        }

        pub fn make_ret_args(aux_info: &CasmBuilderAuxiliaryInfo, cairo_program: &CairoProgram) -> RetArgs {
            let ret_blocks = get_ret_blocks(aux_info, cairo_program);
            check_ret_blocks(&ret_blocks, aux_info.return_args.len());

            let branch_id_pos = if 0 < ret_blocks.len() { ret_blocks[0].1 } else { None };
            let mut ret_args = RetArgs {
                branch_info: ret_blocks,
                arg_names: Vec::new(),
                branch_id_pos: branch_id_pos,
                num_implicit_ret_args: 0, // will be set later
            };
            ret_args.set_arg_names(&aux_info.return_args);
            ret_args.set_implicit_ret_arg_num();
            ret_args
        }

        /// Returns the list of all argument names.
        pub fn get_arg_names(&self) -> Vec<String> {
            self.aux_info.args.iter().map(
                |v| -> String {
                    if let Some(arg) = self.aux_info.var_names.get(&v.0) { String::from(arg) } else { String::from("") }}
            ).collect()
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

        // True if this expression represents a range check: x = [<range check pointer>]
        pub fn is_range_check(&self, expr: &ExprDesc) -> bool {
            expr.op == "*()" &&
            if let Some(str) = self.aux_info.var_names.get(&expr.var_a) {
                str == "range_check"
            } else { false }
        }

        // True if this expression increments the range check pointer.
        pub fn is_range_check_inc(&self, expr: &ExprDesc) -> bool {
            expr.op == "++" &&
            if let Some(str) = self.aux_info.var_names.get(&expr.var_a) {
                str == "range_check"
            } else { false }
        }

}

// Soundness generation functions

fn generate_soundness_prelude(lean_info: &LeanFuncInfo) -> Vec<String> {
    let mut prelude: Vec<String> = Vec::new();
    prelude.push(String::from("import starkware.cairo.lean.semantics.soundness.prelude"));
    prelude.push(format!("import .{}", lean_code_file_name(&lean_info.func_name[..])));
    prelude.push(String::from("open tactic"));
    prelude.push(String::from(""));
    prelude.push(String::from("variables {F : Type} [field F] [decidable_eq F]"));
    prelude.push(String::from("variable  mem : F → F"));
    prelude.push(String::from("variable  σ : register_state F"));
    prelude.push(String::from(""));

    prelude
}

fn generate_consts(lean_info: &LeanFuncInfo) -> Vec<String> {
    let mut consts: Vec<String> = Vec::new();

    for const_def in &lean_info.aux_info.consts {

        // let expr = const_def.expr.replace("::", "_");
        if let CellExpression::Immediate(value) = &const_def.value {
            consts.push(format!(
                "def {const_name} : F := ({value_str} : F) -- {expr}",
                const_name = const_def.name,
                value_str = value.to_string(),
                expr = const_def.expr,
            ));
        } else {
            consts.push(format!(
                "def {const_name} : F := {expr}",
                const_name = const_def.name,
                expr = const_def.expr,
            ));
        }
    }

    consts
}

fn continue_specs(specs: &mut Vec<String>) {
    if let Some(last) = specs.last_mut() {
        if !last.ends_with("=") && !last.ends_with(',') && !last.ends_with('∨') && !last.ends_with('(') {
            last.push_str(" ∧")
        }
    }
}

fn push_spec(specs: &mut Vec<String>, indent: usize, str: &str) {
    if !str.starts_with(')') {
        continue_specs(specs);
    }
    specs.push(" ".repeat(indent) + str);
}

fn append_spec(specs: &mut Vec<String>, mut more_specs: Vec<String>) {
    continue_specs(specs);
    specs.append(&mut more_specs);
}

fn generate_soundness_user_spec(lean_info: &LeanFuncInfo) -> Vec<String> {
    let func_name = &lean_info.func_name[..];
    let args_str =
        lean_info.get_arg_names().iter().chain(lean_info.get_explicit_ret_arg_names().iter()).join(" ");

    let mut user_spec: Vec<String> = Vec::new();
    user_spec.push(format!("def spec_{func_name} (mem : F → F) (κ : ℕ) ({args_str} : F) : Prop :="));
    user_spec.push(String::from("  true"));

    user_spec
}

fn generate_soundness_auto_spec(lean_info: &LeanFuncInfo) -> Vec<String> {
    let func_name = &lean_info.func_name[..];
    let args_str =
        lean_info.get_arg_names().iter().chain(lean_info.get_explicit_ret_arg_names().iter()).join(" ");
    let mut rebind = VarRebind::new();
    for arg in lean_info.get_arg_names() {
        rebind.rebind(&arg);
    }

    let mut auto_spec: Vec<String> = Vec::new();
    auto_spec.push(format!("def spec_{func_name} (mem : F → F) (κ : ℕ) ({args_str} : F) : Prop :="));

    auto_spec.append(&mut generate_soundness_auto_spec_block(lean_info, &mut rebind, 0, 2));

    auto_spec
}

fn generate_soundness_auto_spec_block(
    lean_info: &LeanFuncInfo,
    rebind: &mut VarRebind,
    block_start: usize,
    indent: usize
) -> Vec<String> {
    let mut spec_block: Vec<String> = Vec::new();

    for (i, statement) in lean_info.aux_info.statements[block_start..].iter().enumerate() {
        match statement {
            StatementDesc::TempVar(var) | StatementDesc::LocalVar(var) => {
                // Must rewrite variable names with the rebind subscripts before rebinding the lhs.
                let expr = if let Some(expr) = &var.expr {
                    Some(rebind.replace_var_names_in_expr(&expr.expr))
                } else { None };
                let var_name=rebind.get_rebind_name(&var.name);
                push_spec(
                    &mut spec_block,
                    indent,
                    &format!(
                        "∃ {var_name} : F,{expr}",
                        expr=if let Some(expr_str) = &expr {
                            format!(" {var_name} = {expr_str}")
                        } else { String::new() }
                    ));
            },
            StatementDesc::Let(assign) => {
                // The range check increment is not part of the specification.
                if lean_info.is_range_check_inc(&assign.expr) {
                    continue;
                }
                let expr_str = rebind.replace_var_names_in_expr(&assign.expr.expr);
                push_spec(
                    &mut spec_block,
                    indent,
                    &format!("∃ {var_name} : F, {var_name} = {expr_str}", var_name = rebind.get_rebind_name(&assign.lhs)));
            },
            StatementDesc::Assert(assert) => {
                if lean_info.is_range_check(&assert.expr) {
                    push_spec(
                        &mut spec_block,
                        indent,
                        &format!("is_range_checked (rc_bound F) {var_name}", var_name = rebind.get_var_name(&assert.lhs)));
                } else {
                    push_spec(
                        &mut spec_block,
                        indent,
                        &format!("{var_name} = {expr}",
                            var_name = rebind.get_var_name(&assert.lhs),
                            expr = rebind.replace_var_names_in_expr(&assert.expr.expr)));
                }
            },
            StatementDesc::Jump(jump) => {
                let pos = lean_info.aux_info.statements.iter().position(
                    |s| if let StatementDesc::Label(label) = s { label == &jump.target } else { false }
                );

                // The jump target may be a branch name.
                if pos.is_none() {
                    append_spec(
                        &mut spec_block,
                        generate_soundness_auto_spec_ret_block(lean_info, rebind, &jump.target, indent));
                    return spec_block;
                }

                let pos = pos.unwrap();
                assert!(block_start + i < pos, "No backward jumps supported.");

                match &jump.cond_var {
                    Some(cond_var) => {
                        // Conditional jump, must generate both branches.
                        push_spec(&mut spec_block, indent, "(");
                        let indent = indent + 2;
                        let var_name = rebind.get_var_name(&cond_var.0);

                        // First branch
                        push_spec(&mut spec_block, indent, &format!("({var_name} = 0"));
                        // Clone the rebind object before entering the branch, as the second branch
                        // should be independent of the rebinding accumulated in the first branch.
                        append_spec(&mut spec_block,
                            generate_soundness_auto_spec_block(
                                lean_info, &mut rebind.clone(), block_start + i + 1, indent + 2));

                        push_spec(&mut spec_block, indent, ") ∨");

                        // Second branch
                        push_spec(&mut spec_block, indent, &format!("({var_name} ≠ 0"));
                        append_spec(&mut spec_block,
                            generate_soundness_auto_spec_block(
                                lean_info, rebind, pos + 1, indent + 2));
                        push_spec(&mut spec_block, indent, ")");
                        let indent = indent - 2;
                        push_spec(&mut spec_block, indent, ")");
                    },
                    _ => {
                        // Unconditional jump, must generate only the target block.
                        append_spec(&mut spec_block,
                            generate_soundness_auto_spec_block(lean_info, rebind, pos + 1, indent));
                    }
                };
                return spec_block; // Remain code handled already in the branches.
            },
            StatementDesc::ApPlus(step) => {}, // Do nothing
            StatementDesc::Label(label) => {}, // Do nothing
        }
    }

    append_spec(
        &mut spec_block,
        generate_soundness_auto_spec_ret_block(lean_info, rebind, "Fallthrough", indent));
    spec_block
}

fn generate_soundness_auto_spec_ret_block(
    lean_info: &LeanFuncInfo,
    rebind: &VarRebind,
    ret_block_name: &str,
    indent: usize
) -> Vec<String> {
    let mut spec_block: Vec<String> = Vec::new();

    let branch_id = lean_info.aux_info.return_args.iter().position(
        |b| b.name == ret_block_name
    ).expect("Could not find return block.");

    let ret_arg_names = lean_info.get_explicit_ret_arg_names();
    let flat_exprs = lean_info.aux_info.return_args[branch_id].flat_exprs();

    let mut ret_arg_start = 0;

    if lean_info.ret_args.branch_id_pos.is_some() {
        // The first return argument is the branch ID
        push_spec(&mut spec_block, indent, &format!("{ret_arg} = {branch_id}", ret_arg = &ret_arg_names[0]));
        ret_arg_start = 1;
    }

    for (name, expr) in ret_arg_names[ret_arg_start..].iter().zip(
        flat_exprs[lean_info.ret_args.num_implicit_ret_args..].iter()
    ) {
        let expr = rebind.replace_var_names_in_expr(expr);
        push_spec(&mut spec_block, indent, &format!("{name} = {expr}"));
    }

    spec_block
}

pub fn generate_lean_soundness(test_name: &str, cairo_program: &CairoProgram) -> String {

    let aux_info = match cairo_program.aux_info.as_ref() {
        Some(info) => info,
        _ => { return String::from("-- Could not find lean info."); }
    };

    let lean_info = LeanFuncInfo::new(
        test_name.split_whitespace().next().unwrap().to_string(),
        aux_info,
        cairo_program
    );

    let mut soundness: Vec<String> = Vec::new();
    // Generate the prelude
    soundness.append(generate_soundness_prelude(&lean_info).as_mut());

    // Generate the constant definitions
    soundness.append(generate_consts(&lean_info).as_mut());
    soundness.push(String::from(""));

    // Generate the user spec
    soundness.append(generate_soundness_user_spec(&lean_info).as_mut());
    soundness.push(String::from(""));
    // Generate the auto spec
    soundness.append(generate_soundness_auto_spec(&lean_info).as_mut());

    // For debugging!
    println!("{}", soundness.join("\n"));

    soundness.join("\n")
}

pub fn write_lean_soundness_file(test_path: &Path, test_name: &str, soundness: Option<&String>) -> Result<(), std::io::Error> {
    let soundness_str = match soundness { Some(content) => { content }, _ => { return Ok(()); }};
    let func_name = test_name.split_whitespace().next().unwrap();
    let lean_path = lean_verification_path(test_path);
    let soundness_file_path = lean_file_path(&lean_path, &lean_soundness_file_name(func_name));
    fs::create_dir_all(lean_path)?;
    fs::write(soundness_file_path, soundness_str)
}

// Old code

/// Generates the lean assembly code file for a single function.
pub fn generate_lean_casm(
    test_path: &Path, test_name: &str, casm: Option<&String>
) -> Result<(), std::io::Error> {

    let func_name = test_name.split_whitespace().next().unwrap();
    let lean_path = lean_verification_path(test_path);
    let code_file_path = lean_file_path(&lean_path, &lean_code_file_name(func_name));

    fs::create_dir_all(lean_path)?;

    let mut lean_casm = String::new();
    lean_casm.push_str("import starkware.cairo.lean.semantics.soundness.hoare\n\n");
    lean_casm.push_str("variables {F : Type} [field F] [decidable_eq F]\n\n");
    let casm_body = format!("def code_{func_name} : list F := [\n");
    lean_casm.push_str(&casm_body);
    // Temporary, not valid Lean yet.
    lean_casm.push_str(
        match casm {
            Some(casm_str) => casm_str,
            _ => "-- No casm generated for this function.",
        }
    );
    lean_casm.push_str("]\n");

    // For debugging!
    println!("{}", lean_casm);

    fs::write(code_file_path, lean_casm)

}

/* pub fn dump_to_test_file(
    tests: OrderedHashMap<String, Test>,
    filename: &str,
) -> Result<(), std::io::Error> {
    let mut test_strings = Vec::new();
    for (test_name, test) in tests {
        let mut tag_strings = vec![TAG_PREFIX.to_string() + &test_name];
        for (tag, content) in test.attributes {
            tag_strings.push(
                TAG_PREFIX.to_string()
                    + &tag
                    + if content.is_empty() { "" } else { "\n" }
                    + &content,
            );
        }
        test_strings.push(tag_strings.join("\n\n"));
    }
    fs::write(
        filename,
        test_strings.join(&("\n\n".to_string() + TAG_PREFIX + TEST_SEPARATOR + "\n\n")) + "\n",
    )
}*/