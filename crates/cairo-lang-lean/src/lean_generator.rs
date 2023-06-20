use std::fs;
use std::path::{Path, PathBuf};
use itertools::Itertools;
use cairo_lang_casm::builder::CasmBuilderAuxiliaryInfo;
use cairo_lang_casm::instructions::InstructionBody;
use cairo_lang_casm::operand::ResOperand;
use cairo_lang_sierra_to_casm::compiler::CairoProgram;
use cairo_lang_utils::bigint::BigIntAsHex;

/*struct LeanVar {
    name: String,
    var: Var,
}*/

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

/*pub struct TempVarStatement {
    var: LeanVar,
    //assignment: Option<String>,
    //ap_offset: usize,
}

enum LeanStatement {
    // A tempvar statment has the variable name on the lhs and an optional rhs.
    TempVar(TempVarStatement),
}

pub struct LeanRawInfo {
    args: Vec<LeanVar>,
    statements: Vec<LeanStatement>,
    return_args: Vec<String>
}

impl LeanRawInfo {
    pub fn add_arg(&mut self, name: &str, var: Var) {
        self.args.push(LeanVar { name: String::from(name), var: var });
    }

    pub fn add_return_arg(&mut self, name: &str, var: Var) {
        self.return_args.push(String::from(name));
    }

    pub fn add_tempvar(
        &mut self, var_name: &str, var: Var
    ) {
        self.statements.push(
            LeanStatement::TempVar(
                TempVarStatement {
                    var: Var { name: String::from(var_name), var: var },
                    //assignment: match assignment {
                    //    Some(ass) => Some(String::from(ass)),
                    //    _ => None
                    //},
                    //ap_offset: ap_offset
                }
            )
        );
    }
}

impl Default for LeanRawInfo {
    fn default() -> Self {
        Self {
            args: Default::default(),
            statements: Default::default(),
            return_args: Default::default(),
        }
    }
} */

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
    let casm_body = format!("def code_{} : list F := [\n", func_name);
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


pub fn generate_lean_soundness(test_name: &str, cairo_program: &CairoProgram) -> String {
    let func_name = test_name.split_whitespace().next().unwrap();

    let mut soundness: Vec<String> = Vec::new();
    soundness.push(String::from("import starkware.cairo.lean.semantics.soundness.prelude"));
    soundness.push(format!("import .{}", lean_code_file_name(func_name)));
    soundness.push(String::from("variables {F : Type} [field F] [decidable_eq F]"));

    let aux_info = match cairo_program.aux_info.as_ref() {
        Some(info) => info,
        _ => { return String::from("-- Could not find lean info."); }
    };

    let args: Vec<String> = aux_info.args.iter().map(
        |v| -> String {
            if let Some(arg) = aux_info.var_names.get(&v.0) { String::from(arg) } else { String::from("") }}
    ).collect();
    let ret_args = make_ret_args(aux_info, cairo_program);
    let args_str = args.iter().chain(ret_args.iter()).join(" ");

    soundness.push(format!("def spec_{func_name} (mem : F → F) (κ : ℕ) ({args_str} : F) : Prop :="));
    soundness.push(String::from("  true"));
    soundness.push(String::from(""));
    soundness.push(format!("def auto_spec_{func_name} (mem : F → F) (κ : ℕ) ({args_str} : F) : Prop :="));
    soundness.push(String::from("  true"));

    // For debugging!
    println!("{}", soundness.join("\n"));

    soundness.join("\n")
}

/// Returns the offset from the final ap of the return argument that holds
/// the number of the branch from which the function returned.
/// The returned number is positive (so returns 1 for the last return argument,
/// 2 for the one but last, etc.).
/// Returns None if could not determine the offset.
fn find_branch_num_ret_arg_affset(
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

fn make_subscript(idx: usize) -> String {
    let mut idx = idx;
    let mut res = String::new();
    loop {
        res.insert(0, "₀₁₂₃₄₅₆₇₈₉".chars().nth(idx % 10).unwrap());
        if idx == 0 {
            return res;
        }
        idx /= 10;
    }
}

fn make_ret_args(
    aux_info: &CasmBuilderAuxiliaryInfo,
    cairo_program: &CairoProgram,
) -> Vec<String> {
    // No branch ID and can use the names of the only branch.
    if aux_info.return_args.len() == 1 {
        return aux_info.return_args[0].exprs.iter().flat_map(
            |expr| expr.names.iter()
        ).map(|name| format!("ρ_{}",name)).collect();
    }

    let mut ret_args = Vec::new();
    let branch_id_pos = find_branch_num_ret_arg_affset(aux_info, cairo_program);
    let num_args = aux_info.return_args[0].exprs.iter().flat_map(|expr| expr.names.iter()).count();
    if let Some(pos) = branch_id_pos {
        for (i, name) in
            aux_info.return_args[0].exprs.iter().flat_map(|expr| expr.names.iter()).enumerate() {
            if i < num_args + 1 - pos {
                ret_args.push(format!("ρ_{}", name));
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

pub fn write_lean_soundness_file(test_path: &Path, test_name: &str, soundness: Option<&String>) -> Result<(), std::io::Error> {
    let soundness_str = match soundness { Some(content) => { content }, _ => { return Ok(()); }};
    let func_name = test_name.split_whitespace().next().unwrap();
    let lean_path = lean_verification_path(test_path);
    let soundness_file_path = lean_file_path(&lean_path, &lean_soundness_file_name(func_name));
    fs::create_dir_all(lean_path)?;
    fs::write(soundness_file_path, soundness_str)
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