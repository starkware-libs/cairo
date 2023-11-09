use std::fmt::Write;
use std::fs;
use std::sync::Mutex;

use anyhow::Context;
use cairo_lang_starknet::allowed_libfuncs::{validate_compatible_sierra_version, ListSelector};
use cairo_lang_starknet::casm_contract_class::CasmContractClass;
use cairo_lang_starknet::contract_class::{ContractClass, ContractEntryPoints};
use cairo_lang_utils::bigint::BigUintAsHex;
use clap::Parser;
use indicatif::{ProgressBar, ProgressState, ProgressStyle};
use rayon::prelude::{IntoParallelIterator, ParallelIterator};
use serde::Deserialize;

/// Command line args parser.
/// Exits with 0/1 if the input is formatted correctly/incorrectly.
#[derive(Parser, Debug)]
#[clap(version, verbatim_doc_comment)]
struct Args {
    /// The file to compile
    file: String,
    /// The allowed libfuncs list to use (default: most recent audited list).
    #[arg(long)]
    allowed_libfuncs_list_name: Option<String>,
    /// A file of the allowed libfuncs list to use.
    #[arg(long)]
    allowed_libfuncs_list_file: Option<String>,
    /// Add pythonic hints.
    #[arg(long, default_value_t = false)]
    add_pythonic_hints: bool,
}

/// The contract class from db.
#[derive(Deserialize)]
pub struct ContractClassInfo {
    /// The previous compiled class hash.
    pub compiled_class_hash: BigUintAsHex,
    /// The class hash.
    pub class_hash: BigUintAsHex,
    /// The sierra program.
    pub sierra_program: Vec<BigUintAsHex>,
    /// The entry points by type.
    pub entry_points_by_type: ContractEntryPoints,
}

struct Report {
    /// The classes that failed validation.
    validation_failures: Vec<BigUintAsHex>,
    /// The classes that failed compilation.
    compilation_failures: Vec<BigUintAsHex>,
    /// The classes that had a non matching class hash after compilation.
    compilation_mismatch: Vec<BigUintAsHex>,
}

fn main() -> anyhow::Result<()> {
    let args: Args = Args::parse();
    let list_selector =
        ListSelector::new(args.allowed_libfuncs_list_name, args.allowed_libfuncs_list_file)
            .expect("Both allowed libfunc list name and file were supplied.");
    // Reading the contract classes from the file.
    let tested_classes: Vec<ContractClassInfo> = serde_json::from_str(
        &fs::read_to_string(&args.file)
            .with_context(|| format!("Failed to read {}.", &args.file))?,
    )
    .with_context(|| "deserialization Failed.")?;
    let num_of_classes = tested_classes.len();
    let report = Mutex::new(Report {
        validation_failures: Vec::new(),
        compilation_failures: Vec::new(),
        compilation_mismatch: Vec::new(),
    });
    // Setting up a progress bar.
    let bar = ProgressBar::new(num_of_classes as u64);
    bar.set_style(
        ProgressStyle::with_template(
            "{spinner:.green} [{elapsed_precise}] [{wide_bar:.cyan/blue}] {pos:>6}/{len:6} ({eta})",
        )
        .unwrap()
        .with_key("eta", |state: &ProgressState, w: &mut dyn Write| {
            write!(w, "{:.1}s", state.eta().as_secs_f64()).unwrap()
        })
        .progress_chars("#>-"),
    );
    tested_classes.into_par_iter().for_each(|sierra_class| {
        bar.inc(1);
        // Reconstructing the contract class from the read class.
        let contract_class = ContractClass {
            sierra_program: sierra_class.sierra_program,
            sierra_program_debug_info: None,
            contract_class_version: "0.1.0".to_string(),
            entry_points_by_type: sierra_class.entry_points_by_type,
            abi: None,
        };
        // Validating the contract class.
        if validate_compatible_sierra_version(&contract_class, list_selector.clone()).is_err() {
            report.lock().unwrap().validation_failures.push(sierra_class.compiled_class_hash);
            return;
        };
        // Compiling the contract class.
        let Ok(compiled_contract_class) =
            CasmContractClass::from_contract_class(contract_class, args.add_pythonic_hints)
        else {
            report.lock().unwrap().compilation_failures.push(sierra_class.compiled_class_hash);
            return;
        };
        // Checking that the compiled class hash matches the previous compiled class hash.
        if sierra_class.compiled_class_hash.value
            != compiled_contract_class.compiled_class_hash().to_biguint()
        {
            report.lock().unwrap().compilation_mismatch.push(sierra_class.compiled_class_hash);
        };
    });
    bar.finish_and_clear();
    let report = report.into_inner().unwrap();
    if !report.validation_failures.is_empty() {
        println!(
            "Validation failures: (Printing first 10 out of {})",
            report.validation_failures.len()
        );
        for failure in report.validation_failures.iter().take(10) {
            println!("compiled class hash: {:#x}", failure.value);
        }
    }
    if !report.compilation_failures.is_empty() {
        println!(
            "Compilation failures: (Printing first 10 out of {})",
            report.compilation_failures.len()
        );
        for failure in report.compilation_failures.iter().take(10) {
            println!("compiled class hash: {:#x}", failure.value);
        }
    }
    if !report.compilation_mismatch.is_empty() {
        println!(
            "Compilation mismatch {} out of {num_of_classes}: (Printing first 10)",
            report.compilation_mismatch.len()
        );
        for failure in report.compilation_mismatch.iter().take(10) {
            println!("compiled class hash: {:#x}", failure.value);
        }
    }
    Ok(())
}
