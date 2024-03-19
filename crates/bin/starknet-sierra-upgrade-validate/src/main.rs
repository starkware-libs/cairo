use std::fmt::Write;
use std::fs;
use std::sync::Mutex;

use anyhow::Context;
use cairo_lang_starknet_classes::allowed_libfuncs::{AllowedLibfuncsError, ListSelector};
use cairo_lang_starknet_classes::casm_contract_class::{
    CasmContractClass, StarknetSierraCompilationError,
};
use cairo_lang_starknet_classes::compiler_version::VersionId;
use cairo_lang_starknet_classes::contract_class::{ContractClass, ContractEntryPoints};
use cairo_lang_utils::bigint::BigUintAsHex;
use clap::Parser;
use indicatif::{ProgressBar, ProgressState, ProgressStyle};
use rayon::prelude::{IntoParallelIterator, ParallelIterator};
use serde::Deserialize;

/// Runs validation on existing contract classes to make sure they are still valid and compilable.
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
    /// Sierra version to override to prior to compilation.
    #[arg(long)]
    override_version: Option<String>,
    /// The max bytecode size.
    #[arg(long, default_value_t = 180000)]
    max_bytecode_size: usize,
}

/// Parses version id from string.
fn parse_version_id(major_minor_patch: &str) -> anyhow::Result<VersionId> {
    let context = || format!("Could not parse version {major_minor_patch}.");
    let (major, minor_patch) = major_minor_patch.split_once('.').with_context(context)?;
    let (minor, patch) = minor_patch.split_once('.').with_context(context)?;
    Ok(VersionId {
        major: major.parse().with_context(context)?,
        minor: minor.parse().with_context(context)?,
        patch: patch.parse().with_context(context)?,
    })
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
    validation_failures: Vec<ValidationFailure>,
    /// The classes that failed compilation.
    compilation_failures: Vec<CompilationFailure>,
    /// The classes that had a non matching class hash after compilation.
    compilation_mismatch: Vec<CompilationMismatch>,
}

/// Validation failure information.
struct ValidationFailure {
    class_hash: BigUintAsHex,
    err: AllowedLibfuncsError,
}

/// Compilation failure information.
struct CompilationFailure {
    class_hash: BigUintAsHex,
    err: StarknetSierraCompilationError,
}

/// Compilation mismatch information.
struct CompilationMismatch {
    class_hash: BigUintAsHex,
    old: BigUintAsHex,
    new: BigUintAsHex,
}

fn main() -> anyhow::Result<()> {
    let args: Args = Args::parse();
    let list_selector =
        ListSelector::new(args.allowed_libfuncs_list_name, args.allowed_libfuncs_list_file)
            .expect("Both allowed libfunc list name and file were supplied.");
    let override_version = match args.override_version {
        Some(version) => Some(parse_version_id(&version)?),
        None => None,
    };
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
    let config =
        RunConfig { list_selector, override_version, max_bytecode_size: args.max_bytecode_size };
    tested_classes.into_par_iter().for_each(|sierra_class| {
        bar.inc(1);
        match run_single(sierra_class, &config) {
            RunResult::ValidationFailure(failure) => {
                report.lock().unwrap().validation_failures.push(failure);
            }
            RunResult::CompilationFailure(failure) => {
                report.lock().unwrap().compilation_failures.push(failure);
            }
            RunResult::CompilationMismatch(mismatch) => {
                report.lock().unwrap().compilation_mismatch.push(mismatch);
            }
            RunResult::Success => {}
        };
    });
    bar.finish_and_clear();
    let report = report.into_inner().unwrap();
    if !report.validation_failures.is_empty() {
        println!(
            "Validation failures: (Printing first 10 out of {})",
            report.validation_failures.len()
        );
        for ValidationFailure { class_hash, err } in report.validation_failures.iter().take(10) {
            println!("Validation failure for {:#x}: {err}", class_hash.value);
        }
    }
    if !report.compilation_failures.is_empty() {
        println!(
            "Compilation failures: (Printing first 10 out of {})",
            report.compilation_failures.len()
        );
        for CompilationFailure { class_hash, err } in report.compilation_failures.iter().take(10) {
            println!("Compilation failure for {:#x}: {err}", class_hash.value);
        }
    }
    if !report.compilation_mismatch.is_empty() {
        println!(
            "Compilation mismatch {} out of {num_of_classes}: (Printing first 10)",
            report.compilation_mismatch.len()
        );
        for CompilationMismatch { class_hash, old, new } in
            report.compilation_mismatch.iter().take(10)
        {
            println!(
                "Compilation mismatch for {:#x}: old={:#x}, new={:#x}",
                class_hash.value, old.value, new.value
            );
        }
    }
    Ok(())
}

/// The configuration for a Sierra compilation run.
struct RunConfig {
    list_selector: ListSelector,
    override_version: Option<VersionId>,
    max_bytecode_size: usize,
}

/// The result of a Sierra compilation run.
enum RunResult {
    ValidationFailure(ValidationFailure),
    CompilationFailure(CompilationFailure),
    CompilationMismatch(CompilationMismatch),
    Success,
}

/// Runs a single Sierra compilation.
fn run_single(mut sierra_class: ContractClassInfo, config: &RunConfig) -> RunResult {
    if let Some(override_version) = config.override_version {
        sierra_class.sierra_program[0].value = override_version.major.into();
        sierra_class.sierra_program[1].value = override_version.minor.into();
        sierra_class.sierra_program[2].value = override_version.patch.into();
    }
    let contract_class = ContractClass {
        sierra_program: sierra_class.sierra_program,
        sierra_program_debug_info: None,
        contract_class_version: "0.1.0".to_string(),
        entry_points_by_type: sierra_class.entry_points_by_type,
        abi: None,
    };
    let class_hash = sierra_class.class_hash;
    if let Err(err) = contract_class.validate_version_compatible(config.list_selector.clone()) {
        return RunResult::ValidationFailure(ValidationFailure { class_hash, err });
    };
    let compiled_contract_class = match CasmContractClass::from_contract_class(
        contract_class,
        false,
        config.max_bytecode_size,
    ) {
        Ok(compiled_contract_class) => compiled_contract_class,
        Err(err) => {
            return RunResult::CompilationFailure(CompilationFailure { class_hash, err });
        }
    };
    let old = sierra_class.compiled_class_hash;
    let new = BigUintAsHex { value: compiled_contract_class.compiled_class_hash().to_biguint() };
    if old != new {
        RunResult::CompilationMismatch(CompilationMismatch { class_hash, old, new })
    } else {
        RunResult::Success
    }
}
