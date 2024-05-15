use std::fmt::Write;
use std::io::{BufReader, Read, Seek};
use std::sync::Arc;

use anyhow::Context;
use cairo_lang_starknet_classes::allowed_libfuncs::{AllowedLibfuncsError, ListSelector};
use cairo_lang_starknet_classes::casm_contract_class::{
    CasmContractClass, StarknetSierraCompilationError,
};
use cairo_lang_starknet_classes::compiler_version::VersionId;
use cairo_lang_starknet_classes::contract_class::{ContractClass, ContractEntryPoints};
use cairo_lang_utils::bigint::BigUintAsHex;
use clap::Parser;
use indicatif::{MultiProgress, ProgressBar, ProgressState, ProgressStyle};
use serde::Deserialize;
use tokio::task::JoinSet;

/// Runs validation on existing contract classes to make sure they are still valid and compilable.
#[derive(Parser, Debug)]
#[clap(version, verbatim_doc_comment)]
struct Args {
    /// The input files with declared classes info.
    input_files: Vec<String>,
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

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let args: Args = Args::parse();
    let list_selector =
        ListSelector::new(args.allowed_libfuncs_list_name, args.allowed_libfuncs_list_file)
            .expect("Both allowed libfunc list name and file were supplied.");
    let override_version = match args.override_version {
        Some(version) => Some(parse_version_id(&version)?),
        None => None,
    };

    // Setting up the progress bars.
    let multi_bar = MultiProgress::new();
    let reader_bar = multi_bar.add(ProgressBar::new(0));
    reader_bar.set_style(get_style("Read input"));
    let class_bar = multi_bar.add(ProgressBar::new(0));
    class_bar.set_style(get_style("Process classes"));

    // Setting up results of class recompilation.
    let (results_tx, results_rx) = async_channel::bounded(128);
    let results_handler = {
        let class_bar = class_bar.clone();
        tokio::spawn(async move { collect_result(results_rx, class_bar).await })
    };

    let config: Arc<RunConfig> = Arc::new(RunConfig {
        list_selector,
        override_version,
        max_bytecode_size: args.max_bytecode_size,
    });
    let (classes_tx, classes_rx) = async_channel::bounded(256);
    let mut input_readers =
        spawn_input_file_readers(args.input_files, classes_tx, reader_bar.clone());
    spawn_class_processors(classes_rx, results_tx, class_bar.clone(), config);
    let mut num_of_classes = 0;
    while let Some(file_num_of_classes) = input_readers.join_next().await {
        num_of_classes += file_num_of_classes.with_context(|| "Failed to join file reader.")??;
    }
    reader_bar.finish_and_clear();
    let report = results_handler.await.with_context(|| "Failed to collect results.")?;
    class_bar.finish_and_clear();
    analyze_report(report, num_of_classes)
}

/// Returns a progress style with the given name.
fn get_style(name: &str) -> ProgressStyle {
    ProgressStyle::with_template(&format!(
        "{name}: {}",
        "{spinner:.green} [{elapsed_precise}] [{wide_bar:.cyan/blue}] {pos:>6}/{len:6} ({eta})"
    ))
    .unwrap()
    .with_key("eta", |state: &ProgressState, w: &mut dyn Write| {
        write!(w, "{:.1}s", state.eta().as_secs_f64()).unwrap()
    })
    .progress_chars("#>-")
}

/// Spawns tasks that read the input files and send the classes to the classes channel.
fn spawn_input_file_readers(
    input_files: Vec<String>,
    classes_tx: async_channel::Sender<ContractClassInfo>,
    reader_bar: ProgressBar,
) -> JoinSet<anyhow::Result<usize>> {
    let mut readers_set = JoinSet::new();
    for input_file in input_files {
        let classes_tx = classes_tx.clone();
        let reader_bar = reader_bar.clone();
        readers_set.spawn(async move {
            handle_classes_input_file(&input_file, classes_tx, reader_bar).await
        });
    }
    readers_set
}

/// Reads the classes from an input file and sends them to the classes channel.
async fn handle_classes_input_file(
    input_path: &str,
    classes_tx: async_channel::Sender<ContractClassInfo>,
    reader_bar: ProgressBar,
) -> anyhow::Result<usize> {
    // Reading the contract classes from the file.
    let mut reader = BufReader::new(
        std::fs::File::open(input_path).with_context(|| format!("Failed to open {input_path}."))?,
    );
    reader_bar.inc_length(reader.get_ref().metadata().unwrap().len());
    let mut prev_position = 0;
    let mut num_of_classes = 0;
    loop {
        let mut next_byte: u8 = b'\0';
        reader
            .read_exact(std::slice::from_mut(&mut next_byte))
            .with_context(|| "Failed to read next byte.")?;
        match next_byte {
            b'[' | b',' => {}
            b' ' | b'\n' => continue,
            b']' => break,
            _ => return Err(anyhow::anyhow!("Invalid header")),
        }
        let sierra_class = serde_json::Deserializer::from_reader(&mut reader)
            .into_iter::<ContractClassInfo>()
            .next()
            .unwrap()
            .with_context(|| "deserialization Failed.")?;
        num_of_classes += 1;
        classes_tx.send(sierra_class).await?;
        let new_position = reader.stream_position().unwrap();
        reader_bar.inc(new_position - prev_position);
        prev_position = new_position;
    }
    Ok(num_of_classes)
}

/// Spawns tasks that process the classes and send the results to the results channel.
fn spawn_class_processors(
    classes_rx: async_channel::Receiver<ContractClassInfo>,
    results_tx: async_channel::Sender<RunResult>,
    class_bar: ProgressBar,
    config: Arc<RunConfig>,
) {
    const NUM_OF_PROCESSORS: usize = 32;
    for _ in 0..NUM_OF_PROCESSORS {
        let class_bar = class_bar.clone();
        let classes_rx = classes_rx.clone();
        let results_tx = results_tx.clone();
        let config = config.clone();
        tokio::spawn(async move {
            while let Ok(sierra_class) = classes_rx.recv().await {
                class_bar.inc_length(1);
                if let Err(err) = results_tx.send(run_single(sierra_class, config.as_ref())).await {
                    eprintln!("Failed to send result: {:#?}", err);
                }
                // Additional yield to prevent starvation of the inputs handling stage.
                tokio::task::yield_now().await;
            }
        });
    }
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

/// Spawns a task that collects the results from the results channel and returns the report.
async fn collect_result(
    results_rx: async_channel::Receiver<RunResult>,
    class_bar: ProgressBar,
) -> Report {
    let mut report = Report {
        validation_failures: Vec::new(),
        compilation_failures: Vec::new(),
        compilation_mismatch: Vec::new(),
    };
    while let Ok(result) = results_rx.recv().await {
        class_bar.inc(1);
        match result {
            RunResult::ValidationFailure(failure) => {
                report.validation_failures.push(failure);
            }
            RunResult::CompilationFailure(failure) => {
                report.compilation_failures.push(failure);
            }
            RunResult::CompilationMismatch(mismatch) => {
                report.compilation_mismatch.push(mismatch);
            }
            RunResult::Success => {}
        };
    }
    class_bar.finish_and_clear();
    report
}

/// Analyzes the report and prints the results.
fn analyze_report(report: Report, num_of_classes: usize) -> anyhow::Result<()> {
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
    if report.validation_failures.is_empty()
        && report.compilation_failures.is_empty()
        && report.compilation_mismatch.is_empty()
    {
        println!("All {} classes passed validation and compilation.", num_of_classes);
        Ok(())
    } else {
        Err(anyhow::anyhow!("Failed."))
    }
}
