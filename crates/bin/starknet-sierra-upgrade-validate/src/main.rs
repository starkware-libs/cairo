use std::fmt::Write;
use std::io::{BufReader, Read, Seek};
use std::sync::Arc;
use std::sync::atomic::{AtomicU64, Ordering};

use anyhow::Context;
use cairo_lang_starknet_classes::allowed_libfuncs::{AllowedLibfuncsError, ListSelector};
use cairo_lang_starknet_classes::casm_contract_class::{
    CasmContractClass, StarknetSierraCompilationError,
};
use cairo_lang_starknet_classes::compiler_version::VersionId;
use cairo_lang_starknet_classes::contract_class::{ContractClass, ContractEntryPoints};
use cairo_lang_utils::bigint::BigUintAsHex;
use clap::{Parser, arg};
use indicatif::{MultiProgress, ProgressBar, ProgressState, ProgressStyle};
use serde::de::DeserializeOwned;
use serde::{Deserialize, Serialize};

const NUM_OF_PROCESSORS: usize = 32;

/// Runs validation on existing contract classes to make sure they are still valid and
/// compilable.
/// Gets one of two different inputs:
/// 1. If fullnode_url is provided, it reads the contract classes from the fullnode.
/// 2. If fullnode_url is not provided, it reads the contract classes from the input files. Outputs
///    one of two different outputs:
/// 1. If class_info_output_file is provided, it writes the classes to the file.
/// 2. If class_info_output_file is not provided, it returns the report of the runs over the
///    processes of the classes.
#[derive(Parser, Debug)]
#[clap(version, verbatim_doc_comment)]
struct Cli {
    /// The input files with declared classes info.
    #[arg(
        required_unless_present = "fullnode_url",
        conflicts_with_all = ["fullnode_url", "FullnodeArgs"],
    )]
    input_files: Vec<String>,
    /// The allowed libfuncs list to use (default: most recent audited list).
    #[arg(long, conflicts_with = "allowed_libfuncs_list_file")]
    allowed_libfuncs_list_name: Option<String>,
    /// A file of the allowed libfuncs list to use.
    #[arg(long, conflicts_with = "allowed_libfuncs_list_name")]
    allowed_libfuncs_list_file: Option<String>,
    /// Sierra version to override to prior to compilation.
    #[arg(long)]
    override_version: Option<String>,
    /// The max bytecode size.
    #[arg(long, default_value_t = 180000)]
    max_bytecode_size: usize,
    /// The url of the rpc server, if provided - Sierra classes would be read from it, and no input
    /// files should be provided.
    #[arg(
        long,
        requires_all = &["FullnodeArgs"], 
        required_unless_present = "input_files", 
        conflicts_with = "input_files"
    )]
    fullnode_url: Option<String>,
    #[clap(flatten)]
    fullnode_args: Option<FullnodeArgs>,
    /// The output file to write the Sierra classes into.
    #[arg(long)]
    class_info_output_file: Option<String>,
}

#[derive(Parser, Debug)]
#[command(group = clap::ArgGroup::new("range").multiple(true).conflicts_with("last_n_blocks"))]
struct FullnodeArgs {
    /// The start block of the declared Sierra classes to test.
    #[arg(long, group = "range", requires = "end_block")]
    start_block: Option<u64>,
    /// The end block of the declared Sierra classes to test.
    #[arg(long, group = "range", requires = "start_block")]
    end_block: Option<u64>,
    /// The number of last n blocks to test.
    #[arg(long, conflicts_with = "range")]
    last_n_blocks: Option<u64>,
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
#[derive(Serialize, Deserialize)]
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
    /// The number of processed classes.
    num_of_classes: usize,
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
    let args = Cli::parse();
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
    reader_bar.set_style(get_style(if args.fullnode_url.is_none() {
        "Read input"
    } else {
        "Process blocks"
    }));
    let classes_bar = multi_bar.add(ProgressBar::new(0));
    classes_bar.set_style(get_style("Process classes"));

    let (classes_tx, classes_rx) = async_channel::bounded(256);
    let config: Arc<RunConfig> = Arc::new(RunConfig {
        list_selector,
        override_version,
        max_bytecode_size: args.max_bytecode_size,
    });

    // If fullnode_url is provided, we retrieve the contract class info from the fullnode.
    // Otherwise, we read the contract class info from the input files.
    if let Some(fullnode_url) = args.fullnode_url {
        let client = FullNodeClient::new(fullnode_url.clone());
        let max_end_block = client.post::<_, u64>("starknet_blockNumber", ()).await? + 1;
        let fullnode_args = args.fullnode_args.unwrap();
        let (start_block, end_block) = if let Some(last_n_blocks) = fullnode_args.last_n_blocks {
            let end_block = max_end_block;
            let start_block = end_block - last_n_blocks;
            (start_block, end_block)
        } else {
            let start_block =
                fullnode_args.start_block.with_context(|| "no start block provided")?;
            let mut end_block = fullnode_args.end_block.with_context(|| "no end block provided")?;
            if max_end_block < end_block {
                eprintln!(
                    "provided `end-block` {end_block} is greater than the max possible block \
                     {max_end_block}. Setting `end-block` to {max_end_block}."
                );
                end_block = max_end_block;
            }
            (start_block, end_block)
        };
        let (class_hashes_tx, class_hashes_rx) = async_channel::bounded(128);
        spawn_block_class_hashes_retrievers(
            &client,
            start_block,
            end_block,
            class_hashes_tx,
            reader_bar.clone(),
            classes_bar.clone(),
        );
        spawn_classes_from_class_hashes(&client, class_hashes_rx, classes_tx);
    } else {
        spawn_input_file_readers(
            args.input_files,
            classes_tx,
            reader_bar.clone(),
            classes_bar.clone(),
        );
    }

    // If class_info_output_file is provided, dump all contract class infos into the file.
    // Otherwise, generate a report for the classes analysis.
    if let Some(class_info_output_file) = args.class_info_output_file {
        dump_class_infos_into_json(classes_rx, classes_bar, &class_info_output_file).await
    } else {
        let (results_tx, results_rx) = async_channel::bounded(128);
        let results_handler = {
            let classes_bar = classes_bar.clone();
            tokio::spawn(async move { collect_result(results_rx, classes_bar).await })
        };

        spawn_class_processors(classes_rx, results_tx, config);

        let report = results_handler.await.with_context(|| "Failed to collect results.")?;
        reader_bar.finish_and_clear();
        classes_bar.finish_and_clear();
        analyze_report(report)
    }
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
    classes_bar: ProgressBar,
) {
    for input_file in input_files {
        let classes_tx = classes_tx.clone();
        let reader_bar = reader_bar.clone();
        let classes_bar = classes_bar.clone();
        tokio::spawn(async move {
            handle_classes_input_file(&input_file, classes_tx, reader_bar, classes_bar).await
        });
    }
}

/// Reads the classes from an input file and sends them to the classes channel.
async fn handle_classes_input_file(
    input_path: &str,
    classes_tx: async_channel::Sender<ContractClassInfo>,
    reader_bar: ProgressBar,
    classes_bar: ProgressBar,
) -> anyhow::Result<()> {
    // Reading the contract classes from the file.
    let mut reader = BufReader::new(
        std::fs::File::open(input_path).with_context(|| format!("Failed to open {input_path}."))?,
    );
    reader_bar.inc_length(reader.get_ref().metadata().unwrap().len());
    let mut prev_position = 0;
    loop {
        // Handling the encompassing characters of the array holding the classes infos, as the
        // `serde_json::Deserializer` can only consume a full json object.
        // This allows us to not read the entire json file into memory.
        let mut next_byte: u8 = b'\0';
        reader
            .read_exact(std::slice::from_mut(&mut next_byte))
            .with_context(|| "Failed to read next byte.")?;
        match next_byte {
            // This is the start, or the separator between classes, so we can just handle the
            // following class.
            b'[' | b',' => {}
            // This is a whitespace, so we can see what happens in the next byte and decide what to
            // do.
            b' ' | b'\n' => continue,
            // Handling of the array is done.
            b']' => break,
            _ => return Err(anyhow::anyhow!("Invalid header")),
        }
        let sierra_class = serde_json::Deserializer::from_reader(&mut reader)
            .into_iter::<ContractClassInfo>()
            .next()
            .unwrap()
            .with_context(|| "deserialization Failed.")?;
        classes_tx.send(sierra_class).await?;
        classes_bar.inc_length(1);
        let new_position = reader.stream_position().unwrap();
        reader_bar.inc(new_position - prev_position);
        prev_position = new_position;
    }
    Ok(())
}

/// Spawns tasks that process the classes and send the results to the results channel.
fn spawn_class_processors(
    classes_rx: async_channel::Receiver<ContractClassInfo>,
    results_tx: async_channel::Sender<RunResult>,
    config: Arc<RunConfig>,
) {
    for _ in 0..NUM_OF_PROCESSORS {
        let classes_rx = classes_rx.clone();
        let results_tx = results_tx.clone();
        let config = config.clone();
        tokio::spawn(async move {
            while let Ok(sierra_class) = classes_rx.recv().await {
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
    classes_bar: ProgressBar,
) -> Report {
    let mut report = Report {
        validation_failures: Vec::new(),
        compilation_failures: Vec::new(),
        compilation_mismatch: Vec::new(),
        num_of_classes: 0,
    };
    while let Ok(result) = results_rx.recv().await {
        classes_bar.inc(1);
        report.num_of_classes += 1;
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
    classes_bar.finish_and_clear();
    report
}

/// Analyzes the report and prints the results.
fn analyze_report(
    Report { validation_failures, compilation_failures, compilation_mismatch, num_of_classes }: Report,
) -> anyhow::Result<()> {
    if !validation_failures.is_empty() {
        println!("Validation failures: (Printing first 10 out of {})", validation_failures.len());
        for ValidationFailure { class_hash, err } in validation_failures.iter().take(10) {
            println!("Validation failure for {:#x}: {err}", class_hash.value);
        }
    }
    if !compilation_failures.is_empty() {
        println!("Compilation failures: (Printing first 10 out of {})", compilation_failures.len());
        for CompilationFailure { class_hash, err } in compilation_failures.iter().take(10) {
            println!("Compilation failure for {:#x}: {err}", class_hash.value);
        }
    }
    if !compilation_mismatch.is_empty() {
        println!(
            "Compilation mismatch {} out of {num_of_classes}: (Printing first 10)",
            compilation_mismatch.len()
        );
        for CompilationMismatch { class_hash, old, new } in compilation_mismatch.iter().take(10) {
            println!(
                "Compilation mismatch for {:#x}: old={:#x}, new={:#x}",
                class_hash.value, old.value, new.value
            );
        }
    }
    if validation_failures.is_empty()
        && compilation_failures.is_empty()
        && compilation_mismatch.is_empty()
    {
        println!("All {} classes passed validation and compilation.", num_of_classes);
        Ok(())
    } else {
        Err(anyhow::anyhow!("Failed."))
    }
}

#[derive(Serialize)]
struct BlockId {
    pub block_number: u64,
}
#[derive(Serialize)]
struct GetStateUpdateRequest {
    pub block_id: BlockId,
}
#[derive(Serialize)]
struct GetStateClassRequest {
    pub block_id: String,
    pub class_hash: BigUintAsHex,
}
#[derive(Deserialize, Debug)]
pub struct GetStateUpdateResponse {
    pub state_diff: StateDiff,
}
#[derive(Deserialize, Debug)]
pub struct GetStateClassResponse {
    pub entry_points_by_type: ContractEntryPoints,
    pub sierra_program: Vec<BigUintAsHex>,
}
#[derive(Deserialize, Debug)]
pub struct StateDiff {
    pub declared_classes: Vec<ClassHashes>,
}
#[derive(Debug, Clone, Eq, PartialEq, Deserialize, Serialize)]
pub struct ClassHashes {
    pub class_hash: BigUintAsHex,
    pub compiled_class_hash: BigUintAsHex,
}

/// Given a block number, retrieves the class hashes and compiled class hashes from the state
/// update.
async fn retrieve_block_class_hashes(
    client: &FullNodeClient,
    block_number: u64,
    class_hashes_tx: &async_channel::Sender<ClassHashes>,
    class_hashes_bar: &ProgressBar,
) {
    if let Ok(response) = client
        .post::<_, GetStateUpdateResponse>(
            "starknet_getStateUpdate",
            GetStateUpdateRequest { block_id: BlockId { block_number } },
        )
        .await
    {
        class_hashes_bar.inc_length(response.state_diff.declared_classes.len() as u64);
        for class_hash in response.state_diff.declared_classes {
            class_hashes_tx.send(class_hash).await.unwrap();
        }
    }
}

/// Spawns tasks that retrieve the class hashes and compiled class hashes from the state update.
fn spawn_block_class_hashes_retrievers(
    client: &FullNodeClient,
    start_block: u64,
    end_block: u64,
    class_hashes_tx: async_channel::Sender<ClassHashes>,
    blocks_bar: ProgressBar,
    class_hashes_bar: ProgressBar,
) {
    blocks_bar.set_length(end_block - start_block);
    let block_number_global = Arc::new(AtomicU64::new(start_block));
    for _ in 0..NUM_OF_PROCESSORS {
        let class_hashes_tx = class_hashes_tx.clone();
        let block_number_global = block_number_global.clone();
        let client = client.clone();
        let blocks_bar = blocks_bar.clone();
        let class_hashes_bar = class_hashes_bar.clone();
        tokio::spawn(async move {
            loop {
                let block_number = block_number_global.fetch_add(1, Ordering::SeqCst);
                if block_number >= end_block {
                    break;
                }
                retrieve_block_class_hashes(
                    &client,
                    block_number,
                    &class_hashes_tx,
                    &class_hashes_bar,
                )
                .await;
                blocks_bar.inc(1);
            }
            blocks_bar.finish_and_clear();
        });
    }
}

/// Given a class hash, retrieves the matching ContractClassInfo.
async fn retrieve_class_from_class_hash(
    client: &FullNodeClient,
    class_hashes: ClassHashes,
    classes_tx: &async_channel::Sender<ContractClassInfo>,
) {
    if let Ok(response) = client
        .post::<_, GetStateClassResponse>(
            "starknet_getClass",
            GetStateClassRequest {
                block_id: "latest".to_string(),
                class_hash: class_hashes.class_hash.clone(),
            },
        )
        .await
    {
        classes_tx
            .send(ContractClassInfo {
                compiled_class_hash: class_hashes.compiled_class_hash,
                class_hash: class_hashes.class_hash,
                sierra_program: response.sierra_program,
                entry_points_by_type: response.entry_points_by_type,
            })
            .await
            .unwrap();
    }
}

/// Spawns tasks that retrieve ContractClassInfos from a class hash.
fn spawn_classes_from_class_hashes(
    client: &FullNodeClient,
    class_hashes_rx: async_channel::Receiver<ClassHashes>,
    classes_tx: async_channel::Sender<ContractClassInfo>,
) {
    for _ in 0..NUM_OF_PROCESSORS {
        let class_hashes_rx = class_hashes_rx.clone();
        let classes_tx = classes_tx.clone();
        let client = client.clone();
        tokio::spawn(async move {
            while let Ok(class_hashes) = class_hashes_rx.recv().await {
                retrieve_class_from_class_hash(&client, class_hashes, &classes_tx).await;
            }
        });
    }
}

/// Dumps the classes info from `classes_rx` to the provided file in json format.
async fn dump_class_infos_into_json(
    classes_rx: async_channel::Receiver<ContractClassInfo>,
    classes_bar: ProgressBar,
    class_info_output_file: &str,
) -> anyhow::Result<()> {
    let mut classes_info_data: Vec<ContractClassInfo> = vec![];
    while let Ok(class) = classes_rx.recv().await {
        classes_bar.inc(1);
        classes_info_data.push(class);
    }
    let res_string = serde_json::to_string(&classes_info_data).unwrap();
    std::fs::write(class_info_output_file, res_string).unwrap();
    classes_bar.finish_and_clear();
    Ok(())
}

/// The client for the fullnode.
#[derive(Clone)]
struct FullNodeClient {
    /// The client connection pool.
    client: reqwest::Client,
    /// The fullnode url.
    url: String,
}
impl FullNodeClient {
    /// Creates a new FullNodeClient.
    fn new(url: String) -> Self {
        Self { client: reqwest::Client::new(), url }
    }
    /// Sends a post request to the fullnode.
    async fn post<Request: Serialize, Response: DeserializeOwned>(
        &self,
        method: &str,
        params: Request,
    ) -> anyhow::Result<Response> {
        let res = self
            .client
            .post(&self.url)
            .header("Content-Type", "application/json")
            .json(&serde_json::json!({
                "jsonrpc": "2.0",
                "method": method,
                "params": params,
                "id": 1,
            }))
            .send()
            .await?;
        if !res.status().is_success() {
            return Err(anyhow::anyhow!("Request failed."));
        }
        Ok(res.json::<RpcResponse<Response>>().await?.result)
    }
}

/// The response from the fullnode rpc.
#[derive(Deserialize, Debug)]
struct RpcResponse<T> {
    pub result: T,
}
