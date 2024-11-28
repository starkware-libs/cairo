use std::io::{self, Write};
use std::path::PathBuf;

use anyhow::Context;
use bincode::enc::write::Writer;
use cairo_lang_compiler::diagnostics::DiagnosticsReporter;
use cairo_lang_compiler::project::check_compiler_path;
use cairo_lang_runnable::compile::compile_runnable;
use cairo_lang_runnable::runnable::{EntryPointKind, Runnable};
use cairo_lang_runner::{Arg, CairoHintProcessor, build_hints_dict};
use cairo_vm::cairo_run::{CairoRunConfig, cairo_run_program};
use cairo_vm::types::layout_name::LayoutName;
use cairo_vm::types::program::Program;
use cairo_vm::types::relocatable::MaybeRelocatable;
use cairo_vm::{Felt252, cairo_run};
use clap::Parser;
use num_bigint::BigInt;

/// Compiles a Cairo project and runs a function marked `#[runnable]`.
/// Exits with 1 if the compilation or run fails, otherwise 0.
#[derive(Parser, Debug)]
#[clap(version, verbatim_doc_comment)]
struct Args {
    /// The Cairo project path to compile and run.
    path: PathBuf,

    /// Whether to only build and into the given path.
    #[clap(long)]
    build_only: Option<PathBuf>,

    /// Whether to only run a prebuilt executable.
    #[arg(long, default_value_t = false, conflicts_with = "build_only")]
    prebuilt: bool,

    #[clap(flatten)]
    build: BuildArgs,

    #[clap(flatten)]
    run: RunArgs,
}

#[derive(Parser, Debug)]
struct BuildArgs {
    /// Whether path is a single file.
    #[arg(long, conflicts_with = "prebuilt")]
    single_file: bool,
    /// Allows the compilation to succeed with warnings.
    #[arg(long, conflicts_with = "prebuilt")]
    allow_warnings: bool,
    /// Path to the runnable function.
    #[arg(long, conflicts_with = "prebuilt")]
    runnable: Option<String>,
}

#[derive(Parser, Debug)]
struct RunArgs {
    /// Serialized arguments to the runnable function.
    #[arg(long, value_delimiter = ',', conflicts_with = "build_only")]
    args: Vec<BigInt>,
    /// Whether to print the outputs.
    #[arg(long, default_value_t = false, conflicts_with = "build_only")]
    print_outputs: bool,

    /// When using dynamic layout, its parameters must be specified through a layout params file.
    #[clap(long, default_value = "plain", value_enum, conflicts_with = "build_only")]
    layout: LayoutName,
    /// Required when using with dynamic layout.
    /// Ignored otherwise.
    // TODO(orizi): Actually use this input when updating to the new VM version.
    #[clap(long, required_if_eq("layout", "dynamic"))]
    cairo_layout_params_file: Option<PathBuf>,
    /// If set, the program will be run in proof mode.
    #[clap(long, default_value_t = false, conflicts_with = "build_only", requires_all=["trace_file", "memory_file", "air_public_input", "air_private_input"])]
    proof_mode: bool,
    /// If set, the program will be run in secure mode.
    #[clap(long, conflicts_with = "build_only")]
    secure_run: Option<bool>,
    /// The resulting cairo PIE file.
    #[clap(long, conflicts_with_all = ["proof_mode", "build_only"])]
    cairo_pie_output: Option<PathBuf>,
    /// Can we allow for missing builtins.
    #[clap(long)]
    allow_missing_builtins: Option<bool>,
    #[clap(flatten)]
    proof: ProofModeArgs,
}

#[derive(Parser, Debug)]
struct ProofModeArgs {
    /// The resulting trace file.
    #[clap(long, conflicts_with = "build_only", requires = "proof_mode")]
    trace_file: Option<PathBuf>,
    /// The resulting memory file.
    #[clap(long, conflicts_with = "build_only", requires = "proof_mode")]
    memory_file: Option<PathBuf>,
    /// The resulting AIR public input file.
    #[clap(long, conflicts_with = "build_only", requires = "proof_mode")]
    air_public_input: Option<PathBuf>,
    /// The resulting AIR private input file.
    #[clap(long, conflicts_with = "build_only", requires = "proof_mode")]
    air_private_input: Option<PathBuf>,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    let runnable = {
        if args.prebuilt {
            serde_json::from_reader(std::fs::File::open(&args.path)?)
                .with_context(|| "Failed reading prebuilt runnable.")?
        } else {
            // Check if args.path is a file or a directory.
            check_compiler_path(args.build.single_file, &args.path)?;
            let mut reporter = DiagnosticsReporter::stderr();
            if args.build.allow_warnings {
                reporter = reporter.allow_warnings();
            }
            Runnable::new(compile_runnable(&args.path, args.build.runnable.as_deref(), reporter)?)
        }
    };
    if let Some(path) = &args.build_only {
        serde_json::to_writer(std::fs::File::create(path)?, &runnable)
            .with_context(|| "Failed writing runnable.")?;
        return Ok(());
    }

    let data =
        runnable.program.bytecode.iter().map(Felt252::from).map(MaybeRelocatable::from).collect();
    let (hints, string_to_hint) = build_hints_dict(&runnable.program.hints);
    let program = if args.run.proof_mode {
        let entrypoint = runnable
            .entrypoints
            .iter()
            .find(|e| matches!(e.kind, EntryPointKind::NonReturning))
            .with_context(|| "No function entrypoint found.")?;
        Program::new_for_proof(
            entrypoint.builtins.clone(),
            data,
            entrypoint.offset,
            entrypoint.offset + 4,
            hints,
            Default::default(),
            Default::default(),
            vec![],
            None,
        )
    } else {
        let entrypoint = runnable
            .entrypoints
            .iter()
            .find(|e| matches!(e.kind, EntryPointKind::Function))
            .with_context(|| "No function entrypoint found.")?;
        Program::new(
            entrypoint.builtins.clone(),
            data,
            Some(entrypoint.offset),
            hints,
            Default::default(),
            Default::default(),
            vec![],
            None,
        )
    }
    .with_context(|| "Failed setting up program.")?;
    let mut hint_processor = CairoHintProcessor {
        runner: None,
        user_args: vec![vec![Arg::Array(
            args.run.args.iter().map(|v| Arg::Value(v.into())).collect(),
        )]],
        string_to_hint,
        starknet_state: Default::default(),
        run_resources: Default::default(),
        syscalls_used_resources: Default::default(),
        no_temporary_segments: false,
    };

    let cairo_run_config = CairoRunConfig {
        trace_enabled: args.run.proof_mode,
        relocate_mem: args.run.proof_mode,
        layout: args.run.layout,
        proof_mode: args.run.proof_mode,
        secure_run: args.run.secure_run,
        allow_missing_builtins: args.run.allow_missing_builtins,
        ..Default::default()
    };

    let mut runner = cairo_run_program(&program, &cairo_run_config, &mut hint_processor)
        .with_context(|| "Failed running program.")?;
    if args.run.print_outputs {
        let mut output_buffer = "Program Output:\n".to_string();
        runner.vm.write_output(&mut output_buffer)?;
        print!("{output_buffer}");
    }

    if let Some(trace_path) = &args.run.proof.trace_file {
        let relocated_trace =
            runner.relocated_trace.as_ref().with_context(|| "Trace not relocated.")?;
        let mut writer = FileWriter::new(3 * 1024 * 1024, trace_path)?;
        cairo_run::write_encoded_trace(relocated_trace, &mut writer)?;
        writer.flush()?;
    }

    if let Some(memory_path) = &args.run.proof.memory_file {
        let mut writer = FileWriter::new(5 * 1024 * 1024, memory_path)?;
        cairo_run::write_encoded_memory(&runner.relocated_memory, &mut writer)?;
        writer.flush()?;
    }

    if let Some(file_path) = args.run.proof.air_public_input {
        let json = runner.get_air_public_input()?.serialize_json()?;
        std::fs::write(file_path, json)?;
    }

    if let (Some(file_path), Some(trace_file), Some(memory_file)) =
        (args.run.proof.air_private_input, args.run.proof.trace_file, args.run.proof.memory_file)
    {
        let absolute = |path_buf: PathBuf| {
            path_buf.as_path().canonicalize().unwrap_or(path_buf).to_string_lossy().to_string()
        };
        let json = runner
            .get_air_private_input()
            .to_serializable(absolute(trace_file), absolute(memory_file))
            .serialize_json()
            .with_context(|| "Failed serializing private input")?;
        std::fs::write(file_path, json)?;
    }

    if let Some(file_name) = args.run.cairo_pie_output {
        runner
            .get_cairo_pie()
            .with_context(|| "Failed getting cairo pie")?
            .write_zip_file(&file_name)?
    }

    Ok(())
}

/// Writer implementation for a file.
struct FileWriter {
    buf_writer: io::BufWriter<std::fs::File>,
    bytes_written: usize,
}

impl Writer for FileWriter {
    fn write(&mut self, bytes: &[u8]) -> Result<(), bincode::error::EncodeError> {
        self.buf_writer
            .write_all(bytes)
            .map_err(|e| bincode::error::EncodeError::Io { inner: e, index: self.bytes_written })?;

        self.bytes_written += bytes.len();

        Ok(())
    }
}

impl FileWriter {
    /// Create a new instance of `FileWriter` with the given file path.
    fn new(capacity: usize, path: &PathBuf) -> anyhow::Result<Self> {
        Ok(Self {
            buf_writer: io::BufWriter::with_capacity(capacity, std::fs::File::create(path)?),
            bytes_written: 0,
        })
    }

    /// Flush the writer.
    ///
    /// Would automatically be called when the writer is dropped, but errors are ignored in that
    /// case.
    fn flush(&mut self) -> io::Result<()> {
        self.buf_writer.flush()
    }
}
