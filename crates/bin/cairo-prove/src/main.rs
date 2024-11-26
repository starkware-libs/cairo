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
use clap::{Parser, ValueHint};
use num_bigint::BigInt;

/// Compiles a Cairo project and runs a function marked `#[runnable]`.
/// Exits with 1 if the compilation or run fails, otherwise 0.
#[derive(Parser, Debug)]
#[clap(version, verbatim_doc_comment)]
struct Args {
    /// The Cairo project path to compile and run.
    #[clap(value_parser, value_hint=ValueHint::AnyPath)]
    path: PathBuf,
    /// Whether path is a single file.
    #[arg(short, long)]
    single_file: bool,
    /// Allows the compilation to succeed with warnings.
    #[arg(long)]
    allow_warnings: bool,
    /// Path to the runnable function.
    #[arg(long)]
    runnable: Option<String>,

    /// Serialized arguments to the runnable function.
    #[arg(long, value_delimiter = ',')]
    args: Vec<BigInt>,
    /// Whether to print the outputs.
    #[arg(long, default_value_t = false)]
    print_outputs: bool,

    /// The resulting trace file.
    #[clap(long, value_parser, value_hint=ValueHint::FilePath)]
    trace_file: Option<PathBuf>,
    /// The resulting memory file.
    #[clap(long, value_parser, value_hint=ValueHint::FilePath)]
    memory_file: Option<PathBuf>,
    /// When using dynamic layout, its parameters must be specified through a layout params file.
    #[clap(long, default_value = "plain", value_enum)]
    layout: LayoutName,
    /// Required when using with dynamic layout.
    /// Ignored otherwise.
    // TODO(orizi): Actually use this input when updating to the new VM version.
    #[clap(long, required_if_eq("layout", "dynamic"))]
    cairo_layout_params_file: Option<PathBuf>,
    /// If set, the program will be run in proof mode.
    #[clap(long)]
    proof_mode: bool,
    /// If set, the program will be run in secure mode.
    #[clap(long)]
    secure_run: Option<bool>,
    /// The resulting AIR public input file.
    #[clap(long, requires = "proof_mode")]
    air_public_input: Option<PathBuf>,
    /// The resulting AIR private input file.
    #[clap(long, requires_all = ["proof_mode", "trace_file", "memory_file"])]
    air_private_input: Option<PathBuf>,
    /// The resulting cairo PIE file.
    #[clap(long, conflicts_with_all = ["proof_mode", "air_private_input", "air_public_input"])]
    cairo_pie_output: Option<PathBuf>,
    /// Can we allow for missing builtins.
    #[clap(long)]
    allow_missing_builtins: Option<bool>,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    // Check if args.path is a file or a directory.
    check_compiler_path(args.single_file, &args.path)?;

    let mut reporter = DiagnosticsReporter::stderr();
    if args.allow_warnings {
        reporter = reporter.allow_warnings();
    }
    let runnable = Runnable::new(compile_runnable(&args.path, args.runnable.as_deref(), reporter)?);
    let data =
        runnable.program.bytecode.iter().map(Felt252::from).map(MaybeRelocatable::from).collect();
    let (hints, string_to_hint) = build_hints_dict(&runnable.program.hints);
    let program = if args.proof_mode {
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
        user_args: vec![vec![Arg::Array(args.args.iter().map(|v| Arg::Value(v.into())).collect())]],
        string_to_hint,
        starknet_state: Default::default(),
        run_resources: Default::default(),
        syscalls_used_resources: Default::default(),
        no_temporary_segments: false,
    };

    let trace_enabled = args.trace_file.is_some() || args.air_public_input.is_some();

    let cairo_run_config = CairoRunConfig {
        trace_enabled,
        relocate_mem: args.memory_file.is_some() || args.air_public_input.is_some(),
        layout: args.layout,
        proof_mode: args.proof_mode,
        secure_run: args.secure_run,
        allow_missing_builtins: args.allow_missing_builtins,
        ..Default::default()
    };

    let mut runner = cairo_run_program(&program, &cairo_run_config, &mut hint_processor)
        .with_context(|| "Failed running program.")?;
    if args.print_outputs {
        let mut output_buffer = "Program Output:\n".to_string();
        runner.vm.write_output(&mut output_buffer)?;
        print!("{output_buffer}");
    }

    if let Some(trace_path) = &args.trace_file {
        let relocated_trace =
            runner.relocated_trace.as_ref().with_context(|| "Trace not relocated.")?;
        let mut writer = FileWriter::new(3 * 1024 * 1024, trace_path)?;
        cairo_run::write_encoded_trace(relocated_trace, &mut writer)?;
        writer.flush()?;
    }

    if let Some(memory_path) = &args.memory_file {
        let mut writer = FileWriter::new(5 * 1024 * 1024, memory_path)?;
        cairo_run::write_encoded_memory(&runner.relocated_memory, &mut writer)?;
        writer.flush()?;
    }

    if let Some(file_path) = args.air_public_input {
        let json = runner.get_air_public_input()?.serialize_json()?;
        std::fs::write(file_path, json)?;
    }

    if let (Some(file_path), Some(trace_file), Some(memory_file)) =
        (args.air_private_input, args.trace_file, args.memory_file)
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

    if let Some(file_name) = args.cairo_pie_output {
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
