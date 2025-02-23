use std::io::{self, Write};
use std::path::PathBuf;

use anyhow::Context;
use bincode::enc::write::Writer;
use cairo_lang_compiler::diagnostics::DiagnosticsReporter;
use cairo_lang_compiler::project::check_compiler_path;
use cairo_lang_executable::compile::{ExecutableConfig, compile_executable};
use cairo_lang_executable::executable::{EntryPointKind, Executable};
use cairo_lang_runner::casm_run::format_for_panic;
use cairo_lang_runner::{Arg, CairoHintProcessor, build_hints_dict};
use cairo_lang_utils::bigint::BigUintAsHex;
use cairo_vm::cairo_run::{CairoRunConfig, cairo_run_program};
use cairo_vm::types::layout_name::LayoutName;
use cairo_vm::types::program::Program;
use cairo_vm::types::relocatable::MaybeRelocatable;
use cairo_vm::{Felt252, cairo_run};
use clap::Parser;
use num_bigint::BigInt;

/// Compiles a Cairo project and runs a function marked `#[executable]`.
/// Exits with 1 if the compilation or run fails, otherwise 0.
#[derive(Parser, Debug)]
#[clap(version, verbatim_doc_comment)]
struct Args {
    /// The basic input path for the run.
    /// If `--prebuilt` is provided, this is the path to the prebuilt executable.
    /// Else, if `--single-file` is provided, this is the path to a single cairo file to compile
    /// for execution. Otherwise, this is the path of the Cairo project - the directory
    /// containing the `cairo_project.toml` file.
    input_path: PathBuf,

    /// Whether to only build and save into the given path.
    #[clap(long, requires = "output_path")]
    build_only: bool,

    /// The path to save the result of the run.
    ///
    /// In `--build-only` this would be the executable artifact.
    /// In bootloader mode it will be the resulting cairo PIE file.
    /// In standalone mode this parameter is disallowed.
    #[clap(long, required_unless_present("standalone"))]
    output_path: Option<PathBuf>,

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
    /// Allow warnings and don't print them (implies allow_warnings).
    #[arg(long, conflicts_with = "prebuilt")]
    ignore_warnings: bool,
    /// Allow syscalls in the program.
    #[arg(long, conflicts_with = "prebuilt")]
    allow_syscalls: bool,
    /// The path to the executable function.
    ///
    /// Not required if there is only a single executable function in the project.
    /// If not provided but required, a list of the available executable functions is printed.
    #[arg(long, conflicts_with = "prebuilt")]
    executable: Option<String>,
}

#[derive(Parser, Debug)]
struct RunArgs {
    /// Serialized arguments to the executable function.
    #[clap(flatten)]
    args: SerializedArgs,
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
    /// If set, the program will be run in standalone mode.
    #[clap(
        long,
        default_value_t = false,
        conflicts_with_all = ["build_only", "output_path"],
        requires_all=["air_public_input", "air_private_input"],
    )]
    standalone: bool,
    /// If set, the program will be run in secure mode.
    #[clap(long, conflicts_with = "build_only")]
    secure_run: Option<bool>,
    /// Can we allow for missing builtins.
    #[clap(long)]
    allow_missing_builtins: Option<bool>,
    #[clap(flatten)]
    proof_outputs: ProofOutputArgs,
}

#[derive(Parser, Debug)]
#[command(group = clap::ArgGroup::new("serialized-args").multiple(false).conflicts_with("build_only"))]
struct SerializedArgs {
    /// Serialized arguments to the executable function as a list.
    #[arg(
        long = "args",
        group = "serialized-args",
        value_delimiter = ',',
        conflicts_with = "build_only"
    )]
    as_list: Vec<BigInt>,
    /// Serialized arguments to the executable function from a file.
    #[arg(long = "args-file", group = "serialized-args", conflicts_with = "build_only")]
    as_file: Option<PathBuf>,
}

/// The arguments for output files required for creating a proof.
#[derive(Parser, Debug)]
struct ProofOutputArgs {
    /// The resulting trace file.
    #[clap(long, conflicts_with = "build_only")]
    trace_file: Option<PathBuf>,
    /// The resulting memory file.
    #[clap(long, conflicts_with = "build_only")]
    memory_file: Option<PathBuf>,
    /// The resulting AIR public input file.
    #[clap(long, conflicts_with = "build_only")]
    air_public_input: Option<PathBuf>,
    /// The resulting AIR private input file.
    #[clap(long, conflicts_with = "build_only", requires_all=["trace_file", "memory_file"])]
    air_private_input: Option<PathBuf>,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    let executable = {
        if args.prebuilt {
            serde_json::from_reader(std::fs::File::open(&args.input_path)?)
                .with_context(|| "Failed reading prebuilt executable.")?
        } else {
            // Check if args.path is a file or a directory.
            check_compiler_path(args.build.single_file, &args.input_path)?;
            let mut reporter = DiagnosticsReporter::stderr();
            if args.build.allow_warnings {
                reporter = reporter.allow_warnings();
            }
            if args.build.ignore_warnings {
                reporter = reporter.ignore_all_warnings();
            }

            Executable::new(compile_executable(
                &args.input_path,
                args.build.executable.as_deref(),
                reporter,
                ExecutableConfig { allow_syscalls: args.build.allow_syscalls },
            )?)
        }
    };
    if args.build_only {
        let path = args.output_path.with_context(|| "No output path provided.")?;
        serde_json::to_writer(std::fs::File::create(path)?, &executable)
            .with_context(|| "Failed writing executable.")?;
        return Ok(());
    }

    let data =
        executable.program.bytecode.iter().map(Felt252::from).map(MaybeRelocatable::from).collect();
    let (hints, string_to_hint) = build_hints_dict(&executable.program.hints);
    let program = if args.run.standalone {
        let entrypoint = executable
            .entrypoints
            .iter()
            .find(|e| matches!(e.kind, EntryPointKind::Standalone))
            .with_context(|| "No `Standalone` entrypoint found.")?;
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
        let entrypoint = executable
            .entrypoints
            .iter()
            .find(|e| matches!(e.kind, EntryPointKind::Bootloader))
            .with_context(|| "No `Bootloader` entrypoint found.")?;
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

    let user_args = if let Some(path) = args.run.args.as_file {
        let as_vec: Vec<BigUintAsHex> = serde_json::from_reader(std::fs::File::open(&path)?)
            .with_context(|| "Failed reading args file.")?;
        as_vec.into_iter().map(|v| Arg::Value(v.value.into())).collect()
    } else {
        args.run.args.as_list.iter().map(|v| Arg::Value(v.into())).collect()
    };

    let mut hint_processor = CairoHintProcessor {
        runner: None,
        user_args: vec![vec![Arg::Array(user_args)]],
        string_to_hint,
        starknet_state: Default::default(),
        run_resources: Default::default(),
        syscalls_used_resources: Default::default(),
        no_temporary_segments: false,
        markers: Default::default(),
    };

    let cairo_run_config = CairoRunConfig {
        trace_enabled: args.run.proof_outputs.trace_file.is_some(),
        relocate_mem: args.run.proof_outputs.memory_file.is_some(),
        layout: args.run.layout,
        proof_mode: args.run.standalone,
        secure_run: args.run.secure_run,
        allow_missing_builtins: args.run.allow_missing_builtins,
        ..Default::default()
    };

    let mut runner = cairo_run_program(&program, &cairo_run_config, &mut hint_processor)
        .with_context(|| {
            if let Some(panic_data) = hint_processor.markers.last() {
                format_for_panic(panic_data.iter().copied())
            } else {
                "Failed running program.".into()
            }
        })?;
    if args.run.print_outputs {
        let mut output_buffer = "Program Output:\n".to_string();
        runner.vm.write_output(&mut output_buffer)?;
        print!("{output_buffer}");
    }

    if let Some(trace_path) = &args.run.proof_outputs.trace_file {
        let relocated_trace =
            runner.relocated_trace.as_ref().with_context(|| "Trace not relocated.")?;
        let mut writer = FileWriter::new(3 * 1024 * 1024, trace_path)?;
        cairo_run::write_encoded_trace(relocated_trace, &mut writer)?;
        writer.flush()?;
    }

    if let Some(memory_path) = &args.run.proof_outputs.memory_file {
        let mut writer = FileWriter::new(5 * 1024 * 1024, memory_path)?;
        cairo_run::write_encoded_memory(&runner.relocated_memory, &mut writer)?;
        writer.flush()?;
    }

    if let Some(file_path) = args.run.proof_outputs.air_public_input {
        let json = runner.get_air_public_input()?.serialize_json()?;
        std::fs::write(file_path, json)?;
    }

    if let (Some(file_path), Some(trace_file), Some(memory_file)) = (
        args.run.proof_outputs.air_private_input,
        args.run.proof_outputs.trace_file,
        args.run.proof_outputs.memory_file,
    ) {
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

    if let Some(file_name) = args.output_path {
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
