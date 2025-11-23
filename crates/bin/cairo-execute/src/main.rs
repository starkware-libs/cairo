use std::io::{self, Write};
use std::path::{Path, PathBuf};

use anyhow::Context;
use bincode::enc::write::Writer;
use cairo_lang_compiler::diagnostics::DiagnosticsReporter;
use cairo_lang_compiler::project::{check_compiler_path, setup_project};
use cairo_lang_debug::debug::DebugWithDb;
use cairo_lang_executable::compile::{
    CompileExecutableResult, ExecutableConfig, compile_executable_in_prepared_db, prepare_db,
};
use cairo_lang_executable::executable::{EntryPointKind, Executable, NOT_RETURNING_HEADER_SIZE};
use cairo_lang_execute_utils::{program_and_hints_from_executable, user_args_from_flags};
use cairo_lang_filesystem::ids::CrateInput;
use cairo_lang_runnable_utils::builder::RunnableBuilder;
use cairo_lang_runner::casm_run::format_for_panic;
use cairo_lang_runner::clap::RunProfilerConfigArg;
use cairo_lang_runner::profiling::{
    ProfilingInfo, ProfilingInfoProcessor, ProfilingInfoProcessorParams,
};
use cairo_lang_runner::{Arg, CairoHintProcessor, ProfilingInfoCollectionConfig};
use cairo_lang_sierra_generator::program_generator::SierraProgramDebugInfo;
use cairo_lang_sierra_generator::replace_ids::replace_sierra_ids_in_program;
use cairo_vm::cairo_run;
use cairo_vm::cairo_run::{CairoRunConfig, cairo_run_program};
use cairo_vm::types::layout::CairoLayoutParams;
use cairo_vm::types::layout_name::LayoutName;
use cairo_vm::vm::errors::cairo_run_errors::CairoRunError;
use clap::Parser;
use num_bigint::BigInt;
use salsa::Database;

#[cfg(feature = "mimalloc")]
#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

/// Compiles a Cairo project and runs a function marked `#[executable]`.
/// Exits with 1 if the compilation or run fails, otherwise 0.
#[derive(Parser, Debug)]
#[command(version, verbatim_doc_comment)]
struct Args {
    /// The basic input path for the run.
    /// If `--prebuilt` is provided, this is the path to the prebuilt executable.
    /// Else, if `--single-file` is provided, this is the path to a single Cairo file to compile
    /// for execution. Otherwise, this is the path of the Cairo project - the directory
    /// containing the `cairo_project.toml` file.
    input_path: PathBuf,

    /// Whether to only build and save into the given path.
    #[arg(long, requires = "output_path")]
    build_only: bool,

    /// The path to save the result of the run.
    ///
    /// In `--build-only` this would be the executable artifact.
    /// In bootloader mode it will be the resulting Cairo PIE file.
    /// In standalone mode this parameter is disallowed.
    #[arg(long, required_unless_present_any(["standalone", "run_profiler"]))]
    output_path: Option<PathBuf>,

    /// Whether to only run a prebuilt executable.
    #[arg(long, default_value_t = false, conflicts_with = "build_only")]
    prebuilt: bool,

    /// Whether to run the profiler, and what results to produce. See
    /// [cairo_lang_runner::profiling::ProfilerConfig]
    /// Currently does not work with prebuilt executables as it requires additional debug info.
    #[arg(short, long, default_value_t, value_enum, conflicts_with = "prebuilt")]
    run_profiler: RunProfilerConfigArg,

    #[command(flatten)]
    build: BuildArgs,

    #[command(flatten)]
    run: RunArgs,

    /// Prints the bytecode size.
    #[arg(long, default_value_t = false)]
    print_bytecode_size: bool,
}

#[derive(Parser, Debug)]
struct BuildArgs {
    /// Whether the path is a single file.
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
    /// Replace the panic flow with an unprovable opcode, this reduces code size but might make it
    /// more difficult to debug.
    #[arg(long, conflicts_with = "prebuilt")]
    unsafe_panic: bool,
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
    #[command(flatten)]
    args: SerializedArgs,
    /// Whether to print the outputs.
    #[arg(long, default_value_t = false, conflicts_with = "build_only")]
    print_outputs: bool,

    /// When using dynamic layout, its parameters must be specified through a layout params file.
    #[arg(long, default_value = "plain", value_enum, conflicts_with = "build_only")]
    layout: LayoutName,
    /// Required when using with dynamic layout.
    /// Ignored otherwise.
    #[arg(long, required_if_eq("layout", "dynamic"))]
    cairo_layout_params_file: Option<PathBuf>,
    /// If set, the program will be run in standalone mode.
    #[arg(
        long,
        default_value_t = false,
        conflicts_with_all = ["build_only", "output_path"],
        requires_ifs = [("standalone", "air_public_input"), ("standalone", "air_private_input")],
    )]
    standalone: bool,
    /// If set, the program will be run in secure mode.
    #[arg(long, conflicts_with = "build_only")]
    secure_run: Option<bool>,
    /// Can we allow for missing builtins.
    #[arg(long, conflicts_with = "build_only")]
    allow_missing_builtins: Option<bool>,
    /// Disable padding of the trace.
    /// By default, the trace is padded to accommodate the expected builtins-n_steps relationships
    /// according to the layout.
    /// When the padding is disabled:
    /// - It doesn't modify/pad n_steps.
    /// - It still pads each builtin segment to the next power of 2 (with respect to the number of
    ///   used instances of the builtin) compared to their sizes at the end of the execution.
    #[arg(long, conflicts_with = "build_only")]
    disable_trace_padding: Option<bool>,
    #[command(flatten)]
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
    #[arg(long, conflicts_with = "build_only")]
    trace_file: Option<PathBuf>,
    /// The resulting memory file.
    #[arg(long, conflicts_with = "build_only")]
    memory_file: Option<PathBuf>,
    /// The resulting AIR public input file.
    #[arg(long, conflicts_with = "build_only")]
    air_public_input: Option<PathBuf>,
    /// The resulting AIR private input file.
    #[arg(
        long,
        conflicts_with = "build_only",
        requires_ifs = [("standalone", "trace_file"), ("standalone", "memory_file")]
    )]
    air_private_input: Option<PathBuf>,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();
    let mut diagnostics_reporter = DiagnosticsReporter::stderr();

    let (db, opt_debug_data, executable) = {
        if args.prebuilt {
            let executable: Executable =
                serde_json::from_reader(std::fs::File::open(&args.input_path)?)
                    .with_context(|| "Failed reading prebuilt executable.")?;
            (None, None, Some(executable))
        } else {
            // Check if args.path is a file or a directory.
            check_compiler_path(args.build.single_file, &args.input_path)?;

            if args.build.allow_warnings {
                diagnostics_reporter = diagnostics_reporter.allow_warnings();
            }
            if args.build.ignore_warnings {
                diagnostics_reporter = diagnostics_reporter.ignore_all_warnings();
            }
            let config = ExecutableConfig {
                allow_syscalls: args.build.allow_syscalls,
                unsafe_panic: args.build.unsafe_panic,
            };

            let mut db = prepare_db(&config)?;
            let main_crate_inputs = setup_project(&mut db, Path::new(&args.input_path))?;
            (Some(db), Some((main_crate_inputs, config)), None)
        }
    };

    let (opt_debug_data, executable) = if let Some((main_crate_inputs, config)) = opt_debug_data {
        let db = db.as_ref().expect("db was just created in compilation path");
        let main_crate_ids = CrateInput::into_crate_ids(db, main_crate_inputs);
        let CompileExecutableResult { compiled_function, builder, debug_info } =
            compile_executable_in_prepared_db(
                db,
                args.build.executable.as_deref(),
                main_crate_ids,
                diagnostics_reporter,
                config,
            )?;
        let wrapper_len =
            compiled_function.wrapper.header.iter().map(|insn| insn.body.op_size()).sum::<usize>();
        let header_len = NOT_RETURNING_HEADER_SIZE + wrapper_len;
        let executable = Executable::new(compiled_function);
        (Some(DebugData { db, builder, debug_info, header_len }), executable)
    } else {
        (None, executable.unwrap())
    };

    if args.print_bytecode_size {
        println!("Bytecode size (felt252 count): {}", executable.program.bytecode.len());
    };
    if args.build_only {
        let path = args.output_path.with_context(|| "No output path provided.")?;
        serde_json::to_writer(std::fs::File::create(path)?, &executable)
            .with_context(|| "Failed writing executable.")?;
        return Ok(());
    }

    let entry_point_kind =
        if args.run.standalone { EntryPointKind::Standalone } else { EntryPointKind::Bootloader };

    let entrypoint = executable
        .entrypoints
        .iter()
        .find(|e| e.kind == entry_point_kind)
        .with_context(|| format!("{entry_point_kind:?} entrypoint not found"))?;

    let (program, string_to_hint) = program_and_hints_from_executable(&executable, entrypoint)?;

    let user_args = user_args_from_flags(args.run.args.as_file.as_ref(), &args.run.args.as_list)?;

    let mut hint_processor = CairoHintProcessor {
        runner: None,
        user_args: vec![vec![Arg::Array(user_args)]],
        string_to_hint,
        starknet_state: Default::default(),
        run_resources: Default::default(),
        syscalls_used_resources: Default::default(),
        no_temporary_segments: false,
        markers: Default::default(),
        panic_traceback: Default::default(),
    };
    let dynamic_layout_params = match args.run.cairo_layout_params_file {
        Some(file) => Some(CairoLayoutParams::from_file(&file)?),
        None => None,
    };

    let trace_enabled = args.run_profiler != RunProfilerConfigArg::None
        || args.run.proof_outputs.trace_file.is_some();

    let cairo_run_config = CairoRunConfig {
        trace_enabled,
        relocate_mem: args.run.proof_outputs.memory_file.is_some(),
        relocate_trace: trace_enabled,
        layout: args.run.layout,
        dynamic_layout_params,
        proof_mode: args.run.standalone,
        fill_holes: args.run.standalone,
        secure_run: args.run.secure_run,
        allow_missing_builtins: args.run.allow_missing_builtins,
        disable_trace_padding: args.run.disable_trace_padding.unwrap_or_default(),
        ..Default::default()
    };

    let mut runner = match cairo_run_program(&program, &cairo_run_config, &mut hint_processor) {
        Ok(runner) => runner,
        Err(err) => {
            if let Some(panic_data) = hint_processor.markers.last() {
                return Err(err).with_context(|| format_for_panic(panic_data.iter().copied()));
            };
            if let Some(err_location) = try_get_error_location(&opt_debug_data, &err) {
                return Err(err).context(err_location);
            }
            return Err(err).context("Failed running program.");
        }
    };
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
            .write_zip_file(&file_name, true)?
    }

    if let Ok(profiler_config) = &args.run_profiler.try_into() {
        let DebugData { db, builder, debug_info, header_len } =
            opt_debug_data.expect("debug data should be available when profiling");

        let trace = runner.relocated_trace.as_ref().with_context(|| "Trace not relocated.")?;
        let first_pc = trace.first().unwrap().pc;
        // After relocation, we expect the program to start at offset 1.
        assert_eq!(first_pc - entrypoint.offset, 1);
        let load_offset = 1 + header_len;
        let info = ProfilingInfo::from_trace(
            &builder,
            load_offset,
            &ProfilingInfoCollectionConfig::from_profiler_config(profiler_config),
            trace,
        );

        let processed_profiling_info = ProfilingInfoProcessor::new(
            Some(db),
            &replace_sierra_ids_in_program(db, builder.sierra_program()),
            debug_info.statements_locations.get_statements_functions_map_for_tests(db),
        )
        .process(&info, &ProfilingInfoProcessorParams::from_profiler_config(profiler_config));
        print!("{processed_profiling_info}");
    }

    Ok(())
}

/// Tries to get the location of the error in the source code.
///
/// Returns `None` if `opt_debug_data` is None, the error is not a `VmException` or if the error is
/// in a different segment than the first one.
fn try_get_error_location(
    opt_debug_data: &Option<DebugData<'_>>,
    err: &CairoRunError,
) -> Option<String> {
    let DebugData { db, builder, debug_info, header_len } = opt_debug_data.as_ref()?;
    let CairoRunError::VmException(err) = err else { return None };
    if err.pc.segment_index != 0 {
        return None;
    }

    // Note that pc wasn't relocated here so pc.offset is zero-based.
    let pc = err.pc.offset.checked_sub(*header_len)?;
    let stmt_idx = builder.casm_program().sierra_statement_index_by_pc(pc);
    let loc = debug_info.statements_locations.statement_diagnostic_location(*db, stmt_idx)?;
    Some(format!("#{stmt_idx} {:?}", loc.debug(*db)))
}

/// Data required for profiling and debugging the executable.
struct DebugData<'a> {
    /// The salsa database.
    db: &'a dyn Database,
    /// The builder for the runnable program.
    builder: RunnableBuilder,
    /// Debug information for the Sierra program.
    debug_info: SierraProgramDebugInfo<'a>,
    /// The size of the header that was prepended to the builder's CASM program.
    header_len: usize,
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
