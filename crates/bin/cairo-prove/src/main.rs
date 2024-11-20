use std::path::PathBuf;

use anyhow::Context;
use cairo_lang_compiler::diagnostics::DiagnosticsReporter;
use cairo_lang_compiler::project::check_compiler_path;
use cairo_lang_runnable::compile::compile_runnable;
use cairo_lang_runnable::runnable::{EntryPointKind, Runnable};
use cairo_lang_runner::{Arg, CairoHintProcessor, build_hints_dict};
use cairo_vm::Felt252;
use cairo_vm::types::builtin_name::BuiltinName;
use cairo_vm::types::layout_name::LayoutName;
use cairo_vm::types::program::Program;
use cairo_vm::types::relocatable::MaybeRelocatable;
use cairo_vm::vm::runners::cairo_runner::CairoRunner;
use clap::Parser;
use num_bigint::BigInt;

/// Compiles a Cairo project and runs a function marked `#[runnable]`.
/// Exits with 1 if the compilation or run fails, otherwise 0.
#[derive(Parser, Debug)]
#[clap(version, verbatim_doc_comment)]
struct Args {
    /// The Cairo project path to compile and run.
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
    /// If provided, the path to the Cairo pie file.
    #[arg(long)]
    cairo_pie_path: Option<PathBuf>,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    // Check if args.path is a file or a directory.
    check_compiler_path(args.single_file, &args.path)?;

    let mut reporter = DiagnosticsReporter::stderr();
    if args.allow_warnings {
        reporter = reporter.allow_warnings();
    }
    let v = compile_runnable(&args.path, args.runnable.as_deref(), reporter)?;
    let runnable = Runnable::new(v);
    let entrypoint = runnable
        .entrypoints
        .iter()
        .find(|e| matches!(e.kind, EntryPointKind::NonReturning))
        .with_context(|| "No function entrypoint found.")?;
    let data =
        runnable.program.bytecode.iter().map(Felt252::from).map(MaybeRelocatable::from).collect();
    let (hints, string_to_hint) = build_hints_dict(&runnable.program.hints);
    let program = Program::new_for_proof(
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
    .with_context(|| "Failed setting up program.")?;
    let mut runner = CairoRunner::new(&program, LayoutName::dynamic, true, false)
        .with_context(|| "Failed setting up runner.")?;
    let end = runner.initialize(true).with_context(|| "Failed initializing runner.")?;
    let mut hint_processor = CairoHintProcessor {
        runner: None,
        user_args: vec![vec![Arg::Array(args.args.iter().map(|v| Arg::Value(v.into())).collect())]],
        string_to_hint,
        starknet_state: Default::default(),
        run_resources: Default::default(),
        syscalls_used_resources: Default::default(),
        no_temporary_segments: false,
    };
    runner.run_until_pc(end, &mut hint_processor).with_context(|| "Failed run to pc.")?;
    runner.run_for_steps(1, &mut hint_processor).with_context(|| "Failed run infinite loop.")?;
    runner.end_run(false, false, &mut hint_processor).with_context(|| "Failed end run.")?;
    runner.finalize_segments().with_context(|| "Failed finalizing segments.")?;
    runner.get_builtins_final_stack(runner.vm.get_ap())?;
    if args.print_outputs {
        let output = runner
            .vm
            .builtin_runners
            .iter()
            .find(|b| b.name() == BuiltinName::output)
            .with_context(|| "Could not find builtin runner for output.")?;
        let output_base = output.base();
        let output_size = output.get_used_cells(&runner.vm.segments)?;
        println!("output: [");
        for v in runner.vm.get_integer_range((output_base as isize, 0).into(), output_size)? {
            println!("    {v},");
        }
        println!("]")
    }
    if let Some(file_path) = args.cairo_pie_path {
        runner
            .get_cairo_pie()
            .with_context(|| "Failed generating pie.")?
            .write_zip_file(&file_path)
            .with_context(|| "Failed writing pie.")?
    }
    Ok(())
}
