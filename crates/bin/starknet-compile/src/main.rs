use std::fs;
use std::path::PathBuf;

use anyhow::Context;
use cairo_lang_compiler::diagnostics::DiagnosticsReporter;
use cairo_lang_compiler::project::check_compiler_path;
use cairo_lang_compiler::CompilerConfig;
use cairo_lang_starknet::compile::starknet_compile;
use cairo_lang_starknet_classes::allowed_libfuncs::ListSelector;
use clap::Parser;

/// Compiles the specified contract from a Cairo project, into a contract class file.
/// Exits with 0/1 if the compilation succeeds/fails.
#[derive(Parser, Debug)]
#[clap(version, verbatim_doc_comment)]
struct Args {
    /// The path of the crate to compile.
    path: PathBuf,
    /// Whether path is a single file.
    #[arg(short, long)]
    single_file: bool,
    /// Allows the compilation to succeed with warnings.
    #[arg(long)]
    allow_warnings: bool,
    // The contract fully qualified path.
    #[arg(short, long)]
    contract_path: Option<String>,
    /// The output file name (default: stdout).
    output: Option<String>,
    /// Replaces sierra ids with human-readable ones.
    #[arg(short, long, default_value_t = false)]
    replace_ids: bool,
    /// The allowed libfuncs list to use (default: most recent audited list).
    #[arg(long)]
    allowed_libfuncs_list_name: Option<String>,
    /// A file of the allowed libfuncs list to use.
    #[arg(long)]
    allowed_libfuncs_list_file: Option<String>,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    // Check if args.path is a file or a directory.
    check_compiler_path(args.single_file, &args.path)?;

    let list_selector =
        ListSelector::new(args.allowed_libfuncs_list_name, args.allowed_libfuncs_list_file)
            .expect("Both allowed libfunc list name and file were supplied.");
    let mut diagnostics_reporter = DiagnosticsReporter::stderr();
    if args.allow_warnings {
        diagnostics_reporter = diagnostics_reporter.allow_warnings();
    }
    let res = starknet_compile(
        args.path,
        args.contract_path,
        Some(CompilerConfig {
            replace_ids: args.replace_ids,
            diagnostics_reporter,
            ..CompilerConfig::default()
        }),
        Some(list_selector),
    )?;
    match args.output {
        Some(path) => fs::write(path, res).with_context(|| "Failed to write output.")?,
        None => println!("{res}"),
    }

    Ok(())
}
