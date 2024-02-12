use std::fs;
use std::path::PathBuf;

use anyhow::Context;
use cairo_lang_compiler::project::check_compiler_path;
use cairo_lang_compiler::{compile_cairo_project_at_path, CompilerConfig};
use cairo_lang_utils::logging::init_logging;
use clap::Parser;

/// Compiles a Cairo project to Sierra.
/// Exits with 0/1 if the compilation succeeds/fails.
#[derive(Parser, Debug)]
#[clap(version, verbatim_doc_comment)]
struct Args {
    /// The Cairo project path.
    path: PathBuf,
    /// Whether path is a single file.
    #[arg(short, long)]
    single_file: bool,
    /// The output file name (default: stdout).
    output: Option<String>,
    /// Replaces sierra ids with human-readable ones.
    #[arg(short, long, default_value_t = false)]
    replace_ids: bool,
}

fn main() -> anyhow::Result<()> {
    init_logging(log::LevelFilter::Off);
    log::info!("Starting Cairo compilation.");

    let args = Args::parse();

    // Check if args.path is a file or a directory.
    check_compiler_path(args.single_file, &args.path)?;

    let sierra_program = compile_cairo_project_at_path(
        &args.path,
        CompilerConfig { replace_ids: args.replace_ids, ..CompilerConfig::default() },
    )?;

    match args.output {
        Some(path) => {
            fs::write(path, format!("{sierra_program}")).context("Failed to write output.")?
        }
        None => println!("{sierra_program}"),
    }

    Ok(())
}
