use std::fs;
use std::path::PathBuf;

use anyhow::Context;
use cairo_lang_compiler::project::check_compiler_path;
use cairo_lang_compiler::{CompilerConfig, compile_cairo_project_at_path};
use cairo_lang_utils::logging::init_logging;
use clap::Parser;

#[cfg(feature = "mimalloc")]
#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

/// Options for the `inlining-strategy` arguments.
#[derive(Copy, Clone, Debug, Default, PartialEq, Eq, clap::ValueEnum)]
pub enum InliningStrategy {
    /// Do not override inlining strategy.
    #[default]
    Default,
    /// Inline only in the case of an `inline(always)` annotation.
    Avoid,
}

impl From<crate::InliningStrategy> for cairo_lang_lowering::utils::InliningStrategy {
    fn from(value: crate::InliningStrategy) -> Self {
        match value {
            InliningStrategy::Default => cairo_lang_lowering::utils::InliningStrategy::Default,
            InliningStrategy::Avoid => cairo_lang_lowering::utils::InliningStrategy::Avoid,
        }
    }
}

/// Compiles a Cairo project to Sierra.
/// Exits with 0/1 if the compilation succeeds/fails.
#[derive(Parser, Debug)]
#[command(version, verbatim_doc_comment)]
struct Args {
    /// The Cairo project path.
    path: PathBuf,
    /// Whether path is a single file.
    #[arg(short, long)]
    single_file: bool,
    /// The output file name (default: stdout).
    output: Option<String>,
    /// Replaces Sierra IDs with human-readable ones.
    #[arg(short, long, default_value_t = false)]
    replace_ids: bool,
    /// Overrides inlining behavior.
    #[arg(short, long, default_value = "default")]
    inlining_strategy: InliningStrategy,
}

fn main() -> anyhow::Result<()> {
    init_logging(tracing::Level::ERROR);
    log::info!("Starting Cairo compilation.");

    let args = Args::parse();

    // Check if args.path is a file or a directory.
    check_compiler_path(args.single_file, &args.path)?;

    let sierra_program = compile_cairo_project_at_path(
        &args.path,
        CompilerConfig { replace_ids: args.replace_ids, ..CompilerConfig::default() },
        args.inlining_strategy.into(),
    )?;

    match args.output {
        Some(path) => {
            fs::write(path, sierra_program.to_string()).context("Failed to write output.")?
        }
        None => println!("{sierra_program}"),
    }

    Ok(())
}
