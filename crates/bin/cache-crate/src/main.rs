//! Internal debug utility for printing lowering phases.

use std::fs;
use std::path::{Path, PathBuf};

use anyhow::Context;
use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_compiler::project::{check_compiler_path, setup_project};
use cairo_lang_lowering::cache::generate_crate_cache;
use cairo_lang_starknet::starknet_plugin_suite;
use clap::Parser;

/// Prints the lowering of a concrete function:
///
/// Usage example:
///     cargo run --bin get-lowering corelib/ core::poseidon::poseidon_hash_span
///
/// Exits with 0/1 if the input is formatted correctly/incorrectly.
#[derive(Parser, Debug)]
#[clap(version, verbatim_doc_comment)]
struct Args {
    /// The crate to compile.
    path: PathBuf,
    /// Whether path is a single file.
    #[arg(short, long)]
    single_file: bool,

    output: String,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    // Check if args.path is a file or a directory.
    check_compiler_path(args.single_file, &args.path)?;
    let mut db_val = RootDatabase::builder()
        .detect_corelib()
        // .with_plugin_suite(starknet_plugin_suite())
        .build()?;
    let main_crate_ids = setup_project(&mut db_val, Path::new(&args.path))?;
    let db = &db_val;

    for c in main_crate_ids {
        let artifact = generate_crate_cache(db, c).unwrap();

        // save the artifact to a file name output
        let output_path = Path::new(&args.output).join(c.name(db));
        fs::write(output_path, artifact).context("Failed to write artifact to file")?;
    }

    Ok(())
}
