use std::fs;
use std::path::{PathBuf, Path};

use anyhow::Context;
use cairo_lang_compiler::diagnostics::check_and_eprint_diagnostics;
use cairo_lang_sierra_generator::db::SierraGenGroup;
use cairo_lang_compiler::project::setup_project;
use cairo_lang_diagnostics::ToOption;
use cairo_lang_dojo::db::get_dojo_database;
use clap::Parser;

/// Command line args parser.
/// Exits with 0/1 if the input is formatted correctly/incorrectly.
#[derive(Parser, Debug)]
#[clap(version, verbatim_doc_comment)]
struct Args {
    /// The file to compile
    path: String,
    /// The output file name (default: stdout).
    output: Option<String>,
    /// Replaces sierra ids with human readable ones.
    #[arg(short, long, default_value_t = false)]
    replace_ids: bool,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();
    let path = &PathBuf::from(args.path);

    let mut db_val = get_dojo_database();
    let db = &mut db_val;

    let main_crate_ids = setup_project(db, Path::new(&path))?;

    if check_and_eprint_diagnostics(db) {
        anyhow::bail!("Failed to compile: {}", path.display());
    }

    let sierra_program = db
        .get_sierra_program(main_crate_ids)
        .to_option()
        .context("Compilation failed without any diagnostics")?;


    // let contract = compile_path(path, args.replace_ids)?;
    match args.output {
        Some(path) => {
            fs::write(path, format!("{}", sierra_program)).context("Failed to write output.")?
        }
        None => println!("{}", sierra_program),
    }

    Ok(())
}
