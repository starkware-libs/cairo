use std::fs;
use std::path::{PathBuf, Path};

use anyhow::Context;
use cairo_lang_compiler::diagnostics::check_and_eprint_diagnostics;
use cairo_lang_sierra_generator::db::SierraGenGroup;
use cairo_lang_compiler::project::setup_project;
use cairo_lang_diagnostics::ToOption;
use cairo_lang_dojo::db::{DojoRootDatabaseBuilderEx};
use clap::Parser;
use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_dojo::build::build_corelib;

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
    build_corelib(path.clone());

    let mut db_val = {
        let mut b = RootDatabase::builder();
        b.with_dev_corelib();
        b.with_dojo_and_starknet();
        b.build()
    };

    let db = &mut db_val;

    let main_crate_ids = setup_project(db, Path::new(&path))?;

    if check_and_eprint_diagnostics(db) {
        anyhow::bail!("Failed to compile: {}", path.display());
    }


    let sierra_program = db
        .get_sierra_program(main_crate_ids)
        .to_option()
        .context("Compilation failed without any diagnostics")?;


    match args.output {
        Some(path) => {
            fs::write(path, format!("{}", sierra_program)).context("Failed to write output.")?
        }
        None => println!("{}", sierra_program),
    }

    Ok(())
}

