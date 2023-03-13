use std::fs;

use anyhow::Context;
use cairo_lang_sierra_to_casm::compiler::{compile_at_path, Args};
use cairo_lang_utils::logging::init_logging;
use clap::Parser;

fn main() -> anyhow::Result<()> {
    init_logging(log::LevelFilter::Off);
    log::info!("Starting Sierra compilation.");

    let args = Args::parse();

    let cairo_program = compile_at_path(&args.file[..]).expect("Compilation failed");
    fs::write(args.output, format!("{cairo_program}")).with_context(|| "Failed to write output.")
}
