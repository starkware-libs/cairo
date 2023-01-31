use std::fs;

use cairo_lang_utils::logging::init_logging;
use cairo_lang_sierra_to_casm::compiler::compile_at_path;
use cairo_lang_sierra_to_casm::compiler::Args;
use clap::Parser;

fn main() {
    init_logging(log::LevelFilter::Off);
    log::info!("Starting Sierra compilation.");

    let args = Args::parse();

    let cairo_program = compile_at_path(&args.file[..]).expect("Compilation failed");

    fs::write(args.output, format!("{cairo_program}")).expect("Failed to write output.");
}