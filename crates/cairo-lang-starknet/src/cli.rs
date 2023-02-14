use std::fs;
use std::path::PathBuf;

use anyhow::Context;
use cairo_lang_compiler::CompilerConfig;
use cairo_lang_starknet::contract_class::compile_path;
use clap::Parser;

/// Command line args parser.
/// Exits with 0/1 if the input is formatted correctly/incorrectly.
#[derive(Parser, Debug)]
#[clap(version, verbatim_doc_comment)]
struct Args {
    /// The file to compile.
    path: PathBuf,
    /// The output file name (default: stdout).
    output: Option<String>,
    /// Replaces sierra ids with human-readable ones.
    #[arg(short, long, default_value_t = false)]
    replace_ids: bool,
    /// The allowed libfuncs list to use (default: most recent audited list).
    #[arg(long)]
    allowed_libfuncs_list_name: Option<String>,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();
    let contract = compile_path(
        &args.path,
        CompilerConfig {
            replace_ids: args.replace_ids,
            allowed_libfuncs_list_name: args.allowed_libfuncs_list_name,
            ..CompilerConfig::default()
        },
    )?;
    let res = serde_json::to_string_pretty(&contract).with_context(|| "Serialization failed.")?;
    match args.output {
        Some(path) => fs::write(path, res).with_context(|| "Failed to write output.")?,
        None => println!("{res}"),
    }

    Ok(())
}
