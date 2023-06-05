//! Compiles and runs a Cairo program.

use anyhow::Ok;
use cairo_lang_test_runner::TestRunner;
use clap::Parser;

/// Command line args parser.
/// Exits with 0/1 if the input is formatted correctly/incorrectly.
#[derive(Parser, Debug)]
#[clap(version, verbatim_doc_comment)]
struct Args {
    /// The path to compile and run its tests.
    path: String,
    /// The filter for the tests, running only tests containing the filter string.
    #[arg(short, long, default_value_t = String::default())]
    filter: String,
    /// Should we run ignored tests as well.
    #[arg(long, default_value_t = false)]
    include_ignored: bool,
    /// Should we run only the ignored tests.
    #[arg(long, default_value_t = false)]
    ignored: bool,
    /// Should we add the starknet plugin to run the tests.
    #[arg(long, default_value_t = false)]
    starknet: bool,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    let runner = TestRunner::new(
        &args.path,
        &args.filter,
        args.include_ignored,
        args.ignored,
        args.starknet,
    )?;
    runner.run()?;

    Ok(())
}
