use std::process::ExitCode;

use cairo_lang_formatter::file_formatter::{
    check_result, format_path, format_stdin, FileFormatterArgs, FormatResult,
};
use cairo_lang_formatter::FormatterConfig;
use cairo_lang_utils::logging::init_logging;
use clap::Parser;

/// Sierra to casm compiler.
/// Exits with 0/1 if the the compilation fails.
#[derive(Parser, Debug)]
#[clap(version, verbatim_doc_comment)]
struct FormatterArgs {
    /// Check mode, don't write the formatted files,
    /// just output the diff between the original and the formatted file.
    #[arg(short, long, default_value_t = false)]
    check: bool,
    /// Format directories content recursively.
    #[arg(short, long, default_value_t = false)]
    recursive: bool,
    /// Print verbose output.
    #[arg(short, long, default_value_t = false)]
    verbose: bool,
    /// A list of files and directories to format. Use "-" for stdin.
    files: Vec<String>,
}

fn main() -> ExitCode {
    init_logging(log::LevelFilter::Off);
    log::info!("Starting formatting.");

    let formatter_args = FormatterArgs::parse();
    let config = FormatterConfig::default();

    let args = FileFormatterArgs {
        check: formatter_args.check,
        verbose: formatter_args.verbose,
        recursive: formatter_args.recursive,
        config,
    };

    if args.verbose {
        eprintln!("Start formatting. Check: {}, Recursive: {}.", args.check, args.recursive);
    }

    let result;
    if formatter_args.files.len() == 1 && formatter_args.files[0] == "-" {
        // Input comes from stdin
        result = format_stdin(&args);
    } else if formatter_args.files.is_empty() {
        // Format current directory
        result = format_path(".", &args, 0);
    } else {
        // Format listed files and directories
        let success = formatter_args
            .files
            .iter()
            .map(|file| format_path(file, &args, 0))
            .all(|res| matches!(res, Ok(FormatResult::Identical)));

        result = Ok(if success { FormatResult::Identical } else { FormatResult::DiffFound });
    }

    if check_result(result.unwrap(), args.check) { ExitCode::SUCCESS } else { ExitCode::FAILURE }
}
