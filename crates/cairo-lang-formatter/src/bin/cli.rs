use std::process::ExitCode;

use cairo_lang_formatter::file_formatter::{format_path, format_stdin, FileFormatterArgs};
use cairo_lang_formatter::FormatterConfig;
use cairo_lang_utils::logging::init_logging;
use clap::Parser;

/// Sierra to casm compiler.
/// Exits with 0/1 if the the compilation fails.
#[derive(Parser, Debug)]
#[clap(version, verbatim_doc_comment)]
pub struct FormatterArgs {
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

    let mut all_correct = true;
    if formatter_args.files.len() == 1 && formatter_args.files[0] == "-" {
        // Input comes from stdin
        all_correct = format_stdin(&args);
    } else if formatter_args.files.is_empty() {
        // Format current directory
        all_correct = format_path(".", &args, 0);
    } else {
        // Format listed files and directories
        for file in formatter_args.files.iter() {
            all_correct &= format_path(file, &args, 0);
        }
    }

    if !all_correct && args.check { ExitCode::FAILURE } else { ExitCode::SUCCESS }
}
