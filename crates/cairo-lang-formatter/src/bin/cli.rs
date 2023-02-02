use std::path::Path;
use std::process::ExitCode;

use cairo_lang_formatter::cairo_formatter::{FormatResult, StdinFmt};
use cairo_lang_formatter::{CairoFormatter, FormatterConfig};
use cairo_lang_utils::logging::init_logging;
use clap::Parser;
use colored::Colorize;

/// Outputs a string to stderr if the verbose flag is true.
fn eprintln_if_verbose(s: &str, verbose: bool) {
    if verbose {
        eprintln!("{s}");
    }
}

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
    /// Print parsing errors.
    #[arg(short, long, default_value_t = false)]
    print_parsing_errors: bool,
    /// A list of files and directories to format. Use "-" for stdin.
    files: Vec<String>,
}

fn print_error(error: anyhow::Error, path: String, args: &FormatterArgs) {
    eprintln!(
        "{}",
        format!("A parsing error occurred in {}. The content was not formatted.", path).red()
    );
    if args.print_parsing_errors {
        eprintln!("{}", format!("{}", error).red());
    } else {
        eprintln!("{}", "Run with '--print-parsing-errors' to see error details.".red());
    }
}

fn format_path(start_path: &str, args: &FormatterArgs, fmt: &CairoFormatter) -> bool {
    let mut all_correct = true;
    let base = Path::new(start_path);
    let mut walk = fmt.walk(base);
    if !args.recursive {
        walk.max_depth(Some(1));
    }
    for dir_entry in walk.build() {
        let entry_path = dir_entry.unwrap();
        if entry_path.file_type().unwrap().is_dir() {
            continue;
        }
        let path = entry_path.path();
        if args.verbose {
            eprintln!("Formatting file: {}.", path.display());
        }
        if args.check {
            match fmt.check(&path) {
                Ok((FormatResult::Identical, _)) => continue,
                Ok((FormatResult::DiffFound, diff)) => {
                    println!("Diff found in file {}:\n {}", path.display(), diff.unwrap());
                    all_correct = false;
                }
                Err(parsing_error) => {
                    print_error(parsing_error, path.display().to_string(), args);
                    all_correct = false;
                }
            }
        } else {
            match fmt.format_in_place(&path) {
                Ok(FormatResult::DiffFound) => {
                    all_correct = true;
                }
                Err(parsing_error) => {
                    print_error(parsing_error, path.display().to_string(), args);
                    all_correct = false;
                }
                _ => {}
            }
        }
    }
    all_correct
}

fn format_stdin(args: &FormatterArgs, fmt: &CairoFormatter) -> bool {
    if args.check {
        match fmt.check(&StdinFmt) {
            Ok((FormatResult::Identical, _)) => true,
            Ok((FormatResult::DiffFound, diff)) => {
                println!("{}", diff.unwrap());
                false
            }
            Err(parsing_error) => {
                print_error(parsing_error, String::from("standard input"), args);
                false
            }
        }
    } else {
        match fmt.format_to_string(&StdinFmt) {
            Ok((_, text)) => {
                println!("{}", text);
                true
            }
            Err(parsing_error) => {
                print_error(parsing_error, String::from("standard input"), args);
                false
            }
        }
    }
}

fn main() -> ExitCode {
    init_logging(log::LevelFilter::Off);
    log::info!("Starting formatting.");

    let args = FormatterArgs::parse();
    let config = FormatterConfig::default();
    let fmt = CairoFormatter::new(config);

    eprintln_if_verbose(
        &format!("Start formatting. Check: {}, Recursive: {}.", args.check, args.recursive),
        args.verbose,
    );

    let mut all_correct = true;
    if args.files.len() == 1 && args.files[0] == "-" {
        // Input comes from stdin
        all_correct = format_stdin(&args, &fmt)
    } else if args.files.is_empty() {
        // Input comes from current directory walk
        all_correct = format_path(".", &args, &fmt);
    } else {
        // Input comes from walk of listed locations
        for file in args.files.iter() {
            all_correct &= format_path(file, &args, &fmt);
        }
    }
    if !all_correct && args.check { ExitCode::FAILURE } else { ExitCode::SUCCESS }
}
