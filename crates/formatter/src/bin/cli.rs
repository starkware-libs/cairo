use std::fs;
use std::path::Path;
use std::process::ExitCode;

use clap::Parser;
use colored::Colorize;
use diffy::{create_patch, PatchFormatter};
use formatter::{get_formatted_file, FormatterConfig};
use parser::utils::{get_syntax_root_and_diagnostics_from_file, SimpleParserDatabase};
use utils::logging::init_logging;

/// Format a specific file and return whether it was already correctly formatted.
fn format_file(file_path: &str, args: &FormatterArgs, config: &FormatterConfig) -> bool {
    if !is_cairo_file(file_path) {
        eprintln_if_verbose(
            &format!("The file: {file_path}, is not a cairo file, nothing was done.").red(),
            args.verbose,
        );
        return true;
    }

    let db_val = SimpleParserDatabase::default();
    let db = &db_val;
    let (syntax_root, diagnostics) = get_syntax_root_and_diagnostics_from_file(db, file_path);
    // Checks if the inner ParserDiagnostic is empty.
    if !diagnostics.0.leaves.is_empty() {
        eprintln!(
            "{}",
            format!("A parsing error occurred in file: {file_path}. The file was not formatted.")
                .red()
        );
        return false;
    }
    let formatted_file = get_formatted_file(db, &syntax_root, config.clone());

    let original_file = fs::read_to_string(file_path).unwrap();
    if formatted_file == original_file {
        true
    } else {
        if args.check {
            let patch = create_patch(&original_file, &formatted_file);
            let f = PatchFormatter::new().with_color();
            println!("Diff in {file_path}:");
            print!("{}", f.fmt_patch(&patch));
        } else if fs::write(file_path, formatted_file).is_err() {
            eprintln!("{}", format!("Unable to write result to {file_path}.").red());
        }

        false
    }
}

/// Formats all files in a directory and sub directories (if specified), and return true if all
/// files were formatted correctly.
fn format_directory(
    path: &str,
    args: &FormatterArgs,
    recursion_depth: usize,
    config: &FormatterConfig,
) -> bool {
    if !args.recursive && recursion_depth > 0 {
        return true;
    }
    for sub_path in fs::read_dir(path).unwrap() {
        if sub_path.unwrap().file_name() == ".cairofmtignore" {
            println_if_verbose(&format!("The directory {path} was ignored."), args.verbose);
            return true;
        }
    }
    let mut all_correct = true;
    for sub_path in fs::read_dir(path).unwrap() {
        all_correct &= format_path(
            sub_path.unwrap().path().to_str().unwrap(),
            args,
            recursion_depth + 1,
            config,
        );
    }
    all_correct
}

/// Gets a path to a file or directory and, if exists, calls the respective formatting function,
/// and returns if it was formatted correctly.
fn format_path(
    path: &str,
    args: &FormatterArgs,
    recursion_depth: usize,
    config: &FormatterConfig,
) -> bool {
    match fs::metadata(path) {
        // File exists
        Ok(metadata) => {
            if metadata.is_file() {
                println_if_verbose(&format!("Formatting file: {}.", path), args.verbose);
                format_file(path, args, config)
            } else if metadata.is_dir() {
                println_if_verbose(&format!("Formatting directory: {}.", path), args.verbose);
                format_directory(path, args, recursion_depth, config)
            } else {
                // A symlink.
                eprintln!("{}", format!("The file {} is a symlink. It was ignored.", path).red());
                true
            }
        }
        Err(_) => {
            eprintln!("{}", format!("The file: {path}, was not found.").red());
            false
        }
    }
}

/// Checks if the file extension is "cairo".
/// Should only be called with a file path.
fn is_cairo_file(file_path: &str) -> bool {
    match Path::new(file_path).extension() {
        Some(ext) => ext.to_ascii_lowercase() == "cairo",
        None => false,
    }
}

/// Outputs a string if the verbose flag is true.
fn println_if_verbose(s: &str, verbose: bool) {
    if verbose {
        println!("{s}");
    }
}

/// Outputs an error string if the verbose flag is true.
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
    /// A list of files and directories to format.
    files: Vec<String>,
}

fn main() -> ExitCode {
    init_logging(log::LevelFilter::Off);
    log::info!("Starting formatting.");

    let args = FormatterArgs::parse();
    let config = FormatterConfig::default();
    println_if_verbose(
        &format!("Start formatting. Check: {}, Recursive: {}.", args.check, args.recursive),
        args.verbose,
    );
    let mut all_correct = true;
    if args.files.is_empty() {
        all_correct = format_path(".", &args, 0, &config);
    } else {
        for file in args.files.iter() {
            all_correct &= format_path(file, &args, 0, &config);
        }
    }
    if !all_correct && args.check { ExitCode::FAILURE } else { ExitCode::SUCCESS }
}
