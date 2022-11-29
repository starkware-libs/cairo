use std::fs;
use std::io::{stdin, Read};
use std::path::{Path, PathBuf};
use std::process::ExitCode;
use std::sync::Arc;

use anyhow::{bail, Result};
use clap::Parser;
use colored::Colorize;
use diffy::{create_patch, PatchFormatter};
use filesystem::db::FilesGroup;
use filesystem::ids::{FileId, FileLongId, VirtualFile};
use formatter::{get_formatted_file, FormatterConfig};
use parser::utils::{get_syntax_root_and_diagnostics, SimpleParserDatabase};
use utils::logging::init_logging;

#[derive(Debug)]
enum Input<'a> {
    Stdin,
    File { path: &'a str },
}

#[derive(Debug)]
enum FormatResult {
    Identical,
    DiffFound,
}

impl<'a> Input<'a> {
    pub fn to_file_id<D: FilesGroup>(&self, db: &D) -> Result<FileId, std::io::Error> {
        match self {
            Self::Stdin => {
                let mut buffer = String::new();
                stdin().read_to_string(&mut buffer)?;
                Ok(db.intern_file(FileLongId::Virtual(VirtualFile {
                    parent: None,
                    name: "<stdin>".into(),
                    content: Arc::new(buffer),
                })))
            }
            Self::File { path } => Ok(FileId::new(db, PathBuf::from(path))),
        }
    }

    pub fn write_content(&self, content: &str) -> Result<(), std::io::Error> {
        match self {
            Self::Stdin => {
                print!("{content}");
            }
            Self::File { path } => {
                fs::write(path, content)?;
            }
        }
        Ok(())
    }
}

impl<'a> std::fmt::Display for Input<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Stdin => write!(f, "stdin"),
            Self::File { path } => write!(f, "file {}", path),
        }
    }
}

/// Formats an input from stdin or file
fn format_input(input: &Input<'_>, config: &FormatterConfig, check: bool) -> Result<FormatResult> {
    let db = SimpleParserDatabase::default();
    let file_id = match input.to_file_id(&db) {
        Ok(value) => value,
        Err(_) => {
            eprintln!("{}", format!("Failed to create virtual file from {input}").red());
            bail!("Unable to create virtual file");
        }
    };
    let original_text = match db.file_content(file_id) {
        Some(value) => value,
        None => {
            eprintln!("{}", format!("Failed to read from {input}").red());
            bail!("Unable to read from input");
        }
    };

    let (syntax_root, diagnostics) = get_syntax_root_and_diagnostics(&db, file_id, &original_text);

    // Checks if the inner ParserDiagnostic is empty.
    if !diagnostics.0.leaves.is_empty() {
        eprintln!(
            "{}",
            format!("A parsing error occurred in {input}. The content was not formatted.").red()
        );
        bail!("Unable to parse input");
    }

    let formatted_text = get_formatted_file(&db, &syntax_root, config.clone());

    if &formatted_text == original_text.as_ref() {
        // Always print if input is stdin, unless --check is used
        if matches!(input, Input::Stdin) && !check {
            print!("{formatted_text}");
        }
        Ok(FormatResult::Identical)
    } else if check {
        // Diff found and --check was used
        print_diff(input, &original_text, &formatted_text);
        Ok(FormatResult::DiffFound)
    } else {
        // Diff found but --check is not used
        match input.write_content(&formatted_text) {
            Ok(_) => Ok(FormatResult::DiffFound),
            Err(_) => {
                eprintln!("{}", format!("Unable to write result to {input}.").red());
                bail!("Unable to write to output");
            }
        }
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
            eprintln_if_verbose(&format!("The directory {path} was ignored."), args.verbose);
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
                if !is_cairo_file(path) {
                    eprintln_if_verbose(
                        &format!("The file: {path}, is not a cairo file, nothing was done.").red(),
                        args.verbose,
                    );
                    true
                } else {
                    eprintln_if_verbose(&format!("Formatting file: {}.", path), args.verbose);
                    matches!(
                        (format_input(&Input::File { path }, config, args.check), args.check),
                        (Ok(FormatResult::Identical), _) | (Ok(FormatResult::DiffFound), false)
                    )
                }
            } else if metadata.is_dir() {
                eprintln_if_verbose(&format!("Formatting directory: {}.", path), args.verbose);
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

/// Outputs a string to stderr if the verbose flag is true.
fn eprintln_if_verbose(s: &str, verbose: bool) {
    if verbose {
        eprintln!("{s}");
    }
}

/// Prints diffs to stdout
fn print_diff(input: &Input<'_>, original_text: &str, formatted_text: &str) {
    let patch = create_patch(original_text, formatted_text);
    let f = PatchFormatter::new().with_color();
    match input {
        Input::Stdin => println!("Diff in stdin:"),
        Input::File { path } => println!("Diff in file {path}:"),
    }
    print!("{}", f.fmt_patch(&patch));
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
    /// A list of files and directories to format. Use "-" for stdin.
    files: Vec<String>,
}

fn main() -> ExitCode {
    init_logging(log::LevelFilter::Off);
    log::info!("Starting formatting.");

    let args = FormatterArgs::parse();
    let config = FormatterConfig::default();
    eprintln_if_verbose(
        &format!("Start formatting. Check: {}, Recursive: {}.", args.check, args.recursive),
        args.verbose,
    );

    if args.files.len() == 1 && args.files[0] == "-" {
        // Input comes from stdin
        match (format_input(&Input::Stdin, &config, args.check), args.check) {
            (Ok(FormatResult::Identical), _) => ExitCode::SUCCESS,
            (Ok(FormatResult::DiffFound), false) => ExitCode::SUCCESS,
            _ => ExitCode::FAILURE,
        }
    } else {
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
}
