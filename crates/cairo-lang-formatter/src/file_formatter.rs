use std::fs;
use std::io::{stdin, Read};
use std::path::{Path, PathBuf};
use std::sync::Arc;

use anyhow::{bail, Result};
use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_filesystem::ids::{FileId, FileLongId, VirtualFile};
use cairo_lang_parser::utils::{get_syntax_root_and_diagnostics, SimpleParserDatabase};
use colored::Colorize;
use diffy::{create_patch, PatchFormatter};

use crate::{get_formatted_file, FormatterConfig};

#[derive(Default)]
pub struct FileFormatterArgs {
    pub check: bool,
    pub recursive: bool,
    pub verbose: bool,
    pub config: FormatterConfig,
}

#[derive(Debug)]
enum Input<'a> {
    Stdin,
    File { path: &'a str },
}

#[derive(Debug)]
pub enum FormatResult {
    Identical,
    DiffFound,
    NotFound,
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
            Self::File { path } => write!(f, "file {path}"),
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

/// Formats all files in a directory and sub directories (if specified), and return true if all
/// files were formatted correctly.
fn format_directory(
    path: &str,
    args: &FileFormatterArgs,
    recursion_depth: usize,
) -> Result<FormatResult> {
    if !args.recursive && recursion_depth > 0 {
        return Ok(FormatResult::Identical);
    }
    for sub_path in fs::read_dir(path).unwrap() {
        if sub_path.unwrap().file_name() == ".cairofmtignore" {
            eprintln_if_verbose(&format!("The directory {path} was ignored."), args.verbose);
            return Ok(FormatResult::Identical);
        }
    }

    let all_correct = fs::read_dir(path)
        .unwrap()
        .map(|sub_path| {
            format_path(sub_path.unwrap().path().to_str().unwrap(), args, recursion_depth + 1)
        })
        .all(|result| matches!(result, Ok(FormatResult::Identical)));

    Ok(if all_correct { FormatResult::Identical } else { FormatResult::DiffFound })
}

/// Gets a path to a file or directory and, if exists, calls the respective formatting function,
/// and returns if it was formatted correctly.
pub fn format_path(
    path: &str,
    args: &FileFormatterArgs,
    recursion_depth: usize,
) -> Result<FormatResult> {
    match fs::metadata(path) {
        // File exists
        Ok(metadata) => {
            if metadata.is_file() {
                if !is_cairo_file(path) {
                    eprintln_if_verbose(
                        &format!("The file: {path}, is not a cairo file, nothing was done.").red(),
                        args.verbose,
                    );
                    Ok(FormatResult::Identical)
                } else {
                    eprintln_if_verbose(&format!("Formatting file: {path}."), args.verbose);
                    format_input(&Input::File { path }, &args.config, args.check)
                }
            } else if metadata.is_dir() {
                eprintln_if_verbose(&format!("Formatting directory: {path}."), args.verbose);
                format_directory(path, args, recursion_depth)
            } else {
                // A symlink.
                eprintln!("{}", format!("The file {path} is a symlink. It was ignored.").red());
                Ok(FormatResult::Identical)
            }
        }
        Err(_) => {
            eprintln!("{}", format!("The file: {path}, was not found.").red());
            Ok(FormatResult::NotFound)
        }
    }
}

/// Calls the respective formatting function, on input from stdin
/// and returns if it was formatted correctly.
pub fn format_stdin(args: &FileFormatterArgs) -> Result<FormatResult> {
    format_input(&Input::Stdin, &args.config, args.check)
}

pub fn check_result(result: FormatResult, check: bool) -> bool {
    matches!((result, check), (FormatResult::Identical, _) | (FormatResult::DiffFound, false))
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
