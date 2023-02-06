use std::path::Path;
use std::process::ExitCode;
use std::sync::Mutex;

use cairo_lang_formatter::{CairoFormatter, FormatOutcome, FormatterConfig, StdinFmt};
use cairo_lang_utils::logging::init_logging;
use clap::Parser;
use colored::Colorize;
use ignore::WalkState::Continue;
use ignore::{DirEntry, Error, ParallelVisitor, ParallelVisitorBuilder, WalkState};

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
        format!("A parsing error occurred in {path}. The content was not formatted.").red()
    );
    if args.print_parsing_errors {
        eprintln!("{}", format!("{error}").red());
    } else {
        eprintln!("{}", "Run with '--print-parsing-errors' to see error details.".red());
    }
}

struct PathFormatter<'t> {
    all_correct: &'t Mutex<bool>,
    own_correct: bool,
    args: &'t FormatterArgs,
    fmt: &'t CairoFormatter,
}

struct PathFormatterBuilder<'t> {
    all_correct: &'t Mutex<bool>,
    args: &'t FormatterArgs,
    fmt: &'t CairoFormatter,
}

impl<'s, 't> ParallelVisitorBuilder<'s> for PathFormatterBuilder<'t>
where
    't: 's,
{
    fn build(&mut self) -> Box<dyn ParallelVisitor + 's> {
        Box::new(PathFormatter {
            all_correct: self.all_correct,
            own_correct: true,
            args: self.args,
            fmt: self.fmt,
        })
    }
}

impl<'t> ParallelVisitor for PathFormatter<'t> {
    fn visit(&mut self, dir_entry: Result<DirEntry, Error>) -> WalkState {
        let entry_path = dir_entry.unwrap();
        if entry_path.file_type().unwrap().is_dir() {
            return Continue;
        }
        let path = entry_path.path();
        if self.args.verbose {
            eprintln!("Formatting file: {}.", path.display());
        }
        if self.args.check {
            match self.fmt.check(&path) {
                Ok(FormatOutcome::Identical(_)) => {}
                Ok(FormatOutcome::DiffFound(diff)) => {
                    println!("Diff found in file {}:\n {}", path.display(), diff);
                    self.own_correct = false;
                }
                Err(parsing_error) => {
                    print_error(parsing_error, path.display().to_string(), self.args);
                    self.own_correct = false;
                }
            }
        } else if let Err(parsing_error) = self.fmt.format_in_place(&path) {
            print_error(parsing_error, path.display().to_string(), self.args);
            self.own_correct = false;
        }
        Continue
    }
}

impl<'t> Drop for PathFormatter<'t> {
    fn drop(&mut self) {
        *self.all_correct.lock().unwrap() &= self.own_correct;
    }
}

fn format_path(start_path: &str, args: &FormatterArgs, fmt: &CairoFormatter) -> bool {
    let base = Path::new(start_path);
    let mut walk = fmt.walk(base);
    if !args.recursive {
        walk.max_depth(Some(1));
    }

    let all_correct = Mutex::new(true);
    let mut builder = PathFormatterBuilder { args, fmt, all_correct: &all_correct };
    walk.build_parallel().visit(&mut builder);
    let result = *builder.all_correct.lock().unwrap();

    result
}

fn format_stdin(args: &FormatterArgs, fmt: &CairoFormatter) -> bool {
    if args.check {
        match fmt.check(&StdinFmt) {
            Ok(FormatOutcome::Identical(_)) => true,
            Ok(FormatOutcome::DiffFound(diff)) => {
                println!("{diff}");
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
                println!("{text}");
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

    let all_correct = if args.files.len() == 1 && args.files[0] == "-" {
        // Input comes from stdin
        format_stdin(&args, &fmt)
    } else if args.files.is_empty() {
        // Input comes from current directory walk
        format_path(".", &args, &fmt)
    } else {
        // Input comes from walk of listed locations
        args.files.iter().all(|file| format_path(file, &args, &fmt))
    };
    if !all_correct && args.check { ExitCode::FAILURE } else { ExitCode::SUCCESS }
}
