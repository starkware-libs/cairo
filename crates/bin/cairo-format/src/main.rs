use std::fmt::Debug;
use std::path::Path;
use std::process::ExitCode;
use std::sync::atomic::{AtomicBool, Ordering};

use cairo_lang_formatter::{CairoFormatter, FormatOutcome, FormatterConfig, StdinFmt};
use cairo_lang_utils::logging::init_logging;
use clap::Parser;
use colored::Colorize;
use ignore::WalkState::Continue;
use ignore::{DirEntry, Error, ParallelVisitor, ParallelVisitorBuilder, WalkState};
use log::warn;

/// Outputs a string to stderr if the verbose flag is true.
fn eprintln_if_verbose(s: &str, verbose: bool) {
    if verbose {
        eprintln!("{s}");
    }
}

/// Formats a file or directory with the Cairo formatter.
/// Exits with 0/1 if the input is formatted correctly/incorrectly.
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
    /// Enable sorting the module level items (imports, mod definitions...).
    #[arg(short, long)]
    sort_mod_level_items: Option<bool>,
    /// Controls tuple breaking behavior. Set to 'line-by-line' (default) to format each
    /// tuple item on a new line, or 'single-break-point' to keep as many items as possible on the
    /// same line (as space permits). Defaults to line-by-line.
    #[arg(long)]
    tuple_line_breaking: Option<bool>,
    /// Controls fixed array breaking behavior. Set to 'single-break-point' (default) to format
    /// each array item on a new line, or 'line-by-line' to keep as many items as possible on the
    /// same line (as space permits). Defaults to single line.
    #[arg(long)]
    fixed_array_line_breaking: Option<bool>,
    /// Enable merging of `use` items.
    #[arg(long)]
    merge_use_items: Option<bool>,
    ///  Enable duplicates in `use` items.
    #[arg(long)]
    allow_duplicates: Option<bool>,
    /// A list of files and directories to format. Use "-" for stdin.
    files: Vec<String>,
}

fn print_error(error: String, path: String, args: &FormatterArgs) {
    let parsed_errors = if args.print_parsing_errors {
        error.red()
    } else {
        "Run with '--print-parsing-errors' to see error details.".red()
    };
    eprintln!(
        "{}",
        format!(
            "A parsing error occurred in {path}. The content was not formatted.\n{parsed_errors}"
        )
        .red()
    );
}

struct PathFormatter<'t> {
    all_correct: &'t AtomicBool,
    args: &'t FormatterArgs,
    fmt: &'t CairoFormatter,
}

struct PathFormatterBuilder<'t> {
    all_correct: &'t AtomicBool,
    args: &'t FormatterArgs,
    fmt: &'t CairoFormatter,
}

impl<'s, 't> ParallelVisitorBuilder<'s> for PathFormatterBuilder<'t>
where
    't: 's,
{
    fn build(&mut self) -> Box<dyn ParallelVisitor + 's> {
        Box::new(PathFormatter { all_correct: self.all_correct, args: self.args, fmt: self.fmt })
    }
}

fn check_file_formatting(fmt: &CairoFormatter, args: &FormatterArgs, path: &Path) -> bool {
    match fmt.format_to_string(&path) {
        Ok(FormatOutcome::Identical(_)) => true,
        Ok(FormatOutcome::DiffFound(diff)) => {
            println!("Diff found in file {}:\n {}", path.display(), diff.display_colored());
            false
        }
        Err(parsing_error) => {
            print_error(parsing_error.to_string(), path.display().to_string(), args);
            false
        }
    }
}

fn format_file_in_place(fmt: &CairoFormatter, args: &FormatterArgs, path: &Path) -> bool {
    if let Err(parsing_error) = fmt.format_in_place(&path) {
        print_error(parsing_error.to_string(), path.display().to_string(), args);
        false
    } else {
        true
    }
}

impl ParallelVisitor for PathFormatter<'_> {
    fn visit(&mut self, dir_entry_res: Result<DirEntry, Error>) -> WalkState {
        let dir_entry = if let Ok(dir_entry) = dir_entry_res {
            dir_entry
        } else {
            warn!("Failed to read the file.");
            return Continue;
        };

        let file_type = if let Some(file_type) = dir_entry.file_type() {
            file_type
        } else {
            warn!("Failed to read filetype.");
            return Continue;
        };

        if !file_type.is_file() {
            return Continue;
        }

        let file_path = dir_entry.path();

        if self.args.verbose {
            eprintln!("Formatting file: {}.", file_path.display());
        }

        let success = if self.args.check {
            check_file_formatting(self.fmt, self.args, file_path)
        } else {
            format_file_in_place(self.fmt, self.args, file_path)
        };

        if !success {
            self.all_correct.store(false, Ordering::Release);
        }
        Continue
    }
}

fn format_path(start_path: &str, args: &FormatterArgs, fmt: &CairoFormatter) -> bool {
    let base = Path::new(start_path);
    let mut walk = fmt.walk(base);
    if !args.recursive {
        walk.max_depth(Some(1));
    }

    let all_correct = AtomicBool::new(true);
    let mut builder = PathFormatterBuilder { args, fmt, all_correct: &all_correct };
    walk.build_parallel().visit(&mut builder);

    builder.all_correct.load(Ordering::Acquire)
}

fn format_stdin(args: &FormatterArgs, fmt: &CairoFormatter) -> bool {
    match fmt.format_to_string(&StdinFmt) {
        Ok(outcome) => {
            if args.check {
                match outcome {
                    FormatOutcome::Identical(_) => true,
                    FormatOutcome::DiffFound(diff) => {
                        println!("{diff}");
                        false
                    }
                }
            } else {
                print!("{}", FormatOutcome::into_output_text(outcome));
                true
            }
        }
        Err(parsing_error) => {
            print_error(parsing_error.to_string(), String::from("standard input"), args);
            false
        }
    }
}

fn main() -> ExitCode {
    init_logging(log::LevelFilter::Off);
    log::info!("Starting formatting.");

    let args = FormatterArgs::parse();
    let config = FormatterConfig::default()
        .sort_module_level_items(args.sort_mod_level_items)
        .tuple_breaking_behavior(args.tuple_line_breaking.map(Into::into))
        .fixed_array_breaking_behavior(args.fixed_array_line_breaking.map(Into::into))
        .merge_use_items(args.merge_use_items)
        .allow_duplicate_uses(args.allow_duplicates);
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
    if all_correct { ExitCode::SUCCESS } else { ExitCode::FAILURE }
}
