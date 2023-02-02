use std::fmt::{Debug, Display};
use std::fs;
use std::io::{stdin, Read};
use std::path::{Path, PathBuf};
use std::sync::Arc;

use anyhow::{bail, Result};
use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_filesystem::ids::{FileId, FileLongId, VirtualFile};
use cairo_lang_parser::utils::{get_syntax_root_and_diagnostics, SimpleParserDatabase};
use diffy::{create_patch, PatchFormatter};
use ignore::types::TypesBuilder;
use ignore::WalkBuilder;

use crate::{get_formatted_file, FormatterConfig};

const CAIRO_FILE_EXTENSION: &str = "cairo";
const CAIRO_FILE_GLOB: &str = "*.cairo";
const CAIRO_FMT_IGNORE: &str = ".cairofmtignore";

pub struct FileDiff {
    pub original: String,
    pub formatted: String,
}

fn write_diff(diff: &FileDiff, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let patch = create_patch(&diff.original, &diff.formatted);
    let patch_formatter = PatchFormatter::new().with_color();
    let formatted_patch = patch_formatter.fmt_patch(&patch);
    formatted_patch.fmt(f)
}

impl Display for FileDiff {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write_diff(self, f)
    }
}

impl Debug for FileDiff {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write_diff(self, f)
    }
}

#[derive(Debug)]
pub enum FormattedFileContent {
    Original(String),
    Changed(FileDiff),
}

#[derive(Debug)]
pub enum FormatResult {
    Identical,
    DiffFound,
}

pub struct StdinFmt;

pub trait FormattableInput {
    fn to_file_id(&self, db: &dyn FilesGroup) -> Result<FileId>;
    fn overwrite_content(&self, _content: String) -> Result<()> {
        Ok(())
    }
}

impl FormattableInput for &Path {
    fn to_file_id(&self, db: &dyn FilesGroup) -> Result<FileId> {
        Ok(FileId::new(db, PathBuf::from(self)))
    }
    fn overwrite_content(&self, content: String) -> Result<()> {
        fs::write(self, content)?;
        Ok(())
    }
}

impl FormattableInput for String {
    fn to_file_id(&self, db: &dyn FilesGroup) -> Result<FileId> {
        Ok(db.intern_file(FileLongId::Virtual(VirtualFile {
            parent: None,
            name: "string_to_format".into(),
            content: Arc::new(self.clone()),
        })))
    }
}

impl FormattableInput for StdinFmt {
    fn to_file_id(&self, db: &dyn FilesGroup) -> Result<FileId> {
        let mut buffer = String::new();
        stdin().read_to_string(&mut buffer)?;
        Ok(db.intern_file(FileLongId::Virtual(VirtualFile {
            parent: None,
            name: "<stdin>".into(),
            content: Arc::new(buffer),
        })))
    }
    fn overwrite_content(&self, content: String) -> Result<()> {
        print!("{content}");
        Ok(())
    }
}

fn format_input(
    input: &dyn FormattableInput,
    config: &FormatterConfig,
) -> Result<(FormatResult, FormattedFileContent)> {
    let db = SimpleParserDatabase::default();
    let file_id = match input.to_file_id(&db) {
        Ok(value) => value,
        Err(_) => {
            bail!("Unable to create virtual file.");
        }
    };
    let original_text = match db.file_content(file_id) {
        Some(value) => value,
        None => {
            bail!("Unable to read from input.");
        }
    };
    let (syntax_root, diagnostics) = get_syntax_root_and_diagnostics(&db, file_id, &original_text);
    if !diagnostics.0.leaves.is_empty() {
        bail!(diagnostics.format(&db));
    }
    let formatted_text = get_formatted_file(&db, &syntax_root, config.clone());

    if &formatted_text == original_text.as_ref() {
        Ok((FormatResult::Identical, FormattedFileContent::Original(original_text.to_string())))
    } else {
        let diff = FileDiff { original: original_text.to_string(), formatted: formatted_text };
        Ok((FormatResult::DiffFound, FormattedFileContent::Changed(diff)))
    }
}

pub struct CairoFormatter {
    formatter_config: FormatterConfig,
}

impl CairoFormatter {
    pub fn new(formatter_config: FormatterConfig) -> Self {
        Self { formatter_config }
    }

    /// Returns a preconfigured `ignore::WalkBuilder` for the given path.
    pub fn walk(&self, path: &Path) -> WalkBuilder {
        let mut builder = WalkBuilder::new(path);
        builder.add_custom_ignore_filename(CAIRO_FMT_IGNORE);
        builder.follow_links(false);
        builder.skip_stdout(true);

        let mut types_builder = TypesBuilder::new();
        types_builder.add(CAIRO_FILE_EXTENSION, CAIRO_FILE_GLOB).unwrap();
        types_builder.select(CAIRO_FILE_EXTENSION);
        builder.types(types_builder.build().unwrap());

        builder
    }

    /// Verifies that the path is formatted correctly.
    pub fn check(&self, input: &dyn FormattableInput) -> Result<(FormatResult, Option<FileDiff>)> {
        match format_input(input, &self.formatter_config)? {
            (FormatResult::DiffFound, FormattedFileContent::Changed(diff)) => {
                Ok((FormatResult::DiffFound, Some(diff)))
            }
            (FormatResult::Identical, FormattedFileContent::Original(_)) => {
                Ok((FormatResult::Identical, None))
            }
            _ => unreachable!(),
        }
    }

    /// Formats the path in place, writing changes to the files.
    pub fn format_in_place(&self, input: &dyn FormattableInput) -> Result<FormatResult> {
        match format_input(input, &self.formatter_config)? {
            (FormatResult::DiffFound, FormattedFileContent::Changed(diff)) => {
                // Persist changes.
                input.overwrite_content(diff.formatted)?;
                Ok(FormatResult::DiffFound)
            }
            (FormatResult::Identical, FormattedFileContent::Original(_)) => {
                Ok(FormatResult::Identical)
            }
            _ => unreachable!(),
        }
    }

    /// Formats the path and returns the formatted string.
    pub fn format_to_string(&self, input: &dyn FormattableInput) -> Result<(FormatResult, String)> {
        match format_input(input, &self.formatter_config)? {
            (FormatResult::DiffFound, FormattedFileContent::Changed(diff)) => {
                Ok((FormatResult::DiffFound, diff.formatted))
            }
            (FormatResult::Identical, FormattedFileContent::Original(original)) => {
                Ok((FormatResult::Identical, original))
            }
            _ => unreachable!(),
        }
    }
}
