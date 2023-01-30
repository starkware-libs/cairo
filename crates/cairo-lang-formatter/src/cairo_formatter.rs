use std::path::Path;

use anyhow::Result;
use ignore::types::TypesBuilder;
use ignore::WalkBuilder;

use crate::FormatterConfig;

const CAIRO_FILE_EXTENSION: &str = "cairo";
const CAIRO_FMT_IGNORE: &str = ".cairofmtignore";

#[derive(Debug)]
pub enum FormatResult {
    Identical,
    DiffFound,
}

pub struct StdinFmt;

pub trait FormattableInput {
    fn check(&self) -> Result<FormatResult>;
    fn format_in_place(&self) -> Result<FormatResult>;
    fn format_to_string(&self) -> Result<(FormatResult, String)>;
}

impl FormattableInput for Path {
    fn check(&self) -> Result<FormatResult> {
        Ok(FormatResult::Identical)
    }

    fn format_in_place(&self) -> Result<FormatResult> {
        Ok(FormatResult::Identical)
    }

    fn format_to_string(&self) -> Result<(FormatResult, String)> {
        Ok((FormatResult::Identical, String::new()))
    }
}

impl FormattableInput for String {
    fn check(&self) -> Result<FormatResult> {
        Ok(FormatResult::Identical)
    }

    fn format_in_place(&self) -> Result<FormatResult> {
        Ok(FormatResult::Identical)
    }

    fn format_to_string(&self) -> Result<(FormatResult, String)> {
        Ok((FormatResult::Identical, String::new()))
    }
}

impl FormattableInput for StdinFmt {
    fn check(&self) -> Result<FormatResult> {
        Ok(FormatResult::Identical)
    }

    fn format_in_place(&self) -> Result<FormatResult> {
        Ok(FormatResult::Identical)
    }

    fn format_to_string(&self) -> Result<(FormatResult, String)> {
        Ok((FormatResult::Identical, String::new()))
    }
}

pub struct CairoFormatter {
    formatter_config: FormatterConfig,
}

impl CairoFormatter {
    pub fn new(config: FormatterConfig) -> Self {
        Self { formatter_config: config }
    }

    /// Returns a preconfigured `ignore::WalkBuilder` for the given path.
    pub fn walk(path: &Path) -> WalkBuilder {
        let mut builder = WalkBuilder::new(path);
        builder.add_custom_ignore_filename(CAIRO_FMT_IGNORE);
        builder.follow_links(false);
        builder.skip_stdout(true);

        let mut types_builder = TypesBuilder::new();
        types_builder.select(CAIRO_FILE_EXTENSION);
        builder.types(types_builder.build().unwrap());

        builder
    }

    /// Verifies that the path is formatted correctly.
    pub fn check(&self, input: Box<dyn FormattableInput>) -> Result<FormatResult> {
        input.check()
    }

    /// Formats the path in place, writing changes to the files.
    pub fn format_in_place(&self, input: Box<dyn FormattableInput>) -> Result<FormatResult> {
        input.format_in_place()
    }

    /// Formats the path and returns the formatted string.
    pub fn format_to_string(
        &self,
        input: Box<dyn FormattableInput>,
    ) -> Result<(FormatResult, String)> {
        input.format_to_string()
    }
}
