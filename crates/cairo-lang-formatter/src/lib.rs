//! Cairo formatter.
//!
//! This crate is responsible for formatting Cairo code.
pub mod formatter;
pub mod node_properties;

use std::sync::Arc;

use cairo_lang_diagnostics::DiagnosticsBuilder;
use cairo_lang_filesystem::ids::{FileLongId, VirtualFile};
use cairo_lang_parser::parser::Parser;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{SyntaxNode, TypedSyntaxNode};

use crate::formatter::Formatter;

#[cfg(test)]
mod test;

/// Returns the formatted syntax tree as a string.
/// # Arguments
/// * `db` - The syntax group.
/// * `syntax_root` - The syntax root.
/// * `config` - The formatter configuration.
/// # Returns
/// * `String` - The formatted file.
pub fn get_formatted_file(
    db: &dyn SyntaxGroup,
    syntax_root: &SyntaxNode,
    config: FormatterConfig,
) -> String {
    let mut formatter = Formatter::new(db, config);
    formatter.format_node(syntax_root, false);
    formatter.get_result()
}

/// Formats Cairo code given as a string.
/// # Arguments
/// * `db` - The syntax group.
/// * `content` - The code to format.
/// # Returns
/// * `String` - The formatted code.
pub fn format_string(db: &dyn SyntaxGroup, content: String) -> String {
    let virtual_file = db.upcast().intern_file(FileLongId::Virtual(VirtualFile {
        parent: None,
        name: "string_to_format".into(),
        content: Arc::new(content.clone()),
    }));
    let mut diagnostics = DiagnosticsBuilder::new();
    let syntax_root =
        Parser::parse_file(db, &mut diagnostics, virtual_file, content.as_str()).as_syntax_node();
    get_formatted_file(db, &syntax_root, FormatterConfig::default())
}

#[derive(Clone)]
pub struct FormatterConfig {
    tab_size: usize,
    max_line_length: usize,
}

// Config params
// TODO(Gil): export to file and load from file
const TAB_SIZE: usize = 4;
const MAX_LINE_LENGTH: usize = 100;

impl FormatterConfig {
    pub fn new(tab_size: usize, max_line_length: usize) -> Self {
        Self { tab_size, max_line_length }
    }
}
impl Default for FormatterConfig {
    fn default() -> Self {
        Self::new(TAB_SIZE, MAX_LINE_LENGTH)
    }
}
