//! Cairo formatter.
//!
//! This crate is responsible for formatting Cairo code.
pub mod cairo_formatter;
pub mod formatter_impl;
pub mod node_properties;

use cairo_lang_diagnostics::DiagnosticsBuilder;
use cairo_lang_filesystem::ids::{FileKind, FileLongId, VirtualFile};
use cairo_lang_parser::parser::Parser;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{SyntaxNode, TypedSyntaxNode};
use cairo_lang_utils::Intern;
use serde::{Deserialize, Serialize};

pub use crate::cairo_formatter::{CairoFormatter, FormatOutcome, StdinFmt};
use crate::formatter_impl::FormatterImpl;

#[cfg(test)]
mod test;

pub const CAIRO_FMT_IGNORE: &str = ".cairofmtignore";

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
    let mut formatter = FormatterImpl::new(db, config);
    formatter.get_formatted_string(syntax_root)
}

/// Formats Cairo code given as a string.
/// # Arguments
/// * `db` - The syntax group.
/// * `content` - The code to format.
/// # Returns
/// * `String` - The formatted code.
pub fn format_string(db: &dyn SyntaxGroup, content: String) -> String {
    let virtual_file = FileLongId::Virtual(VirtualFile {
        parent: None,
        name: "string_to_format".into(),
        content: content.clone().into(),
        code_mappings: [].into(),
        kind: FileKind::Module,
    })
    .intern(db);
    let mut diagnostics = DiagnosticsBuilder::default();
    let syntax_root =
        Parser::parse_file(db, &mut diagnostics, virtual_file, content.as_str()).as_syntax_node();
    get_formatted_file(db, &syntax_root, FormatterConfig::default())
}

/// This enum is used to control how multi-element collections (i.e. arrays, tuples)
/// are broken into lines. It provides two options: `SingleBreakPoint` and `LineByLine`, allowing
/// flexible configuration based on desired readability or space efficiency.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum CollectionsBreakingBehavior {
    /// Keeps all elements of the collection on a single line, where possible.
    SingleBreakPoint,
    /// Breaks each element of the collection onto a new line for improved readability.
    LineByLine,
}

/// Impl CollectionsBreakingBehavior from bool, where true is `LineByLine` and false is
/// `SingleBreakPoint`. This adheres to the existing behavior of the formatter CLI.
impl From<bool> for CollectionsBreakingBehavior {
    fn from(b: bool) -> Self {
        if b {
            CollectionsBreakingBehavior::LineByLine
        } else {
            CollectionsBreakingBehavior::SingleBreakPoint
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct FormatterConfig {
    tab_size: usize,
    max_line_length: usize,
    sort_module_level_items: bool,
    tuple_breaking_behavior: CollectionsBreakingBehavior,
    fixed_array_breaking_behavior: CollectionsBreakingBehavior,
    merge_use_items: bool,
    allow_duplicate_uses: bool,
}

// Config params
// TODO(Gil): export to file and load from file
const TAB_SIZE: usize = 4;
const MAX_LINE_LENGTH: usize = 100;

impl FormatterConfig {
    pub fn new(
        tab_size: usize,
        max_line_length: usize,
        sort_module_level_items: bool,
        tuple_breaking_behavior: CollectionsBreakingBehavior,
        fixed_array_breaking_behavior: CollectionsBreakingBehavior,
        merge_use_items: bool,
        allow_duplicate_uses: bool,
    ) -> Self {
        Self {
            tab_size,
            max_line_length,
            sort_module_level_items,
            tuple_breaking_behavior,
            fixed_array_breaking_behavior,
            merge_use_items,
            allow_duplicate_uses,
        }
    }

    pub fn sort_module_level_items(mut self, sort_module_level_items: Option<bool>) -> Self {
        if let Some(sort) = sort_module_level_items {
            self.sort_module_level_items = sort;
        }
        self
    }

    pub fn tuple_breaking_behavior(
        mut self,
        behavior: Option<CollectionsBreakingBehavior>,
    ) -> Self {
        if let Some(behavior) = behavior {
            self.tuple_breaking_behavior = behavior;
        }
        self
    }

    pub fn fixed_array_breaking_behavior(
        mut self,
        behavior: Option<CollectionsBreakingBehavior>,
    ) -> Self {
        if let Some(behavior) = behavior {
            self.fixed_array_breaking_behavior = behavior;
        }
        self
    }
    pub fn merge_use_items(mut self, merge: Option<bool>) -> Self {
        if let Some(merge) = merge {
            self.merge_use_items = merge;
        }
        self
    }
    pub fn allow_duplicate_uses(mut self, allow: Option<bool>) -> Self {
        if let Some(allow) = allow {
            self.allow_duplicate_uses = allow;
        }
        self
    }
}
impl Default for FormatterConfig {
    fn default() -> Self {
        Self {
            tab_size: TAB_SIZE,
            max_line_length: MAX_LINE_LENGTH,
            sort_module_level_items: true,
            tuple_breaking_behavior: CollectionsBreakingBehavior::LineByLine,
            fixed_array_breaking_behavior: CollectionsBreakingBehavior::SingleBreakPoint,
            merge_use_items: true,
            allow_duplicate_uses: false,
        }
    }
}
