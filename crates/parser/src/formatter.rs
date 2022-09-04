use syntax::node::db::SyntaxGroup;
use syntax::node::kind::SyntaxKind;
use syntax::node::{SyntaxNode, SyntaxNodeDetails};
use syntax::token::{self};

use crate::formatter_node_properties::SyntaxNodeFormat;

#[cfg(test)]
#[path = "formatter_test.rs"]
mod test;

pub fn get_formatted_file(
    db: &dyn SyntaxGroup,
    syntax_root: &SyntaxNode,
    config: FormatterConfig,
) -> String {
    let mut formatter = Formatter::new(db, config);
    formatter.format_tree(syntax_root, 0, &mut NodePath::default());
    formatter.result
}

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

/// A struct holding all the data of the next line to be printed.
/// Will be changed to a more complex struct to handle line breaking.
struct FormattedLineBuffer {
    text: String,
    ignore_next_space: bool,
}

impl FormattedLineBuffer {
    pub fn new() -> Self {
        Self { text: String::new(), ignore_next_space: true }
    }
    pub fn reset(&mut self) {
        self.text.clear();
        self.ignore_next_space = true;
    }
}

/// A list of all the node kinds (top-down) leading to the current node
/// includeing the current node, excluding tokens.
/// Temporary until tokens are refactored to be syntax_nodes and will have a parent ptr.
// TODO(Gil): Replace with parent calls when node/token refactor is done.
pub struct NodePath {
    kind_path: Vec<SyntaxKind>,
    pub is_leading_trivia: bool,
}

impl NodePath {
    pub fn new(kind_path: Vec<SyntaxKind>, is_leading_trivia: bool) -> Self {
        Self { kind_path, is_leading_trivia }
    }
    pub fn push(&mut self, kind: SyntaxKind) {
        self.kind_path.push(kind);
    }
    pub fn pop(&mut self) {
        self.kind_path.pop();
    }
    pub fn is_nth_ancestor_of_kind(&self, n: usize, kind: SyntaxKind) -> bool {
        if n >= self.kind_path.len() {
            false
        } else {
            self.kind_path[self.kind_path.len() - n - 1] == kind
        }
    }
    pub fn is_parent_of_kind(&self, kind: SyntaxKind) -> bool {
        self.is_nth_ancestor_of_kind(1, kind)
    }
}

impl Default for NodePath {
    fn default() -> Self {
        Self::new(vec![], false)
    }
}

struct Formatter<'a> {
    db: &'a dyn SyntaxGroup,
    result: String,
    config: FormatterConfig,
    line_buffer: FormattedLineBuffer,
    /// A list of precomputed indent strings (i.e. spaces) for reasonable indent size
    indents_list: Vec<String>,
}

impl<'a> Formatter<'a> {
    fn new(db: &'a dyn SyntaxGroup, config: FormatterConfig) -> Self {
        let indents_list = get_indents_list(&config);
        Self {
            db,
            result: String::new(),
            config,
            line_buffer: FormattedLineBuffer::new(),
            indents_list,
        }
    }
    fn format_tree(
        &mut self,
        syntax_node: &SyntaxNode,
        current_indent: usize, // In tabs
        current_node_path: &mut NodePath,
    ) {
        match syntax_node.details(self.db) {
            SyntaxNodeDetails::Token(token) => {
                self.format_token(&token, current_indent, current_node_path);
            }
            SyntaxNodeDetails::Syntax(kind) => {
                self.format_internal(syntax_node, kind, current_indent, current_node_path);
            }
        }
    }
    /// Should only be called with an internal syntax_node (i.e. non-token)
    fn format_internal(
        &mut self,
        syntax_node: &SyntaxNode,
        node_kind: SyntaxKind,
        current_indent: usize,
        current_node_path: &mut NodePath,
    ) {
        if syntax_node.should_ignore(self.db, current_node_path) {
            return;
        }
        current_node_path.push(node_kind);
        let indent_change = syntax_node.should_change_indent(self.db, current_node_path) as usize;

        for (i, child) in syntax_node.children(self.db).enumerate() {
            if node_kind == SyntaxKind::Terminal {
                current_node_path.is_leading_trivia = i == 0; // First child of a terminal node
                // is a leading trivia
            }
            self.format_tree(&child, current_indent + indent_change, current_node_path);
        }
        if syntax_node.force_line_break(self.db, current_node_path) {
            self.finalize_line(current_indent);
        }
        current_node_path.pop();
    }
    fn format_token(
        &mut self,
        token: &token::Token,
        current_indent: usize,
        current_node_path: &NodePath,
    ) {
        if token.should_ignore(self.db, current_node_path) {
            return;
        }
        self.append_token(token, current_node_path);
        if token.force_line_break(self.db, current_node_path) {
            self.finalize_line(current_indent);
        }
    }
    fn append_token(&mut self, token: &token::Token, current_node_path: &NodePath) {
        if !(token.force_no_space_before(self.db, current_node_path)
            || self.line_buffer.ignore_next_space)
        {
            self.line_buffer.text += " ";
        }
        self.line_buffer.ignore_next_space = token.force_no_space_after(self.db, current_node_path);
        self.line_buffer.text += &token.text;
    }
    fn append_indentation(&mut self, current_indent: usize) {
        if current_indent < self.indents_list.len() {
            self.result += &self.indents_list[current_indent];
        } else {
            self.result += &" ".repeat(self.config.tab_size * current_indent);
        }
    }
    fn append_newline(&mut self) {
        self.result.push('\n');
    }
    fn finalize_line(&mut self, current_indent: usize) {
        self.append_indentation(current_indent);
        self.result.push_str(&self.line_buffer.text);
        self.append_newline();
        self.line_buffer.reset();
    }
}

/// Generates the leading indents for reasonable indent sizes
fn get_indents_list(config: &FormatterConfig) -> Vec<String> {
    let mut indent_list: Vec<String> = vec!["".to_string()];
    let tab = " ".repeat(config.tab_size);
    for i in 1..config.max_line_length / config.tab_size {
        indent_list.push(tab.repeat(i).to_string());
    }
    indent_list
}
