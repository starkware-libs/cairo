use smol_str::SmolStr;
use syntax::node::db::SyntaxGroup;
use syntax::node::kind::SyntaxKind;
use syntax::node::SyntaxNode;

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

/// A struct holding all the data of the pending line to be emitted.
/// TODO(Gil): change to a more complex struct to handle line breaking.
struct PendingLineState {
    /// The text to be emitted.
    text: String,
    /// Should the next space between tokens be ignored.
    force_no_space_after: bool,
}

impl PendingLineState {
    pub fn new() -> Self {
        Self { text: String::new(), force_no_space_after: true }
    }
    /// Resets the line state to a clean state.
    pub fn reset(&mut self) {
        self.text.clear();
        self.force_no_space_after = true;
    }
}

/// A list of all the node kinds (root-down) leading to the current node
/// including the root and the current node, excluding tokens.
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
    /// Checks if the nth ancestor syntax kind is of a certain kind.
    /// It is 1-indexed, i.e. if n==1 the check is for the direct parent.
    pub fn is_nth_ancestor_of_kind(&self, n: usize, kind: SyntaxKind) -> bool {
        if n >= self.kind_path.len() {
            false
        } else {
            self.kind_path[self.kind_path.len() - n - 1] == kind
        }
    }
    /// Checks if the direct parent syntax kind is of a certain kind.
    pub fn is_parent_of_kind(&self, kind: SyntaxKind) -> bool {
        self.is_nth_ancestor_of_kind(1, kind)
    }
}

impl Default for NodePath {
    fn default() -> Self {
        Self::new(vec![], false)
    }
}

pub trait SyntaxNodeFormat {
    /// Returns true if a token should never have a space before it.
    /// Only applicable for token nodes.
    fn force_no_space_before(&self, db: &dyn SyntaxGroup, node_path: &NodePath) -> bool;
    /// Returns true if a token should never have a space after it.
    /// Only applicable for token nodes.
    fn force_no_space_after(&self, db: &dyn SyntaxGroup, node_path: &NodePath) -> bool;
    /// Returns true if the children of a node should be indented inwards relative to the parent.
    /// Only applicable for internal nodes.
    fn should_change_indent(&self, db: &dyn SyntaxGroup, node_path: &NodePath) -> bool;
    /// Returns true if the line should break after the node. Results in a call to finalize_line.
    fn force_line_break(&self, db: &dyn SyntaxGroup, node_path: &NodePath) -> bool;
    /// Returns true if the token should be ignored (e.g. spaces).
    /// Only applicable for token nodes.
    fn should_ignore(&self, db: &dyn SyntaxGroup, node_path: &NodePath) -> bool;
}

struct Formatter<'a> {
    db: &'a dyn SyntaxGroup,
    result: String,
    config: FormatterConfig,
    line_state: PendingLineState,
    /// A list of precomputed indent strings (i.e. spaces) for reasonable indent sizes.
    /// The item in index `i` is a string representing `i` tabs.
    indents_list: Vec<String>,
}

impl<'a> Formatter<'a> {
    fn new(db: &'a dyn SyntaxGroup, config: FormatterConfig) -> Self {
        let indents_list = generate_indents_list(&config);
        Self {
            db,
            result: String::new(),
            config,
            line_state: PendingLineState::new(),
            indents_list,
        }
    }
    fn format_tree(
        &mut self,
        syntax_node: &SyntaxNode,
        current_indent: usize, // In tabs
        current_node_path: &mut NodePath,
    ) {
        let green_node = syntax_node.green_node(self.db);
        let kind = green_node.kind;
        if kind.should_ignore(self.db, current_node_path) {
            return;
        }
        match green_node.details {
            syntax::node::green::GreenNodeDetails::Token(text) => {
                self.format_token(text, kind, current_indent, current_node_path);
            }
            syntax::node::green::GreenNodeDetails::Node { .. } => {
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
        current_node_path.push(node_kind);
        let indent_change =
            if node_kind.should_change_indent(self.db, current_node_path) { 1 } else { 0 };

        for (i, child) in syntax_node.children(self.db).enumerate() {
            if node_kind.is_terminal() {
                // First child of a terminal node is a leading trivia
                current_node_path.is_leading_trivia = i == 0;
            }
            self.format_tree(&child, current_indent + indent_change, current_node_path);
        }
        if node_kind.force_line_break(self.db, current_node_path) {
            self.finalize_line(current_indent);
        }
        current_node_path.pop();
    }
    fn format_token(
        &mut self,
        text: SmolStr,
        kind: SyntaxKind,
        current_indent: usize,
        current_node_path: &NodePath,
    ) {
        self.append_token(text, kind, current_node_path);
        // TODO(Gil) Consider removing this and use terminal instead after the tokens refactor.
        if kind.force_line_break(self.db, current_node_path) {
            self.finalize_line(current_indent);
        }
    }
    fn append_token(&mut self, text: SmolStr, kind: SyntaxKind, current_node_path: &NodePath) {
        if !kind.force_no_space_before(self.db, current_node_path)
            && !self.line_state.force_no_space_after
        {
            self.line_state.text += " ";
        }
        self.line_state.force_no_space_after =
            kind.force_no_space_after(self.db, current_node_path);
        self.line_state.text += &text;
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
        // The line is never empty, so we can safely add indentation.
        self.append_indentation(current_indent);
        self.result.push_str(&self.line_state.text);
        self.append_newline();
        self.line_state.reset();
    }
}

/// Generates the leading indents for reasonable indent sizes
fn generate_indents_list(config: &FormatterConfig) -> Vec<String> {
    let mut indent_list: Vec<String> = vec![];
    let tab = " ".repeat(config.tab_size);
    for i in 0..(config.max_line_length / config.tab_size) {
        indent_list.push(tab.repeat(i).to_string());
    }
    indent_list
}
