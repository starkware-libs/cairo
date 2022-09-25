use smol_str::SmolStr;
use syntax::node::db::SyntaxGroup;
use syntax::node::{ast, SyntaxNode, TypedSyntaxNode};

#[cfg(test)]
#[path = "formatter_test.rs"]
mod test;

pub fn get_formatted_file(
    db: &dyn SyntaxGroup,
    syntax_root: &SyntaxNode,
    config: FormatterConfig,
) -> String {
    let mut formatter = Formatter::new(db, config);
    formatter.format_node(syntax_root, false);
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
    no_space_after: bool,
    /// Current indentation of the produced line.
    indentation: String,
}

impl PendingLineState {
    pub fn new() -> Self {
        Self { text: String::new(), no_space_after: true, indentation: String::new() }
    }
    /// Resets the line state to a clean state.
    pub fn reset(&mut self, indentation: String) {
        self.indentation = indentation;
        self.text.clear();
        self.no_space_after = true;
    }
    /// Appends text to the current line.
    pub fn is_empty(&self) -> bool {
        self.text.is_empty()
    }
    pub fn build(&mut self) -> String {
        let Self { text, indentation, .. } = &self;
        if self.is_empty() { "".into() } else { format!("{indentation}{text}") }
    }
}

// TODO(spapini): Intorduce the correct types here, to reflect the "applicable" nodes types.
pub trait SyntaxNodeFormat {
    /// Returns true if a token should never have a space before it.
    /// Only applicable for token nodes.
    fn force_no_space_before(&self, db: &dyn SyntaxGroup) -> bool;
    /// Returns true if a token should never have a space after it.
    /// Only applicable for token nodes.
    fn force_no_space_after(&self, db: &dyn SyntaxGroup) -> bool;
    /// Returns true if the children of a node should be indented inwards relative to the parent.
    /// Only applicable for internal nodes.
    fn should_change_indent(&self, db: &dyn SyntaxGroup) -> bool;
    /// Returns true if the line should break after the node.
    fn force_line_break(&self, db: &dyn SyntaxGroup) -> bool;
    /// Returns true if the line is allowed to break after the node.
    /// Only applicable for terminal nodes.
    fn allow_newline_after(&self, db: &dyn SyntaxGroup) -> bool;
    /// Returns the number of allowed empty lines between two consecutive children of this node.
    fn allowed_empty_between(&self, db: &dyn SyntaxGroup) -> usize;
}

struct Formatter<'a> {
    db: &'a dyn SyntaxGroup,
    result: String,
    config: FormatterConfig,
    line_state: PendingLineState,
    /// A list of precomputed indent strings (i.e. spaces) for reasonable indent sizes.
    /// The item in index `i` is a string representing `i` tabs.
    indents_list: Vec<String>,
    /// Current indentation in tabs.
    current_indent: usize,
    empty_lines_allowance: usize,
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
            current_indent: 0,
            empty_lines_allowance: 0,
        }
    }
    fn format_node(&mut self, syntax_node: &SyntaxNode, no_space_after: bool) {
        if syntax_node.text(self.db).is_some() {
            panic!("Token reached before terminal.");
        }
        if syntax_node.kind(self.db).is_terminal() {
            self.format_terminal(syntax_node, no_space_after);
        } else {
            self.format_internal(syntax_node, no_space_after);
        }

        if syntax_node.force_line_break(self.db) && !self.line_state.is_empty() {
            self.finalize_line();
        }
    }

    fn format_internal(&mut self, syntax_node: &SyntaxNode, no_space_after: bool) {
        let indent_change = if syntax_node.should_change_indent(self.db) { 1 } else { 0 };
        let allowed_empty_between = syntax_node.allowed_empty_between(self.db);
        let no_space_after = no_space_after || syntax_node.force_no_space_after(self.db);

        let children = syntax_node.children(self.db);
        let n_children = children.len();
        for (i, child) in children.enumerate() {
            self.current_indent += indent_change;
            if self.line_state.is_empty() {
                self.line_state.reset(self.get_indentation())
            }
            self.format_node(&child, no_space_after && i == n_children - 1);
            self.empty_lines_allowance = allowed_empty_between;
            self.current_indent -= indent_change;
        }
    }
    fn format_terminal(&mut self, syntax_node: &SyntaxNode, no_space_after: bool) {
        // TODO(spapini): Introduce a Terminal and a Token enum in ast.rs to make this cleaner.
        let mut children = syntax_node.children(self.db);
        let leading_trivia = ast::Trivia::from_syntax_node(self.db, children.next().unwrap());
        let token = children.next().unwrap();
        let trailing_trivia = ast::Trivia::from_syntax_node(self.db, children.next().unwrap());

        // The first newlines is the leading trivia correspond exactly to empty lines.
        self.format_trivia(leading_trivia, self.empty_lines_allowance);
        self.empty_lines_allowance = 0;
        self.format_token(&token, no_space_after || syntax_node.force_no_space_after(self.db));
        let allowed_newlines = if syntax_node.allow_newline_after(self.db) { 1 } else { 0 };
        self.format_trivia(trailing_trivia, allowed_newlines);
    }
    fn format_trivia(&mut self, trivia: syntax::node::ast::Trivia, mut allowed_newlines: usize) {
        for trivium in trivia.elements(self.db) {
            match trivium {
                ast::Trivium::SingleLineComment(_) => {
                    allowed_newlines = 2;
                    self.format_token(&trivium.as_syntax_node(), true);
                }
                ast::Trivium::Whitespace(_) => {}
                ast::Trivium::Newline(_) => {
                    if allowed_newlines > 0 {
                        allowed_newlines -= 1;
                        self.finalize_line();
                    }
                }
                ast::Trivium::Skipped(_) => {
                    self.format_token(&trivium.as_syntax_node(), false);
                }
            }
        }
    }
    fn format_token(&mut self, syntax_node: &SyntaxNode, no_space_after: bool) {
        let no_space_after = no_space_after || syntax_node.force_no_space_after(self.db);
        let text = syntax_node.text(self.db).unwrap();
        self.append_token(text, syntax_node, no_space_after);
    }
    fn append_token(&mut self, text: SmolStr, syntax_node: &SyntaxNode, no_space_after: bool) {
        if !syntax_node.force_no_space_before(self.db) && !self.line_state.no_space_after {
            self.line_state.text += " ";
        }
        self.line_state.no_space_after = no_space_after;
        self.line_state.text += &text;
    }
    fn get_indentation(&self) -> String {
        if self.current_indent < self.indents_list.len() {
            self.indents_list[self.current_indent].clone()
        } else {
            " ".repeat(self.config.tab_size * self.current_indent)
        }
    }
    fn append_newline(&mut self) {
        self.result.push('\n');
    }
    fn finalize_line(&mut self) {
        self.result.push_str(&self.line_state.build());
        self.append_newline();
        self.line_state.reset(self.get_indentation());
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
