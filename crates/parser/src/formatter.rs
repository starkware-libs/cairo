use core::fmt;
use std::cmp;

use itertools::Itertools;
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

#[derive(Clone)]
pub struct BreakPointProperties {
    pub precedence: usize,
    pub dangling: bool,
    pub is_list: bool,
}

impl BreakPointProperties {
    pub fn new(precedence: usize, dangling: bool, is_list: bool) -> Self {
        Self { precedence, dangling, is_list }
    }
}

/// The possible parts of lines trees.
#[derive(Clone)]
enum LineComponent {
    /// A simple string to be printed.
    Token(String),
    /// An internal LineTree, everything inside won't be broken unless no breakpoints in top level.
    /// For example, ExprParenthesized will be collected into an internal LineTree and will be
    /// broken only there are no operators in the top level of the LineTree.
    Internal(LineTree),
    /// Represent a space in the code.
    Space,
    /// Represent a leading indent.
    Indent(usize),
    /// An optional breakpoint, that will be used if the line is too long.
    BreakPoint(BreakPointProperties),
}

impl LineComponent {
    pub fn width(&self) -> usize {
        match self {
            Self::Token(s) => s.len(),
            Self::Internal(tree) => tree.width(),
            Self::Space => 1,
            Self::Indent(n) => *n,
            Self::BreakPoint(_) => 0,
        }
    }
}

impl fmt::Display for LineComponent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Token(s) => write!(f, "{s}"),
            Self::Internal(tree) => write!(f, "{tree}"),
            Self::Space => write!(f, " "),
            Self::Indent(n) => write!(f, "{}", " ".repeat(*n)),
            Self::BreakPoint(_) => write!(f, ""),
        }
    }
}

/// An intermidiate representation of a code line. Used to accomulate the line parts,
/// and break it if too long.
#[derive(Clone)]
struct LineTree {
    children: Vec<LineComponent>,
    is_sub_tree_open: bool,
}

impl fmt::Display for LineTree {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.children.iter().map(|child| child.to_string()).join(""))
    }
}

impl LineTree {
    pub fn new() -> Self {
        Self { children: vec![], is_sub_tree_open: false }
    }
    pub fn clear(&mut self) {
        self.children.clear();
        self.is_sub_tree_open = false;
    }
    pub fn is_empty(&self) -> bool {
        self.children.is_empty()
    }
    /// Adds a a subtree as the next child.
    /// All subsequent children will be added to this sub tree until set as closed.
    fn open_sub_tree(&mut self) {
        if self.is_sub_tree_open {
            match self.children.last_mut() {
                Some(LineComponent::Internal(tree)) => {
                    tree.open_sub_tree();
                }
                _ => unreachable!(),
            }
        } else {
            self.push_child(LineComponent::Internal(LineTree::new()));
            self.is_sub_tree_open = true;
        }
    }
    /// Sets the last child, which is assumed to be a LineTree, as close.
    /// New children will be siblings of the this subtree.
    fn close_sub_tree(&mut self) {
        match self.children.last_mut() {
            Some(LineComponent::Internal(tree)) => {
                if tree.is_sub_tree_open {
                    tree.close_sub_tree();
                } else {
                    self.is_sub_tree_open = false;
                }
            }
            _ => unreachable!(),
        }
    }
    /// Adds a line component as the next child. If the last child is an open LineTree the child is
    /// recursivly appended to the sub tree.
    fn push_child(&mut self, new_comp: LineComponent) {
        if self.is_sub_tree_open {
            match self.children.last_mut() {
                Some(LineComponent::Internal(tree)) => tree.push_child(new_comp),
                _ => unreachable!(),
            }
        } else {
            self.children.push(new_comp);
        }
    }
    pub fn push_str(&mut self, s: &str) {
        self.push_child(LineComponent::Token(s.to_string()));
    }
    pub fn push_space(&mut self) {
        self.push_child(LineComponent::Space);
    }
    pub fn push_indent(&mut self, n: usize) {
        self.push_child(LineComponent::Indent(n));
    }
    pub fn push_break_point(&mut self, properties: BreakPointProperties) {
        self.push_child(LineComponent::BreakPoint(properties));
    }
    /// The width, in number of chars, of the whole LineTree.
    pub fn width(&self) -> usize {
        self.width_between(0, self.children.len())
    }
    /// The width, in number of chars, between the s-th (inclusive) and t-th (exclusive) children.
    pub fn width_between(&self, s: usize, t: usize) -> usize {
        self.children[s..t].iter().fold(0, |sum, node| sum + node.width())
    }
    /// Returns the minimum break point precidence from within all the break points
    /// which are a direct child of this tree, or None if there are no such break points.
    pub fn get_min_break_precedence(&self) -> Option<usize> {
        let mut min_properties: Option<usize> = None;
        for child in self.children.iter() {
            if let LineComponent::BreakPoint(properties) = child {
                min_properties = Some(cmp::min(
                    properties.precedence,
                    min_properties.unwrap_or(properties.precedence),
                ));
            }
        }
        min_properties
    }
    /// Retruns a vector of the positions of all the break point children which have the specified
    /// precedence.
    pub fn get_break_point_positions_by_precedence(&self, precedence: usize) -> Vec<usize> {
        let mut positions = vec![];
        for (i, child) in self.children.iter().enumerate() {
            if let LineComponent::BreakPoint(properties) = child {
                if properties.precedence == precedence {
                    positions.push(i);
                }
            }
        }
        positions
    }
    /// Returns a vector of the positions of all the break point children which have the minimum
    /// precedence from within all the break point children.
    pub fn get_min_precedence_break_point_positions(&self) -> Vec<usize> {
        if let Some(precedence) = self.get_min_break_precedence() {
            self.get_break_point_positions_by_precedence(precedence)
        } else {
            vec![]
        }
    }
    /// Returns a vec of strings, representing the code in the LineTree,
    /// each one with len < max_Width (if possible).
    pub fn to_broken_string_by_width(&self, max_line_width: usize, tab_size: usize) -> Vec<String> {
        if self.width() < max_line_width {
            return vec![self.to_string()];
        }
        let mut sub_trees = self.to_broken_tree_by_width(max_line_width, tab_size);
        while sub_trees.len() == 1 {
            if !sub_trees[0].is_flat() {
                sub_trees =
                    sub_trees[0].flatten().to_broken_tree_by_width(max_line_width, tab_size);
            } else {
                // Can't break tree to fit within width
                // TODO(Gil): raise something
                return vec![self.to_string()];
            }
        }
        // Keep breaking recursivley the new lines if they are still too long
        sub_trees
            .iter()
            .flat_map(|tree| tree.to_broken_string_by_width(max_line_width, tab_size))
            .collect()
    }

    /// Breaks the LineTree into a vector of LineTrees
    /// according to the lowest precidence breakpoint found in the LineTree.
    pub fn to_broken_tree_by_width(&self, max_line_width: usize, tab_size: usize) -> Vec<LineTree> {
        let mut breaking_positions = self.get_min_precedence_break_point_positions();
        if breaking_positions.is_empty() {
            return vec![self.clone()];
        }
        let mut is_dangling_break =
            if let LineComponent::BreakPoint(properties) = &self.children[breaking_positions[0]] {
                properties.dangling
            } else {
                unreachable!();
            };
        let mut trees: Vec<LineTree> = vec![LineTree::new()];
        let mut added_indent = 0;
        let mut prev_position = 0;
        // Dangling break is overridden if it will cause the line to still be too long.
        if is_dangling_break
            && (breaking_positions.len() == 1
                || self.width_between(0, breaking_positions[1]) > max_line_width)
        {
            is_dangling_break = false;
            added_indent = tab_size;
        }

        breaking_positions.push(self.children.len()); // Dummy break point, makes the loop prettier

        // Iterate over the break points and collect each part between them into one new LineTree
        for (i, position) in breaking_positions.iter().enumerate() {
            for j in prev_position..*position {
                match &self.children[j] {
                    LineComponent::Space => {
                        if !trees.last_mut().unwrap().is_only_indents() {
                            trees.last_mut().unwrap().push_space();
                        }
                    }
                    _ => trees.last_mut().unwrap().push_child(self.children[j].clone()),
                }
            }
            if i == 0 && is_dangling_break {
                added_indent = trees.last_mut().unwrap().width();
            } else if *position < self.children.len() {
                if let LineComponent::BreakPoint(properties) = &self.children[*position] {
                    if properties.is_list {
                        if i == 0 {
                            added_indent += tab_size;
                        }
                        if i == breaking_positions.len() - 2 {
                            added_indent -= tab_size;
                        }
                    }
                    trees.push(LineTree::new());
                    if added_indent > 0 {
                        trees.last_mut().unwrap().push_indent(added_indent);
                    }
                } else {
                    unreachable!();
                }
            }
            prev_position = position + 1;
        }
        trees
    }

    /// Creates a new LineTree where each subchild which is a LineTree, is replaced by all
    /// its children.
    fn flatten(&self) -> LineTree {
        let mut flattened_tree = LineTree::new();
        for child in self.children.iter() {
            match child {
                LineComponent::Internal(sub_tree) => {
                    for sub_child in sub_tree.children.iter() {
                        flattened_tree.push_child(sub_child.clone());
                    }
                }
                // Breakpoints which are direct children of self are removed.
                LineComponent::BreakPoint(_) => {}
                _ => flattened_tree.push_child(child.clone()),
            }
        }
        flattened_tree
    }
    /// Returns whether or not the line contains an internal LineTree.
    fn is_flat(&self) -> bool {
        for child in self.children.iter() {
            if let LineComponent::Internal(_) = child {
                return false;
            }
        }
        true
    }
    /// Returns whether or not the line contains only indent type childrean.
    fn is_only_indents(&self) -> bool {
        for child in self.children.iter() {
            match child {
                LineComponent::Indent(_) => {}
                _ => {
                    return false;
                }
            }
        }
        true
    }
}

/// A struct holding all the data of the pending line to be emitted.
/// TODO(Gil): change to a more complex struct to handle line breaking.
struct PendingLineState {
    /// Intermidiate representation of the text to be emitted.
    line_buffer: LineTree,
    /// Should the next space between tokens be ignored.
    no_space_after: bool,
    /// Current indentation of the produced line.
    indentation: String,
}

impl PendingLineState {
    pub fn new() -> Self {
        Self { line_buffer: LineTree::new(), no_space_after: true, indentation: String::new() }
    }
    /// Resets the line state to a clean state.
    pub fn reset(&mut self, indentation: String) {
        self.indentation = indentation;
        self.line_buffer.clear();
        self.no_space_after = true;
    }
    pub fn is_empty(&self) -> bool {
        self.line_buffer.is_empty()
    }
}

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
    /// Returns true if there should be an optional breakpoint before the node.
    fn add_break_point_before(&self, db: &dyn SyntaxGroup) -> bool;
    /// Returns true if there should be an optional breakpoint after the node.
    fn add_break_point_after(&self, db: &dyn SyntaxGroup) -> bool;
    /// Returns true if the list is optionally breakable.
    /// Only applicable for separated lists kind nodes.
    fn is_breakable_list(&self, db: &dyn SyntaxGroup) -> bool;
    /// Returns the BreakPointProperties associated with the specipic node kind
    fn get_break_point_properties(&self, db: &dyn SyntaxGroup) -> BreakPointProperties;
    /// Returns true if the node is protected from breaking unless no other break points exsists.
    /// For example break points inside ExprParenthesized should only be used if there are no break
    /// points outside the the parenthesis.
    /// Only applicable for internal nodes.
    fn protected_breaking_node(&self, db: &dyn SyntaxGroup) -> bool;
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

        if syntax_node.protected_breaking_node(self.db) {
            self.line_state.line_buffer.open_sub_tree();
        }
        if syntax_node.is_breakable_list(self.db) {
            self.append_break_point(syntax_node.get_break_point_properties(self.db));
        }
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
            // If this is a breakable list is breakable a breakpoint is added after each separator
            if i % 2 == 1 && i != n_children - 1 && syntax_node.is_breakable_list(self.db) {
                self.append_break_point(syntax_node.get_break_point_properties(self.db));
            }
        }
        if syntax_node.is_breakable_list(self.db) {
            self.append_break_point(syntax_node.get_break_point_properties(self.db));
        }
        if syntax_node.protected_breaking_node(self.db) {
            self.line_state.line_buffer.close_sub_tree();
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
                // TODO(gil): Handle SingleLineComments in between a breakable list.
                ast::Trivium::SingleLineComment(_) => {
                    // TODO(gil): add to config params
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
            self.line_state.line_buffer.push_space();
        }
        self.line_state.no_space_after = no_space_after;
        if syntax_node.add_break_point_before(self.db) {
            self.append_break_point(syntax_node.get_break_point_properties(self.db));
        }
        self.line_state.line_buffer.push_str(&text);
        if syntax_node.add_break_point_after(self.db) {
            self.append_break_point(syntax_node.get_break_point_properties(self.db));
        }
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
    fn append_break_point(&mut self, properties: BreakPointProperties) {
        self.line_state.line_buffer.push_break_point(properties);
    }
    fn finalize_line(&mut self) {
        let lines = self.line_state.line_buffer.to_broken_string_by_width(
            self.config.max_line_length - self.current_indent,
            self.config.tab_size,
        );
        for line in lines {
            if !line.is_empty() {
                self.result.push_str(&self.get_indentation());
                self.result.push_str(&line);
            }
            self.append_newline();
        }
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
