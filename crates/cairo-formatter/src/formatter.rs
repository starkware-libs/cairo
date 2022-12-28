use std::fmt;

use itertools::Itertools;
use smol_str::SmolStr;
use syntax::node::db::SyntaxGroup;
use syntax::node::{ast, SyntaxNode, TypedSyntaxNode};
use utils::extract_matches;

use crate::FormatterConfig;

#[derive(Clone, Debug, Copy, PartialEq, Eq)]
/// Defines the break point behaviour.
/// Defined in get_break_line_point_properties.
pub enum BreakLinePointIndentation {
    /// Represents a break line point group which should be indented when broken. For example,
    /// binary expr:
    ///
    /// let x = 1
    ///     + 2
    ///     + 3
    ///     + 4
    Indented,
    /// Represents a break line point group which should be indented when broken, except for the
    /// last one in the group. For example, the break points before and after a StructArgList
    /// indent the list itself, but the closing braces should not be indented:
    ///
    /// let x = Struct {
    ///     first_arg: first_arg, second_arg: second_arg,
    /// };
    IndentedWithTail,
    /// Represents a break line point which should not be indented when broken. For example, the
    /// break points after TerminalComma of StructArgList.
    /// Notice that the break line points StructArgList wrapping it incur indentation.
    ///
    /// let x = Struct {
    ///     first_arg: first_arg,
    ///     second_arg: second_arg,
    ///     third_arg: third_arg,
    ///     fourth_arg: fourth_arg,
    ///     fifth_arg: fifth_arg,
    /// };
    NotIndented,
}

#[derive(Clone, Debug)]
/// Properties defining the behaviour of a break line point.
pub struct BreakLinePointProperties {
    /// Breaking precedence, lower values will break first.
    pub precedence: usize,
    /// Dictates the breaking indentation behaviour.
    pub break_indentation: BreakLinePointIndentation,
}
impl BreakLinePointProperties {
    pub fn new(precedence: usize, break_indentation: BreakLinePointIndentation) -> Self {
        Self { precedence, break_indentation }
    }
}

/// The possible parts of line trees.
#[derive(Clone, Debug)]
enum LineComponent {
    /// A simple string to be printed.
    Token(String),
    /// A child LineBuilder, any break line point inside will be ignored unless
    /// it has no sibling break line points.
    /// For example, in the expression let x = 1 * (2 + 3); everything inside the parenthesis
    /// will be collected into an internal LineBuilder and will be broken only if the line is too
    /// long after the first break line point, the one before the '*' operator, is broken.
    /// More generally, any break point inside a child LineBuilder will be ignored unless there
    /// are no breakpoint which are direct children of the parent LineBuilder.
    Internal(LineBuilder),
    /// Represent a space in the code.
    Space,
    /// Represent a leading indent.
    Indent(usize),
    /// An optional break line point, that will be used if the line is too long.
    BreakLinePoint(BreakLinePointProperties),
}
impl LineComponent {
    pub fn width(&self) -> usize {
        match self {
            Self::Token(s) => s.len(),
            Self::Internal(builder) => builder.width(),
            Self::Space => 1,
            Self::Indent(n) => *n,
            Self::BreakLinePoint(_) => 0,
        }
    }
}
impl fmt::Display for LineComponent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Token(s) => write!(f, "{s}"),
            Self::Internal(builder) => write!(f, "{builder}"),
            Self::Space => write!(f, " "),
            Self::Indent(n) => write!(f, "{}", " ".repeat(*n)),
            Self::BreakLinePoint(_) => write!(f, ""),
        }
    }
}

/// Represents a line in the code, separated by optional break line points.
/// Used to break the line if too long.
#[derive(Clone, Debug)]
struct LineBuilder {
    children: Vec<LineComponent>,
    /// Indicates whether this builder is open, which means any new child should
    /// be (recursively) appended to it. Otherwise, new children will be appended as its sibling.
    is_open: bool,
    /// Added break line points are temporarly collected into this vector. The vector is flushed
    /// into the children vector if any other LineComponent is pushed. This prevents break line
    /// points being added to the end of a line.
    pending_break_line_points: Vec<LineComponent>,
}
impl fmt::Display for LineBuilder {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.children.iter().map(|child| child.to_string()).join(""))
    }
}
impl LineBuilder {
    /// Creates a new intermediate line.
    pub fn default() -> Self {
        Self { children: vec![], is_open: true, pending_break_line_points: vec![] }
    }
    /// Creates a new line builder initialized with an indent component.
    pub fn new(indent_size: usize) -> Self {
        if indent_size > 0 {
            Self {
                children: vec![LineComponent::Indent(indent_size)],
                is_open: true,
                pending_break_line_points: vec![],
            }
        } else {
            Self::default()
        }
    }
    /// Clears the line. Represent an empty line after the call.
    pub fn clear(&mut self) {
        self.children.clear();
        self.is_open = true;
    }
    /// Is the line empty.
    fn is_empty(&self) -> bool {
        self.children.is_empty()
    }
    /// Adds a a sub-builder as the next child.
    /// All subsequent children will be added to this sub builder until set as closed.
    fn open_sub_builder(&mut self) {
        let active_builder = self.get_active_builder_mut();
        active_builder.flush_pending_break_line_points();
        active_builder.push_child(LineComponent::Internal(LineBuilder::default()));
    }
    /// Sets the last child, which is assumed to be a LineBuilder, as close.
    /// New children will be siblings of this subtree.
    fn close_sub_builder(&mut self) {
        let active_builder = self.get_active_builder_mut();
        active_builder.flush_pending_break_line_points();
        active_builder.is_open = false;
    }
    /// Adds a line component as the next child. If the last child is an open LineBuilder the child
    /// is recursively appended to the sub builder.
    fn push_child(&mut self, component: LineComponent) {
        match component {
            LineComponent::BreakLinePoint(_) => {
                if !self.is_only_indents() {
                    self.get_active_builder_mut().pending_break_line_points.push(component);
                }
            }
            _ => {
                let active_builder = self.get_active_builder_mut();
                active_builder.flush_pending_break_line_points();
                active_builder.children.push(component);
            }
        }
    }
    /// Appends a string to the line.
    pub fn push_str(&mut self, s: &str) {
        self.push_child(LineComponent::Token(s.to_string()));
    }
    /// Appends a space to the line.
    pub fn push_space(&mut self) {
        self.push_child(LineComponent::Space);
    }
    /// Appends an optional break line point.
    pub fn push_break_line_point(&mut self, properties: BreakLinePointProperties) {
        self.push_child(LineComponent::BreakLinePoint(properties));
    }
    /// Appends all the pending break line points to the builder. Should be called whenever a
    /// component of another type (i.e. not a break line point) is appended.
    fn flush_pending_break_line_points(&mut self) {
        self.children.append(&mut self.pending_break_line_points);
    }
    /// The width, in number of chars, of the whole LineTree.
    fn width(&self) -> usize {
        self.width_between(0, self.children.len())
    }
    /// The width, in number of chars, between the `start`th (inclusive)
    /// and `end`th (exclusive) children.
    fn width_between(&self, start: usize, end: usize) -> usize {
        // TODO(Gil): Optimize to O(1).
        self.children[start..end].iter().fold(0, |sum, node| sum + node.width())
    }
    /// Returns the minimum break line point precedence from within all the break line points
    /// which are a direct child of this tree, or None if there are no such break line points.
    fn get_min_break_precedence(&self) -> Option<usize> {
        self.children
            .iter()
            .filter_map(|child| {
                if let LineComponent::BreakLinePoint(properties) = child {
                    Some(properties.precedence)
                } else {
                    None
                }
            })
            .min()
    }
    /// Returns a vector of the positions of all the break line point children which have the
    /// specified precedence.
    fn get_break_point_indices_by_precedence(&self, precedence: usize) -> Vec<usize> {
        self.children
            .iter()
            .enumerate()
            .filter_map(|(i, child)| match child {
                LineComponent::BreakLinePoint(properties)
                    if properties.precedence == precedence =>
                {
                    Some(i)
                }
                _ => None,
            })
            .collect()
    }
    /// Returns a vector of the positions of all the break line point children which have the
    /// minimum precedence from within all the break line point children.
    fn get_current_break_positions(&self) -> Vec<usize> {
        if let Some(precedence) = self.get_min_break_precedence() {
            self.get_break_point_indices_by_precedence(precedence)
        } else {
            vec![]
        }
    }
    /// Repeatedly calls break_line_tree until each line length is less than max_width.
    /// Returns a vec of strings, each one represents a line.
    fn break_line_tree(&self, max_line_width: usize, tab_size: usize) -> Vec<String> {
        // TODO(gil): consider using a write buffer similar to 'write!()' to reduce string
        // allocations.
        if self.width() < max_line_width {
            return vec![self.to_string()];
        }
        let mut sub_builders = self.break_line_tree_single_level(tab_size);
        // While the line is not broken into several lines, try to flatten it and then break it.
        while sub_builders.len() == 1 {
            if !sub_builders[0].is_flat() {
                sub_builders = sub_builders[0].flatten().break_line_tree_single_level(tab_size);
            } else {
                // Can't break tree to fit within width
                // TODO(Gil): Propagate error to user.
                return vec![self.to_string()];
            }
        }
        // Keep breaking recursively the new lines if they are still too long
        sub_builders
            .iter()
            .flat_map(|tree| tree.break_line_tree(max_line_width, tab_size))
            .collect()
    }
    /// Breaks the LineTree once into a vector of LineTrees according to the highest precedence
    /// (lowest precedence number) break line point found in the LineTree.
    fn break_line_tree_single_level(&self, tab_size: usize) -> Vec<LineBuilder> {
        let mut breaking_positions = self.get_current_break_positions();
        if breaking_positions.is_empty() {
            return vec![self.clone()];
        }
        let break_line_point_indentation_type =
            extract_matches!(&self.children[breaking_positions[0]], LineComponent::BreakLinePoint)
                .break_indentation;
        let base_indent = self.get_leading_indent();
        let mut trees: Vec<LineBuilder> = vec![];
        let n_children = self.children.len();
        // Dummy break line point, simplifies the loop.
        breaking_positions.push(n_children);
        let n_break_points = breaking_positions.len();

        let mut current_line_start = 0;
        // Iterate over the break line points and collect each part between them into one new
        // LineBuilder.
        for (i, current_line_end) in breaking_positions.iter().enumerate() {
            let added_indent = match break_line_point_indentation_type {
                BreakLinePointIndentation::Indented if i != 0 => tab_size,
                BreakLinePointIndentation::IndentedWithTail
                    if i != 0 && i != n_break_points - 1 =>
                {
                    tab_size
                }
                _ => 0,
            };
            trees.push(LineBuilder::new(base_indent + added_indent));
            for j in current_line_start..*current_line_end {
                match &self.children[j] {
                    LineComponent::Indent(_) => {}
                    LineComponent::Space => {
                        // Ignore spaces at the start of a line
                        if !trees.last().unwrap().is_only_indents() {
                            trees.last_mut().unwrap().push_space();
                        }
                    }
                    _ => trees.last_mut().unwrap().push_child(self.children[j].clone()),
                }
            }
            current_line_start = *current_line_end + 1;
        }
        trees
    }
    /// Returns a reference to the currently active builder.
    fn get_active_builder_mut(&mut self) -> &mut LineBuilder {
        // Splitted into two match statements since self is mutably borrowed in the second match,
        // and thus a mutable ref to self can't be returned in it.
        match self.children.last() {
            Some(LineComponent::Internal(sub_builder)) if sub_builder.is_open => {}
            _ => {
                return self;
            }
        }
        match self.children.last_mut() {
            Some(LineComponent::Internal(sub_builder)) if sub_builder.is_open => {
                sub_builder.get_active_builder_mut()
            }
            _ => {
                unreachable!("This case is covered in the first match.")
            }
        }
    }
    /// Creates a string of the code represented in the builder. The string may represent
    /// several lines (separated by '\n'), where each line length is
    /// less than max_line_width (if possible).
    /// Each line is prepended by the leading
    pub fn build(&self, max_line_width: usize, tab_size: usize, leading_indent: &str) -> String {
        self.break_line_tree(max_line_width, tab_size)
            .iter()
            .map(
                |line| {
                    if line.is_empty() { "".to_string() } else { leading_indent.to_string() + line }
                },
            )
            .join("\n")
    }
    /// Creates a new LineBuilder where the first subchild which is a LineBuilder, is replaced by
    /// all its children.
    fn flatten(&self) -> LineBuilder {
        let mut flattened_tree = LineBuilder::default();
        let mut first_tree_found = false;
        for child in self.children.iter() {
            match child {
                LineComponent::Internal(sub_tree) if !first_tree_found => {
                    first_tree_found = true;
                    for sub_child in sub_tree.children.iter() {
                        flattened_tree.push_child(sub_child.clone());
                    }
                }
                _ => flattened_tree.push_child(child.clone()),
            }
        }
        flattened_tree
    }
    /// Returns whether or not the line contains an internal LineBuilder.
    fn is_flat(&self) -> bool {
        !self.children.iter().any(|child| matches!(child, LineComponent::Internal(_)))
    }
    /// Returns whether the line contains only indents.
    fn is_only_indents(&self) -> bool {
        !self.children.iter().any(|child| !matches!(child, LineComponent::Indent(_)))
    }
    fn get_leading_indent(&self) -> usize {
        let mut leading_indent = 0;
        let mut children_iter = self.children.iter();
        while let Some(LineComponent::Indent(indent_size)) = children_iter.next() {
            leading_indent += indent_size
        }
        leading_indent
    }
}

/// A struct holding all the data of the pending line to be emitted.
/// TODO(Gil): change to a more complex struct to handle line breaking.
struct PendingLineState {
    /// Intermidiate representation of the text to be emitted.
    line_buffer: LineBuilder,
    /// Should the next space between tokens be ignored.
    no_space_after: bool,
    /// Current indentation of the produced line.
    indentation: String,
}

impl PendingLineState {
    pub fn new() -> Self {
        Self {
            line_buffer: LineBuilder::default(),
            no_space_after: true,
            indentation: String::new(),
        }
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

/// Represents the break line points before and after a syntax node.
pub struct WrappingBreakLinePoints {
    pub leading: Option<BreakLinePointProperties>,
    pub trailing: Option<BreakLinePointProperties>,
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
    /// Returns the break point properties before and after a specific node if a break point should
    /// exist, otherwise returns None.
    fn get_wrapping_break_line_point_properties(
        &self,
        db: &dyn SyntaxGroup,
    ) -> WrappingBreakLinePoints;
    /// Returns true if the node is protected from breaking unless no other break points exist.
    /// For example break points inside ExprParenthesized should only be used if there are no break
    /// points outside the parentheses. Only applicable for internal nodes.
    fn is_protected_breaking_node(&self, db: &dyn SyntaxGroup) -> bool;
}

pub struct Formatter<'a> {
    db: &'a dyn SyntaxGroup,
    result: String,
    config: FormatterConfig,
    /// A buffer for the current line.
    line_state: PendingLineState,
    /// A list of precomputed indent strings (i.e. spaces) for reasonable indent sizes.
    /// The item in index `i` is a string representing `i` tabs.
    indents_list: Vec<String>,
    /// Current indentation in tabs.
    current_indent: usize,
    /// The number of empty lines allowed after the current node.
    empty_lines_allowance: usize,
}

impl<'a> Formatter<'a> {
    pub fn new(db: &'a dyn SyntaxGroup, config: FormatterConfig) -> Self {
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
    /// Returns the result of the formatter after format_node was called.
    pub fn get_result(&self) -> String {
        self.result.clone()
    }
    /// Appends a formatted string, representing the syntax_node, to the result.
    /// Should be called with a root syntax node to format a file.
    pub fn format_node(&mut self, syntax_node: &SyntaxNode, no_space_after: bool) {
        if syntax_node.text(self.db).is_some() {
            panic!("Token reached before terminal.");
        }
        let is_protected_zone = syntax_node.is_protected_breaking_node(self.db);
        let node_break_points = syntax_node.get_wrapping_break_line_point_properties(self.db);
        self.append_break_line_point(node_break_points.leading);
        if is_protected_zone {
            self.line_state.line_buffer.open_sub_builder();
        }
        if syntax_node.kind(self.db).is_terminal() {
            self.format_terminal(syntax_node, no_space_after);
        } else {
            self.format_internal(syntax_node, no_space_after);
        }
        if is_protected_zone {
            self.line_state.line_buffer.close_sub_builder();
        }
        self.append_break_line_point(node_break_points.trailing);
        if syntax_node.force_line_break(self.db) && !self.line_state.is_empty() {
            self.finalize_line();
        }
    }
    /// Formats an internal node and appends the formatted string to the result.
    fn format_internal(&mut self, syntax_node: &SyntaxNode, no_space_after: bool) {
        let indent_change = usize::from(syntax_node.should_change_indent(self.db));
        let allowed_empty_between = syntax_node.allowed_empty_between(self.db);
        let no_space_after = no_space_after || syntax_node.force_no_space_after(self.db);
        let children = syntax_node.children(self.db);
        let n_children = children.len();
        for (i, child) in children.enumerate() {
            if child.width(self.db) == 0 {
                continue;
            }

            self.current_indent += indent_change;
            if self.line_state.is_empty() {
                self.line_state.reset(self.get_indentation())
            }
            self.format_node(&child, no_space_after && i == n_children - 1);

            self.empty_lines_allowance = allowed_empty_between;
            self.current_indent -= indent_change;
        }
    }
    /// Formats a terminal node and appends the formatted string to the result.
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
        let allowed_newlines = usize::from(syntax_node.allow_newline_after(self.db));
        self.format_trivia(trailing_trivia, allowed_newlines);
    }
    /// Appends a trivia node (if needed) to the result.
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
    /// Formats a token node and it to the result.
    fn format_token(&mut self, syntax_node: &SyntaxNode, no_space_after: bool) {
        let no_space_after = no_space_after || syntax_node.force_no_space_after(self.db);
        let text = syntax_node.text(self.db).unwrap();
        self.append_token(text, syntax_node, no_space_after);
    }
    /// Appends a token node to the result.
    fn append_token(&mut self, text: SmolStr, syntax_node: &SyntaxNode, no_space_after: bool) {
        if !syntax_node.force_no_space_before(self.db) && !self.line_state.no_space_after {
            self.line_state.line_buffer.push_space();
        }
        self.line_state.no_space_after = no_space_after;
        self.line_state.line_buffer.push_str(&text);
    }
    fn append_break_line_point(&mut self, properties: Option<BreakLinePointProperties>) {
        if let Some(properties) = properties {
            self.line_state.line_buffer.push_break_line_point(properties);
        }
    }
    /// Returns the leading indentation according to the current indent and the tab size.
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
    /// Builds the pending line states into a string, and append it to the result.
    fn finalize_line(&mut self) {
        self.result.push_str(&self.line_state.line_buffer.build(
            self.config.max_line_length - self.current_indent * self.config.tab_size,
            self.config.tab_size,
            &self.get_indentation(),
        ));
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
