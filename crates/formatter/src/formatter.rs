use std::fmt;

use itertools::Itertools;
use smol_str::SmolStr;
use syntax::node::db::SyntaxGroup;
use syntax::node::{ast, SyntaxNode, TypedSyntaxNode};

use crate::FormatterConfig;

#[derive(Clone)]
/// Defines the break point behaviour.
/// Defined in get_break_line_point_properties.
pub enum BreakLinePointType {
    /// Dangling points are usually operators which should be aligned to the first operator, i.e.:
    /// let x = 1 + 2
    ///           + 3
    ///           + 4
    Dangling,
    /// Non-dangling are not aligned to a token above, but instead just indented by tabsize.
    /// For example, method calls, i.e.:
    /// a_variable.function1()
    ///     .function2()
    ///     .function3()
    NonDangling,
    /// List breaks are similar to non-dangling points, but have a non-indented trailing suffix.
    /// Should only be used for separated lists.
    /// For example, struct c`tor:
    /// let x = Struct {
    ///     first_arg: first_arg,
    ///     second_arg: second_arg,
    /// };
    SeparatedListBreak,
    /// Same as separated list breaks, but for lists with no separator such as StatementList or
    /// ItemList.
    ListBreak,
    /// Represent an allowed new line in the original code.
    Newline,
}
impl BreakLinePointType {
    pub fn is_dangling(&self) -> bool {
        matches!(self, BreakLinePointType::Dangling)
    }
    pub fn is_nondangling(&self) -> bool {
        matches!(self, BreakLinePointType::NonDangling)
    }
    pub fn is_separated_list_break(&self) -> bool {
        matches!(self, BreakLinePointType::SeparatedListBreak)
    }
    pub fn is_list_break(&self) -> bool {
        matches!(self, BreakLinePointType::ListBreak)
    }
    pub fn is_new_line(&self) -> bool {
        matches!(self, BreakLinePointType::Newline)
    }
}

#[derive(Clone)]
/// Properties defining the behaviour of a break line point.
pub struct BreakLinePointProperties {
    /// Breaking precedence, lower values will break first.
    pub precedence: usize,
    /// The breaking behaviour type.
    pub break_type: BreakLinePointType,
    /// Indicates if a breakpoint is optional. An optional breakpoint will be broken only if the
    /// line is too long. A non-optional breakpoint will always be broken.
    pub is_optional: bool,
    /// Indicates if the indent is changed if a list is broken.
    /// For example, if StatementList is broken the code is indented, but if AttributeList is
    /// broken there is no indentation.
    pub add_indent: bool,
}
impl BreakLinePointProperties {
    pub fn new(
        precedence: usize,
        break_type: BreakLinePointType,
        is_optional: bool,
        add_indent: bool,
    ) -> Self {
        Self { precedence, break_type, is_optional, add_indent }
    }
}
/// Represents the relative position of a break line point inside a node.
pub enum BreakingPosition {
    /// A break line point which appears before all the children of a node.
    Leading,
    /// A break line point which appears between two children of a node.
    Internal,
    /// A break line point which appears after all the children of a node.
    Trailing,
}

/// The possible parts of line trees.
#[derive(Clone)]
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
    /// The precedence dictates the order in which the internal builders will be open.
    /// For example, the body of a function should be broken into separate lines before the
    /// function signature.
    Internal { builder: LineBuilder, precedence: usize },
    /// Represent a space in the code.
    Space,
    /// Represent a leading indent.
    Indent(usize),
    /// An optional break line point, that will be used if the line is too long.
    BreakLinePoint(BreakLinePointProperties),
    /// A comment string in the code.
    // TODO(Gil): Separate between leading and trailing comments.
    Comment(String),
}
impl LineComponent {
    pub fn width(&self) -> usize {
        match self {
            Self::Token(s) => s.len(),
            Self::Internal { builder, .. } => builder.width(),
            Self::Space => 1,
            Self::Indent(n) => *n,
            Self::BreakLinePoint(_) => 0,
            // TODO(Gil): Only leading comments should be ignored.
            Self::Comment(_) => 0,
        }
    }
}
impl fmt::Display for LineComponent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Token(s) => write!(f, "{s}"),
            Self::Internal { builder, .. } => write!(f, "{builder}"),
            Self::Space => write!(f, " "),
            Self::Indent(n) => write!(f, "{}", " ".repeat(*n)),
            Self::BreakLinePoint(_) => write!(f, ""),
            Self::Comment(s) => write!(f, "{s}"),
        }
    }
}

/// Used for aggregating the code and then break it into separate lines.
#[derive(Clone)]
struct LineBuilder {
    children: Vec<LineComponent>,
    /// Indicates whether this builder is open, which means any new child should
    /// be (recursively) appended to it. Otherwise, new children will be appended as its sibling.
    is_open: bool,
}
impl fmt::Display for LineBuilder {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_only_indents() {
            write!(f, "")
        } else {
            write!(f, "{}", self.children.iter().map(|child| child.to_string()).join(""))
        }
    }
}
impl LineBuilder {
    /// Creates a new intermediate line.
    pub fn new() -> Self {
        Self { children: vec![], is_open: true }
    }
    /// Adds a a sub-builder as the next child.
    /// All subsequent children will be added to this sub builder until set as closed.
    fn open_sub_builder(&mut self, flatten_precedence: usize) {
        self.get_active_builder().push_child(LineComponent::Internal {
            builder: LineBuilder::new(),
            precedence: flatten_precedence,
        });
    }
    /// Sets the last child, which is assumed to be a LineBuilder, as close.
    /// New children will be siblings of this subtree.
    fn close_sub_builder(&mut self) {
        self.get_active_builder().is_open = false;
    }
    /// Adds a line component as the next child. If the last child is an open LineBuilder the child
    /// is recursively appended to the sub builder.
    fn push_child(&mut self, new_comp: LineComponent) {
        self.get_active_builder().children.push(new_comp);
    }
    /// Appends a string to the line.
    pub fn push_str(&mut self, s: &str) {
        self.push_child(LineComponent::Token(s.to_string()));
    }
    /// Appends a comment to the line.
    pub fn push_comment(&mut self, s: &str) {
        self.push_child(LineComponent::Comment(s.to_string()));
    }
    /// Appends a space to the line.
    pub fn push_space(&mut self) {
        self.push_child(LineComponent::Space);
    }
    /// Appends an indent to the line.
    pub fn push_indent(&mut self, n: usize) {
        if n > 0 {
            self.push_child(LineComponent::Indent(n));
        }
    }
    /// Appends an optional break line point.
    pub fn push_break_line_point(&mut self, properties: BreakLinePointProperties) {
        self.push_child(LineComponent::BreakLinePoint(properties));
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
    fn get_min_break_precedence(&self, include_new_lines: bool) -> Option<usize> {
        self.children
            .iter()
            .filter_map(|child| {
                if let LineComponent::BreakLinePoint(properties) = child {
                    if !include_new_lines && properties.break_type.is_new_line() {
                        None
                    } else {
                        Some(properties.precedence)
                    }
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
    fn get_preceding_break_points_indices(&self, include_new_lines: bool) -> Vec<usize> {
        if let Some(precedence) = self.get_min_break_precedence(include_new_lines) {
            self.get_break_point_indices_by_precedence(precedence)
        } else {
            vec![]
        }
    }
    /// Returns a vec of strings, representing the code in the LineTree,
    /// each one with len < max_Width (if possible).
    fn to_broken_string_by_width(&self, max_line_width: usize, tab_size: usize) -> Vec<String> {
        // TODO(gil): consider using a write buffer similar to 'write!()' to reduce string
        // allocations.
        let mut sub_builders = self.to_broken_tree(max_line_width, tab_size, false);
        // While the line is not broken into several lines, try to flatten it and then break it.
        while sub_builders.len() == 1 {
            if !sub_builders[0].is_flat() {
                sub_builders =
                    sub_builders[0].flatten().to_broken_tree(max_line_width, tab_size, false);
            } else if sub_builders[0].contains_break_line_points() {
                // New lines are broken only after all other points were handled, i.e. the builder
                // is flat, and trying to break non-newline points doesn't result in two or more
                // lines.
                sub_builders = sub_builders[0].to_broken_tree(max_line_width, tab_size, true);
            } else {
                // Can't break tree to fit within width
                // TODO(Gil): Propagate error to user.
                return vec![self.to_string()];
            }
        }
        // Keep breaking recursively the new lines if they are still too long
        sub_builders
            .iter()
            .flat_map(|tree| tree.to_broken_string_by_width(max_line_width, tab_size))
            .collect()
    }
    /// Breaks the LineTree into a vector of LineTrees
    /// according to the lowest precedence break line point found in the LineTree.
    fn to_broken_tree(
        &self,
        max_line_width: usize,
        tab_size: usize,
        break_new_lines: bool,
    ) -> Vec<LineBuilder> {
        let break_only_non_optional_points = self.width() <= max_line_width;
        let mut breaking_positions = self.get_preceding_break_points_indices(break_new_lines);
        if breaking_positions.is_empty() {
            return vec![self.clone()];
        }
        let mut break_line_point_properties = if let LineComponent::BreakLinePoint(properties) =
            &self.children[breaking_positions[0]]
        {
            properties.clone()
        } else {
            unreachable!("Index is taken from a break line points positions vector.");
        };
        if break_only_non_optional_points && break_line_point_properties.is_optional {
            return vec![
                self.remove_optional_break_line_point(break_line_point_properties.precedence),
            ];
        }
        let mut base_indent = self.get_leading_indent();
        let mut trees: Vec<LineBuilder> = vec![LineBuilder::new()];
        trees.last_mut().unwrap().push_indent(base_indent);
        let mut added_indent = 0;
        let mut prev_position = 0;
        // Dangling break is overridden if it will cause the line to still be too long.
        if break_line_point_properties.break_type.is_dangling()
            && (breaking_positions.len() == 1
                || self.width_between(0, breaking_positions[1]) > max_line_width)
        {
            break_line_point_properties.break_type = BreakLinePointType::NonDangling;
            added_indent = tab_size;
        }

        breaking_positions.push(self.children.len()); // Dummy break line point, simplifies the loop.

        // Iterate over the break line points and collect each part between them into one new
        // LineBuilder.
        for (i, position) in breaking_positions.iter().enumerate() {
            for j in prev_position..*position {
                match &self.children[j] {
                    LineComponent::Indent(_) => {}
                    LineComponent::BreakLinePoint(node_properties)
                        if node_properties.break_type.is_new_line() && (j == *position - 1) => {}
                    LineComponent::Space => {
                        // Ignore spaces at the start of a line
                        if !trees.last_mut().unwrap().is_only_indents() {
                            trees.last_mut().unwrap().push_space();
                        }
                    }
                    _ => trees.last_mut().unwrap().push_child(self.children[j].clone()),
                }
            }
            if i == 0 && break_line_point_properties.break_type.is_dangling() {
                added_indent = trees.last_mut().unwrap().width();
                base_indent = 0;
            } else if *position < self.children.len() {
                if break_line_point_properties.break_type.is_separated_list_break()
                    || break_line_point_properties.break_type.is_list_break()
                {
                    // In a breakable list, add indent after the first break point
                    // (e.g. after "Struct{" to indent all struct builder args )
                    if i == 0 && break_line_point_properties.add_indent {
                        added_indent += tab_size;
                    }
                    // In a breakable list, remove indent before the trailing tokens
                    // (e.g. before "};" to unindent the closing brace)
                    if i == breaking_positions.len() - 2 && break_line_point_properties.add_indent {
                        added_indent -= tab_size;
                    }
                }
                trees.push(LineBuilder::new());
                trees.last_mut().unwrap().push_indent(base_indent + added_indent);
            }
            prev_position = position + 1;
        }
        trees
    }
    /// Returns a reference to the currently active builder.
    fn get_active_builder(&mut self) -> &mut LineBuilder {
        // Splitted into two match statements since self is mutably borrowed in the second match,
        // and thus a mutable ref to self can't be returned in it.
        match self.children.last() {
            Some(LineComponent::Internal { builder: sub_builder, .. }) if sub_builder.is_open => {}
            _ => {
                return self;
            }
        }
        match self.children.last_mut() {
            Some(LineComponent::Internal { builder: sub_builder, .. }) if sub_builder.is_open => {
                sub_builder.get_active_builder()
            }
            _ => {
                unreachable!("This case is covered in the first match.")
            }
        }
    }
    /// Creates a string of the code represented in the builder. The string may represent
    /// several lines (separated by '\n'), where each line length is
    /// less than max_line_width (if possible).
    pub fn build(&self, max_line_width: usize, tab_size: usize) -> String {
        self.to_broken_string_by_width(max_line_width, tab_size).iter().join("\n")
    }

    /// Returns the minimum protected zone precedence from within all the protected zones
    /// which are a direct child of this builder, or None if there are no protected zones.
    fn get_min_flatten_precedence(&self) -> Option<usize> {
        self.children
            .iter()
            .filter_map(|child| {
                if let LineComponent::Internal { precedence, .. } = child {
                    Some(*precedence)
                } else {
                    None
                }
            })
            .min()
    }
    /// Creates a new LineBuilder where the first subchild which is a LineBuilder, is replaced by
    /// all its children.
    fn flatten(&self) -> LineBuilder {
        let mut flattened_tree = LineBuilder::new();
        let mut first_tree_found = false;
        let min_precedence = self.get_min_flatten_precedence().unwrap_or(0);
        for child in self.children.iter() {
            match child {
                LineComponent::Internal { builder: sub_tree, precedence }
                    if *precedence == min_precedence && !first_tree_found =>
                {
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
        !self.children.iter().any(|child| matches!(child, LineComponent::Internal { .. }))
    }
    /// Returns whether the line contains only indents.
    fn is_only_indents(&self) -> bool {
        !self.children.iter().any(|child| !matches!(child, LineComponent::Indent(_)))
    }
    /// Returns whether the line contains a break point.
    fn contains_break_line_points(&self) -> bool {
        self.children.iter().any(|child| matches!(child, LineComponent::BreakLinePoint(_)))
    }
    /// Returns the amount of indent until the first token/space.
    fn get_leading_indent(&self) -> usize {
        let mut leading_indent = 0;
        for child in self.children.iter() {
            match child {
                LineComponent::Token(_) | LineComponent::Internal { .. } | LineComponent::Space => {
                    break;
                }
                LineComponent::Indent(indent) => leading_indent += *indent,
                LineComponent::BreakLinePoint(_) | LineComponent::Comment(_) => {}
            }
        }
        leading_indent
    }
    // Removes all the break line points with a given precedence.
    fn remove_optional_break_line_point(&self, precedence: usize) -> LineBuilder {
        LineBuilder {
            children: self
                .children
                .iter()
                .map(|child| match child {
                    LineComponent::BreakLinePoint(node_properties)
                        if node_properties.precedence == precedence =>
                    {
                        LineComponent::Token(child.to_string())
                    }
                    _ => child.clone(),
                })
                .collect_vec(),
            is_open: true,
        }
    }
}

/// A struct holding all the data of the pending line to be emitted.
/// TODO(Gil): change to a more complex struct to handle line breaking.
struct PendingLineState {
    /// Intermidiate representation of the text to be emitted.
    line_buffer: LineBuilder,
    /// Should the next space between tokens be ignored.
    no_space_after: bool,
}

impl PendingLineState {
    pub fn new() -> Self {
        Self { line_buffer: LineBuilder::new(), no_space_after: true }
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
    /// Returns the number of allowed empty lines between two consecutive children of this node.
    fn allowed_empty_between(&self, db: &dyn SyntaxGroup) -> usize;
    /// Returns the break point properties of a specific node if a break point should exist,
    /// otherwise returns None. The position indicates the place within the node, before,
    /// between or after the children.
    fn get_break_line_point_properties(
        &self,
        db: &dyn SyntaxGroup,
        position: BreakingPosition,
    ) -> Option<BreakLinePointProperties>;
    /// Returns the node precedence of the protected zone if the node is protected from breaking
    /// unless no other break points exists, otherwise, returns None. For example break points
    /// inside ExprParenthesized should only be used if there are no break points outside the
    /// the parenthesis. Only applicable for internal nodes.
    fn get_protected_zone_precedence(&self, db: &dyn SyntaxGroup) -> Option<usize>;
}

pub struct Formatter<'a> {
    db: &'a dyn SyntaxGroup,
    config: FormatterConfig,
    /// A buffer for the current line.
    line_state: PendingLineState,
    /// The number of empty lines allowed after the current node.
    empty_lines_allowance: usize,
}

impl<'a> Formatter<'a> {
    pub fn new(db: &'a dyn SyntaxGroup, config: FormatterConfig) -> Self {
        Self { db, config, line_state: PendingLineState::new(), empty_lines_allowance: 0 }
    }

    /// Gets a root of a syntax tree and return the formatted string of the code.
    pub fn get_formatted_string(&mut self, syntax_node: &SyntaxNode) -> String {
        self.format_node(syntax_node, false);
        self.line_state.line_buffer.build(self.config.max_line_length, self.config.tab_size)
    }

    /// Aggregates the nodes in the tree of a node into the formatter line builder.
    pub fn format_node(&mut self, syntax_node: &SyntaxNode, no_space_after: bool) {
        if syntax_node.text(self.db).is_some() {
            panic!("Token reached before terminal.");
        }
        if syntax_node.kind(self.db).is_terminal() {
            self.format_terminal(syntax_node, no_space_after);
        } else {
            self.format_internal(syntax_node, no_space_after);
        }
    }

    /// Formats an internal node and appends the formatted string to the result.
    fn format_internal(&mut self, syntax_node: &SyntaxNode, no_space_after: bool) {
        let allowed_empty_between = syntax_node.allowed_empty_between(self.db);
        let no_space_after = no_space_after || syntax_node.force_no_space_after(self.db);

        if let Some(flatten_precedence) = syntax_node.get_protected_zone_precedence(self.db) {
            self.line_state.line_buffer.open_sub_builder(flatten_precedence);
        }
        if let Some(break_properties) =
            syntax_node.get_break_line_point_properties(self.db, BreakingPosition::Leading)
        {
            self.append_break_line_point(break_properties);
        }
        let children = syntax_node.children(self.db);
        let n_children = children.len();
        for (i, child) in children.enumerate() {
            if child.width(self.db) == 0 {
                continue;
            }

            self.format_node(&child, no_space_after && i == n_children - 1);

            self.empty_lines_allowance = allowed_empty_between;

            if let Some(break_properties) =
                syntax_node.get_break_line_point_properties(self.db, BreakingPosition::Internal)
            {
                match break_properties.break_type {
                    BreakLinePointType::SeparatedListBreak if i % 2 == 1 && i != n_children - 1 => {
                        self.append_break_line_point(break_properties);
                    }
                    BreakLinePointType::ListBreak if i != n_children - 1 => {
                        self.append_break_line_point(break_properties);
                    }
                    _ => {}
                }
            }
        }
        if let Some(break_properties) =
            syntax_node.get_break_line_point_properties(self.db, BreakingPosition::Trailing)
        {
            self.append_break_line_point(break_properties);
        }
        if syntax_node.get_protected_zone_precedence(self.db).is_some() {
            self.line_state.line_buffer.close_sub_builder();
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
        self.format_trivia(leading_trivia);
        self.format_token(&token, no_space_after || syntax_node.force_no_space_after(self.db));
        self.format_trivia(trailing_trivia);
    }
    /// Appends a trivia node (if needed) to the result.
    fn format_trivia(&mut self, trivia: syntax::node::ast::Trivia) {
        for trivium in trivia.elements(self.db) {
            match trivium {
                ast::Trivium::SingleLineComment(_) => {
                    self.empty_lines_allowance = 2;
                    self.line_state.line_buffer.push_space();
                    self.line_state
                        .line_buffer
                        .push_comment(&trivium.as_syntax_node().text(self.db).unwrap());
                }
                ast::Trivium::Whitespace(_) => {}
                ast::Trivium::Newline(_) => {
                    if self.empty_lines_allowance > 0 {
                        self.empty_lines_allowance -= 1;
                        self.append_break_line_point(
                            trivium
                                .as_syntax_node()
                                .get_break_line_point_properties(
                                    self.db,
                                    BreakingPosition::Trailing,
                                )
                                .unwrap(),
                        );
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
        if let Some(break_properties) =
            syntax_node.get_break_line_point_properties(self.db, BreakingPosition::Leading)
        {
            self.append_break_line_point(break_properties);
        }
        self.line_state.line_buffer.push_str(&text);
        if let Some(break_properties) =
            syntax_node.get_break_line_point_properties(self.db, BreakingPosition::Trailing)
        {
            self.append_break_line_point(break_properties);
        }
    }
    fn append_break_line_point(&mut self, properties: BreakLinePointProperties) {
        self.line_state.line_buffer.push_break_line_point(properties);
    }
}
