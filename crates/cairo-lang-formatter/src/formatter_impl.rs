use std::cmp::Ordering;
use std::fmt;
use std::ops::Deref;

use cairo_lang_filesystem::span::TextWidth;
use cairo_lang_syntax as syntax;
use cairo_lang_syntax::attribute::consts::FMT_SKIP_ATTR;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{ast, SyntaxNode, Terminal, TypedSyntaxNode};
use itertools::Itertools;
use smol_str::SmolStr;
use syntax::node::ast::MaybeModuleBody;
use syntax::node::helpers::QueryAttrs;
use syntax::node::kind::SyntaxKind;

use crate::FormatterConfig;

#[derive(Clone, Debug, Copy, PartialEq, Eq, PartialOrd, Ord)]
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

#[derive(Clone, Debug, PartialEq, Eq)]
/// Properties defining the behaviour of a break line point.
pub struct BreakLinePointProperties {
    /// Indicates that the break line point was added instead of an empty line in the code, which
    /// means it must be preserved in the output. Notice that the number of consecutive empty line
    /// break points is limited and not all empty lines in the code creates an empty line break
    /// points.
    pub is_empty_line_breakpoint: bool,
    /// Breaking precedence, lower values will break first.
    pub precedence: usize,
    /// Dictates the breaking indentation behaviour.
    pub break_indentation: BreakLinePointIndentation,
    /// Indicates whether a breakpoint is optional. An optional breakpoint may be broken only if
    /// the line is too long. A non-optional breakpoint is always broken.
    pub is_optional: bool,
    /// Indicates to put a space instead of the break line point if it were not broken.
    pub space_if_not_broken: bool,
    /// Indicates that in a group of such breakpoints, only one should be broken, specifically the
    /// last one which fits in the line length.
    pub is_single_breakpoint: bool,
}
impl Ord for BreakLinePointProperties {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self.is_empty_line_breakpoint, other.is_empty_line_breakpoint) {
            (true, true) | (false, false) => self.precedence.cmp(&other.precedence),
            (true, false) => Ordering::Greater,
            (false, true) => Ordering::Less,
        }
    }
}

impl PartialOrd for BreakLinePointProperties {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl BreakLinePointProperties {
    pub fn new(
        precedence: usize,
        break_indentation: BreakLinePointIndentation,
        is_optional: bool,
        space_if_not_broken: bool,
    ) -> Self {
        Self {
            precedence,
            break_indentation,
            is_optional,
            space_if_not_broken,
            is_empty_line_breakpoint: false,
            is_single_breakpoint: false,
        }
    }
    pub fn new_empty_line() -> Self {
        Self {
            precedence: 0,
            break_indentation: BreakLinePointIndentation::NotIndented,
            is_optional: false,
            space_if_not_broken: false,
            is_empty_line_breakpoint: true,
            is_single_breakpoint: false,
        }
    }
    pub fn set_single_breakpoint(&mut self) {
        self.is_single_breakpoint = true;
    }
}

/// The possible parts of line trees.
#[derive(Clone, Debug)]
enum LineComponent {
    /// A simple string to be printed.
    Token(String),
    /// Any break line point inside a protected zone is ignored unless
    /// it has no sibling break line points.
    /// For example, in the expression let "x = 1 * (2 + 3);" everything inside the parentheses
    /// is collected into a protected zone and is broken only if the line is too
    /// long after the first break line point (the one before the '*' operator) is broken.
    /// More generally, any break point inside a protected zone is ignored unless there
    /// are no breakpoints which are direct children of the parent LineBuilder.
    /// The precedence dictates the order of breaking the protected zones, among sibling protected
    /// zones. For example, the body of a function should be broken into separate lines before
    /// the function signature.
    ProtectedZone { builder: LineBuilder, precedence: usize },
    /// Represent a space in the code.
    Space,
    /// Represent a leading indent.
    Indent(usize),
    /// An optional break line point, that will be used if the line is too long.
    BreakLinePoint(BreakLinePointProperties),
    /// A component representing a comment in the code. Leading (not trailing) comments are
    /// disregarded when computing line width as it belongs to another line.
    Comment { content: String, is_trailing: bool },
}
impl LineComponent {
    pub fn width(&self) -> usize {
        match self {
            Self::Token(s) => s.len(),
            Self::ProtectedZone { builder, .. } => builder.width(),
            Self::Space => 1,
            Self::Indent(n) => *n,
            Self::BreakLinePoint(properties) => usize::from(properties.space_if_not_broken),
            Self::Comment { content, is_trailing } => {
                if *is_trailing {
                    content.len()
                } else {
                    // Comments before a line are not accounted for the line width.
                    0
                }
            }
        }
    }
}
impl fmt::Display for LineComponent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Token(s) => write!(f, "{s}"),
            Self::ProtectedZone { builder, .. } => write!(f, "{builder}"),
            Self::Space => write!(f, " "),
            Self::Indent(n) => write!(f, "{}", " ".repeat(*n)),
            Self::BreakLinePoint(properties) => {
                write!(f, "{}", if properties.space_if_not_broken { " " } else { "" })
            }
            Self::Comment { content, .. } => write!(f, "{content}"),
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
    /// Added break line points are temporarily collected into this vector. The vector is flushed
    /// into the children vector if any other LineComponent is pushed. This prevents break line
    /// points being added to the end of a line.
    pending_break_line_points: Vec<LineComponent>,
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
    /// Adds a a sub-builder as the next child.
    /// All subsequent children will be added to this sub builder until set as closed.
    fn open_sub_builder(&mut self, precedence: usize) {
        let active_builder = self.get_active_builder_mut();
        active_builder.flush_pending_break_line_points();
        active_builder.push_child(LineComponent::ProtectedZone {
            builder: LineBuilder::default(),
            precedence,
        });
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
        match &component {
            LineComponent::BreakLinePoint(properties) if !properties.is_empty_line_breakpoint => {
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
    /// Appends a user-inserted empty line to the line.
    pub fn push_empty_line_break_line_point(&mut self) {
        self.push_child(LineComponent::BreakLinePoint(BreakLinePointProperties::new_empty_line()));
    }
    /// Appends an optional break line point.
    pub fn push_break_line_point(&mut self, properties: BreakLinePointProperties) {
        self.push_child(LineComponent::BreakLinePoint(properties));
    }
    /// Appends a comment to the line.
    pub fn push_comment(&mut self, content: &str, is_trailing: bool) {
        self.push_child(LineComponent::Comment { content: content.to_string(), is_trailing });
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
    /// Returns the next break line point properties from within all the break line points
    /// which are a direct child of this tree, or None if there are no such break line points.
    fn get_next_break_properties(&self) -> Option<BreakLinePointProperties> {
        self.children
            .iter()
            .filter_map(|child| {
                if let LineComponent::BreakLinePoint(properties) = child {
                    Some(properties.clone())
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
    /// Recursively calls break_line_tree until no break_line_point or protected zone exists in the
    /// tree. Returns a vec of strings, each one represents a line.
    fn break_line_tree(&self, max_line_width: usize, tab_size: usize) -> Vec<String> {
        // TODO(gil): improve the complexity of this function. Right now the line builder is
        // entirely cloned for each protected zone, which results in a worst case complexity of
        // O(n*m) where n is the line length and m is the number of protected zones. The actual
        // complexity is lower since the line is broken into smaller pieces and each one is handled
        // separately.
        let mut sub_builders = self.break_line_tree_single_level(max_line_width, tab_size);
        // If the line was not broken into several lines (i.e. only one sub_builder), open the
        // highest precedence protected zone and try to break again.
        while sub_builders.len() == 1 {
            // Break the line according to the break_line_points.
            // TODO(Gil): remove the "contains_break_line_points" and return from the breaking
            // function whether it broke any break points.
            if sub_builders[0].contains_break_line_points() {
                sub_builders =
                    sub_builders[0].break_line_tree_single_level(max_line_width, tab_size)
            } else if sub_builders[0].contains_protected_zone() {
                // No break_line_points, open the highest precedence protected zone.
                sub_builders = vec![sub_builders[0].open_protected_zone()];
            } else {
                // All break line points were already broken or removed.
                // TODO(Gil): Propagate error to user if line is still too long.
                return vec![self.to_string()];
            }
        }
        // Keep breaking recursively the new lines.
        sub_builders
            .iter()
            .flat_map(|tree| tree.break_line_tree(max_line_width, tab_size))
            .collect()
    }
    /// Breaks the LineTree once into a vector of LineTrees according to the highest precedence
    /// (lowest precedence number) break line point found in the LineTree.
    fn break_line_tree_single_level(
        &self,
        max_line_width: usize,
        tab_size: usize,
    ) -> Vec<LineBuilder> {
        let Some(break_line_point_properties) = self.get_next_break_properties() else {
            return vec![self.clone()];
        };
        let mut breaking_positions =
            self.get_break_point_indices_by_precedence(break_line_point_properties.precedence);
        if break_line_point_properties.is_single_breakpoint {
            // If the break line point is a single breakpoint, search for the last one which fits
            // in the line, and remove all the others.
            let mut last_break_point_index = breaking_positions.len() - 1;
            let mut first_break_point_index = 0;
            while first_break_point_index < last_break_point_index {
                let middle_break_point_index =
                    (first_break_point_index + last_break_point_index + 1) / 2;
                let middle_break_point = breaking_positions[middle_break_point_index];
                let middle_break_point_width = self.width_between(0, middle_break_point);
                if middle_break_point_width <= max_line_width {
                    first_break_point_index = middle_break_point_index;
                } else {
                    last_break_point_index = middle_break_point_index - 1;
                }
            }
            breaking_positions = vec![breaking_positions[first_break_point_index]];
        }
        if self.width() <= max_line_width && break_line_point_properties.is_optional {
            return vec![self.remove_all_optional_break_line_points()];
        }
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
            let added_indent = match break_line_point_properties.break_indentation {
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
        // Split into two match statements since self is mutably borrowed in the second match,
        // and thus a mutable ref to self can't be returned in it.
        match self.children.last() {
            Some(LineComponent::ProtectedZone { builder: sub_builder, .. })
                if sub_builder.is_open => {}
            _ => {
                return self;
            }
        }
        match self.children.last_mut() {
            Some(LineComponent::ProtectedZone { builder: sub_builder, .. })
                if sub_builder.is_open =>
            {
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
    pub fn build(&self, max_line_width: usize, tab_size: usize) -> String {
        self.break_line_tree(max_line_width, tab_size).iter().join("\n") + "\n"
    }
    /// Returns the highest protected zone precedence (minimum number) from within all the protected
    /// zones which are direct children of this builder, or None if there are no protected zones
    /// direct-children.
    fn get_highest_protected_zone_precedence(&self) -> Option<usize> {
        self.children
            .iter()
            .filter_map(|child| {
                if let LineComponent::ProtectedZone { precedence, .. } = child {
                    Some(*precedence)
                } else {
                    None
                }
            })
            .min()
    }
    /// Creates a new LineBuilder where the first subchild which is a protected zone, is now
    /// unprotected.
    fn open_protected_zone(&self) -> LineBuilder {
        let mut unprotected_builder = LineBuilder::default();
        let mut first_protected_zone_found = false;
        let highest_precedence = self
            .get_highest_protected_zone_precedence()
            .expect("Tried to unprotect a line builder with no protected zones.");
        for child in self.children.iter() {
            match child {
                LineComponent::ProtectedZone { builder: sub_tree, precedence }
                    if *precedence == highest_precedence && !first_protected_zone_found =>
                {
                    first_protected_zone_found = true;
                    for sub_child in sub_tree.children.iter() {
                        unprotected_builder.push_child(sub_child.clone());
                    }
                }
                _ => unprotected_builder.push_child(child.clone()),
            }
        }
        unprotected_builder
    }
    /// Returns whether or not the line contains a protected zone.
    fn contains_protected_zone(&self) -> bool {
        self.children.iter().any(|child| matches!(child, LineComponent::ProtectedZone { .. }))
    }
    /// Returns whether the line contains only indents.
    fn is_only_indents(&self) -> bool {
        !self.children.iter().any(|child| !matches!(child, LineComponent::Indent { .. }))
    }
    fn get_leading_indent(&self) -> usize {
        let mut leading_indent = 0;
        let mut children_iter = self.children.iter();
        while let Some(LineComponent::Indent(indent_size)) = children_iter.next() {
            leading_indent += indent_size
        }
        leading_indent
    }
    /// Returns whether the line contains a break point.
    fn contains_break_line_points(&self) -> bool {
        self.children.iter().any(|child| matches!(child, LineComponent::BreakLinePoint(_)))
    }
    // Removes all the break line points.
    fn remove_all_optional_break_line_points(&self) -> LineBuilder {
        LineBuilder {
            children: self
                .children
                .iter()
                .map(|child| match child {
                    LineComponent::BreakLinePoint(node_properties)
                        if node_properties.is_optional =>
                    {
                        LineComponent::Token(child.to_string())
                    }
                    _ => child.clone(),
                })
                .collect_vec(),
            is_open: true,
            pending_break_line_points: vec![],
        }
    }
}

/// A struct holding all the data of the pending line to be emitted.
struct PendingLineState {
    /// Intermediate representation of the text to be emitted.
    line_buffer: LineBuilder,
    /// Should the next space between tokens be ignored.
    force_no_space_after: bool,
}

impl PendingLineState {
    pub fn new() -> Self {
        Self { line_buffer: LineBuilder::default(), force_no_space_after: true }
    }
}

/// Represents the break line points before and after a syntax node.
pub enum BreakLinePointsPositions {
    Leading(BreakLinePointProperties),
    Trailing(BreakLinePointProperties),
    Both { leading: BreakLinePointProperties, trailing: BreakLinePointProperties },
    List { properties: BreakLinePointProperties, breaking_frequency: usize },
    None,
}

impl BreakLinePointsPositions {
    pub fn new_symmetric(break_line_point_properties: BreakLinePointProperties) -> Self {
        Self::Both {
            leading: break_line_point_properties.clone(),
            trailing: break_line_point_properties,
        }
    }
    pub fn leading(&self) -> Option<BreakLinePointProperties> {
        match self {
            Self::Leading(properties) | Self::Both { leading: properties, .. } => {
                Some(properties.clone())
            }
            _ => None,
        }
    }
    pub fn trailing(&self) -> Option<BreakLinePointProperties> {
        match self {
            Self::Trailing(properties) | Self::Both { trailing: properties, .. } => {
                Some(properties.clone())
            }
            _ => None,
        }
    }
}

// TODO(spapini): Introduce the correct types here, to reflect the "applicable" nodes types.
pub trait SyntaxNodeFormat {
    /// Returns true if a token should never have a space before it.
    /// Only applicable for token nodes.
    fn force_no_space_before(&self, db: &dyn SyntaxGroup) -> bool;
    /// Returns true if a token should never have a space after it.
    /// Only applicable for token nodes.
    fn force_no_space_after(&self, db: &dyn SyntaxGroup) -> bool;
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
    ) -> BreakLinePointsPositions;
    /// Returns the breaking position between the children of a syntax node.
    fn get_internal_break_line_point_properties(
        &self,
        db: &dyn SyntaxGroup,
    ) -> BreakLinePointsPositions;
    /// If self is a protected zone, returns its precedence (highest precedence == lowest number).
    /// Otherwise, returns None.
    fn get_protected_zone_precedence(&self, db: &dyn SyntaxGroup) -> Option<usize>;
    fn should_skip_terminal(&self, db: &dyn SyntaxGroup) -> bool;
}

pub struct FormatterImpl<'a> {
    db: &'a dyn SyntaxGroup,
    config: FormatterConfig,
    /// A buffer for the current line.
    line_state: PendingLineState,
    /// The number of empty lines allowed after the current node.
    empty_lines_allowance: usize,
    /// Indicates whether the current line only consists of whitespace tokens (since the last
    /// newline).
    is_current_line_whitespaces: bool,
}

impl<'a> FormatterImpl<'a> {
    pub fn new(db: &'a dyn SyntaxGroup, config: FormatterConfig) -> Self {
        Self {
            db,
            config,
            line_state: PendingLineState::new(),
            empty_lines_allowance: 0,
            is_current_line_whitespaces: true,
        }
    }
    /// Gets a root of a syntax tree and returns the formatted string of the code it represents.
    pub fn get_formatted_string(&mut self, syntax_node: &SyntaxNode) -> String {
        self.format_node(syntax_node, false);
        self.line_state.line_buffer.build(self.config.max_line_length, self.config.tab_size)
    }
    /// Appends a formatted string, representing the syntax_node, to the result.
    /// Should be called with a root syntax node to format a file.
    pub fn format_node(&mut self, syntax_node: &SyntaxNode, no_space_after: bool) {
        if syntax_node.text(self.db).is_some() {
            panic!("Token reached before terminal.");
        }
        let protected_zone_precedence = syntax_node.get_protected_zone_precedence(self.db);
        let node_break_points = syntax_node.get_wrapping_break_line_point_properties(self.db);
        self.append_break_line_point(node_break_points.leading());
        if let Some(precedence) = protected_zone_precedence {
            self.line_state.line_buffer.open_sub_builder(precedence);
        }
        if self.should_ignore_node_format(syntax_node) {
            self.line_state.line_buffer.push_str(syntax_node.get_text(self.db).trim());
        } else if syntax_node.kind(self.db).is_terminal() {
            self.format_terminal(syntax_node, no_space_after);
        } else {
            self.format_internal(syntax_node, no_space_after);
        }
        if protected_zone_precedence.is_some() {
            self.line_state.line_buffer.close_sub_builder();
        }
        self.append_break_line_point(node_break_points.trailing());
    }
    /// Formats an internal node and appends the formatted string to the result.
    fn format_internal(&mut self, syntax_node: &SyntaxNode, no_space_after: bool) {
        let allowed_empty_between = syntax_node.allowed_empty_between(self.db);
        let no_space_after = no_space_after || syntax_node.force_no_space_after(self.db);
        let internal_break_line_points_positions =
            syntax_node.get_internal_break_line_point_properties(self.db);

        // TODO(ilya): consider not copying here.
        let mut children = self.db.get_children(syntax_node.clone()).deref().clone();
        let n_children = children.len();
        if self.config.sort_module_level_items {
            children.sort_by_key(|c| MovableNode::new(self.db, c));
        };
        for (i, child) in children.iter().enumerate() {
            if child.width(self.db) == TextWidth::default() {
                continue;
            }
            self.format_node(child, no_space_after && i == n_children - 1);
            if let BreakLinePointsPositions::List { properties, breaking_frequency } =
                &internal_break_line_points_positions
            {
                if i % breaking_frequency == breaking_frequency - 1 && i < n_children - 1 {
                    self.append_break_line_point(Some(properties.clone()));
                }
            }
            self.empty_lines_allowance = allowed_empty_between;
        }
    }
    /// Formats a terminal node and appends the formatted string to the result.
    fn format_terminal(&mut self, syntax_node: &SyntaxNode, no_space_after: bool) {
        // TODO(spapini): Introduce a Terminal and a Token enum in ast.rs to make this cleaner.
        let children = self.db.get_children(syntax_node.clone());
        let mut children_iter = children.iter().cloned();
        let leading_trivia = ast::Trivia::from_syntax_node(self.db, children_iter.next().unwrap());
        let token = children_iter.next().unwrap();
        let trailing_trivia = ast::Trivia::from_syntax_node(self.db, children_iter.next().unwrap());

        // The first newlines is the leading trivia correspond exactly to empty lines.
        self.format_trivia(leading_trivia, true);
        if !syntax_node.should_skip_terminal(self.db) {
            self.format_token(&token, no_space_after || syntax_node.force_no_space_after(self.db));
        }
        self.format_trivia(trailing_trivia, false);
    }
    /// Appends a trivia node (if needed) to the result.
    fn format_trivia(&mut self, trivia: syntax::node::ast::Trivia, is_leading: bool) {
        for trivium in trivia.elements(self.db) {
            match trivium {
                ast::Trivium::SingleLineComment(_) => {
                    if !is_leading {
                        self.line_state.line_buffer.push_space();
                    }
                    self.line_state.line_buffer.push_comment(
                        &trivium.as_syntax_node().text(self.db).unwrap(),
                        !is_leading,
                    );
                    self.is_current_line_whitespaces = false;
                    self.empty_lines_allowance = 1;

                    self.line_state.line_buffer.push_break_line_point(
                        BreakLinePointProperties::new(
                            // Should be greater than any other precedence.
                            usize::MAX,
                            BreakLinePointIndentation::NotIndented,
                            false,
                            false,
                        ),
                    );
                }
                ast::Trivium::Whitespace(_) => {}
                ast::Trivium::Newline(_) => {
                    if self.empty_lines_allowance > 0 && self.is_current_line_whitespaces {
                        self.empty_lines_allowance -= 1;
                        self.line_state.line_buffer.push_empty_line_break_line_point();
                    }
                    self.is_current_line_whitespaces = true;
                }
                ast::Trivium::Skipped(_) => {
                    self.format_token(&trivium.as_syntax_node(), false);
                }
                ast::Trivium::SkippedNode(node) => {
                    self.format_node(&node.as_syntax_node(), false);
                }
            }
        }
    }
    /// Formats a token node and appends it to the result.
    /// Assumes the given SyntaxNode is a token.
    fn format_token(&mut self, syntax_node: &SyntaxNode, no_space_after: bool) {
        let no_space_after = no_space_after || syntax_node.force_no_space_after(self.db);
        let text = syntax_node.text(self.db).unwrap();
        if !syntax_node.force_no_space_before(self.db) && !self.line_state.force_no_space_after {
            self.line_state.line_buffer.push_space();
        }
        self.line_state.force_no_space_after = no_space_after;

        if syntax_node.kind(self.db) != SyntaxKind::TokenWhitespace {
            self.is_current_line_whitespaces = false;
        }
        let node_break_points = syntax_node.get_wrapping_break_line_point_properties(self.db);
        self.append_break_line_point(node_break_points.leading());
        self.line_state.line_buffer.push_str(&text);
        self.append_break_line_point(node_break_points.trailing());
    }
    fn append_break_line_point(&mut self, properties: Option<BreakLinePointProperties>) {
        if let Some(properties) = properties {
            self.line_state.line_buffer.push_break_line_point(properties);
            self.line_state.force_no_space_after = true;
        }
    }

    /// Gets a syntax node and returns if the node has an cairofmt::skip attribute.
    pub fn should_ignore_node_format(&self, syntax_node: &SyntaxNode) -> bool {
        syntax_node.has_attr(self.db, FMT_SKIP_ATTR)
    }
}

/// Represents a sortable SyntaxNode.
#[derive(PartialEq, Eq)]
enum MovableNode {
    ItemModule(SmolStr),
    ItemUse(SmolStr),
    Immovable,
}
impl MovableNode {
    fn new(db: &dyn SyntaxGroup, node: &SyntaxNode) -> Self {
        match node.kind(db) {
            SyntaxKind::ItemModule => {
                let item = ast::ItemModule::from_syntax_node(db, node.clone());
                if matches!(item.body(db), MaybeModuleBody::None(_)) {
                    Self::ItemModule(item.name(db).text(db))
                } else {
                    Self::Immovable
                }
            }
            SyntaxKind::ItemUse => Self::ItemUse(node.clone().get_text_without_trivia(db).into()),
            _ => Self::Immovable,
        }
    }
}

impl Ord for MovableNode {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (MovableNode::Immovable, MovableNode::Immovable) => Ordering::Equal,
            (MovableNode::ItemModule(a), MovableNode::ItemModule(b))
            | (MovableNode::ItemUse(a), MovableNode::ItemUse(b)) => a.cmp(b),
            (_, MovableNode::Immovable) | (MovableNode::ItemModule(_), MovableNode::ItemUse(_)) => {
                Ordering::Less
            }
            (MovableNode::Immovable, _) | (MovableNode::ItemUse(_), MovableNode::ItemModule(_)) => {
                Ordering::Greater
            }
        }
    }
}

impl PartialOrd for MovableNode {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
