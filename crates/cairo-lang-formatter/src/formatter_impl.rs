use std::cmp::Ordering;
use std::fmt;

use cairo_lang_diagnostics::DiagnosticsBuilder;
use cairo_lang_filesystem::ids::{FileKind, FileLongId, VirtualFile};
use cairo_lang_filesystem::span::TextWidth;
use cairo_lang_parser::ParserDiagnostic;
use cairo_lang_parser::parser::Parser;
use cairo_lang_syntax as syntax;
use cairo_lang_syntax::attribute::consts::FMT_SKIP_ATTR;
use cairo_lang_syntax::node::ast::UsePath;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{SyntaxNode, Terminal, TypedSyntaxNode, ast};
use cairo_lang_utils::Intern;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use itertools::{Itertools, chain};
use syntax::node::helpers::QueryAttrs;
use syntax::node::kind::SyntaxKind;

use crate::FormatterConfig;

/// Represents a tree structure for organizing and merging `use` statements.
#[derive(Default, Debug)]
struct UseTree {
    /// A map of child nodes, where the key is the segment of the `use` path and the value is
    /// another `UseTree` representing the nested structure.
    children: OrderedHashMap<String, UseTree>,
    /// A list of `Leaf` nodes, representing individual `use` path endpoints
    /// or aliases. Each leaf contains optional alias information.
    leaves: Vec<Leaf>,
}

/// Represents a terminal node in a `UseTree`, corresponding to a specific `use` path.
#[derive(Default, Debug, Clone)]
struct Leaf {
    name: String,
    alias: Option<String>,
}

impl UseTree {
    /// Inserts a path into the `UseTree`, creating nested entries as needed.
    fn insert_path(&mut self, db: &dyn SyntaxGroup, use_path: UsePath) {
        match use_path {
            UsePath::Leaf(leaf) => {
                let name = leaf.extract_ident(db);
                let alias = leaf.extract_alias(db);
                self.leaves.push(Leaf { name, alias });
            }
            UsePath::Single(single) => {
                let segment = single.extract_ident(db);
                let subtree = self.children.entry(segment).or_default();
                subtree.insert_path(db, single.use_path(db));
            }
            UsePath::Multi(multi) => {
                for sub_path in multi.use_paths(db).elements(db) {
                    self.insert_path(db, sub_path);
                }
            }
            UsePath::Star(_) => {
                self.leaves.push(Leaf { name: "*".to_string(), alias: None });
            }
        }
    }

    /// Merge and organize the `use` paths in a hierarchical structure.
    pub fn create_merged_use_items(
        self,
        allow_duplicate_uses: bool,
        top_level: bool,
    ) -> Vec<String> {
        let mut leaf_paths: Vec<String> = self
            .leaves
            .into_iter()
            .map(|leaf| {
                if let Some(alias) = leaf.alias {
                    format!("{} as {alias}", leaf.name)
                } else {
                    leaf.name
                }
            })
            .collect();

        let mut nested_paths = vec![];
        for (segment, subtree) in self.children {
            let subtree_merged_use_items =
                subtree.create_merged_use_items(allow_duplicate_uses, false);
            nested_paths.extend(
                subtree_merged_use_items.into_iter().map(|child| format!("{segment}::{child}")),
            );
        }

        if !allow_duplicate_uses {
            leaf_paths.sort();
            leaf_paths.dedup();
        }

        match leaf_paths.len() {
            0 => {}
            1 if nested_paths.is_empty() => return leaf_paths,
            1 => nested_paths.extend(leaf_paths),
            _ if top_level => nested_paths.extend(leaf_paths),
            _ => nested_paths.push(format!("{{{}}}", leaf_paths.join(", "))),
        }

        nested_paths
    }

    /// Formats `use` items, creates a virtual file, and parses it into a syntax node.
    pub fn generate_syntax_node_from_use(
        self,
        db: &dyn SyntaxGroup,
        allow_duplicate_uses: bool,
        decorations: String,
    ) -> SyntaxNode {
        let mut formatted_use_items = String::new();
        for statement in self.create_merged_use_items(allow_duplicate_uses, false) {
            formatted_use_items.push_str(&format!("{decorations}use {statement};\n"));
        }

        // Create a virtual file ID for the formatted statements.
        let file_id = FileLongId::Virtual(VirtualFile {
            parent: None,
            name: "parser_input".into(),
            content: formatted_use_items.clone().into(),
            code_mappings: [].into(),
            kind: FileKind::Module,
        })
        .intern(db);

        let mut diagnostics = DiagnosticsBuilder::<ParserDiagnostic>::default();
        Parser::parse_file(db, &mut diagnostics, file_id, &formatted_use_items).as_syntax_node()
    }
}

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
    /// Indicates whether a comma should be added when the line breaks.
    pub is_comma_if_broken: bool,
}
impl Ord for BreakLinePointProperties {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.is_empty_line_breakpoint.cmp(&other.is_empty_line_breakpoint) {
            Ordering::Equal => self.precedence.cmp(&other.precedence),
            other => other,
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
            is_comma_if_broken: false,
        }
    }
    pub fn set_comma_if_broken(&mut self) {
        self.is_comma_if_broken = true;
    }
    pub fn is_comma_if_broken(&self) -> bool {
        self.is_comma_if_broken
    }
    pub fn new_empty_line() -> Self {
        Self {
            precedence: 0,
            break_indentation: BreakLinePointIndentation::NotIndented,
            is_optional: false,
            space_if_not_broken: false,
            is_empty_line_breakpoint: true,
            is_single_breakpoint: false,
            is_comma_if_broken: false,
        }
    }
    pub fn set_single_breakpoint(&mut self) {
        self.is_single_breakpoint = true;
    }
    pub fn set_line_by_line(&mut self) {
        self.is_single_breakpoint = false;
        self.is_optional = true;
    }
    pub fn unset_comma_if_broken(&mut self) {
        self.is_comma_if_broken = false;
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
    /// Returns if the component is a trivia component, i.e. does not contain any code.
    fn is_trivia(&self) -> bool {
        matches!(
            self,
            Self::Comment { .. } | Self::Space | Self::Indent(_) | Self::BreakLinePoint(_)
        )
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
    /// Adds a sub-builder as the next child.
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
        // Aggregate consecutive comment lines into one.
        let active_builder = self.get_active_builder_mut();
        if let Some(LineComponent::Comment { content: prev_content, is_trailing }) =
            active_builder.children.last_mut()
        {
            if !*is_trailing {
                *prev_content += "\n";
                *prev_content += content;
                return;
            }
        }
        self.push_child(LineComponent::Comment { content: content.to_string(), is_trailing });
        self.push_break_line_point(BreakLinePointProperties::new(
            // Should be greater than any other precedence.
            usize::MAX,
            BreakLinePointIndentation::NotIndented,
            false,
            false,
        ));
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
                    (first_break_point_index + last_break_point_index).div_ceil(2);
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
            // If a comment follows the last IndentedWithTail break point, it should also be
            // indented.
            let mut comment_only_added_indent = if let BreakLinePointIndentation::IndentedWithTail =
                break_line_point_properties.break_indentation
            {
                if i == n_break_points - 1 { tab_size } else { 0 }
            } else {
                0
            };
            let cur_indent = base_indent + added_indent;
            trees.push(LineBuilder::new(cur_indent));
            for j in current_line_start..*current_line_end {
                match &self.children[j] {
                    LineComponent::Indent(_) => {}
                    LineComponent::Space => {
                        // Ignore spaces at the start of a line
                        if !trees.last().unwrap().is_only_indents() {
                            trees.last_mut().unwrap().push_space();
                        }
                    }
                    LineComponent::Comment { content, is_trailing } if !is_trailing => {
                        trees.last_mut().unwrap().push_str(&" ".repeat(comment_only_added_indent));
                        let formatted_comment = format_leading_comment(
                            content,
                            cur_indent + comment_only_added_indent,
                            max_line_width,
                        );
                        trees.last_mut().unwrap().push_child(LineComponent::Comment {
                            content: formatted_comment,
                            is_trailing: *is_trailing,
                        });
                    }
                    _ => trees.last_mut().unwrap().push_child(self.children[j].clone()),
                }
                // Indent the comment only if it directly follows the break point.
                if !self.children[j].is_trivia() {
                    comment_only_added_indent = 0;
                }
            }
            if let Some(LineComponent::BreakLinePoint(cur_break_line_points_properties)) =
                self.children.get(*current_line_end)
            {
                if cur_break_line_points_properties.is_comma_if_broken() {
                    trees.last_mut().unwrap().push_str(",");
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
                        if node_properties.space_if_not_broken {
                            LineComponent::Space
                        } else {
                            LineComponent::Token("".to_string())
                        }
                    }
                    _ => child.clone(),
                })
                .collect_vec(),
            is_open: true,
            pending_break_line_points: vec![],
        }
    }
}
/// Represents a comment line in the code.
#[derive(Clone, PartialEq, Eq)]
struct CommentLine {
    /// The number of slashes in the comment prefix.
    n_slashes: usize,
    /// The number of exclamation marks in the comment prefix.
    n_exclamations: usize,
    /// The number of leading spaces in the comment prefix.
    n_leading_spaces: usize,
    /// The content of the comment.
    content: String,
}

impl CommentLine {
    /// Creates a new comment prefix.
    pub fn from_string(mut comment_line: String) -> Self {
        comment_line = comment_line.trim().to_string();
        let n_slashes = comment_line.chars().take_while(|c| *c == '/').count();
        comment_line = comment_line.chars().skip(n_slashes).collect();
        let n_exclamations = comment_line.chars().take_while(|c| *c == '!').count();
        comment_line = comment_line.chars().skip(n_exclamations).collect();
        let n_leading_spaces = comment_line.chars().take_while(|c| *c == ' ').count();
        let content = comment_line.chars().skip(n_leading_spaces).collect();
        Self { n_slashes, n_exclamations, n_leading_spaces, content }
    }
    /// Returns true if the comment prefix is the same as the other comment prefix.
    pub fn is_same_prefix(&self, other: &Self) -> bool {
        self.n_slashes == other.n_slashes
            && self.n_exclamations == other.n_exclamations
            && self.n_leading_spaces == other.n_leading_spaces
    }
    /// Returns true if the comment ends with an alphanumeric character, or a comma, indicating that
    /// the next line is probably a continuation of the comment, and thus in case of a line break it
    /// should prepend the content of the next line.
    pub fn is_open_line(&self) -> bool {
        self.content.ends_with(|c: char| c.is_alphanumeric() || c == ',')
    }
}

impl fmt::Display for CommentLine {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}{}{}{}",
            "/".repeat(self.n_slashes),
            "!".repeat(self.n_exclamations),
            " ".repeat(self.n_leading_spaces),
            self.content.trim()
        )
    }
}

/// Formats a comment to fit in the line width. There are no merges of lines, as this is not clear
/// when to merge two lines the user choose to write on separate lines, so all original line breaks
/// are preserved.
fn format_leading_comment(content: &str, cur_indent: usize, max_line_width: usize) -> String {
    let mut formatted_comment = String::new();
    let mut prev_comment_line = CommentLine::from_string("".to_string());
    let append_line = |formatted_comment: &mut String, comment_line: &CommentLine| {
        formatted_comment.push_str(&" ".repeat(cur_indent));
        formatted_comment.push_str(&comment_line.to_string());
        formatted_comment.push('\n');
    };
    let mut last_line_broken = false;
    for line in content.lines() {
        let orig_comment_line = CommentLine::from_string(line.to_string());
        let max_comment_width = max_line_width
            - cur_indent
            - orig_comment_line.n_slashes
            - orig_comment_line.n_exclamations
            - orig_comment_line.n_leading_spaces;
        // The current line is initialized with the previous line only if it was broken (to avoid
        // merging user separated lines).
        let mut current_line = if last_line_broken
            && prev_comment_line.is_open_line()
            && prev_comment_line.is_same_prefix(&orig_comment_line)
        {
            prev_comment_line.content += " ";
            prev_comment_line
        } else {
            append_line(&mut formatted_comment, &prev_comment_line);
            CommentLine { content: "".to_string(), ..orig_comment_line }
        };
        last_line_broken = false;
        for word in orig_comment_line.content.split(' ') {
            if current_line.content.is_empty()
                || current_line.content.len() + word.len() <= max_comment_width
            {
                current_line.content.push_str(word);
                current_line.content.push(' ');
            } else {
                append_line(&mut formatted_comment, &current_line);
                last_line_broken = true;
                current_line = CommentLine { content: word.to_string(), ..current_line };
                current_line.content.push(' ');
            }
        }
        prev_comment_line = CommentLine {
            n_slashes: orig_comment_line.n_slashes,
            n_exclamations: orig_comment_line.n_exclamations,
            n_leading_spaces: orig_comment_line.n_leading_spaces,
            content: current_line.content.trim().to_string(),
        };
    }
    append_line(&mut formatted_comment, &prev_comment_line);
    // Remove the leading spaces of the first line, as they are added by the LineBuilder for the
    // first line.
    formatted_comment.trim().to_string()
}

/// A struct holding all the data of the pending line to be emitted.
struct PendingLineState {
    /// Intermediate representation of the text to be emitted.
    line_buffer: LineBuilder,
    /// Should the next space between tokens be ignored.
    prevent_next_space: bool,
}

impl PendingLineState {
    pub fn new() -> Self {
        Self { line_buffer: LineBuilder::default(), prevent_next_space: true }
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
    fn force_no_space_before(&self, db: &dyn SyntaxGroup) -> bool;
    /// Returns true if a token should never have a space after it.
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
        config: &FormatterConfig,
    ) -> BreakLinePointsPositions;
    /// If self is a protected zone, returns its precedence (highest precedence == lowest number).
    /// Otherwise, returns None.
    fn get_protected_zone_precedence(&self, db: &dyn SyntaxGroup) -> Option<usize>;
    fn should_skip_terminal(&self, db: &dyn SyntaxGroup) -> bool;
    /// Returns the sorting kind of the syntax node. This method will be used to sections in the
    /// syntax tree.
    fn as_sort_kind(&self, db: &dyn SyntaxGroup) -> SortKind;
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
    /// Indicates whether the last element handled was a comment.
    is_last_element_comment: bool,
    is_merging_use_items: bool,
}

impl<'a> FormatterImpl<'a> {
    pub fn new(db: &'a dyn SyntaxGroup, config: FormatterConfig) -> Self {
        Self {
            db,
            config,
            line_state: PendingLineState::new(),
            empty_lines_allowance: 0,
            is_current_line_whitespaces: true,
            is_last_element_comment: false,
            is_merging_use_items: false,
        }
    }
    /// Gets a root of a syntax tree and returns the formatted string of the code it represents.
    pub fn get_formatted_string(&mut self, syntax_node: &SyntaxNode) -> String {
        self.format_node(syntax_node);
        self.line_state.line_buffer.build(self.config.max_line_length, self.config.tab_size)
    }
    /// Appends a formatted string, representing the syntax_node, to the result.
    /// Should be called with a root syntax node to format a file.
    fn format_node(&mut self, syntax_node: &SyntaxNode) {
        if self.is_merging_use_items {
            // When merging, only format this node once and return to avoid recursion.
            self.line_state.line_buffer.push_str(syntax_node.get_text(self.db).trim());
            return;
        }
        if syntax_node.text(self.db).is_some() {
            panic!("Token reached before terminal.");
        }
        let protected_zone_precedence = syntax_node.get_protected_zone_precedence(self.db);
        let node_break_points = syntax_node.get_wrapping_break_line_point_properties(self.db);
        self.append_break_line_point(node_break_points.leading());
        if let Some(precedence) = protected_zone_precedence {
            self.line_state.line_buffer.open_sub_builder(precedence);
        }
        if syntax_node.force_no_space_before(self.db) {
            self.line_state.prevent_next_space = true;
        }
        if self.should_ignore_node_format(syntax_node) {
            self.line_state.line_buffer.push_str(syntax_node.get_text(self.db).trim());
        } else if syntax_node.kind(self.db).is_terminal() {
            self.format_terminal(syntax_node);
        } else {
            self.format_internal(syntax_node);
        }
        if syntax_node.force_no_space_after(self.db) {
            self.line_state.prevent_next_space = true;
        }
        if protected_zone_precedence.is_some() {
            self.line_state.line_buffer.close_sub_builder();
        }
        if let Some(mut trailing_break_point) = node_break_points.trailing() {
            if self.is_last_element_comment {
                trailing_break_point.unset_comma_if_broken();
            }
            self.append_break_line_point(Some(trailing_break_point));
        }
    }

    /// Formats an internal node and appends the formatted string to the result.
    fn format_internal(&mut self, syntax_node: &SyntaxNode) {
        let allowed_empty_between = syntax_node.allowed_empty_between(self.db);
        let internal_break_line_points_positions =
            syntax_node.get_internal_break_line_point_properties(self.db, &self.config);
        // TODO(ilya): consider not copying here.
        let mut children = self.db.get_children(syntax_node.clone()).to_vec();
        let n_children = children.len();

        if self.config.merge_use_items {
            self.merge_use_items(&mut children);
        }

        if self.config.sort_module_level_items {
            self.sort_items_sections(&mut children);
            if let SyntaxKind::UsePathList = syntax_node.kind(self.db) {
                self.sort_inner_use_path(&mut children);
            }
        }

        // Format each child node, inserting breaks where specified.
        for (i, child) in children.iter().enumerate() {
            if child.width(self.db) == TextWidth::default() {
                continue;
            }

            self.format_node(child);

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

    /// Merges `use` statements within a given set of syntax nodes, organizing and deduplicating
    /// them into a clean, structured format.
    fn merge_use_items(&mut self, children: &mut Vec<SyntaxNode>) {
        let mut new_children = Vec::new();

        for (section_kind, section_nodes) in extract_sections(children, self.db) {
            if section_kind != SortKind::UseItem {
                new_children.extend(section_nodes.iter().cloned());
                continue;
            }

            let mut decoration_to_use_tree: OrderedHashMap<String, UseTree> =
                OrderedHashMap::default();

            for node in section_nodes {
                if !self.has_only_whitespace_trivia(node) {
                    new_children.push(node.clone());
                    continue;
                }

                let use_item = ast::ItemUse::from_syntax_node(self.db, node.clone());

                let decorations = chain!(
                    use_item
                        .attributes(self.db)
                        .elements(self.db)
                        .into_iter()
                        .map(|attr| attr.as_syntax_node().get_text_without_trivia(self.db)),
                    [use_item.visibility(self.db).as_syntax_node().get_text(self.db)],
                )
                .join("\n");

                let tree = decoration_to_use_tree.entry(decorations).or_default();
                tree.insert_path(self.db, use_item.use_path(self.db));
            }

            // Generate merged syntax nodes from the `decoration_to_use_tree`.
            for (decorations, tree) in decoration_to_use_tree {
                let merged_node = tree.generate_syntax_node_from_use(
                    self.db,
                    self.config.allow_duplicate_uses,
                    decorations,
                );

                // Add merged children to the new_children list.
                let children = self.db.get_children(merged_node.clone()).to_vec();
                if !children.is_empty() {
                    let grandchildren = self.db.get_children(children[0].clone()).to_vec();
                    for child in grandchildren {
                        new_children.push(child.clone());
                    }
                }
            }
        }

        *children = new_children;
    }

    /// Returns whether the node has only whitespace trivia.
    fn has_only_whitespace_trivia(&self, node: &SyntaxNode) -> bool {
        node.descendants(self.db).all(|descendant| {
            if let Some(trivia) = ast::Trivia::cast(self.db, descendant) {
                trivia.elements(self.db).into_iter().all(|element| {
                    matches!(element, ast::Trivium::Whitespace(_) | ast::Trivium::Newline(_))
                })
            } else {
                true
            }
        })
    }

    /// Sorting function for `UsePathMulti` children.
    fn sort_inner_use_path(&self, children: &mut Vec<SyntaxNode>) {
        // If any child has non-trivial trivia, do not sort.
        if children.iter().any(|child| !self.has_only_whitespace_trivia(child)) {
            return;
        }
        // Split list into `use` path parts and TokenComma.
        let (mut sorted_elements, commas): (Vec<_>, Vec<_>) = std::mem::take(children)
            .into_iter()
            .partition(|node| node.kind(self.db) != SyntaxKind::TerminalComma);

        // Sort the filtered nodes by comparing their `UsePath`.
        sorted_elements.sort_by(|a_node, b_node| {
            let a_use_path = extract_use_path(a_node, self.db);
            let b_use_path = extract_use_path(b_node, self.db);

            match (a_use_path, b_use_path) {
                (Some(a_path), Some(b_path)) => compare_use_paths(&a_path, &b_path, self.db),
                (None, Some(_)) => Ordering::Less,
                (Some(_), None) => Ordering::Greater,
                (None, None) => Ordering::Equal,
            }
        });

        // Intersperse the sorted elements with commas.
        *children = itertools::Itertools::intersperse_with(sorted_elements.into_iter(), || {
            commas.first().cloned().unwrap()
        })
        .collect();
    }

    /// Sorting function for module-level items.
    fn sort_items_sections(&self, children: &mut Vec<SyntaxNode>) {
        let sections = extract_sections(children, self.db);
        let mut sorted_children = Vec::with_capacity(children.len());
        for (section_kind, section_nodes) in sections {
            match section_kind {
                SortKind::Module => {
                    // Sort `Module` items alphabetically by their name.
                    let mut sorted_section = section_nodes.to_vec();
                    sorted_section.sort_by_key(|node| {
                        ast::ItemModule::from_syntax_node(self.db, node.clone())
                            .name(self.db)
                            .text(self.db)
                    });
                    sorted_children.extend(sorted_section);
                }
                SortKind::UseItem => {
                    // Sort `UseItem` items based on their use paths.
                    let mut sorted_section = section_nodes.to_vec();
                    sorted_section.sort_by(|a, b| {
                        compare_use_paths(
                            &ast::ItemUse::from_syntax_node(self.db, a.clone()).use_path(self.db),
                            &ast::ItemUse::from_syntax_node(self.db, b.clone()).use_path(self.db),
                            self.db,
                        )
                    });
                    sorted_children.extend(sorted_section);
                }
                SortKind::Immovable => {
                    sorted_children.extend(section_nodes.iter().cloned());
                }
            }
        }

        *children = sorted_children;
    }

    /// Formats a terminal node and appends the formatted string to the result.
    fn format_terminal(&mut self, syntax_node: &SyntaxNode) {
        // TODO(spapini): Introduce a Terminal and a Token enum in ast.rs to make this cleaner.
        let children = self.db.get_children(syntax_node.clone());
        let mut children_iter = children.iter().cloned();
        let leading_trivia = ast::Trivia::from_syntax_node(self.db, children_iter.next().unwrap());
        let token = children_iter.next().unwrap();
        let trailing_trivia = ast::Trivia::from_syntax_node(self.db, children_iter.next().unwrap());

        // The first newlines is the leading trivia correspond exactly to empty lines.
        self.format_trivia(leading_trivia, true);
        if !syntax_node.should_skip_terminal(self.db) {
            self.format_token(&token);
        }
        self.format_trivia(trailing_trivia, false);
    }
    /// Appends a trivia node (if needed) to the result.
    fn format_trivia(&mut self, trivia: syntax::node::ast::Trivia, is_leading: bool) {
        for trivium in trivia.elements(self.db) {
            match trivium {
                ast::Trivium::SingleLineComment(_)
                | ast::Trivium::SingleLineDocComment(_)
                | ast::Trivium::SingleLineInnerComment(_) => {
                    if !is_leading {
                        self.line_state.line_buffer.push_space();
                    }
                    self.line_state.line_buffer.push_comment(
                        &trivium.as_syntax_node().text(self.db).unwrap(),
                        !is_leading,
                    );
                    self.is_current_line_whitespaces = false;
                    self.empty_lines_allowance = 1;
                    self.is_last_element_comment = true;
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
                    self.format_token(&trivium.as_syntax_node());
                }
                ast::Trivium::SkippedNode(node) => {
                    self.format_node(&node.as_syntax_node());
                }
            }
        }
    }
    /// Formats a token node and appends it to the result.
    /// Assumes the given SyntaxNode is a token.
    fn format_token(&mut self, syntax_node: &SyntaxNode) {
        let text = syntax_node.text(self.db).unwrap();
        if !syntax_node.force_no_space_before(self.db) && !self.line_state.prevent_next_space {
            self.line_state.line_buffer.push_space();
        }
        self.line_state.prevent_next_space = syntax_node.force_no_space_after(self.db);
        if syntax_node.kind(self.db) != SyntaxKind::TokenWhitespace {
            self.is_current_line_whitespaces = false;
        }
        let node_break_points = syntax_node.get_wrapping_break_line_point_properties(self.db);
        self.append_break_line_point(node_break_points.leading());
        self.line_state.line_buffer.push_str(&text);
        self.append_break_line_point(node_break_points.trailing());
        self.is_last_element_comment = false;
    }
    fn append_break_line_point(&mut self, properties: Option<BreakLinePointProperties>) {
        if let Some(properties) = properties {
            self.line_state.line_buffer.push_break_line_point(properties);
            self.line_state.prevent_next_space = true;
        }
    }

    /// Gets a syntax node and returns if the node has an cairofmt::skip attribute.
    pub fn should_ignore_node_format(&self, syntax_node: &SyntaxNode) -> bool {
        syntax_node.has_attr(self.db, FMT_SKIP_ATTR)
    }
}

/// Compares two `UsePath` nodes to determine their ordering.
fn compare_use_paths(a: &UsePath, b: &UsePath, db: &dyn SyntaxGroup) -> Ordering {
    match (a, b) {
        // Case for multi vs multi.
        (UsePath::Multi(a_multi), UsePath::Multi(b_multi)) => {
            let get_min_child = |multi: &ast::UsePathMulti| {
                multi.use_paths(db).elements(db).into_iter().min_by_key(|child| match child {
                    UsePath::Leaf(leaf) => leaf.extract_ident(db),
                    UsePath::Single(single) => single.extract_ident(db),
                    _ => "".to_string(),
                })
            };
            match (get_min_child(a_multi), get_min_child(b_multi)) {
                (Some(a_min), Some(b_min)) => compare_use_paths(&a_min, &b_min, db),
                (None, Some(_)) => Ordering::Less,
                (Some(_), None) => Ordering::Greater,
                (None, None) => Ordering::Equal,
            }
        }

        // Case for multi is always after other types of paths.
        (UsePath::Multi(_), _) => Ordering::Greater,
        (_, UsePath::Multi(_)) => Ordering::Less,

        // Case for Leaf vs Single and Single vs Leaf.
        (UsePath::Leaf(a_leaf), UsePath::Single(b_single)) => {
            let a_str = a_leaf.extract_ident(db);
            let b_str = b_single.extract_ident(db);

            match a_str.cmp(&b_str) {
                // Leaf is always ordered before Single if equal.
                Ordering::Equal => Ordering::Less,
                other => other,
            }
        }

        (UsePath::Single(a_single), UsePath::Leaf(b_leaf)) => {
            let a_str = a_single.extract_ident(db);
            let b_str = b_leaf.extract_ident(db);

            // Compare the extracted identifiers.
            match a_str.cmp(&b_str) {
                // Single is ordered after Leaf if equal.
                Ordering::Equal => Ordering::Greater,
                other => other,
            }
        }

        // Case for Leaf vs Leaf: compare their identifiers, and only if they are equal, compare
        // their aliases.
        (UsePath::Leaf(a_leaf), UsePath::Leaf(b_leaf)) => {
            match a_leaf.extract_ident(db).cmp(&b_leaf.extract_ident(db)) {
                Ordering::Equal => a_leaf.extract_alias(db).cmp(&b_leaf.extract_alias(db)),
                other => other,
            }
        }

        // Case for Single vs Single: compare their identifiers, then move to the next segment if
        // equal.
        (UsePath::Single(a_single), UsePath::Single(b_single)) => {
            let a_ident = a_single.extract_ident(db);
            let b_ident = b_single.extract_ident(db);

            match (a_ident.as_str(), b_ident.as_str()) {
                ("super" | "crate", "super" | "crate") => a_ident.cmp(&b_ident),
                ("super" | "crate", _) => Ordering::Greater,
                (_, "super" | "crate") => Ordering::Less,
                _ => match a_ident.cmp(&b_ident) {
                    Ordering::Equal => {
                        compare_use_paths(&a_single.use_path(db), &b_single.use_path(db), db)
                    }
                    other => other,
                },
            }
        }

        // Star is always considered first.
        (UsePath::Star(_), UsePath::Star(_)) => Ordering::Equal,
        (UsePath::Star(_), _) => Ordering::Less,
        (_, UsePath::Star(_)) => Ordering::Greater,
    }
}

/// Helper function to extract `UsePath` from a `SyntaxNode`.
fn extract_use_path(node: &SyntaxNode, db: &dyn SyntaxGroup) -> Option<ast::UsePath> {
    match node.kind(db) {
        SyntaxKind::UsePathLeaf => {
            Some(ast::UsePath::Leaf(ast::UsePathLeaf::from_syntax_node(db, node.clone())))
        }
        SyntaxKind::UsePathSingle => {
            Some(ast::UsePath::Single(ast::UsePathSingle::from_syntax_node(db, node.clone())))
        }
        SyntaxKind::UsePathMulti => {
            Some(ast::UsePath::Multi(ast::UsePathMulti::from_syntax_node(db, node.clone())))
        }
        SyntaxKind::UsePathStar => {
            Some(ast::UsePath::Star(ast::UsePathStar::from_syntax_node(db, node.clone())))
        }
        _ => None,
    }
}

/// A trait for extracting identifiers from UsePathLeaf and UsePathSingle.
trait IdentExtractor {
    /// Extracts the identifier and aliases from the syntax node, removing any trivia.
    fn extract_ident(&self, db: &dyn SyntaxGroup) -> String;
    fn extract_alias(&self, db: &dyn SyntaxGroup) -> Option<String>;
}
impl IdentExtractor for ast::UsePathLeaf {
    fn extract_ident(&self, db: &dyn SyntaxGroup) -> String {
        self.ident(db).as_syntax_node().get_text_without_trivia(db)
    }

    fn extract_alias(&self, db: &dyn SyntaxGroup) -> Option<String> {
        match self.alias_clause(db) {
            ast::OptionAliasClause::Empty(_) => None,
            ast::OptionAliasClause::AliasClause(alias_clause) => {
                Some(alias_clause.alias(db).as_syntax_node().get_text_without_trivia(db))
            }
        }
    }
}

impl IdentExtractor for ast::UsePathSingle {
    fn extract_ident(&self, db: &dyn SyntaxGroup) -> String {
        self.ident(db).as_syntax_node().get_text_without_trivia(db)
    }

    fn extract_alias(&self, _db: &dyn SyntaxGroup) -> Option<String> {
        None
    }
}

/// Extracts sections of syntax nodes based on their `SortKind`.
fn extract_sections<'a>(
    children: &'a [SyntaxNode],
    db: &'a dyn SyntaxGroup,
) -> Vec<(SortKind, &'a [SyntaxNode])> {
    let mut sections = Vec::new();
    let mut start_idx = 0;

    while start_idx < children.len() {
        let kind = children[start_idx].as_sort_kind(db);
        let mut end_idx = start_idx + 1;
        while end_idx < children.len() && kind == children[end_idx].as_sort_kind(db) {
            end_idx += 1;
        }
        sections.push((kind, &children[start_idx..end_idx]));
        start_idx = end_idx;
    }

    sections
}

/// Represents the kind of sections in the syntax tree that can be sorted.
/// Classify consecutive nodes into sections that are eligible for sorting.
#[derive(PartialEq, Eq)]
pub enum SortKind {
    /// Module items without body, e.g. `mod a;`.
    Module,

    /// Use items, e.g. `use a::b;` or `use c::{d, e as f};`.
    UseItem,

    /// Items that cannot be moved - would be skipped and not included in any sorted segment.
    Immovable,
}
