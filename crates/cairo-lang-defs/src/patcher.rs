use cairo_lang_filesystem::ids::{CodeMapping, CodeOrigin};
use cairo_lang_filesystem::span::{TextOffset, TextSpan, TextWidth};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{SyntaxNode, TypedSyntaxNode};
use cairo_lang_utils::extract_matches;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use itertools::Itertools;

/// Interface for modifying syntax nodes.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum RewriteNode {
    /// A rewrite node that represents a trimmed copy of a syntax node:
    /// one with the leading and trailing trivia excluded.
    Trimmed {
        node: SyntaxNode,
        trim_left: bool,
        trim_right: bool,
    },
    Copied(SyntaxNode),
    Modified(ModifiedNode),
    Mapped {
        origin: TextSpan,
        node: Box<RewriteNode>,
    },
    Text(String),
    TextAndMapping(String, Vec<CodeMapping>),
}
impl RewriteNode {
    pub fn new_trimmed(syntax_node: SyntaxNode) -> Self {
        Self::Trimmed { node: syntax_node, trim_left: true, trim_right: true }
    }

    pub fn new_modified(children: Vec<RewriteNode>) -> Self {
        Self::Modified(ModifiedNode { children: Some(children) })
    }

    pub fn text(text: &str) -> Self {
        Self::Text(text.to_string())
    }

    pub fn mapped_text(
        text: impl Into<String>,
        db: &dyn SyntaxGroup,
        origin: &impl TypedSyntaxNode,
    ) -> Self {
        RewriteNode::Text(text.into()).mapped(db, origin)
    }

    pub fn empty() -> Self {
        Self::text("")
    }

    /// Creates a rewrite node from an AST object.
    pub fn from_ast(node: &impl TypedSyntaxNode) -> Self {
        RewriteNode::Copied(node.as_syntax_node())
    }

    /// Creates a rewrite node from an AST object - with .
    pub fn from_ast_trimmed(node: &impl TypedSyntaxNode) -> Self {
        Self::new_trimmed(node.as_syntax_node())
    }

    /// Prepares a node for modification.
    pub fn modify(&mut self, db: &dyn SyntaxGroup) -> &mut ModifiedNode {
        match self {
            RewriteNode::Copied(syntax_node) => {
                *self = RewriteNode::new_modified(
                    db.get_children(syntax_node.clone())
                        .iter()
                        .cloned()
                        .map(RewriteNode::Copied)
                        .collect(),
                );
                extract_matches!(self, RewriteNode::Modified)
            }
            RewriteNode::Trimmed { node, trim_left, trim_right } => {
                let children = db.get_children(node.clone());
                let num_children = children.len();
                let mut new_children = Vec::new();

                // Get the index of the leftmost nonempty child.
                let Some(left_idx) =
                    children.iter().position(|child| child.width(db) != TextWidth::default())
                else {
                    *self = RewriteNode::Modified(ModifiedNode { children: None });
                    return extract_matches!(self, RewriteNode::Modified);
                };
                // Get the index of the rightmost nonempty child.
                let right_idx = children
                    .iter()
                    .rposition(|child| child.width(db) != TextWidth::default())
                    .unwrap();
                new_children.extend(itertools::repeat_n(
                    RewriteNode::Modified(ModifiedNode { children: None }),
                    left_idx,
                ));

                // The number of children between the first and last nonempty nodes.
                let num_middle = right_idx - left_idx + 1;
                let children = db.get_children(node.clone());
                let mut children_iter = children.iter().skip(left_idx);
                match num_middle {
                    1 => {
                        new_children.push(RewriteNode::Trimmed {
                            node: children_iter.next().unwrap().clone(),
                            trim_left: *trim_left,
                            trim_right: *trim_right,
                        });
                    }
                    _ => {
                        new_children.push(RewriteNode::Trimmed {
                            node: children_iter.next().unwrap().clone(),
                            trim_left: *trim_left,
                            trim_right: false,
                        });
                        for _ in 0..(num_middle - 2) {
                            let child = children_iter.next().unwrap().clone();
                            new_children.push(RewriteNode::Copied(child));
                        }
                        new_children.push(RewriteNode::Trimmed {
                            node: children_iter.next().unwrap().clone(),
                            trim_left: false,
                            trim_right: *trim_right,
                        });
                    }
                };
                new_children.extend(itertools::repeat_n(
                    RewriteNode::Modified(ModifiedNode { children: None }),
                    num_children - right_idx - 1,
                ));

                *self = RewriteNode::Modified(ModifiedNode { children: Some(new_children) });
                extract_matches!(self, RewriteNode::Modified)
            }
            RewriteNode::Modified(modified) => modified,
            RewriteNode::Text(_) | RewriteNode::TextAndMapping(_, _) => {
                panic!("A text node can't be modified")
            }
            RewriteNode::Mapped { .. } => panic!("A mapped node can't be modified"),
        }
    }

    /// Prepares a node for modification and returns a specific child.
    pub fn modify_child(&mut self, db: &dyn SyntaxGroup, index: usize) -> &mut RewriteNode {
        if matches!(self, RewriteNode::Modified(ModifiedNode { children: None })) {
            // Modification of an empty node is idempotent.
            return self;
        }
        &mut self.modify(db).children.as_mut().unwrap()[index]
    }

    /// Replaces this node with text.
    pub fn set_str(&mut self, s: String) {
        *self = RewriteNode::Text(s)
    }
    /// Creates a new Rewrite node by interpolating a string with patches.
    /// Each substring of the form `$<name>$` is replaced with syntax nodes from `patches`.
    /// A `$$` substring is replaced with `$`.
    pub fn interpolate_patched(
        code: &str,
        patches: &UnorderedHashMap<String, RewriteNode>,
    ) -> RewriteNode {
        let mut chars = code.chars().peekable();
        let mut pending_text = String::new();
        let mut children = Vec::new();
        while let Some(c) = chars.next() {
            if c != '$' {
                pending_text.push(c);
                continue;
            }

            // An opening $ was detected.

            // Read the name
            let mut name = String::new();
            for c in chars.by_ref() {
                if c == '$' {
                    break;
                }
                name.push(c);
            }

            // A closing $ was found.
            // If the string between the `$`s is empty - push a single `$` to the output.
            if name.is_empty() {
                pending_text.push('$');
                continue;
            }
            // If the string wasn't empty and there is some pending text, first flush it as a text
            // child.
            if !pending_text.is_empty() {
                children.push(RewriteNode::text(&pending_text));
                pending_text.clear();
            }
            // Replace the substring with the relevant rewrite node.
            // TODO(yuval): this currently panics. Fix it.
            children.push(
                patches.get(&name).cloned().unwrap_or_else(|| panic!("No patch named {}.", name)),
            );
        }
        // Flush the remaining text as a text child.
        if !pending_text.is_empty() {
            children.push(RewriteNode::text(&pending_text));
        }

        RewriteNode::new_modified(children)
    }

    /// Creates a new Rewrite node by inserting a `separator` between each two given children.
    pub fn interspersed(
        children: impl IntoIterator<Item = RewriteNode>,
        separator: RewriteNode,
    ) -> RewriteNode {
        RewriteNode::new_modified(itertools::intersperse(children, separator).collect_vec())
    }

    /// Creates a new rewrite node wrapped in a mapping to the original code.
    pub fn mapped(self, db: &dyn SyntaxGroup, origin: &impl TypedSyntaxNode) -> Self {
        RewriteNode::Mapped {
            origin: origin.as_syntax_node().span_without_trivia(db),
            node: Box::new(self),
        }
    }
}
impl Default for RewriteNode {
    fn default() -> Self {
        Self::empty()
    }
}
impl From<SyntaxNode> for RewriteNode {
    fn from(node: SyntaxNode) -> Self {
        RewriteNode::Copied(node)
    }
}

/// A modified rewrite node.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ModifiedNode {
    /// Children of the node.
    /// Can be None, in which case this is an empty node (of width 0). It's not the same as
    /// Some(vec![]) - A child can be (idempotently) modified for None, whereas modifying a child
    /// for Some(vec![]) would panic.
    pub children: Option<Vec<RewriteNode>>,
}

pub struct PatchBuilder<'a> {
    pub db: &'a dyn SyntaxGroup,
    code: String,
    code_mappings: Vec<CodeMapping>,
    origin: CodeOrigin,
}
impl<'a> PatchBuilder<'a> {
    /// Creates a new patch builder, originating from `origin` typed node.
    pub fn new(db: &'a dyn SyntaxGroup, origin: &impl TypedSyntaxNode) -> Self {
        Self::new_ex(db, &origin.as_syntax_node())
    }

    /// Creates a new patch builder, originating from `origin` node.
    pub fn new_ex(db: &'a dyn SyntaxGroup, origin: &SyntaxNode) -> Self {
        Self {
            db,
            code: String::default(),
            code_mappings: vec![],
            origin: CodeOrigin::Span(origin.span_without_trivia(db)),
        }
    }

    /// Builds the resulting code and code mappings.
    pub fn build(mut self) -> (String, Vec<CodeMapping>) {
        // Adds the mapping to the original node from all code not previously mapped.
        self.code_mappings
            .push(CodeMapping { span: TextSpan::from_str(&self.code), origin: self.origin });
        (self.code, self.code_mappings)
    }

    /// Builds the patcher into a rewrite node enabling adding it to other patchers.
    pub fn into_rewrite_node(self) -> RewriteNode {
        let (code, mappings) = self.build();
        RewriteNode::TextAndMapping(code, mappings)
    }

    pub fn add_char(&mut self, c: char) {
        self.code.push(c);
    }

    pub fn add_str(&mut self, s: &str) {
        self.code += s;
    }

    pub fn add_modified(&mut self, node: RewriteNode) {
        match node {
            RewriteNode::Copied(node) => self.add_node(node),
            RewriteNode::Mapped { origin, node } => self.add_mapped(*node, origin),
            RewriteNode::Trimmed { node, trim_left, trim_right } => {
                self.add_trimmed_node(node, trim_left, trim_right)
            }
            RewriteNode::Modified(modified) => {
                if let Some(children) = modified.children {
                    for child in children {
                        self.add_modified(child)
                    }
                }
            }
            RewriteNode::Text(s) => self.add_str(s.as_str()),
            RewriteNode::TextAndMapping(s, mappings) => {
                let mapping_fix = TextWidth::from_str(&self.code);
                self.add_str(&s);
                self.code_mappings.extend(mappings.into_iter().map(|mut mapping| {
                    mapping.span.start = mapping.span.start.add_width(mapping_fix);
                    mapping.span.end = mapping.span.end.add_width(mapping_fix);
                    mapping
                }));
            }
        }
    }

    pub fn add_node(&mut self, node: SyntaxNode) {
        let start = TextOffset::from_str(&self.code);
        let orig_span = node.span(self.db);
        self.code_mappings.push(CodeMapping {
            span: TextSpan { start, end: start.add_width(orig_span.width()) },
            origin: CodeOrigin::Start(orig_span.start),
        });
        self.code += &node.get_text(self.db);
    }

    fn add_mapped(&mut self, node: RewriteNode, origin: TextSpan) {
        let start = TextOffset::from_str(&self.code);
        self.add_modified(node);
        let end = TextOffset::from_str(&self.code);
        self.code_mappings
            .push(CodeMapping { span: TextSpan { start, end }, origin: CodeOrigin::Span(origin) });
    }

    fn add_trimmed_node(&mut self, node: SyntaxNode, trim_left: bool, trim_right: bool) {
        let TextSpan { start: trimmed_start, end: trimmed_end } = node.span_without_trivia(self.db);
        let orig_start = if trim_left { trimmed_start } else { node.span(self.db).start };
        let orig_end = if trim_right { trimmed_end } else { node.span(self.db).end };
        let origin_span = TextSpan { start: orig_start, end: orig_end };

        let text = node.get_text_of_span(self.db, origin_span);
        let start = TextOffset::from_str(&self.code);

        self.code += &text;

        self.code_mappings.push(CodeMapping {
            span: TextSpan { start, end: start.add_width(TextWidth::from_str(&text)) },
            origin: CodeOrigin::Start(orig_start),
        });
    }
}
