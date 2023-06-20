use cairo_lang_defs::db::DefsGroup;
use cairo_lang_filesystem::span::{TextOffset, TextSpan, TextWidth};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{SyntaxNode, TypedSyntaxNode};
use cairo_lang_utils::extract_matches;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;

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
    Text(String),
}
impl RewriteNode {
    pub fn new_trimmed(syntax_node: SyntaxNode) -> Self {
        Self::Trimmed { node: syntax_node, trim_left: true, trim_right: true }
    }

    pub fn new_modified(children: Vec<RewriteNode>) -> Self {
        Self::Modified(ModifiedNode { children: Some(children) })
    }

    /// Creates a rewrite node from an AST object.
    pub fn from_ast<T: TypedSyntaxNode>(node: &T) -> Self {
        RewriteNode::Copied(node.as_syntax_node())
    }

    /// Prepares a node for modification.
    pub fn modify(&mut self, db: &dyn SyntaxGroup) -> &mut ModifiedNode {
        match self {
            RewriteNode::Copied(syntax_node) => {
                *self = RewriteNode::new_modified(
                    syntax_node.children(db).map(RewriteNode::Copied).collect(),
                );
                extract_matches!(self, RewriteNode::Modified)
            }

            RewriteNode::Trimmed { node, trim_left, trim_right } => {
                let num_children = node.children(db).len();
                let mut new_children = Vec::new();

                // Get the index of the leftmost nonempty child.
                let Some(left_idx) = node.children(db).position(|child| child.width(db) != TextWidth::default()) else {
                    *self = RewriteNode::Modified(ModifiedNode { children: None });
                    return extract_matches!(self, RewriteNode::Modified);
                };
                // Get the index of the rightmost nonempty child.
                let right_idx = node
                    .children(db)
                    .rposition(|child| child.width(db) != TextWidth::default())
                    .unwrap();
                new_children.extend(itertools::repeat_n(
                    RewriteNode::Modified(ModifiedNode { children: None }),
                    left_idx,
                ));

                // The number of children between the first and last nonempty nodes.
                let num_middle = right_idx - left_idx + 1;
                let mut children_iter = node.children(db).skip(left_idx);
                match num_middle {
                    1 => {
                        new_children.push(RewriteNode::Trimmed {
                            node: children_iter.next().unwrap(),
                            trim_left: *trim_left,
                            trim_right: *trim_right,
                        });
                    }
                    _ => {
                        new_children.push(RewriteNode::Trimmed {
                            node: children_iter.next().unwrap(),
                            trim_left: *trim_left,
                            trim_right: false,
                        });
                        for _ in 0..(num_middle - 2) {
                            let child = children_iter.next().unwrap();
                            new_children.push(RewriteNode::Copied(child));
                        }
                        new_children.push(RewriteNode::Trimmed {
                            node: children_iter.next().unwrap(),
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
            RewriteNode::Text(_) => panic!("A text node can't be modified"),
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
        patches: UnorderedHashMap<String, RewriteNode>,
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
                children.push(RewriteNode::Text(pending_text.clone()));
                pending_text.clear();
            }
            // Replace the substring with the relevant rewrite node.
            // TODO(yuval): this currently panics. Fix it.
            children.push(patches[&name].clone());
        }
        // Flush the remaining text as a text child.
        if !pending_text.is_empty() {
            children.push(RewriteNode::Text(pending_text.clone()));
        }

        RewriteNode::new_modified(children)
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
    /// Some(vec![]) - None can be (idempotently) modified, whereas modifying Some(vec![]) would
    /// panic.
    pub children: Option<Vec<RewriteNode>>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Patch {
    span: TextSpan,
    origin_span: TextSpan,
}

#[derive(Debug, Default, PartialEq, Eq)]
pub struct Patches {
    patches: Vec<Patch>,
}
impl Patches {
    pub fn translate(&self, _db: &dyn DefsGroup, span: TextSpan) -> Option<TextSpan> {
        for Patch { span: patch_span, origin_span } in &self.patches {
            if patch_span.contains(span) {
                let start = origin_span.start.add_width(span.start - patch_span.start);
                return Some(TextSpan { start, end: start.add_width(span.end - span.start) });
            }
        }
        None
    }
}

pub struct PatchBuilder<'a> {
    pub db: &'a dyn SyntaxGroup,
    pub code: String,
    pub patches: Patches,
}
impl<'a> PatchBuilder<'a> {
    pub fn new(db: &'a dyn SyntaxGroup) -> Self {
        Self { db, code: String::default(), patches: Patches::default() }
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
        }
    }

    pub fn add_node(&mut self, node: SyntaxNode) {
        let orig_span = node.span(self.db);
        let start = TextOffset::default().add_width(TextWidth::from_str(&self.code));
        self.patches.patches.push(Patch {
            span: TextSpan { start, end: start.add_width(orig_span.end - orig_span.start) },
            origin_span: node.span(self.db),
        });
        self.code += node.get_text(self.db).as_str();
    }

    fn add_trimmed_node(&mut self, node: SyntaxNode, trim_left: bool, trim_right: bool) {
        let TextSpan { start: trimmed_start, end: trimmed_end } = node.span_without_trivia(self.db);
        let orig_start = if trim_left { trimmed_start } else { node.span(self.db).start };
        let orig_end = if trim_right { trimmed_end } else { node.span(self.db).end };
        let origin_span = TextSpan { start: orig_start, end: orig_end };

        let text = node.get_text_of_span(self.db, origin_span);
        let start = TextOffset::default().add_width(TextWidth::from_str(&self.code));

        self.code += &text;

        self.patches.patches.push(Patch {
            span: TextSpan { start, end: start.add_width(TextWidth::from_str(&text)) },
            origin_span,
        });
    }
}
