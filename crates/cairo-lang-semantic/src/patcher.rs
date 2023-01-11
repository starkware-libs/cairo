use std::collections::HashMap;

use cairo_lang_defs::db::DefsGroup;
use cairo_lang_filesystem::span::{TextOffset, TextSpan};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{SyntaxNode, TypedSyntaxNode};
use cairo_lang_utils::extract_matches;

/// Interface for modifying syntax nodes.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum RewriteNode {
    /// A rewrite node that represents a trimmed copy of a syntax node:
    /// one with the leading and trailing trivia excluded.
    Trimmed(SyntaxNode),
    Copied(SyntaxNode),
    Modified(ModifiedNode),
    Text(String),
}
impl RewriteNode {
    /// Creates a rewrite node from an AST object.
    pub fn from_ast<T: TypedSyntaxNode>(node: &T) -> Self {
        RewriteNode::Copied(node.as_syntax_node())
    }

    /// Prepares a node for modification.
    pub fn modify(&mut self, db: &dyn SyntaxGroup) -> &mut ModifiedNode {
        match self {
            RewriteNode::Copied(syntax_node) => {
                *self = RewriteNode::Modified(ModifiedNode {
                    children: syntax_node.children(db).map(RewriteNode::Copied).collect(),
                });
                extract_matches!(self, RewriteNode::Modified)
            }
            RewriteNode::Trimmed(_) => {
                panic!("Not supported.")
            }
            RewriteNode::Modified(modified) => modified,
            RewriteNode::Text(_) => {
                *self = RewriteNode::Modified(ModifiedNode { children: vec![] });
                extract_matches!(self, RewriteNode::Modified)
            }
        }
    }

    /// Prepares a node for modification and returns a specific child.
    pub fn modify_child(&mut self, db: &dyn SyntaxGroup, index: usize) -> &mut RewriteNode {
        &mut self.modify(db).children[index]
    }

    /// Replaces this node with text.
    pub fn set_str(&mut self, s: String) {
        *self = RewriteNode::Text(s)
    }
    /// Creates a new Rewrite node by interpolating a string with patches.
    /// Each substring of the form `$<name>$` is replaced with syntax nodes from `patches`.
    /// A `$$` substring is replaced with `$`.
    pub fn interpolate_patched(code: &str, patches: HashMap<String, RewriteNode>) -> RewriteNode {
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

        RewriteNode::Modified(ModifiedNode { children })
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
    pub children: Vec<RewriteNode>,
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
                let start = origin_span.start.add(span.start - patch_span.start);
                return Some(TextSpan { start, end: start.add(span.end - span.start) });
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
            RewriteNode::Trimmed(node) => self.add_trimmed_node(node),
            RewriteNode::Modified(modified) => {
                for child in modified.children {
                    self.add_modified(child)
                }
            }
            RewriteNode::Text(s) => self.add_str(s.as_str()),
        }
    }

    pub fn add_node(&mut self, node: SyntaxNode) {
        let orig_span = node.span(self.db);
        let start = TextOffset(self.code.len());
        self.patches.patches.push(Patch {
            span: TextSpan { start, end: start.add(orig_span.end - orig_span.start) },
            origin_span: node.span(self.db),
        });
        self.code += node.get_text(self.db).as_str();
    }

    pub fn add_trimmed_node(&mut self, node: SyntaxNode) {
        let origin_span = node.span_without_trivia(self.db);
        let text = node.get_text_of_span(self.db, origin_span);
        let start = TextOffset(self.code.len());

        self.code += &text;

        self.patches
            .patches
            .push(Patch { span: TextSpan { start, end: start.add(text.len()) }, origin_span });
    }
}
