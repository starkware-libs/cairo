use std::collections::HashMap;

use defs::db::DefsGroup;
use filesystem::span::{TextOffset, TextSpan};
use syntax::node::db::SyntaxGroup;
use syntax::node::{SyntaxNode, TypedSyntaxNode};
use utils::extract_matches;

/// Interface for modifying syntax nodes.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum RewriteNode {
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
                    children: syntax_node.children(db).map(RewriteNode::from).collect(),
                });
                extract_matches!(self, RewriteNode::Modified)
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

    /// Interpolates a string with patches.
    /// Each substring of the form `$<name>$` is replaced with syntax nodes from `replaces`.
    /// The `$$` substring is replaced with `$`.
    pub fn interpolate_patched(&mut self, code: &str, replaces: HashMap<String, RewriteNode>) {
        let mut chars = code.chars().peekable();
        while let Some(c) = chars.next() {
            if c != '$' {
                self.add_char(c);
                continue;
            }

            // An opening $ was detected.
            let mut name = String::new();
            for d in chars.by_ref() {
                if d == '$' {
                    break;
                }
                name.push(d);
            }

            // A closing $ was found.
            // If the string between the `$` is empty, push a single `$` to the output.
            if name.is_empty() {
                self.add_char('$');
                continue;
            }

            // Replace the substring with a syntax node.
            self.add_modified(replaces[&name].clone());
        }
    }
}
