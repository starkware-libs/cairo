use std::collections::HashMap;

use defs::db::DefsGroup;
use filesystem::span::{TextOffset, TextSpan};
use syntax::node::db::SyntaxGroup;
use syntax::node::SyntaxNode;

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

#[derive(Default)]
pub struct PatchBuilder {
    pub code: String,
    pub patches: Patches,
}
impl PatchBuilder {
    fn add_char(&mut self, c: char) {
        self.code.push(c);
    }
    fn add_node(&mut self, db: &dyn SyntaxGroup, node: SyntaxNode) {
        let orig_span = node.span(db);
        let start = TextOffset(self.code.len());
        self.patches.patches.push(Patch {
            span: TextSpan { start, end: start.add(orig_span.end - orig_span.start) },
            origin_span: node.span(db),
        });
        self.code += node.get_text(db).as_str();
    }
}

/// Interpolare a string with patches.
/// Each substring of the form `$<name>$` is replaced with syntax nodes from `replaces`.
/// The `$$` substring is replaced with `$`.
pub fn interpolate_patched(
    db: &dyn SyntaxGroup,
    code: &str,
    replaces: HashMap<String, SyntaxNode>,
) -> PatchBuilder {
    let mut builder = PatchBuilder::default();
    let mut chars = code.chars().peekable();
    while let Some(c) = chars.next() {
        if c != '$' {
            builder.add_char(c);
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
            builder.add_char('$');
            continue;
        }

        // Replace the substring with a syntax node.
        builder.add_node(db, replaces[&name].clone());
    }
    builder
}
