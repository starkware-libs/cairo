use cairo_lang_defs::db::DefsGroup;
use cairo_lang_doc::db::{DocGroup, Documentation};
use cairo_lang_filesystem::ids::FileId;
use cairo_lang_syntax::node::ast::TerminalIdentifier;
use cairo_lang_syntax::node::TypedSyntaxNode;
use cairo_lang_utils::Upcast;
use itertools::chain;
use tower_lsp::lsp_types::Hover;

use crate::ide::hover::markdown_contents;
use crate::lang::db::AnalysisDatabase;
use crate::lang::inspect::defs::{MemberDef, SymbolDef};
use crate::lang::lsp::ToLsp;
use crate::markdown::{fenced_code_block, RULE};

/// Get declaration and documentation "definition" of an item referred by the given identifier.
#[tracing::instrument(level = "trace", skip_all)]
pub fn definition(
    db: &AnalysisDatabase,
    identifier: &TerminalIdentifier,
    file_id: FileId,
) -> Option<Hover> {
    let symbol = SymbolDef::find(db, identifier)?;

    let md = match &symbol {
        SymbolDef::Item(item) => {
            let mut md = String::new();
            md += &fenced_code_block(&item.definition_path(db));
            if let Some(signature) = item.signature(db) {
                md += &fenced_code_block(&signature);
            }

            let item_documentation = item.documentation(db);
            if let Some(doc) = parse_and_concat_documentation(item_documentation) {
                md += RULE;
                md += &doc;
            }
            md
        }

        SymbolDef::Variable(var) => fenced_code_block(&var.signature(db)),
        SymbolDef::ExprInlineMacro(macro_name) => {
            let mut md = fenced_code_block(macro_name);
            if let Some(doc) = db.inline_macro_plugins().get(macro_name)?.documentation() {
                md += RULE;
                md += &doc;
            }
            md
        }
        SymbolDef::Member(MemberDef { member, structure }) => {
            let mut md = String::new();

            // Signature is the signature of the struct, so it makes sense that the definition
            // path is too.
            md += &fenced_code_block(&structure.definition_path(db));
            if let Some(signature) = structure.signature(db) {
                md += &fenced_code_block(&signature);
            }
            let item_documentation = db.get_item_documentation((*member).into());
            if let Some(doc) = parse_and_concat_documentation(item_documentation) {
                md += RULE;
                md += &doc;
            }
            md
        }
    };

    Some(Hover {
        contents: markdown_contents(md),
        range: identifier
            .as_syntax_node()
            .span_without_trivia(db.upcast())
            .position_in_file(db.upcast(), file_id)
            .map(|p| p.to_lsp()),
    })
}

fn parse_and_concat_documentation(item_documentation: Documentation) -> Option<String> {
    let mut comments: Vec<String> = Vec::new();
    match (
        item_documentation.prefix_comments,
        item_documentation.inner_comments,
        item_documentation.module_level_comments,
    ) {
        (None, None, None) => (),
        (prefix_comments, inner_comments, module_level_comments) => {
            for comment_content in chain!(prefix_comments, inner_comments, module_level_comments) {
                comments.push(comment_content.trim_end().to_string());
            }
        }
    }
    if comments.is_empty() {
        return None;
    }
    Some(comments.join(" "))
}
