use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_filesystem::ids::FileId;
use cairo_lang_syntax::node::ast::TerminalIdentifier;
use cairo_lang_syntax::node::TypedSyntaxNode;
use cairo_lang_utils::Upcast;
use tower_lsp::lsp_types::Hover;

use crate::ide::hover::markdown_contents;
use crate::lang::inspect::defs::SymbolDef;
use crate::lang::lsp::ToLsp;
use crate::markdown::Markdown;

/// Get declaration and documentation "definition" of an item referred by the given identifier.
#[tracing::instrument(level = "trace", skip_all)]
pub fn definition(
    db: &RootDatabase,
    identifier: &TerminalIdentifier,
    file_id: FileId,
) -> Option<Hover> {
    let symbol = SymbolDef::find(db, identifier)?;

    let md = match &symbol {
        SymbolDef::Item(item) => {
            // TODO(mkaput): Format this with Cairo formatter.
            let mut md = Markdown::fenced_code_block(&item.signature(db));
            if let Some(doc) = item.documentation(db) {
                md += Markdown::rule();
                md += doc;
            }
            md
        }

        SymbolDef::Variable(var) => Markdown::fenced_code_block(&var.signature(db)),
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
