use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_defs::db::DefsGroup;
use cairo_lang_filesystem::ids::FileId;
use cairo_lang_syntax::node::ast::TerminalIdentifier;
use cairo_lang_syntax::node::TypedSyntaxNode;
use cairo_lang_utils::Upcast;
use tower_lsp::lsp_types::Hover;

use crate::find_definition;
use crate::ide::hover::markdown_contents;
use crate::lang::db::LsSemanticGroup;
use crate::lang::lsp::ToLsp;
use crate::markdown::Markdown;

/// Get declaration and documentation "definition" of an item referred by the given identifier.
#[tracing::instrument(level = "trace", skip_all)]
pub fn definition(
    db: &RootDatabase,
    identifier: &TerminalIdentifier,
    file_id: FileId,
) -> Option<Hover> {
    // Get the syntax node of the definition.
    let definition_node = {
        let lookup_items = db.collect_lookup_items_stack(&identifier.as_syntax_node())?;
        let stable_ptr = find_definition(db, identifier, &lookup_items)?;
        stable_ptr.lookup(db.upcast())
    };
    // Get the lookup item representing the defining item.
    let lookup_item_id = db.find_lookup_item(&definition_node)?;

    let mut md = Markdown::empty();

    let signature = db.get_item_signature(lookup_item_id);
    // TODO(mkaput): Format this with Cairo formatter.
    md += Markdown::fenced_code_block(&signature);

    let documentation = db.get_item_documentation(lookup_item_id).unwrap_or_default();

    if !documentation.is_empty() {
        md += Markdown::rule();

        let mut doc = Markdown::from(documentation);
        doc.convert_fenced_code_blocks_to_cairo();
        doc.ensure_trailing_newline();
        md += doc;
    }

    Some(Hover {
        contents: markdown_contents(md),
        range: identifier
            .as_syntax_node()
            .span_without_trivia(db.upcast())
            .position_in_file(db.upcast(), file_id)
            .map(|p| p.to_lsp()),
    })
}
