use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::LookupItemId;
use cairo_lang_utils::Upcast;
use tower_lsp::lsp_types::{Hover, HoverContents, HoverParams, MarkedString};

use crate::lang::lsp::{LsProtoGroup, ToLsp};
use crate::{get_definition_location, get_node_and_lookup_items};

/// Get hover information at a given text document position.
#[tracing::instrument(
    level = "debug",
    skip_all,
    fields(uri = %params.text_document_position_params.text_document.uri)
)]
pub fn hover(params: HoverParams, db: &RootDatabase) -> Option<Hover> {
    let file_id = db.file_for_url(&params.text_document_position_params.text_document.uri);
    let position = params.text_document_position_params.position;
    // Get the item id of the definition.
    let (found_file, span) = get_definition_location(db, file_id, position)?;
    // Get the documentation and declaration of the item.
    let (_, lookup_items) = get_node_and_lookup_items(
        db,
        found_file,
        span.start.position_in_file(db.upcast(), found_file)?.to_lsp(),
    )?;
    // Build texts.
    let mut hints = Vec::new();
    if let Some(hint) = get_expr_hint(db.upcast(), lookup_items.into_iter().next()?) {
        hints.extend(hint);
    };

    Some(Hover { contents: HoverContents::Array(hints), range: None })
}

/// If the node is an expression, retrieves a hover hint for it.
#[tracing::instrument(level = "trace", skip_all)]
fn get_expr_hint(db: &dyn DefsGroup, lookup_item_id: LookupItemId) -> Option<Vec<MarkedString>> {
    let mut hints = vec![];
    let definition = db.get_item_definition(lookup_item_id);
    hints.push(MarkedString::from_language_code("cairo".to_owned(), definition));
    let documentation = db.get_item_documentation(lookup_item_id).unwrap_or_default();
    // Add a separator.
    if !documentation.is_empty() {
        hints.push(MarkedString::String("\n---\n".to_string()));
    }
    let mut doc = "".to_string();
    let mut is_cairo_string = false;
    for line in documentation.lines() {
        if line.starts_with("```") {
            // If is_cairo_string is true, it means that we end the code block so append the code
            // in a cairo formatted string.
            hints.push(if is_cairo_string {
                MarkedString::from_language_code("cairo".to_owned(), doc)
            } else {
                // else append a regular string.
                MarkedString::from_markdown(doc)
            });
            // Switch the variable to properly format the following block.
            is_cairo_string = !is_cairo_string;
            // reset the string to start the next block.
            doc = "".to_string();
            continue;
        }
        // Append the current block string.
        doc += if let Some(stripped) = line.strip_prefix("///") {
            stripped
        } else if let Some(stripped) = line.strip_prefix("//!") {
            stripped
        } else {
            line
        };
        // add new line to have a proper formatting.
        doc += "\n";
    }
    hints.push(MarkedString::from_markdown(doc));

    Some(hints)
}
