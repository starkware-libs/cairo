use cairo_lang_diagnostics::{DiagnosticAdded, DiagnosticsBuilder, Maybe};
use cairo_lang_filesystem::ids::{FileKind, FileLongId, VirtualFile};
use cairo_lang_formatter::{FormatterConfig, get_formatted_file};
use cairo_lang_parser::db::ParserGroup;
use cairo_lang_parser::parser::Parser;
use cairo_lang_parser::utils::SimpleParserDatabase;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::green::GreenNodeDetails;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{SyntaxNode, TypedSyntaxNode};
use cairo_lang_utils::Intern;

use crate::documentable_item::DocumentableItemId;

/// A helper struct to map parts of item signature on respective documentable items.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LocationLink {
    /// Link's start offset in documentable item's signature.
    pub start: usize,
    /// Link's end offset in documentable item's signature.
    pub end: usize,
    /// Linked item identifier.
    pub item_id: DocumentableItemId,
}

/// Collects all [`cairo_lang_syntax::node::green::GreenNode`]s for a [`SyntaxNode`],
/// returns a vector of their [`SyntaxKind`] and text.
fn collect_green_nodes(
    db: &dyn SyntaxGroup,
    syntax_node: &SyntaxNode,
    green_nodes: &mut Vec<(SyntaxKind, String)>,
) -> Vec<(SyntaxKind, String)> {
    let green_node = syntax_node.green_node(db);

    match &green_node.details {
        GreenNodeDetails::Token(text) => green_nodes.push((green_node.kind, text.to_string())),
        GreenNodeDetails::Node { .. } => {
            let syntax_node_children = syntax_node.get_children(db);
            syntax_node_children.iter().for_each(|child| {
                collect_green_nodes(db, child, green_nodes);
            });
        }
    }
    green_nodes.to_owned()
}

/// Creates a virtual file for further signature syntax processing.
fn get_virtual_syntax_file_signature(
    signature: String,
) -> Maybe<(SimpleParserDatabase, SyntaxNode)> {
    let sig_db = SimpleParserDatabase::default();
    let virtual_file = FileLongId::Virtual(VirtualFile {
        parent: None,
        name: "string_to_format".into(),
        content: signature.clone().into(),
        code_mappings: [].into(),
        kind: FileKind::Module,
        original_item_removed: false,
    })
    .intern(&sig_db);

    let mut diagnostics_builder = DiagnosticsBuilder::default();
    let syntax_file =
        Parser::parse_file(&sig_db, &mut diagnostics_builder, virtual_file, signature.as_str())
            .as_syntax_node();

    let diagnostics = sig_db.file_syntax_diagnostics(virtual_file);
    // allow single "Missing token '{'..." error
    if diagnostics.0.error_count <= 1 { Ok((sig_db, syntax_file)) } else { Err(DiagnosticAdded) }
}

/// Calculates offsets for original and formatted signature,
/// thanks to that [`LocationLink`]s can be adjusted.
fn get_offsets(
    signature1: Vec<(SyntaxKind, String)>,
    signature2: Vec<(SyntaxKind, String)>,
) -> Vec<(usize, i32)> {
    let mut offset_vector: Vec<(usize, i32)> = Vec::new();
    let mut original_length_tracker = 0;
    let mut signature1_index = 0;
    let mut signature2_index = 0;

    while signature1_index < signature1.len() && signature2_index < signature2.len() {
        let (kind1, ref string1) = signature1[signature1_index];
        let (kind2, ref string2) = signature2[signature2_index];

        if kind1 == kind2 {
            signature1_index += 1;

            if string1.len() != string2.len() {
                offset_vector
                    .push((original_length_tracker, string2.len() as i32 - string1.len() as i32));
            }
            original_length_tracker += string1.len();
        } else {
            offset_vector.push((original_length_tracker, string2.len() as i32));
        }
        signature2_index += 1;
    }
    offset_vector
}

/// Adjusts [`LocationLink`]s based on differences created in signature syntax formatting.
fn move_location_links(
    mut location_links: Vec<LocationLink>,
    offset_vector: Vec<(usize, i32)>,
) -> Vec<LocationLink> {
    for link in &mut location_links {
        let mut new_start = link.start as i32;
        let mut new_end = link.end as i32;

        for (location, length) in offset_vector.iter() {
            if link.end < *location {
                break;
            }
            if link.start >= *location {
                new_start += length;
            }
            if link.end > *location {
                new_end += length;
            } else {
                break;
            }
        }
        link.start = new_start as usize;
        link.end = new_end as usize;
    }
    location_links
}

/// Performs set of actions to return formatted signature with [`LocationLink`]s adjusted.
pub fn format_signature(
    signature: String,
    location_links: Vec<LocationLink>,
) -> (String, Vec<LocationLink>) {
    match get_virtual_syntax_file_signature(signature.clone()) {
        Ok((simple_db, syntax_file)) => {
            let formatted_file =
                get_formatted_file(&simple_db, &syntax_file, FormatterConfig::default());

            if !location_links.is_empty() {
                match get_virtual_syntax_file_signature(formatted_file.clone()) {
                    Ok((simple_db_formatted, syntax_file_formatted)) => {
                        let nodes_original =
                            collect_green_nodes(&simple_db, &syntax_file, &mut Vec::new());
                        let nodes_formatted = collect_green_nodes(
                            &simple_db_formatted,
                            &syntax_file_formatted,
                            &mut Vec::new(),
                        );

                        let offsets = get_offsets(nodes_original, nodes_formatted);
                        (
                            formatted_file.trim_end().to_string(),
                            move_location_links(location_links, offsets),
                        )
                    }
                    Err(_) => (formatted_file.trim_end().to_string(), location_links),
                }
            } else {
                (formatted_file.trim_end().to_string(), location_links)
            }
        }
        Err(_) => (signature, location_links),
    }
}
