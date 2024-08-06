use cairo_lang_defs::db::DefsGroup;
use cairo_lang_parser::utils::SimpleParserDatabase;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_utils::Upcast;
use itertools::Itertools;

use crate::documentable_item::DocumentableItemId;
use crate::markdown::cleanup_doc_markdown;

#[salsa::query_group(DocDatabase)]
pub trait DocGroup: Upcast<dyn DefsGroup> + Upcast<dyn SyntaxGroup> + SyntaxGroup {
    // TODO(mkaput): Add tests.
    // TODO(mkaput): Support #[doc] attribute. This will be a bigger chunk of work because it would
    //   be the best to convert all /// comments to #[doc] attrs before processing items by plugins,
    //   so that plugins would get a nice and clean syntax of documentation to manipulate further.
    /// Gets the documentation above an item definition.
    fn get_item_documentation(&self, item_id: DocumentableItemId) -> Option<String>;

    // TODO(mkaput): Add tests.
    /// Gets the signature of an item (i.e., item without its body).
    fn get_item_signature(&self, item_id: DocumentableItemId) -> String;
}

fn get_item_documentation(db: &dyn DocGroup, item_id: DocumentableItemId) -> Option<String> {
    // Get the text of the item (trivia + definition)
    let doc = item_id.stable_location(db.upcast()).syntax_node(db.upcast()).get_text(db.upcast());

    // Only get the doc comments (start with `///` or `//!`) above the function.
    let doc = doc
        .lines()
        .take_while_ref(|line| {
            !line.trim_start().chars().next().map_or(false, |c| c.is_alphabetic())
        })
        .filter_map(|line| {
            // Remove indentation.
            let dedent = line.trim_start();
            // Check if this is a doc comment.
            for prefix in ["///", "//!"] {
                if let Some(content) = dedent.strip_prefix(prefix) {
                    // TODO(mkaput): The way how removing this indentation is performed is probably
                    //   wrong. The code should probably learn how many spaces are used at the first
                    //   line of comments block, and then remove the same amount of spaces in the
                    //   block, instead of assuming just one space.
                    // Remove inner indentation if one exists.
                    return Some(content.strip_prefix(' ').unwrap_or(content));
                }
            }
            None
        })
        .join("\n");

    // Cleanup the markdown.
    let doc = cleanup_doc_markdown(doc);

    // Nullify empty or just-whitespace documentation strings as they are not useful.
    (!doc.trim().is_empty()).then_some(doc)
}

fn get_item_signature(db: &dyn DocGroup, item_id: DocumentableItemId) -> String {
    let syntax_node = item_id.stable_location(db.upcast()).syntax_node(db.upcast());
    let definition = match syntax_node.green_node(db.upcast()).kind {
        SyntaxKind::ItemConstant
        | SyntaxKind::TraitItemFunction
        | SyntaxKind::ItemTypeAlias
        | SyntaxKind::ItemImplAlias => syntax_node.clone().get_text_without_trivia(db.upcast()),
        SyntaxKind::FunctionWithBody | SyntaxKind::ItemExternFunction => {
            let children = db.get_children(syntax_node);
            children[1..]
                .iter()
                .map_while(|node| {
                    let kind = node.kind(db.upcast());
                    (kind != SyntaxKind::ExprBlock
                        && kind != SyntaxKind::ImplBody
                        && kind != SyntaxKind::TraitBody)
                        .then_some(
                            if kind == SyntaxKind::VisibilityPub
                                || kind == SyntaxKind::TerminalExtern
                            {
                                node.clone().get_text_without_trivia(db.upcast()).trim().to_owned()
                                    + " "
                            } else {
                                node.clone()
                                    .get_text_without_trivia(db.upcast())
                                    .lines()
                                    .map(|line| line.trim())
                                    .collect::<Vec<&str>>()
                                    .join("")
                            },
                        )
                })
                .collect::<Vec<String>>()
                .join("")
        }
        SyntaxKind::ItemEnum | SyntaxKind::ItemExternType | SyntaxKind::ItemStruct => db
            .get_children(syntax_node)
            .iter()
            .skip(1)
            .map(|node| node.clone().get_text(db.upcast()))
            .collect::<Vec<String>>()
            .join(""),
        SyntaxKind::ItemTrait | SyntaxKind::ItemImpl => {
            let children = db.get_children(syntax_node);
            children[1..]
                .iter()
                .enumerate()
                .map_while(|(index, node)| {
                    let kind = node.kind(db.upcast());
                    if kind != SyntaxKind::ImplBody && kind != SyntaxKind::TraitBody {
                        let text = node
                            .clone()
                            .get_text_without_trivia(db.upcast())
                            .lines()
                            .map(|line| line.trim())
                            .collect::<Vec<&str>>()
                            .join("");

                        Some(if index == 0 || kind == SyntaxKind::WrappedGenericParamList {
                            text
                        } else {
                            " ".to_owned() + &text
                        })
                    } else {
                        None
                    }
                })
                .collect::<Vec<String>>()
                .join("")
        }
        _ => "".to_owned(),
    };
    fmt(definition)
}

/// Run Cairo formatter over code with extra post-processing that is specific to signatures.
fn fmt(code: String) -> String {
    let code = cairo_lang_formatter::format_string(&SimpleParserDatabase::default(), code);

    code
        // Trim any whitespace that formatter tends to leave.
        .trim_end()
        // Trim trailing semicolons, that are present in trait/impl functions, constants, etc.
        // and that formatter tends to put in separate line.
        .trim_end_matches("\n;")
        .to_owned()
}
