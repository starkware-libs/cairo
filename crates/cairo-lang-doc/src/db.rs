use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::{ImplItemId, LookupItemId, ModuleId, ModuleItemId, TraitItemId};
use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_filesystem::ids::{CrateId, FileId};
use cairo_lang_parser::utils::SimpleParserDatabase;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_utils::Upcast;
use itertools::Itertools;
use serde::Serialize;

use crate::documentable_item::{DocumentableItemId, DocumentableModuleItemId};
use crate::markdown::cleanup_doc_markdown;

#[derive(PartialEq, Eq, Debug, Clone, Serialize)]
pub struct Documentation {
    pub prefix_comments: Option<String>,
    pub inner_comments: Option<String>,
    pub module_level_comments: Option<String>,
}

#[salsa::query_group(DocDatabase)]
pub trait DocGroup:
    Upcast<dyn DefsGroup>
    + Upcast<dyn SyntaxGroup>
    + Upcast<dyn FilesGroup>
    + SyntaxGroup
    + FilesGroup
    + DefsGroup
{
    // TODO(mkaput): Add tests.
    // TODO(mkaput): Support #[doc] attribute. This will be a bigger chunk of work because it would
    //   be the best to convert all /// comments to #[doc] attrs before processing items by plugins,
    //   so that plugins would get a nice and clean syntax of documentation to manipulate further.
    /// Gets the documentation above an item definition.
    fn get_item_documentation(&self, item_id: DocumentableItemId) -> Documentation;

    // TODO(mkaput): Add tests.
    /// Gets the signature of an item (i.e., item without its body).
    fn get_item_signature(&self, item_id: DocumentableModuleItemId) -> String;
}

fn get_item_documentation(db: &dyn DocGroup, item_id: DocumentableItemId) -> Documentation {
    match item_id {
        DocumentableItemId::Item(item_id) => {
            let prefix_comments = extract_prefixed_comments_from_raw_text(db, item_id);
            let inner_comments = get_item_inner_documentation(db, item_id);
            let module_level_comments = extract_module_level_comments(db.upcast(), item_id);
            Documentation { prefix_comments, inner_comments, module_level_comments }
        }
        DocumentableItemId::Crate(crate_id) => Documentation {
            prefix_comments: None,
            inner_comments: None,
            module_level_comments: get_crate_documentation(db, crate_id),
        },
    }
}

// Gets the crate level comments
fn get_crate_documentation(db: &dyn DocGroup, crate_id: CrateId) -> Option<String> {
    let root_crate_file_id = db.module_main_file(ModuleId::CrateRoot(crate_id));
    match root_crate_file_id {
        Ok(module_file_id) => extract_module_level_commets_from_file(db, module_file_id),
        Err(_) => None,
    }
}

// Gets the "//!" inner comment of the item (if only item supports inner comments).
fn get_item_inner_documentation(
    db: &dyn DocGroup,
    item_id: DocumentableModuleItemId,
) -> Option<String> {
    match item_id {
        DocumentableModuleItemId::LookupItem(
            LookupItemId::ModuleItem(ModuleItemId::FreeFunction(_))
            | LookupItemId::ModuleItem(ModuleItemId::Submodule(_))
            | LookupItemId::ImplItem(ImplItemId::Function(_))
            | LookupItemId::TraitItem(TraitItemId::Function(_)),
        ) => {
            let raw_text = item_id
                .stable_location(db.upcast())
                .syntax_node(db.upcast())
                .get_shallow_inner_comments_text(db.upcast());
            extract_inner_comments_from_raw_text(raw_text, &["//!"])
        }
        _ => None,
    }
}

fn get_item_signature(db: &dyn DocGroup, item_id: DocumentableModuleItemId) -> String {
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

// Only gets the doc comments above the item.
fn extract_prefixed_comments_from_raw_text(
    db: &dyn DocGroup,
    item_id: DocumentableModuleItemId,
) -> Option<String> {
    // Get the text of the item (trivia + definition)
    let raw_text =
        item_id.stable_location(db.upcast()).syntax_node(db.upcast()).get_text(db.upcast());
    let doc = raw_text
        .lines()
        .take_while_ref(|line| {
            !line.trim_start().chars().next().map_or(false, |c| c.is_alphabetic())
        })
        .filter_map(|line| map_raw_text_line_to_comment(line, &["///", "//!"]))
        .join("\n");

    // Cleanup the markdown.
    let doc = cleanup_doc_markdown(doc);

    // Nullify empty or just-whitespace documentation strings as they are not useful.
    (!doc.trim().is_empty()).then_some(doc)
}

// Only gets the comments inside the item.
fn extract_inner_comments_from_raw_text(
    raw_text: String,
    prefixes: &[&'static str],
) -> Option<String> {
    let doc = raw_text
        .lines()
        .skip_while(|line| !line.trim_start().chars().next().map_or(false, |c| c.is_alphabetic()))
        .filter_map(|line| map_raw_text_line_to_comment(line, prefixes))
        .join("\n");

    // Cleanup the markdown.
    let doc = cleanup_doc_markdown(doc);

    // Nullify empty or just-whitespace documentation strings as they are not useful.
    (!doc.trim().is_empty()).then_some(doc)
}

// Gets the module level comments of certain file.
fn extract_module_level_commets_from_file(db: &dyn DocGroup, file_id: FileId) -> Option<String> {
    let file_content = db.file_content(file_id)?.to_string();

    let doc = file_content
        .lines()
        .take_while_ref(|line| {
            !line.trim_start().chars().next().map_or(false, |c| c.is_alphabetic())
                || line.trim().is_empty()
        })
        .filter_map(|line| {
            if line.is_empty() {
                return None;
            }
            map_raw_text_line_to_comment(line, &["//!"])
        })
        .join("\n");

    let doc = cleanup_doc_markdown(doc);

    Some(doc)
}

// Gets the module level comments of the item.
fn extract_module_level_comments(
    db: &dyn DocGroup,
    item_id: DocumentableModuleItemId,
) -> Option<String> {
    match item_id {
        DocumentableModuleItemId::LookupItem(LookupItemId::ModuleItem(
            ModuleItemId::Submodule(submodule_id),
        )) => {
            if db.is_submodule_inline(submodule_id).is_ok_and(|is_inline| is_inline) {
                return None;
            }
            let module_file_id = db.module_main_file(ModuleId::Submodule(submodule_id));
            match module_file_id {
                Ok(module_file_id) => extract_module_level_commets_from_file(db, module_file_id),
                Err(_) => None,
            }
        }
        _ => None,
    }
}

fn map_raw_text_line_to_comment(line: &str, prefixes: &[&'static str]) -> Option<String> {
    // Remove indentation.
    let dedent = line.trim_start();
    // Check if this is a doc comment.
    for prefix in prefixes {
        if let Some(content) = dedent.strip_prefix(*prefix) {
            // TODO(mkaput): The way how removing this indentation is performed is probably
            //   wrong. The code should probably learn how many spaces are used at the first
            //   line of comments block, and then remove the same amount of spaces in the
            //   block, instead of assuming just one space.
            // Remove inner indentation if one exists.
            return Some(content.strip_prefix(' ').unwrap_or(content).to_string());
        }
    }
    None
}
