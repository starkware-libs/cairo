use std::ops::Not;

use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::{ImplItemId, LookupItemId, ModuleId, ModuleItemId, TraitItemId};
use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_filesystem::ids::{CrateId, FileId};
use cairo_lang_parser::utils::SimpleParserDatabase;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_syntax::node::SyntaxNode;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_utils::Upcast;
use itertools::{Itertools, chain, intersperse};

use crate::documentable_item::DocumentableItemId;
use crate::markdown::cleanup_doc_markdown;
use crate::parser::{DocumentationCommentParser, DocumentationCommentToken};

#[salsa::query_group(DocDatabase)]
pub trait DocGroup:
    Upcast<dyn DefsGroup>
    + Upcast<dyn SyntaxGroup>
    + Upcast<dyn FilesGroup>
    + Upcast<dyn SemanticGroup>
    + SyntaxGroup
    + FilesGroup
    + DefsGroup
{
    // TODO(mkaput): Support #[doc] attribute. This will be a bigger chunk of work because it would
    //   be the best to convert all /// comments to #[doc] attrs before processing items by plugins,
    //   so that plugins would get a nice and clean syntax of documentation to manipulate further.
    /// Gets the documentation of an item.
    fn get_item_documentation(&self, item_id: DocumentableItemId) -> Option<String>;

    /// Gets the documentation of a certain as a vector of continuous tokens.
    fn get_item_documentation_as_tokens(
        &self,
        item_id: DocumentableItemId,
    ) -> Option<Vec<DocumentationCommentToken>>;

    /// Gets the signature of an item (i.e., item without its body).
    fn get_item_signature(&self, item_id: DocumentableItemId) -> String;
}

fn get_item_documentation(db: &dyn DocGroup, item_id: DocumentableItemId) -> Option<String> {
    match item_id {
        DocumentableItemId::Crate(crate_id) => get_crate_root_module_documentation(db, crate_id),
        item_id => {
            let (outer_comments, inner_comments, module_level_comments) =
                get_item_documentation_content(db, item_id);
            match (module_level_comments, outer_comments, inner_comments) {
                (None, None, None) => None,
                (module_level_comments, outer_comments, inner_comments) => Some(
                    chain!(&module_level_comments, &outer_comments, &inner_comments)
                        .map(|comment| comment.trim_end())
                        .join(" "),
                ),
            }
        }
    }
}

fn get_item_signature(db: &dyn DocGroup, item_id: DocumentableItemId) -> String {
    if let DocumentableItemId::Crate(crate_id) = item_id {
        return format!("crate {}", crate_id.name(db.upcast()));
    }

    let syntax_node = item_id.stable_location(db.upcast()).unwrap().syntax_node(db.upcast());
    let definition = match syntax_node.green_node(db.upcast()).kind {
        SyntaxKind::ItemConstant | SyntaxKind::ItemTypeAlias | SyntaxKind::ItemImplAlias => {
            syntax_node.clone().get_text_without_all_comment_trivia(db.upcast())
        }
        SyntaxKind::FunctionWithBody
        | SyntaxKind::ItemExternFunction
        | SyntaxKind::TraitItemFunction => {
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
                                node.clone()
                                    .get_text_without_all_comment_trivia(db.upcast())
                                    .trim()
                                    .to_owned()
                                    + " "
                            } else {
                                node.clone()
                                    .get_text_without_all_comment_trivia(db.upcast())
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
        SyntaxKind::ItemEnum | SyntaxKind::ItemStruct => {
            let children = db.get_children(syntax_node);

            let item_content_children_without_trivia = db
                .get_children(children[6].clone())
                .iter()
                .map(|node| node.clone().get_text_without_all_comment_trivia(db.upcast()))
                .join("");

            let [attributes, visibility, keyword, name, generic_types, left_brace, _, right_brace] =
                &children
                    .iter()
                    .map(|node| node.clone().get_text_without_all_comment_trivia(db.upcast()))
                    .collect::<Vec<_>>()[..]
            else {
                return "".to_owned();
            };

            format!(
                "{}\n{} {} {}{} {}\n {}{}",
                attributes,
                visibility,
                keyword,
                name,
                generic_types,
                left_brace,
                item_content_children_without_trivia,
                right_brace
            )
        }
        SyntaxKind::ItemExternType => {
            let [attributes, visibility, extern_keyword, keyword, name, generic_types, _] = &db
                .get_children(syntax_node)
                .iter()
                .map(|node| node.clone().get_text_without_all_comment_trivia(db.upcast()))
                .collect::<Vec<_>>()[..]
            else {
                return "".to_owned();
            };
            format!(
                "{}\n{} {} {} {}{}",
                attributes, visibility, extern_keyword, keyword, name, generic_types
            )
        }
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
                            .get_text_without_all_comment_trivia(db.upcast())
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
        SyntaxKind::TraitItemConstant | SyntaxKind::TraitItemType => {
            let children = db.get_children(syntax_node.clone());
            let get_text =
                |node: &SyntaxNode| node.clone().get_text_without_all_comment_trivia(db.upcast());
            format!("{} {}", get_text(&children[1]), children[2..].iter().map(get_text).join(""))
        }
        SyntaxKind::Member => {
            let children_text = db
                .get_children(syntax_node)
                .iter()
                .map(|node| node.clone().get_text_without_all_comment_trivia(db.upcast()))
                .collect::<Vec<String>>();
            // Returning straight away as we don't want to format it.
            return children_text[1..].join("").trim().into();
        }
        SyntaxKind::Variant => syntax_node.get_text_without_all_comment_trivia(db.upcast()),
        _ => "".to_owned(),
    };
    fmt(definition)
}

fn get_item_documentation_as_tokens(
    db: &dyn DocGroup,
    item_id: DocumentableItemId,
) -> Option<Vec<DocumentationCommentToken>> {
    let (outer_comment, inner_comment, module_level_comment) = match item_id {
        DocumentableItemId::Crate(crate_id) => {
            (None, None, get_crate_root_module_documentation(db, crate_id))
        }
        item_id => get_item_documentation_content(db, item_id),
    };

    let doc_parser = DocumentationCommentParser::new(db.upcast());

    let mut outer_comment_tokens =
        outer_comment.map(|comment| doc_parser.parse_documentation_comment(item_id, comment));

    if let Some(outer_comment_tokens) = &mut outer_comment_tokens {
        trim_last_token(outer_comment_tokens);
    }

    let mut inner_comment_tokens =
        inner_comment.map(|comment| doc_parser.parse_documentation_comment(item_id, comment));

    if let Some(inner_comment_tokens) = &mut inner_comment_tokens {
        trim_last_token(inner_comment_tokens);
    }

    let mut module_level_comment_tokens = module_level_comment
        .map(|comment| doc_parser.parse_documentation_comment(item_id, comment));

    if let Some(module_level_comment_tokens) = &mut module_level_comment_tokens {
        trim_last_token(module_level_comment_tokens);
    }

    let separator_token = vec![DocumentationCommentToken::Content(" ".to_string())];

    let result: Vec<Vec<DocumentationCommentToken>> =
        [outer_comment_tokens, inner_comment_tokens, module_level_comment_tokens]
            .into_iter()
            .flatten()
            .collect();

    if result.is_empty() {
        return None;
    }

    Some(intersperse(result, separator_token).flatten().collect())
}

/// Get the raw documentation content from the item.
fn get_item_documentation_content(
    db: &dyn DocGroup,
    item_id: DocumentableItemId,
) -> (Option<String>, Option<String>, Option<String>) {
    // We check for different type of comments for the item. Even modules can have both
    // inner and module level comments.
    let outer_comments = extract_item_outer_documentation(db, item_id);
    // In case if item_id is a module, there are 2 possible cases:
    // 1. Inline module: It could have inner comments, but not the module_level.
    // 2. Non-inline Module (module as a file): It could have module level comments, but not the
    //    inner ones.
    let inner_comments = extract_item_inner_documentation(db, item_id);
    let module_level_comments = extract_item_module_level_documentation(db.upcast(), item_id);

    (outer_comments, inner_comments, module_level_comments)
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

/// Gets the crate level documentation.
fn get_crate_root_module_documentation(db: &dyn DocGroup, crate_id: CrateId) -> Option<String> {
    let module_file_id = db.module_main_file(ModuleId::CrateRoot(crate_id)).ok()?;
    extract_item_module_level_documentation_from_file(db, module_file_id)
}

/// Gets the "//!" inner comment of the item (if only item supports inner comments).
fn extract_item_inner_documentation(
    db: &dyn DocGroup,
    item_id: DocumentableItemId,
) -> Option<String> {
    if matches!(
        item_id,
        DocumentableItemId::LookupItem(
            LookupItemId::ModuleItem(ModuleItemId::FreeFunction(_) | ModuleItemId::Submodule(_))
                | LookupItemId::ImplItem(ImplItemId::Function(_))
                | LookupItemId::TraitItem(TraitItemId::Function(_))
        )
    ) {
        let raw_text = item_id
            .stable_location(db.upcast())?
            .syntax_node(db.upcast())
            .get_text_without_inner_commentable_children(db.upcast());
        extract_item_inner_documentation_from_raw_text(raw_text)
    } else {
        None
    }
}

/// Only gets the doc comments above the item.
fn extract_item_outer_documentation(
    db: &dyn DocGroup,
    item_id: DocumentableItemId,
) -> Option<String> {
    // Get the text of the item (trivia + definition)
    let raw_text =
        item_id.stable_location(db.upcast())?.syntax_node(db.upcast()).get_text(db.upcast());
    let lines = raw_text
        .lines()
        .filter(|line| !line.trim().is_empty())
        // Takes all the lines before the definition.
        // Anything other than doc comments will be filtered out later.
        .take_while_ref(|line| is_comment_line(line) || line.trim_start().starts_with("#"))
        .filter_map(|line| extract_comment_from_code_line(line, &["///"]))
        .collect::<Vec<_>>();

    let result = join_lines_of_comments(&lines);

    cleanup_doc(result)
}

/// Gets the module level comments of the item.
fn extract_item_module_level_documentation(
    db: &dyn DocGroup,
    item_id: DocumentableItemId,
) -> Option<String> {
    match item_id {
        DocumentableItemId::LookupItem(LookupItemId::ModuleItem(ModuleItemId::Submodule(
            submodule_id,
        ))) => {
            if db.is_submodule_inline(submodule_id).is_ok_and(|is_inline| is_inline) {
                return None;
            }
            let module_file_id = db.module_main_file(ModuleId::Submodule(submodule_id)).ok()?;
            extract_item_module_level_documentation_from_file(db, module_file_id)
        }
        _ => None,
    }
}

/// Only gets the comments inside the item.
fn extract_item_inner_documentation_from_raw_text(raw_text: String) -> Option<String> {
    let lines = raw_text
        .lines()
        .filter(|line| !line.trim().is_empty())
        .skip_while(|line| is_comment_line(line))
        .filter_map(|line| extract_comment_from_code_line(line, &["//!"]))
        .collect::<Vec<_>>();

    let result = join_lines_of_comments(&lines);

    cleanup_doc(result)
}

/// Formats markdown part of the documentation, and returns None, if the final documentation is
/// empty or contains only whitespaces.
fn cleanup_doc(doc: String) -> Option<String> {
    let doc = cleanup_doc_markdown(doc);

    // Nullify empty or just-whitespace documentation strings as they are not useful.
    doc.trim().is_empty().not().then_some(doc)
}

/// Gets the module level comments of certain file.
fn extract_item_module_level_documentation_from_file(
    db: &dyn DocGroup,
    file_id: FileId,
) -> Option<String> {
    let file_content = db.file_content(file_id)?.to_string();

    let lines = file_content
        .lines()
        .filter(|line| !line.trim().is_empty())
        .take_while_ref(|line| is_comment_line(line))
        .filter_map(|line| extract_comment_from_code_line(line, &["//!"]))
        .collect::<Vec<_>>();

    let result = join_lines_of_comments(&lines);
    cleanup_doc(result)
}

/// This function does 2 things to the line of comment:
/// 1. Removes indentation
/// 2. If it starts with one of the passed prefixes, removes the given prefixes (including the space
///    after the prefix).
/// 3. If the comment starts with a slash, returns None.
fn extract_comment_from_code_line(line: &str, comment_markers: &[&'static str]) -> Option<String> {
    // Remove indentation.
    let dedent = line.trim_start();
    // Check if this is a doc comment.
    for comment_marker in comment_markers {
        if let Some(content) = dedent.strip_prefix(*comment_marker) {
            // TODO(mkaput): The way how removing this indentation is performed is probably
            //   wrong. The code should probably learn how many spaces are used at the first
            //   line of comments block, and then remove the same amount of spaces in the
            //   block, instead of assuming just one space.
            // Remove inner indentation if one exists.
            if content.starts_with('/') {
                return None;
            }
            return Some(content.strip_prefix(' ').unwrap_or(content).to_string());
        }
    }
    None
}

/// Check whether the code line is a comment line.
fn is_comment_line(line: &str) -> bool {
    line.trim_start().starts_with("//")
}

/// Parses the lines of extracted comments so it can be displayed.
/// It also takes note for Fenced and Indented code blocks, and doesn't trim them.
fn join_lines_of_comments(lines: &Vec<String>) -> String {
    let mut in_code_block = false;
    let mut result = String::new();

    for line in lines {
        let trimmed_line = line.trim_start();
        // 4 spaces or a tab.
        let is_indented_code_line =
            (line.starts_with("    ") || line.starts_with("\t")) && !in_code_block;
        let contains_delimiter = trimmed_line.starts_with("```") || is_indented_code_line;

        if contains_delimiter {
            // If we stumble upon the opening of a code block, we have to make a newline.
            if !in_code_block && !result.ends_with('\n') {
                result.push('\n');
            }
            in_code_block = !in_code_block;

            result.push_str(line);
            result.push('\n');
            continue;
        }

        if in_code_block {
            result.push_str(line);
            result.push('\n');
        } else {
            // Outside code blocks, handle paragraph breaks identified by empty lines.
            if trimmed_line.is_empty() {
                result.push_str("\n\n");
            } else {
                if !result.is_empty() && !result.ends_with("\n\n") && !result.ends_with('\n') {
                    result.push(' ');
                }
                result.push_str(trimmed_line);
            }
        }
    }
    result.trim_end().to_string()
}

fn trim_last_token(tokens: &mut [DocumentationCommentToken]) {
    if let Some(DocumentationCommentToken::Content(token)) = tokens.last_mut() {
        *token = token.trim_end().to_string();
    }
}
