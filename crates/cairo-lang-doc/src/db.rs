use std::fmt::Write;

use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::{ImplItemId, LookupItemId, ModuleId, ModuleItemId, TraitItemId};
use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_filesystem::ids::{CrateId, FileId, Tracked};
use itertools::{Itertools, intersperse};
use salsa::Database;

use crate::documentable_item::DocumentableItemId;
use crate::location_links::LocationLink;
use crate::parser::{DocumentationCommentParser, DocumentationCommentToken};

pub trait DocGroup: Database {
    // TODO(mkaput): Support #[doc] attribute. This will be a bigger chunk of work because it would
    //   be the best to convert all /// comments to #[doc] attrs before processing items by plugins,
    //   so that plugins would get a nice and clean syntax of documentation to manipulate further.
    /// Gets the documentation of an item.
    fn get_item_documentation<'db>(&'db self, item_id: DocumentableItemId<'db>) -> Option<String> {
        get_item_documentation(self.as_dyn_database(), (), item_id)
    }

    /// Gets the documentation of a certain as a vector of continuous tokens.
    fn get_item_documentation_as_tokens<'db>(
        &'db self,
        item_id: DocumentableItemId<'db>,
    ) -> Option<Vec<DocumentationCommentToken<'db>>> {
        get_item_documentation_as_tokens(self.as_dyn_database(), (), item_id)
    }

    /// Gets the signature of an item (i.e., item without its body).
    fn get_item_signature<'db>(&'db self, item_id: DocumentableItemId<'db>) -> Option<String> {
        self.get_item_signature_with_links(item_id).0
    }

    /// Gets the signature of an item and a list of [`LocationLink`]s to enable mapping
    /// signature slices on documentable items.
    fn get_item_signature_with_links<'db>(
        &'db self,
        item_id: DocumentableItemId<'db>,
    ) -> (Option<String>, Vec<LocationLink<'db>>) {
        crate::documentable_formatter::get_item_signature_with_links(
            self.as_dyn_database(),
            (),
            item_id,
        )
    }
}
impl<T: Database + ?Sized> DocGroup for T {}

#[salsa::tracked]
fn get_item_documentation<'db>(
    db: &'db dyn Database,
    _tracked: Tracked,
    item_id: DocumentableItemId<'db>,
) -> Option<String> {
    let tokens = db.get_item_documentation_as_tokens(item_id)?;
    let mut buff = String::new();
    for doc_token in &tokens {
        match doc_token {
            DocumentationCommentToken::Content(content) => buff.push_str(content.as_str()),
            DocumentationCommentToken::Link(link) => {
                write!(&mut buff, "[{}]", link.label).ok()?;
                if let Some(path) = &link.path {
                    write!(&mut buff, "({path})").ok()?;
                }
            }
        }
    }
    Some(buff)
}

#[salsa::tracked]
fn get_item_documentation_as_tokens<'db>(
    db: &'db dyn Database,
    _tracked: Tracked,
    item_id: DocumentableItemId<'db>,
) -> Option<Vec<DocumentationCommentToken<'db>>> {
    let (outer_comment, inner_comment, module_level_comment) = match item_id {
        DocumentableItemId::Crate(crate_id) => {
            (None, None, get_crate_root_module_documentation(db, crate_id))
        }
        item_id => (
            // We check for different types of comments for the item. Even modules can have both
            // inner and module level comments.
            extract_item_outer_documentation(db, item_id),
            // In case if item_id is a module, there are 2 possible cases:
            // 1. Inline module: It could have inner comments, but not the module_level.
            // 2. Non-inline Module (module as a file): It could have module level comments, but
            //    not the inner ones.
            extract_item_inner_documentation(db, item_id),
            extract_item_module_level_documentation(db, item_id),
        ),
    };

    let doc_parser: DocumentationCommentParser<'db> = DocumentationCommentParser::new(db);

    let outer_comment_tokens =
        outer_comment.map(|comment| doc_parser.parse_documentation_comment(item_id, comment));
    let inner_comment_tokens =
        inner_comment.map(|comment| doc_parser.parse_documentation_comment(item_id, comment));
    let module_level_comment_tokens = module_level_comment
        .map(|comment| doc_parser.parse_documentation_comment(item_id, comment));

    let mut result: Vec<Vec<DocumentationCommentToken<'db>>> =
        [module_level_comment_tokens, outer_comment_tokens, inner_comment_tokens]
            .into_iter()
            .flatten()
            .collect();
    result.retain(|v| !v.is_empty());
    if result.is_empty() {
        return None;
    }
    let separator_token = vec![DocumentationCommentToken::Content(" ".to_string())];
    Some(intersperse(result, separator_token).flatten().collect())
}

/// Gets the crate level documentation.
fn get_crate_root_module_documentation<'db>(
    db: &'db dyn Database,
    crate_id: CrateId<'db>,
) -> Option<String> {
    let module_id = db.module_main_file(ModuleId::CrateRoot(crate_id)).ok()?;
    extract_item_module_level_documentation_from_file(db, module_id)
}

/// Gets the "//!" inner comment of the item (if only item supports inner comments).
fn extract_item_inner_documentation<'db>(
    db: &'db dyn Database,
    item_id: DocumentableItemId<'db>,
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
            .stable_location(db)?
            .syntax_node(db)
            .get_text_without_inner_commentable_children(db);
        Some(extract_item_inner_documentation_from_raw_text(raw_text))
    } else {
        None
    }
}

/// Only gets the doc comments above the item.
fn extract_item_outer_documentation<'db>(
    db: &'db dyn Database,
    item_id: DocumentableItemId<'db>,
) -> Option<String> {
    // Get the text of the item (trivia + definition)
    let raw_text = item_id.stable_location(db)?.syntax_node(db).get_text(db);
    Some(
        raw_text
        .lines()
        .filter(|line| !line.trim().is_empty())
        // Takes all the lines before the definition.
        // Anything other than doc comments will be filtered out later.
        .take_while_ref(|line| is_comment_line(line) || line.trim_start().starts_with("#"))
        .filter_map(|line| extract_comment_from_code_line(line, &["///"]))
        .join("\n"),
    )
}

/// Gets the module level comments of the item.
fn extract_item_module_level_documentation<'db>(
    db: &'db dyn Database,
    item_id: DocumentableItemId<'db>,
) -> Option<String> {
    match item_id {
        DocumentableItemId::LookupItem(LookupItemId::ModuleItem(ModuleItemId::Submodule(
            submodule_id,
        ))) => {
            if db.is_submodule_inline(submodule_id) {
                return None;
            }
            let module_id = db.module_main_file(ModuleId::Submodule(submodule_id)).ok()?;
            extract_item_module_level_documentation_from_file(db, module_id)
        }
        _ => None,
    }
}

/// Only gets the comments inside the item.
fn extract_item_inner_documentation_from_raw_text(raw_text: String) -> String {
    raw_text
        .lines()
        .filter(|line| !line.trim().is_empty())
        .skip_while(|line| is_comment_line(line))
        .filter_map(|line| extract_comment_from_code_line(line, &["//!"]))
        .join("\n")
}

/// Gets the module level comments of certain file.
fn extract_item_module_level_documentation_from_file<'db>(
    db: &'db dyn Database,
    file_id: FileId<'db>,
) -> Option<String> {
    let file_content = db.file_content(file_id)?.to_string();
    Some(
        file_content
            .lines()
            .filter(|line| !line.trim().is_empty())
            .take_while_ref(|line| is_comment_line(line))
            .filter_map(|line| extract_comment_from_code_line(line, &["//!"]))
            .join("\n"),
    )
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
