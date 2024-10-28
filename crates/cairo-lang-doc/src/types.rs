use std::{fmt, iter};

use cairo_lang_defs::ids::{
    FileIndex, GenericTypeId, LookupItemId, ModuleFileId, ModuleId, ModuleItemId, TraitItemId,
};
use cairo_lang_diagnostics::DiagnosticsBuilder;
use cairo_lang_filesystem::ids::{FileKind, FileLongId, VirtualFile};
use cairo_lang_parser::parser::Parser;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::diagnostic::{NotFoundItemType, SemanticDiagnostics};
use cairo_lang_semantic::expr::inference::InferenceId;
use cairo_lang_semantic::items::functions::GenericFunctionId;
use cairo_lang_semantic::resolve::{AsSegments, ResolvedGenericItem, Resolver};
use cairo_lang_syntax::node::ast::{Expr, ExprPath, ItemModule};
use cairo_lang_syntax::node::helpers::GetIdentifier;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{SyntaxNode, TypedSyntaxNode};
use cairo_lang_utils::Intern;
use regex::{Captures, Regex};

use crate::db::DocGroup;
use crate::documentable_item::DocumentableItemId;

/// Token representing a link to another item inside the documentation.
#[derive(Debug, PartialEq, Clone, Eq)]
pub struct CommentLinkToken {
    /// A link part that's inside "[]" brackets.
    pub label: String,
    /// A link part that's inside "()" brackets, right after the label.
    pub path: Option<String>,
    /// Item resolved based on the path provided by user. If resolver cannot resolve the item, we
    /// leave it as None.
    pub resolved_item: Option<DocumentableItemId>,
}

/// Generic type for a comment token. It's either a plain content or a link.
/// Notice that the Content type of token can store much more than just one word.
#[derive(Debug, PartialEq, Clone, Eq)]
pub enum DocumentationCommentToken {
    /// Token with plain documentation content.
    Content(String),
    /// Link token.
    Link(CommentLinkToken),
}

/// Parses plain documentation comments into [DocumentationCommentToken]s.
pub struct DocumentationCommentParser<'a> {
    db: &'a dyn DocGroup,
}

impl<'a> DocumentationCommentParser<'a> {
    pub fn new(db: &'a dyn DocGroup) -> Self {
        Self { db }
    }

    /// Parses documentation comment content into vector of [DocumentationCommentToken]s, keeping
    /// the order in which they were present in the content.
    ///
    /// We look for 3 types of patterns when it comes to link (ignore the backslash): "\[label\](path)", "\[path\]" or
    /// "\[`path`\]".
    pub fn parse_documentation_comment(
        &self,
        item_id: DocumentableItemId,
        documentation_comment: String,
    ) -> Vec<DocumentationCommentToken> {
        let inline_link_pattern = Regex::new(r"\[(?<label>[^\[]*?)\]\((?<path>.*?)\)").unwrap();
        let implied_link_pattern = Regex::new(r"\[(?<path>.*?)\]").unwrap();

        let docs_no_inline_links: Vec<&str> =
            inline_link_pattern.split(&documentation_comment).collect();
        let inline_link_captures: Vec<Captures<'_>> =
            inline_link_pattern.captures_iter(&documentation_comment).collect();

        // Tokenize only plain text and inline link comments.
        let mut docs_and_inline_links_tokenized: Vec<DocumentationCommentToken> = Vec::default();

        for (i, content_part) in docs_no_inline_links.iter().enumerate() {
            if !content_part.is_empty() {
                docs_and_inline_links_tokenized
                    .push(DocumentationCommentToken::Content(content_part.to_string()));
            }

            if i < docs_no_inline_links.len() - 1 && i < inline_link_captures.len() {
                let matched_link: &Captures<'_> = inline_link_captures.get(i).unwrap();
                docs_and_inline_links_tokenized.push(DocumentationCommentToken::Link(
                    CommentLinkToken {
                        label: matched_link["label"].to_string(),
                        path: Some(matched_link["path"].to_string()),
                        resolved_item: self
                            .resolve_linked_item(item_id, matched_link["path"].to_string()),
                    },
                ))
            }
        }

        // Stores all plain text, inline and implied link comments.
        let mut result: Vec<DocumentationCommentToken> = Vec::default();

        for comment in docs_and_inline_links_tokenized.into_iter() {
            match comment {
                DocumentationCommentToken::Content(content) => {
                    let content_no_implied_links: Vec<&str> =
                        implied_link_pattern.split(&content).collect();
                    let implied_link_captures: Vec<Captures<'_>> =
                        implied_link_pattern.captures_iter(&content).collect();

                    for (i, content_part) in content_no_implied_links.iter().enumerate() {
                        if !content_part.is_empty() {
                            result
                                .push(DocumentationCommentToken::Content(content_part.to_string()));
                        }

                        if i < content_no_implied_links.len() - 1 && i < implied_link_captures.len()
                        {
                            let matched_link: &Captures<'_> = implied_link_captures.get(i).unwrap();
                            let path_raw = matched_link["path"].to_string();
                            let path = if path_raw.starts_with("`") && path_raw.ends_with("`") {
                                path_raw.trim_start_matches("`").trim_end_matches("`").to_string()
                            } else {
                                path_raw.clone()
                            };

                            result.push(DocumentationCommentToken::Link(CommentLinkToken {
                                label: path_raw,
                                path: None,
                                resolved_item: self.resolve_linked_item(item_id, path.clone()),
                            }))
                        }
                    }
                }
                link => result.push(link),
            }
        }

        result
    }

    /// Resolves item based on the provided path as a string.
    fn resolve_linked_item(
        &self,
        item_id: DocumentableItemId,
        path: String,
    ) -> Option<DocumentableItemId> {
        let syntax_node = item_id.stable_location(self.db.upcast())?.syntax_node(self.db.upcast());
        let containing_module = self.find_module_file_containing_node(&syntax_node)?;
        let mut resolver =
            Resolver::new(self.db.upcast(), containing_module, InferenceId::NoContext);
        let mut diagnostics = SemanticDiagnostics::default();
        let segments = self.prase_comment_link_path(path)?;
        resolver
            .resolve_generic_path(
                &mut diagnostics,
                segments.to_segments(self.db.upcast()),
                NotFoundItemType::Identifier,
                None,
            )
            .ok()?
            .to_documentable_item_id(self.db.upcast())
    }

    /// Parses the path as a string to an Path Expression, which can be later used by a resolver.
    fn prase_comment_link_path(&self, path: String) -> Option<ExprPath> {
        let virtual_file = FileLongId::Virtual(VirtualFile {
            parent: Default::default(),
            name: Default::default(),
            content: Default::default(),
            code_mappings: Default::default(),
            kind: FileKind::Module,
        })
        .intern(self.db);

        let expr = Parser::parse_file_expr(
            self.db.upcast(),
            &mut DiagnosticsBuilder::default(),
            virtual_file,
            &path,
        );

        if let Expr::Path(expr_path) = expr {
            Some(expr_path)
        } else {
            None
        }
    }

    /// Returns a [`ModuleFileId`] containing the node.
    ///
    /// If the node is located in a virtual file generated by a compiler plugin, this method will
    /// return a [`ModuleFileId`] pointing to the main, user-written file of the module.
    fn find_module_file_containing_node(&self, node: &SyntaxNode) -> Option<ModuleFileId> {
        let module_id = self.find_module_containing_node(node)?;
        let file_index = FileIndex(0);
        Some(ModuleFileId(module_id, file_index))
    }
    /// Finds a [`ModuleId`] containing the node.
    ///
    /// If the node is located in a virtual file generated by a compiler plugin, this method will
    /// return the (sub)module of the main, user-written file that leads to the node.
    fn find_module_containing_node(&self, node: &SyntaxNode) -> Option<ModuleId> {
        let db: &dyn SemanticGroup = self.db.upcast();
        let syntax_db = db.upcast();

        // Get the main module of the main file that leads to the node.
        // The node may be located in a virtual file of a submodule.
        // This code attempts to get the absolute "parent" of both "module" and "file" parts.
        let main_module = {
            // Get the file where the node is located.
            // This might be a virtual file generated by a compiler plugin.
            let node_file_id = node.stable_ptr().file_id(syntax_db);

            // Get the root module of a file containing the node.
            let node_main_module = db.file_modules(node_file_id).ok()?.iter().copied().next()?;

            // Get the main module of the file.
            let main_file = db.module_main_file(node_main_module).ok()?;

            // Get the main module of that file.
            db.file_modules(main_file).ok()?.iter().copied().next()?
        };

        // Get the stack (bottom-up) of submodule names in the file containing the node, in the main
        // module, that lead to the node.
        iter::successors(node.parent(), SyntaxNode::parent)
            .filter(|node| node.kind(syntax_db) == SyntaxKind::ItemModule)
            .map(|node| {
                ItemModule::from_syntax_node(syntax_db, node)
                    .stable_ptr()
                    .name_green(syntax_db)
                    .identifier(syntax_db)
            })
            // Buffer the stack to get DoubleEndedIterator.
            .collect::<Vec<_>>()
            .into_iter()
            // And get id of the (sub)module containing the node by traversing this stack top-down.
            .try_rfold(main_module, |module, name| {
                let ModuleItemId::Submodule(submodule) =
                    db.module_item_by_name(module, name).ok()??
                else {
                    return None;
                };
                Some(ModuleId::Submodule(submodule))
            })
    }
}

trait ToDocumentableItemId<T> {
    fn to_documentable_item_id(self, db: &dyn SemanticGroup) -> Option<DocumentableItemId>;
}

impl ToDocumentableItemId<DocumentableItemId> for ResolvedGenericItem {
    /// Converts the [ResolvedGenericItem] to [DocumentableItemId].
    /// As for now, returns None only for a common Variable, as those are not a supported
    /// documentable item.
    fn to_documentable_item_id(self, db: &dyn SemanticGroup) -> Option<DocumentableItemId> {
        match self {
            ResolvedGenericItem::GenericConstant(id) => Some(DocumentableItemId::LookupItem(
                LookupItemId::ModuleItem(ModuleItemId::Constant(id)),
            )),
            ResolvedGenericItem::GenericFunction(GenericFunctionId::Free(id)) => {
                Some(DocumentableItemId::LookupItem(LookupItemId::ModuleItem(
                    ModuleItemId::FreeFunction(id),
                )))
            }
            ResolvedGenericItem::GenericType(GenericTypeId::Struct(id)) => Some(
                DocumentableItemId::LookupItem(LookupItemId::ModuleItem(ModuleItemId::Struct(id))),
            ),
            ResolvedGenericItem::GenericType(GenericTypeId::Enum(id)) => Some(
                DocumentableItemId::LookupItem(LookupItemId::ModuleItem(ModuleItemId::Enum(id))),
            ),
            ResolvedGenericItem::GenericTypeAlias(id) => Some(DocumentableItemId::LookupItem(
                LookupItemId::ModuleItem(ModuleItemId::TypeAlias(id)),
            )),
            ResolvedGenericItem::GenericImplAlias(id) => Some(DocumentableItemId::LookupItem(
                LookupItemId::ModuleItem(ModuleItemId::ImplAlias(id)),
            )),
            ResolvedGenericItem::Trait(id) => Some(DocumentableItemId::LookupItem(
                LookupItemId::ModuleItem(ModuleItemId::Trait(id)),
            )),
            ResolvedGenericItem::Impl(id) => Some(DocumentableItemId::LookupItem(
                LookupItemId::ModuleItem(ModuleItemId::Impl(id)),
            )),
            ResolvedGenericItem::GenericType(GenericTypeId::Extern(id)) => {
                Some(DocumentableItemId::LookupItem(LookupItemId::ModuleItem(
                    ModuleItemId::ExternType(id),
                )))
            }
            ResolvedGenericItem::GenericFunction(GenericFunctionId::Extern(id)) => {
                Some(DocumentableItemId::LookupItem(LookupItemId::ModuleItem(
                    ModuleItemId::ExternFunction(id),
                )))
            }
            ResolvedGenericItem::Module(ModuleId::Submodule(id)) => {
                Some(DocumentableItemId::LookupItem(LookupItemId::ModuleItem(
                    ModuleItemId::Submodule(id),
                )))
            }
            ResolvedGenericItem::Module(ModuleId::CrateRoot(id)) => {
                Some(DocumentableItemId::Crate(id))
            }
            ResolvedGenericItem::Variant(variant) => Some(DocumentableItemId::Variant(variant.id)),
            ResolvedGenericItem::TraitFunction(id) => Some(DocumentableItemId::LookupItem(
                LookupItemId::TraitItem(TraitItemId::Function(id)),
            )),
            ResolvedGenericItem::GenericFunction(GenericFunctionId::Impl(generic_impl_func)) => {
                if let Some(impl_function) = generic_impl_func.impl_function(db).ok().flatten() {
                    Some(DocumentableItemId::LookupItem(LookupItemId::ImplItem(
                        cairo_lang_defs::ids::ImplItemId::Function(impl_function),
                    )))
                } else {
                    Some(DocumentableItemId::LookupItem(LookupItemId::TraitItem(
                        TraitItemId::Function(generic_impl_func.function),
                    )))
                }
            }
            ResolvedGenericItem::GenericFunction(GenericFunctionId::Trait(trait_func)) => {
                Some(DocumentableItemId::LookupItem(LookupItemId::TraitItem(
                    TraitItemId::Function(trait_func.trait_function(db)),
                )))
            }
            ResolvedGenericItem::Variable(_) => None,
        }
    }
}

impl fmt::Display for CommentLinkToken {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.path.clone() {
            Some(path) => write!(f, "[{}]({})", self.label, path),
            None => write!(f, "[{}]", self.label),
        }
    }
}

impl fmt::Display for DocumentationCommentToken {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DocumentationCommentToken::Content(ref content) => {
                write!(f, "{}", content)
            }
            DocumentationCommentToken::Link(ref link_token) => {
                write!(f, "{}", link_token)
            }
        }
    }
}
