use std::sync::Arc;

use cairo_lang_defs::ids::{LanguageElementId, LookupItemId, ModuleItemId, UseId};
use cairo_lang_diagnostics::{Diagnostics, Maybe, ToMaybe};
use cairo_lang_proc_macros::DebugWithDb;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{ast, TypedSyntaxNode};
use cairo_lang_utils::Upcast;

use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnosticKind::*;
use crate::diagnostic::{NotFoundItemType, SemanticDiagnostics};
use crate::expr::inference::InferenceId;
use crate::resolve::{ResolvedGenericItem, Resolver, ResolverData};
use crate::SemanticDiagnostic;

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct UseData {
    diagnostics: Diagnostics<SemanticDiagnostic>,
    resolved_item: Maybe<ResolvedGenericItem>,
    resolver_data: Arc<ResolverData>,
}

/// Query implementation of [crate::db::SemanticGroup::priv_use_semantic_data].
pub fn priv_use_semantic_data(db: &dyn SemanticGroup, use_id: UseId) -> Maybe<UseData> {
    let module_file_id = use_id.module_file_id(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_file_id.file_id(db.upcast())?);
    // TODO(spapini): Add generic args when they are supported on structs.
    let inference_id =
        InferenceId::LookupItemDeclaration(LookupItemId::ModuleItem(ModuleItemId::Use(use_id)));
    let mut resolver = Resolver::new(db, module_file_id, inference_id);
    // TODO(spapini): when code changes in a file, all the AST items change (as they contain a path
    // to the green root that changes. Once ASTs are rooted on items, use a selector that picks only
    // the item instead of all the module data.
    let use_ast = db.module_use_by_id(use_id)?.to_maybe()?;
    let mut segments = vec![];
    get_use_segments(db.upcast(), &ast::UsePath::Leaf(use_ast.clone()), &mut segments)?;
    let resolved_item =
        resolver.resolve_generic_path(&mut diagnostics, segments, NotFoundItemType::Identifier);
    let resolver_data = Arc::new(resolver.data);

    Ok(UseData { diagnostics: diagnostics.build(), resolved_item, resolver_data })
}

/// Returns the segments of a use path.
pub fn get_use_segments(
    db: &dyn SyntaxGroup,
    use_path: &ast::UsePath,
    segments: &mut Vec<ast::PathSegment>,
) -> Maybe<()> {
    // Add parent's segments.
    if let Some(parent_use_path) = get_parent_use_path(db, use_path) {
        get_use_segments(db, &parent_use_path, segments)?;
    }
    // Add current segment.
    match use_path {
        ast::UsePath::Leaf(use_ast) => {
            segments.push(use_ast.ident(db));
        }
        ast::UsePath::Single(use_ast) => {
            segments.push(use_ast.ident(db));
        }
        ast::UsePath::Multi(_) => {}
    };
    Ok(())
}

/// Returns the parent path segment of a use path.
fn get_parent_use_path(db: &dyn SyntaxGroup, use_path: &ast::UsePath) -> Option<ast::UsePath> {
    let mut node = use_path.as_syntax_node();
    loop {
        node = node.parent().expect("UsePath is not under an ItemUse.");
        return match node.kind(db) {
            SyntaxKind::ItemUse => None,
            SyntaxKind::UsePathSingle => {
                Some(ast::UsePath::Single(ast::UsePathSingle::from_syntax_node(db, node)))
            }
            SyntaxKind::UsePathMulti => {
                Some(ast::UsePath::Multi(ast::UsePathMulti::from_syntax_node(db, node)))
            }
            SyntaxKind::UsePathList => {
                continue;
            }
            SyntaxKind::UsePathLeaf => {
                unreachable!("UsePathLeaf can't be a parent of another UsePath.");
            }
            _ => {
                unreachable!();
            }
        };
    }
}

/// Cycle handling for [crate::db::SemanticGroup::priv_use_semantic_data].
pub fn priv_use_semantic_data_cycle(
    db: &dyn SemanticGroup,
    cycle: &[String],
    use_id: &UseId,
) -> Maybe<UseData> {
    let module_file_id = use_id.module_file_id(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_file_id.file_id(db.upcast())?);
    let use_ast = db.module_use_by_id(*use_id)?.to_maybe()?;
    let err = Err(diagnostics.report(
        &use_ast,
        if cycle.len() == 1 {
            // `use bad_name`, finds itself but we don't want to report a cycle in that case.
            PathNotFound(NotFoundItemType::Identifier)
        } else {
            UseCycle
        },
    ));
    let inference_id =
        InferenceId::LookupItemDeclaration(LookupItemId::ModuleItem(ModuleItemId::Use(*use_id)));
    Ok(UseData {
        diagnostics: diagnostics.build(),
        resolved_item: err,
        resolver_data: Arc::new(ResolverData::new(module_file_id, inference_id)),
    })
}

/// Query implementation of [crate::db::SemanticGroup::use_semantic_diagnostics].
pub fn use_semantic_diagnostics(
    db: &dyn SemanticGroup,
    use_id: UseId,
) -> Diagnostics<SemanticDiagnostic> {
    db.priv_use_semantic_data(use_id).map(|data| data.diagnostics).unwrap_or_default()
}

/// Query implementation of [crate::db::SemanticGroup::use_resolver_data].
pub fn use_resolver_data(db: &dyn SemanticGroup, use_id: UseId) -> Maybe<Arc<ResolverData>> {
    Ok(db.priv_use_semantic_data(use_id)?.resolver_data)
}

/// Trivial cycle handler for [crate::db::SemanticGroup::use_resolver_data].
pub fn use_resolver_data_cycle(
    db: &dyn SemanticGroup,
    _cycle: &[String],
    use_id: &UseId,
) -> Maybe<Arc<ResolverData>> {
    // Forwarding (not as a query) cycle handling to `priv_use_semantic_data` cycle handler.
    use_resolver_data(db, *use_id)
}

pub trait SemanticUseEx<'a>: Upcast<dyn SemanticGroup + 'a> {
    /// Returns the resolved item or an error if it can't be resolved.
    ///
    /// This is not a query as the cycle handling is done in priv_use_semantic_data.
    fn use_resolved_item(&self, use_id: UseId) -> Maybe<ResolvedGenericItem> {
        let db = self.upcast();
        db.priv_use_semantic_data(use_id)?.resolved_item
    }
}

impl<'a, T: Upcast<dyn SemanticGroup + 'a> + ?Sized> SemanticUseEx<'a> for T {}
