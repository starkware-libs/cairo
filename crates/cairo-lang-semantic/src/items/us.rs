use std::sync::Arc;

use cairo_lang_defs::ids::{
    GlobalUseId, LanguageElementId, LookupItemId, ModuleId, ModuleItemId, UseId,
};
use cairo_lang_diagnostics::{Diagnostics, Maybe, ToMaybe};
use cairo_lang_proc_macros::DebugWithDb;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::UsePathEx;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{TypedSyntaxNode, ast};
use cairo_lang_utils::Upcast;

use crate::SemanticDiagnostic;
use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnosticKind::*;
use crate::diagnostic::{
    ElementKind, NotFoundItemType, SemanticDiagnostics, SemanticDiagnosticsBuilder,
};
use crate::expr::inference::InferenceId;
use crate::resolve::{ResolvedGenericItem, Resolver, ResolverData};

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
    let mut diagnostics = SemanticDiagnostics::default();
    let inference_id =
        InferenceId::LookupItemDeclaration(LookupItemId::ModuleItem(ModuleItemId::Use(use_id)));
    let mut resolver = Resolver::new(db, module_file_id, inference_id);
    // TODO(spapini): when code changes in a file, all the AST items change (as they contain a path
    // to the green root that changes. Once ASTs are rooted on items, use a selector that picks only
    // the item instead of all the module data.
    let use_ast = ast::UsePath::Leaf(db.module_use_by_id(use_id)?.to_maybe()?);
    let item = use_ast.get_item(db.upcast());
    resolver.set_feature_config(&use_id, &item, &mut diagnostics);
    let segments = get_use_path_segments(db.upcast(), use_ast)?;
    let resolved_item = resolver.resolve_generic_path(
        &mut diagnostics,
        segments,
        NotFoundItemType::Identifier,
        None,
    );
    let resolver_data = Arc::new(resolver.data);

    Ok(UseData { diagnostics: diagnostics.build(), resolved_item, resolver_data })
}

/// Query implementation of [crate::db::SemanticGroup::priv_global_use_semantic_data].
pub fn priv_global_use_semantic_data(
    db: &dyn SemanticGroup,
    global_use_id: GlobalUseId,
) -> Maybe<UseGlobalData> {
    let module_file_id = global_use_id.module_file_id(db.upcast());
    let mut diagnostics = SemanticDiagnostics::default();
    let inference_id = InferenceId::GlobalUseStar(global_use_id);
    let star_ast = ast::UsePath::Star(db.module_global_use_by_id(global_use_id)?.to_maybe()?);
    let mut resolver = Resolver::new(db, module_file_id, inference_id);

    let item = star_ast.get_item(db.upcast());
    let segments = get_use_path_segments(db.upcast(), star_ast.clone())?;
    resolver.set_feature_config(&global_use_id, &item, &mut diagnostics);
    let resolved_item = resolver.resolve_generic_path(
        &mut diagnostics,
        segments,
        NotFoundItemType::Identifier,
        None,
    );
    let resolver_data = Arc::new(resolver.data);
    let imported_module = resolved_item.and_then(|item| {
        if let ResolvedGenericItem::Module(module_id) = item {
            Ok(module_id)
        } else {
            Err(diagnostics.report(&star_ast, UnexpectedElement {
                expected: vec![ElementKind::Module],
                actual: (&item).into(),
            }))
        }
    });

    Ok(UseGlobalData { diagnostics: diagnostics.build(), imported_module, resolver_data })
}

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct UseGlobalData {
    diagnostics: Diagnostics<SemanticDiagnostic>,
    pub imported_module: Maybe<ModuleId>,
    resolver_data: Arc<ResolverData>,
}

/// Returns the segments that are the parts of the use path.
///
/// The segments are returned in the order they appear in the use path.
/// Only `UsePathLeaf` and `UsePathSingle` are supported.
///
/// For example:
/// Given the `c` of `use a::b::{c, d};` will return `[a, b, c]`.
/// Given the `b` of `use a::b::{c, d};` will return `[a, b]`.
pub fn get_use_path_segments(
    db: &dyn SyntaxGroup,
    use_path: ast::UsePath,
) -> Maybe<Vec<ast::PathSegment>> {
    let mut rev_segments = vec![];
    match &use_path {
        ast::UsePath::Leaf(use_ast) => rev_segments.push(use_ast.ident(db)),
        ast::UsePath::Single(use_ast) => rev_segments.push(use_ast.ident(db)),
        ast::UsePath::Star(_) => {}
        ast::UsePath::Multi(_) => {
            panic!("Only `UsePathLeaf` and `UsePathSingle` are supported.")
        }
    }
    let mut current_use_path = use_path;
    while let Some(parent_use_path) = get_parent_single_use_path(db, &current_use_path) {
        rev_segments.push(parent_use_path.ident(db));
        current_use_path = ast::UsePath::Single(parent_use_path);
    }
    Ok(rev_segments.into_iter().rev().collect())
}

/// Returns the parent `UsePathSingle` of a use path if exists.
fn get_parent_single_use_path(
    db: &dyn SyntaxGroup,
    use_path: &ast::UsePath,
) -> Option<ast::UsePathSingle> {
    use SyntaxKind::*;
    let mut node = use_path.as_syntax_node();
    loop {
        node = node.parent().expect("`UsePath` is not under an `ItemUse`.");
        match node.kind(db) {
            ItemUse => return None,
            UsePathSingle => return Some(ast::UsePathSingle::from_syntax_node(db, node)),
            UsePathList | UsePathMulti => continue,
            UsePathLeaf => unreachable!("`UsePathLeaf` can't be a parent of another `UsePath`."),
            other => unreachable!("`{other:?}` can't be a parent of `UsePath`."),
        };
    }
}

/// Cycle handling for [crate::db::SemanticGroup::priv_use_semantic_data].
pub fn priv_use_semantic_data_cycle(
    db: &dyn SemanticGroup,
    cycle: &salsa::Cycle,
    use_id: &UseId,
) -> Maybe<UseData> {
    let module_file_id = use_id.module_file_id(db.upcast());
    let mut diagnostics = SemanticDiagnostics::default();
    let use_ast = db.module_use_by_id(*use_id)?.to_maybe()?;
    let err = Err(diagnostics.report(
        &use_ast,
        if cycle.participant_keys().count() == 1 {
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
    _cycle: &salsa::Cycle,
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
