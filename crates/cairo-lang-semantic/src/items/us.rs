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
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use cairo_lang_utils::unordered_hash_set::UnorderedHashSet;

use super::module::get_module_global_uses;
use super::visibility::peek_visible_in;
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
    let resolver_data: Arc<ResolverData> = Arc::new(resolver.data);

    Ok(UseData { diagnostics: diagnostics.build(), resolved_item, resolver_data })
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

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct UseGlobalData {
    diagnostics: Diagnostics<SemanticDiagnostic>,
    imported_module: Maybe<ModuleId>,
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
    let edition = resolver.settings.edition;
    if edition.ignore_visibility() {
        // We block support for global use where visibility is ignored.
        diagnostics.report(&star_ast, GlobalUsesNotSupportedInEdition(edition));
    }

    let item = star_ast.get_item(db.upcast());
    let segments = get_use_path_segments(db.upcast(), star_ast.clone())?;
    if segments.is_empty() {
        let imported_module = Err(diagnostics.report(star_ast.stable_ptr(), UseStarEmptyPath));
        return Ok(UseGlobalData { diagnostics: diagnostics.build(), imported_module });
    }
    resolver.set_feature_config(&global_use_id, &item, &mut diagnostics);
    let resolved_item = resolver.resolve_generic_path(
        &mut diagnostics,
        segments.clone(),
        NotFoundItemType::Identifier,
        None,
    )?;
    // unwrap always safe as the resolver already resolved the entire path.
    let last_segment = segments.last().unwrap();
    let imported_module = match resolved_item {
        ResolvedGenericItem::Module(module_id) => Ok(module_id),
        _ => Err(diagnostics.report(
            last_segment.stable_ptr(),
            UnexpectedElement {
                expected: vec![ElementKind::Module],
                actual: (&resolved_item).into(),
            },
        )),
    };
    Ok(UseGlobalData { diagnostics: diagnostics.build(), imported_module })
}

/// Query implementation of [crate::db::SemanticGroup::priv_global_use_imported_module].
pub fn priv_global_use_imported_module(
    db: &dyn SemanticGroup,
    global_use_id: GlobalUseId,
) -> Maybe<ModuleId> {
    db.priv_global_use_semantic_data(global_use_id)?.imported_module
}

/// Query implementation of [crate::db::SemanticGroup::global_use_semantic_diagnostics].
pub fn global_use_semantic_diagnostics(
    db: &dyn SemanticGroup,
    global_use_id: GlobalUseId,
) -> Diagnostics<SemanticDiagnostic> {
    db.priv_global_use_semantic_data(global_use_id).map(|data| data.diagnostics).unwrap_or_default()
}

/// Cycle handling for [crate::db::SemanticGroup::priv_global_use_semantic_data].
pub fn priv_global_use_semantic_data_cycle(
    db: &dyn SemanticGroup,
    cycle: &salsa::Cycle,
    global_use_id: &GlobalUseId,
) -> Maybe<UseGlobalData> {
    let mut diagnostics = SemanticDiagnostics::default();
    let global_use_ast = db.module_global_use_by_id(*global_use_id)?.to_maybe()?;
    let star_ast = ast::UsePath::Star(db.module_global_use_by_id(*global_use_id)?.to_maybe()?);
    let segments = get_use_path_segments(db.upcast(), star_ast)?;
    let err = if cycle.participant_keys().count() <= 3 && segments.len() == 1 {
        // `use bad_name::*`, will attempt to find `bad_name` in the current module's global
        // uses, but which includes itself - but we don't want to report a cycle in this case.
        diagnostics.report(
            segments.last().unwrap().stable_ptr(),
            PathNotFound(NotFoundItemType::Identifier),
        )
    } else {
        diagnostics.report(&global_use_ast, UseCycle)
    };
    Ok(UseGlobalData { diagnostics: diagnostics.build(), imported_module: Err(err) })
}

/// The modules that are imported by a module, using global uses.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImportedModules {
    /// The imported modules that have a path where each step is visible by the previous module.
    pub accessible: OrderedHashSet<(ModuleId, ModuleId)>,
    // TODO(Tomer-StarkWare): consider changing from all_modules to inaccessible_modules
    /// All the imported modules.
    pub all: OrderedHashSet<ModuleId>,
}
/// Returns the modules that are imported with `use *` in the current module.
/// Query implementation of [crate::db::SemanticGroup::priv_module_use_star_modules].
pub fn priv_module_use_star_modules(
    db: &dyn SemanticGroup,
    module_id: ModuleId,
) -> Arc<ImportedModules> {
    let mut visited = UnorderedHashSet::<_>::default();
    let mut stack = vec![(module_id, module_id)];
    let mut accessible_modules = OrderedHashSet::default();
    // Iterate over all modules that are imported through `use *`, and are accessible from the
    // current module.
    while let Some((user_module, containing_module)) = stack.pop() {
        if !visited.insert((user_module, containing_module)) {
            continue;
        }
        let Ok(glob_uses) = get_module_global_uses(db, containing_module) else {
            continue;
        };
        for (glob_use, item_visibility) in glob_uses.iter() {
            let Ok(module_id_found) = db.priv_global_use_imported_module(*glob_use) else {
                continue;
            };
            if peek_visible_in(db.upcast(), *item_visibility, containing_module, user_module) {
                stack.push((containing_module, module_id_found));
                accessible_modules.insert((containing_module, module_id_found));
            }
        }
    }
    let mut visited = UnorderedHashSet::<_>::default();
    let mut stack = vec![module_id];
    let mut all_modules = OrderedHashSet::default();
    // Iterate over all modules that are imported through `use *`.
    while let Some(curr_module_id) = stack.pop() {
        if !visited.insert(curr_module_id) {
            continue;
        }
        all_modules.insert(curr_module_id);
        let Ok(glob_uses) = get_module_global_uses(db, curr_module_id) else { continue };
        for glob_use in glob_uses.keys() {
            let Ok(module_id_found) = db.priv_global_use_imported_module(*glob_use) else {
                continue;
            };
            stack.push(module_id_found);
        }
    }
    Arc::new(ImportedModules { accessible: accessible_modules, all: all_modules })
}
