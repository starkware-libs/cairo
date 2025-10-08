use std::sync::Arc;

use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::{
    GlobalUseId, LanguageElementId, LookupItemId, ModuleId, ModuleItemId, UseId,
};
use cairo_lang_diagnostics::{Diagnostics, Maybe};
use cairo_lang_filesystem::ids::Tracked;
use cairo_lang_proc_macros::DebugWithDb;
use cairo_lang_syntax::node::helpers::UsePathEx;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{TypedSyntaxNode, ast};
use cairo_lang_utils::ordered_hash_map::{Entry, OrderedHashMap};
use cairo_lang_utils::unordered_hash_set::UnorderedHashSet;
use itertools::{Itertools, chain};
use salsa::Database;

use super::module::get_module_global_uses;
use super::visibility::peek_visible_in;
use crate::SemanticDiagnostic;
use crate::diagnostic::SemanticDiagnosticKind::*;
use crate::diagnostic::{
    ElementKind, NotFoundItemType, SemanticDiagnostics, SemanticDiagnosticsBuilder,
};
use crate::expr::inference::InferenceId;
use crate::items::macro_call::module_macro_modules;
use crate::resolve::{ResolutionContext, ResolvedGenericItem, Resolver, ResolverData};

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb, salsa::Update)]
#[debug_db(dyn Database)]
pub struct UseData<'db> {
    diagnostics: Diagnostics<'db, SemanticDiagnostic<'db>>,
    resolved_item: Maybe<ResolvedGenericItem<'db>>,
    resolver_data: Arc<ResolverData<'db>>,
}

/// Represents path segments in a use statement, with support for a leading dollar token ($).
pub struct UseAsPathSegments<'db> {
    pub segments: Vec<ast::PathSegment<'db>>,
    pub is_placeholder: Option<ast::TerminalDollar<'db>>,
}

/// Indicates whether we found a UsePathSingle node, a leading dollar token ($), or hit ItemUse
/// without a dollar when traversing the AST.
pub enum UsePathOrDollar<'db> {
    UsePathSingle(ast::UsePathSingle<'db>),
    Dollar(ast::TerminalDollar<'db>),
    None,
}

/// Implementation of [UseSemantic::priv_use_semantic_data].
fn priv_use_semantic_data<'db>(
    db: &'db dyn Database,
    use_id: UseId<'db>,
) -> Maybe<Arc<UseData<'db>>> {
    let module_id = use_id.module_id(db);
    let mut diagnostics = SemanticDiagnostics::default();
    let module_item_id = ModuleItemId::Use(use_id);
    let inference_id = InferenceId::LookupItemDeclaration(LookupItemId::ModuleItem(module_item_id));
    let mut resolver = Resolver::new(db, module_id, inference_id);
    // TODO(spapini): when code changes in a file, all the AST items change (as they contain a path
    // to the green root that changes. Once ASTs are rooted on items, use a selector that picks only
    // the item instead of all the module data.
    let use_ast = ast::UsePath::Leaf(db.module_use_by_id(use_id)?);
    let item = use_ast.get_item(db);
    resolver.set_feature_config(&use_id, &item, &mut diagnostics);
    let resolved_item = resolver.resolve_use_path(
        &mut diagnostics,
        use_ast,
        ResolutionContext::ModuleItem(module_item_id),
    );
    let resolver_data: Arc<ResolverData<'_>> = Arc::new(resolver.data);

    Ok(Arc::new(UseData { diagnostics: diagnostics.build(), resolved_item, resolver_data }))
}

/// Query implementation of [UseSemantic::priv_use_semantic_data].
#[salsa::tracked(cycle_result=priv_use_semantic_data_cycle)]
fn priv_use_semantic_data_tracked<'db>(
    db: &'db dyn Database,
    use_id: UseId<'db>,
) -> Maybe<Arc<UseData<'db>>> {
    priv_use_semantic_data(db, use_id)
}

/// Returns the segments that are the parts of the use path.
///
/// The segments are returned in the order they appear in the use path.
/// Only `UsePathLeaf` and `UsePathSingle` are supported.
///
/// For example:
/// Given the `c` of `use a::b::{c, d};` will return `[a, b, c]`.
/// Given the `b` of `use a::b::{c, d};` will return `[a, b]`.
pub fn get_use_path_segments<'db>(
    db: &'db dyn Database,
    use_path: ast::UsePath<'db>,
) -> Maybe<UseAsPathSegments<'db>> {
    let mut rev_segments = vec![];
    match &use_path {
        ast::UsePath::Leaf(use_ast) => rev_segments.push(use_ast.ident(db)),
        ast::UsePath::Single(use_ast) => rev_segments.push(use_ast.ident(db)),
        ast::UsePath::Star(_) => {}
        ast::UsePath::Multi(_) => {
            panic!("Only `UsePathLeaf` and `UsePathSingle` are supported.")
        }
    }
    let mut current_path = use_path;
    let mut dollar = None;
    loop {
        match get_parent_single_use_path(db, &current_path)? {
            UsePathOrDollar::UsePathSingle(parent_use_path) => {
                let ident = parent_use_path.ident(db);
                rev_segments.push(ident);
                current_path = ast::UsePath::Single(parent_use_path);
            }
            UsePathOrDollar::Dollar(d) => {
                dollar = Some(d);
                break;
            }
            UsePathOrDollar::None => break,
        }
    }
    Ok(UseAsPathSegments {
        segments: rev_segments.into_iter().rev().collect(),
        is_placeholder: dollar,
    })
}

/// Returns the parent `UsePathSingle` of a use path if it exists.
fn get_parent_single_use_path<'db>(
    db: &'db dyn Database,
    use_path: &ast::UsePath<'db>,
) -> Maybe<UsePathOrDollar<'db>> {
    let node = use_path.as_syntax_node();
    let parent = node.parent(db).expect("`UsePath` is not under an `ItemUse`.");
    match parent.kind(db) {
        SyntaxKind::UsePathSingle => {
            Ok(UsePathOrDollar::UsePathSingle(ast::UsePathSingle::from_syntax_node(db, parent)))
        }
        SyntaxKind::ItemUse => {
            let typed_use_node = ast::ItemUse::from_syntax_node(db, parent);
            Ok(match typed_use_node.dollar(db) {
                ast::OptionTerminalDollar::TerminalDollar(dollar) => {
                    UsePathOrDollar::Dollar(dollar)
                }
                ast::OptionTerminalDollar::Empty(_) => UsePathOrDollar::None,
            })
        }
        SyntaxKind::UsePathList | SyntaxKind::UsePathMulti => {
            let mut current = parent;
            while let Some(parent) = current.parent(db) {
                match parent.kind(db) {
                    SyntaxKind::UsePathSingle => {
                        return Ok(UsePathOrDollar::UsePathSingle(
                            ast::UsePathSingle::from_syntax_node(db, parent),
                        ));
                    }
                    SyntaxKind::ItemUse => {
                        let item_use = ast::ItemUse::from_syntax_node(db, parent);
                        if let ast::OptionTerminalDollar::TerminalDollar(d) = item_use.dollar(db) {
                            return Ok(UsePathOrDollar::Dollar(d));
                        }
                        return Ok(UsePathOrDollar::None);
                    }
                    SyntaxKind::UsePathList | SyntaxKind::UsePathMulti => {
                        current = parent;
                        continue;
                    }
                    _ => return Ok(UsePathOrDollar::None),
                }
            }
            Ok(UsePathOrDollar::None)
        }
        _ => Ok(UsePathOrDollar::None),
    }
}

/// Cycle handling for [UseSemantic::priv_use_semantic_data].
fn priv_use_semantic_data_cycle<'db>(
    db: &'db dyn Database,
    use_id: UseId<'db>,
) -> Maybe<Arc<UseData<'db>>> {
    let module_id = use_id.module_id(db);
    let mut diagnostics = SemanticDiagnostics::default();
    let use_ast = db.module_use_by_id(use_id)?;
    let err = Err(diagnostics.report(use_ast.stable_ptr(db), UseCycle));
    let inference_id =
        InferenceId::LookupItemDeclaration(LookupItemId::ModuleItem(ModuleItemId::Use(use_id)));
    Ok(Arc::new(UseData {
        diagnostics: diagnostics.build(),
        resolved_item: err,
        resolver_data: Arc::new(ResolverData::new(module_id, inference_id)),
    }))
}

/// Implementation of [UseSemantic::use_semantic_diagnostics].
fn use_semantic_diagnostics<'db>(
    db: &'db dyn Database,
    use_id: UseId<'db>,
) -> Diagnostics<'db, SemanticDiagnostic<'db>> {
    db.priv_use_semantic_data(use_id).map(|data| data.diagnostics.clone()).unwrap_or_default()
}

/// Query implementation of [UseSemantic::use_semantic_diagnostics].
#[salsa::tracked]
fn use_semantic_diagnostics_tracked<'db>(
    db: &'db dyn Database,
    use_id: UseId<'db>,
) -> Diagnostics<'db, SemanticDiagnostic<'db>> {
    use_semantic_diagnostics(db, use_id)
}

/// Implementation of [UseSemantic::use_resolver_data].
fn use_resolver_data<'db>(
    db: &'db dyn Database,
    use_id: UseId<'db>,
) -> Maybe<Arc<ResolverData<'db>>> {
    Ok(db.priv_use_semantic_data(use_id)?.resolver_data.clone())
}

/// Query implementation of [UseSemantic::use_resolver_data].
#[salsa::tracked(cycle_result=use_resolver_data_cycle)]
fn use_resolver_data_tracked<'db>(
    db: &'db dyn Database,
    use_id: UseId<'db>,
) -> Maybe<Arc<ResolverData<'db>>> {
    use_resolver_data(db, use_id)
}

/// Trivial cycle handler for [UseSemantic::use_resolver_data].
fn use_resolver_data_cycle<'db>(
    db: &'db dyn Database,
    use_id: UseId<'db>,
) -> Maybe<Arc<ResolverData<'db>>> {
    // Forwarding (not as a query) cycle handling to `priv_use_semantic_data` cycle handler.
    use_resolver_data(db, use_id)
}

pub trait SemanticUseEx<'a>: Database {
    /// Returns the resolved item or an error if it can't be resolved.
    ///
    /// This is not a query as the cycle handling is done in priv_use_semantic_data.
    fn use_resolved_item(&'a self, use_id: UseId<'a>) -> Maybe<ResolvedGenericItem<'a>> {
        let db = self.as_dyn_database();
        db.priv_use_semantic_data(use_id)?.resolved_item.clone()
    }
}

impl<'a, T: Database + ?Sized> SemanticUseEx<'a> for T {}

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb, salsa::Update)]
#[debug_db(dyn Database)]
pub struct UseGlobalData<'db> {
    diagnostics: Diagnostics<'db, SemanticDiagnostic<'db>>,
    imported_module: Maybe<ModuleId<'db>>,
}

/// Implementation of [UseSemantic::priv_global_use_semantic_data].
fn priv_global_use_semantic_data<'db>(
    db: &'db dyn Database,
    global_use_id: GlobalUseId<'db>,
) -> Maybe<UseGlobalData<'db>> {
    let module_id = global_use_id.module_id(db);
    let mut diagnostics = SemanticDiagnostics::default();
    let inference_id = InferenceId::GlobalUseStar(global_use_id);
    let star_ast = ast::UsePath::Star(db.module_global_use_by_id(global_use_id)?);
    let mut resolver = Resolver::new(db, module_id, inference_id);
    let edition = resolver.settings.edition;
    if edition.ignore_visibility() {
        // We block support for global use where visibility is ignored.
        diagnostics.report(star_ast.stable_ptr(db), GlobalUsesNotSupportedInEdition(edition));
    }

    let item = star_ast.get_item(db);
    let segments = get_use_path_segments(db, star_ast.clone())?;
    let Some(last_segment) = segments.segments.last() else {
        let imported_module = Err(diagnostics.report(star_ast.stable_ptr(db), UseStarEmptyPath));
        return Ok(UseGlobalData { diagnostics: diagnostics.build(), imported_module });
    };
    let last_element_ptr = last_segment.stable_ptr(db);
    resolver.set_feature_config(&global_use_id, &item, &mut diagnostics);
    let imported_module = resolver
        .resolve_generic_path(
            &mut diagnostics,
            segments,
            NotFoundItemType::Identifier,
            ResolutionContext::Default,
        )
        .and_then(|resolved_item| match &resolved_item {
            ResolvedGenericItem::Module(module_id) => Ok(*module_id),
            other => Err(diagnostics.report(
                last_element_ptr,
                UnexpectedElement { expected: vec![ElementKind::Module], actual: other.into() },
            )),
        });
    if imported_module == Ok(module_id) {
        diagnostics.report(star_ast.stable_ptr(db), SelfGlobalUse);
    }
    Ok(UseGlobalData { diagnostics: diagnostics.build(), imported_module })
}

/// Query implementation of [UseSemantic::priv_global_use_semantic_data].
#[salsa::tracked(cycle_fn=priv_global_use_semantic_data_tracked_cycle_fn, cycle_initial=priv_global_use_semantic_data_tracked_initial)]
fn priv_global_use_semantic_data_tracked<'db>(
    db: &'db dyn Database,
    global_use_id: GlobalUseId<'db>,
) -> Maybe<UseGlobalData<'db>> {
    priv_global_use_semantic_data(db, global_use_id)
}

/// Cycle handling for [UseSemantic::priv_global_use_semantic_data].
fn priv_global_use_semantic_data_tracked_cycle_fn<'db>(
    _db: &'db dyn Database,
    _value: &Maybe<UseGlobalData<'db>>,
    _count: u32,
    _global_use_id: GlobalUseId<'db>,
) -> salsa::CycleRecoveryAction<Maybe<UseGlobalData<'db>>> {
    salsa::CycleRecoveryAction::Iterate
}

/// Cycle handling for [UseSemantic::priv_global_use_semantic_data].
fn priv_global_use_semantic_data_tracked_initial<'db>(
    db: &'db dyn Database,
    global_use_id: GlobalUseId<'db>,
) -> Maybe<UseGlobalData<'db>> {
    let mut diagnostics = SemanticDiagnostics::default();
    let global_use_ast = db.module_global_use_by_id(global_use_id)?;
    let star_ast = ast::UsePath::Star(db.module_global_use_by_id(global_use_id)?);
    let segments = get_use_path_segments(db, star_ast)?;
    let err = if segments.segments.len() == 1 {
        // `use bad_name::*`, will attempt to find `bad_name` in the current module's global
        // uses, which includes the global use `use bad_name::*` (itself) - but we don't want to
        // report a cycle in this case.
        diagnostics.report(
            segments.segments.last().unwrap().stable_ptr(db),
            PathNotFound(NotFoundItemType::Identifier),
        )
    } else {
        diagnostics.report(global_use_ast.stable_ptr(db), UseCycle)
    };
    Ok(UseGlobalData { diagnostics: diagnostics.build(), imported_module: Err(err) })
}

/// Implementation of [UseSemantic::priv_global_use_imported_module].
fn priv_global_use_imported_module<'db>(
    db: &'db dyn Database,
    global_use_id: GlobalUseId<'db>,
) -> Maybe<ModuleId<'db>> {
    db.priv_global_use_semantic_data(global_use_id)?.imported_module
}

/// Query implementation of [UseSemantic::priv_global_use_imported_module].
#[salsa::tracked]
fn priv_global_use_imported_module_tracked<'db>(
    db: &'db dyn Database,
    global_use_id: GlobalUseId<'db>,
) -> Maybe<ModuleId<'db>> {
    priv_global_use_imported_module(db, global_use_id)
}

/// Implementation of [UseSemantic::global_use_semantic_diagnostics].
fn global_use_semantic_diagnostics<'db>(
    db: &'db dyn Database,
    global_use_id: GlobalUseId<'db>,
) -> Diagnostics<'db, SemanticDiagnostic<'db>> {
    db.priv_global_use_semantic_data(global_use_id).map(|data| data.diagnostics).unwrap_or_default()
}

/// Query implementation of [UseSemantic::global_use_semantic_diagnostics].
#[salsa::tracked]
fn global_use_semantic_diagnostics_tracked<'db>(
    db: &'db dyn Database,
    global_use_id: GlobalUseId<'db>,
) -> Diagnostics<'db, SemanticDiagnostic<'db>> {
    global_use_semantic_diagnostics(db, global_use_id)
}

/// The modules that are imported by a module, using global uses and macro calls.
pub type ImportedModules<'db> = OrderedHashMap<ModuleId<'db>, ImportInfo<'db>>;

/// Information about a module that is imported by a module, using global uses and macro calls.
#[derive(Debug, Default, Clone, PartialEq, Eq, salsa::Update)]
pub struct ImportInfo<'db> {
    /// The modules that directly imported this module.
    pub user_modules: Vec<ModuleId<'db>>,
}

/// Returns the modules that are imported with `use *` and macro calls in the current module.
/// Query implementation of [UseSemantic::module_imported_modules].
#[salsa::tracked(returns(ref),cycle_fn=module_imported_modules_cycle_fn, cycle_initial=module_imported_modules_initial)]
fn module_imported_modules<'db>(
    db: &'db dyn Database,
    _tracked: Tracked,
    module_id: ModuleId<'db>,
) -> ImportedModules<'db> {
    let mut visited = UnorderedHashSet::<_>::default();
    let mut stack = vec![(module_id, module_id)];
    let mut modules = OrderedHashMap::<ModuleId<'db>, ImportInfo<'db>>::default();
    modules.insert(module_id, ImportInfo { user_modules: vec![module_id] });
    // Iterate over all modules that are imported through `use *`, and are accessible from the
    // current module.
    while let Some((user_module, containing_module)) = stack.pop() {
        if !visited.insert((user_module, containing_module)) {
            continue;
        }
        for defined_module in
            chain!([&containing_module], module_macro_modules(db, false, containing_module))
        {
            let Ok(glob_uses) = get_module_global_uses(db, *defined_module) else {
                continue;
            };
            for (glob_use, item_visibility) in glob_uses.iter() {
                let Ok(module_id_found) = db.priv_global_use_imported_module(*glob_use) else {
                    continue;
                };
                // Add the module to the map to find all reachable modules.
                let entry = modules.entry(module_id_found).or_default();
                if peek_visible_in(db, *item_visibility, containing_module, user_module) {
                    stack.push((containing_module, module_id_found));
                    entry.user_modules.push(containing_module);
                }
            }
        }
    }
    let mut stack =
        modules.iter().filter(|(_, v)| v.user_modules.is_empty()).map(|(k, _)| *k).collect_vec();
    // Iterate over all modules that are imported through `use *`.
    while let Some(curr_module_id) = stack.pop() {
        for defined_module in
            chain!([&curr_module_id], module_macro_modules(db, false, curr_module_id))
        {
            let Ok(glob_uses) = get_module_global_uses(db, *defined_module) else { continue };
            for glob_use in glob_uses.keys() {
                if let Ok(module_id_found) = db.priv_global_use_imported_module(*glob_use)
                    && let Entry::Vacant(entry) = modules.entry(module_id_found)
                {
                    entry.insert(ImportInfo::default());
                    stack.push(module_id_found);
                }
            }
        }
    }
    modules
}

/// Cycle handling for [UseSemantic::module_imported_modules].
fn module_imported_modules_cycle_fn<'db>(
    _db: &dyn Database,
    _value: &ImportedModules<'db>,
    _count: u32,
    _tracked: Tracked,
    _module_id: ModuleId<'db>,
) -> salsa::CycleRecoveryAction<ImportedModules<'db>> {
    salsa::CycleRecoveryAction::Iterate
}

/// Cycle handling for [UseSemantic::module_imported_modules].
fn module_imported_modules_initial<'db>(
    _db: &'db dyn Database,
    _tracked: Tracked,
    _module_id: ModuleId<'db>,
) -> ImportedModules<'db> {
    Default::default()
}

/// Trait for use-related semantic queries.
pub trait UseSemantic<'db>: Database {
    /// Private query to compute data about a use.
    fn priv_use_semantic_data(&'db self, use_id: UseId<'db>) -> Maybe<Arc<UseData<'db>>> {
        priv_use_semantic_data_tracked(self.as_dyn_database(), use_id)
    }
    /// Returns the semantic diagnostics of a use.
    fn use_semantic_diagnostics(
        &'db self,
        use_id: UseId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>> {
        use_semantic_diagnostics_tracked(self.as_dyn_database(), use_id)
    }
    /// Returns the resolver data of a use.
    fn use_resolver_data(&'db self, use_id: UseId<'db>) -> Maybe<Arc<ResolverData<'db>>> {
        use_resolver_data_tracked(self.as_dyn_database(), use_id)
    }
    /// Private query to compute data about a global use.
    fn priv_global_use_semantic_data(
        &'db self,
        global_use_id: GlobalUseId<'db>,
    ) -> Maybe<UseGlobalData<'db>> {
        priv_global_use_semantic_data_tracked(self.as_dyn_database(), global_use_id)
    }
    /// Private query to compute the imported module, given a global use.
    fn priv_global_use_imported_module(
        &'db self,
        global_use_id: GlobalUseId<'db>,
    ) -> Maybe<ModuleId<'db>> {
        priv_global_use_imported_module_tracked(self.as_dyn_database(), global_use_id)
    }
    /// Returns the semantic diagnostics of a global use.
    fn global_use_semantic_diagnostics(
        &'db self,
        global_use_id: GlobalUseId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>> {
        global_use_semantic_diagnostics_tracked(self.as_dyn_database(), global_use_id)
    }
    /// Computes the imported modules of a module, using global uses and macro calls.
    fn module_imported_modules(
        &'db self,
        _tracked: Tracked,
        module_id: ModuleId<'db>,
    ) -> &'db ImportedModules<'db> {
        module_imported_modules(self.as_dyn_database(), _tracked, module_id)
    }
}
impl<'db, T: Database + ?Sized> UseSemantic<'db> for T {}
