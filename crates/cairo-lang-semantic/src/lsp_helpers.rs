use std::sync::Arc;

use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::{
    GenericTypeId, ImportableId, LanguageElementId, ModuleId, ModuleItemId, NamedLanguageElementId,
    TraitFunctionId, TraitId,
};
use cairo_lang_filesystem::db::{CORELIB_CRATE_NAME, FilesGroup, default_crate_settings};
use cairo_lang_filesystem::ids::{CrateId, CrateLongId, FileId, SmolStrId, Tracked};
use cairo_lang_syntax::node::ast::ItemModule;
use cairo_lang_syntax::node::helpers::GetIdentifier;
use cairo_lang_syntax::node::{SyntaxNode, TypedSyntaxNode};
use cairo_lang_utils::Intern;
use cairo_lang_utils::ordered_hash_map::{Entry, OrderedHashMap};
use cairo_lang_utils::unordered_hash_set::UnorderedHashSet;
use itertools::chain;
use salsa::Database;

use crate::Variant;
use crate::corelib::{self, core_submodule, get_submodule};
use crate::expr::inference::InferenceId;
use crate::items::constant::ConstantSemantic;
use crate::items::enm::EnumSemantic;
use crate::items::free_function::FreeFunctionSemantic;
use crate::items::functions::GenericFunctionId;
use crate::items::imp::ImplSemantic;
use crate::items::macro_call::{MacroCallSemantic, module_macro_modules};
use crate::items::module::ModuleSemantic;
use crate::items::module_type_alias::ModuleTypeAliasSemantic;
use crate::items::trt::TraitSemantic;
use crate::items::us::SemanticUseEx;
use crate::keyword::SELF_PARAM_KW;
use crate::resolve::{ResolvedGenericItem, Resolver};
use crate::types::TypeHead;

/// A filter for types.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum TypeFilter<'db> {
    /// No filter is applied.
    NoFilter,
    /// Only methods with the given type head are returned.
    TypeHead(TypeHead<'db>),
}

/// Implementation of [LspHelpers::methods_in_module].
pub fn methods_in_module<'db>(
    db: &'db dyn Database,
    module_id: ModuleId<'db>,
    type_filter: TypeFilter<'db>,
) -> Arc<Vec<TraitFunctionId<'db>>> {
    let mut result = Vec::new();
    let Ok(module_traits_ids) = db.module_traits_ids(module_id) else {
        return result.into();
    };
    for trait_id in module_traits_ids.iter().copied() {
        if let Ok(trait_functions) = db.trait_functions(trait_id) {
            for trait_function in trait_functions.values() {
                let Ok(signature) = db.trait_function_signature(*trait_function) else {
                    continue;
                };
                let Some(first_param) = signature.params.first() else {
                    continue;
                };
                if first_param.name.long(db) != SELF_PARAM_KW {
                    continue;
                }
                if let TypeFilter::TypeHead(type_head) = &type_filter
                    && let Some(head) = first_param.ty.head(db)
                    && !fit_for_method(&head, type_head)
                {
                    continue;
                }

                result.push(*trait_function)
            }
        }
    }
    Arc::new(result)
}

/// Query implementation of [LspHelpers::methods_in_module].
#[salsa::tracked]
pub fn methods_in_module_tracked<'db>(
    db: &'db dyn Database,
    module_id: ModuleId<'db>,
    type_filter: TypeFilter<'db>,
) -> Arc<Vec<TraitFunctionId<'db>>> {
    methods_in_module(db, module_id, type_filter)
}

/// Checks if a type head can fit for a method.
fn fit_for_method(head: &TypeHead<'_>, type_head: &TypeHead<'_>) -> bool {
    // Allow generics so we can filter them later with resolver.
    if let TypeHead::Generic(_) = head {
        return true;
    }
    if head == type_head {
        return true;
    }
    if let TypeHead::Snapshot(snapshot_head) = head {
        return fit_for_method(snapshot_head.as_ref(), type_head);
    }
    false
}

/// Implementation of [LspHelpers::methods_in_crate].
pub fn methods_in_crate<'db>(
    db: &'db dyn Database,
    crate_id: CrateId<'db>,
    type_filter: TypeFilter<'db>,
) -> Arc<Vec<TraitFunctionId<'db>>> {
    let mut result = Vec::new();
    for module_id in db.crate_modules(crate_id).iter() {
        result.extend_from_slice(&db.methods_in_module(*module_id, type_filter.clone())[..])
    }
    Arc::new(result)
}

/// Query implementation of [LspHelpers::methods_in_crate].
#[salsa::tracked]
pub fn methods_in_crate_tracked<'db>(
    db: &'db dyn Database,
    crate_id: CrateId<'db>,
    type_filter: TypeFilter<'db>,
) -> Arc<Vec<TraitFunctionId<'db>>> {
    methods_in_crate(db, crate_id, type_filter)
}

/// Implementation of [LspHelpers::visible_importables_in_module].
pub fn visible_importables_in_module<'db>(
    db: &'db dyn Database,
    module_id: ModuleId<'db>,
    user_module_id: ModuleId<'db>,
    include_parent: bool,
) -> Arc<Vec<(ImportableId<'db>, String)>> {
    let mut visited_modules = UnorderedHashSet::default();
    visible_importables_in_module_ex(
        db,
        module_id,
        user_module_id,
        include_parent,
        &mut visited_modules,
    )
    .unwrap_or_else(|| Arc::new(Vec::new()))
}

/// Query implementation of [LspHelpers::visible_importables_in_module].
#[salsa::tracked]
pub fn visible_importables_in_module_tracked<'db>(
    db: &'db dyn Database,
    module_id: ModuleId<'db>,
    user_module_id: ModuleId<'db>,
    include_parent: bool,
) -> Arc<Vec<(ImportableId<'db>, String)>> {
    visible_importables_in_module(db, module_id, user_module_id, include_parent)
}

/// Returns the visible importables in a module, including the importables in the parent module if
/// needed. The visibility is relative to the module `user_module_id`.
fn visible_importables_in_module_ex<'db>(
    db: &'db dyn Database,
    module_id: ModuleId<'db>,
    user_module_id: ModuleId<'db>,
    include_parent: bool,
    visited_modules: &mut UnorderedHashSet<ModuleId<'db>>,
) -> Option<Arc<Vec<(ImportableId<'db>, String)>>> {
    let mut result = Vec::new();
    if visited_modules.contains(&module_id) {
        return Some(result.into());
    }

    let resolver = Resolver::new(db, user_module_id, InferenceId::NoContext);

    // Check if an item in the current module is visible from the user module.
    let is_visible = |item_name: SmolStrId<'_>| {
        let item_info = db.module_item_info_by_name(module_id, item_name).ok()??;
        Some(resolver.is_item_visible(module_id, &item_info, user_module_id))
    };
    visited_modules.insert(module_id);
    let mut modules_to_visit = vec![];
    // Add importables and traverse modules imported into the current module.
    for use_id in db.module_uses_ids(module_id).unwrap_or_default().iter().copied() {
        if !is_visible(use_id.name(db)).unwrap_or_default() {
            continue;
        }
        let Ok(resolved_item) = db.use_resolved_item(use_id) else {
            continue;
        };

        let (resolved_item, name) = match resolved_item {
            ResolvedGenericItem::Module(ModuleId::CrateRoot(crate_id)) => {
                result.extend_from_slice(&db.visible_importables_in_crate(crate_id, module_id)[..]);

                (ImportableId::Crate(crate_id), crate_id.long(db).name().long(db))
            }
            ResolvedGenericItem::Module(inner_module_id @ ModuleId::Submodule(module)) => {
                modules_to_visit.push(inner_module_id);

                (ImportableId::Submodule(module), module.name(db).long(db))
            }
            ResolvedGenericItem::Module(ModuleId::MacroCall { .. }) => {
                continue;
            }
            ResolvedGenericItem::GenericConstant(item_id) => {
                (ImportableId::Constant(item_id), item_id.name(db).long(db))
            }
            ResolvedGenericItem::GenericFunction(GenericFunctionId::Free(item_id)) => {
                (ImportableId::FreeFunction(item_id), item_id.name(db).long(db))
            }
            ResolvedGenericItem::GenericFunction(GenericFunctionId::Extern(item_id)) => {
                (ImportableId::ExternFunction(item_id), item_id.name(db).long(db))
            }
            ResolvedGenericItem::GenericType(GenericTypeId::Struct(item_id)) => {
                (ImportableId::Struct(item_id), item_id.name(db).long(db))
            }
            ResolvedGenericItem::GenericType(GenericTypeId::Enum(item_id)) => {
                let enum_name = item_id.name(db);

                if let Ok(variants) = db.enum_variants(item_id) {
                    for (name, id) in variants.iter() {
                        result.push((
                            ImportableId::Variant(*id),
                            format!("{}::{}", enum_name.long(db), name.long(db)),
                        ));
                    }
                }

                (ImportableId::Enum(item_id), enum_name.long(db))
            }
            ResolvedGenericItem::GenericType(GenericTypeId::Extern(item_id)) => {
                (ImportableId::ExternType(item_id), item_id.name(db).long(db))
            }
            ResolvedGenericItem::GenericTypeAlias(item_id) => {
                (ImportableId::TypeAlias(item_id), item_id.name(db).long(db))
            }
            ResolvedGenericItem::GenericImplAlias(item_id) => {
                (ImportableId::ImplAlias(item_id), item_id.name(db).long(db))
            }
            ResolvedGenericItem::Variant(Variant { id, .. }) => {
                (ImportableId::Variant(id), id.name(db).long(db))
            }
            ResolvedGenericItem::Trait(item_id) => {
                (ImportableId::Trait(item_id), item_id.name(db).long(db))
            }
            ResolvedGenericItem::Impl(item_id) => {
                (ImportableId::Impl(item_id), item_id.name(db).long(db))
            }
            ResolvedGenericItem::Macro(item_id) => {
                (ImportableId::MacroDeclaration(item_id), item_id.name(db).long(db))
            }
            ResolvedGenericItem::Variable(_)
            | ResolvedGenericItem::TraitItem(_)
            | ResolvedGenericItem::GenericFunction(GenericFunctionId::Impl(_)) => continue,
        };

        result.push((resolved_item, name.to_string()));
    }

    if !matches!(module_id, ModuleId::MacroCall { .. }) {
        modules_to_visit.extend(module_macro_modules(db, false, module_id).iter().copied());
    }

    for submodule_id in db.module_submodules_ids(module_id).unwrap_or_default().iter().copied() {
        if !is_visible(submodule_id.name(db)).unwrap_or_default() {
            continue;
        }
        result.push((ImportableId::Submodule(submodule_id), submodule_id.name(db).to_string(db)));
        modules_to_visit.push(ModuleId::Submodule(submodule_id));
    }

    // Handle enums separately because we need to include their variants.
    for enum_id in db.module_enums_ids(module_id).unwrap_or_default().iter().copied() {
        let enum_name = enum_id.name(db);
        if !is_visible(enum_name).unwrap_or_default() {
            continue;
        }

        result.push((ImportableId::Enum(enum_id), enum_name.to_string(db)));

        if let Ok(variants) = db.enum_variants(enum_id) {
            for (name, id) in variants.iter() {
                result.push((
                    ImportableId::Variant(*id),
                    format!("{}::{}", enum_name.long(db), name.long(db)),
                ));
            }
        }
    }

    macro_rules! module_importables {
        ($query:ident, $map:expr) => {
            for item_id in db.$query(module_id).ok().unwrap_or_default().iter().copied() {
                if !is_visible(item_id.name(db)).unwrap_or_default() {
                    continue;
                }
                result.push(($map(item_id), item_id.name(db).to_string(db)));
            }
        };
    }

    module_importables!(module_constants_ids, ImportableId::Constant);
    module_importables!(module_free_functions_ids, ImportableId::FreeFunction);
    module_importables!(module_structs_ids, ImportableId::Struct);
    module_importables!(module_type_aliases_ids, ImportableId::TypeAlias);
    module_importables!(module_impl_aliases_ids, ImportableId::ImplAlias);
    module_importables!(module_traits_ids, ImportableId::Trait);
    module_importables!(module_impls_ids, ImportableId::Impl);
    module_importables!(module_extern_functions_ids, ImportableId::ExternFunction);
    module_importables!(module_extern_types_ids, ImportableId::ExternType);
    module_importables!(module_macro_declarations_ids, ImportableId::MacroDeclaration);

    for submodule in modules_to_visit {
        for (item_id, path) in
            visible_importables_in_module_ex(db, submodule, user_module_id, false, visited_modules)
                .unwrap_or_default()
                .iter()
        {
            result.push((*item_id, format!("{}::{}", submodule.name(db).long(db), path)));
        }
    }
    // Traverse the parent module if needed.
    if include_parent {
        match module_id {
            ModuleId::CrateRoot(_) => {}
            ModuleId::Submodule(submodule_id) => {
                let parent_module_id = submodule_id.parent_module(db);
                for (item_id, path) in visible_importables_in_module_ex(
                    db,
                    parent_module_id,
                    user_module_id,
                    include_parent,
                    visited_modules,
                )
                .unwrap_or_default()
                .iter()
                {
                    result.push((*item_id, format!("super::{path}")));
                }
            }
            ModuleId::MacroCall { .. } => {}
        }
    }
    Some(Arc::new(result))
}

/// Implementation of [LspHelpers::visible_importables_in_crate].
pub fn visible_importables_in_crate<'db>(
    db: &'db dyn Database,
    crate_id: CrateId<'db>,
    user_module_id: ModuleId<'db>,
) -> Arc<Vec<(ImportableId<'db>, String)>> {
    let is_current_crate = user_module_id.owning_crate(db) == crate_id;
    let crate_name = if is_current_crate { "crate" } else { crate_id.long(db).name().long(db) };
    let crate_as_module = ModuleId::CrateRoot(crate_id);
    db.visible_importables_in_module(crate_as_module, user_module_id, false)
        .iter()
        .map(|(item_id, path)| (*item_id, format!("{crate_name}::{path}")))
        .collect::<Vec<_>>()
        .into()
}

/// Query implementation of [LspHelpers::visible_importables_in_crate].
#[salsa::tracked]
pub fn visible_importables_in_crate_tracked<'db>(
    db: &'db dyn Database,
    crate_id: CrateId<'db>,
    user_module_id: ModuleId<'db>,
) -> Arc<Vec<(ImportableId<'db>, String)>> {
    visible_importables_in_crate(db, crate_id, user_module_id)
}

/// Implementation of [LspHelpers::visible_importables_from_module].
pub fn visible_importables_from_module<'db>(
    db: &'db dyn Database,
    module_id: ModuleId<'db>,
) -> Option<Arc<OrderedHashMap<ImportableId<'db>, String>>> {
    let current_crate_id = module_id.owning_crate(db);
    let prelude_submodule_name =
        db.crate_config(current_crate_id)?.settings.edition.prelude_submodule_name(db);
    let core_prelude_submodule = core_submodule(db, SmolStrId::from(db, "prelude"));
    let prelude_submodule = get_submodule(db, core_prelude_submodule, prelude_submodule_name)?;

    let mut module_visible_importables = Vec::new();
    // Collect importables from the prelude.
    module_visible_importables.extend_from_slice(
        &db.visible_importables_in_module(prelude_submodule, prelude_submodule, false)[..],
    );
    // Collect importables from all dependency crates, including the current crate and corelib.
    let settings = db
        .crate_config(current_crate_id)
        .map(|c| &c.settings)
        .unwrap_or_else(|| default_crate_settings(db));
    for crate_id in chain!(
        [current_crate_id],
        (!settings.dependencies.contains_key(CORELIB_CRATE_NAME)).then(|| corelib::core_crate(db)),
        settings.dependencies.iter().map(|(name, setting)| {
            CrateLongId::Real {
                name: SmolStrId::from(db, name.clone()),
                discriminator: setting.discriminator.clone(),
            }
            .intern(db)
        })
    ) {
        module_visible_importables
            .extend_from_slice(&db.visible_importables_in_crate(crate_id, module_id)[..]);
        module_visible_importables
            .push((ImportableId::Crate(crate_id), crate_id.long(db).name().to_string(db)));
    }

    // Collect importables visible in the current module.
    module_visible_importables
        .extend_from_slice(&db.visible_importables_in_module(module_id, module_id, true)[..]);

    // Deduplicate importables, preferring shorter paths.
    // This is the reason for searching in the crates before the current module - to prioritize
    // shorter, canonical paths prefixed with `crate::` over paths using `super::` or local
    // imports.
    let mut result: OrderedHashMap<ImportableId<'_>, String> = OrderedHashMap::default();
    for (trait_id, path) in module_visible_importables {
        match result.entry(trait_id) {
            Entry::Occupied(existing_path) => {
                if path.split("::").count() < existing_path.get().split("::").count() {
                    *existing_path.into_mut() = path;
                }
            }
            Entry::Vacant(entry) => {
                entry.insert(path);
            }
        }
    }
    Some(Arc::new(result))
}

/// Query implementation of [LspHelpers::visible_importables_from_module].
pub fn visible_importables_from_module_tracked<'db>(
    db: &'db dyn Database,
    module_id: ModuleId<'db>,
) -> Option<Arc<OrderedHashMap<ImportableId<'db>, String>>> {
    visible_importables_from_module_helper(db, (), module_id)
}

#[salsa::tracked]
fn visible_importables_from_module_helper<'db>(
    db: &'db dyn Database,
    _tracked: Tracked,
    module_id: ModuleId<'db>,
) -> Option<Arc<OrderedHashMap<ImportableId<'db>, String>>> {
    visible_importables_from_module(db, module_id)
}

/// Implementation of [LspHelpers::visible_traits_from_module].
pub fn visible_traits_from_module<'db>(
    db: &'db dyn Database,
    module_id: ModuleId<'db>,
) -> Option<Arc<OrderedHashMap<TraitId<'db>, String>>> {
    let importables = db.visible_importables_from_module(module_id)?;

    let traits = importables
        .iter()
        .filter_map(|(item, path)| {
            if let ImportableId::Trait(trait_id) = item {
                Some((*trait_id, path.clone()))
            } else {
                None
            }
        })
        .collect::<OrderedHashMap<_, _>>()
        .into();

    Some(traits)
}

/// Query implementation of [LspHelpers::visible_traits_from_module].
fn visible_traits_from_module_tracked<'db>(
    db: &'db dyn Database,
    module_id: ModuleId<'db>,
) -> Option<Arc<OrderedHashMap<TraitId<'db>, String>>> {
    visible_traits_from_module_helper(db, (), module_id)
}

#[salsa::tracked]
fn visible_traits_from_module_helper<'db>(
    db: &'db dyn Database,
    _tracked: Tracked,
    module_id: ModuleId<'db>,
) -> Option<Arc<OrderedHashMap<TraitId<'db>, String>>> {
    visible_traits_from_module(db, module_id)
}

/// Query implementation of [LspHelpers::inline_macro_expansion_files].
#[salsa::tracked(returns(ref))]
fn inline_macro_expansion_files_tracked<'db>(
    db: &'db dyn Database,
    _tracked: Tracked,
    module_id: ModuleId<'db>,
) -> Vec<FileId<'db>> {
    let mut files = vec![];
    if let Ok(module_macro_calls_ids) = db.module_macro_calls_ids(module_id) {
        for macro_call in module_macro_calls_ids {
            if let Ok(macro_call_data) = db.priv_macro_call_data(*macro_call)
                && let Ok(ModuleId::MacroCall { generated_file_id, .. }) =
                    macro_call_data.macro_call_module
            {
                files.push(generated_file_id);
            }
        }
    }
    if let Ok(traits) = db.module_traits_ids(module_id) {
        for trait_id in traits {
            if let Ok(trait_functions) = db.trait_functions(*trait_id) {
                for trait_function_id in trait_functions.values() {
                    if let Ok(Some(data)) = db.trait_function_body_resolver_data(*trait_function_id)
                    {
                        files.extend(data.files.iter().copied())
                    }
                }
            }
        }
    }
    if let Ok(impls) = db.module_impls_ids(module_id) {
        for impls_id in impls {
            if let Ok(impl_functions) = db.impl_functions(*impls_id) {
                for impl_function_id in impl_functions.values() {
                    if let Ok(data) = db.impl_function_body_resolver_data(*impl_function_id) {
                        files.extend(data.files.iter().copied())
                    }
                }
            }
        }
    }
    if let Ok(free_functions) = db.module_free_functions_ids(module_id) {
        for free_function_id in free_functions {
            if let Ok(data) = db.free_function_body_resolver_data(*free_function_id) {
                files.extend(data.files.iter().copied())
            }
        }
    }
    if let Ok(constants) = db.module_constants_ids(module_id) {
        for constant_id in constants {
            if let Ok(data) = db.constant_resolver_data(*constant_id) {
                files.extend(data.files.iter().copied())
            }
        }
    }
    if let Ok(type_aliases) = db.module_type_aliases_ids(module_id) {
        for type_alias_id in type_aliases {
            if let Ok(data) = db.module_type_alias_resolver_data(*type_alias_id) {
                files.extend(data.files.iter().copied())
            }
        }
    }

    files
}

#[salsa::tracked]
fn find_module_containing_node<'db>(
    db: &'db dyn Database,
    _tracked: Tracked,
    node: SyntaxNode<'db>,
) -> Option<ModuleId<'db>> {
    // Get the main module of the main file containing the node.
    // The node may be located in a virtual file of a submodule.
    let main_module = {
        // Get the file where the node is located.
        // This might be a virtual file generated by a compiler plugin.
        let node_file_id = node.stable_ptr(db).file_id(db);

        // Get the root module of a file containing the node.
        *db.file_modules(node_file_id).ok()?.first()?
    };

    // Get the stack (bottom-up) of submodule names in the file containing the node, in the main
    // module, that lead to the node.
    node.ancestors(db)
        .filter_map(|node| ItemModule::cast(db, node))
        .map(|item_module| {
            item_module
                .stable_ptr(db)
                .name_green(db)
                .identifier(db)
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

/// Trait for LSP helpers.
pub trait LspHelpers<'db>: Database {
    /// Returns all methods in a module that match the given type filter.
    fn methods_in_module(
        &'db self,
        module_id: ModuleId<'db>,
        type_filter: TypeFilter<'db>,
    ) -> Arc<Vec<TraitFunctionId<'db>>> {
        methods_in_module_tracked(self.as_dyn_database(), module_id, type_filter)
    }
    /// Returns all methods in a crate that match the given type filter.
    fn methods_in_crate(
        &'db self,
        crate_id: CrateId<'db>,
        type_filter: TypeFilter<'db>,
    ) -> Arc<Vec<TraitFunctionId<'db>>> {
        methods_in_crate_tracked(self.as_dyn_database(), crate_id, type_filter)
    }
    /// Returns all the importables visible from a module, alongside a visible use path to the
    /// trait.
    fn visible_importables_from_module(
        &'db self,
        module_id: ModuleId<'db>,
    ) -> Option<Arc<OrderedHashMap<ImportableId<'db>, String>>> {
        visible_importables_from_module_tracked(self.as_dyn_database(), module_id)
    }
    /// Returns all visible importables in a module, alongside a visible use path to the trait.
    /// `user_module_id` is the module from which the importables should be visible. If
    /// `include_parent` is true, the parent module of `module_id` is also considered.
    fn visible_importables_in_module(
        &'db self,
        module_id: ModuleId<'db>,
        user_module_id: ModuleId<'db>,
        include_parent: bool,
    ) -> Arc<Vec<(ImportableId<'db>, String)>> {
        visible_importables_in_module_tracked(
            self.as_dyn_database(),
            module_id,
            user_module_id,
            include_parent,
        )
    }
    /// Returns all visible importables in a crate, alongside a visible use path to the trait.
    /// `user_module_id` is the module from which the importables should be visible.
    fn visible_importables_in_crate(
        &'db self,
        crate_id: CrateId<'db>,
        user_module_id: ModuleId<'db>,
    ) -> Arc<Vec<(ImportableId<'db>, String)>> {
        visible_importables_in_crate_tracked(self.as_dyn_database(), crate_id, user_module_id)
    }
    /// Returns all the traits visible from a module, alongside a visible use path to the trait.
    fn visible_traits_from_module(
        &'db self,
        module_id: ModuleId<'db>,
    ) -> Option<Arc<OrderedHashMap<TraitId<'db>, String>>> {
        visible_traits_from_module_tracked(self.as_dyn_database(), module_id)
    }
    /// Returns all files generated by inline macro expansion inside provided module.
    fn inline_macro_expansion_files(&'db self, module_id: ModuleId<'db>) -> &'db Vec<FileId<'db>> {
        inline_macro_expansion_files_tracked(self.as_dyn_database(), (), module_id)
    }

    /// Finds a [`ModuleId`] containing the node.
    fn find_module_containing_node(&'db self, node: SyntaxNode<'db>) -> Option<ModuleId<'db>> {
        find_module_containing_node(self.as_dyn_database(), (), node)
    }
}
impl<'db, T: Database + ?Sized> LspHelpers<'db> for T {}
