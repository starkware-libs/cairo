use std::sync::Arc;

use cairo_lang_defs::ids::{
    FileIndex, LanguageElementId, ModuleFileId, ModuleId, NamedLanguageElementId, TraitFunctionId,
    TraitId,
};
use cairo_lang_filesystem::db::CORELIB_CRATE_NAME;
use cairo_lang_filesystem::ids::{CrateId, CrateLongId};
use cairo_lang_utils::Intern;
use cairo_lang_utils::ordered_hash_map::{Entry, OrderedHashMap};
use cairo_lang_utils::unordered_hash_set::UnorderedHashSet;
use itertools::chain;
use smol_str::SmolStr;

use crate::corelib::{self, core_submodule, get_submodule};
use crate::db::SemanticGroup;
use crate::expr::inference::InferenceId;
use crate::items::us::SemanticUseEx;
use crate::items::visibility::peek_visible_in;
use crate::resolve::{ResolvedGenericItem, Resolver};
use crate::types::TypeHead;

/// A filter for types.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum TypeFilter {
    /// No filter is applied.
    NoFilter,
    /// Only methods with the given type head are returned.
    TypeHead(TypeHead),
}

/// Query implementation of [crate::db::SemanticGroup::methods_in_module].
pub fn methods_in_module(
    db: &dyn SemanticGroup,
    module_id: ModuleId,
    type_filter: TypeFilter,
) -> Arc<[TraitFunctionId]> {
    let mut result = Vec::new();
    let Ok(module_traits_ids) = db.module_traits_ids(module_id) else {
        return result.into();
    };
    for trait_id in module_traits_ids.iter().copied() {
        for (_, trait_function) in db.trait_functions(trait_id).unwrap_or_default() {
            let Ok(signature) = db.trait_function_signature(trait_function) else {
                continue;
            };
            let Some(first_param) = signature.params.first() else {
                continue;
            };
            if first_param.name != "self" {
                continue;
            }
            if let TypeFilter::TypeHead(type_head) = &type_filter {
                if let Some(head) = first_param.ty.head(db) {
                    if !fit_for_method(&head, type_head) {
                        continue;
                    }
                }
            }

            result.push(trait_function)
        }
    }
    result.into()
}

/// Checks if a type head can fit for a method.
fn fit_for_method(head: &TypeHead, type_head: &TypeHead) -> bool {
    if head == type_head {
        return true;
    }
    if let TypeHead::Snapshot(snapshot_head) = head {
        return snapshot_head.as_ref() == type_head;
    }
    false
}

/// Query implementation of [crate::db::SemanticGroup::methods_in_crate].
pub fn methods_in_crate(
    db: &dyn SemanticGroup,
    crate_id: CrateId,
    type_filter: TypeFilter,
) -> Arc<[TraitFunctionId]> {
    let mut result = Vec::new();
    for module_id in db.crate_modules(crate_id).iter() {
        result.extend_from_slice(&db.methods_in_module(*module_id, type_filter.clone())[..])
    }
    result.into()
}

/// Query implementation of [crate::db::SemanticGroup::visible_traits_in_module].
pub fn visible_traits_in_module(
    db: &dyn SemanticGroup,
    module_id: ModuleId,
    user_module_file_id: ModuleFileId,
    include_parent: bool,
) -> Arc<[(TraitId, String)]> {
    let mut visited_modules = UnorderedHashSet::default();
    visible_traits_in_module_ex(
        db,
        module_id,
        user_module_file_id,
        include_parent,
        &mut visited_modules,
    )
    .unwrap_or_else(|| Vec::new().into())
}

/// Returns the visible traits in a module, including the traits in the parent module if needed.
/// The visibility is relative to the module `user_module_id`.
fn visible_traits_in_module_ex(
    db: &dyn SemanticGroup,
    module_id: ModuleId,
    user_module_file_id: ModuleFileId,
    include_parent: bool,
    visited_modules: &mut UnorderedHashSet<ModuleId>,
) -> Option<Arc<[(TraitId, String)]>> {
    let mut result = Vec::new();
    if visited_modules.contains(&module_id) {
        return Some(result.into());
    }

    let resolver = Resolver::new(db, user_module_file_id, InferenceId::NoContext);
    let ignore_visibility = resolver.ignore_visibility_checks(module_id);
    // Check if an item in the current module is visible from the user module.
    let is_visible = |item_name: SmolStr| {
        if ignore_visibility {
            Some(true)
        } else {
            let item_info = db.module_item_info_by_name(module_id, item_name).ok()??;
            Some(peek_visible_in(
                db.upcast(),
                item_info.visibility,
                module_id,
                user_module_file_id.0,
            ))
        }
    };
    visited_modules.insert(module_id);
    let mut modules_to_visit = vec![];
    // Add traits and traverse modules imported into the current module.
    for use_id in db.module_uses_ids(module_id).ok()?.iter().copied() {
        if !is_visible(use_id.name(db.upcast()))? {
            continue;
        }
        let resolved_item = db.use_resolved_item(use_id).ok()?;
        match resolved_item {
            ResolvedGenericItem::Module(inner_module_id) => {
                modules_to_visit.push(inner_module_id);
            }
            ResolvedGenericItem::Trait(trait_id) => {
                result.push((trait_id, trait_id.name(db.upcast()).to_string()));
            }
            _ => continue,
        }
    }
    for submodule_id in db.module_submodules_ids(module_id).ok()?.iter().copied() {
        if !is_visible(submodule_id.name(db.upcast()))? {
            continue;
        }
        modules_to_visit.push(ModuleId::Submodule(submodule_id));
    }
    for trait_id in db.module_traits_ids(module_id).ok()?.iter().copied() {
        if !is_visible(trait_id.name(db.upcast()))? {
            continue;
        }
        result.push((trait_id, trait_id.name(db.upcast()).to_string()));
    }

    for submodule in modules_to_visit {
        for (trait_id, path) in
            visible_traits_in_module_ex(db, submodule, user_module_file_id, false, visited_modules)?
                .iter()
        {
            result.push((*trait_id, format!("{}::{}", submodule.name(db.upcast()), path)));
        }
    }
    // Traverse the parent module if needed.
    if include_parent {
        match module_id {
            ModuleId::CrateRoot(_) => {}
            ModuleId::Submodule(submodule_id) => {
                let parent_module_id = submodule_id.parent_module(db.upcast());
                for (trait_id, path) in visible_traits_in_module_ex(
                    db,
                    parent_module_id,
                    user_module_file_id,
                    include_parent,
                    visited_modules,
                )?
                .iter()
                {
                    result.push((*trait_id, format!("super::{}", path)));
                }
            }
        }
    }
    Some(result.into())
}

/// Query implementation of [crate::db::SemanticGroup::visible_traits_in_crate].
pub fn visible_traits_in_crate(
    db: &dyn SemanticGroup,
    crate_id: CrateId,
    user_module_file_id: ModuleFileId,
) -> Arc<[(TraitId, String)]> {
    let is_current_crate = user_module_file_id.0.owning_crate(db.upcast()) == crate_id;
    let crate_name = if is_current_crate { "crate" } else { &crate_id.name(db.upcast()) };
    let crate_as_module = ModuleId::CrateRoot(crate_id);
    db.visible_traits_in_module(crate_as_module, user_module_file_id, false)
        .iter()
        .cloned()
        .map(|(trait_id, path)| (trait_id, format!("{crate_name}::{path}",)))
        .collect::<Vec<_>>()
        .into()
}

/// Query implementation of [crate::db::SemanticGroup::visible_traits_from_module].
pub fn visible_traits_from_module(
    db: &dyn SemanticGroup,
    module_file_id: ModuleFileId,
) -> Option<Arc<OrderedHashMap<TraitId, String>>> {
    let module_id = module_file_id.0;
    let mut current_top_module = module_id;
    while let ModuleId::Submodule(submodule_id) = current_top_module {
        current_top_module = submodule_id.parent_module(db.upcast());
    }
    let current_crate_id = match current_top_module {
        ModuleId::CrateRoot(crate_id) => crate_id,
        ModuleId::Submodule(_) => unreachable!("current module is not a top-level module"),
    };
    let edition = db.crate_config(current_crate_id)?.settings.edition;
    let prelude_submodule_name = edition.prelude_submodule_name();
    let core_prelude_submodule = core_submodule(db, "prelude");
    let prelude_submodule = get_submodule(db, core_prelude_submodule, prelude_submodule_name)?;
    let prelude_submodule_file_id = ModuleFileId(prelude_submodule, FileIndex(0));

    let mut module_visible_traits = Vec::new();
    // Collect traits from the prelude.
    module_visible_traits.extend_from_slice(
        &db.visible_traits_in_module(prelude_submodule, prelude_submodule_file_id, false)[..],
    );
    // Collect traits from all visible crates, including the current crate.
    let settings = db.crate_config(current_crate_id).map(|c| c.settings).unwrap_or_default();
    for crate_id in chain!(
        [current_crate_id],
        (!settings.dependencies.contains_key(CORELIB_CRATE_NAME)).then(|| corelib::core_crate(db)),
        settings.dependencies.iter().map(|(name, setting)| {
            CrateLongId::Real {
                name: name.clone().into(),
                discriminator: setting.discriminator.clone(),
            }
            .intern(db)
        })
    ) {
        module_visible_traits
            .extend_from_slice(&db.visible_traits_in_crate(crate_id, module_file_id)[..]);
    }

    // Collect traits visible in the current module.
    module_visible_traits
        .extend_from_slice(&db.visible_traits_in_module(module_id, module_file_id, true)[..]);

    // Deduplicate traits, preferring shorter paths.
    // This is the reason for searching in the crates before the current module- to prioritize
    // shorter, canonical paths prefixed with `crate::` over paths using `super::` or local
    // imports.
    let mut result: OrderedHashMap<TraitId, String> = OrderedHashMap::default();
    for (trait_id, path) in module_visible_traits {
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
    Some(result.into())
}
