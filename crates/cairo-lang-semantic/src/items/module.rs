use std::ops::Deref;
use std::sync::Arc;

use cairo_lang_defs::diagnostic_utils::StableLocation;
use cairo_lang_defs::ids::{LanguageElementId, ModuleId, ModuleItemId, TraitId};
use cairo_lang_diagnostics::{Diagnostics, DiagnosticsBuilder, Maybe};
use cairo_lang_syntax::attribute::structured::{Attribute, AttributeListStructurize};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use smol_str::SmolStr;

use super::us::SemanticUseEx;
use super::visibility::Visibility;
use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnosticKind;
use crate::resolve::ResolvedGenericItem;
use crate::SemanticDiagnostic;

/// Information per item in a module.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ItemInfo {
    pub item_id: ModuleItemId,
    pub visibility: Visibility,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ModuleSemanticData {
    // The items in the module without duplicates.
    pub items: OrderedHashMap<SmolStr, ItemInfo>,
    pub diagnostics: Diagnostics<SemanticDiagnostic>,
}

pub fn priv_module_semantic_data(
    db: &dyn SemanticGroup,
    module_id: ModuleId,
) -> Maybe<Arc<ModuleSemanticData>> {
    let def_db = db.upcast();
    // We use the builder here since the items can come from different file_ids.
    let mut diagnostics = DiagnosticsBuilder::default();
    let mut items = OrderedHashMap::default();
    // let defs_info = def_db.module_constants(module_id)
    for item_id in db.module_items(module_id)?.iter().copied() {
        let (name, visibility) = match item_id {
            ModuleItemId::Constant(item_id) => {
                (item_id.name(def_db), def_db.constant_visibility(item_id))
            }
            ModuleItemId::Submodule(item_id) => {
                (item_id.name(def_db), def_db.module_visibility(item_id))
            }
            ModuleItemId::Use(item_id) => (item_id.name(def_db), def_db.use_visibility(item_id)),
            ModuleItemId::FreeFunction(item_id) => {
                (item_id.name(def_db), def_db.free_function_visibility(item_id))
            }
            ModuleItemId::Struct(item_id) => {
                (item_id.name(def_db), def_db.struct_visibility(item_id))
            }
            ModuleItemId::Enum(item_id) => (item_id.name(def_db), def_db.enum_visibility(item_id)),
            ModuleItemId::TypeAlias(item_id) => {
                (item_id.name(def_db), def_db.type_alias_visibility(item_id))
            }
            ModuleItemId::ImplAlias(item_id) => {
                (item_id.name(def_db), def_db.impl_alias_visibility(item_id))
            }
            ModuleItemId::Trait(item_id) => {
                (item_id.name(def_db), def_db.trait_visibility(item_id))
            }
            ModuleItemId::Impl(item_id) => {
                (item_id.name(def_db), def_db.impl_def_visibility(item_id))
            }
            ModuleItemId::ExternType(item_id) => {
                (item_id.name(def_db), def_db.extern_type_visibility(item_id))
            }
            ModuleItemId::ExternFunction(item_id) => {
                (item_id.name(def_db), def_db.extern_function_visibility(item_id))
            }
        };
        // Defaulting to pub as if diagnostics are added privacy diagnostics are less interesting.
        let visibility = visibility
            .map(|v| Visibility::from_ast(db.upcast(), &mut diagnostics, &v))
            .unwrap_or(Visibility::Public);
        if items.insert(name.clone(), ItemInfo { item_id, visibility }).is_some() {
            // `item` is extracted from `module_items` and thus `module_item_name_stable_ptr` is
            // guaranteed to succeed.
            let stable_location =
                StableLocation::new(db.module_item_name_stable_ptr(module_id, item_id).unwrap());
            let kind = SemanticDiagnosticKind::NameDefinedMultipleTimes { name: name.clone() };
            diagnostics.add(SemanticDiagnostic::new(stable_location, kind));
        }
    }
    Ok(Arc::new(ModuleSemanticData { items, diagnostics: diagnostics.build() }))
}

pub fn module_item_by_name(
    db: &dyn SemanticGroup,
    module_id: ModuleId,
    name: SmolStr,
) -> Maybe<Option<ModuleItemId>> {
    let module_data = db.priv_module_semantic_data(module_id)?;
    Ok(module_data.items.get(&name).map(|info| info.item_id))
}

/// Query implementation of [SemanticGroup::module_attributes].
pub fn module_attributes(db: &dyn SemanticGroup, module_id: ModuleId) -> Maybe<Vec<Attribute>> {
    Ok(match module_id {
        ModuleId::CrateRoot(_) => vec![],
        ModuleId::Submodule(submodule_id) => {
            let module_ast =
                &db.module_submodules(submodule_id.parent_module(db.upcast()))?[submodule_id];
            let syntax_db = db.upcast();

            module_ast.attributes(syntax_db).structurize(syntax_db)
        }
    })
}

/// Finds all the trait ids usable in the current context.
pub fn module_usable_trait_ids(
    db: &dyn SemanticGroup,
    module_id: ModuleId,
) -> Maybe<Arc<OrderedHashSet<TraitId>>> {
    let mut module_traits =
        OrderedHashSet::from_iter(db.module_traits_ids(module_id)?.deref().clone());
    // Add traits from impls in the module.
    for imp in db.module_impls_ids(module_id)?.iter().copied() {
        let Ok(trait_id) = db.impl_def_trait(imp) else {
            continue;
        };
        module_traits.insert(trait_id);
    }
    // Add traits from impl aliases in the module.
    for alias in db.module_impl_aliases_ids(module_id)?.iter().copied() {
        let Ok(impl_id) = db.impl_alias_impl_def(alias) else {
            continue;
        };
        let Ok(trait_id) = db.impl_def_trait(impl_id) else {
            continue;
        };
        module_traits.insert(trait_id);
    }
    // Add traits from uses in the module.
    for use_id in db.module_uses_ids(module_id)?.iter().copied() {
        let Ok(resolved_item) = db.use_resolved_item(use_id) else {
            continue;
        };
        match resolved_item {
            // use of a trait.
            ResolvedGenericItem::Trait(trait_id) => {
                module_traits.insert(trait_id);
            }
            // use of an impl from which we get the trait.
            ResolvedGenericItem::Impl(impl_def_id) => {
                let Ok(trait_id) = db.impl_def_trait(impl_def_id) else {
                    continue;
                };
                module_traits.insert(trait_id);
            }
            _ => {}
        };
    }
    Ok(module_traits.into())
}
