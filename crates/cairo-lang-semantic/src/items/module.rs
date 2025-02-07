use std::sync::Arc;

use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::{
    GlobalUseId, LanguageElementId, LookupItemId, ModuleId, ModuleItemId, NamedLanguageElementId,
    TraitId,
};
use cairo_lang_diagnostics::{Diagnostics, DiagnosticsBuilder, Maybe};
use cairo_lang_syntax::attribute::structured::{Attribute, AttributeListStructurize};
use cairo_lang_syntax::node::ast;
use cairo_lang_syntax::node::helpers::UsePathEx;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use smol_str::SmolStr;

use super::feature_kind::FeatureKind;
use super::us::SemanticUseEx;
use super::visibility::{Visibility, peek_visible_in};
use crate::SemanticDiagnostic;
use crate::db::{SemanticGroup, get_resolver_data_options};
use crate::diagnostic::{SemanticDiagnosticKind, SemanticDiagnosticsBuilder};
use crate::items::feature_kind::HasFeatureKind;
use crate::resolve::ResolvedGenericItem;

/// Information per item in a module.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ModuleItemInfo {
    pub item_id: ModuleItemId,
    pub visibility: Visibility,
    pub feature_kind: FeatureKind,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ModuleSemanticData {
    /// The items in the module without duplicates.
    pub items: OrderedHashMap<SmolStr, ModuleItemInfo>,
    pub global_uses: OrderedHashMap<GlobalUseId, Visibility>,
    pub diagnostics: Diagnostics<SemanticDiagnostic>,
}

pub fn priv_module_semantic_data(
    db: &dyn SemanticGroup,
    module_id: ModuleId,
) -> Maybe<Arc<ModuleSemanticData>> {
    let def_db: &dyn DefsGroup = db.upcast();
    let syntax_db = db.upcast();
    // We use the builder here since the items can come from different file_ids.
    let mut diagnostics = DiagnosticsBuilder::default();
    let mut items = OrderedHashMap::default();
    for item_id in db.module_items(module_id)?.iter().copied() {
        let (name, attributes, visibility) = match &item_id {
            ModuleItemId::Constant(item_id) => {
                let item = &def_db.module_constants(module_id)?[item_id];
                (item_id.name(def_db), item.attributes(syntax_db), item.visibility(syntax_db))
            }
            ModuleItemId::Submodule(item_id) => {
                let item = &def_db.module_submodules(module_id)?[item_id];
                (item_id.name(def_db), item.attributes(syntax_db), item.visibility(syntax_db))
            }
            ModuleItemId::Use(item_id) => {
                let use_ast = &def_db.module_uses(module_id)?[item_id];
                let item = ast::UsePath::Leaf(use_ast.clone()).get_item(syntax_db);
                (item_id.name(def_db), item.attributes(syntax_db), item.visibility(syntax_db))
            }
            ModuleItemId::FreeFunction(item_id) => {
                let item = &def_db.module_free_functions(module_id)?[item_id];
                (item_id.name(def_db), item.attributes(syntax_db), item.visibility(syntax_db))
            }
            ModuleItemId::Struct(item_id) => {
                let item = &def_db.module_structs(module_id)?[item_id];
                (item_id.name(def_db), item.attributes(syntax_db), item.visibility(syntax_db))
            }
            ModuleItemId::Enum(item_id) => {
                let item = &def_db.module_enums(module_id)?[item_id];
                (item_id.name(def_db), item.attributes(syntax_db), item.visibility(syntax_db))
            }
            ModuleItemId::TypeAlias(item_id) => {
                let item = &def_db.module_type_aliases(module_id)?[item_id];
                (item_id.name(def_db), item.attributes(syntax_db), item.visibility(syntax_db))
            }
            ModuleItemId::ImplAlias(item_id) => {
                let item = &def_db.module_impl_aliases(module_id)?[item_id];
                (item_id.name(def_db), item.attributes(syntax_db), item.visibility(syntax_db))
            }
            ModuleItemId::Trait(item_id) => {
                let item = &def_db.module_traits(module_id)?[item_id];
                (item_id.name(def_db), item.attributes(syntax_db), item.visibility(syntax_db))
            }
            ModuleItemId::Impl(item_id) => {
                let item = &def_db.module_impls(module_id)?[item_id];
                (item_id.name(def_db), item.attributes(syntax_db), item.visibility(syntax_db))
            }
            ModuleItemId::ExternType(item_id) => {
                let item = &def_db.module_extern_types(module_id)?[item_id];
                (item_id.name(def_db), item.attributes(syntax_db), item.visibility(syntax_db))
            }
            ModuleItemId::ExternFunction(item_id) => {
                let item = &def_db.module_extern_functions(module_id)?[item_id];
                (item_id.name(def_db), item.attributes(syntax_db), item.visibility(syntax_db))
            }
        };
        let visibility = Visibility::from_ast(db.upcast(), &mut diagnostics, &visibility);
        let feature_kind = FeatureKind::from_ast(db.upcast(), &mut diagnostics, &attributes);
        if items
            .insert(name.clone(), ModuleItemInfo { item_id, visibility, feature_kind })
            .is_some()
        {
            // `item` is extracted from `module_items` and thus `module_item_name_stable_ptr` is
            // guaranteed to succeed.
            diagnostics.report(
                db.module_item_name_stable_ptr(module_id, item_id).unwrap(),
                SemanticDiagnosticKind::NameDefinedMultipleTimes(name.clone()),
            );
        }
    }

    let global_uses = db
        .module_global_uses(module_id)?
        .iter()
        .map(|(global_use_id, use_path_star)| {
            let item = ast::UsePath::Star(use_path_star.clone()).get_item(syntax_db);
            let visibility = item.visibility(syntax_db);
            (*global_use_id, Visibility::from_ast(db.upcast(), &mut diagnostics, &visibility))
        })
        .collect();
    Ok(Arc::new(ModuleSemanticData { items, global_uses, diagnostics: diagnostics.build() }))
}

pub fn module_item_by_name(
    db: &dyn SemanticGroup,
    module_id: ModuleId,
    name: SmolStr,
) -> Maybe<Option<ModuleItemId>> {
    let module_data = db.priv_module_semantic_data(module_id)?;
    Ok(module_data.items.get(&name).map(|info| info.item_id))
}

pub fn module_item_info_by_name(
    db: &dyn SemanticGroup,
    module_id: ModuleId,
    name: SmolStr,
) -> Maybe<Option<ModuleItemInfo>> {
    let module_data = db.priv_module_semantic_data(module_id)?;
    Ok(module_data.items.get(&name).cloned())
}

/// Get the imported global uses of a module, and their visibility.
pub fn get_module_global_uses(
    db: &dyn SemanticGroup,
    module_id: ModuleId,
) -> Maybe<OrderedHashMap<GlobalUseId, Visibility>> {
    let module_data = db.priv_module_semantic_data(module_id)?;
    Ok(module_data.global_uses.clone())
}

/// Query implementation of [SemanticGroup::module_all_used_items].
pub fn module_all_used_items(
    db: &dyn SemanticGroup,
    module_id: ModuleId,
) -> Maybe<Arc<OrderedHashSet<LookupItemId>>> {
    let mut all_used_items = OrderedHashSet::default();
    let module_items = db.module_items(module_id)?;
    for item in module_items.iter() {
        if let Some(items) = match *item {
            ModuleItemId::Submodule(submodule_id) => {
                Some(db.module_all_used_items(ModuleId::Submodule(submodule_id))?)
            }
            ModuleItemId::Trait(trait_id) => Some(db.trait_all_used_items(trait_id)?),
            ModuleItemId::Impl(impl_id) => Some(db.impl_all_used_items(impl_id)?),
            _ => None,
        } {
            all_used_items.extend(items.iter().cloned());
        } else {
            for resolver_data in get_resolver_data_options(LookupItemId::ModuleItem(*item), db) {
                all_used_items.extend(resolver_data.used_items.iter().cloned());
            }
        }
    }
    Ok(all_used_items.into())
}

/// Query implementation of [SemanticGroup::module_attributes].
pub fn module_attributes(db: &dyn SemanticGroup, module_id: ModuleId) -> Maybe<Vec<Attribute>> {
    Ok(match &module_id {
        ModuleId::CrateRoot(_) => vec![],
        ModuleId::Submodule(submodule_id) => {
            let module_ast =
                &db.module_submodules(submodule_id.parent_module(db.upcast()))?[submodule_id];
            let syntax_db = db.upcast();

            module_ast.attributes(syntax_db).structurize(syntax_db)
        }
    })
}

/// Finds all the trait ids usable in the current context, using `global use` imports.
pub fn module_usable_trait_ids(
    db: &dyn SemanticGroup,
    module_id: ModuleId,
) -> Maybe<Arc<OrderedHashMap<TraitId, LookupItemId>>> {
    // Get the traits first from the module, do not change this order.
    let mut module_traits = specific_module_usable_trait_ids(db, module_id, module_id)?;
    for (user_module, containing_module) in &db.priv_module_use_star_modules(module_id).accessible {
        if let Ok(star_module_traits) =
            specific_module_usable_trait_ids(db, *user_module, *containing_module)
        {
            for (trait_id, local_item_id) in star_module_traits {
                module_traits.entry(trait_id).or_insert(local_item_id);
            }
        }
    }
    Ok(module_traits.into())
}

/// Finds all the trait ids usable in the current context, not using `global use` imports.
fn specific_module_usable_trait_ids(
    db: &dyn SemanticGroup,
    user_module: ModuleId,
    containing_module: ModuleId,
) -> Maybe<OrderedHashMap<TraitId, LookupItemId>> {
    let mut module_traits: OrderedHashMap<TraitId, LookupItemId> = OrderedHashMap::default();
    for item in db.priv_module_semantic_data(containing_module)?.items.values() {
        if !matches!(
            item.item_id,
            ModuleItemId::Trait(_)
                | ModuleItemId::Impl(_)
                | ModuleItemId::ImplAlias(_)
                | ModuleItemId::Use(_)
        ) {
            continue;
        }
        if !peek_visible_in(db.upcast(), item.visibility, containing_module, user_module) {
            continue;
        }
        match item.item_id {
            ModuleItemId::Trait(trait_id) => {
                module_traits
                    .insert(trait_id, LookupItemId::ModuleItem(ModuleItemId::Trait(trait_id)));
            }
            ModuleItemId::Impl(impl_def_id) => {
                // Add traits from impls in the module.
                let Ok(trait_id) = db.impl_def_trait(impl_def_id) else {
                    continue;
                };
                module_traits
                    .entry(trait_id)
                    .or_insert(LookupItemId::ModuleItem(ModuleItemId::Impl(impl_def_id)));
            }
            ModuleItemId::ImplAlias(impl_alias_id) => {
                // Add traits from impl aliases in the module.
                let Ok(impl_id) = db.impl_alias_impl_def(impl_alias_id) else {
                    continue;
                };
                let Ok(trait_id) = db.impl_def_trait(impl_id) else {
                    continue;
                };
                module_traits
                    .entry(trait_id)
                    .or_insert(LookupItemId::ModuleItem(ModuleItemId::ImplAlias(impl_alias_id)));
            }
            ModuleItemId::Use(use_id) => {
                // Add traits from uses in the module.
                let Ok(resolved_item) = db.use_resolved_item(use_id) else {
                    continue;
                };
                match resolved_item {
                    // use of a trait.
                    ResolvedGenericItem::Trait(trait_id) => {
                        module_traits
                            .insert(trait_id, LookupItemId::ModuleItem(ModuleItemId::Use(use_id)));
                    }
                    // use of an impl from which we get the trait.
                    ResolvedGenericItem::Impl(impl_def_id) => {
                        if let Ok(trait_id) = db.impl_def_trait(impl_def_id) {
                            module_traits
                                .entry(trait_id)
                                .or_insert(LookupItemId::ModuleItem(ModuleItemId::Use(use_id)));
                        };
                    }
                    _ => {}
                }
            }
            _ => {}
        }
    }
    Ok(module_traits)
}

impl HasFeatureKind for ModuleItemInfo {
    fn feature_kind(&self) -> &FeatureKind {
        &self.feature_kind
    }
}
