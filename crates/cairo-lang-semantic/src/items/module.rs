use std::sync::Arc;

use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::{
    LanguageElementId, LookupItemId, ModuleId, ModuleItemId, NamedLanguageElementId, TraitId,
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
use super::visibility::Visibility;
use crate::db::{get_resolver_data_options, SemanticGroup};
use crate::diagnostic::{SemanticDiagnosticKind, SemanticDiagnosticsBuilder};
use crate::resolve::ResolvedGenericItem;
use crate::SemanticDiagnostic;

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

pub fn module_item_info_by_name(
    db: &dyn SemanticGroup,
    module_id: ModuleId,
    name: SmolStr,
) -> Maybe<Option<ModuleItemInfo>> {
    let module_data = db.priv_module_semantic_data(module_id)?;
    Ok(module_data.items.get(&name).cloned())
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

/// Finds all the trait ids usable in the current context.
pub fn module_usable_trait_ids(
    db: &dyn SemanticGroup,
    module_id: ModuleId,
) -> Maybe<Arc<OrderedHashMap<TraitId, LookupItemId>>> {
    let mut module_traits = OrderedHashMap::from_iter(
        db.module_traits_ids(module_id)?
            .iter()
            .map(|traid_id| (*traid_id, LookupItemId::ModuleItem(ModuleItemId::Trait(*traid_id)))),
    );
    // Add traits from impls in the module.
    for imp in db.module_impls_ids(module_id)?.iter().copied() {
        let Ok(trait_id) = db.impl_def_trait(imp) else {
            continue;
        };
        module_traits.entry(trait_id).or_insert(LookupItemId::ModuleItem(ModuleItemId::Impl(imp)));
    }
    // Add traits from impl aliases in the module.
    for alias in db.module_impl_aliases_ids(module_id)?.iter().copied() {
        let Ok(impl_id) = db.impl_alias_impl_def(alias) else {
            continue;
        };
        let Ok(trait_id) = db.impl_def_trait(impl_id) else {
            continue;
        };
        module_traits
            .entry(trait_id)
            .or_insert(LookupItemId::ModuleItem(ModuleItemId::ImplAlias(alias)));
    }
    // Add traits from uses in the module.
    for use_id in db.module_uses_ids(module_id)?.iter().copied() {
        let Ok(resolved_item) = db.use_resolved_item(use_id) else {
            continue;
        };
        match resolved_item {
            // use of a trait.
            ResolvedGenericItem::Trait(trait_id) => {
                module_traits.insert(trait_id, LookupItemId::ModuleItem(ModuleItemId::Use(use_id)));
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
    Ok(module_traits.into())
}
