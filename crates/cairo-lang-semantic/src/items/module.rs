use std::sync::Arc;

use cairo_lang_defs::ids::{
    GlobalUseId, LanguageElementId, LookupItemId, ModuleId, ModuleItemId, NamedLanguageElementId,
    TraitId, UseId,
};
use cairo_lang_diagnostics::{Diagnostics, DiagnosticsBuilder, Maybe};
use cairo_lang_filesystem::ids::StrRef;
use cairo_lang_syntax::attribute::structured::{Attribute, AttributeListStructurize};
use cairo_lang_syntax::node::ast;
use cairo_lang_syntax::node::helpers::UsePathEx;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;

use super::feature_kind::FeatureKind;
use super::us::SemanticUseEx;
use super::visibility::{Visibility, peek_visible_in};
use crate::SemanticDiagnostic;
use crate::db::{SemanticGroup, get_resolver_data_options};
use crate::diagnostic::{SemanticDiagnosticKind, SemanticDiagnosticsBuilder};
use crate::items::feature_kind::HasFeatureKind;
use crate::resolve::ResolvedGenericItem;

/// Information per item in a module.
#[derive(Clone, Debug, PartialEq, Eq, salsa::Update)]
pub struct ModuleItemInfo<'db> {
    pub item_id: ModuleItemId<'db>,
    pub visibility: Visibility,
    pub feature_kind: FeatureKind<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq, salsa::Update)]
pub struct ModuleSemanticData<'db> {
    /// The items in the module without duplicates.
    pub items: OrderedHashMap<StrRef<'db>, ModuleItemInfo<'db>>,
    pub global_uses: OrderedHashMap<GlobalUseId<'db>, Visibility>,
    pub diagnostics: Diagnostics<'db, SemanticDiagnostic<'db>>,
}

pub fn priv_module_semantic_data<'db>(
    db: &'db dyn SemanticGroup,
    module_id: ModuleId<'db>,
) -> Maybe<Arc<ModuleSemanticData<'db>>> {
    // We use the builder here since the items can come from different file_ids.
    let mut diagnostics = DiagnosticsBuilder::default();
    let mut items = OrderedHashMap::default();
    let module_data = module_id.module_data(db)?;
    for item_id in module_data.items(db).iter().copied() {
        let (name, attributes, visibility) = match &item_id {
            ModuleItemId::Constant(item_id) => {
                let item = &module_data.constants(db)[item_id];
                (item_id.name(db), item.attributes(db), item.visibility(db))
            }
            ModuleItemId::Submodule(item_id) => {
                let item = &module_data.submodules(db)[item_id];
                (item_id.name(db), item.attributes(db), item.visibility(db))
            }
            ModuleItemId::Use(item_id) => {
                let use_ast = &module_data.uses(db)[item_id];
                let item = ast::UsePath::Leaf(use_ast.clone()).get_item(db);
                (item_id.name(db), item.attributes(db), item.visibility(db))
            }
            ModuleItemId::FreeFunction(item_id) => {
                let item = &module_data.free_functions(db)[item_id];
                (item_id.name(db), item.attributes(db), item.visibility(db))
            }
            ModuleItemId::Struct(item_id) => {
                let item = &module_data.structs(db)[item_id];
                (item_id.name(db), item.attributes(db), item.visibility(db))
            }
            ModuleItemId::Enum(item_id) => {
                let item = &module_data.enums(db)[item_id];
                (item_id.name(db), item.attributes(db), item.visibility(db))
            }
            ModuleItemId::TypeAlias(item_id) => {
                let item = &module_data.type_aliases(db)[item_id];
                (item_id.name(db), item.attributes(db), item.visibility(db))
            }
            ModuleItemId::ImplAlias(item_id) => {
                let item = &module_data.impl_aliases(db)[item_id];
                (item_id.name(db), item.attributes(db), item.visibility(db))
            }
            ModuleItemId::Trait(item_id) => {
                let item = &module_data.traits(db)[item_id];
                (item_id.name(db), item.attributes(db), item.visibility(db))
            }
            ModuleItemId::Impl(item_id) => {
                let item = &module_data.impls(db)[item_id];
                (item_id.name(db), item.attributes(db), item.visibility(db))
            }
            ModuleItemId::ExternType(item_id) => {
                let item = &module_data.extern_types(db)[item_id];
                (item_id.name(db), item.attributes(db), item.visibility(db))
            }
            ModuleItemId::ExternFunction(item_id) => {
                let item = &module_data.extern_functions(db)[item_id];
                (item_id.name(db), item.attributes(db), item.visibility(db))
            }
            ModuleItemId::MacroDeclaration(item_id) => {
                let item = &module_data.macro_declarations(db)[item_id];
                (item_id.name(db), item.attributes(db), item.visibility(db))
            }
        };
        let visibility = Visibility::from_ast(db, &mut diagnostics, &visibility);
        let feature_kind = FeatureKind::from_ast(db, &mut diagnostics, &attributes);
        if items.insert(name.into(), ModuleItemInfo { item_id, visibility, feature_kind }).is_some()
        {
            // `item` is extracted from `module_items` and thus `module_item_name_stable_ptr` is
            // guaranteed to succeed.
            diagnostics.report(
                db.module_item_name_stable_ptr(module_id, item_id).unwrap(),
                SemanticDiagnosticKind::NameDefinedMultipleTimes(name.into()),
            );
        }
    }

    let global_uses = module_data
        .global_uses(db)
        .iter()
        .map(|(global_use_id, use_path_star)| {
            let item = ast::UsePath::Star(use_path_star.clone()).get_item(db);
            let visibility = item.visibility(db);
            (*global_use_id, Visibility::from_ast(db, &mut diagnostics, &visibility))
        })
        .collect();
    Ok(Arc::new(ModuleSemanticData { items, global_uses, diagnostics: diagnostics.build() }))
}

pub fn module_item_by_name<'db>(
    db: &'db dyn SemanticGroup,
    module_id: ModuleId<'db>,
    name: StrRef<'db>,
) -> Maybe<Option<ModuleItemId<'db>>> {
    let module_data = db.priv_module_semantic_data(module_id)?;
    Ok(module_data.items.get(&name).map(|info| info.item_id))
}

pub fn module_item_info_by_name<'db>(
    db: &'db dyn SemanticGroup,
    module_id: ModuleId<'db>,
    name: StrRef<'db>,
) -> Maybe<Option<ModuleItemInfo<'db>>> {
    let module_data = db.priv_module_semantic_data(module_id)?;
    Ok(module_data.items.get(&name).cloned())
}

/// Get the imported global uses of a module, and their visibility.
pub fn get_module_global_uses<'db>(
    db: &'db dyn SemanticGroup,
    module_id: ModuleId<'db>,
) -> Maybe<OrderedHashMap<GlobalUseId<'db>, Visibility>> {
    let module_data = db.priv_module_semantic_data(module_id)?;
    Ok(module_data.global_uses.clone())
}

/// Query implementation of [SemanticGroup::module_all_used_uses].
pub fn module_all_used_uses<'db>(
    db: &'db dyn SemanticGroup,
    module_id: ModuleId<'db>,
) -> Maybe<Arc<OrderedHashSet<UseId<'db>>>> {
    let mut all_used_uses = OrderedHashSet::default();
    let module_items = module_id.module_data(db)?.items(db);
    for item in module_items.iter() {
        if let Some(items) = match *item {
            ModuleItemId::Submodule(submodule_id) => {
                Some(db.module_all_used_uses(ModuleId::Submodule(submodule_id))?)
            }
            ModuleItemId::Trait(trait_id) => Some(db.trait_all_used_uses(trait_id)?),
            ModuleItemId::Impl(impl_id) => Some(db.impl_all_used_uses(impl_id)?),
            _ => None,
        } {
            all_used_uses.extend(items.iter().cloned());
        } else {
            for resolver_data in get_resolver_data_options(LookupItemId::ModuleItem(*item), db) {
                all_used_uses.extend(resolver_data.used_uses.iter().cloned());
            }
        }
    }
    Ok(all_used_uses.into())
}

/// Query implementation of [SemanticGroup::module_attributes].
pub fn module_attributes<'db>(
    db: &'db dyn SemanticGroup,
    module_id: ModuleId<'db>,
) -> Maybe<Vec<Attribute<'db>>> {
    Ok(match &module_id {
        ModuleId::CrateRoot(_) | ModuleId::MacroCall { id: _, generated_file_id: _ } => vec![],
        ModuleId::Submodule(submodule_id) => {
            let module_ast = &submodule_id.module_data(db)?.submodules(db)[submodule_id];

            module_ast.attributes(db).structurize(db)
        }
    })
}

/// Finds all the trait ids usable in the current context, using `global use` imports.
pub fn module_usable_trait_ids<'db>(
    db: &'db dyn SemanticGroup,
    module_id: ModuleId<'db>,
) -> Maybe<Arc<OrderedHashMap<TraitId<'db>, LookupItemId<'db>>>> {
    // Get the traits first from the module, do not change this order.
    let mut module_traits = specific_module_usable_trait_ids(db, module_id, module_id)?;
    for (user_module, containing_module) in &db.module_imported_modules(module_id).accessible {
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

/// Returns a map of all names declared in the given module, including names declared in
/// macro-generated modules (i.e., modules created by macro expansions within this module).
pub fn module_declared_names<'db>(
    db: &'db dyn SemanticGroup,
    module_id: ModuleId<'db>,
) -> OrderedHashMap<StrRef<'db>, ModuleItemId<'db>> {
    let mut names: OrderedHashMap<StrRef<'db>, ModuleItemId<'db>> = OrderedHashMap::default();
    if let Ok(items) = module_id.module_data(db).map(|data| data.items(db)) {
        for item in items.iter() {
            let name: StrRef<'db> = item.name(db).into();
            names.entry(name).or_insert(*item);
        }
    }
    if let Ok(macro_call_ids) = db.module_macro_calls_ids(module_id) {
        for macro_call_id in macro_call_ids.iter() {
            if let Ok(generated_module_id) = db.macro_call_module_id(*macro_call_id) {
                let generated_names = db.module_declared_names(generated_module_id);
                for (name, id) in generated_names {
                    names.entry(name).or_insert(id);
                }
            }
        }
    }
    names
}

// Returns all (name, item_id, module_id) triples declared in the given module and all its
// macro-generated modules.
pub fn module_all_declared_names<'db>(
    db: &'db dyn SemanticGroup,
    module_id: ModuleId<'db>,
) -> Vec<(StrRef<'db>, ModuleItemId<'db>, ModuleId<'db>)> {
    let mut result = Vec::new();
    if let Ok(items) = module_id.module_data(db).map(|data| data.items(db)) {
        for item in items.iter() {
            let name: StrRef<'db> = item.name(db).into();
            result.push((name, *item, module_id));
        }
    }
    if let Ok(macro_call_ids) = db.module_macro_calls_ids(module_id) {
        for macro_call_id in macro_call_ids.iter() {
            if let Ok(generated_module_id) = db.macro_call_module_id(*macro_call_id) {
                result.extend(db.module_all_declared_names(generated_module_id));
            }
        }
    }
    result
}

/// Finds all the trait ids usable in the current context, not using `global use` imports.
fn specific_module_usable_trait_ids<'db>(
    db: &'db dyn SemanticGroup,
    user_module: ModuleId<'db>,
    containing_module: ModuleId<'db>,
) -> Maybe<OrderedHashMap<TraitId<'db>, LookupItemId<'db>>> {
    let mut module_traits: OrderedHashMap<TraitId<'_>, LookupItemId<'_>> =
        OrderedHashMap::default();
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
        if !peek_visible_in(db, item.visibility, containing_module, user_module) {
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

impl<'db> HasFeatureKind<'db> for ModuleItemInfo<'db> {
    fn feature_kind(&self) -> &FeatureKind<'db> {
        &self.feature_kind
    }
}
