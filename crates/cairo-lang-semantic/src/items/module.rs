use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::{
    GlobalUseId, LanguageElementId, LookupItemId, ModuleId, ModuleItemId, NamedLanguageElementId,
    TraitId, UseId,
};
use cairo_lang_diagnostics::{Diagnostics, DiagnosticsBuilder, Maybe, MaybeAsRef};
use cairo_lang_filesystem::ids::{SmolStrId, Tracked};
use cairo_lang_syntax::attribute::structured::{Attribute, AttributeListStructurize};
use cairo_lang_syntax::node::ast;
use cairo_lang_syntax::node::helpers::UsePathEx;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use itertools::chain;
use salsa::Database;

use super::feature_kind::FeatureKind;
use super::us::SemanticUseEx;
use super::visibility::{Visibility, peek_visible_in};
use crate::SemanticDiagnostic;
use crate::db::{SemanticGroup, get_resolver_data_options};
use crate::diagnostic::{SemanticDiagnosticKind, SemanticDiagnosticsBuilder};
use crate::items::feature_kind::HasFeatureKind;
use crate::items::imp::ImplSemantic;
use crate::items::impl_alias::ImplAliasSemantic;
use crate::items::macro_call::module_macro_modules;
use crate::items::trt::TraitSemantic;
use crate::items::us::{ImportInfo, UseSemantic};
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
    pub items: OrderedHashMap<SmolStrId<'db>, ModuleItemInfo<'db>>,
    pub global_uses: OrderedHashMap<GlobalUseId<'db>, Visibility>,
    pub diagnostics: Diagnostics<'db, SemanticDiagnostic<'db>>,
}

/// Query implementation of [ModuleSemantic::priv_module_semantic_data].
#[salsa::tracked(returns(ref))]
fn priv_module_semantic_data<'db>(
    db: &'db dyn Database,
    _tracked: Tracked,
    module_id: ModuleId<'db>,
) -> Maybe<ModuleSemanticData<'db>> {
    if let Some(data) = db.cached_crate_semantic_data(module_id.owning_crate(db)) {
        if let Some(module_data) = data.modules_semantic_data.get(&module_id) {
            return Ok(module_data.clone());
        } else {
            panic!("module not found in cached modules_data {:?}", module_id.name(db));
        }
    };
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
        if items.insert(name, ModuleItemInfo { item_id, visibility, feature_kind }).is_some() {
            // `item` is extracted from `module_items` and thus `module_item_name_stable_ptr` is
            // guaranteed to succeed.
            diagnostics.report(
                db.module_item_name_stable_ptr(module_id, item_id).unwrap(),
                SemanticDiagnosticKind::NameDefinedMultipleTimes(name),
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
    Ok(ModuleSemanticData { items, global_uses, diagnostics: diagnostics.build() })
}

pub fn module_item_by_name<'db>(
    db: &'db dyn Database,
    module_id: ModuleId<'db>,
    name: SmolStrId<'db>,
) -> Maybe<Option<ModuleItemId<'db>>> {
    let module_data = db.priv_module_semantic_data(module_id)?;
    Ok(module_data.items.get(&name).map(|info| info.item_id))
}

fn module_item_by_name_tracked<'db>(
    db: &'db dyn Database,
    module_id: ModuleId<'db>,
    name: SmolStrId<'db>,
) -> Maybe<Option<ModuleItemId<'db>>> {
    module_item_by_name_helper(db, (), module_id, name)
}

#[salsa::tracked]
fn module_item_by_name_helper<'db>(
    db: &'db dyn Database,
    _tracked: Tracked,
    module_id: ModuleId<'db>,
    name: SmolStrId<'db>,
) -> Maybe<Option<ModuleItemId<'db>>> {
    module_item_by_name(db, module_id, name)
}

pub fn module_item_info_by_name<'db>(
    db: &'db dyn Database,
    module_id: ModuleId<'db>,

    name: SmolStrId<'db>,
) -> Maybe<Option<ModuleItemInfo<'db>>> {
    let module_data = db.priv_module_semantic_data(module_id)?;
    Ok(module_data.items.get(&name).cloned())
}

fn module_item_info_by_name_tracked<'db>(
    db: &'db dyn Database,
    module_id: ModuleId<'db>,
    name: SmolStrId<'db>,
) -> Maybe<Option<ModuleItemInfo<'db>>> {
    module_item_info_by_name_helper(db, (), module_id, name)
}

#[salsa::tracked]
fn module_item_info_by_name_helper<'db>(
    db: &'db dyn Database,
    _tracked: Tracked,
    module_id: ModuleId<'db>,
    name: SmolStrId<'db>,
) -> Maybe<Option<ModuleItemInfo<'db>>> {
    module_item_info_by_name(db, module_id, name)
}

/// Get the imported global uses of a module, and their visibility.
pub fn get_module_global_uses<'db>(
    db: &'db dyn Database,
    module_id: ModuleId<'db>,
) -> Maybe<OrderedHashMap<GlobalUseId<'db>, Visibility>> {
    let module_data = db.priv_module_semantic_data(module_id)?;
    Ok(module_data.global_uses.clone())
}

/// Query implementation of [ModuleSemantic::module_all_used_uses].
#[salsa::tracked(returns(ref))]
pub fn module_all_used_uses<'db>(
    db: &'db dyn Database,
    _tracked: Tracked,
    module_id: ModuleId<'db>,
) -> Maybe<OrderedHashSet<UseId<'db>>> {
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
    Ok(all_used_uses)
}

/// Query implementation of [ModuleSemantic::module_attributes].
#[salsa::tracked(returns(ref))]
pub fn module_attributes<'db>(
    db: &'db dyn Database,
    _tracked: Tracked,
    module_id: ModuleId<'db>,
) -> Maybe<Vec<Attribute<'db>>> {
    Ok(match &module_id {
        ModuleId::CrateRoot(_) | ModuleId::MacroCall { .. } => vec![],
        ModuleId::Submodule(submodule_id) => {
            let module_ast = &submodule_id.module_data(db)?.submodules(db)[submodule_id];

            module_ast.attributes(db).structurize(db)
        }
    })
}

/// Finds all the trait ids usable in the current context, using `global use` imports.
/// Query implementation of [ModuleSemantic::module_usable_trait_ids].
#[salsa::tracked(returns(ref))]
pub fn module_usable_trait_ids<'db>(
    db: &'db dyn Database,
    _tracked: Tracked,
    module_id: ModuleId<'db>,
) -> OrderedHashMap<TraitId<'db>, LookupItemId<'db>> {
    let mut module_traits = OrderedHashMap::<TraitId<'db>, LookupItemId<'db>>::default();
    for (containing_module, info) in db.module_imported_modules((), module_id).iter() {
        for defined_module in
            chain!([containing_module], module_macro_modules(db, false, *containing_module))
        {
            extend_specific_module_usable_trait_ids(db, info, *defined_module, &mut module_traits);
        }
    }
    module_traits
}

/// Extends the `module_traits` with all the trait ids usable in the current context, not using
/// `global use` imports.
fn extend_specific_module_usable_trait_ids<'db>(
    db: &'db dyn Database,
    info: &ImportInfo<'db>,
    containing_module: ModuleId<'db>,
    module_traits: &mut OrderedHashMap<TraitId<'db>, LookupItemId<'db>>,
) {
    let Ok(data) = db.priv_module_semantic_data(containing_module) else {
        return;
    };
    for item in data.items.values() {
        if !matches!(
            item.item_id,
            ModuleItemId::Trait(_)
                | ModuleItemId::Impl(_)
                | ModuleItemId::ImplAlias(_)
                | ModuleItemId::Use(_)
        ) {
            continue;
        }
        if !info.user_modules.iter().any(|user_module_id| {
            peek_visible_in(db, item.visibility, containing_module, *user_module_id)
        }) {
            continue;
        }
        match item.item_id {
            ModuleItemId::Trait(trait_id) => {
                module_traits
                    .entry(trait_id)
                    .or_insert(LookupItemId::ModuleItem(ModuleItemId::Trait(trait_id)));
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
}

impl<'db> HasFeatureKind<'db> for ModuleItemInfo<'db> {
    fn feature_kind(&self) -> &FeatureKind<'db> {
        &self.feature_kind
    }
}

/// Trait for module-related semantic queries.
pub trait ModuleSemantic<'db>: Database {
    /// Private query to compute data about the module.
    fn priv_module_semantic_data(
        &'db self,
        module_id: ModuleId<'db>,
    ) -> Maybe<&'db ModuleSemanticData<'db>> {
        priv_module_semantic_data(self.as_dyn_database(), (), module_id).maybe_as_ref()
    }
    /// Returns [Maybe::Err] if the module was not properly resolved.
    /// Returns [Maybe::Ok(None)] if the item does not exist.
    fn module_item_by_name(
        &'db self,
        module_id: ModuleId<'db>,

        name: SmolStrId<'db>,
    ) -> Maybe<Option<ModuleItemId<'db>>> {
        module_item_by_name_tracked(self.as_dyn_database(), module_id, name)
    }
    /// Returns [Maybe::Err] if the module was not properly resolved.
    /// Returns [Maybe::Ok(None)] if the item does not exist.
    fn module_item_info_by_name(
        &'db self,
        module_id: ModuleId<'db>,

        name: SmolStrId<'db>,
    ) -> Maybe<Option<ModuleItemInfo<'db>>> {
        module_item_info_by_name_tracked(self.as_dyn_database(), module_id, name)
    }
    /// Returns all the items used within the module.
    fn module_all_used_uses(
        &'db self,
        module_id: ModuleId<'db>,
    ) -> Maybe<&'db OrderedHashSet<UseId<'db>>> {
        module_all_used_uses(self.as_dyn_database(), (), module_id).maybe_as_ref()
    }
    /// Returns the attributes of a module.
    fn module_attributes(&'db self, module_id: ModuleId<'db>) -> Maybe<&'db [Attribute<'db>]> {
        Ok(module_attributes(self.as_dyn_database(), (), module_id).maybe_as_ref()?)
    }
    /// Finds all the trait ids usable in the module.
    fn module_usable_trait_ids(
        &'db self,
        module_id: ModuleId<'db>,
    ) -> &'db OrderedHashMap<TraitId<'db>, LookupItemId<'db>> {
        module_usable_trait_ids(self.as_dyn_database(), (), module_id)
    }
}

impl<'db, T: Database + ?Sized> ModuleSemantic<'db> for T {}
