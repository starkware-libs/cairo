use std::sync::Arc;

use cairo_lang_defs::diagnostic_utils::StableLocation;
use cairo_lang_defs::ids::{LanguageElementId, ModuleId, ModuleItemId};
use cairo_lang_diagnostics::{Diagnostics, DiagnosticsBuilder, Maybe};
use cairo_lang_syntax::attribute::structured::{Attribute, AttributeListStructurize};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use smol_str::SmolStr;

use crate::corelib::core_module;
use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnosticKind;
use crate::resolve::scope::Scope;
use crate::resolve::ResolvedGenericItem;
use crate::SemanticDiagnostic;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ModuleSemanticData {
    // The items in the module without duplicates.
    pub items: OrderedHashMap<SmolStr, ModuleItemId>,
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
    for item in db.module_items(module_id)?.iter() {
        let name = match item {
            ModuleItemId::Constant(item_id) => item_id.name(def_db),
            ModuleItemId::Submodule(item_id) => item_id.name(def_db),
            ModuleItemId::Use(item_id) => item_id.name(def_db),
            ModuleItemId::FreeFunction(item_id) => item_id.name(def_db),
            ModuleItemId::Struct(item_id) => item_id.name(def_db),
            ModuleItemId::Enum(item_id) => item_id.name(def_db),
            ModuleItemId::TypeAlias(item_id) => item_id.name(def_db),
            ModuleItemId::Trait(item_id) => item_id.name(def_db),
            ModuleItemId::Impl(item_id) => item_id.name(def_db),
            ModuleItemId::ExternType(item_id) => item_id.name(def_db),
            ModuleItemId::ExternFunction(item_id) => item_id.name(def_db),
        };

        if items.insert(name.clone(), *item).is_some() {
            let stable_location = StableLocation::new(
                item.module_file_id(def_db),
                db.module_item_name_stable_ptr(module_id, *item)?,
            );
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
    Ok(module_data.items.get(&name).copied())
}

/// Query implementation of [crate::db::SemanticGroup::module_scope].
pub fn module_scope(db: &dyn SemanticGroup, module_id: ModuleId) -> Maybe<Arc<Scope>> {
    let module_data = db.priv_module_semantic_data(module_id)?;
    let mut generic_items = OrderedHashMap::default();
    // Note: this is done in a separate query, since it contains resolved `use` items.
    for (name, module_item) in module_data.items.iter() {
        generic_items
            .insert(name.clone(), ResolvedGenericItem::from_module_item(db, *module_item)?);
    }
    let core_module = core_module(db);
    let parent = match module_id {
        ModuleId::CrateRoot(_) if module_id == core_module => None,
        ModuleId::CrateRoot(_) => Some(db.module_scope(core_module)?),
        ModuleId::Submodule(submodule) => {
            Some(db.module_scope(submodule.parent_module(db.upcast()))?)
        }
    };
    Ok(Arc::new(Scope { generic_items, concrete_items: Default::default(), parent }))
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
