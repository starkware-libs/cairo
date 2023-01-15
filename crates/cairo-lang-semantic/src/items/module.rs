use std::sync::Arc;

use cairo_lang_defs::diagnostic_utils::StableLocation;
use cairo_lang_defs::ids::{LanguageElementId, ModuleId, ModuleItemId};
use cairo_lang_diagnostics::{Diagnostics, DiagnosticsBuilder, Maybe};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use smol_str::SmolStr;

use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnosticKind;
use crate::SemanticDiagnostic;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ModuleSemanticData {
    // The items in the module without duplicates.
    pub items: OrderedHashMap<SmolStr, ModuleItemId>,
    pub diagnostics: Diagnostics<SemanticDiagnostic>,
}

pub fn priv_module_items_data(
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
            diagnostics.add(SemanticDiagnostic {
                stable_location: StableLocation::new(
                    item.module_file_id(def_db),
                    // TODO(ilya): Give the name as the location.
                    item.untyped_stable_ptr(def_db),
                ),
                kind: SemanticDiagnosticKind::NameDefinedMultipleTimes { name: name.clone() },
            });
        }
    }
    Ok(Arc::new(ModuleSemanticData { items, diagnostics: diagnostics.build() }))
}

pub fn module_item_by_name(
    db: &dyn SemanticGroup,
    module_id: ModuleId,
    name: SmolStr,
) -> Maybe<Option<ModuleItemId>> {
    let module_data = db.priv_module_items_data(module_id)?;
    Ok(module_data.items.get(&name).copied())
}
