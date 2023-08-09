use std::sync::Arc;

use cairo_lang_defs::ids::{ModuleId, TraitFunctionId};
use cairo_lang_filesystem::ids::CrateId;

use crate::db::SemanticGroup;
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
) -> Arc<Vec<TraitFunctionId>> {
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
) -> Arc<Vec<TraitFunctionId>> {
    let mut result = Vec::new();
    for module_id in db.crate_modules(crate_id).iter() {
        result.extend_from_slice(&db.methods_in_module(*module_id, type_filter.clone())[..])
    }
    result.into()
}
