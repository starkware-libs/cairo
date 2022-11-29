use defs::ids::{ModuleItemId, StructId};
use semantic::db::SemanticGroup;
use semantic::items::attribute::Attribute;

#[cfg(test)]
#[path = "contract_test.rs"]
mod test;

/// Represents a struct and an attribute that marks it as a contract.
pub struct UnresolvedContractDefinition {
    /// The struct that defines the contracts storage.
    pub id: StructId,
    /// The contract attribute
    pub attr: Attribute,
}

/// Finds the structs annotated as contracts in the current compilation units and
/// returns the corresponding UnresolvedContractDefinition.
pub fn find_contract_structs(db: &dyn SemanticGroup) -> Vec<UnresolvedContractDefinition> {
    let mut contracts = vec![];
    for crate_id in db.crates() {
        let modules = db.crate_modules(crate_id);
        for module_id in modules.iter() {
            let Some(module_items) = db.module_items(*module_id) else {
                continue;
            };

            for item in module_items.items.values() {
                if let ModuleItemId::Struct(struct_id) = item {
                    if let Some(attrs) = db.struct_attributes(*struct_id) {
                        if let [attr] = attrs.as_slice() {
                            if attr.id == "contract" {
                                contracts.push(UnresolvedContractDefinition {
                                    id: *struct_id,
                                    attr: attr.clone(),
                                });
                            }
                        };
                    }
                }
            }
        }
    }
    contracts
}
