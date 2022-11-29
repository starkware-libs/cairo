use defs::ids::{ModuleItemId, StructId};
use semantic::db::SemanticGroup;

#[cfg(test)]
#[path = "contract_test.rs"]
mod test;

/// Represents a declaration of a contract.
pub struct ContractDeclaration {
    /// The id of the struct that defines the contracts storage.
    pub struct_id: StructId,
    /// A list of Expressions that specify the implementations included in the contract.
    pub impls: Vec<syntax::node::ast::Expr>,
}

/// Finds the structs annotated as contracts in the current compilation units and
/// returns the corresponding ContractDeclarations.
pub fn find_contract_structs(db: &dyn SemanticGroup) -> Vec<ContractDeclaration> {
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
                                contracts.push(ContractDeclaration {
                                    struct_id: *struct_id,
                                    impls: attr.args.clone(),
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
