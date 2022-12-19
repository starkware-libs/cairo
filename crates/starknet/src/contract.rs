use anyhow::Context;
use defs::ids::{FreeFunctionId, LanguageElementId, ModuleId, ModuleItemId, SubmoduleId};
use diagnostics::ToOption;
use filesystem::ids::CrateId;
use num_bigint::BigUint;
use semantic::db::SemanticGroup;
use sha3::{Digest, Keccak256};

use crate::plugin::{CONTRACT_ATTR, EXTERNAL_MODULE};

#[cfg(test)]
#[path = "contract_test.rs"]
mod test;

/// Represents a declaration of a contract.
pub struct ContractDeclaration {
    /// The id of the module that defines the contract.
    pub submodule_id: SubmoduleId,
}

impl ContractDeclaration {
    pub fn module_id(&self) -> ModuleId {
        ModuleId::Submodule(self.submodule_id)
    }
}

/// A variant of eth-keccak that computes a value that fits in a StarkNet field element.
pub fn starknet_keccak(data: &[u8]) -> BigUint {
    let mut hasher = Keccak256::new();
    hasher.update(data);
    let mut result = hasher.finalize();

    // Truncate result to 250 bits.
    *result.first_mut().unwrap() &= 3;
    BigUint::from_bytes_be(&result)
}

/// Finds the inline modules annotated as contracts in the given crate_ids and
/// returns the corresponding ContractDeclarations.
pub fn find_contracts(db: &dyn SemanticGroup, crate_ids: &[CrateId]) -> Vec<ContractDeclaration> {
    let mut contracts = vec![];
    for crate_id in crate_ids {
        let modules = db.crate_modules(*crate_id);
        for module_id in modules.iter() {
            let Ok(submodules) = db.module_submodules(*module_id) else {
                continue;
            };

            for module_id in submodules {
                if let ModuleId::Submodule(submodule_id) = module_id {
                    if let Ok(attrs) = db.module_attributes(module_id) {
                        if attrs.iter().any(|attr| attr.id == CONTRACT_ATTR) {
                            contracts.push(ContractDeclaration { submodule_id });
                        };
                    }
                }
            }
        }
    }
    contracts
}

/// Returns the list of external functions for a given contract.
pub fn get_external_functions(
    db: &(dyn SemanticGroup + 'static),
    contract: &ContractDeclaration,
) -> anyhow::Result<Vec<FreeFunctionId>> {
    // The wrappers are currently in the parent module.
    let parent_module_id = contract.submodule_id.module(db.upcast());

    match db
        .module_items(parent_module_id)
        .to_option()
        .with_context(|| "Failed to get module items.")?
        .items
        .get(EXTERNAL_MODULE)
    {
        Some(ModuleItemId::Submodule(external_module_id)) => Ok(db
            .module_free_functions(ModuleId::Submodule(*external_module_id))
            .to_option()
            .with_context(|| "Failed to get module items.")?),
        _ => anyhow::bail!("Failed to get the entry points module."),
    }
}
