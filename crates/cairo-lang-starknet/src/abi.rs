use cairo_lang_defs::ids::{TraitFunctionId, TraitId};
use cairo_lang_semantic::db::SemanticGroup;
use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::plugin::{EXTERNAL_ATTR, VIEW_ATTR};

#[cfg(test)]
#[path = "abi_test.rs"]
mod test;

/// Contract ABI.
#[derive(Default, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(transparent)]
pub struct Contract {
    // TODO(spapini): Add storage variables.
    pub items: Vec<Item>,
}
impl Contract {
    /// Creates a Starknet contract ABI from a TraitId.
    pub fn from_trait(db: &dyn SemanticGroup, trait_id: TraitId) -> Result<Self, ABIError> {
        if !db.trait_generic_params(trait_id).map_err(|_| ABIError::CompilationError)?.is_empty() {
            return Err(ABIError::GenericTraitsUnsupported);
        }

        let mut contract = Self::default();

        for trait_function_id in db.trait_functions(trait_id).unwrap_or_default().values() {
            contract.add_function(db, *trait_function_id)?;
        }

        Ok(contract)
    }

    /// Adds a function to the ABI from a TraitFunctionId.
    fn add_function(
        &mut self,
        db: &dyn SemanticGroup,
        trait_function_id: TraitFunctionId,
    ) -> Result<(), ABIError> {
        let attrs = db
            .trait_function_attributes(trait_function_id)
            .map_err(|_| ABIError::CompilationError)?;
        let state_mutability = if attrs.iter().any(|attr| attr.id.to_string() == EXTERNAL_ATTR) {
            StateMutability::External
        } else if attrs.iter().any(|attr| attr.id.to_string() == VIEW_ATTR) {
            StateMutability::View
        } else {
            StateMutability::Unknown
        };
        let defs_db = db.upcast();
        let name = trait_function_id.name(defs_db).into();
        let signature = db
            .trait_function_signature(trait_function_id)
            .map_err(|_| ABIError::CompilationError)?;
        self.items.push(Item::Function(Function {
            name,
            inputs: signature
                .params
                .into_iter()
                .map(|param| Input {
                    name: param.id.name(db.upcast()).into(),
                    ty: param.ty.format(db),
                })
                .collect(),
            // TODO(spapini): output refs?
            output_ty: signature.return_type.format(db),
            state_mutability,
        }));

        Ok(())
    }

    pub fn json(&self) -> String {
        serde_json::to_string_pretty(&self).unwrap()
    }
}

#[derive(Error, Debug)]
pub enum ABIError {
    #[error("Generic traits are unsupported.")]
    GenericTraitsUnsupported,
    #[error("Compilation error.")]
    CompilationError,
}

/// Enum of contract item ABIs.
#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum Item {
    #[serde(rename = "function")]
    Function(Function),
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum StateMutability {
    #[serde(rename = "unknown")]
    Unknown,
    #[serde(rename = "external")]
    External,
    #[serde(rename = "view")]
    View,
}

/// Contract function ABI.
#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Function {
    pub name: String,
    pub inputs: Vec<Input>,
    pub output_ty: String,
    pub state_mutability: StateMutability,
}

/// Function input ABI.
#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Input {
    pub name: String,
    pub ty: String,
}
