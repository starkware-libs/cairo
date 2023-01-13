use cairo_lang_defs::ids::{TraitFunctionId, TraitId};
use cairo_lang_semantic::db::SemanticGroup;
use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::plugin::{EVENT_ATTR, VIEW_ATTR};

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
            if trait_function_has_attr(db, *trait_function_id, EVENT_ATTR)? {
                contract.add_event(db, *trait_function_id)?;
            } else {
                contract.add_function(db, *trait_function_id)?;
            }
        }

        Ok(contract)
    }

    /// Adds a function to the ABI from a TraitFunctionId.
    fn add_function(
        &mut self,
        db: &dyn SemanticGroup,
        trait_function_id: TraitFunctionId,
    ) -> Result<(), ABIError> {
        let state_mutability = if trait_function_has_attr(db, trait_function_id, VIEW_ATTR)? {
            StateMutability::View
        } else {
            StateMutability::External
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

    /// Adds an event to the ABI from a TraitFunctionId.
    fn add_event(
        &mut self,
        db: &dyn SemanticGroup,
        trait_function_id: TraitFunctionId,
    ) -> Result<(), ABIError> {
        let defs_db = db.upcast();
        let name = trait_function_id.name(defs_db).into();
        let signature = db
            .trait_function_signature(trait_function_id)
            .map_err(|_| ABIError::CompilationError)?;
        self.items.push(Item::Event(Event {
            name,
            inputs: signature
                .params
                .into_iter()
                .map(|param| Input {
                    name: param.id.name(db.upcast()).into(),
                    ty: param.ty.format(db),
                })
                .collect(),
        }));

        Ok(())
    }

    pub fn json(&self) -> String {
        serde_json::to_string_pretty(&self).unwrap()
    }
}

/// Checks whether the trait function has the given attribute.
fn trait_function_has_attr(
    db: &dyn SemanticGroup,
    trait_function_id: TraitFunctionId,
    attr: &str,
) -> Result<bool, ABIError> {
    Ok(db
        .trait_function_attributes(trait_function_id)
        .map_err(|_| ABIError::CompilationError)?
        .iter()
        .any(|a| a.id.to_string() == attr))
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
    #[serde(rename = "event")]
    Event(Event),
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum StateMutability {
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

/// Contract event.
#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Event {
    pub name: String,
    pub inputs: Vec<Input>,
}

/// Function input ABI.
#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Input {
    pub name: String,
    pub ty: String,
}
