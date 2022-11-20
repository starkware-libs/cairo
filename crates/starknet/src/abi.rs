use defs::ids::{TraitFunctionId, TraitId};
use semantic::db::SemanticGroup;
use serde::{Deserialize, Serialize};
use thiserror::Error;

#[cfg(test)]
#[path = "abi_test.rs"]
mod test;

#[derive(Default, Debug, Serialize, Deserialize)]
pub struct Contract {
    // TODO(spapini): Add storage variables.
    pub items: Vec<Item>,
}
impl Contract {
    /// Creates a Starknet contract ABI from a TraitId.
    pub fn from_trait(db: &dyn SemanticGroup, trait_id: TraitId) -> Result<Self, ABIError> {
        if !db.trait_generic_params(trait_id).unwrap().is_empty() {
            return Err(ABIError::GenericTraitsUnsupported);
        }

        let mut contract = Self::default();
        for trait_function_id in db.trait_functions(trait_id).unwrap_or_default() {
            contract.add_function(db, trait_function_id)?;
        }

        Ok(contract)
    }

    /// Adds a function to the ABI from a TraitFunctionId.
    fn add_function(
        &mut self,
        db: &dyn SemanticGroup,
        trait_function_id: TraitFunctionId,
    ) -> Result<(), ABIError> {
        let defs_db = db.upcast();
        let name = trait_function_id.name(defs_db).into();
        let signature =
            db.trait_function_signature(trait_function_id).ok_or(ABIError::CompilationError)?;
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
        }));

        Ok(())
    }
}

#[derive(Error, Debug)]
pub enum ABIError {
    #[error("Generic traits are unsupported.")]
    GenericTraitsUnsupported,
    #[error("Compilation error.")]
    CompilationError,
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum Item {
    #[serde(rename = "function")]
    Function(Function),
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Function {
    pub name: String,
    pub inputs: Vec<Input>,
    pub output_ty: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Input {
    pub name: String,
    pub ty: String,
}
