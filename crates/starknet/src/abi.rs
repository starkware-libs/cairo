use defs::ids::{TraitFunctionId, TraitId};
use semantic::db::SemanticGroup;
use thiserror::Error;

#[cfg(test)]
#[path = "abi_test.rs"]
mod test;

#[derive(Default, Debug)]
pub struct Contract {
    // TODO(spapini): Add storage variables.
    pub functions: Vec<Function>,
}
impl Contract {
    fn deduce_function(
        &mut self,
        db: &dyn SemanticGroup,
        trait_function_id: TraitFunctionId,
    ) -> Result<(), ABIError> {
        let defs_db = db.upcast();
        let name = trait_function_id.name(defs_db).into();
        let signature =
            db.trait_function_signature(trait_function_id).ok_or(ABIError::CompilationError)?;
        self.functions.push(Function {
            name,
            inputs: signature
                .all_params()
                .map(|param| Input {
                    name: param.id.name(db.upcast()).into(),
                    ty: param.ty.format(db),
                })
                .collect(),
            // TODO(spapini): output refs?
            output_ty: signature.return_type.format(db),
        });

        Ok(())
    }
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub inputs: Vec<Input>,
    pub output_ty: String,
}

#[derive(Debug)]
pub struct Input {
    pub name: String,
    pub ty: String,
}

#[derive(Error, Debug)]
pub enum ABIError {
    #[error("Generic traits are unsupported.")]
    GenericTraitsUnsupported,
    #[error("Compilation error.")]
    CompilationError,
}

pub fn deduce_abi(db: &dyn SemanticGroup, trait_id: TraitId) -> Result<Contract, ABIError> {
    if !db.trait_generic_params(trait_id).unwrap().is_empty() {
        return Err(ABIError::GenericTraitsUnsupported);
    }

    let mut contract = Contract::default();
    for trait_function_id in db.trait_functions(trait_id).unwrap_or_default() {
        contract.deduce_function(db, trait_function_id)?;
    }

    Ok(contract)
}
