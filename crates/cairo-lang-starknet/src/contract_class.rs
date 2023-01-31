use std::path::Path;

use anyhow::{Context, Result};
use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_compiler::diagnostics::check_and_eprint_diagnostics;
use cairo_lang_compiler::project::setup_project;
use cairo_lang_defs::ids::TopLevelLanguageElementId;
use cairo_lang_diagnostics::ToOption;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::{ConcreteFunctionWithBodyId, FunctionLongId};
use cairo_lang_sierra::{self};
use cairo_lang_sierra_generator::canonical_id_replacer::CanonicalReplacer;
use cairo_lang_sierra_generator::db::SierraGenGroup;
use cairo_lang_sierra_generator::replace_ids::{replace_sierra_ids_in_program, SierraIdReplacer};
use itertools::chain;
use num_bigint::BigUint;
use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::abi::Contract;
use crate::casm_contract_class::{deserialize_big_uint, serialize_big_uint, BigIntAsHex};
use crate::contract::{find_contracts, get_abi, get_module_functions, starknet_keccak};
use crate::db::StarknetRootDatabaseBuilderEx;
use crate::felt_serde::{sierra_from_felts, sierra_to_felts};
use crate::plugin::{CONSTRUCTOR_MODULE, EXTERNAL_MODULE};
use crate::sierra_version::{self, lookup_sierra_version, SierraVersionError};

#[cfg(test)]
#[path = "contract_class_test.rs"]
mod test;

#[derive(Error, Debug, Eq, PartialEq)]
pub enum StarknetCompilationError {
    #[error("Invalid entry point.")]
    EntryPointError,
    #[error(transparent)]
    SierraVersionError(#[from] SierraVersionError),
}

/// Represents a contract in the StarkNet network.
#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ContractClass {
    pub sierra_program: Vec<BigIntAsHex>,
    pub sierra_program_debug_info: cairo_lang_sierra::debug_info::DebugInfo,
    /// The sierra version used in compilation.
    pub sierra_version_id: usize,
    pub entry_points_by_type: ContractEntryPoints,
    pub abi: Contract,
}

impl ContractClass {
    /// Checks that all the used libfuncs in the contract class are allwed in the contract class
    /// sierra version.
    pub fn verify_compatible_sierra_version(&self) -> Result<(), SierraVersionError> {
        let sierra_version = lookup_sierra_version(self.sierra_version_id)?;
        let sierra_program = sierra_from_felts(&self.sierra_program)
            .map_err(|_| SierraVersionError::SierraProgramError)?;
        let allowed_libfuncs_ids = sierra_version.get_allowed_libfuncs_ids();
        for libfunc in sierra_program.libfunc_declarations.iter() {
            if !allowed_libfuncs_ids.contains(&libfunc.long_id.generic_id) {
                return Err(SierraVersionError::UnsupportedLibfunc {
                    invalid_libfunc: libfunc.long_id.generic_id.to_string(),
                    sierra_version_id: self.sierra_version_id,
                });
            }
        }
        Ok(())
    }
}

#[derive(Default, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ContractEntryPoints {
    #[serde(rename = "EXTERNAL")]
    pub external: Vec<ContractEntryPoint>,
    #[serde(rename = "L1_HANDLER")]
    pub l1_handler: Vec<ContractEntryPoint>,
    #[serde(rename = "CONSTRUCTOR")]
    pub constructor: Vec<ContractEntryPoint>,
}

#[derive(Default, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ContractEntryPoint {
    /// A field element that encodes the signature of the called function.
    #[serde(serialize_with = "serialize_big_uint", deserialize_with = "deserialize_big_uint")]
    pub selector: BigUint,
    /// The idx of the user function declaration in the sierra program.
    pub function_idx: usize,
}

/// Compile the contract given by path.
/// If `replace_ids` is true, replaces sierra ids with human-readable ones.
pub fn compile_path(path: &Path, replace_ids: bool) -> Result<ContractClass> {
    let mut db_val = {
        let mut b = RootDatabase::builder();
        b.with_dev_corelib().unwrap();
        b.with_starknet();
        b.build()
    };
    let db = &mut db_val;

    let main_crate_ids = setup_project(db, Path::new(&path))?;

    if check_and_eprint_diagnostics(db) {
        anyhow::bail!("Failed to compile: {}", path.display());
    }

    let contracts = find_contracts(db, &main_crate_ids);
    let contract = match &contracts[..] {
        [contract] => contract,
        [] => anyhow::bail!("Contract not found."),
        _ => {
            // TODO(ilya): Add contract names.
            anyhow::bail!("Compilation unit must include only one contract.",)
        }
    };

    let external_functions: Vec<_> = get_module_functions(db, contract, EXTERNAL_MODULE)?
        .into_iter()
        .flat_map(|f| ConcreteFunctionWithBodyId::from_no_generics_free(db, f))
        .collect();
    let constructor_functions: Vec<_> = get_module_functions(db, contract, CONSTRUCTOR_MODULE)?
        .into_iter()
        .flat_map(|f| ConcreteFunctionWithBodyId::from_no_generics_free(db, f))
        .collect();
    let sierra_program = db
        .get_sierra_program_for_functions(
            chain!(&external_functions, &constructor_functions).cloned().collect(),
        )
        .to_option()
        .with_context(|| "Compilation failed without any diagnostics.")?;

    let replacer = CanonicalReplacer::from_program(&sierra_program);
    let sierra_program = if replace_ids {
        replace_sierra_ids_in_program(db, &sierra_program)
    } else {
        replacer.apply(&sierra_program)
    };

    let entry_points_by_type = ContractEntryPoints {
        external: get_entry_points(db, &external_functions, &replacer)?,
        l1_handler: vec![],
        /// TODO(orizi): Validate there is at most one constructor.
        constructor: get_entry_points(db, &constructor_functions, &replacer)?,
    };
    let contract_class = ContractClass {
        sierra_program: sierra_to_felts(&sierra_program)?,
        sierra_program_debug_info: cairo_lang_sierra::debug_info::DebugInfo::extract(
            &sierra_program,
        ),
        sierra_version_id: sierra_version::CURRENT_VERSION_ID,
        entry_points_by_type,
        abi: Contract::from_trait(db, get_abi(db, contract)?).with_context(|| "ABI error")?,
    };
    contract_class.verify_compatible_sierra_version()?;
    Ok(contract_class)
}

/// Returns the entry points given their IDs.
fn get_entry_points(
    db: &mut RootDatabase,
    entry_point_functions: &[ConcreteFunctionWithBodyId],
    replacer: &CanonicalReplacer,
) -> Result<Vec<ContractEntryPoint>> {
    let mut entry_points = vec![];
    for function_with_body_id in entry_point_functions {
        let function_id =
            db.intern_function(FunctionLongId { function: function_with_body_id.concrete(db) });

        let sierra_id = db.intern_sierra_function(function_id);

        entry_points.push(ContractEntryPoint {
            selector: starknet_keccak(
                function_with_body_id.function_with_body_id(db).name(db).as_bytes(),
            ),
            function_idx: replacer.replace_function_id(&sierra_id).id as usize,
        });
    }
    Ok(entry_points)
}
