#[cfg(test)]
#[path = "casm_contract_class_test.rs"]
mod test;

use std::collections::HashMap;

use cairo_lang_sierra::extensions::builtin_cost::CostTokenType;
use cairo_lang_sierra::extensions::gas::GasBuiltinType;
use cairo_lang_sierra::extensions::modules::starknet::syscalls::SystemType;
use cairo_lang_sierra::extensions::pedersen::PedersenType;
use cairo_lang_sierra::extensions::range_check::RangeCheckType;
use cairo_lang_sierra::extensions::NoGenericArgsGenericType;
use cairo_lang_sierra::ids::ConcreteTypeId;
use cairo_lang_sierra_to_casm::compiler::CompilationError;
use cairo_lang_sierra_to_casm::metadata::{calc_metadata, MetadataError};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use convert_case::{Case, Casing};
use num_bigint::BigUint;
use num_integer::Integer;
use num_traits::{Num, Signed};
use serde::ser::Serializer;
use serde::{Deserialize, Deserializer, Serialize};
use thiserror::Error;

use crate::contract_class::{ContractClass, ContractEntryPoint};
use crate::felt_serde::{sierra_from_felts, FeltSerdeError};

/// The expected gas cost of an entrypoint that begins with get_gas() immediately.
const ENTRY_POINT_COST: i64 = 15;

#[derive(Error, Debug, Eq, PartialEq)]
pub enum StarknetSierraCompilationError {
    #[error(transparent)]
    CompilationError(#[from] CompilationError),
    #[error(transparent)]
    FeltSerdeError(#[from] FeltSerdeError),
    #[error(transparent)]
    MetadataError(#[from] MetadataError),
    #[error("Invalid entry point.")]
    EntryPointError,
    #[error("{0} is not a supported builtin type.")]
    InvalidBuiltinType(ConcreteTypeId),
    #[error("Invalid entry point signature")]
    InvalidEntryPointSignature,
}

/// Represents a contract in the StarkNet network.
#[derive(Default, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct CasmContractClass {
    #[serde(serialize_with = "serialize_big_uint", deserialize_with = "deserialize_big_uint")]
    pub prime: BigUint,
    pub compiler_version: String,
    pub bytecode: Vec<BigIntAsHex>,
    pub hints: Vec<(usize, Vec<String>)>,
    pub entry_points_by_type: CasmContractEntryPoints,
}

impl CasmContractClass {
    // TODO(ilya): Reduce the size of CompilationError.
    #[allow(clippy::result_large_err)]
    pub fn from_contract_class(
        contract_class: ContractClass,
    ) -> Result<Self, StarknetSierraCompilationError> {
        let prime = BigUint::from_str_radix(
            "800000000000011000000000000000000000000000000000000000000000001",
            16,
        )
        .unwrap();

        let program = sierra_from_felts(&contract_class.sierra_program)?;

        let gas_usage_check = true;
        let metadata = calc_metadata(&program)?;
        let cairo_program =
            cairo_lang_sierra_to_casm::compiler::compile(&program, &metadata, gas_usage_check)?;

        let mut bytecode = vec![];
        let mut hints = vec![];
        for instruction in cairo_program.instructions {
            if !instruction.hints.is_empty() {
                hints.push((
                    bytecode.len(),
                    instruction.hints.iter().map(|hint| hint.to_string()).collect(),
                ))
            }
            bytecode.extend(instruction.assemble().encode().iter().map(|big_int| {
                let (_q, reminder) = big_int.magnitude().div_rem(&prime);

                BigIntAsHex {
                    value: if big_int.is_negative() { &prime - reminder } else { reminder },
                }
            }))
        }

        let name_by_debug_id = HashMap::<u64, String>::from(
            [RangeCheckType::ID, PedersenType::ID, GasBuiltinType::ID, SystemType::ID].map(
                |generic_id| {
                    (
                        generic_id.id,
                        generic_id
                            .debug_name
                            .expect("Sierra generic types have a full name.")
                            .as_str()
                            .to_case(Case::Snake),
                    )
                },
            ),
        );

        let mut name_by_short_id = HashMap::<u64, &str>::default();
        for decl in program.type_declarations {
            if !decl.long_id.generic_args.is_empty() {
                continue;
            }

            if let Some(name) = name_by_debug_id.get(&decl.long_id.generic_id.id) {
                name_by_short_id.insert(decl.id.id, name);
            }
        }

        let as_casm_entry_point = |contract_entry_point: ContractEntryPoint| {
            let Some(function) = program.funcs.get(contract_entry_point.function_idx) else {
                return Err(StarknetSierraCompilationError::EntryPointError);
            };
            let statement_id = function.entry_point;
            let mut builtins = vec![];

            // The expected return types are [builtins.., gas_builtin, system, PanicResult],
            // So we ignore the last two return types.
            let (signature_builtins, leftover) =
                function.signature.ret_types.split_at(function.signature.ret_types.len() - 3);

            // TODO(ilya): Check that the last argument is PanicResult.
            if leftover[..2]
                .iter()
                .map(|type_id| name_by_short_id.get(&type_id.id))
                .ne([Some(&"gas_builtin"), Some(&"system")])
            {
                return Err(StarknetSierraCompilationError::InvalidEntryPointSignature);
            }

            for type_id in signature_builtins.iter() {
                if let Some(name) = name_by_short_id.get(&type_id.id) {
                    builtins.push(name.to_string());
                } else {
                    return Err(StarknetSierraCompilationError::InvalidBuiltinType(
                        type_id.clone(),
                    ));
                }
            }

            let code_offset = cairo_program
                .debug_info
                .sierra_statement_info
                .get(statement_id.0)
                .ok_or(StarknetSierraCompilationError::EntryPointError)?
                .code_offset;
            assert_eq!(
                metadata.gas_info.function_costs[function.id.clone()],
                OrderedHashMap::from_iter([(CostTokenType::Step, ENTRY_POINT_COST)]),
                "Unexpected entry point cost."
            );
            Ok::<CasmContractEntryPoint, StarknetSierraCompilationError>(CasmContractEntryPoint {
                selector: contract_entry_point.selector,
                offset: code_offset,
                builtins,
            })
        };

        let as_casm_entry_points = |contract_entry_points: Vec<ContractEntryPoint>| {
            let mut entry_points = vec![];
            for contract_entry_point in contract_entry_points.into_iter() {
                entry_points.push(as_casm_entry_point(contract_entry_point)?);
            }
            Ok::<Vec<CasmContractEntryPoint>, StarknetSierraCompilationError>(entry_points)
        };

        Ok(Self {
            prime,
            compiler_version: "1.0.0".to_string(),
            bytecode,
            hints,
            entry_points_by_type: CasmContractEntryPoints {
                external: as_casm_entry_points(contract_class.entry_points_by_type.external)?,
                l1_handler: as_casm_entry_points(contract_class.entry_points_by_type.l1_handler)?,
                constructor: as_casm_entry_points(contract_class.entry_points_by_type.constructor)?,
            },
        })
    }
}

#[derive(Default, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct CasmContractEntryPoint {
    /// A field element that encodes the signature of the called function.
    #[serde(serialize_with = "serialize_big_uint", deserialize_with = "deserialize_big_uint")]
    pub selector: BigUint,
    /// The offset of the instruction that should be called within the contract bytecode.
    pub offset: usize,
    // list of builtins.
    pub builtins: Vec<String>,
}

#[derive(Default, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct CasmContractEntryPoints {
    #[serde(rename = "EXTERNAL")]
    pub external: Vec<CasmContractEntryPoint>,
    #[serde(rename = "L1_HANDLER")]
    pub l1_handler: Vec<CasmContractEntryPoint>,
    #[serde(rename = "CONSTRUCTOR")]
    pub constructor: Vec<CasmContractEntryPoint>,
}

pub fn serialize_big_uint<S>(num: &BigUint, serializer: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    serializer.serialize_str(&format!("{:#x}", num))
}

pub fn deserialize_big_uint<'a, D>(deserializer: D) -> Result<BigUint, D::Error>
where
    D: Deserializer<'a>,
{
    let s = &String::deserialize(deserializer)?;
    match s.strip_prefix("0x") {
        Some(num_no_prefix) => BigUint::from_str_radix(num_no_prefix, 16)
            .map_err(|error| serde::de::Error::custom(format!("{}", error))),
        None => Err(serde::de::Error::custom(format!("{s} does not start with `0x` is missing."))),
    }
}

// A wrapper for BigUint that serializes as hex.
#[derive(Default, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(transparent)]
pub struct BigIntAsHex {
    /// A field element that encodes the signature of the called function.
    #[serde(serialize_with = "serialize_big_uint", deserialize_with = "deserialize_big_uint")]
    pub value: BigUint,
}
