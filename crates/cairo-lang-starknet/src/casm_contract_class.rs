#[cfg(test)]
#[path = "casm_contract_class_test.rs"]
mod test;

use cairo_lang_casm::hints::Hint;
use cairo_lang_sierra::extensions::builtin_cost::CostTokenType;
use cairo_lang_sierra::extensions::ec::EcOpType;
use cairo_lang_sierra::extensions::gas::GasBuiltinType;
use cairo_lang_sierra::extensions::pedersen::PedersenType;
use cairo_lang_sierra::extensions::range_check::RangeCheckType;
use cairo_lang_sierra::extensions::segment_arena::SegmentArenaType;
use cairo_lang_sierra::extensions::starknet::syscalls::SystemType;
use cairo_lang_sierra::extensions::NoGenericArgsGenericType;
use cairo_lang_sierra::ids::{ConcreteTypeId, GenericTypeId};
use cairo_lang_sierra_to_casm::compiler::CompilationError;
use cairo_lang_sierra_to_casm::metadata::{
    calc_metadata, MetadataComputationConfig, MetadataError,
};
use cairo_lang_utils::bigint::{deserialize_big_uint, serialize_big_uint, BigUintAsHex};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use cairo_lang_utils::unordered_hash_set::UnorderedHashSet;
use convert_case::{Case, Casing};
use itertools::{chain, Itertools};
use num_bigint::BigUint;
use num_integer::Integer;
use num_traits::{Num, Signed};
use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::allowed_libfuncs::AllowedLibfuncsError;
use crate::contract_class::{ContractClass, ContractEntryPoint};
use crate::felt252_serde::{sierra_from_felt252s, Felt252SerdeError};

/// The expected gas cost of an entrypoint.
pub const ENTRY_POINT_COST: i32 = 10000;

#[derive(Error, Debug, Eq, PartialEq)]
pub enum StarknetSierraCompilationError {
    #[error(transparent)]
    CompilationError(#[from] Box<CompilationError>),
    #[error(transparent)]
    Felt252SerdeError(#[from] Felt252SerdeError),
    #[error(transparent)]
    MetadataError(#[from] MetadataError),
    #[error(transparent)]
    AllowedLibfuncsError(#[from] AllowedLibfuncsError),
    #[error("Invalid entry point.")]
    EntryPointError,
    #[error("Missing arguments in the entry point.")]
    InvalidEntryPointSignatureMissingArgs,
    #[error("{0} is not a supported builtin type.")]
    InvalidBuiltinType(ConcreteTypeId),
    #[error("Invalid entry point signature - builtins are not in the expected order.")]
    InvalidEntryPointSignatureWrongBuiltinsOrder,
    #[error("Entry points not sorted by selectors.")]
    EntryPointsOutOfOrder,
    #[error("Out of range value in serialization.")]
    ValueOutOfRange,
}

fn skip_if_none<T>(opt_field: &Option<T>) -> bool {
    opt_field.is_none()
}

/// Represents a contract in the Starknet network.
#[derive(Default, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct CasmContractClass {
    #[serde(serialize_with = "serialize_big_uint", deserialize_with = "deserialize_big_uint")]
    pub prime: BigUint,
    pub compiler_version: String,
    pub bytecode: Vec<BigUintAsHex>,
    pub hints: Vec<(usize, Vec<Hint>)>,

    // Optional pythonic hints in a format that can be executed by the python vm.
    #[serde(skip_serializing_if = "skip_if_none")]
    pub pythonic_hints: Option<Vec<(usize, Vec<String>)>>,
    pub entry_points_by_type: CasmContractEntryPoints,
}

impl CasmContractClass {
    // TODO(ilya): Reduce the size of CompilationError.
    #[allow(clippy::result_large_err)]
    pub fn from_contract_class(
        contract_class: ContractClass,
        add_pythonic_hints: bool,
    ) -> Result<Self, StarknetSierraCompilationError> {
        let prime = BigUint::from_str_radix(
            "800000000000011000000000000000000000000000000000000000000000001",
            16,
        )
        .unwrap();

        for felt252 in &contract_class.sierra_program {
            if felt252.value >= prime {
                return Err(StarknetSierraCompilationError::ValueOutOfRange);
            }
        }

        let (_, program) = sierra_from_felt252s(&contract_class.sierra_program)?;
        for entry_points in [
            &contract_class.entry_points_by_type.constructor,
            &contract_class.entry_points_by_type.external,
            &contract_class.entry_points_by_type.l1_handler,
        ] {
            // TODO(orizi): Use `is_sorted` when it becomes stable.
            if (1..entry_points.len())
                .any(|i| entry_points[i - 1].selector > entry_points[i].selector)
            {
                return Err(StarknetSierraCompilationError::EntryPointsOutOfOrder);
            }
        }

        let entrypoint_ids = chain!(
            &contract_class.entry_points_by_type.constructor,
            &contract_class.entry_points_by_type.external,
            &contract_class.entry_points_by_type.l1_handler,
        )
        .map(|entrypoint| program.funcs[entrypoint.function_idx].id.clone());
        let metadata_computation_config = MetadataComputationConfig {
            function_set_costs: entrypoint_ids
                .map(|id| (id, [(CostTokenType::Const, ENTRY_POINT_COST)].into()))
                .collect(),
        };
        let metadata = calc_metadata(&program, metadata_computation_config)?;

        let gas_usage_check = true;
        let cairo_program =
            cairo_lang_sierra_to_casm::compiler::compile(&program, &metadata, gas_usage_check)?;

        let mut bytecode = vec![];
        let mut hints = vec![];
        for instruction in cairo_program.instructions {
            if !instruction.hints.is_empty() {
                hints.push((bytecode.len(), instruction.hints.clone()))
            }
            bytecode.extend(instruction.assemble().encode().iter().map(|big_int| {
                let (_q, reminder) = big_int.magnitude().div_rem(&prime);

                BigUintAsHex {
                    value: if big_int.is_negative() { &prime - reminder } else { reminder },
                }
            }))
        }

        let builtin_types = UnorderedHashSet::<GenericTypeId>::from_iter(
            [
                RangeCheckType::ID,
                PedersenType::ID,
                EcOpType::ID,
                // TODO(lior): Uncomment the line below once Poseidon is supported.
                //   PoseidonType::ID,
                SegmentArenaType::ID,
                GasBuiltinType::ID,
                SystemType::ID,
            ]
            .into_iter(),
        );
        let name_by_short_id = UnorderedHashMap::<u64, String>::from_iter(
            program
                .type_declarations
                .iter()
                .filter(|decl| {
                    decl.long_id.generic_args.is_empty()
                        && builtin_types.contains(&decl.long_id.generic_id)
                })
                .map(|decl| (decl.id.id, decl.long_id.generic_id.0.as_str().to_case(Case::Snake))),
        );

        let as_casm_entry_point = |contract_entry_point: ContractEntryPoint| {
            let Some(function) = program.funcs.get(contract_entry_point.function_idx) else {
                return Err(StarknetSierraCompilationError::EntryPointError);
            };
            let statement_id = function.entry_point;

            // The expected return types are [builtins.., gas_builtin, system, PanicResult].
            if function.signature.ret_types.len() < 3 {
                return Err(StarknetSierraCompilationError::InvalidEntryPointSignatureMissingArgs);
            }
            // TODO(ilya): Check that the last argument is PanicResult.
            let (_panic_result, builtins) = function.signature.ret_types.split_last().unwrap();

            for type_id in builtins.iter() {
                if !name_by_short_id.contains_key(&type_id.id) {
                    return Err(StarknetSierraCompilationError::InvalidBuiltinType(
                        type_id.clone(),
                    ));
                }
            }
            let (system, builtins) = builtins.split_last().unwrap();
            let (gas, builtins) = builtins.split_last().unwrap();

            // Check that the last builtins are gas and system.
            if name_by_short_id[gas.id] != "gas_builtin" || name_by_short_id[system.id] != "system"
            {
                return Err(
                    StarknetSierraCompilationError::InvalidEntryPointSignatureWrongBuiltinsOrder,
                );
            }

            let builtins =
                builtins.iter().map(|type_id| name_by_short_id[type_id.id].clone()).collect();

            let code_offset = cairo_program
                .debug_info
                .sierra_statement_info
                .get(statement_id.0)
                .ok_or(StarknetSierraCompilationError::EntryPointError)?
                .code_offset;
            assert_eq!(
                metadata.gas_info.function_costs[function.id.clone()],
                OrderedHashMap::from_iter([(CostTokenType::Const, ENTRY_POINT_COST as i64)]),
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

        let pythonic_hints = if add_pythonic_hints {
            Some(
                hints
                    .iter()
                    .map(|(pc, hints)| {
                        (*pc, hints.iter().map(|hint| hint.to_string()).collect_vec())
                    })
                    .collect_vec(),
            )
        } else {
            None
        };

        Ok(Self {
            prime,
            compiler_version: "1.0.0".to_string(),
            bytecode,
            hints,
            pythonic_hints,
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
