use std::cmp::Ordering;
use std::sync::LazyLock;

use cairo_lang_casm::assembler::AssembledCairoProgram;
use cairo_lang_casm::hints::{Hint, PythonicHint};
use cairo_lang_sierra::extensions::NamedType;
use cairo_lang_sierra::extensions::array::ArrayType;
use cairo_lang_sierra::extensions::bitwise::BitwiseType;
use cairo_lang_sierra::extensions::circuit::{AddModType, MulModType};
use cairo_lang_sierra::extensions::ec::EcOpType;
use cairo_lang_sierra::extensions::enm::EnumType;
use cairo_lang_sierra::extensions::felt252::Felt252Type;
use cairo_lang_sierra::extensions::gas::{CostTokenType, GasBuiltinType};
use cairo_lang_sierra::extensions::pedersen::PedersenType;
use cairo_lang_sierra::extensions::poseidon::PoseidonType;
use cairo_lang_sierra::extensions::range_check::{RangeCheck96Type, RangeCheckType};
use cairo_lang_sierra::extensions::segment_arena::SegmentArenaType;
use cairo_lang_sierra::extensions::snapshot::SnapshotType;
use cairo_lang_sierra::extensions::starknet::syscalls::SystemType;
use cairo_lang_sierra::extensions::structure::StructType;
use cairo_lang_sierra::ids::{ConcreteTypeId, GenericTypeId};
use cairo_lang_sierra::program::{ConcreteTypeLongId, GenericArg, TypeDeclaration};
use cairo_lang_sierra_to_casm::compiler::{
    CairoProgramDebugInfo, CompilationError, SierraToCasmConfig,
};
use cairo_lang_sierra_to_casm::metadata::{
    MetadataComputationConfig, MetadataError, calc_metadata,
};
use cairo_lang_utils::bigint::{BigUintAsHex, deserialize_big_uint, serialize_big_uint};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::require;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use cairo_lang_utils::unordered_hash_set::UnorderedHashSet;
use convert_case::{Case, Casing};
use itertools::{Itertools, chain};
use num_bigint::BigUint;
use num_integer::Integer;
use num_traits::Signed;
use serde::{Deserialize, Serialize};
use starknet_types_core::felt::Felt as Felt252;
use starknet_types_core::hash::{Poseidon, StarkHash};
use thiserror::Error;

use crate::allowed_libfuncs::AllowedLibfuncsError;
use crate::compiler_version::{
    CONTRACT_SEGMENTATION_MINOR_VERSION, VersionId, current_compiler_version_id,
    current_sierra_version_id,
};
use crate::contract_class::{ContractClass, ContractEntryPoint};
use crate::contract_segmentation::{
    NestedIntList, SegmentationError, compute_bytecode_segment_lengths,
};
use crate::felt252_serde::{Felt252SerdeError, sierra_from_felt252s};
use crate::keccak::starknet_keccak;

#[cfg(test)]
#[path = "casm_contract_class_test.rs"]
mod test;

/// The expected gas cost of an entrypoint.
pub const ENTRY_POINT_COST: i32 = 10000;

static CONSTRUCTOR_ENTRY_POINT_SELECTOR: LazyLock<BigUint> =
    LazyLock::new(|| starknet_keccak(b"constructor"));

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
    #[error(transparent)]
    SegmentationError(#[from] SegmentationError),
    #[error("Invalid entry point.")]
    EntryPointError,
    #[error("Missing arguments in the entry point.")]
    InvalidEntryPointSignatureMissingArgs,
    #[error("Invalid entry point signature.")]
    InvalidEntryPointSignature,
    #[error("Invalid constructor entry point.")]
    InvalidConstructorEntryPoint,
    #[error("{0} is not a supported builtin type.")]
    InvalidBuiltinType(ConcreteTypeId),
    #[error("Invalid entry point signature - builtins are not in the expected order.")]
    InvalidEntryPointSignatureWrongBuiltinsOrder,
    #[error("Entry points not sorted by selectors.")]
    EntryPointsOutOfOrder,
    #[error("Duplicate entry point selector {selector}.")]
    DuplicateEntryPointSelector { selector: BigUint },
    #[error("Duplicate entry point function index {index}.")]
    DuplicateEntryPointSierraFunction { index: usize },
    #[error("Out of range value in serialization.")]
    ValueOutOfRange,
    #[error(
        "Cannot compile Sierra version {version_in_contract} with the current compiler (sierra \
         version: {version_of_compiler})"
    )]
    UnsupportedSierraVersion { version_in_contract: VersionId, version_of_compiler: VersionId },
}

fn skip_if_none<T>(opt_field: &Option<T>) -> bool {
    opt_field.is_none()
}

/// Represents a contract in the Starknet network.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct CasmContractClass {
    #[serde(serialize_with = "serialize_big_uint", deserialize_with = "deserialize_big_uint")]
    pub prime: BigUint,
    pub compiler_version: String,
    pub bytecode: Vec<BigUintAsHex>,
    #[serde(skip_serializing_if = "skip_if_none")]
    pub bytecode_segment_lengths: Option<NestedIntList>,
    pub hints: Vec<(usize, Vec<Hint>)>,

    // Optional pythonic hints in a format that can be executed by the python vm.
    #[serde(skip_serializing_if = "skip_if_none")]
    pub pythonic_hints: Option<Vec<(usize, Vec<String>)>>,
    pub entry_points_by_type: CasmContractEntryPoints,
}
impl CasmContractClass {
    /// Returns the hash value for the compiled contract class.
    pub fn compiled_class_hash(&self) -> Felt252 {
        // Compute hashes on each component separately.
        let external_funcs_hash = self.entry_points_hash(&self.entry_points_by_type.external);
        let l1_handlers_hash = self.entry_points_hash(&self.entry_points_by_type.l1_handler);
        let constructors_hash = self.entry_points_hash(&self.entry_points_by_type.constructor);
        let bytecode_hash = self.compute_bytecode_hash();

        // Compute total hash by hashing each component on top of the previous one.
        Poseidon::hash_array(&[
            Felt252::from_bytes_be_slice(b"COMPILED_CLASS_V1"),
            external_funcs_hash,
            l1_handlers_hash,
            constructors_hash,
            bytecode_hash,
        ])
    }

    /// Returns the lengths of the bytecode segments.
    ///
    /// If the length field is missing, the entire bytecode is considered a single segment.
    pub fn get_bytecode_segment_lengths(&self) -> NestedIntList {
        self.bytecode_segment_lengths.clone().unwrap_or(NestedIntList::Leaf(self.bytecode.len()))
    }

    /// Returns the hash for a set of entry points.
    fn entry_points_hash(&self, entry_points: &[CasmContractEntryPoint]) -> Felt252 {
        let mut entry_point_hash_elements = vec![];
        for entry_point in entry_points {
            entry_point_hash_elements.push(Felt252::from(&entry_point.selector));
            entry_point_hash_elements.push(Felt252::from(entry_point.offset));
            entry_point_hash_elements.push(Poseidon::hash_array(
                &entry_point
                    .builtins
                    .iter()
                    .map(|builtin| Felt252::from_bytes_be_slice(builtin.as_bytes()))
                    .collect_vec(),
            ));
        }
        Poseidon::hash_array(&entry_point_hash_elements)
    }

    /// Returns the bytecode hash.
    fn compute_bytecode_hash(&self) -> Felt252 {
        let mut bytecode_iter = self.bytecode.iter().map(|big_uint| Felt252::from(&big_uint.value));

        let (len, bytecode_hash) =
            bytecode_hash_node(&mut bytecode_iter, &self.get_bytecode_segment_lengths());
        assert_eq!(len, self.bytecode.len());

        bytecode_hash
    }
}

/// Computes the hash of a bytecode segment. See the documentation of `bytecode_hash_node` in
/// the Starknet OS.
///
/// Returns the length of the processed segment and its hash.
fn bytecode_hash_node(
    iter: &mut impl Iterator<Item = Felt252>,
    node: &NestedIntList,
) -> (usize, Felt252) {
    match node {
        NestedIntList::Leaf(len) => {
            let data = &iter.take(*len).collect_vec();
            assert_eq!(data.len(), *len);
            (*len, Poseidon::hash_array(data))
        }
        NestedIntList::Node(nodes) => {
            // Compute `1 + poseidon(len0, hash0, len1, hash1, ...)`.
            let inner_nodes = nodes.iter().map(|node| bytecode_hash_node(iter, node)).collect_vec();
            let hash = Poseidon::hash_array(
                &inner_nodes.iter().flat_map(|(len, hash)| [(*len).into(), *hash]).collect_vec(),
            ) + 1;
            (inner_nodes.iter().map(|(len, _)| len).sum(), hash)
        }
    }
}

/// Context for resolving types.
pub struct TypeResolver<'a> {
    type_decl: &'a [TypeDeclaration],
}

impl TypeResolver<'_> {
    fn get_long_id(&self, type_id: &ConcreteTypeId) -> &ConcreteTypeLongId {
        &self.type_decl[type_id.id as usize].long_id
    }

    fn get_generic_id(&self, type_id: &ConcreteTypeId) -> &GenericTypeId {
        &self.get_long_id(type_id).generic_id
    }

    fn is_felt252_array_snapshot(&self, ty: &ConcreteTypeId) -> bool {
        let long_id = self.get_long_id(ty);
        if long_id.generic_id != SnapshotType::id() {
            return false;
        }

        let [GenericArg::Type(inner_ty)] = long_id.generic_args.as_slice() else {
            return false;
        };

        self.is_felt252_array(inner_ty)
    }

    fn is_felt252_array(&self, ty: &ConcreteTypeId) -> bool {
        let long_id = self.get_long_id(ty);
        if long_id.generic_id != ArrayType::id() {
            return false;
        }

        let [GenericArg::Type(element_ty)] = long_id.generic_args.as_slice() else {
            return false;
        };

        *self.get_generic_id(element_ty) == Felt252Type::id()
    }

    fn is_felt252_span(&self, ty: &ConcreteTypeId) -> bool {
        let long_id = self.get_long_id(ty);
        if long_id.generic_id != StructType::ID {
            return false;
        }

        let [GenericArg::UserType(_), GenericArg::Type(element_ty)] =
            long_id.generic_args.as_slice()
        else {
            return false;
        };

        self.is_felt252_array_snapshot(element_ty)
    }

    fn is_valid_entry_point_return_type(&self, ty: &ConcreteTypeId) -> bool {
        // The return type must be an enum with two variants: (result, error).
        let Some((result_tuple_ty, err_ty)) = self.extract_result_ty(ty) else {
            return false;
        };

        // The result variant must be a tuple with one element: Span<felt252>;
        let Some(result_ty) = self.extract_struct1(result_tuple_ty) else {
            return false;
        };
        if !self.is_felt252_span(result_ty) {
            return false;
        }

        // If the error type is Array<felt252>, it's a good error type, using the old panic
        // mechanism.
        if self.is_felt252_array(err_ty) {
            return true;
        }

        // Otherwise, the error type must be a struct with two fields: (panic, data)
        let Some((_panic_ty, err_data_ty)) = self.extract_struct2(err_ty) else {
            return false;
        };

        // The data field must be a Span<felt252>.
        self.is_felt252_array(err_data_ty)
    }

    /// Extracts types `TOk`, `TErr` from the type `Result<TOk, TErr>`.
    fn extract_result_ty(&self, ty: &ConcreteTypeId) -> Option<(&ConcreteTypeId, &ConcreteTypeId)> {
        let long_id = self.get_long_id(ty);
        require(long_id.generic_id == EnumType::id())?;
        let [GenericArg::UserType(_), GenericArg::Type(result_tuple_ty), GenericArg::Type(err_ty)] =
            long_id.generic_args.as_slice()
        else {
            return None;
        };
        Some((result_tuple_ty, err_ty))
    }

    /// Extracts type `T` from the tuple type `(T,)`.
    fn extract_struct1(&self, ty: &ConcreteTypeId) -> Option<&ConcreteTypeId> {
        let long_id = self.get_long_id(ty);
        require(long_id.generic_id == StructType::id())?;
        let [GenericArg::UserType(_), GenericArg::Type(ty0)] = long_id.generic_args.as_slice()
        else {
            return None;
        };
        Some(ty0)
    }

    /// Extracts types `T0`, `T1` from the tuple type `(T0, T1)`.
    fn extract_struct2(&self, ty: &ConcreteTypeId) -> Option<(&ConcreteTypeId, &ConcreteTypeId)> {
        let long_id = self.get_long_id(ty);
        require(long_id.generic_id == StructType::id())?;
        let [GenericArg::UserType(_), GenericArg::Type(ty0), GenericArg::Type(ty1)] =
            long_id.generic_args.as_slice()
        else {
            return None;
        };
        Some((ty0, ty1))
    }
}

impl CasmContractClass {
    pub fn from_contract_class(
        contract_class: ContractClass,
        add_pythonic_hints: bool,
        max_bytecode_size: usize,
    ) -> Result<Self, StarknetSierraCompilationError> {
        Ok(Self::from_contract_class_with_debug_info(
            contract_class,
            add_pythonic_hints,
            max_bytecode_size,
        )?
        .0)
    }

    pub fn from_contract_class_with_debug_info(
        contract_class: ContractClass,
        add_pythonic_hints: bool,
        max_bytecode_size: usize,
    ) -> Result<(Self, CairoProgramDebugInfo), StarknetSierraCompilationError> {
        let prime = Felt252::prime();
        for felt252 in &contract_class.sierra_program {
            if felt252.value >= prime {
                return Err(StarknetSierraCompilationError::ValueOutOfRange);
            }
        }

        let (sierra_version, _, program) = sierra_from_felt252s(&contract_class.sierra_program)?;
        let current_sierra_version = current_sierra_version_id();
        if !(sierra_version.major == current_sierra_version.major
            && sierra_version.minor <= current_sierra_version.minor)
        {
            return Err(StarknetSierraCompilationError::UnsupportedSierraVersion {
                version_in_contract: sierra_version,
                version_of_compiler: current_sierra_version,
            });
        }

        match &contract_class.entry_points_by_type.constructor.as_slice() {
            [] => {}
            [ContractEntryPoint { selector, .. }]
                if selector == &*CONSTRUCTOR_ENTRY_POINT_SELECTOR => {}
            _ => {
                return Err(StarknetSierraCompilationError::InvalidConstructorEntryPoint);
            }
        };

        for entry_points in [
            &contract_class.entry_points_by_type.constructor,
            &contract_class.entry_points_by_type.external,
            &contract_class.entry_points_by_type.l1_handler,
        ] {
            for (prev, next) in entry_points.iter().tuple_windows() {
                match prev.selector.cmp(&next.selector) {
                    Ordering::Less => {}
                    Ordering::Equal => {
                        return Err(StarknetSierraCompilationError::DuplicateEntryPointSelector {
                            selector: prev.selector.clone(),
                        });
                    }
                    Ordering::Greater => {
                        return Err(StarknetSierraCompilationError::EntryPointsOutOfOrder);
                    }
                }
            }
        }

        let entrypoint_function_indices = chain!(
            &contract_class.entry_points_by_type.constructor,
            &contract_class.entry_points_by_type.external,
            &contract_class.entry_points_by_type.l1_handler,
        )
        .map(|entrypoint| entrypoint.function_idx);
        // Count the number of times each function is used as an entry point.
        let mut function_idx_usages = UnorderedHashMap::<usize, usize>::default();
        for index in entrypoint_function_indices.clone() {
            let usages = function_idx_usages.entry(index).or_default();
            *usages += 1;
            const MAX_SIERRA_FUNCTION_USAGES: usize = 2;
            if *usages > MAX_SIERRA_FUNCTION_USAGES {
                return Err(StarknetSierraCompilationError::DuplicateEntryPointSierraFunction {
                    index,
                });
            }
        }
        let entrypoint_ids = entrypoint_function_indices.map(|idx| program.funcs[idx].id.clone());
        // TODO(lior): Remove this assert and condition once the equation solver is removed in major
        //   version 2.
        assert_eq!(sierra_version.major, 1);
        let no_eq_solver = sierra_version.minor >= 4;
        let metadata_computation_config = MetadataComputationConfig {
            function_set_costs: entrypoint_ids
                .map(|id| (id, [(CostTokenType::Const, ENTRY_POINT_COST)].into()))
                .collect(),
            linear_gas_solver: no_eq_solver,
            linear_ap_change_solver: no_eq_solver,
            skip_non_linear_solver_comparisons: false,
            compute_runtime_costs: false,
        };
        let metadata = calc_metadata(&program, metadata_computation_config)?;

        let cairo_program = cairo_lang_sierra_to_casm::compiler::compile(
            &program,
            &metadata,
            SierraToCasmConfig { gas_usage_check: true, max_bytecode_size },
        )?;

        let AssembledCairoProgram { bytecode, hints } = cairo_program.assemble();
        let bytecode = bytecode
            .iter()
            .map(|big_int| {
                let (_q, reminder) = big_int.magnitude().div_rem(&prime);
                BigUintAsHex {
                    value: if big_int.is_negative() { &prime - reminder } else { reminder },
                }
            })
            .collect_vec();

        let bytecode_segment_lengths =
            if sierra_version.minor >= CONTRACT_SEGMENTATION_MINOR_VERSION {
                Some(compute_bytecode_segment_lengths(&program, &cairo_program, bytecode.len())?)
            } else {
                None
            };

        let builtin_types = UnorderedHashSet::<GenericTypeId>::from_iter([
            RangeCheckType::id(),
            BitwiseType::id(),
            PedersenType::id(),
            EcOpType::id(),
            PoseidonType::id(),
            SegmentArenaType::id(),
            GasBuiltinType::id(),
            SystemType::id(),
            RangeCheck96Type::id(),
            AddModType::id(),
            MulModType::id(),
        ]);

        let as_casm_entry_point = |contract_entry_point: ContractEntryPoint| {
            let Some(function) = program.funcs.get(contract_entry_point.function_idx) else {
                return Err(StarknetSierraCompilationError::EntryPointError);
            };
            let statement_id = function.entry_point;

            // The expected return types are [builtins.., gas_builtin, system, PanicResult].
            let (panic_result, output_builtins) = function
                .signature
                .ret_types
                .split_last()
                .ok_or(StarknetSierraCompilationError::InvalidEntryPointSignature)?;
            let (builtins, [gas_ty, system_ty]) = output_builtins
                .split_last_chunk()
                .ok_or(StarknetSierraCompilationError::InvalidEntryPointSignature)?;
            let (input_span, input_builtins) = function
                .signature
                .param_types
                .split_last()
                .ok_or(StarknetSierraCompilationError::InvalidEntryPointSignatureMissingArgs)?;
            require(input_builtins == output_builtins)
                .ok_or(StarknetSierraCompilationError::InvalidEntryPointSignature)?;

            let type_resolver = TypeResolver { type_decl: &program.type_declarations };
            require(type_resolver.is_felt252_span(input_span))
                .ok_or(StarknetSierraCompilationError::InvalidEntryPointSignature)?;

            require(type_resolver.is_valid_entry_point_return_type(panic_result))
                .ok_or(StarknetSierraCompilationError::InvalidEntryPointSignature)?;

            for type_id in input_builtins.iter() {
                if !builtin_types.contains(type_resolver.get_generic_id(type_id)) {
                    return Err(StarknetSierraCompilationError::InvalidBuiltinType(
                        type_id.clone(),
                    ));
                }
            }

            // Check that the last builtins are gas and system.
            if *type_resolver.get_generic_id(system_ty) != SystemType::id()
                || *type_resolver.get_generic_id(gas_ty) != GasBuiltinType::id()
            {
                return Err(
                    StarknetSierraCompilationError::InvalidEntryPointSignatureWrongBuiltinsOrder,
                );
            }

            let builtins = builtins
                .iter()
                .map(|type_id| match type_resolver.get_generic_id(type_id).0.as_str() {
                    "RangeCheck96" => "range_check96".to_string(),
                    name => name.to_case(Case::Snake),
                })
                .collect_vec();

            let code_offset = cairo_program
                .debug_info
                .sierra_statement_info
                .get(statement_id.0)
                .ok_or(StarknetSierraCompilationError::EntryPointError)?
                .start_offset;
            assert_eq!(
                metadata.gas_info.function_costs[&function.id],
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
                        (*pc, hints.iter().map(|hint| hint.get_pythonic_hint()).collect_vec())
                    })
                    .collect_vec(),
            )
        } else {
            None
        };

        let compiler_version = current_compiler_version_id().to_string();
        let casm_contract_class = Self {
            prime,
            compiler_version,
            bytecode,
            bytecode_segment_lengths,
            hints,
            pythonic_hints,
            entry_points_by_type: CasmContractEntryPoints {
                external: as_casm_entry_points(contract_class.entry_points_by_type.external)?,
                l1_handler: as_casm_entry_points(contract_class.entry_points_by_type.l1_handler)?,
                constructor: as_casm_entry_points(contract_class.entry_points_by_type.constructor)?,
            },
        };

        Ok((casm_contract_class, cairo_program.debug_info))
    }
}

#[derive(Clone, Default, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct CasmContractEntryPoint {
    /// A field element that encodes the signature of the called function.
    #[serde(serialize_with = "serialize_big_uint", deserialize_with = "deserialize_big_uint")]
    pub selector: BigUint,
    /// The offset of the instruction that should be called within the contract bytecode.
    pub offset: usize,
    // list of builtins.
    pub builtins: Vec<String>,
}

#[derive(Clone, Default, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct CasmContractEntryPoints {
    #[serde(rename = "EXTERNAL")]
    pub external: Vec<CasmContractEntryPoint>,
    #[serde(rename = "L1_HANDLER")]
    pub l1_handler: Vec<CasmContractEntryPoint>,
    #[serde(rename = "CONSTRUCTOR")]
    pub constructor: Vec<CasmContractEntryPoint>,
}
