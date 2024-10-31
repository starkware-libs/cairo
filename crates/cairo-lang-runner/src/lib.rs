//! Basic runner for running a Sierra program on the vm.
use std::collections::{HashMap, HashSet};
use std::ops::{Add, Sub};

use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::cell_expression::CellExpression;
use cairo_lang_casm::hints::Hint;
use cairo_lang_casm::instructions::Instruction;
use cairo_lang_casm::operand::{CellRef, Register};
use cairo_lang_casm::{casm, casm_build_extend};
use cairo_lang_sierra::extensions::bitwise::BitwiseType;
use cairo_lang_sierra::extensions::circuit::{AddModType, MulModType};
use cairo_lang_sierra::extensions::core::{CoreConcreteLibfunc, CoreLibfunc, CoreType};
use cairo_lang_sierra::extensions::ec::EcOpType;
use cairo_lang_sierra::extensions::enm::EnumType;
use cairo_lang_sierra::extensions::gas::{CostTokenType, GasBuiltinType};
use cairo_lang_sierra::extensions::pedersen::PedersenType;
use cairo_lang_sierra::extensions::poseidon::PoseidonType;
use cairo_lang_sierra::extensions::range_check::{RangeCheck96Type, RangeCheckType};
use cairo_lang_sierra::extensions::segment_arena::SegmentArenaType;
use cairo_lang_sierra::extensions::starknet::syscalls::SystemType;
use cairo_lang_sierra::extensions::{ConcreteType, NamedType};
use cairo_lang_sierra::ids::{ConcreteTypeId, GenericTypeId};
use cairo_lang_sierra::program::{Function, GenStatement, GenericArg, StatementIdx};
use cairo_lang_sierra::program_registry::{ProgramRegistry, ProgramRegistryError};
use cairo_lang_sierra_ap_change::ApChangeError;
use cairo_lang_sierra_gas::CostError;
use cairo_lang_sierra_to_casm::compiler::{CairoProgram, CompilationError, SierraToCasmConfig};
use cairo_lang_sierra_to_casm::metadata::{
    Metadata, MetadataComputationConfig, MetadataError, calc_metadata, calc_metadata_ap_change_only,
};
use cairo_lang_sierra_type_size::{TypeSizeMap, get_type_size_map};
use cairo_lang_starknet::contract::ContractInfo;
use cairo_lang_utils::casts::IntoOrPanic;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use cairo_lang_utils::{extract_matches, require};
use cairo_vm::hint_processor::hint_processor_definition::HintProcessor;
use cairo_vm::serde::deserialize_program::HintParams;
use cairo_vm::types::builtin_name::BuiltinName;
use cairo_vm::vm::errors::cairo_run_errors::CairoRunError;
use cairo_vm::vm::errors::memory_errors::MemoryError;
use cairo_vm::vm::runners::cairo_runner::{ExecutionResources, RunResources};
use cairo_vm::vm::trace::trace_entry::RelocatedTraceEntry;
use cairo_vm::vm::vm_core::VirtualMachine;
use casm_run::hint_to_hint_params;
pub use casm_run::{CairoHintProcessor, StarknetState};
use itertools::{Itertools, chain};
use num_bigint::BigInt;
use num_traits::ToPrimitive;
use profiling::{ProfilingInfo, user_function_idx_by_sierra_statement_idx};
use starknet_types_core::felt::Felt as Felt252;
use thiserror::Error;

use crate::casm_run::RunFunctionResult;

pub mod casm_run;
pub mod profiling;
pub mod short_string;

const MAX_STACK_TRACE_DEPTH_DEFAULT: usize = 100;

#[derive(Debug, Error)]
pub enum RunnerError {
    #[error("Not enough gas to call function.")]
    NotEnoughGasToCall,
    #[error(
        "Failed calculating gas usage, it is likely a call for `gas::withdraw_gas` is missing. \
         Inner error: {0}"
    )]
    FailedGasCalculation(#[from] CostError),
    #[error("Function with suffix `{suffix}` to run not found.")]
    MissingFunction { suffix: String },
    #[error("Function param {param_index} only partially contains argument {arg_index}.")]
    ArgumentUnaligned { param_index: usize, arg_index: usize },
    #[error("Function expects arguments of size {expected} and received {actual} instead.")]
    ArgumentsSizeMismatch { expected: usize, actual: usize },
    #[error(transparent)]
    ProgramRegistryError(#[from] Box<ProgramRegistryError>),
    #[error(transparent)]
    SierraCompilationError(#[from] Box<CompilationError>),
    #[error(transparent)]
    ApChangeError(#[from] ApChangeError),
    #[error(transparent)]
    CairoRunError(#[from] Box<CairoRunError>),
}

/// The full result of a run with Starknet state.
pub struct RunResultStarknet {
    pub gas_counter: Option<Felt252>,
    pub memory: Vec<Option<Felt252>>,
    pub value: RunResultValue,
    pub starknet_state: StarknetState,
    pub used_resources: StarknetExecutionResources,
    /// The profiling info of the run, if requested.
    pub profiling_info: Option<ProfilingInfo>,
}

/// The full result of a run.
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct RunResult {
    pub gas_counter: Option<Felt252>,
    pub memory: Vec<Option<Felt252>>,
    pub value: RunResultValue,
    pub used_resources: ExecutionResources,
    /// The profiling info of the run, if requested.
    pub profiling_info: Option<ProfilingInfo>,
}

/// The execution resources in a run.
/// Extends [ExecutionResources] by including the used syscalls for starknet.
#[derive(Debug, Eq, PartialEq, Clone, Default)]
pub struct StarknetExecutionResources {
    /// The basic execution resources.
    pub basic_resources: ExecutionResources,
    /// The used syscalls.
    pub syscalls: HashMap<String, usize>,
}

impl std::ops::AddAssign<StarknetExecutionResources> for StarknetExecutionResources {
    /// Adds the resources of `other` to `self`.
    fn add_assign(&mut self, other: Self) {
        self.basic_resources += &other.basic_resources;
        for (k, v) in other.syscalls {
            *self.syscalls.entry(k).or_default() += v;
        }
    }
}

/// The ran function return value.
#[derive(Debug, Eq, PartialEq, Clone)]
pub enum RunResultValue {
    /// Run ended successfully, returning the memory of the non-implicit returns.
    Success(Vec<Felt252>),
    /// Run panicked, returning the carried error data.
    Panic(Vec<Felt252>),
}

// Approximated costs token types.
pub fn token_gas_cost(token_type: CostTokenType) -> usize {
    match token_type {
        CostTokenType::Const => 1,
        CostTokenType::Step
        | CostTokenType::Hole
        | CostTokenType::RangeCheck
        | CostTokenType::RangeCheck96 => {
            panic!("Token type {:?} has no gas cost.", token_type)
        }
        CostTokenType::Pedersen => 4050,
        CostTokenType::Poseidon => 491,
        CostTokenType::Bitwise => 583,
        CostTokenType::EcOp => 4085,
        CostTokenType::AddMod => 230,
        CostTokenType::MulMod => 604,
    }
}

/// An argument to a sierra function run,
#[derive(Debug)]
pub enum Arg {
    Value(Felt252),
    Array(Vec<Arg>),
}
impl Arg {
    /// Returns the size of the argument in the vm.
    pub fn size(&self) -> usize {
        match self {
            Self::Value(_) => 1,
            Self::Array(_) => 2,
        }
    }
}
impl From<Felt252> for Arg {
    fn from(value: Felt252) -> Self {
        Self::Value(value)
    }
}

/// Builds hints_dict required in cairo_vm::types::program::Program from instructions.
pub fn build_hints_dict(
    hints: &[(usize, Vec<Hint>)],
) -> (HashMap<usize, Vec<HintParams>>, HashMap<String, Hint>) {
    let mut hints_dict: HashMap<usize, Vec<HintParams>> = HashMap::new();
    let mut string_to_hint: HashMap<String, Hint> = HashMap::new();

    for (offset, offset_hints) in hints {
        // Register hint with string for the hint processor.
        for hint in offset_hints {
            string_to_hint.insert(hint.representing_string(), hint.clone());
        }
        // Add hint, associated with the instruction offset.
        hints_dict.insert(*offset, offset_hints.iter().map(hint_to_hint_params).collect());
    }
    (hints_dict, string_to_hint)
}

/// Runner enabling running a Sierra program on the vm.
pub struct SierraCasmRunner {
    /// The sierra program.
    sierra_program: cairo_lang_sierra::program::Program,
    /// Metadata for the Sierra program.
    metadata: Metadata,
    /// Program registry for the Sierra program.
    sierra_program_registry: ProgramRegistry<CoreType, CoreLibfunc>,
    /// Program registry for the Sierra program.
    type_sizes: TypeSizeMap,
    /// The casm program matching the Sierra code.
    casm_program: CairoProgram,
    /// Mapping from class_hash to contract info.
    starknet_contracts_info: OrderedHashMap<Felt252, ContractInfo>,
    /// Whether to run the profiler when running using this runner.
    run_profiler: Option<ProfilingInfoCollectionConfig>,
}
impl SierraCasmRunner {
    pub fn new(
        sierra_program: cairo_lang_sierra::program::Program,
        metadata_config: Option<MetadataComputationConfig>,
        starknet_contracts_info: OrderedHashMap<Felt252, ContractInfo>,
        run_profiler: Option<ProfilingInfoCollectionConfig>,
    ) -> Result<Self, RunnerError> {
        let gas_usage_check = metadata_config.is_some();
        let metadata = create_metadata(&sierra_program, metadata_config)?;
        let sierra_program_registry =
            ProgramRegistry::<CoreType, CoreLibfunc>::new(&sierra_program)?;
        let type_sizes = get_type_size_map(&sierra_program, &sierra_program_registry).unwrap();
        let casm_program = cairo_lang_sierra_to_casm::compiler::compile(
            &sierra_program,
            &metadata,
            SierraToCasmConfig { gas_usage_check, max_bytecode_size: usize::MAX },
        )?;

        // Find all contracts.
        Ok(Self {
            sierra_program,
            metadata,
            sierra_program_registry,
            type_sizes,
            casm_program,
            starknet_contracts_info,
            run_profiler,
        })
    }

    /// Runs the vm starting from a function in the context of a given starknet state.
    pub fn run_function_with_starknet_context(
        &self,
        func: &Function,
        args: &[Arg],
        available_gas: Option<usize>,
        starknet_state: StarknetState,
    ) -> Result<RunResultStarknet, RunnerError> {
        let initial_gas = self.get_initial_available_gas(func, available_gas)?;
        let (entry_code, builtins) = self.create_entry_code(func)?;
        let footer = Self::create_code_footer();
        let assembled_program = self.casm_program.clone().assemble_ex(&entry_code, &footer);
        let (hints_dict, string_to_hint) = build_hints_dict(&assembled_program.hints);

        let mut hint_processor = CairoHintProcessor {
            runner: Some(self),
            starknet_state,
            string_to_hint,
            run_resources: RunResources::default(),
            syscalls_used_resources: Default::default(),
        };
        let RunResult { gas_counter, memory, value, used_resources, profiling_info } = self
            .run_function(
                func,
                (initial_gas, args),
                &mut hint_processor,
                hints_dict,
                assembled_program.bytecode.iter(),
                builtins,
            )?;
        let mut all_used_resources = hint_processor.syscalls_used_resources;
        all_used_resources.basic_resources += &used_resources;
        // Remove the used resources of generated entry code - as it isn't a real part of the run.
        // The used resources of the code is just its length, as it is a simple param setup, and a
        // call and ret.
        all_used_resources.basic_resources.n_steps -= entry_code.len();
        Ok(RunResultStarknet {
            gas_counter,
            memory,
            value,
            starknet_state: hint_processor.starknet_state,
            used_resources: all_used_resources,
            profiling_info,
        })
    }

    /// Collects profiling info of the current run using the trace.
    fn collect_profiling_info(
        &self,
        trace: &[RelocatedTraceEntry],
        profiling_config: ProfilingInfoCollectionConfig,
    ) -> ProfilingInfo {
        let sierra_len = self.casm_program.debug_info.sierra_statement_info.len();
        let bytecode_len =
            self.casm_program.debug_info.sierra_statement_info.last().unwrap().end_offset;
        // The CASM program starts with a header of instructions to wrap the real program.
        // `real_pc_0` is the PC in the trace that points to the same CASM instruction which is in
        // the real PC=0 in the original CASM program. That is, all trace's PCs need to be
        // subtracted by `real_pc_0` to get the real PC they point to in the original CASM
        // program.
        // This is the same as the PC of the last trace entry plus 1, as the header is built to have
        // a `ret` last instruction, which must be the last in the trace of any execution.
        // The first instruction after that is the first instruction in the original CASM program.

        let real_pc_0 = trace.last().unwrap().pc.add(1);

        // The function stack trace of the current function, excluding the current function (that
        // is, the stack of the caller). Represented as a vector of indices of the functions
        // in the stack (indices of the functions according to the list in the sierra program).
        // Limited to depth `max_stack_trace_depth`. Note `function_stack_depth` tracks the real
        // depth, even if >= `max_stack_trace_depth`.
        let mut function_stack = Vec::new();
        // Tracks the depth of the function stack, without limit. This is usually equal to
        // `function_stack.len()`, but if the actual stack is deeper than `max_stack_trace_depth`,
        // this remains reliable while `function_stack` does not.
        let mut function_stack_depth = 0;
        let mut cur_weight = 0;
        // The key is a function stack trace (see `function_stack`, but including the current
        // function).
        // The value is the weight of the stack trace so far, not including the pending weight being
        // tracked at the time.
        let mut stack_trace_weights = OrderedHashMap::default();
        let mut end_of_program_reached = false;
        // The total weight of each Sierra statement.
        // Note the header and footer (CASM instructions added for running the program by the
        // runner). The header is not counted, and the footer is, but then the relevant
        // entry is removed.
        let mut sierra_statement_weights = UnorderedHashMap::default();
        for step in trace.iter() {
            // Skip the header.
            if step.pc < real_pc_0 {
                continue;
            }
            let real_pc: usize = step.pc.sub(real_pc_0);
            // Skip the footer.
            // Also if pc is greater or equal the bytecode length it means that it is the outside
            // ret used for e.g. getting pointer to builtins costs table, const segments
            // etc.
            if real_pc >= bytecode_len {
                continue;
            }

            if end_of_program_reached {
                unreachable!("End of program reached, but trace continues.");
            }

            cur_weight += 1;

            // TODO(yuval): Maintain a map of pc to sierra statement index (only for PCs we saw), to
            // save lookups.
            let sierra_statement_idx = self.sierra_statement_index_by_pc(real_pc);
            let user_function_idx = user_function_idx_by_sierra_statement_idx(
                &self.sierra_program,
                sierra_statement_idx,
            );

            *sierra_statement_weights.entry(sierra_statement_idx).or_insert(0) += 1;

            let Some(gen_statement) = self.sierra_program.statements.get(sierra_statement_idx.0)
            else {
                panic!("Failed fetching statement index {}", sierra_statement_idx.0);
            };

            match gen_statement {
                GenStatement::Invocation(invocation) => {
                    if matches!(
                        self.sierra_program_registry.get_libfunc(&invocation.libfunc_id),
                        Ok(CoreConcreteLibfunc::FunctionCall(_))
                    ) {
                        // Push to the stack.
                        if function_stack_depth < profiling_config.max_stack_trace_depth {
                            function_stack.push((user_function_idx, cur_weight));
                            cur_weight = 0;
                        }
                        function_stack_depth += 1;
                    }
                }
                GenStatement::Return(_) => {
                    // Pop from the stack.
                    if function_stack_depth <= profiling_config.max_stack_trace_depth {
                        // The current stack trace, including the current function.
                        let cur_stack: Vec<_> =
                            chain!(function_stack.iter().map(|f| f.0), [user_function_idx])
                                .collect();
                        *stack_trace_weights.entry(cur_stack).or_insert(0) += cur_weight;

                        let Some(popped) = function_stack.pop() else {
                            // End of the program.
                            end_of_program_reached = true;
                            continue;
                        };
                        cur_weight += popped.1;
                    }
                    function_stack_depth -= 1;
                }
            }
        }

        // Remove the footer.
        sierra_statement_weights.remove(&StatementIdx(sierra_len));

        ProfilingInfo { sierra_statement_weights, stack_trace_weights }
    }

    fn sierra_statement_index_by_pc(&self, pc: usize) -> StatementIdx {
        // the `-1` here can't cause an underflow as the first statement is always at
        // offset 0, so it is always on the left side of the
        // partition, and thus the partition index is >0.
        StatementIdx(
            self.casm_program
                .debug_info
                .sierra_statement_info
                .partition_point(|x| x.start_offset <= pc)
                - 1,
        )
    }

    /// Extract inner type if `ty` is a panic wrapper
    fn inner_type_from_panic_wrapper(
        &self,
        ty: &GenericTypeId,
        func: &Function,
    ) -> Option<ConcreteTypeId> {
        let info = func
            .signature
            .ret_types
            .iter()
            .find_map(|rt| {
                let info = self.get_info(rt);
                (info.long_id.generic_id == *ty).then_some(info)
            })
            .unwrap();

        if *ty == EnumType::ID
            && matches!(&info.long_id.generic_args[0], GenericArg::UserType(ut)
                if ut.debug_name.as_ref().unwrap().starts_with("core::panics::PanicResult::"))
        {
            return Some(extract_matches!(&info.long_id.generic_args[1], GenericArg::Type).clone());
        }
        None
    }

    /// Runs the vm starting from a function with custom hint processor. Function may have
    /// implicits, but no other ref params. The cost of the function is deducted from
    /// `available_gas` before the execution begins.
    pub fn run_function<'a, Bytecode>(
        &self,
        func: &Function,
        (available_gas, args): (usize, &[Arg]),
        hint_processor: &mut dyn HintProcessor,
        hints_dict: HashMap<usize, Vec<HintParams>>,
        bytecode: Bytecode,
        builtins: Vec<BuiltinName>,
    ) -> Result<RunResult, RunnerError>
    where
        Bytecode: ExactSizeIterator<Item = &'a BigInt> + Clone,
    {
        self.validate_args(func, args)?;
        let return_types = self.generic_id_and_size_from_concrete(&func.signature.ret_types);
        let data_len = bytecode.len();
        let gas = self.requires_gas_builtin(func).then_some(available_gas);
        let RunFunctionResult { ap, used_resources, memory, relocated_trace } =
            casm_run::run_function(
                bytecode,
                builtins,
                |vm| initialize_vm(vm, gas, args, data_len),
                hint_processor,
                hints_dict,
            )?;

        let (results_data, gas_counter) = Self::get_results_data(&return_types, &memory, ap);
        assert!(results_data.len() <= 1);

        let value = if results_data.is_empty() {
            // No result type - no panic.
            RunResultValue::Success(vec![])
        } else {
            let (ty, values) = results_data[0].clone();
            let inner_ty =
                self.inner_type_from_panic_wrapper(&ty, func).map(|it| self.type_sizes[&it]);
            Self::handle_main_return_value(inner_ty, values, &memory)
        };

        let profiling_info = self
            .run_profiler
            .as_ref()
            .map(|config| self.collect_profiling_info(&relocated_trace, config.clone()));

        Ok(RunResult { gas_counter, memory, value, used_resources, profiling_info })
    }

    /// Validates the arguments given shallowly matches the parameters of a function.
    fn validate_args(&self, func: &Function, args: &[Arg]) -> Result<(), RunnerError> {
        let non_args_params = HashSet::from([
            AddModType::ID,
            BitwiseType::ID,
            GasBuiltinType::ID,
            EcOpType::ID,
            MulModType::ID,
            PedersenType::ID,
            PoseidonType::ID,
            RangeCheck96Type::ID,
            RangeCheckType::ID,
            SegmentArenaType::ID,
            SystemType::ID,
        ]);
        let mut expected_arguments_size = 0;
        let mut arg_iter = args.iter().enumerate();
        for (param_index, (_, param_size)) in self
            .generic_id_and_size_from_concrete(&func.signature.param_types)
            .into_iter()
            .filter(|(ty, _)| !non_args_params.contains(ty))
            .enumerate()
        {
            let param_size: usize = param_size.into_or_panic();
            expected_arguments_size += param_size;
            let mut taken_size = 0;
            while taken_size < param_size {
                let Some((arg_index, arg)) = arg_iter.next() else {
                    break;
                };
                taken_size += arg.size();
                if taken_size > param_size {
                    return Err(RunnerError::ArgumentUnaligned { param_index, arg_index });
                }
            }
        }
        let actual_args_size = args_size(args);
        if expected_arguments_size != actual_args_size {
            return Err(RunnerError::ArgumentsSizeMismatch {
                expected: expected_arguments_size,
                actual: actual_args_size,
            });
        }
        Ok(())
    }

    /// Handling the main return value to create a `RunResultValue`.
    pub fn handle_main_return_value(
        inner_type_size: Option<i16>,
        values: Vec<Felt252>,
        cells: &[Option<Felt252>],
    ) -> RunResultValue {
        if let Some(inner_type_size) = inner_type_size {
            // The function includes a panic wrapper.
            if values[0] == Felt252::from(0) {
                // The run resulted successfully, returning the inner value.
                let inner_ty_size = inner_type_size as usize;
                let skip_size = values.len() - inner_ty_size;
                RunResultValue::Success(values.into_iter().skip(skip_size).collect())
            } else {
                // The run resulted in a panic, returning the error data.
                let err_data_start = values[values.len() - 2].to_usize().unwrap();
                let err_data_end = values[values.len() - 1].to_usize().unwrap();
                RunResultValue::Panic(
                    cells[err_data_start..err_data_end]
                        .iter()
                        .cloned()
                        .map(Option::unwrap)
                        .collect(),
                )
            }
        } else {
            // No panic wrap - so always successful.
            RunResultValue::Success(values)
        }
    }

    /// Returns the final values and type of all `func`s returning variables.
    pub fn get_results_data(
        return_types: &[(GenericTypeId, i16)],
        cells: &[Option<Felt252>],
        mut ap: usize,
    ) -> (Vec<(GenericTypeId, Vec<Felt252>)>, Option<Felt252>) {
        let mut results_data = vec![];
        for (ty, ty_size) in return_types.iter().rev() {
            let size = *ty_size as usize;
            let values: Vec<Felt252> =
                ((ap - size)..ap).map(|index| cells[index].unwrap()).collect();
            ap -= size;
            results_data.push((ty.clone(), values));
        }

        // Handling implicits.
        let mut gas_counter = None;
        results_data.retain_mut(|(ty, values)| {
            let generic_ty = ty;
            if *generic_ty == GasBuiltinType::ID {
                gas_counter = Some(values.remove(0));
                assert!(values.is_empty());
                false
            } else {
                *generic_ty != RangeCheckType::ID
                    && *generic_ty != BitwiseType::ID
                    && *generic_ty != EcOpType::ID
                    && *generic_ty != PedersenType::ID
                    && *generic_ty != PoseidonType::ID
                    && *generic_ty != SystemType::ID
                    && *generic_ty != SegmentArenaType::ID
                    && *generic_ty != RangeCheck96Type::ID
                    && *generic_ty != AddModType::ID
                    && *generic_ty != MulModType::ID
            }
        });

        (results_data, gas_counter)
    }

    /// Finds first function ending with `name_suffix`.
    pub fn find_function(&self, name_suffix: &str) -> Result<&Function, RunnerError> {
        self.sierra_program
            .funcs
            .iter()
            .find(|f| {
                if let Some(name) = &f.id.debug_name { name.ends_with(name_suffix) } else { false }
            })
            .ok_or_else(|| RunnerError::MissingFunction { suffix: name_suffix.to_owned() })
    }

    /// Converts array of `ConcreteTypeId`s into corresponding `GenericTypeId`s and their sizes
    fn generic_id_and_size_from_concrete(
        &self,
        types: &[ConcreteTypeId],
    ) -> Vec<(GenericTypeId, i16)> {
        types
            .iter()
            .map(|pt| {
                let info = self.get_info(pt);
                let generic_id = &info.long_id.generic_id;
                let size = self.type_sizes[pt];
                (generic_id.clone(), size)
            })
            .collect()
    }

    /// Returns whether the gas builtin is required in the given function.
    fn requires_gas_builtin(&self, func: &Function) -> bool {
        func.signature
            .param_types
            .iter()
            .any(|ty| self.get_info(ty).long_id.generic_id == GasBuiltinType::ID)
    }

    fn get_info(
        &self,
        ty: &cairo_lang_sierra::ids::ConcreteTypeId,
    ) -> &cairo_lang_sierra::extensions::types::TypeInfo {
        self.sierra_program_registry.get_type(ty).unwrap().info()
    }

    pub fn create_entry_code_from_params(
        param_types: &[(GenericTypeId, i16)],
        code_offset: usize,
    ) -> Result<(Vec<Instruction>, Vec<BuiltinName>), RunnerError> {
        let mut ctx = CasmBuilder::default();
        let mut builtin_offset = 3;
        let mut builtin_vars = HashMap::new();
        let mut builtins = vec![];
        for (builtin_name, builtin_ty) in [
            (BuiltinName::mul_mod, MulModType::ID),
            (BuiltinName::add_mod, AddModType::ID),
            (BuiltinName::range_check96, RangeCheck96Type::ID),
            (BuiltinName::poseidon, PoseidonType::ID),
            (BuiltinName::ec_op, EcOpType::ID),
            (BuiltinName::bitwise, BitwiseType::ID),
            (BuiltinName::range_check, RangeCheckType::ID),
            (BuiltinName::pedersen, PedersenType::ID),
        ] {
            if param_types.iter().any(|(ty, _)| ty == &builtin_ty) {
                // The offset [fp - i] for each of this builtins in this configuration.
                builtin_vars.insert(
                    builtin_ty,
                    ctx.add_var(CellExpression::Deref(CellRef {
                        register: Register::FP,
                        offset: -builtin_offset,
                    })),
                );
                builtin_offset += 1;
                builtins.push(builtin_name);
            }
        }
        builtins.reverse();

        let emulated_builtins = HashSet::from([SystemType::ID]);

        let mut args_vars = vec![];
        for (ty, size) in param_types {
            if !builtin_vars.contains_key(ty)
                && !emulated_builtins.contains(ty)
                && ty != &SegmentArenaType::ID
            {
                args_vars.push((0..*size).map(|_| ctx.alloc_var(false)).collect_vec());
            }
        }
        // Giving space for the VM to fill the arguments.
        casm_build_extend!(ctx, ap += args_vars.iter().map(Vec::len).sum(););
        if param_types.iter().any(|(ty, _)| ty == &SegmentArenaType::ID) {
            casm_build_extend! {ctx,
                tempvar segment_arena;
                tempvar infos;
                hint AllocSegment {} into {dst: segment_arena};
                hint AllocSegment {} into {dst: infos};
                const czero = 0;
                tempvar zero = czero;
                // Write Infos segment, n_constructed (0), and n_destructed (0) to the segment.
                assert infos = *(segment_arena++);
                assert zero = *(segment_arena++);
                assert zero = *(segment_arena++);
            }
            // Adding the segment arena to the builtins var map.
            builtin_vars.insert(SegmentArenaType::ID, segment_arena);
        }
        let mut args_vars_iter = args_vars.into_iter();
        for (generic_ty, _) in param_types {
            if let Some(var) = builtin_vars.get(generic_ty).cloned() {
                casm_build_extend!(ctx, tempvar _builtin = var;);
            } else if emulated_builtins.contains(generic_ty) {
                casm_build_extend! {ctx,
                    tempvar system;
                    hint AllocSegment {} into {dst: system};
                    ap += 1;
                };
            } else {
                for cell in args_vars_iter.next().unwrap() {
                    casm_build_extend!(ctx, tempvar _cell = cell;);
                }
            };
        }
        casm_build_extend! {ctx,
            let () = call FUNCTION;
            ret;
        };
        ctx.future_label("FUNCTION".into(), code_offset);
        Ok((ctx.build([]).instructions, builtins))
    }

    /// Returns the instructions to add to the beginning of the code to successfully call the main
    /// function, as well as the builtins required to execute the program.
    pub fn create_entry_code(
        &self,
        func: &Function,
    ) -> Result<(Vec<Instruction>, Vec<BuiltinName>), RunnerError> {
        let params = self.generic_id_and_size_from_concrete(&func.signature.param_types);

        let entry_point = func.entry_point.0;
        let code_offset =
            self.casm_program.debug_info.sierra_statement_info[entry_point].start_offset;

        Self::create_entry_code_from_params(&params, code_offset)
    }

    /// Returns the initial value for the gas counter.
    /// If `available_gas` is None returns 0.
    pub fn get_initial_available_gas(
        &self,
        func: &Function,
        available_gas: Option<usize>,
    ) -> Result<usize, RunnerError> {
        let Some(available_gas) = available_gas else {
            return Ok(0);
        };

        // In case we don't have any costs - it means no gas equations were solved (and we are in
        // the case of no gas checking enabled) - so the gas builtin is irrelevant, and we
        // can return any value.
        let Some(required_gas) = self.initial_required_gas(func) else {
            return Ok(0);
        };

        available_gas.checked_sub(required_gas).ok_or(RunnerError::NotEnoughGasToCall)
    }

    pub fn initial_required_gas(&self, func: &Function) -> Option<usize> {
        require(!self.metadata.gas_info.function_costs.is_empty())?;
        Some(
            self.metadata.gas_info.function_costs[&func.id]
                .iter()
                .map(|(token_type, val)| val.into_or_panic::<usize>() * token_gas_cost(*token_type))
                .sum(),
        )
    }

    /// Creates a list of instructions that will be appended to the program's bytecode.
    pub fn create_code_footer() -> Vec<Instruction> {
        casm! {
            // Add a `ret` instruction used in libfuncs that retrieve the current value of the `fp`
            // and `pc` registers.
            ret;
        }
        .instructions
    }

    pub fn get_casm_program(&self) -> &CairoProgram {
        &self.casm_program
    }
}

/// Configuration for the profiling info collection phase.
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ProfilingInfoCollectionConfig {
    /// The maximum depth of the stack trace to collect.
    max_stack_trace_depth: usize,
}

impl ProfilingInfoCollectionConfig {
    pub fn set_max_stack_trace_depth(&mut self, max_stack_depth: usize) -> &mut Self {
        self.max_stack_trace_depth = max_stack_depth;
        self
    }
}

impl Default for ProfilingInfoCollectionConfig {
    // TODO(yuval): consider changing this setting to use flags.
    /// Gets the max_stack_trace_depth according to the environment variable
    /// `MAX_STACK_TRACE_DEPTH`, if set.
    fn default() -> Self {
        Self {
            max_stack_trace_depth: if let Ok(max) = std::env::var("MAX_STACK_TRACE_DEPTH") {
                if max.is_empty() {
                    MAX_STACK_TRACE_DEPTH_DEFAULT
                } else {
                    max.parse::<usize>()
                        .expect("MAX_STACK_TRACE_DEPTH_DEFAULT env var is not numeric")
                }
            } else {
                MAX_STACK_TRACE_DEPTH_DEFAULT
            },
        }
    }
}

/// Initializes a vm by adding a new segment with builtins cost and a necessary pointer at the end
/// of the program, as well as placing the arguments at the initial ap values.
pub fn initialize_vm(
    vm: &mut VirtualMachine,
    gas_value: Option<usize>,
    args: &[Arg],
    data_len: usize,
) -> Result<(), Box<CairoRunError>> {
    // Create the builtin cost segment, with dummy values.
    let builtin_cost_segment = vm.add_memory_segment();
    let mut ap = vm.get_ap();
    if let Some(value) = gas_value {
        // Adding the gas as the first argument.
        vm.insert_value(ap, value).map_err(|e| Box::new(e.into()))?;
        ap += 1;
    }
    let mut stack = vec![(ap, args)];
    while let Some((mut buffer, values)) = stack.pop() {
        for value in values {
            match value {
                Arg::Value(v) => {
                    vm.insert_value(buffer, v).map_err(|e| Box::new(e.into()))?;
                    buffer += 1;
                }
                Arg::Array(arr) => {
                    let arr_buffer = vm.add_memory_segment();
                    stack.push((arr_buffer, arr));
                    vm.insert_value(buffer, arr_buffer).map_err(|e| Box::new(e.into()))?;
                    buffer += 1;
                    vm.insert_value(
                        buffer,
                        (arr_buffer + args_size(arr))
                            .map_err(|e| Box::new(MemoryError::Math(e).into()))?,
                    )
                    .map_err(|e| Box::new(e.into()))?;
                    buffer += 1;
                }
            }
        }
    }
    for token_type in CostTokenType::iter_precost() {
        vm.insert_value(
            (builtin_cost_segment + (token_type.offset_in_builtin_costs() as usize)).unwrap(),
            Felt252::from(token_gas_cost(*token_type)),
        )
        .map_err(|e| Box::new(e.into()))?;
    }
    // Put a pointer to the builtin cost segment at the end of the program (after the
    // additional `ret` statement).
    vm.insert_value((vm.get_pc() + data_len).unwrap(), builtin_cost_segment)
        .map_err(|e| Box::new(e.into()))?;
    Ok(())
}

/// The size in memory of the arguments.
fn args_size(args: &[Arg]) -> usize {
    args.iter().map(Arg::size).sum()
}

/// Creates the metadata required for a Sierra program lowering to casm.
fn create_metadata(
    sierra_program: &cairo_lang_sierra::program::Program,
    metadata_config: Option<MetadataComputationConfig>,
) -> Result<Metadata, RunnerError> {
    if let Some(metadata_config) = metadata_config {
        calc_metadata(sierra_program, metadata_config)
    } else {
        calc_metadata_ap_change_only(sierra_program)
    }
    .map_err(|err| match err {
        MetadataError::ApChangeError(err) => RunnerError::ApChangeError(err),
        MetadataError::CostError(err) => RunnerError::FailedGasCalculation(err),
    })
}
