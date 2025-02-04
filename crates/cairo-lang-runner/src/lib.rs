//! Basic runner for running a Sierra program on the vm.
use std::collections::HashMap;
use std::ops::{Add, Sub};

use cairo_lang_casm::hints::Hint;
use cairo_lang_runnable_utils::builder::{BuildError, EntryCodeConfig, RunnableBuilder};
use cairo_lang_sierra::extensions::NamedType;
use cairo_lang_sierra::extensions::core::CoreConcreteLibfunc;
use cairo_lang_sierra::extensions::enm::EnumType;
use cairo_lang_sierra::extensions::gas::{CostTokenType, GasBuiltinType};
use cairo_lang_sierra::ids::{ConcreteTypeId, GenericTypeId};
use cairo_lang_sierra::program::{Function, GenStatement, GenericArg, StatementIdx};
use cairo_lang_sierra_to_casm::metadata::MetadataComputationConfig;
use cairo_lang_starknet::contract::ContractInfo;
use cairo_lang_utils::casts::IntoOrPanic;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use cairo_lang_utils::{extract_matches, require};
use cairo_vm::hint_processor::hint_processor_definition::HintProcessor;
use cairo_vm::serde::deserialize_program::HintParams;
use cairo_vm::types::builtin_name::BuiltinName;
use cairo_vm::vm::errors::cairo_run_errors::CairoRunError;
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
    #[error(transparent)]
    BuildError(#[from] BuildError),
    #[error("Not enough gas to call function.")]
    NotEnoughGasToCall,
    #[error("Function param {param_index} only partially contains argument {arg_index}.")]
    ArgumentUnaligned { param_index: usize, arg_index: usize },
    #[error("Function expects arguments of size {expected} and received {actual} instead.")]
    ArgumentsSizeMismatch { expected: usize, actual: usize },
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
    /// Builder for runnable functions.
    builder: RunnableBuilder,
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
        // Find all contracts.
        Ok(Self {
            builder: RunnableBuilder::new(sierra_program, metadata_config)?,
            starknet_contracts_info,
            run_profiler,
        })
    }

    /// Runs the vm starting from a function in the context of a given starknet state.
    pub fn run_function_with_starknet_context(
        &self,
        func: &Function,
        args: Vec<Arg>,
        available_gas: Option<usize>,
        starknet_state: StarknetState,
    ) -> Result<RunResultStarknet, RunnerError> {
        let (assembled_program, builtins) =
            self.builder.assemble_function_program(func, EntryCodeConfig::testing())?;
        let (hints_dict, string_to_hint) = build_hints_dict(&assembled_program.hints);
        let user_args = self.prepare_args(func, available_gas, args)?;
        let mut hint_processor = CairoHintProcessor {
            runner: Some(self),
            user_args,
            starknet_state,
            string_to_hint,
            run_resources: RunResources::default(),
            syscalls_used_resources: Default::default(),
            no_temporary_segments: true,
            markers: Default::default(),
        };
        let RunResult { gas_counter, memory, value, used_resources, profiling_info } = self
            .run_function(
                func,
                &mut hint_processor,
                hints_dict,
                assembled_program.bytecode.iter(),
                builtins,
            )?;
        let mut all_used_resources = hint_processor.syscalls_used_resources;
        all_used_resources.basic_resources += &used_resources;
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
        let sierra_statement_info = &self.builder.casm_program().debug_info.sierra_statement_info;
        let sierra_len = sierra_statement_info.len();
        let bytecode_len = sierra_statement_info.last().unwrap().end_offset;
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
        // Total weight of Sierra statements grouped by the respective (collapsed) user function
        // call stack.
        let mut scoped_sierra_statement_weights = OrderedHashMap::default();
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
                self.builder.sierra_program(),
                sierra_statement_idx,
            );

            *sierra_statement_weights.entry(sierra_statement_idx).or_insert(0) += 1;

            if profiling_config.collect_scoped_sierra_statement_weights {
                // The current stack trace, including the current function (recursive calls
                // collapsed).
                let cur_stack: Vec<usize> =
                    chain!(function_stack.iter().map(|&(idx, _)| idx), [user_function_idx])
                        .dedup()
                        .collect();

                *scoped_sierra_statement_weights
                    .entry((cur_stack, sierra_statement_idx))
                    .or_insert(0) += 1;
            }

            let Some(gen_statement) =
                self.builder.sierra_program().statements.get(sierra_statement_idx.0)
            else {
                panic!("Failed fetching statement index {}", sierra_statement_idx.0);
            };

            match gen_statement {
                GenStatement::Invocation(invocation) => {
                    if matches!(
                        self.builder.registry().get_libfunc(&invocation.libfunc_id),
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

        ProfilingInfo {
            sierra_statement_weights,
            stack_trace_weights,
            scoped_sierra_statement_weights,
        }
    }

    fn sierra_statement_index_by_pc(&self, pc: usize) -> StatementIdx {
        // the `-1` here can't cause an underflow as the first statement is always at
        // offset 0, so it is always on the left side of the
        // partition, and thus the partition index is >0.
        StatementIdx(
            self.builder
                .casm_program()
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
        let generic_args = &func
            .signature
            .ret_types
            .iter()
            .find_map(|rt| {
                let long_id = self.builder.type_long_id(rt);
                (long_id.generic_id == *ty).then_some(long_id)
            })
            .unwrap()
            .generic_args;

        if *ty == EnumType::ID
            && matches!(&generic_args[0], GenericArg::UserType(ut)
                if ut.debug_name.as_ref().unwrap().starts_with("core::panics::PanicResult::"))
        {
            return Some(extract_matches!(&generic_args[1], GenericArg::Type).clone());
        }
        None
    }

    /// Runs the vm starting from a function with custom hint processor. Function may have
    /// implicits, but no other ref params. The cost of the function is deducted from
    /// `available_gas` before the execution begins.
    pub fn run_function<'a, Bytecode>(
        &self,
        func: &Function,
        hint_processor: &mut dyn HintProcessor,
        hints_dict: HashMap<usize, Vec<HintParams>>,
        bytecode: Bytecode,
        builtins: Vec<BuiltinName>,
    ) -> Result<RunResult, RunnerError>
    where
        Bytecode: ExactSizeIterator<Item = &'a BigInt> + Clone,
    {
        let return_types =
            self.builder.generic_id_and_size_from_concrete(&func.signature.ret_types);
        let data_len = bytecode.len();
        let RunFunctionResult { ap, mut used_resources, memory, relocated_trace } =
            casm_run::run_function(
                bytecode,
                builtins,
                |vm| initialize_vm(vm, data_len),
                hint_processor,
                hints_dict,
            )?;
        let header_end = relocated_trace.last().unwrap().pc;
        used_resources.n_steps -=
            relocated_trace.iter().position(|e| e.pc > header_end).unwrap() - 1;
        used_resources.n_steps -=
            relocated_trace.iter().rev().position(|e| e.pc > header_end).unwrap() - 1;

        let (results_data, gas_counter) = self.get_results_data(&return_types, &memory, ap);
        assert!(results_data.len() <= 1);

        let value = if results_data.is_empty() {
            // No result type - no panic.
            RunResultValue::Success(vec![])
        } else {
            let (ty, values) = results_data[0].clone();
            let inner_ty =
                self.inner_type_from_panic_wrapper(&ty, func).map(|it| self.builder.type_size(&it));
            Self::handle_main_return_value(inner_ty, values, &memory)
        };

        let profiling_info = self
            .run_profiler
            .as_ref()
            .map(|config| self.collect_profiling_info(&relocated_trace, config.clone()));

        Ok(RunResult { gas_counter, memory, value, used_resources, profiling_info })
    }

    /// Groups the args by parameters, and additionally add `gas` as the first if required.
    fn prepare_args(
        &self,
        func: &Function,
        available_gas: Option<usize>,
        args: Vec<Arg>,
    ) -> Result<Vec<Vec<Arg>>, RunnerError> {
        let mut user_args = vec![];
        if let Some(gas) = self
            .requires_gas_builtin(func)
            .then_some(self.get_initial_available_gas(func, available_gas)?)
        {
            user_args.push(vec![Arg::Value(Felt252::from(gas))]);
        }
        let mut expected_arguments_size = 0;
        let actual_args_size = args_size(&args);
        let mut arg_iter = args.into_iter().enumerate();
        for (param_index, (_, param_size)) in self
            .builder
            .generic_id_and_size_from_concrete(&func.signature.param_types)
            .into_iter()
            .filter(|(ty, _)| self.builder.is_user_arg_type(ty))
            .enumerate()
        {
            let mut curr_arg = vec![];
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
                curr_arg.push(arg);
            }
            user_args.push(curr_arg);
        }
        if expected_arguments_size != actual_args_size {
            return Err(RunnerError::ArgumentsSizeMismatch {
                expected: expected_arguments_size,
                actual: actual_args_size,
            });
        }
        Ok(user_args)
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
        &self,
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
                self.builder.is_user_arg_type(generic_ty)
            }
        });

        (results_data, gas_counter)
    }

    /// Finds first function ending with `name_suffix`.
    pub fn find_function(&self, name_suffix: &str) -> Result<&Function, RunnerError> {
        Ok(self.builder.find_function(name_suffix)?)
    }

    /// Returns whether the gas builtin is required in the given function.
    fn requires_gas_builtin(&self, func: &Function) -> bool {
        func.signature
            .param_types
            .iter()
            .any(|ty| self.builder.type_long_id(ty).generic_id == GasBuiltinType::ID)
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
        let gas_info = &self.builder.metadata().gas_info;
        require(!gas_info.function_costs.is_empty())?;
        Some(
            gas_info.function_costs[&func.id]
                .iter()
                .map(|(token_type, val)| val.into_or_panic::<usize>() * token_gas_cost(*token_type))
                .sum(),
        )
    }
}

/// Configuration for the profiling info collection phase.
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ProfilingInfoCollectionConfig {
    /// The maximum depth of the stack trace to collect.
    pub max_stack_trace_depth: usize,
    /// If this flag is set, in addition to the Sierra statement weights and stack trace weights
    /// the runner will also collect weights for Sierra statements taking into account current call
    /// stack and collapsing recursive calls (which also includes loops).
    /// The resulting dictionary can be pretty huge hence this feature is optional and disabled by
    /// default.
    pub collect_scoped_sierra_statement_weights: bool,
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
            collect_scoped_sierra_statement_weights: false,
        }
    }
}

/// Initializes a vm by adding a new segment with builtins cost and a necessary pointer at the end
/// of the program, as well as placing the arguments at the initial ap values.
pub fn initialize_vm(vm: &mut VirtualMachine, data_len: usize) -> Result<(), Box<CairoRunError>> {
    // Create the builtin cost segment, with dummy values.
    let builtin_cost_segment = vm.add_memory_segment();
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
