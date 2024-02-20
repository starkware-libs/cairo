//! Basic runner for running a Sierra program on the vm.
use std::collections::HashMap;

use ark_std::iterable::Iterable;
use cairo_felt::Felt252;
use cairo_lang_casm::hints::Hint;
use cairo_lang_casm::instructions::Instruction;
use cairo_lang_casm::{casm, casm_extend};
use cairo_lang_sierra::extensions::bitwise::BitwiseType;
use cairo_lang_sierra::extensions::core::{CoreConcreteLibfunc, CoreLibfunc, CoreType};
use cairo_lang_sierra::extensions::ec::EcOpType;
use cairo_lang_sierra::extensions::enm::EnumType;
use cairo_lang_sierra::extensions::gas::{CostTokenType, GasBuiltinType};
use cairo_lang_sierra::extensions::pedersen::PedersenType;
use cairo_lang_sierra::extensions::poseidon::PoseidonType;
use cairo_lang_sierra::extensions::range_check::RangeCheckType;
use cairo_lang_sierra::extensions::segment_arena::SegmentArenaType;
use cairo_lang_sierra::extensions::starknet::syscalls::SystemType;
use cairo_lang_sierra::extensions::{ConcreteType, NamedType};
use cairo_lang_sierra::ids::{ConcreteTypeId, GenericTypeId};
use cairo_lang_sierra::program::{Function, GenStatement, GenericArg, StatementIdx};
use cairo_lang_sierra::program_registry::{ProgramRegistry, ProgramRegistryError};
use cairo_lang_sierra_ap_change::ApChangeError;
use cairo_lang_sierra_to_casm::compiler::{CairoProgram, CompilationError};
use cairo_lang_sierra_to_casm::metadata::{
    calc_metadata, calc_metadata_ap_change_only, Metadata, MetadataComputationConfig, MetadataError,
};
use cairo_lang_sierra_type_size::{get_type_size_map, TypeSizeMap};
use cairo_lang_starknet::contract::ContractInfo;
use cairo_lang_utils::casts::IntoOrPanic;
use cairo_lang_utils::extract_matches;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use cairo_vm::hint_processor::hint_processor_definition::HintProcessor;
use cairo_vm::serde::deserialize_program::{BuiltinName, HintParams};
use cairo_vm::vm::errors::cairo_run_errors::CairoRunError;
use cairo_vm::vm::runners::cairo_runner::RunResources;
use cairo_vm::vm::trace::trace_entry::TraceEntry;
use cairo_vm::vm::vm_core::VirtualMachine;
use casm_run::hint_to_hint_params;
pub use casm_run::{CairoHintProcessor, StarknetState};
use itertools::chain;
use num_bigint::BigInt;
use num_traits::ToPrimitive;
use profiling::{user_function_idx_by_sierra_statement_idx, ProfilingInfo};
use thiserror::Error;

use crate::casm_run::RunFunctionContext;

pub mod casm_run;
pub mod profiling;
pub mod short_string;

const MAX_STACK_TRACE_DEPTH_DEFAULT: usize = 100;

#[derive(Debug, Error)]
pub enum RunnerError {
    #[error("Not enough gas to call function.")]
    NotEnoughGasToCall,
    #[error("GasBuiltin is required while `available_gas` value is provided.")]
    GasBuiltinRequired,
    #[error(
        "Failed calculating gas usage, it is likely a call for `gas::withdraw_gas` is missing."
    )]
    FailedGasCalculation,
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
    /// The profiling info of the run, if requested.
    pub profiling_info: Option<ProfilingInfo>,
}

/// The full result of a run.
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct RunResult {
    pub gas_counter: Option<Felt252>,
    pub memory: Vec<Option<Felt252>>,
    pub value: RunResultValue,
    /// The profiling info of the run, if requested.
    pub profiling_info: Option<ProfilingInfo>,
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
        CostTokenType::Pedersen => 4130,
        CostTokenType::Poseidon => 500,
        CostTokenType::Bitwise => 594,
        CostTokenType::EcOp => 4166,
    }
}

/// An argument to a sierra function run,
#[derive(Debug)]
pub enum Arg {
    Value(Felt252),
    Array(Vec<Felt252>),
}
impl From<Felt252> for Arg {
    fn from(value: Felt252) -> Self {
        Self::Value(value)
    }
}

/// Builds hints_dict required in cairo_vm::types::program::Program from instructions.
pub fn build_hints_dict<'b>(
    instructions: impl Iterator<Item = &'b Instruction>,
) -> (HashMap<usize, Vec<HintParams>>, HashMap<String, Hint>) {
    let mut hints_dict: HashMap<usize, Vec<HintParams>> = HashMap::new();
    let mut string_to_hint: HashMap<String, Hint> = HashMap::new();

    let mut hint_offset = 0;

    for instruction in instructions {
        if !instruction.hints.is_empty() {
            // Register hint with string for the hint processor.
            for hint in instruction.hints.iter() {
                string_to_hint.insert(hint.representing_string(), hint.clone());
            }
            // Add hint, associated with the instruction offset.
            hints_dict
                .insert(hint_offset, instruction.hints.iter().map(hint_to_hint_params).collect());
        }
        hint_offset += instruction.body.op_size();
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
    #[allow(dead_code)]
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
            gas_usage_check,
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
        let (entry_code, builtins) = self.create_entry_code(func, args, initial_gas)?;
        let footer = Self::create_code_footer();
        let (hints_dict, string_to_hint) =
            build_hints_dict(chain!(entry_code.iter(), self.casm_program.instructions.iter()));
        let assembled_program = self.casm_program.clone().assemble_ex(&entry_code, &footer);

        let mut hint_processor = CairoHintProcessor {
            runner: Some(self),
            starknet_state,
            string_to_hint,
            run_resources: RunResources::default(),
        };
        let RunResult { gas_counter, memory, value, profiling_info } = self.run_function(
            func,
            &mut hint_processor,
            hints_dict,
            assembled_program.bytecode.iter(),
            builtins,
        )?;
        Ok(RunResultStarknet {
            gas_counter,
            memory,
            value,
            starknet_state: hint_processor.starknet_state,
            profiling_info,
        })
    }

    /// Runs the vm starting from a function with custom hint processor. Function may have
    /// implicits, but no other ref params. The cost of the function is deducted from
    /// `available_gas` before the execution begins.
    ///
    /// Allows injecting Cairo `VirtualMachine`
    pub fn run_function_with_vm<'a, Bytecode>(
        &self,
        func: &Function,
        vm: &mut VirtualMachine,
        hint_processor: &mut dyn HintProcessor,
        hints_dict: HashMap<usize, Vec<HintParams>>,
        bytecode: Bytecode,
        builtins: Vec<BuiltinName>,
    ) -> Result<RunResult, RunnerError>
    where
        Bytecode: Iterator<Item = &'a BigInt> + Clone,
    {
        let return_types = self.generic_id_and_size_from_concrete(&func.signature.ret_types);

        let (cells, ap) = casm_run::run_function(
            vm,
            bytecode,
            builtins,
            initialize_vm,
            hint_processor,
            hints_dict,
        )?;
        let (results_data, gas_counter) = Self::get_results_data(&return_types, &cells, ap);
        assert!(results_data.len() <= 1);

        let value = if results_data.is_empty() {
            // No result type - no panic.
            RunResultValue::Success(vec![])
        } else {
            let (ty, values) = results_data[0].clone();
            let inner_ty =
                self.inner_type_from_panic_wrapper(&ty, func).map(|it| self.type_sizes[&it]);
            Self::handle_main_return_value(inner_ty, values, &cells)
        };

        let profiling_info = self.run_profiler.as_ref().map(|config| {
            self.collect_profiling_info(vm.get_relocated_trace().unwrap(), config.clone())
        });

        Ok(RunResult { gas_counter, memory: cells, value, profiling_info })
    }

    /// Collects profiling info of the current run using the trace.
    fn collect_profiling_info(
        &self,
        trace: &[TraceEntry],
        profiling_config: ProfilingInfoCollectionConfig,
    ) -> ProfilingInfo {
        let sierra_len = self.casm_program.debug_info.sierra_statement_info.len() - 1;
        let bytecode_len =
            self.casm_program.debug_info.sierra_statement_info.last().unwrap().code_offset;
        // The CASM program starts with a header of instructions to wrap the real program.
        // `real_pc_0` is the PC in the trace that points to the same CASM instruction which is in
        // the real PC=0 in the original CASM program. That is, all trace's PCs need to be
        // subtracted by `real_pc_0` to get the real PC they point to in the original CASM
        // program.
        // This is the same as the PC of the last trace entry plus 1, as the header is built to have
        // a `ret` last instruction, which must be the last in the trace of any execution.
        // The first instruction after that is the first instruction in the original CASM program.
        let real_pc_0 = trace.last().unwrap().pc + 1;

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
        let mut stack_trace_weights = UnorderedHashMap::default();
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
            let real_pc = step.pc - real_pc_0;
            // Skip the footer.
            if real_pc == bytecode_len {
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
                .partition_point(|x| x.code_offset <= pc)
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
        hint_processor: &mut dyn HintProcessor,
        hints_dict: HashMap<usize, Vec<HintParams>>,
        bytecode: Bytecode,
        builtins: Vec<BuiltinName>,
    ) -> Result<RunResult, RunnerError>
    where
        Bytecode: Iterator<Item = &'a BigInt> + Clone,
    {
        let mut vm = VirtualMachine::new(true);
        self.run_function_with_vm(func, &mut vm, hint_processor, hints_dict, bytecode, builtins)
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
                ((ap - size)..ap).map(|index| cells[index].clone().unwrap()).collect();
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

    fn get_info(
        &self,
        ty: &cairo_lang_sierra::ids::ConcreteTypeId,
    ) -> &cairo_lang_sierra::extensions::types::TypeInfo {
        self.sierra_program_registry.get_type(ty).unwrap().info()
    }

    pub fn create_entry_code_from_params(
        param_types: &[(GenericTypeId, i16)],
        args: &[Arg],
        initial_gas: usize,
        code_offset: usize,
    ) -> Result<(Vec<Instruction>, Vec<BuiltinName>), RunnerError> {
        let mut ctx = casm! {};
        // The builtins in the formatting expected by the runner.
        let builtins = vec![
            BuiltinName::pedersen,
            BuiltinName::range_check,
            BuiltinName::bitwise,
            BuiltinName::ec_op,
            BuiltinName::poseidon,
        ];
        // The offset [fp - i] for each of this builtins in this configuration.
        let builtin_offset: HashMap<GenericTypeId, i16> = HashMap::from([
            (PedersenType::ID, 7),
            (RangeCheckType::ID, 6),
            (BitwiseType::ID, 5),
            (EcOpType::ID, 4),
            (PoseidonType::ID, 3),
        ]);
        // Load all array args content to memory.
        let mut array_args_data = vec![];
        let mut ap_offset: i16 = 0;
        for arg in args {
            let Arg::Array(values) = arg else { continue };
            array_args_data.push(ap_offset);
            casm_extend! {ctx,
                %{ memory[ap + 0] = segments.add() %}
                ap += 1;
            }
            for (i, v) in values.iter().enumerate() {
                let arr_at: i16 = (i + 1).into_or_panic();
                casm_extend! {ctx,
                    [ap + 0] = (v.to_bigint());
                    [ap + 0] = [[ap - arr_at] + i.into_or_panic()], ap++;
                }
            }
            ap_offset += (1 + values.len()).into_or_panic::<i16>();
        }
        let mut array_args_data_iter = array_args_data.iter();
        let after_arrays_data_offset = ap_offset;
        if param_types.iter().any(|(ty, _)| ty == &SegmentArenaType::ID) {
            casm_extend! {ctx,
                // SegmentArena segment.
                %{ memory[ap + 0] = segments.add() %}
                // Infos segment.
                %{ memory[ap + 1] = segments.add() %}
                ap += 2;
                [ap + 0] = 0, ap++;
                // Write Infos segment, n_constructed (0), and n_destructed (0) to the segment.
                [ap - 2] = [[ap - 3]];
                [ap - 1] = [[ap - 3] + 1];
                [ap - 1] = [[ap - 3] + 2];
            }
            ap_offset += 3;
        }
        let mut expected_arguments_size = 0;
        let mut param_index = 0;
        let mut arg_iter = args.iter().enumerate();
        for ty in param_types {
            let (generic_ty, ty_size) = ty;
            if let Some(offset) = builtin_offset.get(generic_ty) {
                casm_extend! {ctx,
                    [ap + 0] = [fp - offset], ap++;
                }
                ap_offset += 1;
            } else if generic_ty == &SystemType::ID {
                casm_extend! {ctx,
                    %{ memory[ap + 0] = segments.add() %}
                    ap += 1;
                }
                ap_offset += 1;
            } else if generic_ty == &GasBuiltinType::ID {
                casm_extend! {ctx,
                    [ap + 0] = initial_gas, ap++;
                }
                ap_offset += 1;
            } else if generic_ty == &SegmentArenaType::ID {
                let offset = -ap_offset + after_arrays_data_offset;
                casm_extend! {ctx,
                    [ap + 0] = [ap + offset] + 3, ap++;
                }
                ap_offset += 1;
            } else {
                let arg_size = *ty_size;
                let param_ap_offset_end = ap_offset + arg_size;
                expected_arguments_size += arg_size.into_or_panic::<usize>();
                while ap_offset < param_ap_offset_end {
                    let Some((arg_index, arg)) = arg_iter.next() else {
                        break;
                    };
                    match arg {
                        Arg::Value(value) => {
                            casm_extend! {ctx,
                                [ap + 0] = (value.to_bigint()), ap++;
                            }
                            ap_offset += 1;
                        }
                        Arg::Array(values) => {
                            let offset = -ap_offset + array_args_data_iter.next().unwrap();
                            casm_extend! {ctx,
                                [ap + 0] = [ap + (offset)], ap++;
                                [ap + 0] = [ap - 1] + (values.len()), ap++;
                            }
                            ap_offset += 2;
                            if ap_offset > param_ap_offset_end {
                                return Err(RunnerError::ArgumentUnaligned {
                                    param_index,
                                    arg_index,
                                });
                            }
                        }
                    }
                }
                param_index += 1;
            };
        }
        let actual_args_size = args
            .iter()
            .map(|arg| match arg {
                Arg::Value(_) => 1,
                Arg::Array(_) => 2,
            })
            .sum::<usize>();
        if expected_arguments_size != actual_args_size {
            return Err(RunnerError::ArgumentsSizeMismatch {
                expected: expected_arguments_size,
                actual: actual_args_size,
            });
        }
        let before_final_call = ctx.current_code_offset;
        let final_call_size = 3;
        let offset = final_call_size + code_offset;
        casm_extend! {ctx,
            call rel offset;
            ret;
        }
        assert_eq!(before_final_call + final_call_size, ctx.current_code_offset);
        Ok((ctx.instructions, builtins))
    }

    /// Returns the instructions to add to the beginning of the code to successfully call the main
    /// function, as well as the builtins required to execute the program.
    pub fn create_entry_code(
        &self,
        func: &Function,
        args: &[Arg],
        initial_gas: usize,
    ) -> Result<(Vec<Instruction>, Vec<BuiltinName>), RunnerError> {
        let params = self.generic_id_and_size_from_concrete(&func.signature.param_types);

        let entry_point = func.entry_point.0;
        let code_offset =
            self.casm_program.debug_info.sierra_statement_info[entry_point].code_offset;

        Self::create_entry_code_from_params(&params, args, initial_gas, code_offset)
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
        if self.metadata.gas_info.function_costs.is_empty() {
            return None;
        }
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
/// of the program
pub fn initialize_vm(context: RunFunctionContext<'_>) -> Result<(), Box<CairoRunError>> {
    let vm = context.vm;
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
    vm.insert_value((vm.get_pc() + context.data_len).unwrap(), builtin_cost_segment)
        .map_err(|e| Box::new(e.into()))?;
    Ok(())
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
        MetadataError::CostError(_) => RunnerError::FailedGasCalculation,
    })
}
