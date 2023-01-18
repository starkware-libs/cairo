//! Basic runner for running a Sierra program on the vm.
use std::collections::HashMap;

use cairo_felt::{Felt, FeltOps};
use cairo_lang_casm::instructions::Instruction;
use cairo_lang_casm::{casm, casm_extend};
use cairo_lang_sierra::extensions::builtin_cost::CostTokenType;
use cairo_lang_sierra::extensions::core::{CoreLibfunc, CoreType};
use cairo_lang_sierra::extensions::ConcreteType;
use cairo_lang_sierra::program::{Function, GenericArg};
use cairo_lang_sierra::program_registry::{ProgramRegistry, ProgramRegistryError};
use cairo_lang_sierra_ap_change::{calc_ap_changes, ApChangeError};
use cairo_lang_sierra_gas::gas_info::GasInfo;
use cairo_lang_sierra_to_casm::compiler::{CairoProgram, CompilationError};
use cairo_lang_sierra_to_casm::metadata::{calc_metadata, Metadata, MetadataError};
use cairo_lang_utils::extract_matches;
use cairo_vm::vm::errors::vm_errors::VirtualMachineError;
use itertools::chain;
use num_traits::ToPrimitive;
use thiserror::Error;

mod casm_run;
pub mod short_string;

#[derive(Debug, Error)]
pub enum RunnerError {
    #[error("Not enough gas to call function.")]
    NotEnoughGasToCall,
    #[error("GasBuiltin is required while `available_gas` value is provided.")]
    GasBuiltinRequired,
    #[error("Failed calculating gas usage, it is likely a call for `get_gas` is missing.")]
    FailedGasCalculation,
    #[error("Function with suffix `{suffix}` to run not found.")]
    MissingFunction { suffix: String },
    #[error("Function expects arguments of size {expected} and received {actual} instead.")]
    ArgumentsSizeMismatch { expected: usize, actual: usize },
    #[error(transparent)]
    ProgramRegistryError(#[from] Box<ProgramRegistryError>),
    #[error(transparent)]
    SierraCompilationError(#[from] CompilationError),
    #[error(transparent)]
    ApChangeError(#[from] ApChangeError),
    #[error(transparent)]
    VirtualMachineError(#[from] Box<VirtualMachineError>),
}

/// The full result of a run.
pub struct RunResult {
    pub gas_counter: Option<Felt>,
    pub memory: Vec<Option<Felt>>,
    pub value: RunResultValue,
}

/// The ran function return value.
#[derive(Debug, Eq, PartialEq)]
pub enum RunResultValue {
    /// Run ended successfully, returning the memory of the non-implicit returns.
    Success(Vec<Felt>),
    /// Run panicked, returning the carried error data.
    Panic(Vec<Felt>),
}

// Dummy cost of a builtin invocation.
pub const DUMMY_BUILTIN_GAS_COST: usize = 10000;

/// Runner enabling running a Sierra program on the vm.
pub struct SierraCasmRunner {
    /// The sierra program.
    sierra_program: cairo_lang_sierra::program::Program,
    /// Metadata for the Sierra program.
    metadata: Metadata,
    /// Program registry for the Sierra program.
    sierra_program_registry: ProgramRegistry<CoreType, CoreLibfunc>,
    /// The casm program matching the Sierra code.
    casm_program: CairoProgram,
}
impl SierraCasmRunner {
    pub fn new(
        sierra_program: cairo_lang_sierra::program::Program,
        calc_gas: bool,
    ) -> Result<Self, RunnerError> {
        let metadata = create_metadata(&sierra_program, calc_gas)?;
        let sierra_program_registry =
            ProgramRegistry::<CoreType, CoreLibfunc>::new(&sierra_program)?;
        let casm_program =
            cairo_lang_sierra_to_casm::compiler::compile(&sierra_program, &metadata, calc_gas)?;
        Ok(Self { sierra_program, metadata, sierra_program_registry, casm_program })
    }

    /// Runs the vm starting from a function. Function may have implicits, but no other ref params.
    /// The cost of the function is deducted from available_gas before the execution begins.
    pub fn run_function(
        &self,
        name_suffix: &str,
        args: &[Felt],
        available_gas: Option<usize>,
    ) -> Result<RunResult, RunnerError> {
        let func = self.find_function(name_suffix)?;
        let initial_gas = self.get_initial_gas(func, available_gas)?;
        let (entry_code, builtins) = self.create_entry_code(func, args, initial_gas)?;
        let footer = self.create_code_footer();
        let (cells, ap) = casm_run::run_function(
            chain!(entry_code.iter(), self.casm_program.instructions.iter(), footer.iter()),
            builtins,
            |context| {
                let vm = context.vm;
                // Create the builtin cost segment, with dummy values.
                let builtin_cost_segment = vm.add_memory_segment();
                for token_type in CostTokenType::iter() {
                    if *token_type == CostTokenType::Step {
                        continue;
                    }
                    vm.insert_value(
                        &(builtin_cost_segment + (token_type.offset_in_builtin_costs() as usize)),
                        Felt::from(DUMMY_BUILTIN_GAS_COST),
                    )?;
                }
                // Put a pointer to the builtin cost segment at the end of the program (after the
                // additional `ret` statement).
                vm.insert_value(&(vm.get_pc() + context.data_len), builtin_cost_segment)?;
                Ok(())
            },
        )?;
        let mut results_data = self.get_results_data(func, &cells, ap)?;
        // Handling implicits.
        let mut gas_counter = None;
        results_data.retain_mut(|(ty, values)| {
            if *ty == "GasBuiltin".into() {
                gas_counter = Some(values.remove(0));
                assert!(values.is_empty());
                false
            } else {
                *ty != "RangeCheck".into()
                    && *ty != "Bitwise".into()
                    && *ty != "EcOp".into()
                    && *ty != "Pedersen".into()
                    && *ty != "System".into()
                    && *ty != "DictManager".into()
            }
        });
        assert!(results_data.len() <= 1);
        let value = if results_data.is_empty() {
            // No result type - no panic.
            RunResultValue::Success(vec![])
        } else {
            let [(ty, values)] = <[_; 1]>::try_from(results_data).ok().unwrap();
            self.handle_main_return_value(ty, values, &cells)?
        };
        Ok(RunResult { gas_counter, memory: cells, value })
    }

    /// Handling the main return value to create a `RunResultValue`.
    fn handle_main_return_value(
        &self,
        ty: cairo_lang_sierra::ids::ConcreteTypeId,
        values: Vec<Felt>,
        cells: &[Option<Felt>],
    ) -> Result<RunResultValue, RunnerError> {
        let info = self.sierra_program_registry.get_type(&ty)?.info();
        let long_id = &info.long_id;
        Ok(
            if long_id.generic_id == "Enum".into()
                && matches!(&long_id.generic_args[0], GenericArg::UserType(ut) if ut.debug_name.as_ref().unwrap().starts_with("core::PanicResult::"))
            {
                // The function includes a panic wrapper.
                if values[0] != Felt::from(0) {
                    // The run resulted in a panic, returning the error data.
                    let err_data_start = values[1].to_usize().unwrap();
                    let err_data_end = values[2].to_usize().unwrap();
                    RunResultValue::Panic(
                        cells[err_data_start..err_data_end]
                            .iter()
                            .cloned()
                            .map(|cell| cell.unwrap())
                            .collect(),
                    )
                } else {
                    // The run resulted successfully, returning the inner value.
                    let inner_ty = extract_matches!(&long_id.generic_args[1], GenericArg::Type);
                    let inner_ty_size =
                        self.sierra_program_registry.get_type(inner_ty)?.info().size as usize;
                    RunResultValue::Success(
                        values.into_iter().skip(1).take(inner_ty_size).collect(),
                    )
                }
            } else {
                // No panic wrap - so always successful.
                RunResultValue::Success(values)
            },
        )
    }

    /// Returns the final values and type of all `func`s returning variables.
    fn get_results_data(
        &self,
        func: &Function,
        cells: &[Option<Felt>],
        mut ap: usize,
    ) -> Result<Vec<(cairo_lang_sierra::ids::ConcreteTypeId, Vec<Felt>)>, RunnerError> {
        let mut results_data = vec![];
        for ty in func.signature.ret_types.iter().rev() {
            let size = self.sierra_program_registry.get_type(ty)?.info().size as usize;
            let values: Vec<Felt> =
                cells[(ap - size)..ap].iter().cloned().map(|cell| cell.unwrap()).collect();
            ap -= size;
            results_data.push((ty.clone(), values));
        }
        Ok(results_data)
    }

    /// Finds first function ending with `name_suffix`.
    fn find_function(&self, name_suffix: &str) -> Result<&Function, RunnerError> {
        self.sierra_program
            .funcs
            .iter()
            .find(|f| {
                if let Some(name) = &f.id.debug_name { name.ends_with(name_suffix) } else { false }
            })
            .ok_or_else(|| RunnerError::MissingFunction { suffix: name_suffix.to_owned() })
    }

    /// Returns the instructions to add to the beginning of the code to successfully call the main
    /// function, as well as the builtins required to execute the program.
    fn create_entry_code(
        &self,
        func: &Function,
        args: &[Felt],
        initial_gas: usize,
    ) -> Result<(Vec<Instruction>, Vec<String>), RunnerError> {
        let mut arg_iter = args.iter();
        let mut expected_arguments_size = 0;
        let mut ctx = casm! {};
        // The builtins in the formatting expected by the runner.
        let builtins: Vec<_> = ["pedersen", "range_check", "bitwise", "ec_op"]
            .map(&str::to_string)
            .into_iter()
            .collect();
        // The offset [fp - i] for each of this builtins in this configuration.
        let builtin_offset: HashMap<cairo_lang_sierra::ids::ConcreteTypeId, i16> = HashMap::from([
            (cairo_lang_sierra::ids::ConcreteTypeId::new_inline("Pedersen"), 6),
            (cairo_lang_sierra::ids::ConcreteTypeId::new_inline("RangeCheck"), 5),
            (cairo_lang_sierra::ids::ConcreteTypeId::new_inline("Bitwise"), 4),
            (cairo_lang_sierra::ids::ConcreteTypeId::new_inline("EcOp"), 3),
        ]);
        if func.signature.param_types.contains(&"DictManager".into()) {
            casm_extend! {ctx,
                // DictManager segment.
                %{ memory[ap + 0] = segments.add() %}
                // DictInfos segment.
                %{ memory[ap + 1] = segments.add() %}
                ap += 2;
                [ap + 0] = 0, ap++;
                // Write DictInfos segment, n_dicts (0), and n_destructed (0) to the DictManager segment.
                [ap - 2] = [[ap - 3]];
                [ap - 1] = [[ap - 3] + 1];
                [ap - 1] = [[ap - 3] + 2];
            }
        }
        for (i, ty) in func.signature.param_types.iter().enumerate() {
            if let Some(offset) = builtin_offset.get(ty) {
                casm_extend! {ctx,
                    [ap + 0] = [fp - offset], ap++;
                }
            } else if ty == &"System".into() {
                casm_extend! {ctx,
                    %{ memory[ap + 0] = segments.add() %}
                    ap += 1;
                }
            } else if ty == &"GasBuiltin".into() {
                casm_extend! {ctx,
                    [ap + 0] = initial_gas, ap++;
                }
            } else if ty == &"DictManager".into() {
                let offset = -(i as i16) - 3;
                casm_extend! {ctx,
                    [ap + 0] = [ap + offset], ap++;
                }
            } else {
                let arg_size = self.sierra_program_registry.get_type(ty)?.info().size;
                expected_arguments_size += arg_size as usize;
                for _ in 0..arg_size {
                    if let Some(value) = arg_iter.next() {
                        casm_extend! {ctx,
                            [ap + 0] = (value.to_bigint()), ap++;
                        }
                    }
                }
            }
        }
        if expected_arguments_size != args.len() {
            return Err(RunnerError::ArgumentsSizeMismatch {
                expected: expected_arguments_size,
                actual: args.len(),
            });
        }
        let before_final_call = ctx.current_code_offset;
        let final_call_size = 3;
        let offset = final_call_size
            + self.casm_program.debug_info.sierra_statement_info[func.entry_point.0].code_offset;
        casm_extend! {ctx,
            call rel offset;
            ret;
        }
        assert_eq!(before_final_call + final_call_size, ctx.current_code_offset);
        Ok((ctx.instructions, builtins))
    }

    /// Returns the initial value for the gas counter.
    /// If available_gas is None returns 0.
    fn get_initial_gas(
        &self,
        func: &Function,
        available_gas: Option<usize>,
    ) -> Result<usize, RunnerError> {
        // In case we don't have any costs - it means no equations were solved - so the gas builtin
        // is irrelevant, and we can return any value.
        if self.metadata.gas_info.function_costs.is_empty() {
            return Ok(0);
        }
        let Some(available_gas) = available_gas else { return Ok(0); };
        // TODO(lior): Handle the other token types.
        let required_gas =
            self.metadata.gas_info.function_costs[func.id.clone()][CostTokenType::Step] as usize;
        available_gas.checked_sub(required_gas).ok_or(RunnerError::NotEnoughGasToCall)
    }

    /// Creates a list of instructions that will be appended to the program's bytecode.
    pub fn create_code_footer(&self) -> Vec<Instruction> {
        casm! {
            // Add a `ret` instruction used in libfuncs that retrieve the current value of the `fp`
            // and `pc` registers.
            ret;
        }
        .instructions
    }
}

/// Creates the metadata required for a Sierra program lowering to casm.
fn create_metadata(
    sierra_program: &cairo_lang_sierra::program::Program,
    calc_gas: bool,
) -> Result<Metadata, RunnerError> {
    if calc_gas {
        calc_metadata(sierra_program).map_err(|err| match err {
            MetadataError::ApChangeError(err) => RunnerError::ApChangeError(err),
            MetadataError::CostError(_) => RunnerError::FailedGasCalculation,
        })
    } else {
        Ok(Metadata {
            ap_change_info: calc_ap_changes(sierra_program, |_, _| 0)?,
            gas_info: GasInfo {
                variable_values: Default::default(),
                function_costs: Default::default(),
            },
        })
    }
}
