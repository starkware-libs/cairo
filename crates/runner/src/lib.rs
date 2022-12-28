use std::collections::HashMap;

use cairo_rs::vm::errors::vm_errors::VirtualMachineError;
use casm::instructions::Instruction;
use casm::{casm, casm_extend};
use itertools::chain;
use num_bigint::BigInt;
use sierra::extensions::builtin_cost::CostTokenType;
use sierra::extensions::core::{CoreLibFunc, CoreType};
use sierra::extensions::ConcreteType;
use sierra::program::{Function, GenericArg};
use sierra::program_registry::{ProgramRegistry, ProgramRegistryError};
use sierra_ap_change::{calc_ap_changes, ApChangeError};
use sierra_gas::calc_gas_info;
use sierra_gas::gas_info::GasInfo;
use sierra_to_casm::compiler::{CairoProgram, CompilationError};
use sierra_to_casm::metadata::Metadata;
use thiserror::Error;
use utils::extract_matches;

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
    pub gas_counter: Option<BigInt>,
    pub memory: Vec<Option<BigInt>>,
    pub value: RunResultValue,
}

/// The ran function return value.
#[derive(Debug, Eq, PartialEq)]
pub enum RunResultValue {
    /// Run ended successfully, returning the memory of the non-implicit returns.
    Success(Vec<BigInt>),
    /// Run panicked, returning the carried error data.
    Panic(Vec<BigInt>),
}

/// Runner enabling running a Sierra program on the vm.
pub struct SierraCasmRunner {
    /// The sierra program.
    sierra_program: sierra::program::Program,
    /// Metadata for the Sierra program.
    metadata: Metadata,
    /// Program registry for the Sierra program.
    sierra_program_registry: ProgramRegistry<CoreType, CoreLibFunc>,
    /// The casm program matching the Sierra code.
    casm_program: CairoProgram,
}
impl SierraCasmRunner {
    pub fn new(
        sierra_program: sierra::program::Program,
        calc_gas: bool,
    ) -> Result<Self, RunnerError> {
        let metadata = create_metadata(&sierra_program, calc_gas)?;
        let sierra_program_registry =
            ProgramRegistry::<CoreType, CoreLibFunc>::new(&sierra_program)?;
        let casm_program = sierra_to_casm::compiler::compile(&sierra_program, &metadata, calc_gas)?;
        Ok(Self { sierra_program, metadata, sierra_program_registry, casm_program })
    }

    /// Runs the vm starting from a function. Function may have implicits, but no other ref params.
    /// The cost of the function is deducted from available_gas before the execution begins.
    pub fn run_function(
        &self,
        name_suffix: &str,
        args: &[BigInt],
        available_gas: Option<usize>,
    ) -> Result<RunResult, RunnerError> {
        let func = self.find_function(name_suffix)?;
        let initial_gas = self.get_initial_gas(func, available_gas)?;
        let (entry_code, builtins) = self.create_entry_code(func, args, initial_gas)?;
        let (cells, ap) = casm::run::run_function(
            chain!(entry_code.iter(), self.casm_program.instructions.iter()),
            builtins,
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
                // TODO(orizi): Actually return the range check data.
                *ty != "RangeCheck".into() && *ty != "Bitwise".into() && *ty != "Pedersen".into()
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
        ty: sierra::ids::ConcreteTypeId,
        values: Vec<BigInt>,
        cells: &[Option<BigInt>],
    ) -> Result<RunResultValue, RunnerError> {
        let info = self.sierra_program_registry.get_type(&ty)?.info();
        let long_id = &info.long_id;
        Ok(
            if long_id.generic_id == "Enum".into()
                && matches!(&long_id.generic_args[0], GenericArg::UserType(ut) if ut.debug_name.as_ref().unwrap().starts_with("core::PanicResult::"))
            {
                // The function includes a panic wrapper.
                if values[0] != BigInt::from(0) {
                    // The run resulted in a panic, returning the error data.
                    let err_data_start = usize::try_from(&values[1]).unwrap();
                    let err_data_end = usize::try_from(&values[2]).unwrap();
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
        cells: &[Option<BigInt>],
        mut ap: usize,
    ) -> Result<Vec<(sierra::ids::ConcreteTypeId, Vec<BigInt>)>, RunnerError> {
        let mut results_data = vec![];
        for ty in func.signature.ret_types.iter().rev() {
            let size = self.sierra_program_registry.get_type(ty)?.info().size as usize;
            let values: Vec<BigInt> =
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
        args: &[BigInt],
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
        let builtin_offset: HashMap<sierra::ids::ConcreteTypeId, i16> = HashMap::from([
            (sierra::ids::ConcreteTypeId::new_inline("Pedersen"), 6),
            (sierra::ids::ConcreteTypeId::new_inline("RangeCheck"), 5),
            (sierra::ids::ConcreteTypeId::new_inline("Bitwise"), 4),
            (sierra::ids::ConcreteTypeId::new_inline("EcOp"), 3),
        ]);
        for ty in func.signature.param_types.iter() {
            if let Some(offset) = builtin_offset.get(ty) {
                casm_extend! {ctx,
                    [ap + 0] = [fp - offset], ap++;
                }
            } else if ty == &"GasBuiltin".into() {
                casm_extend! {ctx,
                    [ap + 0] = initial_gas, ap++;
                }
            } else {
                let arg_size = self.sierra_program_registry.get_type(ty)?.info().size;
                expected_arguments_size += arg_size as usize;
                for _ in 0..arg_size {
                    if let Some(value) = arg_iter.next() {
                        casm_extend! {ctx,
                            [ap + 0] = (value.clone()), ap++;
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
            self.metadata.gas_info.function_costs[&func.id][CostTokenType::Step] as usize;
        available_gas.checked_sub(required_gas).ok_or(RunnerError::NotEnoughGasToCall)
    }
}

/// Creates the metadata required for a Sierra program lowering to casm.
fn create_metadata(
    sierra_program: &sierra::program::Program,
    calc_gas: bool,
) -> Result<Metadata, RunnerError> {
    let gas_info = if calc_gas {
        calc_gas_info(sierra_program).map_err(|_| RunnerError::FailedGasCalculation)?
    } else {
        GasInfo { variable_values: HashMap::new(), function_costs: HashMap::new() }
    };
    let metadata = Metadata { ap_change_info: calc_ap_changes(sierra_program)?, gas_info };
    Ok(metadata)
}
