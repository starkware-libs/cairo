use cairo_lang_casm::assembler::AssembledCairoProgram;
use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::cell_expression::CellExpression;
use cairo_lang_casm::hints::ExternalHint;
use cairo_lang_casm::instructions::Instruction;
use cairo_lang_casm::{casm, casm_build_extend, deref};
use cairo_lang_sierra::extensions::bitwise::BitwiseType;
use cairo_lang_sierra::extensions::circuit::{AddModType, MulModType};
use cairo_lang_sierra::extensions::core::{CoreLibfunc, CoreType};
use cairo_lang_sierra::extensions::ec::EcOpType;
use cairo_lang_sierra::extensions::gas::GasBuiltinType;
use cairo_lang_sierra::extensions::pedersen::PedersenType;
use cairo_lang_sierra::extensions::poseidon::PoseidonType;
use cairo_lang_sierra::extensions::range_check::{RangeCheck96Type, RangeCheckType};
use cairo_lang_sierra::extensions::segment_arena::SegmentArenaType;
use cairo_lang_sierra::extensions::starknet::syscalls::SystemType;
use cairo_lang_sierra::extensions::{ConcreteType, NamedType};
use cairo_lang_sierra::ids::{ConcreteTypeId, GenericTypeId};
use cairo_lang_sierra::program::{
    ConcreteTypeLongId, Function, Program as SierraProgram, StatementIdx,
};
use cairo_lang_sierra::program_registry::{ProgramRegistry, ProgramRegistryError};
use cairo_lang_sierra_ap_change::ApChangeError;
use cairo_lang_sierra_gas::CostError;
use cairo_lang_sierra_to_casm::compiler::{CairoProgram, CompilationError, SierraToCasmConfig};
use cairo_lang_sierra_to_casm::metadata::{
    Metadata, MetadataComputationConfig, MetadataError, calc_metadata, calc_metadata_ap_change_only,
};
use cairo_lang_sierra_type_size::{TypeSizeMap, get_type_size_map};
use cairo_lang_utils::casts::IntoOrPanic;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use cairo_lang_utils::unordered_hash_set::UnorderedHashSet;
use cairo_vm::types::builtin_name::BuiltinName;
use itertools::{chain, zip_eq};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum BuildError {
    #[error(
        "Failed calculating gas usage, it is likely a call for `gas::withdraw_gas` is missing. \
         Inner error: {0}"
    )]
    FailedGasCalculation(#[from] CostError),
    #[error("Function with suffix `{suffix}` to run not found.")]
    MissingFunction { suffix: String },
    #[error(transparent)]
    ProgramRegistryError(#[from] Box<ProgramRegistryError>),
    #[error(transparent)]
    SierraCompilationError(#[from] Box<CompilationError>),
    #[error(transparent)]
    ApChangeError(#[from] ApChangeError),
    #[error("Emulated builtin `{0}` not allowed for executable.")]
    DisallowedBuiltinForExecutable(GenericTypeId),
}

impl BuildError {
    pub fn stmt_indices(&self) -> Vec<StatementIdx> {
        match self {
            BuildError::SierraCompilationError(err) => err.stmt_indices(),
            _ => vec![],
        }
    }
}

/// Builder for creating a runnable CASM program from Sierra code.
pub struct RunnableBuilder {
    /// The sierra program.
    sierra_program: SierraProgram,
    /// Metadata for the Sierra program.
    metadata: Metadata,
    /// Program registry for the Sierra program.
    sierra_program_registry: ProgramRegistry<CoreType, CoreLibfunc>,
    /// Program registry for the Sierra program.
    type_sizes: TypeSizeMap,
    /// The casm program matching the Sierra code.
    casm_program: CairoProgram,
    /// The types of the non-user argument variables.
    non_args_types: UnorderedHashSet<GenericTypeId>,
}

impl RunnableBuilder {
    /// Creates a new `RunnableBuilder` for a Sierra program.
    pub fn new(
        sierra_program: SierraProgram,
        metadata_config: Option<MetadataComputationConfig>,
    ) -> Result<Self, BuildError> {
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

        Ok(Self {
            sierra_program,
            metadata,
            sierra_program_registry,
            type_sizes,
            casm_program,
            non_args_types: UnorderedHashSet::from_iter([
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
            ]),
        })
    }

    /// Returns the Sierra program.
    pub fn sierra_program(&self) -> &SierraProgram {
        &self.sierra_program
    }

    /// Returns the CASM program of the Sierra program, without any modifications.
    pub fn casm_program(&self) -> &CairoProgram {
        &self.casm_program
    }

    /// Returns the metadata of the Sierra program.
    pub fn metadata(&self) -> &Metadata {
        &self.metadata
    }

    /// Returns the program registry of the Sierra program.
    pub fn registry(&self) -> &ProgramRegistry<CoreType, CoreLibfunc> {
        &self.sierra_program_registry
    }

    /// Finds first function ending with `name_suffix`.
    pub fn find_function(&self, name_suffix: &str) -> Result<&Function, BuildError> {
        self.sierra_program
            .funcs
            .iter()
            .find(|f| {
                if let Some(name) = &f.id.debug_name { name.ends_with(name_suffix) } else { false }
            })
            .ok_or_else(|| BuildError::MissingFunction { suffix: name_suffix.to_owned() })
    }

    /// Returns the type info of a given `ConcreteTypeId`.
    fn type_info(&self, ty: &ConcreteTypeId) -> &cairo_lang_sierra::extensions::types::TypeInfo {
        self.sierra_program_registry.get_type(ty).unwrap().info()
    }

    /// Returns the long id of a given `ConcreteTypeId`.
    pub fn type_long_id(&self, ty: &ConcreteTypeId) -> &ConcreteTypeLongId {
        &self.type_info(ty).long_id
    }

    /// Returns the long id of a given `ConcreteTypeId`.
    pub fn type_size(&self, ty: &ConcreteTypeId) -> i16 {
        self.type_sizes[ty]
    }

    /// Returns whether `ty` is a user arg sort of type.
    pub fn is_user_arg_type(&self, ty: &GenericTypeId) -> bool {
        !self.non_args_types.contains(ty)
    }

    /// Creates the wrapper info for a given function.
    pub fn create_wrapper_info(
        &self,
        func: &Function,
        config: EntryCodeConfig,
    ) -> Result<CasmProgramWrapperInfo, BuildError> {
        let (header, builtins) = self.create_entry_code(func, config)?;
        Ok(CasmProgramWrapperInfo { header, builtins, footer: create_code_footer() })
    }

    /// Assembles a function program for a given function.
    pub fn assemble_function_program(
        &self,
        func: &Function,
        config: EntryCodeConfig,
    ) -> Result<(AssembledCairoProgram, Vec<BuiltinName>), BuildError> {
        let info = self.create_wrapper_info(func, config)?;
        let assembled_cairo_program = self.casm_program.assemble_ex(&info.header, &info.footer);
        Ok((assembled_cairo_program, info.builtins))
    }

    /// Returns the instructions to add to the beginning of the code to successfully call the main
    /// function, as well as the builtins required to execute the program.
    fn create_entry_code(
        &self,
        func: &Function,
        config: EntryCodeConfig,
    ) -> Result<(Vec<Instruction>, Vec<BuiltinName>), BuildError> {
        let param_types = self.generic_id_and_size_from_concrete(&func.signature.param_types);
        let return_types = self.generic_id_and_size_from_concrete(&func.signature.ret_types);

        let entry_point = func.entry_point.0;
        let code_offset =
            self.casm_program.debug_info.sierra_statement_info[entry_point].start_offset;
        // Finalizing for proof only if all returned values are builtins or droppable.
        let droppable_return_value = func.signature.ret_types.iter().all(|ty| {
            let info = self.type_info(ty);
            info.droppable || !self.is_user_arg_type(&info.long_id.generic_id)
        });
        if !droppable_return_value {
            assert!(
                !config.finalize_segment_arena,
                "Cannot finalize the segment arena when returning non-droppable values."
            );
        }

        create_entry_code_from_params(&param_types, &return_types, code_offset, config)
    }

    /// Converts array of `ConcreteTypeId`s into corresponding `GenericTypeId`s and their sizes
    pub fn generic_id_and_size_from_concrete(
        &self,
        types: &[ConcreteTypeId],
    ) -> Vec<(GenericTypeId, i16)> {
        types
            .iter()
            .map(|pt| {
                let info = self.type_info(pt);
                let generic_id = &info.long_id.generic_id;
                let size = self.type_sizes[pt];
                (generic_id.clone(), size)
            })
            .collect()
    }
}

/// Configuration for the entry code creation.
#[derive(Clone, Debug)]
pub struct EntryCodeConfig {
    /// Whether to finalize the segment arena after calling the function.
    pub finalize_segment_arena: bool,
    /// Whether the wrapped function is expecting the output builtin.
    ///
    /// If true, will expect the function signature to be `(Span<felt252>, Array<felt252>) ->
    /// Array<felt252>`. And will inject the output builtin to be the supplied array input, and
    /// to be the result of the output.
    pub outputting_function: bool,
}
impl EntryCodeConfig {
    /// Returns a configuration for testing purposes.
    ///
    /// This configuration will not finalize the segment arena after calling the function, to
    /// prevent failure in case of functions returning values.
    pub fn testing() -> Self {
        Self { finalize_segment_arena: false, outputting_function: false }
    }

    /// Returns a configuration for execution purposes.
    pub fn executable() -> Self {
        Self { finalize_segment_arena: true, outputting_function: true }
    }
}

/// Information about a CASM program.
pub struct CasmProgramWrapperInfo {
    /// The builtins used in the program.
    pub builtins: Vec<BuiltinName>,
    /// The instructions before the program.
    pub header: Vec<Instruction>,
    /// The instructions after the program.
    pub footer: Vec<Instruction>,
}

/// Returns the entry code to call the function with `param_types` as its inputs and
/// `return_types` as outputs, located at `code_offset`. If `finalize_for_proof` is true,
/// will make sure to remove the segment arena after calling the function. For testing purposes,
/// `finalize_for_proof` can be set to false, to avoid a failure of the segment arena validation.
pub fn create_entry_code_from_params(
    param_types: &[(GenericTypeId, i16)],
    return_types: &[(GenericTypeId, i16)],
    code_offset: usize,
    config: EntryCodeConfig,
) -> Result<(Vec<Instruction>, Vec<BuiltinName>), BuildError> {
    let mut ctx = CasmBuilder::default();
    let mut builtin_offset = 3;
    let mut builtin_vars = OrderedHashMap::<_, _>::default();
    let mut builtin_ty_to_vm_name = UnorderedHashMap::<_, _>::default();
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
                builtin_name,
                ctx.add_var(CellExpression::Deref(deref!([fp - builtin_offset]))),
            );
            builtin_ty_to_vm_name.insert(builtin_ty.clone(), builtin_name);
            builtin_offset += 1;
            builtins.push(builtin_name);
        }
    }
    if config.outputting_function {
        let output_builtin_var = ctx.add_var(CellExpression::Deref(deref!([fp - builtin_offset])));
        builtin_vars.insert(BuiltinName::output, output_builtin_var);
        builtins.push(BuiltinName::output);
    }
    builtins.reverse();

    let emulated_builtins = UnorderedHashSet::<_>::from_iter([SystemType::ID]);

    let got_segment_arena = param_types.iter().any(|(ty, _)| ty == &SegmentArenaType::ID);
    let has_post_calculation_loop = got_segment_arena && config.finalize_segment_arena;

    let mut local_exprs = vec![];
    if has_post_calculation_loop {
        let mut allocate_cell = || {
            casm_build_extend!(ctx, localvar local;);
            local_exprs.push(ctx.get_value(local, false));
        };
        // In `outputting_function` case, the return data does not need to be returned to the
        // caller.
        if !config.outputting_function {
            for (ty, size) in return_types {
                // Builtins are handled later.
                if !builtin_ty_to_vm_name.contains_key(ty) {
                    for _ in 0..*size {
                        allocate_cell();
                    }
                }
            }
        }
        for name in builtin_vars.keys() {
            if name != &BuiltinName::segment_arena {
                allocate_cell();
            }
        }
        if !local_exprs.is_empty() {
            casm_build_extend!(ctx, ap += local_exprs.len(););
        }
    }
    if got_segment_arena {
        casm_build_extend! {ctx,
            tempvar segment_arena;
            tempvar infos;
            hint AllocSegment into {dst: segment_arena};
            hint AllocSegment into {dst: infos};
            const czero = 0;
            tempvar zero = czero;
            // Write Infos segment, n_constructed (0), and n_destructed (0) to the segment.
            assert infos = *(segment_arena++);
            assert zero = *(segment_arena++);
            assert zero = *(segment_arena++);
        }
        // Adding the segment arena to the builtins var map.
        builtin_vars.insert(BuiltinName::segment_arena, segment_arena);
        builtin_ty_to_vm_name.insert(SegmentArenaType::ID, BuiltinName::segment_arena);
    }
    let mut unallocated_count = 0;
    let mut param_index = 0;
    for (generic_ty, ty_size) in param_types {
        if let Some(name) = builtin_ty_to_vm_name.get(generic_ty).cloned() {
            let var = builtin_vars[&name];
            casm_build_extend!(ctx, tempvar _builtin = var;);
        } else if emulated_builtins.contains(generic_ty) {
            if config.outputting_function {
                // Emulated builtins are not supported when compiling an executable.
                return Err(BuildError::DisallowedBuiltinForExecutable(generic_ty.clone()));
            }
            casm_build_extend! {ctx,
                tempvar system;
                hint AllocSegment into {dst: system};
            };
            unallocated_count += ty_size;
        } else if !config.outputting_function {
            if *ty_size > 0 {
                casm_build_extend! {ctx,
                    tempvar first;
                    const param_index = param_index;
                    hint ExternalHint::WriteRunParam { index: param_index } into { dst: first };
                };
                for _ in 1..*ty_size {
                    casm_build_extend!(ctx, tempvar _cell;);
                }
            }
            param_index += 1;
            unallocated_count += ty_size;
        } else if generic_ty == &GasBuiltinType::ID {
            // Setting gas to be far from u128 boundaries.
            casm_build_extend! {ctx,
                const max_gas = i128::MAX;
                tempvar gas = max_gas;
            };
        }
    }
    if config.outputting_function {
        let output_ptr = builtin_vars[&BuiltinName::output];
        casm_build_extend! { ctx,
            tempvar input_start;
            tempvar _input_end;
            const param_index = 0;
            hint ExternalHint::WriteRunParam { index: param_index } into { dst: input_start };
            const user_data_offset = 1;
            tempvar output_start = output_ptr + user_data_offset;
            tempvar output_end = output_start;
        };
        unallocated_count += 2;
    }
    if unallocated_count > 0 {
        casm_build_extend!(ctx, ap += unallocated_count.into_or_panic::<usize>(););
    }
    casm_build_extend! (ctx, let () = call FUNCTION;);
    let mut unprocessed_return_size = return_types.iter().map(|(_, size)| size).sum::<i16>();
    let mut next_unprocessed_deref = || {
        let ret = CellExpression::Deref(deref!([ap - unprocessed_return_size]));
        unprocessed_return_size -= 1;
        ret
    };
    let mut return_data = vec![];
    for (ret_ty, size) in return_types {
        if let Some(name) = builtin_ty_to_vm_name.get(ret_ty) {
            *builtin_vars.get_mut(name).unwrap() = ctx.add_var(next_unprocessed_deref());
        } else if config.outputting_function {
            if ret_ty == &GasBuiltinType::ID {
                next_unprocessed_deref();
                continue;
            }
            let output_ptr_var = builtin_vars[&BuiltinName::output];
            // The output builtin values.
            let new_output_ptr = if *size == 3 {
                let panic_indicator = ctx.add_var(next_unprocessed_deref());
                // The start ptr of the output in the case of successful run,
                // or the panic data in case of a failure.
                let ptr_start = ctx.add_var(next_unprocessed_deref());
                // The end ptr of the output in the case of successful run,
                // or the panic data in case of a failure.
                let ptr_end = ctx.add_var(next_unprocessed_deref());
                casm_build_extend! {ctx,
                    tempvar new_output_ptr;
                    jump PANIC if panic_indicator != 0;
                    // SUCCESS:
                    assert new_output_ptr = ptr_end;
                    jump AFTER_PANIC_HANDLING;
                    PANIC:
                    // Marking the panic message in case of a panic.
                    hint ExternalHint::SetMarker { marker: ptr_start };
                    hint ExternalHint::SetMarker { marker: ptr_end };
                    const one = 1;
                    // In the case of an error, we assume no values are written to the output_ptr.
                    assert new_output_ptr = output_ptr_var + one;
                    AFTER_PANIC_HANDLING:
                    assert panic_indicator = output_ptr_var[0];
                };
                new_output_ptr
            } else if *size == 2 {
                // No panic possible.
                next_unprocessed_deref();
                let output_ptr_end = ctx.add_var(next_unprocessed_deref());
                casm_build_extend! {ctx,
                    const czero = 0;
                    tempvar zero = czero;
                    assert zero = *(output_ptr_var++);
                };
                output_ptr_end
            } else {
                panic!("Unexpected output type: {:?}", ret_ty.0);
            };
            *builtin_vars.get_mut(&BuiltinName::output).unwrap() = new_output_ptr;
        } else {
            for _ in 0..*size {
                return_data.push(ctx.add_var(next_unprocessed_deref()));
            }
        }
    }
    assert_eq!(unprocessed_return_size, 0);
    if has_post_calculation_loop {
        // Storing local data on FP - as we have a loop now.
        // Note that `return_data` is empty in `outputting_function` mode.
        let saved_post_loop =
            chain!(&return_data, builtin_vars.iter().filter_map(non_segment_arena_var));
        for (cell, local_expr) in zip_eq(saved_post_loop.cloned(), &local_exprs) {
            let local_cell = ctx.add_var(local_expr.clone());
            casm_build_extend!(ctx, assert local_cell = cell;);
        }
    }
    if got_segment_arena && config.finalize_segment_arena {
        let segment_arena = builtin_vars[&BuiltinName::segment_arena];
        // Validating the segment arena's segments are one after the other.
        casm_build_extend! {ctx,
            tempvar n_segments = segment_arena[-2];
            tempvar n_finalized = segment_arena[-1];
            assert n_segments = n_finalized;
            jump STILL_LEFT_PRE if n_segments != 0;
            rescope{};
            jump DONE_VALIDATION;
            STILL_LEFT_PRE:
            const one = 1;
            tempvar infos = segment_arena[-3];
            tempvar remaining_segments = n_segments - one;
            rescope{infos = infos, remaining_segments = remaining_segments};
            LOOP_START:
            jump STILL_LEFT_LOOP if remaining_segments != 0;
            rescope{};
            jump DONE_VALIDATION;
            STILL_LEFT_LOOP:
            tempvar prev_end = infos[1];
            tempvar curr_start = infos[3];
            const one = 1;
            let expected_curr_start = prev_end + one;
            hint ExternalHint::AddRelocationRule { src: curr_start, dst: expected_curr_start };
            assert curr_start = prev_end + one;
            const three = 3;
            tempvar next_infos = infos + three;
            tempvar next_remaining_segments = remaining_segments - one;
            rescope{infos = next_infos, remaining_segments = next_remaining_segments};
            #{ steps = 0; }
            jump LOOP_START;
            DONE_VALIDATION:
        };
    }
    if has_post_calculation_loop {
        let mut locals = local_exprs.into_iter();
        // Copying the result back to AP, for consistency of the result location.
        // Note that `return_data` is empty in `outputting_function` mode.
        for _ in 0..return_data.len() {
            let local_cell = ctx.add_var(locals.next().unwrap().clone());
            casm_build_extend!(ctx, tempvar _cell = local_cell;);
        }
        for (var, local_expr) in
            zip_eq(builtin_vars.iter_mut().filter_map(non_segment_arena_var), locals)
        {
            *var = ctx.add_var(local_expr);
        }
    }
    if config.outputting_function {
        for name in [
            BuiltinName::output,
            BuiltinName::pedersen,
            BuiltinName::range_check,
            BuiltinName::bitwise,
            BuiltinName::ec_op,
            BuiltinName::poseidon,
            BuiltinName::range_check96,
            BuiltinName::add_mod,
            BuiltinName::mul_mod,
        ] {
            if let Some(var) = builtin_vars.get(&name).copied() {
                casm_build_extend!(ctx, tempvar cell = var;);
            }
        }
    }
    casm_build_extend! (ctx, ret;);
    ctx.future_label("FUNCTION".into(), code_offset);
    Ok((ctx.build([]).instructions, builtins))
}

/// Helper to filter out the segment arena from a builtin vars map.
fn non_segment_arena_var<T>((name, var): (&BuiltinName, T)) -> Option<T> {
    if *name == BuiltinName::segment_arena { None } else { Some(var) }
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

/// Creates the metadata required for a Sierra program lowering to casm.
fn create_metadata(
    sierra_program: &SierraProgram,
    metadata_config: Option<MetadataComputationConfig>,
) -> Result<Metadata, BuildError> {
    if let Some(metadata_config) = metadata_config {
        calc_metadata(sierra_program, metadata_config)
    } else {
        calc_metadata_ap_change_only(sierra_program)
    }
    .map_err(|err| match err {
        MetadataError::ApChangeError(err) => BuildError::ApChangeError(err),
        MetadataError::CostError(err) => BuildError::FailedGasCalculation(err),
    })
}
