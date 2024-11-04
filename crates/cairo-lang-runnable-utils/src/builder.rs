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
use cairo_lang_sierra::program::{ConcreteTypeLongId, Function, Program as SierraProgram};
use cairo_lang_sierra::program_registry::{ProgramRegistry, ProgramRegistryError};
use cairo_lang_sierra_ap_change::ApChangeError;
use cairo_lang_sierra_gas::CostError;
use cairo_lang_sierra_to_casm::compiler::{CairoProgram, CompilationError, SierraToCasmConfig};
use cairo_lang_sierra_to_casm::metadata::{
    Metadata, MetadataComputationConfig, MetadataError, calc_metadata, calc_metadata_ap_change_only,
};
use cairo_lang_sierra_type_size::{TypeSizeMap, get_type_size_map};
use cairo_lang_utils::casts::IntoOrPanic;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use cairo_lang_utils::unordered_hash_set::UnorderedHashSet;
use cairo_vm::types::builtin_name::BuiltinName;
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

    /// Assembles a function program for a given function.
    pub fn assemble_function_program(
        &self,
        func: &Function,
    ) -> Result<(AssembledCairoProgram, Vec<BuiltinName>), BuildError> {
        let (header, builtins) = self.create_entry_code(func)?;
        let footer = create_code_footer();

        let assembled_cairo_program = self.casm_program.assemble_ex(&header, &footer);
        Ok((assembled_cairo_program, builtins))
    }

    /// Returns the instructions to add to the beginning of the code to successfully call the main
    /// function, as well as the builtins required to execute the program.
    fn create_entry_code(
        &self,
        func: &Function,
    ) -> Result<(Vec<Instruction>, Vec<BuiltinName>), BuildError> {
        let param_types = self.generic_id_and_size_from_concrete(&func.signature.param_types);
        let return_types = self.generic_id_and_size_from_concrete(&func.signature.ret_types);

        let entry_point = func.entry_point.0;
        let code_offset =
            self.casm_program.debug_info.sierra_statement_info[entry_point].start_offset;
        // Finalizing for proof only if all returned values are builtins or droppable.
        // (To handle cases such as a function returning a non-squashed dict, which can't be
        // provable by itself)
        let finalize_for_proof = func.signature.ret_types.iter().all(|ty| {
            let info = self.type_info(ty);
            info.droppable || !self.is_user_arg_type(&info.long_id.generic_id)
        });

        create_entry_code_from_params(&param_types, &return_types, code_offset, finalize_for_proof)
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

/// Returns the entry code to call the function with `param_types` as its inputs and
/// `return_types` as outputs, located at `code_offset`. If `finalize_for_proof` is true,
/// will make sure to remove the segment arena after calling the function. For testing purposes,
/// `finalize_for_proof` can be set to false, to avoid a failure of the segment arena validation.
pub fn create_entry_code_from_params(
    param_types: &[(GenericTypeId, i16)],
    return_types: &[(GenericTypeId, i16)],
    code_offset: usize,
    finalize_for_proof: bool,
) -> Result<(Vec<Instruction>, Vec<BuiltinName>), BuildError> {
    let mut ctx = CasmBuilder::default();
    let mut builtin_offset = 3;
    let mut builtin_vars = UnorderedHashMap::<_, _>::default();
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
                ctx.add_var(CellExpression::Deref(deref!([fp - builtin_offset]))),
            );
            builtin_offset += 1;
            builtins.push(builtin_name);
        }
    }
    builtins.reverse();

    let emulated_builtins = UnorderedHashSet::<_>::from_iter([SystemType::ID]);

    let got_segment_arena = param_types.iter().any(|(ty, _)| ty == &SegmentArenaType::ID);
    let has_post_calculation_loop = got_segment_arena && finalize_for_proof;

    let mut local_exprs = vec![];
    if has_post_calculation_loop {
        for (ty, size) in return_types {
            if ty != &SegmentArenaType::ID {
                for _ in 0..*size {
                    casm_build_extend!(ctx, localvar local;);
                    local_exprs.push(ctx.get_value(local, false));
                }
            }
        }
    }
    casm_build_extend!(ctx, ap += local_exprs.len(););
    if got_segment_arena {
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
    let mut unallocated_count = 0;
    let mut param_index = 0;
    for (generic_ty, ty_size) in param_types {
        if let Some(var) = builtin_vars.get(generic_ty).cloned() {
            casm_build_extend!(ctx, tempvar _builtin = var;);
        } else if emulated_builtins.contains(generic_ty) {
            casm_build_extend! {ctx,
                tempvar system;
                hint AllocSegment {} into {dst: system};
            };
            unallocated_count += ty_size;
        } else {
            if *ty_size > 0 {
                casm_build_extend! { ctx,
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
        };
    }
    if unallocated_count > 0 {
        casm_build_extend!(ctx, ap += unallocated_count.into_or_panic::<usize>(););
    }
    casm_build_extend! (ctx, let () = call FUNCTION;);
    let mut unprocessed_return_size = return_types.iter().map(|(_, size)| size).sum::<i16>();
    let mut return_data = vec![];
    for (ret_ty, size) in return_types {
        if let Some(var) = builtin_vars.get_mut(ret_ty) {
            *var = ctx.add_var(CellExpression::Deref(deref!([ap - unprocessed_return_size])));
            unprocessed_return_size -= 1;
        } else {
            for _ in 0..*size {
                return_data.push(
                    ctx.add_var(CellExpression::Deref(deref!([ap - unprocessed_return_size]))),
                );
                unprocessed_return_size -= 1;
            }
        }
    }
    assert_eq!(unprocessed_return_size, 0);
    if has_post_calculation_loop {
        // Storing local data on FP - as we have a loop now:
        for (cell, local_expr) in return_data.iter().cloned().zip(&local_exprs) {
            let local_cell = ctx.add_var(local_expr.clone());
            casm_build_extend!(ctx, assert local_cell = cell;);
        }
    }
    if got_segment_arena && finalize_for_proof {
        let segment_arena = builtin_vars[&SegmentArenaType::ID];
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
            hint ExternalHint::AddRelocationRule { src: curr_start, dst: expected_curr_start } into {};
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
        for (_, local_expr) in return_data.iter().zip(&local_exprs) {
            let local_cell = ctx.add_var(local_expr.clone());
            casm_build_extend! {ctx,
                tempvar _cell = local_cell;
            };
        }
    }
    casm_build_extend! (ctx, ret;);
    ctx.future_label("FUNCTION".into(), code_offset);
    Ok((ctx.build([]).instructions, builtins))
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
