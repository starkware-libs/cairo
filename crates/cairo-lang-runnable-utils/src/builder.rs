use cairo_lang_casm::assembler::AssembledCairoProgram;
use cairo_lang_casm::builder::{CasmBuilder, Var};
use cairo_lang_casm::cell_expression::CellExpression;
use cairo_lang_casm::hints::ExternalHint;
use cairo_lang_casm::instructions::Instruction;
use cairo_lang_casm::{ap_change, casm, casm_build_extend, cell_ref};
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
use cairo_lang_sierra_to_casm::annotations::AnnotationError;
use cairo_lang_sierra_to_casm::compiler::{CairoProgram, CompilationError, SierraToCasmConfig};
use cairo_lang_sierra_to_casm::metadata::{
    Metadata, MetadataComputationConfig, MetadataError, calc_metadata, calc_metadata_ap_change_only,
};
use cairo_lang_sierra_type_size::ProgramRegistryInfo;
use cairo_lang_utils::casts::IntoOrPanic;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
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

impl BuildError {
    pub fn stmt_indices(&self) -> Vec<StatementIdx> {
        match self {
            BuildError::SierraCompilationError(err) => err.stmt_indices(),
            _ => vec![],
        }
    }

    pub fn is_ap_overflow_error(&self) -> bool {
        let BuildError::SierraCompilationError(err) = self else {
            return false;
        };

        let CompilationError::AnnotationError(AnnotationError::ApChangeError { ref error, .. }) =
            **err
        else {
            return false;
        };

        *error == ap_change::ApChangeError::OffsetOverflow
    }
}

/// Builder for creating a runnable CASM program from Sierra code.
pub struct RunnableBuilder {
    /// The Sierra program.
    sierra_program: SierraProgram,
    /// Metadata for the Sierra program.
    metadata: Metadata,
    /// Program registry and type sizes for the Sierra program.
    program_info: ProgramRegistryInfo,
    /// The CASM program matching the Sierra code.
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
        let program_info = ProgramRegistryInfo::new(&sierra_program)?;
        let gas_usage_check = metadata_config.is_some();
        let metadata = create_metadata(&sierra_program, &program_info, metadata_config)?;
        let casm_program = cairo_lang_sierra_to_casm::compiler::compile(
            &sierra_program,
            &program_info,
            &metadata,
            SierraToCasmConfig { gas_usage_check, max_bytecode_size: usize::MAX },
        )?;

        Ok(Self {
            sierra_program,
            metadata,
            program_info,
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
        &self.program_info.registry
    }

    /// Finds the first function ending with `name_suffix`.
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
        self.program_info.registry.get_type(ty).unwrap().info()
    }

    /// Returns the long id of a given `ConcreteTypeId`.
    pub fn type_long_id(&self, ty: &ConcreteTypeId) -> &ConcreteTypeLongId {
        &self.type_info(ty).long_id
    }

    /// Returns the size of a given `ConcreteTypeId`.
    pub fn type_size(&self, ty: &ConcreteTypeId) -> i16 {
        self.program_info.type_sizes[ty]
    }

    /// Returns whether `ty` is a user argument type (e.g., not a builtin).
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
    ///
    /// # Arguments
    ///
    /// * `func` - The function to create the entry code for.
    /// * `config` - The entry code configuration.
    ///
    /// # Returns
    ///
    /// A `Result` containing a tuple with a vector of `Instruction`s and a vector of
    /// `BuiltinName`s, or a `BuildError`.
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
                let size = self.type_size(pt);
                (generic_id.clone(), size)
            })
            .collect()
    }
}

/// Configuration for the entry code creation.
#[derive(Clone, Debug)]
pub struct EntryCodeConfig {
    /// Whether this is a testing configuration.
    ///
    /// In the case of testing, the function signature can be anything.
    /// In the case of execution, the function signature is expected to be
    /// `(Span<felt252>, Array<felt252>) -> Array<felt252>`.
    /// Additionally, the output builtin will be injected to be the supplied array input,
    /// and to be the result of the output.
    ///
    /// Currently, if exists in params, the segment arena will also be finalized in execution mode.
    pub testing: bool,
    /// Whether to allow unsound operations in the program.
    /// Currently relevant for supporting emulated builtins.
    pub allow_unsound: bool,

    /// An optional list of builtins to use in the entry code (if its none the builtins will be
    /// inferred from the param_types)
    pub builtin_list: Option<Vec<BuiltinName>>,
}
impl EntryCodeConfig {
    /// Returns a configuration for testing purposes.
    ///
    /// This configuration will not finalize the segment arena after calling the function, to
    /// prevent failure in case of functions returning values.
    pub fn testing() -> Self {
        Self { testing: true, allow_unsound: true, builtin_list: None }
    }

    /// Returns a configuration for execution purposes.
    pub fn executable(allow_unsound: bool, builtin_list: Option<Vec<BuiltinName>>) -> Self {
        Self { testing: false, allow_unsound, builtin_list }
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

/// Creates the entry code to call the function.
///
/// # Arguments
///
/// * `param_types` - The types and sizes of the parameters of the function.
/// * `return_types` - The types and sizes of the return values of the function.
/// * `code_offset` - The offset of the entry_point in the CASM program (before adding the
///   executable header).
/// * `config` - The configuration for the entry code creation.
///
/// # Returns
///
/// A `Result` containing a tuple with a vector of `Instruction`s and a vector of `BuiltinName`s, or
/// a `BuildError`.
pub fn create_entry_code_from_params(
    param_types: &[(GenericTypeId, i16)],
    return_types: &[(GenericTypeId, i16)],
    code_offset: usize,
    config: EntryCodeConfig,
) -> Result<(Vec<Instruction>, Vec<BuiltinName>), BuildError> {
    let mut helper = EntryCodeHelper::new(config);
    if let Some(builtin_list) = &helper.config.builtin_list {
        helper.builtins = builtin_list.clone();
        let mut builtin_offset = 3;
        for builtin_name in helper.builtins.iter().rev() {
            helper.builtin_vars.insert(
                *builtin_name,
                helper.ctx.add_var(CellExpression::Deref(cell_ref!([fp - builtin_offset]))),
            );
            builtin_offset += 1;
        }
    } else {
        helper.process_builtins(param_types);
    }
    helper.process_params(param_types);
    casm_build_extend!(helper.ctx, let () = call FUNCTION;);
    helper.process_output(return_types);

    if helper.has_post_calculation_loop {
        helper.validate_segment_arena();
    }

    if !helper.config.testing {
        helper.process_builtins_output();
    }

    casm_build_extend! (helper.ctx, ret;);
    // Point `FUNCTION` to offset `code_offset` from this point which is the beginning of the Sierra
    // based code.
    helper.ctx.future_label("FUNCTION", code_offset);
    Ok((helper.ctx.build([]).instructions, helper.builtins))
}

/// Helper struct for [create_entry_code_from_params].
struct EntryCodeHelper {
    ctx: CasmBuilder,
    config: EntryCodeConfig,
    builtin_ty_to_vm_name: OrderedHashMap<GenericTypeId, BuiltinName>,
    builtins: Vec<BuiltinName>,
    got_segment_arena: bool,
    has_post_calculation_loop: bool,
    /// Maps each builtin name to the variable that stores it.
    builtin_vars: UnorderedHashMap<BuiltinName, Var>,
    /// Maps each builtin name to the local cell expression that stores it.
    /// This is needed because variables in `builtin_vars` might be invalidated.
    local_exprs: OrderedHashMap<BuiltinName, CellExpression>,
    emulated_builtins: UnorderedHashSet<GenericTypeId>,
}

impl EntryCodeHelper {
    /// Creates a new `EntryCodeHelper` with the given configuration.
    fn new(config: EntryCodeConfig) -> Self {
        Self {
            ctx: CasmBuilder::default(),
            config,
            builtin_ty_to_vm_name: OrderedHashMap::from_iter([
                (PedersenType::ID, BuiltinName::pedersen),
                (RangeCheckType::ID, BuiltinName::range_check),
                (BitwiseType::ID, BuiltinName::bitwise),
                (EcOpType::ID, BuiltinName::ec_op),
                (PoseidonType::ID, BuiltinName::poseidon),
                (RangeCheck96Type::ID, BuiltinName::range_check96),
                (AddModType::ID, BuiltinName::add_mod),
                (MulModType::ID, BuiltinName::mul_mod),
                (SegmentArenaType::ID, BuiltinName::segment_arena),
            ]),
            builtins: vec![],
            got_segment_arena: false,
            has_post_calculation_loop: false,
            builtin_vars: UnorderedHashMap::default(),
            local_exprs: OrderedHashMap::default(),
            emulated_builtins: UnorderedHashSet::<_>::from_iter([SystemType::ID]),
        }
    }

    /// Processes the builtins required for the function parameters.
    fn process_builtins(&mut self, param_types: &[(GenericTypeId, i16)]) {
        let mut builtin_offset = 3;

        // Process all builtins except the segment arena in reverse order.
        for (builtin_ty, builtin_name) in self.builtin_ty_to_vm_name.iter().rev().skip(1) {
            if param_types.iter().any(|(ty, _)| ty == builtin_ty) {
                self.builtin_vars.insert(
                    *builtin_name,
                    self.ctx.add_var(CellExpression::Deref(cell_ref!([fp - builtin_offset]))),
                );
                builtin_offset += 1;
                self.builtins.push(*builtin_name);
            }
        }
        if !self.config.testing {
            let output_builtin_var =
                self.ctx.add_var(CellExpression::Deref(cell_ref!([fp - builtin_offset])));
            self.builtin_vars.insert(BuiltinName::output, output_builtin_var);
            self.builtins.push(BuiltinName::output);
        }
        self.builtins.reverse();
    }

    /// Processes the function parameters in preparation for the function call.
    fn process_params(&mut self, param_types: &[(GenericTypeId, i16)]) {
        self.got_segment_arena = param_types.iter().any(|(ty, _)| ty == &SegmentArenaType::ID);
        self.has_post_calculation_loop = self.got_segment_arena && !self.config.testing;

        if self.has_post_calculation_loop {
            if !self.config.testing {
                // Add a local variable for the output builtin
                casm_build_extend!(self.ctx, localvar local;);
                self.local_exprs
                    .insert(BuiltinName::output, self.ctx.get_unadjusted(local).clone());
            }

            for (generic_ty, _ty_size) in param_types {
                if let Some(name) = self.builtin_ty_to_vm_name.get(generic_ty)
                    && name != &BuiltinName::segment_arena
                {
                    casm_build_extend!(self.ctx, localvar local;);
                    self.local_exprs.insert(*name, self.ctx.get_unadjusted(local).clone());
                }
            }

            if !self.local_exprs.is_empty() {
                casm_build_extend!(self.ctx, ap += self.local_exprs.len(););
            }
        }

        if self.got_segment_arena {
            casm_build_extend! {self.ctx,
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
            self.builtin_vars.insert(BuiltinName::segment_arena, segment_arena);
        }
        let mut unallocated_count = 0;
        let mut param_index = 0;

        // Not processing the user function params in the case of a proof, as these are processed
        // after.
        let non_proof_signature_params =
            if self.config.testing { param_types } else { &param_types[..(param_types.len() - 2)] };
        for (generic_ty, ty_size) in non_proof_signature_params {
            if let Some(name) = self.builtin_ty_to_vm_name.get(generic_ty).cloned() {
                let var = self.builtin_vars[&name];
                casm_build_extend!(self.ctx, tempvar _builtin = var;);
            } else if self.emulated_builtins.contains(generic_ty) {
                assert!(
                    self.config.allow_unsound,
                    "Cannot support emulated builtins if not configured to `allow_unsound`."
                );
                casm_build_extend! {self.ctx,
                    tempvar system;
                    hint AllocSegment into {dst: system};
                };
                unallocated_count += ty_size;
            } else if self.config.testing {
                if *ty_size > 0 {
                    casm_build_extend! {self.ctx,
                        tempvar first;
                        const param_index = param_index;
                        hint ExternalHint::WriteRunParam { index: param_index } into { dst: first };
                    };
                    for _ in 1..*ty_size {
                        casm_build_extend!(self.ctx, tempvar _cell;);
                    }
                }
                param_index += 1;
                unallocated_count += ty_size;
            } else if generic_ty == &GasBuiltinType::ID {
                casm_build_extend! {self.ctx,
                    const max_gas = i64::MAX;
                    tempvar gas = max_gas;
                };
            } else {
                unreachable!("Unexpected argument type: {:?}", generic_ty);
            }
        }
        if !self.config.testing {
            let output_ptr = self.builtin_vars[&BuiltinName::output];
            casm_build_extend! { self.ctx,
                tempvar input_start;
                tempvar _input_end;
                const param_index = 0;
                hint ExternalHint::WriteRunParam { index: param_index } into { dst: input_start };
                tempvar output_start = output_ptr;
                tempvar output_end = output_ptr;
            };
            unallocated_count += 2;
        }
        if unallocated_count > 0 {
            casm_build_extend!(self.ctx, ap += unallocated_count.into_or_panic::<usize>(););
        }
    }

    /// Processes the function return types.
    fn process_output(&mut self, return_types: &[(GenericTypeId, i16)]) {
        let mut unprocessed_return_size = return_types.iter().map(|(_, size)| size).sum::<i16>();
        let mut next_unprocessed_deref = || {
            let deref_cell = CellExpression::Deref(cell_ref!([ap - unprocessed_return_size]));
            assert!(unprocessed_return_size > 0);
            unprocessed_return_size -= 1;
            deref_cell
        };
        // Not processing the user function return type in the case of a proof, as it is processed
        // after.
        let non_proof_return_types = if self.config.testing {
            return_types
        } else {
            &return_types[..(return_types.len() - 1)]
        };
        let mut new_builtin_vars = UnorderedHashMap::<BuiltinName, Var>::default();
        for (ret_ty, size) in non_proof_return_types {
            if let Some(name) = self.builtin_ty_to_vm_name.get(ret_ty) {
                new_builtin_vars.insert(*name, self.ctx.add_var(next_unprocessed_deref()));
            } else if self.config.testing {
                for _ in 0..*size {
                    next_unprocessed_deref();
                }
            } else if self.emulated_builtins.contains(ret_ty) {
                assert!(
                    self.config.allow_unsound,
                    "Cannot support emulated builtins if not configured to `allow_unsound`."
                );
                let _ = next_unprocessed_deref();
            } else {
                assert_eq!(ret_ty, &GasBuiltinType::ID);
                let _ = next_unprocessed_deref();
            }
        }
        if !self.config.testing {
            let (ret_ty, size) = return_types.last().unwrap();
            let opt_panic_indicator = match *size {
                2 => None,
                3 => Some(self.ctx.add_var(next_unprocessed_deref())),
                _ => panic!("Unexpected output type: {:?}", ret_ty.0),
            };
            // The start ptr of the output in the case of successful run,
            // or the panic data in case of a failure.
            let ptr_start = self.ctx.add_var(next_unprocessed_deref());
            // The end ptr of the output in the case of successful run,
            // or the panic data in case of a failure.
            let ptr_end = self.ctx.add_var(next_unprocessed_deref());
            if let Some(panic_indicator) = opt_panic_indicator {
                casm_build_extend! {self.ctx,
                    const czero = 0;
                    tempvar zero = czero;
                    hint ExternalHint::AddMarker { start: ptr_start, end: ptr_end };
                    assert zero = panic_indicator;
                };
            }
            new_builtin_vars.insert(BuiltinName::output, ptr_end);
        }
        assert_eq!(unprocessed_return_size, 0);
        if self.has_post_calculation_loop {
            // Store the new builtin vars that were assigned local variables, as they are going to
            // be invalidated by loop. Note that the builtin vars map is not updated
            // with the new locals, since they will be invalidated by the rescoping
            // performed in `validate_segment_arena`.
            for (name, local_expr) in self.local_exprs.iter() {
                let builtin_var = new_builtin_vars[name];
                let local_expr = local_expr.clone();
                let local_var = self.ctx.add_var(local_expr.clone());
                casm_build_extend!(self.ctx, assert local_var = builtin_var;);
            }
        }

        // All the builtins not in return_types should be copied from self.builtin_vars to
        // new_builtin_vars and local_exprs.
        for name in &self.builtins {
            new_builtin_vars.entry(*name).or_insert_with(|| {
                assert!(
                    self.config.builtin_list.is_some(),
                    "if builtin_list is not some, output builtins should cover all input builtins"
                );

                let var = self.builtin_vars[name];
                self.local_exprs.insert(*name, self.ctx.get_unadjusted(var).clone());
                var
            });
        }
        self.builtin_vars = new_builtin_vars;
    }

    /// Handles `SegmentArena` validation.
    fn validate_segment_arena(&mut self) {
        let segment_arena = self.builtin_vars.remove(&BuiltinName::segment_arena).unwrap();
        casm_build_extend! {self.ctx,
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

    /// Processes the output builtins for the end of a run.
    fn process_builtins_output(&mut self) {
        for name in &self.builtins {
            if let Some(mut var) = self.builtin_vars.remove(name) {
                if let Some(local_expr) = self.local_exprs.get(name) {
                    // If the builtin was stored in a local variable, use it instead of the output
                    // builtin var.
                    var = self.ctx.add_var(local_expr.clone());
                }
                casm_build_extend!(self.ctx, tempvar cell = var;);
            }
        }
        assert!(self.builtin_vars.is_empty());
    }
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

/// Creates the metadata required for a Sierra program lowering to CASM.
fn create_metadata(
    sierra_program: &SierraProgram,
    program_info: &ProgramRegistryInfo,
    metadata_config: Option<MetadataComputationConfig>,
) -> Result<Metadata, BuildError> {
    if let Some(metadata_config) = metadata_config {
        calc_metadata(sierra_program, program_info, metadata_config)
    } else {
        calc_metadata_ap_change_only(sierra_program, program_info)
    }
    .map_err(|err| match err {
        MetadataError::ApChangeError(err) => BuildError::ApChangeError(err),
        MetadataError::CostError(err) => BuildError::FailedGasCalculation(err),
    })
}
