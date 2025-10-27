use std::fmt::{Debug, Display};

use cairo_lang_lowering::ids::FunctionLongId;
use cairo_lang_runnable_utils::builder::RunnableBuilder;
use cairo_lang_sierra::extensions::core::CoreConcreteLibfunc;
use cairo_lang_sierra::ids::ConcreteLibfuncId;
use cairo_lang_sierra::program::{GenStatement, Program, StatementIdx};
use cairo_lang_sierra_generator::db::SierraGenGroup;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::require;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use cairo_vm::vm::trace::trace_entry::RelocatedTraceEntry;
use itertools::{Itertools, chain};
use salsa::Database;

use crate::ProfilingInfoCollectionConfig;

#[cfg(test)]
#[path = "profiling_test.rs"]
mod test;

/// Profiler configuration.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ProfilerConfig {
    /// Profiler with Cairo level debug information.
    Cairo,
    /// Similar to Cairo, but stack frames are deduplicated, and the output format is more compact.
    Scoped,
    /// Sierra-level profiling, no Cairo-level debug information.
    Sierra,
}

impl ProfilerConfig {
    /// Returns true if the profiling config requires Cairo-level debug information.
    pub fn requires_cairo_debug_info(&self) -> bool {
        matches!(self, ProfilerConfig::Cairo | ProfilerConfig::Scoped)
    }
}

/// Profiling info of a single run. This is the raw info — it went through minimal processing, as
/// this is done during the run. To enrich it before viewing/printing, use the
/// `ProfilingInfoProcessor`.
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ProfilingInfo {
    /// The number of steps in the trace that originated from each Sierra statement.
    pub sierra_statement_weights: UnorderedHashMap<StatementIdx, usize>,

    /// A map of weights of each stack trace.
    /// The key is a function stack trace of an executed function. The stack trace is represented
    /// as a vector of indices of the functions in the stack (indices of the functions according to
    /// the list in the Sierra program).
    /// The value is the weight of the stack trace.
    /// The stack trace entries are sorted in the order they occur.
    pub stack_trace_weights: OrderedHashMap<Vec<usize>, usize>,

    /// The number of steps in the trace that originated from each Sierra statement
    /// combined with information about the user function call stack.
    /// The call stack items are deduplicated to flatten and aggregate recursive calls
    /// and loops (which are tail recursion).
    /// The entries are sorted in the order they occur.
    pub scoped_sierra_statement_weights: OrderedHashMap<(Vec<usize>, StatementIdx), usize>,
}

impl ProfilingInfo {
    pub fn from_trace(
        builder: &RunnableBuilder,
        // The offset in memory where builder.casm_program() was loaded.
        load_offset: usize,
        profiling_config: &ProfilingInfoCollectionConfig,
        trace: &[RelocatedTraceEntry],
    ) -> Self {
        let sierra_statement_info = &builder.casm_program().debug_info.sierra_statement_info;
        let sierra_len = sierra_statement_info.len();
        let bytecode_len = sierra_statement_info.last().unwrap().end_offset;

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
        for step in trace {
            // Skip the header.
            let Some(real_pc) = step.pc.checked_sub(load_offset) else {
                continue;
            };

            // Skip the footer.
            // Also if pc is greater or equal the bytecode length it means that it is the outside
            // ret used for, e.g., getting pointer to builtins costs table, const segments
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
            let sierra_statement_idx = builder.casm_program().sierra_statement_index_by_pc(real_pc);
            let user_function_idx = user_function_idx_by_sierra_statement_idx(
                builder.sierra_program(),
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
                builder.sierra_program().statements.get(sierra_statement_idx.0)
            else {
                panic!("Failed fetching statement index {}", sierra_statement_idx.0);
            };

            match gen_statement {
                GenStatement::Invocation(invocation) => {
                    if matches!(
                        builder.registry().get_libfunc(&invocation.libfunc_id),
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
}

/// Weights per libfunc.
#[derive(Default)]
pub struct LibfuncWeights {
    /// Weight (in steps in the relevant run) of each concrete libfunc.
    pub concrete_libfunc_weights: Option<OrderedHashMap<String, usize>>,
    /// Weight (in steps in the relevant run) of each generic libfunc.
    pub generic_libfunc_weights: Option<OrderedHashMap<String, usize>>,
    /// Weight (in steps in the relevant run) of return statements.
    pub return_weight: Option<usize>,
}
impl Display for LibfuncWeights {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(concrete_libfunc_weights) = &self.concrete_libfunc_weights {
            writeln!(f, "Weight by concrete libfunc:")?;
            for (concrete_name, weight) in concrete_libfunc_weights.iter() {
                writeln!(f, "  libfunc {concrete_name}: {weight}")?;
            }
            writeln!(
                f,
                "  return: {}",
                self.return_weight.expect(
                    "return_weight should have a value if concrete_libfunc_weights has a value"
                )
            )?;
        }
        if let Some(generic_libfunc_weights) = &self.generic_libfunc_weights {
            writeln!(f, "Weight by generic libfunc:")?;
            for (generic_name, weight) in generic_libfunc_weights.iter() {
                writeln!(f, "  libfunc {generic_name}: {weight}")?;
            }
            writeln!(
                f,
                "  return: {}",
                self.return_weight.expect(
                    "return_weight should have a value if generic_libfunc_weights has a value"
                )
            )?;
        }
        Ok(())
    }
}

/// Weights per user function.
#[derive(Default)]
pub struct UserFunctionWeights {
    /// Weight (in steps in the relevant run) of each user function (including generated
    /// functions).
    pub user_function_weights: Option<OrderedHashMap<String, usize>>,
    /// Weight (in steps in the relevant run) of each original user function.
    pub original_user_function_weights: Option<OrderedHashMap<String, usize>>,
}
impl Display for UserFunctionWeights {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(user_function_weights) = &self.user_function_weights {
            writeln!(f, "Weight by user function (inc. generated):")?;
            for (name, weight) in user_function_weights.iter() {
                writeln!(f, "  function {name}: {weight}")?;
            }
        }
        if let Some(original_user_function_weights) = &self.original_user_function_weights {
            writeln!(f, "Weight by original user function (exc. generated):")?;
            for (name, weight) in original_user_function_weights.iter() {
                writeln!(f, "  function {name}: {weight}")?;
            }
        }
        Ok(())
    }
}

/// Weights per stack trace.
#[derive(Default)]
pub struct StackTraceWeights {
    /// A map of weights of each Sierra stack trace.
    /// The key is a function stack trace of an executed function. The stack trace is represented
    /// as a vector of the function names.
    /// The value is the weight of the stack trace.
    pub sierra_stack_trace_weights: Option<OrderedHashMap<Vec<String>, usize>>,
    /// A map of weights of each stack trace, only for stack traces that are fully semantic
    /// (equivalent to Cairo traces). That is, none of the trace components is generated.
    /// This is a filtered map of `sierra_stack_trace_weights`.
    pub cairo_stack_trace_weights: Option<OrderedHashMap<Vec<String>, usize>>,
}
impl Display for StackTraceWeights {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(sierra_stack_trace_weights) = &self.sierra_stack_trace_weights {
            writeln!(f, "Weight by Sierra stack trace:")?;
            for (stack_trace, weight) in sierra_stack_trace_weights.iter() {
                let stack_trace_str = stack_trace.join(" -> ");
                writeln!(f, "  {stack_trace_str}: {weight}")?;
            }
        }

        if let Some(cairo_stack_trace_weights) = &self.cairo_stack_trace_weights {
            writeln!(f, "Weight by Cairo stack trace:")?;
            for (stack_trace, weight) in cairo_stack_trace_weights.iter() {
                let stack_trace_str = stack_trace.join(" -> ");
                writeln!(f, "  {stack_trace_str}: {weight}")?;
            }
        }
        Ok(())
    }
}

/// Full profiling info of a single run. This is the processed info which went through additional
/// processing after collecting the raw data during the run itself.
pub struct ProcessedProfilingInfo {
    /// For each Sierra statement: the number of steps in the trace that originated from it, and
    /// the relevant GenStatement.
    pub sierra_statement_weights:
        Option<OrderedHashMap<StatementIdx, (usize, GenStatement<StatementIdx>)>>,

    /// Weights per stack trace.
    pub stack_trace_weights: StackTraceWeights,

    /// Weights per libfunc.
    pub libfunc_weights: LibfuncWeights,

    /// Weights per user function.
    pub user_function_weights: UserFunctionWeights,

    /// Weight (in steps in the relevant run) of each Cairo function.
    pub cairo_function_weights: Option<OrderedHashMap<String, usize>>,

    /// For each Sierra statement in the scope of a particular call stack with deduplicated frames
    /// (collapsed recursion): the number of steps in the trace that originated from it.
    pub scoped_sierra_statement_weights: Option<OrderedHashMap<Vec<String>, usize>>,
}
impl Display for ProcessedProfilingInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(sierra_statement_weights) = &self.sierra_statement_weights {
            writeln!(f, "Weight by sierra statement:")?;
            for (statement_idx, (weight, gen_statement)) in sierra_statement_weights.iter() {
                writeln!(f, "  statement {statement_idx}: {weight} ({gen_statement})")?;
            }
        }

        // libfunc weights.
        self.libfunc_weights.fmt(f)?;

        // user functions.
        self.user_function_weights.fmt(f)?;

        if let Some(cairo_function_weights) = &self.cairo_function_weights {
            writeln!(f, "Weight by Cairo function:")?;
            for (function_identifier, weight) in cairo_function_weights.iter() {
                writeln!(f, "  function {function_identifier}: {weight}")?;
            }
        }

        self.stack_trace_weights.fmt(f)?;

        if let Some(weights) = &self.scoped_sierra_statement_weights {
            format_scoped_sierra_statement_weights(weights, f)?;
        }

        Ok(())
    }
}

/// Parameters controlling what profiling info is processed and how, by the
/// `ProfilingInfoProcessor`.
pub struct ProfilingInfoProcessorParams {
    /// The minimal weight to include in the output. Used for all collected stats. That is - the
    /// sum of the weights per statement may be smaller than the sum of the weights per concrete
    /// libfunc, that may be smaller than the sum of the weights per generic libfunc.
    pub min_weight: usize,
    /// Whether to process the profiling info by Sierra statement.
    pub process_by_statement: bool,
    /// Whether to process the profiling info by concrete libfunc.
    pub process_by_concrete_libfunc: bool,
    /// Whether to process the profiling info by generic libfunc.
    pub process_by_generic_libfunc: bool,
    /// Whether to process the profiling info by user function, including generated functions.
    pub process_by_user_function: bool,
    /// Whether to process the profiling info by original user function only.
    pub process_by_original_user_function: bool,
    /// Whether to process the profiling info by Cairo function (computed from the compiler
    /// StableLocation).
    pub process_by_cairo_function: bool,
    /// Whether to process the profiling info by Sierra stack trace (including generated
    /// functions in the traces).
    pub process_by_stack_trace: bool,
    /// Whether to process the profiling info by Cairo stack trace (that is, no generated
    /// functions in the traces).
    pub process_by_cairo_stack_trace: bool,
    /// Process the profiling info by Sierra statement in the scope of a particular
    /// call stack (recursion collapsed) and output in a format compatible with Flamegraph.
    pub process_by_scoped_statement: bool,
}
impl Default for ProfilingInfoProcessorParams {
    fn default() -> Self {
        Self {
            min_weight: 1,
            process_by_statement: true,
            process_by_concrete_libfunc: true,
            process_by_generic_libfunc: true,
            process_by_user_function: true,
            process_by_original_user_function: true,
            process_by_cairo_function: true,
            process_by_stack_trace: true,
            process_by_cairo_stack_trace: true,
            process_by_scoped_statement: false,
        }
    }
}

impl ProfilingInfoProcessorParams {
    pub fn from_profiler_config(config: &ProfilerConfig) -> Self {
        match config {
            ProfilerConfig::Cairo => Default::default(),
            ProfilerConfig::Scoped => Self {
                min_weight: 1,
                process_by_statement: false,
                process_by_concrete_libfunc: false,
                process_by_generic_libfunc: false,
                process_by_user_function: false,
                process_by_original_user_function: false,
                process_by_cairo_function: false,
                process_by_stack_trace: false,
                process_by_cairo_stack_trace: false,
                process_by_scoped_statement: true,
            },
            ProfilerConfig::Sierra => Self {
                process_by_generic_libfunc: false,
                process_by_cairo_stack_trace: false,
                process_by_original_user_function: false,
                process_by_cairo_function: false,
                ..ProfilingInfoProcessorParams::default()
            },
        }
    }
}

/// A processor for profiling info. Used to process the raw profiling info (basic info collected
/// during the run) into a more detailed profiling info that can also be formatted.
pub struct ProfilingInfoProcessor<'a> {
    db: Option<&'a dyn Database>,
    sierra_program: &'a Program,
    /// A map between Sierra statement index and the string representation of the Cairo function
    /// that generated it. The function representation is composed of the function name and the
    /// path (modules and impls) to the function in the file.
    statements_functions: UnorderedHashMap<StatementIdx, String>,
}
impl<'a> ProfilingInfoProcessor<'a> {
    pub fn new(
        db: Option<&'a dyn Database>,
        sierra_program: &'a Program,
        statements_functions: UnorderedHashMap<StatementIdx, String>,
    ) -> Self {
        Self { db, sierra_program, statements_functions }
    }

    /// Processes the raw profiling info according to the given params.
    pub fn process(
        &self,
        raw_profiling_info: &ProfilingInfo,
        params: &ProfilingInfoProcessorParams,
    ) -> ProcessedProfilingInfo {
        let sierra_statement_weights_iter = raw_profiling_info
            .sierra_statement_weights
            .iter_sorted_by_key(|(pc, count)| (usize::MAX - **count, **pc));

        let sierra_statement_weights =
            self.process_sierra_statement_weights(sierra_statement_weights_iter.clone(), params);

        let stack_trace_weights = self.process_stack_trace_weights(raw_profiling_info, params);

        let libfunc_weights =
            self.process_libfunc_weights(sierra_statement_weights_iter.clone(), params);

        let user_function_weights =
            self.process_user_function_weights(sierra_statement_weights_iter.clone(), params);

        let cairo_function_weights =
            self.process_cairo_function_weights(sierra_statement_weights_iter, params);

        let scoped_sierra_statement_weights =
            self.process_scoped_sierra_statement_weights(raw_profiling_info, params);

        ProcessedProfilingInfo {
            sierra_statement_weights,
            stack_trace_weights,
            libfunc_weights,
            user_function_weights,
            cairo_function_weights,
            scoped_sierra_statement_weights,
        }
    }

    /// Process the weights per Sierra statement.
    fn process_sierra_statement_weights(
        &self,
        sierra_statement_weights_iter: std::vec::IntoIter<(&StatementIdx, &usize)>,
        params: &ProfilingInfoProcessorParams,
    ) -> Option<OrderedHashMap<StatementIdx, (usize, GenStatement<StatementIdx>)>> {
        require(params.process_by_statement)?;

        Some(
            sierra_statement_weights_iter
                .filter(|&(_, weight)| *weight >= params.min_weight)
                .map(|(statement_idx, weight)| {
                    (*statement_idx, (*weight, self.statement_idx_to_gen_statement(statement_idx)))
                })
                .collect(),
        )
    }

    /// Process the weights per stack trace.
    fn process_stack_trace_weights(
        &self,
        raw_profiling_info: &ProfilingInfo,
        params: &ProfilingInfoProcessorParams,
    ) -> StackTraceWeights {
        let resolve_names = |(idx_stack_trace, weight): (&Vec<usize>, &usize)| {
            (index_stack_trace_to_name_stack_trace(self.sierra_program, idx_stack_trace), *weight)
        };

        let sierra_stack_trace_weights = params.process_by_stack_trace.then(|| {
            raw_profiling_info
                .stack_trace_weights
                .iter()
                .sorted_by_key(|&(trace, weight)| (usize::MAX - *weight, trace.clone()))
                .map(resolve_names)
                .collect()
        });

        let cairo_stack_trace_weights = params.process_by_cairo_stack_trace.then(|| {
            let db = self.db.expect("DB must be set with `process_by_cairo_stack_trace=true`.");
            raw_profiling_info
                .stack_trace_weights
                .iter()
                .filter(|(trace, _)| is_cairo_trace(db, self.sierra_program, trace))
                .sorted_by_key(|&(trace, weight)| (usize::MAX - *weight, trace.clone()))
                .map(resolve_names)
                .collect()
        });

        StackTraceWeights { sierra_stack_trace_weights, cairo_stack_trace_weights }
    }

    /// Process the weights per libfunc.
    fn process_libfunc_weights(
        &self,
        sierra_statement_weights: std::vec::IntoIter<(&StatementIdx, &usize)>,
        params: &ProfilingInfoProcessorParams,
    ) -> LibfuncWeights {
        if !params.process_by_concrete_libfunc && !params.process_by_generic_libfunc {
            return LibfuncWeights::default();
        }

        let mut return_weight = 0;
        let mut libfunc_weights = UnorderedHashMap::<ConcreteLibfuncId, usize>::default();
        for (statement_idx, weight) in sierra_statement_weights {
            match self.statement_idx_to_gen_statement(statement_idx) {
                GenStatement::Invocation(invocation) => {
                    *(libfunc_weights.entry(invocation.libfunc_id.clone()).or_insert(0)) += weight;
                }
                GenStatement::Return(_) => {
                    return_weight += weight;
                }
            }
        }

        let generic_libfunc_weights = params.process_by_generic_libfunc.then(|| {
            let db: &dyn Database =
                self.db.expect("DB must be set with `process_by_generic_libfunc=true`.");
            libfunc_weights
                .aggregate_by(
                    |k| db.lookup_concrete_lib_func(k).generic_id.to_string(),
                    |v1: &usize, v2| v1 + v2,
                    &0,
                )
                .filter(|_, weight| *weight >= params.min_weight)
                .into_iter_sorted_by_key(|(generic_name, weight)| {
                    (usize::MAX - *weight, (*generic_name).clone())
                })
                .collect()
        });

        // This is done second as .filter() is consuming and to avoid cloning.
        let concrete_libfunc_weights = params.process_by_concrete_libfunc.then(|| {
            libfunc_weights
                .filter(|_, weight| *weight >= params.min_weight)
                .into_iter_sorted_by_key(|(libfunc_id, weight)| {
                    (usize::MAX - *weight, libfunc_id.to_string())
                })
                .map(|(libfunc_id, weight)| (libfunc_id.to_string(), weight))
                .collect()
        });

        LibfuncWeights {
            concrete_libfunc_weights,
            generic_libfunc_weights,
            return_weight: Some(return_weight),
        }
    }

    /// Process the weights per user function.
    fn process_user_function_weights(
        &self,
        sierra_statement_weights: std::vec::IntoIter<(&StatementIdx, &usize)>,
        params: &ProfilingInfoProcessorParams,
    ) -> UserFunctionWeights {
        if !params.process_by_user_function && !params.process_by_original_user_function {
            return UserFunctionWeights::default();
        }

        let mut user_functions = UnorderedHashMap::<usize, usize>::default();
        for (statement_idx, weight) in sierra_statement_weights {
            let function_idx: usize =
                user_function_idx_by_sierra_statement_idx(self.sierra_program, *statement_idx);
            *(user_functions.entry(function_idx).or_insert(0)) += weight;
        }

        let original_user_function_weights = params.process_by_original_user_function.then(|| {
            let db: &dyn Database =
                self.db.expect("DB must be set with `process_by_original_user_function=true`.");
            user_functions
                .aggregate_by(
                    |idx| {
                        let lowering_function_id =
                            db.lookup_sierra_function(&self.sierra_program.funcs[*idx].id);
                        lowering_function_id.semantic_full_path(db)
                    },
                    |x, y| x + y,
                    &0,
                )
                .filter(|_, weight| *weight >= params.min_weight)
                .iter_sorted_by_key(|(orig_name, weight)| {
                    (usize::MAX - **weight, (*orig_name).clone())
                })
                .map(|(orig_name, weight)| (orig_name.clone(), *weight))
                .collect()
        });

        // This is done second as .filter() is consuming and to avoid cloning.
        let user_function_weights = params.process_by_user_function.then(|| {
            user_functions
                .filter(|_, weight| *weight >= params.min_weight)
                .iter_sorted_by_key(|(idx, weight)| {
                    (usize::MAX - **weight, self.sierra_program.funcs[**idx].id.to_string())
                })
                .map(|(idx, weight)| {
                    let func: &cairo_lang_sierra::program::GenFunction<StatementIdx> =
                        &self.sierra_program.funcs[*idx];
                    (func.id.to_string(), *weight)
                })
                .collect()
        });

        UserFunctionWeights { user_function_weights, original_user_function_weights }
    }

    /// Process the weights per Cairo function.
    fn process_cairo_function_weights(
        &self,
        sierra_statement_weights: std::vec::IntoIter<(&StatementIdx, &usize)>,
        params: &ProfilingInfoProcessorParams,
    ) -> Option<OrderedHashMap<String, usize>> {
        require(params.process_by_cairo_function)?;

        let mut cairo_functions = UnorderedHashMap::<_, _>::default();
        for (statement_idx, weight) in sierra_statement_weights {
            // TODO(Gil): Fill all the `Unknown functions` in the Cairo functions profiling.
            let function_identifier = self
                .statements_functions
                .get(statement_idx)
                .unwrap_or(&"unknown".to_string())
                .clone();
            *(cairo_functions.entry(function_identifier).or_insert(0)) += weight;
        }

        Some(
            cairo_functions
                .filter(|_, weight| *weight >= params.min_weight)
                .iter_sorted_by_key(|(function_identifier, weight)| {
                    (usize::MAX - **weight, (*function_identifier).clone())
                })
                .map(|(function_identifier, weight)| (function_identifier.clone(), *weight))
                .collect(),
        )
    }

    /// Process Sierra statement weights in the scope of a particular call stack
    fn process_scoped_sierra_statement_weights(
        &self,
        raw_profiling_info: &ProfilingInfo,
        params: &ProfilingInfoProcessorParams,
    ) -> Option<OrderedHashMap<Vec<String>, usize>> {
        if params.process_by_scoped_statement {
            let mut scoped_sierra_statement_weights: OrderedHashMap<Vec<String>, usize> =
                Default::default();
            for ((idx_stack_trace, statement_idx), weight) in
                raw_profiling_info.scoped_sierra_statement_weights.iter()
            {
                let statement_name = match self.statement_idx_to_gen_statement(statement_idx) {
                    GenStatement::Invocation(invocation) => invocation.libfunc_id.to_string(),
                    GenStatement::Return(_) => "return".into(),
                };
                let key: Vec<String> = chain!(
                    index_stack_trace_to_name_stack_trace(self.sierra_program, idx_stack_trace),
                    [statement_name]
                )
                .collect();
                // Accumulating statements with the same name.
                *scoped_sierra_statement_weights.entry(key).or_default() += *weight;
            }
            return Some(scoped_sierra_statement_weights);
        }
        None
    }

    /// Translates the given Sierra statement index into the actual statement.
    fn statement_idx_to_gen_statement(
        &self,
        statement_idx: &StatementIdx,
    ) -> GenStatement<StatementIdx> {
        self.sierra_program
            .statements
            .get(statement_idx.0)
            .unwrap_or_else(|| panic!("Failed fetching statement index {}", statement_idx.0))
            .clone()
    }
}

/// Checks if the given stack trace is fully semantic (so it is equivalent to a Cairo trace). That
/// is, none of the trace components is generated.
fn is_cairo_trace(db: &dyn Database, sierra_program: &Program, sierra_trace: &[usize]) -> bool {
    sierra_trace.iter().all(|sierra_function_idx| {
        let sierra_function = &sierra_program.funcs[*sierra_function_idx];
        let lowering_function_id = db.lookup_sierra_function(&sierra_function.id);
        matches!(lowering_function_id.long(db), FunctionLongId::Semantic(_))
    })
}

/// Converts a Sierra statement index to the index of the function that contains it (the index in
/// the list in the Sierra program).
///
/// Assumes that the given `statement_idx` is valid (that is within range of the given
/// `sierra_program`) and that the given `sierra_program` is valid, specifically that the first
/// function's entry point is 0.
pub fn user_function_idx_by_sierra_statement_idx(
    sierra_program: &Program,
    statement_idx: StatementIdx,
) -> usize {
    // The `-1` here can't cause an underflow as the first function's entry point is
    // always 0, so it is always on the left side of the partition, and thus the
    // partition index is >0.
    sierra_program.funcs.partition_point(|f| f.entry_point.0 <= statement_idx.0) - 1
}

/// Converts a stack trace represented as a vector of indices of functions in the Sierra program to
/// a stack trace represented as a vector of function names.
/// Assumes that the given `idx_stack_trace` is valid with respect to the given `sierra_program`.
/// That is, each index in the stack trace is within range of the Sierra program.
fn index_stack_trace_to_name_stack_trace(
    sierra_program: &Program,
    idx_stack_trace: &[usize],
) -> Vec<String> {
    idx_stack_trace.iter().map(|idx| sierra_program.funcs[*idx].id.to_string()).collect()
}

/// Writes scoped Sierra statement weights data in a FlameGraph compatible format.
fn format_scoped_sierra_statement_weights(
    weights: &OrderedHashMap<Vec<String>, usize>,
    f: &mut std::fmt::Formatter<'_>,
) -> std::fmt::Result {
    for (key, weight) in weights.iter() {
        f.write_fmt(format_args!("{} {weight}\n", key.join(";")))?;
    }
    Ok(())
}
