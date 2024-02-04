use std::fmt::Display;

use cairo_lang_lowering::ids::FunctionLongId;
use cairo_lang_sierra::program::{GenStatement, Program, StatementIdx};
use cairo_lang_sierra_generator::db::SierraGenGroup;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use smol_str::SmolStr;

#[cfg(test)]
#[path = "profiling_test.rs"]
mod test;

/// Profiling into of a single run. This is the raw info - went through minimum processing, as this
/// is done during the run. To enrich it before viewing/printing, use the `ProfilingInfoProcessor`.
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ProfilingInfo {
    /// The number of steps in the trace that originated from each sierra statement.
    pub sierra_statements_weights: UnorderedHashMap<StatementIdx, usize>,

    /// A map of weights of each stack trace.
    /// The key is a function stack trace of an executed function. The stack trace is represented
    /// as a vector of indices of the functions in the stack (indices of the functions according to
    /// the list in the sierra program).
    /// The value is the weight of the stack trace.
    pub stack_trace_weights: UnorderedHashMap<Vec<usize>, usize>,
}

// Full profiling info of a single run. This is the processed info which went through additional
// processing after collecting the raw data during the run itself.
pub struct ProcessedProfilingInfo {
    /// For each sierra statement: the number of steps in the trace that originated from it, and
    /// the relevant GenStatement.
    pub sierra_statements_weights:
        OrderedHashMap<StatementIdx, (usize, GenStatement<StatementIdx>)>,
    /// A map of weights of each stack trace.
    /// The key is a function stack trace of an executed function. The stack trace is represented
    /// as a vector of the function names.
    /// The value is the weight of the stack trace.
    pub stack_trace_weights: OrderedHashMap<Vec<String>, usize>,

    /// Weight (in steps in the relevant run) of each concrete libfunc.
    pub concrete_libfuncs_weights: Option<OrderedHashMap<SmolStr, usize>>,
    /// Weight (in steps in the relevant run) of each generic libfunc.
    pub generic_libfuncs_weights: Option<OrderedHashMap<SmolStr, usize>>,
    /// Weight (in steps in the relevant run) of each user function (including generated
    /// functions).
    pub user_functions_weights: Option<OrderedHashMap<SmolStr, usize>>,
    /// Weight (in steps in the relevant run) of each original user function.
    pub original_user_functions_weights: Option<OrderedHashMap<String, usize>>,
    /// Weight (in steps in the relevant run) of each Cairo function.
    pub cairo_functions_weights: Option<OrderedHashMap<String, usize>>,
    /// A map of weights of each stack trace, only for stack traces that are fully semantic
    /// (equivalent to Cairo traces). That is, none of the traces components is generated.
    /// This is a filtered map of stack_trace_weights.
    pub cairo_stack_trace_weights: Option<OrderedHashMap<Vec<String>, usize>>,
    /// Weight (in steps in the relevant run) of return statements.
    pub return_weight: usize,
}
impl Display for ProcessedProfilingInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Weight by sierra statement:")?;
        for (statement_idx, (weight, gen_statement)) in self.sierra_statements_weights.iter() {
            writeln!(f, "  statement {statement_idx}: {weight} ({gen_statement})")?;
        }
        if let Some(concrete_libfuncs_weights) = &self.concrete_libfuncs_weights {
            writeln!(f, "Weight by concrete libfunc:")?;
            for (concrete_name, weight) in concrete_libfuncs_weights.iter() {
                writeln!(f, "  libfunc {concrete_name}: {weight}")?;
            }
            writeln!(f, "  return: {}", self.return_weight)?;
        }
        if let Some(generic_libfuncs_weights) = &self.generic_libfuncs_weights {
            writeln!(f, "Weight by generic libfunc:")?;
            for (generic_name, weight) in generic_libfuncs_weights.iter() {
                writeln!(f, "  libfunc {generic_name}: {weight}")?;
            }
            writeln!(f, "  return: {}", self.return_weight)?;
        }
        if let Some(user_functions_weights) = &self.user_functions_weights {
            writeln!(f, "Weight by user function (inc. generated):")?;
            for (name, weight) in user_functions_weights.iter() {
                writeln!(f, "  function {name}: {weight}")?;
            }
        }
        if let Some(original_user_functions_weights) = &self.original_user_functions_weights {
            writeln!(f, "Weight by original user function (exc. generated):")?;
            for (name, weight) in original_user_functions_weights.iter() {
                writeln!(f, "  function {name}: {weight}")?;
            }
        }
        if let Some(cairo_functions_weights) = &self.cairo_functions_weights {
            writeln!(f, "Weight by Cairo function:")?;
            for (function_identifier, weight) in cairo_functions_weights.iter() {
                writeln!(f, "  function {function_identifier}: {weight}")?;
            }
        }

        writeln!(f, "Weight by Sierra stack trace:")?;
        for (stack_trace, weight) in self.stack_trace_weights.iter() {
            let stack_trace_str = stack_trace.join(" -> ");
            writeln!(f, "  {stack_trace_str}: {weight}")?;
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

/// Parameters controlling what profiling info is processed and how, by the
/// `ProfilingInfoProcessor`.
pub struct ProfilingInfoProcessorParams {
    /// The minimal weight to include in the output. Used for all collected stats. That is - the
    /// sum of the weights per statement may be smaller than the sum of the weights per concrete
    /// libfunc, that may be smaller than the sum of the weights per generic libfunc.
    pub min_weight: usize,
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
    /// Whether to process the profiling info by Cairo stack trace (that is, no generated
    /// functions in the traces).
    pub process_by_cairo_stack_trace: bool,
}
impl Default for ProfilingInfoProcessorParams {
    fn default() -> Self {
        Self {
            min_weight: 1,
            process_by_concrete_libfunc: true,
            process_by_generic_libfunc: true,
            process_by_user_function: true,
            process_by_original_user_function: true,
            process_by_cairo_function: true,
            process_by_cairo_stack_trace: true,
        }
    }
}
/// A processor for profiling info. Used to process the raw profiling info (basic info collected
/// during the run) into a more detailed profiling info that can also be formatted.
pub struct ProfilingInfoProcessor<'a> {
    db: Option<&'a dyn SierraGenGroup>,
    sierra_program: Program,
    /// A map between sierra statement index and the string representation of the Cairo function
    /// that generated it. The function representation is composed of the function name and the
    /// path (modules and impls) to the function in the file.
    statements_functions: UnorderedHashMap<StatementIdx, String>,
    params: ProfilingInfoProcessorParams,
}
impl<'a> ProfilingInfoProcessor<'a> {
    pub fn new(
        db: Option<&'a dyn SierraGenGroup>,
        sierra_program: Program,
        statements_functions: UnorderedHashMap<StatementIdx, String>,
    ) -> Self {
        Self {
            db,
            sierra_program,
            statements_functions,
            params: ProfilingInfoProcessorParams::default(),
        }
    }

    /// Processes the raw profiling info according to the params set in the processor.
    pub fn process(&self, raw_profiling_info: &ProfilingInfo) -> ProcessedProfilingInfo {
        self.process_ex(raw_profiling_info, &self.params)
    }

    /// Processes the raw profiling info according to the given params (can be used to override the
    /// processor params).
    pub fn process_ex(
        &self,
        raw_profiling_info: &ProfilingInfo,
        params: &ProfilingInfoProcessorParams,
    ) -> ProcessedProfilingInfo {
        let mut concrete_libfuncs = if params.process_by_concrete_libfunc {
            self.sierra_program
                .libfunc_declarations
                .iter()
                .map(|concrete_libfunc| (concrete_libfunc.long_id.to_string().into(), 0))
                .collect::<UnorderedHashMap<SmolStr, usize>>()
        } else {
            UnorderedHashMap::default()
        };

        let mut generic_libfuncs = if params.process_by_generic_libfunc {
            self.sierra_program
                .libfunc_declarations
                .iter()
                .map(|generic_libfunc| (generic_libfunc.long_id.generic_id.0.clone(), 0))
                .collect::<UnorderedHashMap<SmolStr, usize>>()
        } else {
            UnorderedHashMap::default()
        };

        let mut user_functions = if params.process_by_user_function {
            self.sierra_program
                .funcs
                .iter()
                .enumerate()
                .map(|(idx, _)| (idx, 0))
                .collect::<UnorderedHashMap<usize, usize>>()
        } else {
            UnorderedHashMap::default()
        };

        let mut cairo_functions = UnorderedHashMap::<_, _>::default();
        let mut return_weight = 0;
        let mut sierra_statements_weights = OrderedHashMap::default();

        for (statement_idx, weight) in raw_profiling_info
            .sierra_statements_weights
            .iter_sorted_by_key(|(pc, count)| (usize::MAX - **count, **pc))
        {
            let Some(gen_statement) = self.sierra_program.statements.get(statement_idx.0) else {
                panic!("Failed fetching statement index {}", statement_idx.0);
            };
            match gen_statement {
                GenStatement::Invocation(invocation) => {
                    let concrete_name: SmolStr = format!("{}", invocation.libfunc_id).into();
                    if params.process_by_concrete_libfunc {
                        *(concrete_libfuncs.get_mut(&concrete_name).unwrap()) += weight;
                    }

                    if params.process_by_generic_libfunc {
                        let generic_name = concrete_name_to_generic_name(concrete_name);
                        *(generic_libfuncs.get_mut(&generic_name).unwrap()) += weight;
                    }
                }
                GenStatement::Return(_) => {
                    return_weight += weight;
                }
            }

            if params.process_by_user_function {
                let function_idx: usize =
                    user_function_idx_by_sierra_statement_idx(&self.sierra_program, statement_idx);
                *(user_functions.get_mut(&function_idx).unwrap()) += weight;
            }

            if params.process_by_cairo_function {
                // TODO(Gil): Fill all the `Unknown functions` in the cairo functions
                // profiling.
                let function_identifier = self
                    .statements_functions
                    .get(statement_idx)
                    .unwrap_or(&"unknown".to_string())
                    .clone();
                *(cairo_functions.entry(function_identifier).or_insert(0)) += weight;
            }
            if *weight >= params.min_weight {
                sierra_statements_weights.insert(*statement_idx, (*weight, gen_statement.clone()));
            }
        }

        let concrete_libfuncs_weights = params.process_by_concrete_libfunc.then(|| {
            let mut concrete_libfuncs_weights = OrderedHashMap::default();
            for (concrete_name, weight) in
                concrete_libfuncs.iter_sorted_by_key(|(concrete_name, weight)| {
                    (usize::MAX - **weight, (*concrete_name).clone())
                })
            {
                if *weight >= params.min_weight {
                    concrete_libfuncs_weights.insert(concrete_name.clone(), *weight);
                }
            }
            concrete_libfuncs_weights
        });

        let generic_libfuncs_weights = params.process_by_generic_libfunc.then(|| {
            let mut generic_libfuncs_weights = OrderedHashMap::default();
            for (generic_name, weight) in
                generic_libfuncs.iter_sorted_by_key(|(generic_name, weight)| {
                    (usize::MAX - **weight, (*generic_name).clone())
                })
            {
                if *weight >= params.min_weight {
                    generic_libfuncs_weights.insert(generic_name.clone(), *weight);
                }
            }
            generic_libfuncs_weights
        });

        let user_functions_weights = params.process_by_user_function.then(|| {
            let mut user_functions_weights = OrderedHashMap::default();
            for (idx, weight) in user_functions.iter_sorted_by_key(|(idx, weight)| {
                (usize::MAX - **weight, self.sierra_program.funcs[**idx].id.to_string())
            }) {
                if *weight >= params.min_weight {
                    let func = &self.sierra_program.funcs[*idx];
                    user_functions_weights.insert(func.id.to_string().into(), *weight);
                }
            }
            user_functions_weights
        });

        let original_user_functions_weights = params.process_by_original_user_function.then(|| {
            let db: &dyn SierraGenGroup =
                self.db.expect("DB must be set with `process_by_original_user_function=true`.");
            let mut original_user_functions_weights = OrderedHashMap::default();
            for (orig_name, weight) in user_functions
                .aggregate_by(
                    |idx| {
                        let lowering_function_id = db.lookup_intern_sierra_function(
                            self.sierra_program.funcs[*idx].id.clone(),
                        );
                        lowering_function_id.semantic_full_path(db.upcast())
                    },
                    |x, y| x + y,
                    &0,
                )
                .iter_sorted_by_key(|(orig_name, weight)| {
                    (usize::MAX - **weight, (*orig_name).clone())
                })
            {
                if *weight >= params.min_weight {
                    original_user_functions_weights.insert(orig_name.clone(), *weight);
                }
            }
            original_user_functions_weights
        });

        let cairo_functions_weights = params.process_by_cairo_function.then(|| {
            let mut cairo_functions_weights = OrderedHashMap::default();
            for (function_identifier, weight) in
                cairo_functions.iter_sorted_by_key(|(function_identifier, weight)| {
                    (usize::MAX - **weight, (*function_identifier).clone())
                })
            {
                if *weight >= params.min_weight {
                    cairo_functions_weights.insert(function_identifier.clone(), *weight);
                }
            }
            cairo_functions_weights
        });

        let stack_trace_weights = raw_profiling_info
            .stack_trace_weights
            .iter_sorted_by_key(|(idx_stack_trace, weight)| {
                (usize::MAX - **weight, (*idx_stack_trace).clone())
            })
            .map(|(idx_stack_trace, weight)| {
                (
                    index_stack_trace_to_name_stack_trace(&self.sierra_program, idx_stack_trace),
                    *weight,
                )
            })
            .collect();

        let cairo_stack_trace_weights = params.process_by_cairo_stack_trace.then(|| {
            let db = self.db.expect("DB must be set with `process_by_cairo_stack_trace=true`.");
            raw_profiling_info
                .stack_trace_weights
                .filter_cloned(|trace, _| is_cairo_trace(db, &self.sierra_program, trace))
                .into_iter_sorted_by_key(|(trace, weight)| (usize::MAX - *weight, trace.clone()))
                .map(|(idx_stack_trace, weight)| {
                    (
                        index_stack_trace_to_name_stack_trace(
                            &self.sierra_program,
                            &idx_stack_trace,
                        ),
                        weight,
                    )
                })
                .collect()
        });

        ProcessedProfilingInfo {
            sierra_statements_weights,
            stack_trace_weights,
            concrete_libfuncs_weights,
            generic_libfuncs_weights,
            user_functions_weights,
            original_user_functions_weights,
            cairo_functions_weights,
            cairo_stack_trace_weights,
            return_weight,
        }
    }
}

/// Checks if the given stack trace is fully semantic (so it is equivalent to a Cairo trace). That
/// is, none of the trace components is generated.
fn is_cairo_trace(
    db: &dyn SierraGenGroup,
    sierra_program: &Program,
    sierra_trace: &[usize],
) -> bool {
    let lowering_db = db.upcast();
    sierra_trace.iter().all(|sierra_function_idx| {
        let sierra_function = &sierra_program.funcs[*sierra_function_idx];
        let lowering_function_id = db.lookup_intern_sierra_function(sierra_function.id.clone());
        matches!(lowering_function_id.lookup(lowering_db), FunctionLongId::Semantic(_))
    })
}

/// Converts a sierra statement index to the index of the function that contains it (the index in
/// the list in the sierra program).
/// Assumes that the given `statement_idx` is valid (that is within range of the given
/// `sierra_program`) and that the given `sierra_program` is valid, specifically that the first
/// function's entry point is 0.
pub fn user_function_idx_by_sierra_statement_idx(
    sierra_program: &Program,
    statement_idx: &StatementIdx,
) -> usize {
    // The `-1` here can't cause an underflow as the first function's entry point is
    // always 0, so it is always on the left side of the partition, and thus the
    // partition index is >0.
    sierra_program.funcs.partition_point(|f| f.entry_point.0 <= statement_idx.0) - 1
}

/// Converts a stack trace represented as a vector of indices of functions in the sierra program to
/// a stack trace represented as a vector of function names.
/// Assumes that the given `idx_stack_trace` is valid with respect to the given `sierra_program`.
/// That is, each index in the stack trace is within range of the sierra program.
fn index_stack_trace_to_name_stack_trace(
    sierra_program: &Program,
    idx_stack_trace: &[usize],
) -> Vec<String> {
    idx_stack_trace.iter().map(|idx| sierra_program.funcs[*idx].id.to_string()).collect()
}

/// Extracts the generic libfunc name of the given concrete libfunc name.
fn concrete_name_to_generic_name(concrete_name: SmolStr) -> SmolStr {
    // TODO(yuval): This depends on the format of a concrete libfunc name. Fix to be more resilient
    // by getting the long ID from the short ID and from there get the generic name. This can be
    // done either using a DB, or with a sierra program registry, if it's extended to have this
    // mapping.
    concrete_name.split('<').next().unwrap().into()
}
