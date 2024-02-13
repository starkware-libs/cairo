use std::fmt::Display;

use cairo_lang_lowering::ids::FunctionLongId;
use cairo_lang_sierra::ids::ConcreteLibfuncId;
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
    pub sierra_statement_weights: UnorderedHashMap<StatementIdx, usize>,

    /// A map of weights of each stack trace.
    /// The key is a function stack trace of an executed function. The stack trace is represented
    /// as a vector of indices of the functions in the stack (indices of the functions according to
    /// the list in the sierra program).
    /// The value is the weight of the stack trace.
    pub stack_trace_weights: UnorderedHashMap<Vec<usize>, usize>,
}

/// Weights per libfunc.
#[derive(Default)]
pub struct LibfuncWeights {
    /// Weight (in steps in the relevant run) of each concrete libfunc.
    pub concrete_libfunc_weights: Option<OrderedHashMap<SmolStr, usize>>,
    /// Weight (in steps in the relevant run) of each generic libfunc.
    pub generic_libfunc_weights: Option<OrderedHashMap<SmolStr, usize>>,
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
    pub user_function_weights: Option<OrderedHashMap<SmolStr, usize>>,
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
    /// For each sierra statement: the number of steps in the trace that originated from it, and
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

        ProcessedProfilingInfo {
            sierra_statement_weights,
            stack_trace_weights,
            libfunc_weights,
            user_function_weights,
            cairo_function_weights,
        }
    }

    /// Process the weights per Sierra statement.
    fn process_sierra_statement_weights(
        &self,
        sierra_statement_weights_iter: std::vec::IntoIter<(&StatementIdx, &usize)>,
        params: &ProfilingInfoProcessorParams,
    ) -> Option<OrderedHashMap<StatementIdx, (usize, GenStatement<StatementIdx>)>> {
        if !params.process_by_statement {
            return None;
        }

        Some(
            sierra_statement_weights_iter
                .filter(|&(_, weight)| (*weight >= params.min_weight))
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
        let sierra_stack_trace_weights = params.process_by_stack_trace.then(|| {
            raw_profiling_info
                .stack_trace_weights
                .iter_sorted_by_key(|(idx_stack_trace, weight)| {
                    (usize::MAX - **weight, (*idx_stack_trace).clone())
                })
                .map(|(idx_stack_trace, weight)| {
                    (
                        index_stack_trace_to_name_stack_trace(
                            &self.sierra_program,
                            idx_stack_trace,
                        ),
                        *weight,
                    )
                })
                .collect()
        });

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
            let db: &dyn SierraGenGroup =
                self.db.expect("DB must be set with `process_by_generic_libfunc=true`.");
            libfunc_weights
                .aggregate_by(
                    |k| -> SmolStr {
                        db.lookup_intern_concrete_lib_func(k.clone()).generic_id.to_string().into()
                    },
                    |v1: &usize, v2| v1 + v2,
                    &0,
                )
                .filter(|_, weight| *weight >= params.min_weight)
                .iter_sorted_by_key(|(generic_name, weight)| {
                    (usize::MAX - **weight, (*generic_name).clone())
                })
                .map(|(generic_name, weight)| (generic_name.clone(), *weight))
                .collect()
        });

        // This is done second as .filter() is consuming and to avoid cloning.
        let concrete_libfunc_weights = params.process_by_concrete_libfunc.then(|| {
            libfunc_weights
                .filter(|_, weight| *weight >= params.min_weight)
                .iter_sorted_by_key(|(libfunc_id, weight)| {
                    (usize::MAX - **weight, (*libfunc_id).to_string())
                })
                .map(|(libfunc_id, weight)| (SmolStr::from(libfunc_id.to_string()), *weight))
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
                user_function_idx_by_sierra_statement_idx(&self.sierra_program, *statement_idx);
            *(user_functions.entry(function_idx).or_insert(0)) += weight;
        }

        let original_user_function_weights = params.process_by_original_user_function.then(|| {
            let db: &dyn SierraGenGroup =
                self.db.expect("DB must be set with `process_by_original_user_function=true`.");
            user_functions
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
                    (func.id.to_string().into(), *weight)
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
        if !params.process_by_cairo_function {
            return None;
        }

        let mut cairo_functions = UnorderedHashMap::<_, _>::default();
        for (statement_idx, weight) in sierra_statement_weights {
            // TODO(Gil): Fill all the `Unknown functions` in the cairo functions profiling.
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
    statement_idx: StatementIdx,
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
