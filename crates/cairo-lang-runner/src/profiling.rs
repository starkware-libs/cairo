use std::collections::HashMap;
use std::fmt::Display;

use cairo_lang_sierra::program::{GenStatement, Program, StatementIdx};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use itertools::Itertools;
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
}

// Full profiling info of a single run. This is the processed info which went through additional
// processing after collecting the raw data during the run itself.
pub struct ProcessedProfilingInfo {
    /// For each sierra statement: the number of steps in the trace that originated for it, and the
    /// relevant GenStatement.
    pub sierra_statements_weights:
        OrderedHashMap<StatementIdx, (usize, GenStatement<StatementIdx>)>,
    // Weight (in steps in the relevant run) of each concrete libfunc.
    pub concrete_libfuncs_weights: Option<OrderedHashMap<SmolStr, usize>>,
    // Weight (in steps in the relevant run) of each generic libfunc.
    pub generic_libfuncs_weights: Option<OrderedHashMap<SmolStr, usize>>,
    // Weight (in steps in the relevant run) of return statements.
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
    pub process_by_concrete_libfunc: bool,
    pub process_by_generic_libfunc: bool,
}
impl Default for ProfilingInfoProcessorParams {
    fn default() -> Self {
        Self { min_weight: 1, process_by_concrete_libfunc: true, process_by_generic_libfunc: true }
    }
}
/// A processor for profiling info. Used to process the raw profiling info (basic info collected
/// during the run) into a more detailed profiling info that can also be formatted.
pub struct ProfilingInfoProcessor {
    sierra_program: Program,
    params: ProfilingInfoProcessorParams,
}
impl ProfilingInfoProcessor {
    pub fn new(sierra_program: Program) -> Self {
        Self { sierra_program, params: ProfilingInfoProcessorParams::default() }
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
                .collect::<HashMap<SmolStr, usize>>()
        } else {
            HashMap::new()
        };

        let mut generic_libfuncs = if params.process_by_generic_libfunc {
            self.sierra_program
                .libfunc_declarations
                .iter()
                .map(|concrete_libfunc| (concrete_libfunc.long_id.generic_id.0.clone(), 0))
                .collect::<HashMap<SmolStr, usize>>()
        } else {
            HashMap::new()
        };

        let mut return_weight = 0;
        let mut statements_weights = OrderedHashMap::default();
        for (statement_idx, weight) in raw_profiling_info
            .sierra_statements_weights
            .iter()
            .sorted_by(|x, y| Ord::cmp(&(y.1, x.0.0), &(x.1, y.0.0)))
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
                        // TODO(yg): This depends on the format of a concrete libfunc name. Fix to
                        // be more resilient by getting the long ID from the short ID and from there
                        // get the generic name. This can be done either using a DB, or with a
                        // sierra program registry, if it's extended to have this mapping.
                        let generic_name: SmolStr = concrete_name.split('<').next().unwrap().into();
                        *(generic_libfuncs.get_mut(&generic_name).unwrap()) += weight;
                    }
                }
                GenStatement::Return(_) => {
                    return_weight += weight;
                }
            }
            if *weight >= params.min_weight {
                statements_weights.insert(*statement_idx, (*weight, gen_statement.clone()));
            }
        }

        let mut concrete_libfuncs_weights = OrderedHashMap::default();
        if params.process_by_concrete_libfunc {
            for (concrete_name, weight) in
                concrete_libfuncs.iter().sorted_by(|x, y| Ord::cmp(&(y.1, x.0), &(x.1, y.0)))
            {
                if *weight >= params.min_weight {
                    concrete_libfuncs_weights.insert(concrete_name.clone(), *weight);
                }
            }
        }

        let mut generic_libfuncs_weights = OrderedHashMap::default();
        if params.process_by_generic_libfunc {
            for (generic_name, weight) in
                generic_libfuncs.iter().sorted_by(|x, y| Ord::cmp(&(y.1, x.0), &(x.1, y.0)))
            {
                if *weight >= params.min_weight {
                    generic_libfuncs_weights.insert(generic_name.clone(), *weight);
                }
            }
        }

        ProcessedProfilingInfo {
            sierra_statements_weights: statements_weights,
            generic_libfuncs_weights: Some(generic_libfuncs_weights),
            concrete_libfuncs_weights: Some(concrete_libfuncs_weights),
            return_weight,
        }

        // result
    }
}
