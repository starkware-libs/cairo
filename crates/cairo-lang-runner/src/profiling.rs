use std::collections::HashMap;

use cairo_lang_sierra::program::{GenStatement, Program, StatementIdx};
use itertools::Itertools;
use smol_str::SmolStr;

#[cfg(test)]
#[path = "profiling_test.rs"]
mod test;

/// Profiling into of a single run.
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ProfilingInfo {
    /// The number of steps in the trace that originated from each sierra statement.
    pub sierra_statements_weights: HashMap<StatementIdx, usize>,
}

/// Parameters controlling how the profiling info is printed by the `ProfilingInfoPrinter`.
pub struct ProfilingInfoPrinterParams {
    /// The minimal weight to print. Used for all printed stats. That is - the sum of the weights
    /// per statement may smaller than the sum of the weights per concrete libfunc, that may be
    /// smaller than the sum of the weights ber generic libfunc.
    pub min_weight: usize,
    pub print_by_concrete_libfunc: bool,
    pub print_by_generic_libfunc: bool,
}
impl Default for ProfilingInfoPrinterParams {
    fn default() -> Self {
        Self { min_weight: 1, print_by_concrete_libfunc: true, print_by_generic_libfunc: true }
    }
}
/// A printer for profiling info.
pub struct ProfilingInfoPrinter {
    sierra_program: Program,
    params: ProfilingInfoPrinterParams,
}
impl ProfilingInfoPrinter {
    pub fn new(sierra_program: Program) -> Self {
        Self { sierra_program, params: ProfilingInfoPrinterParams::default() }
    }

    /// Prints the profiling info according to the params set in the printer.
    pub fn print(&self, profiling_info: &ProfilingInfo) -> String {
        self.print_ex(profiling_info, &self.params)
    }

    /// Prints the profiling info according to the given params (can be used to override the printer
    /// params).
    pub fn print_ex(
        &self,
        profiling_info: &ProfilingInfo,
        params: &ProfilingInfoPrinterParams,
    ) -> String {
        let mut concrete_libfuncs = if params.print_by_concrete_libfunc {
            self.sierra_program
                .libfunc_declarations
                .iter()
                .map(|concrete_libfunc| (concrete_libfunc.long_id.to_string().into(), 0))
                .collect::<HashMap<SmolStr, usize>>()
        } else {
            HashMap::new()
        };

        let mut generic_libfuncs = if params.print_by_generic_libfunc {
            self.sierra_program
                .libfunc_declarations
                .iter()
                .map(|concrete_libfunc| (concrete_libfunc.long_id.generic_id.0.clone(), 0))
                .collect::<HashMap<SmolStr, usize>>()
        } else {
            HashMap::new()
        };

        let mut result = "Weight by sierra statement:\n".to_string();

        let mut return_weight = 0;
        for (statement_idx, weight) in profiling_info
            .sierra_statements_weights
            .iter()
            .sorted_by(|x, y| Ord::cmp(&(y.1, y.0.0), &(x.1, x.0.0)))
        {
            let Some(gen_statement) = self.sierra_program.statements.get(statement_idx.0) else {
                panic!("Failed fetching statement index {}", statement_idx.0);
            };
            match gen_statement {
                GenStatement::Invocation(invocation) => {
                    let concrete_name: SmolStr = format!("{}", invocation.libfunc_id).into();
                    if params.print_by_concrete_libfunc {
                        *(concrete_libfuncs.get_mut(&concrete_name).unwrap()) += weight;
                    }

                    if params.print_by_generic_libfunc {
                        // TODO(yg): there must be a better way - e.g. get the long ID with a db
                        // from the short ID, and from there get the generic name.
                        let generic_name: SmolStr = concrete_name.split('<').next().unwrap().into();
                        *(generic_libfuncs.get_mut(&generic_name).unwrap()) += weight;
                    }
                }
                GenStatement::Return(_) => {
                    return_weight += weight;
                }
            }
            if *weight >= params.min_weight {
                result.push_str(&format!(
                    "  statement {}: {} ({})\n",
                    *statement_idx, *weight, gen_statement
                ));
            }
        }

        if params.print_by_concrete_libfunc {
            result.push_str("Weight by concrete libfunc:\n");
            for (concrete_name, weight) in
                concrete_libfuncs.iter().sorted_by(|x, y| Ord::cmp(&(y.1, y.0), &(x.1, x.0)))
            {
                if *weight >= params.min_weight {
                    result.push_str(&format!("  libfunc {}: {}\n", concrete_name, *weight));
                }
            }
            result.push_str(&format!("  return: {return_weight}\n"));
        }

        if params.print_by_generic_libfunc {
            result.push_str("Weight by generic libfunc:\n");
            for (generic_name, weight) in
                generic_libfuncs.iter().sorted_by(|x, y| Ord::cmp(&(y.1, y.0), &(x.1, x.0)))
            {
                if *weight >= params.min_weight {
                    result.push_str(&format!("  libfunc {}: {}\n", generic_name, *weight));
                }
            }
            result.push_str(&format!("  return: {return_weight}\n"));
        }

        result
    }
}
