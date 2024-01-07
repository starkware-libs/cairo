use std::collections::HashMap;

use cairo_lang_sierra::program::{Program, StatementIdx};
use itertools::Itertools;
use smol_str::SmolStr;

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
    pub fn print(&self, profiling_info: &ProfilingInfo) {
        self.print_ex(profiling_info, &self.params);
    }

    /// Prints the profiling info according to the given params (can be used to override the printer
    /// params).
    pub fn print_ex(&self, profiling_info: &ProfilingInfo, params: &ProfilingInfoPrinterParams) {
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

        println!("Profiling info:");
        println!("Weight by sierra statement:");
        let mut return_weight = 0;
        for (statement_idx, weight) in profiling_info
            .sierra_statements_weights
            .iter()
            .sorted_by(|x, y| Ord::cmp(&(x.1, x.0.0), &(y.1, y.0.0)))
        {
            let Some(gen_statement) = self.sierra_program.statements.get(statement_idx.0) else {
                panic!("Failed fetching statement index {}", statement_idx.0);
            };
            if let Some(concrete_name) = gen_statement.concrete_name() {
                let concrete_name: SmolStr = concrete_name.into();
                if params.print_by_concrete_libfunc {
                    *(concrete_libfuncs.get_mut(&concrete_name).unwrap()) += weight;
                }

                if params.print_by_generic_libfunc {
                    // TODO(yg): there must be a better way - e.g. get the long ID with a db from
                    // the short ID, and from there the generic name.

                    let generic_name: SmolStr = concrete_name.split('<').next().unwrap().into();
                    *(generic_libfuncs.get_mut(&generic_name).unwrap()) += weight;
                }
            } else {
                return_weight += weight;
            }
            if *weight >= params.min_weight {
                println!("  statement {}: {} ({})", *statement_idx, *weight, gen_statement);
            }
        }

        if params.print_by_concrete_libfunc {
            println!("Weight by concrete libfunc:");
            for (concrete_name, weight) in
                concrete_libfuncs.iter().sorted_by(|x, y| Ord::cmp(&x.1, &y.1))
            {
                if *weight >= params.min_weight {
                    println!("  libfunc {}: {}", concrete_name, *weight);
                }
            }
            println!("  return: {return_weight}");
        }

        if params.print_by_generic_libfunc {
            println!("Weight by generic libfunc:");
            for (generic_name, weight) in
                generic_libfuncs.iter().sorted_by(|x, y| Ord::cmp(&x.1, &y.1))
            {
                if *weight >= params.min_weight {
                    println!("  libfunc {}: {}", generic_name, *weight);
                }
            }
            println!("  return: {return_weight}");
        }
    }
}
