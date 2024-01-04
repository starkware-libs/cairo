use std::collections::HashMap;

use cairo_lang_sierra::program::{Program, StatementIdx};
use itertools::Itertools;
use smol_str::SmolStr;

/// Profiling into of a single run.
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ProfilingInfo {
    /// The number of steps in the trace that "belongs" to each sierra statement.
    pub concrete_sierra_statements_weights: HashMap<StatementIdx, usize>,
}

/// Flags controlling how the profiling info is printed by the `ProfilingInfoPrinter`.
pub struct ProfilingInfoPrinterFlags {
    print_zeros: bool,
    print_by_concrete_libfunc: bool,
    print_by_generic_libfunc: bool,
}
/// A printer for profiling info.
pub struct ProfilingInfoPrinter {
    sierra_program: Program,
    flags: ProfilingInfoPrinterFlags,
}
impl ProfilingInfoPrinter {
    pub fn new(sierra_program: Program) -> Self {
        Self {
            sierra_program,
            flags: ProfilingInfoPrinterFlags {
                print_zeros: false,
                print_by_concrete_libfunc: true,
                print_by_generic_libfunc: true,
            },
        }
    }

    /// Prints the profiling info according to the flags set in the printer.
    pub fn print(&self, profiling_info: &ProfilingInfo) {
        self.print_ex(profiling_info, &self.flags);
    }

    /// Prints the profiling info according to the given flags (can be used to override the printer
    /// flags).
    pub fn print_ex(&self, profiling_info: &ProfilingInfo, flags: &ProfilingInfoPrinterFlags) {
        let mut concrete_libfuncs = if flags.print_by_concrete_libfunc {
            self.sierra_program
                .libfunc_declarations
                .iter()
                .map(|concrete_libfunc| (concrete_libfunc.long_id.to_string().into(), 0))
                .collect::<HashMap<SmolStr, usize>>()
        } else {
            HashMap::new()
        };

        let mut generic_libfuncs = if flags.print_by_generic_libfunc {
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
            .concrete_sierra_statements_weights
            .iter()
            .sorted_by(|x, y| Ord::cmp(&x.1, &y.1))
        {
            let Some(gen_statement) = self.sierra_program.statements.get(statement_idx.0) else {
                println!("Failed fetching statement index {}", statement_idx.0);
                return;
            };
            if let Some(concrete_name) = gen_statement.concrete_name() {
                if flags.print_by_concrete_libfunc {
                    let concrete_name: SmolStr = concrete_name.into();
                    *(concrete_libfuncs.get_mut(&concrete_name).unwrap()) += weight;
                }

                if flags.print_by_generic_libfunc {
                    let generic_name: SmolStr = gen_statement.generic_name().unwrap().into();
                    *(generic_libfuncs.get_mut(&generic_name).unwrap()) += weight;
                }
            } else {
                return_weight += weight;
            }
            if *weight > 0 || flags.print_zeros {
                println!("  statement {}: {} ({})", *statement_idx, *weight, gen_statement);
            }
        }

        if flags.print_by_concrete_libfunc {
            println!("Weight by concrete libfunc:");
            for (concrete_name, weight) in
                concrete_libfuncs.iter().sorted_by(|x, y| Ord::cmp(&x.1, &y.1))
            {
                if *weight > 0 || flags.print_zeros {
                    println!("  libfunc {}: {}", concrete_name, *weight);
                }
            }
            println!("  return: {return_weight}");
        }

        if flags.print_by_generic_libfunc {
            println!("Weight by generic libfunc:");
            for (generic_name, weight) in
                generic_libfuncs.iter().sorted_by(|x, y| Ord::cmp(&x.1, &y.1))
            {
                if *weight > 0 || flags.print_zeros {
                    println!("  libfunc {}: {}", generic_name, *weight);
                }
            }
            println!("  return: {return_weight}");
        }
    }
}
