use std::fs;
use std::path::PathBuf;

use itertools::Itertools;

<<<<<<< HEAD
use crate::metadata::{calc_metadata, Metadata, MetadataComputationConfig};

/// Builds the metadata for a Sierra program.
pub fn build_metadata(program: &Program, calculate_gas_info: bool) -> Metadata {
    if calculate_gas_info {
        calc_metadata(program, MetadataComputationConfig::default(), true)
            .expect("Failed calculating gas or ap change.")
    } else {
        Metadata {
            ap_change_info: calc_ap_changes(program, |_, _| 0).unwrap_or(ApChangeInfo {
                function_ap_change: Default::default(),
                variable_values: Default::default(),
            }),
            gas_info: GasInfo {
                variable_values: Default::default(),
                function_costs: Default::default(),
            },
        }
    }
}

||||||| 4b74855b0
use crate::metadata::{calc_metadata, Metadata, MetadataComputationConfig};

/// Builds the metadata for a Sierra program.
///
/// `no_eq_solver` uses a linear-time algorithm for calculating the gas, instead of solving
/// equations.
pub fn build_metadata(program: &Program, calculate_gas_info: bool, no_eq_solver: bool) -> Metadata {
    if calculate_gas_info {
        calc_metadata(program, MetadataComputationConfig::default(), no_eq_solver)
            .expect("Failed calculating gas or ap change.")
    } else {
        Metadata {
            ap_change_info: calc_ap_changes(program, |_, _| 0).unwrap_or(ApChangeInfo {
                function_ap_change: Default::default(),
                variable_values: Default::default(),
            }),
            gas_info: GasInfo {
                variable_values: Default::default(),
                function_costs: Default::default(),
            },
        }
    }
}

=======
>>>>>>> origin/main
/// Reads an example Sierra program that matches `name`.
pub fn read_sierra_example_file(name: &str) -> String {
    // Pop the "/sierra_to_casm" suffix.
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).parent().unwrap().to_owned();
    path.extend(["cairo-lang-sierra", "examples", &format!("{name}.sierra")]);
    fs::read_to_string(path).unwrap()
}

/// Removes all comments and empty lines from the given program.
pub fn strip_comments_and_linebreaks(program: &str) -> String {
    return program
        .split('\n')
        .filter(|line| !(line.is_empty() || line.starts_with("//")))
        .join("\n")
        + "\n";
}
