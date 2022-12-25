use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;

use itertools::Itertools;
use sierra::program::Program;
use sierra_ap_change::ap_change_info::ApChangeInfo;
use sierra_ap_change::calc_ap_changes;
use sierra_gas::calc_gas_info;
use sierra_gas::gas_info::GasInfo;

use crate::metadata::Metadata;

/// Builds the metadata for a Sierra program.
pub fn build_metadata(program: &Program, calculate_gas_info: bool) -> Metadata {
    Metadata {
        ap_change_info: calc_ap_changes(program).unwrap_or(ApChangeInfo {
            function_ap_change: HashMap::default(),
            variable_values: HashMap::default(),
        }),
        gas_info: if calculate_gas_info {
            calc_gas_info(program).expect("Failed calculating gas variables.")
        } else {
            GasInfo { variable_values: HashMap::new(), function_costs: HashMap::new() }
        },
    }
}

/// Reads an example Sierra program that matches `name`.
pub fn read_sierra_example_file(name: &str) -> String {
    // Pop the "/sierra_to_casm" suffix.
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).parent().unwrap().to_owned();
    path.extend(["sierra", "examples", &format!("{name}.sierra")].into_iter());
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
