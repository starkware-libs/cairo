//! Similar to examples tests - with code embedded in the test case, and only run tests is performed.

use std::collections::HashMap;
use std::sync::Arc;

use cairo_rs::vm::errors::vm_errors::VirtualMachineError;
use casm::instructions::Instruction;
use casm::run::run_function_return_values;
use casm::{casm, casm_extend};
use compiler::db::RootDatabase;
use compiler::diagnostics::check_diagnostics;
use defs::db::DefsGroup;
use defs::ids::ModuleId;
use filesystem::db::{AsFilesGroupMut, FilesGroup, FilesGroupEx};
use filesystem::ids::{CrateLongId, Directory};
use num_bigint::BigInt;
use sierra_gas::gas_info::GasInfo;
use sierra_generator::db::SierraGenGroup;
use sierra_generator::replace_ids::replace_sierra_ids_in_program;
use sierra_to_casm::metadata::Metadata;
use test_case::test_case;
use crate::common::run_sierra_program;

mod common;

/// Setups the cairo lowering for the content.
fn setup(content: &str) -> RootDatabase {
    let mut db = RootDatabase::default();
    let crate_id = db.intern_crate(CrateLongId("test_crate/lib.cairo".into()));
    db.set_crate_root(crate_id, Some(Directory("test_crate".into())));

    let module_id = ModuleId::CrateRoot(crate_id);
    let file_id = db.module_file(module_id).unwrap();
    db.as_files_group_mut().override_file_content(file_id, Some(Arc::new(content.to_owned())));
    assert!(!check_diagnostics(&mut db));
    db
}

#[test_case(
    "func foo(a: uint128, b: uint128) -> bool {
        uint128_le(a, b)
    }",
    &[1, 1].map(BigInt::from),
    &[Some(BigInt::from(0))];
    "1 less than 1"
)]
#[test_case(
    "func foo(a: uint128, b: uint128) -> bool {
        a < b
    }",
    &[1, 2].map(BigInt::from),
    &[Some(BigInt::from(1))];
    "1 less than 2"
)]
#[test_case(
    "func foo(a: uint128, b: uint128) -> bool {
        a < b
    }",
    &[2, 1].map(BigInt::from),
    &[Some(BigInt::from(0))];
    "2 less than 1"
)]
fn run_function_test(
    content: &str,
    params: &[BigInt],
    expected: &[Option<BigInt>],
) {
    let db = setup(content);
    let sierra_program = db.get_sierra_program().unwrap();
    replace_sierra_ids_in_program(&db, &sierra_program);
    assert_eq!(run_sierra_program(&sierra_program, params, expected.len(), false), expected);
}

