//! Similar to examples tests - with code embedded in the test case, and only run tests is
//! performed.

use std::sync::Arc;

use compiler::db::RootDatabase;
use compiler::diagnostics::check_diagnostics;
use defs::db::DefsGroup;
use defs::ids::ModuleId;
use filesystem::db::{AsFilesGroupMut, FilesGroup, FilesGroupEx};
use filesystem::ids::{CrateId, CrateLongId, Directory};
use num_bigint::BigInt;
use plugins::derive::DerivePlugin;
use plugins::panicable::PanicablePlugin;
use sierra_generator::db::SierraGenGroup;
use sierra_generator::replace_ids::replace_sierra_ids_in_program;
use test_case::test_case;

use crate::common::run_sierra_program;

mod common;

/// Setups the cairo lowering for the content.
fn setup(content: &str) -> (RootDatabase, CrateId) {
    let mut db = RootDatabase::default();
    db.set_macro_plugins(vec![Arc::new(DerivePlugin {}), Arc::new(PanicablePlugin {})]);
    let crate_id = db.intern_crate(CrateLongId("test_crate/lib.cairo".into()));
    db.set_crate_root(crate_id, Some(Directory("test_crate".into())));

    let module_id = ModuleId::CrateRoot(crate_id);
    let file_id = db.module_main_file(module_id).unwrap();
    db.as_files_group_mut().override_file_content(file_id, Some(Arc::new(content.to_owned())));
    assert!(!check_diagnostics(&mut db));
    (db, crate_id)
}

#[test_case(
    "func foo(a: uint128, b: uint128) -> bool {
        a < b
    }",
    &[1, 1].map(BigInt::from),
    &[BigInt::from(0)];
    "1 less than 1"
)]
#[test_case(
    "func foo(a: uint128, b: uint128) -> bool {
        a < b
    }",
    &[1, 2].map(BigInt::from),
    &[BigInt::from(1)];
    "1 less than 2"
)]
#[test_case(
    "func foo(a: uint128, b: uint128) -> bool {
        a < b
    }",
    &[2, 1].map(BigInt::from),
    &[BigInt::from(0)];
    "2 less than 1"
)]
#[test_case(
    "
    func foo(idx: uint128) -> Option::<felt> {
        let arr = array_new::<felt>();
        array_append_aux(arr, 10);
        array_append_aux(arr, 11);
        array_append_aux(arr, 12);
        // Temporary hack to prevent padding.
        let x = array_at::<felt>(arr, idx)?;
        Option::<felt>::Some(x)
    }
    // TODO: remove this when array_append is supported.
    func array_append_aux(ref arr: Array::<felt>, val: felt) -> felt {
        array_append::<felt>(arr, val);
        val
    }
    ",
    &[0].map(BigInt::from),
    &[BigInt::from(0), BigInt::from(10)];
    "arr[0] == 10"
)]
#[test_case(
    "
    func foo(idx: uint128) -> Option::<felt> {
        let arr = array_new::<felt>();
        array_append_aux(arr, 10);
        array_append_aux(arr, 11);
        array_append_aux(arr, 12);
        // Temporary hack to prevent padding.
        let x = array_at::<felt>(arr, idx)?;
        Option::<felt>::Some(x)
    }
    // TODO: remove this when array_append is supported.
    func array_append_aux(ref arr: Array::<felt>, val: felt) -> felt {
        array_append::<felt>(arr, val);
        val
    }
    ",
    &[1].map(BigInt::from),
    &[BigInt::from(0), BigInt::from(11)];
    "arr[1] == 11"
)]
#[test_case(
    "
    func foo(idx: uint128) -> Option::<felt> {
        let arr = array_new::<felt>();
        array_append_aux(arr, 10);
        array_append_aux(arr, 11);
        array_append_aux(arr, 12);
        // Temporary hack to prevent padding.
        let x = array_at::<felt>(arr, idx)?;
        Option::<felt>::Some(x)
    }
    // TODO: remove this when array_append is supported.
    func array_append_aux(ref arr: Array::<felt>, val: felt) -> felt {
        array_append::<felt>(arr, val);
        val
    }
    ",
    &[2].map(BigInt::from),
    &[BigInt::from(0), BigInt::from(12)];
    "arr[2] == 12"
)]
#[test_case(
    "
    func foo(idx: uint128) -> Option::<felt> {
        let arr = array_new::<felt>();
        array_append_aux(arr, 10);
        array_append_aux(arr, 11);
        array_append_aux(arr, 12);
        // Temporary hack to prevent padding.
        let x = array_at::<felt>(arr, idx)?;
        Option::<felt>::Some(x)
    }
    // TODO: remove this when array_append is supported.
    func array_append_aux(ref arr: Array::<felt>, val: felt) -> felt {
        array_append::<felt>(arr, val);
        val
    }
    ",
    &[5].map(BigInt::from),
    &[BigInt::from(1), BigInt::from(0)];
    "arr[5] out of bounds"
)]
fn run_function_test(content: &str, params: &[BigInt], expected: &[BigInt]) {
    let (db, crate_id) = setup(content);
    let sierra_program = db.get_sierra_program(vec![crate_id]).unwrap();
    let sierra_program = replace_sierra_ids_in_program(&db, &sierra_program);
    assert_eq!(run_sierra_program(&sierra_program, params, expected.len(), false), expected);
}
