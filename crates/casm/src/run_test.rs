#![allow(clippy::result_large_err)]
use cairo_rs::types::relocatable::{MaybeRelocatable, Relocatable};
use cairo_rs::vm::errors::vm_errors::VirtualMachineError;
use cairo_rs::vm::vm_core::VirtualMachine;
use num_bigint::BigInt;
use test_case::test_case;

use crate::casm;
use crate::inline::CasmContext;
use crate::run::{get_prime, mod_prime, run};

// Returns n values from the execution segment of vm. Panics if memory isn't set.
fn get_vm_execution(vm: VirtualMachine, n: usize) -> Result<Vec<BigInt>, VirtualMachineError> {
    Ok(vm
        .get_range(&MaybeRelocatable::from(Relocatable::from((1, 0))), n)?
        .into_iter()
        .map(|x| {
            if let MaybeRelocatable::Int(value) = x.unwrap().as_ref() {
                value.clone()
            } else {
                panic!("Memory not set.");
            }
        })
        .collect())
}

fn to_felts(nums: Vec<i128>) -> Vec<BigInt> {
    nums.into_iter().map(BigInt::from).map(mod_prime).collect()
}

#[test_case(
    casm! {
        [ap] = (-5), ap++;
        [ap] = 7, ap++;
        jmp rel -2;
    },
    6,
    to_felts(vec![0, -5, 7, 7, 7])
)]
#[test_case(
    casm! {
        ap += 1;
        [ap] = 123, ap++;
        [ap] = 456, ap++;
        [fp] = [ap - 2] + [ap - 1];
    },
    4,
    to_felts(vec![0, 579, 123, 456])
)]
#[test_case(
    casm! {
        [ap] = 0, ap++;
        jmp rel 2 if [ap] != 0;
        [ap] = 1, ap++;
        jmp rel 2 if [ap - 1] != 0;
        [ap] = 2, ap++;
    },
    5,
    to_felts(vec![0, 0, 1, 2])
)]
fn test_runner(
    program: CasmContext,
    n_steps: usize,
    expected: Vec<BigInt>,
) -> Result<(), VirtualMachineError> {
    let vm = run(program.instructions, get_prime(), n_steps)?;
    assert_eq!(get_vm_execution(vm, expected.len())?, expected);
    Ok(())
}
