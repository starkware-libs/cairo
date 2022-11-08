use cairo_rs::types::relocatable::{MaybeRelocatable, Relocatable};
use cairo_rs::vm::vm_core::VirtualMachine;
use num_bigint::BigInt;
use test_case::test_case;

use crate::casm;
use crate::inline::CasmContext;
use crate::run::{get_prime, mod_prime, run};

// Returns n values from the execution segment of vm. Panics if memory isn't set.
fn get_vm_execution(vm: VirtualMachine, n: usize) -> Vec<BigInt> {
    vm.get_range(&MaybeRelocatable::from(Relocatable::from((1, 0))), n)
        .expect("Unable to get range from VM.")
        .into_iter()
        .map(|x| {
            if let MaybeRelocatable::Int(value) = x.unwrap().as_ref() {
                value.clone()
            } else {
                panic!("Memory not set.");
            }
        })
        .collect()
}

fn as_felts(nums: &[i128]) -> Vec<BigInt> {
    nums.iter().map(|num| mod_prime(BigInt::from(*num))).collect()
}

#[test_case(
    casm! {
        [ap] = (-5), ap++;
        [ap] = 7, ap++;
        jmp rel -2;
    },
    6,
    as_felts(&[0, -5, 7, 7, 7])
)]
#[test_case(
    casm! {
        ap += 1;
        [ap] = 123, ap++;
        [ap] = 456, ap++;
        [fp] = [ap - 2] + [ap - 1];
    },
    4,
    as_felts(&[0, 579, 123, 456])
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
    as_felts(&[0, 0, 1, 2])
)]
fn test_runner(program: CasmContext, n_steps: usize, expected: Vec<BigInt>) {
    let vm = run(program.instructions, get_prime(), n_steps);
    assert_eq!(get_vm_execution(vm, expected.len()), expected);
}
