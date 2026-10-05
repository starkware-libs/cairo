#![expect(clippy::disallowed_types)]

use std::collections::HashMap;

use cairo_lang_sierra::ProgramParser;
use cairo_lang_sierra::program::StatementIdx;
use cairo_lang_sierra::simulation::value::CoreValue;
use cairo_lang_sierra::simulation::{LibfuncSimulationError, SimulationError, run};

fn assert_wrong_arg_type(source: &str, inputs: Vec<CoreValue>, statement_idx: usize) {
    let program = ProgramParser::new().parse(source).expect("valid Sierra syntax");
    let function_id = program.funcs[0].id.clone();

    assert_eq!(
        run(&program, &HashMap::new(), &function_id, inputs),
        Err(SimulationError::LibfuncSimulationError(
            LibfuncSimulationError::WrongArgType,
            StatementIdx(statement_idx),
        )),
    );
}

#[test]
fn felt252_div_rejects_zero_divisor() {
    assert_wrong_arg_type(
        r#"
type felt252 = felt252;
type NonZeroFelt = NonZero<felt252>;
libfunc one = felt252_const<1>;
libfunc zero = felt252_const<0>;
libfunc div = felt252_div;
libfunc store_temp_felt = store_temp<felt252>;

one() -> ([0]);
zero() -> ([1]);
div([0], [1]) -> ([2]);
store_temp_felt([2]) -> ([3]);
return([3]);

f@0() -> (felt252);
"#,
        vec![],
        2,
    );
}

#[test]
fn u128_divmod_rejects_zero_divisor() {
    assert_wrong_arg_type(
        r#"
type u128 = u128;
type NonZeroU128 = NonZero<u128>;
type RangeCheck = RangeCheck;
libfunc seven = u128_const<7>;
libfunc zero = u128_const<0>;
libfunc divmod = u128_safe_divmod;

seven() -> (a);
zero() -> (b);
divmod(rc, a, b) -> (rc2, q, r);
return(rc2, q, r);

f@0(rc: RangeCheck) -> (RangeCheck, u128, u128);
"#,
        vec![CoreValue::RangeCheck],
        2,
    );
}

#[test]
fn bool_not_rejects_non_bool_enum_index() {
    assert_wrong_arg_type(
        r#"
type felt252 = felt252;
type S = Struct<ut@Tuple>;
type Bool = Enum<ut@core::bool, S, S>;
type Enum3 = Enum<ut@X, felt252, felt252, felt252>;
libfunc init2 = enum_init<Enum3, 2>;
libfunc boolnot = bool_not_impl;

init2(x) -> (e);
boolnot(e) -> (r);
return(r);

f@0(x: felt252) -> (Enum3);
"#,
        vec![CoreValue::Felt252(1u32.into())],
        1,
    );
}

#[test]
fn bool_or_rejects_non_bool_enum_index() {
    assert_wrong_arg_type(
        r#"
type S = Struct<ut@Tuple>;
type Bool = Enum<ut@core::bool, S, S>;
libfunc boolor = bool_or_impl;

boolor(a, b) -> (r);
return(r);

f@0(a: Bool, b: Bool) -> (Bool);
"#,
        vec![
            CoreValue::Enum { value: Box::new(CoreValue::Struct(vec![])), index: usize::MAX },
            CoreValue::Enum { value: Box::new(CoreValue::Struct(vec![])), index: 1 },
        ],
        0,
    );
}

#[test]
fn print_rejects_non_felt_array_elements() {
    assert_wrong_arg_type(
        r#"
type felt252 = felt252;
type Enum3 = Enum<ut@X, felt252, felt252, felt252>;
type ArrayEnum3 = Array<Enum3>;
type ArrayFelt = Array<felt252>;
libfunc new_arr = array_new<Enum3>;
libfunc init2 = enum_init<Enum3, 2>;
libfunc append = array_append<Enum3>;
libfunc dbg = print;

new_arr() -> (arr);
init2(x) -> (e);
append(arr, e) -> (arr2);
dbg(arr2) -> ();
return();

f@0(x: felt252) -> ();
"#,
        vec![CoreValue::Felt252(1u32.into())],
        3,
    );
}
