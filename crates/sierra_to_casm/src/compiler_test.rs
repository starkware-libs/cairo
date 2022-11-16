use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;

use indoc::indoc;
use itertools::Itertools;
use pretty_assertions;
use sierra::extensions::lib_func::SierraApChange;
use sierra::ids::FunctionId;
use sierra::program::Program;
use sierra::ProgramParser;
use sierra_gas::calc_gas_info;
use sierra_gas::gas_info::GasInfo;
use test_case::test_case;

use crate::compiler::compile;
use crate::metadata::Metadata;

/// Builds the metadata for a Sierra program.
fn build_metadata(
    program: &Program,
    ap_change_data: &[(&str, usize)],
    calculate_gas_info: bool,
) -> Metadata {
    Metadata {
        function_ap_change: ap_change_data
            .iter()
            .map(|(func_name, change)| {
                (FunctionId::from_string(func_name), SierraApChange::Known(*change))
            })
            .collect(),
        gas_info: if calculate_gas_info {
            calc_gas_info(program).expect("Failed calculating gas variables.")
        } else {
            GasInfo { variable_values: HashMap::new(), function_costs: HashMap::new() }
        },
    }
}

/// Reads an example Sierra program that matches `name`.
fn read_sierra_example_file(name: &str) -> String {
    // Pop the "/sierra_to_casm" suffix.
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).parent().unwrap().to_owned();
    path.extend(["sierra", "examples", &format!("{name}.sierra")].into_iter());
    fs::read_to_string(path).unwrap()
}

/// Removes all comments and empty lines from the given program.
fn strip_comments_and_linebreaks(program: &str) -> String {
    return program
        .split('\n')
        .filter(|line| !(line.is_empty() || line.starts_with("//")))
        .join("\n")
        + "\n";
}

#[test_case(indoc! {"
                type felt = felt;
                type NonZeroFelt = NonZero<felt>;
                type BoxFelt = Box<felt>;

                libfunc finalize_locals = finalize_locals;
                libfunc felt_add = felt_add;
                libfunc felt_mul_2 = felt_mul<2>;
                libfunc felt_sub = felt_sub;
                libfunc felt_dup = dup<felt>;
                libfunc felt_jump_nz = felt_jump_nz;
                libfunc felt_into_box = into_box<felt>;
                libfunc felt_unbox = unbox<felt>;
                libfunc jump = jump;
                libfunc felt_unwrap_nz = unwrap_nz<felt>;
                libfunc store_temp_felt = store_temp<felt>;
                libfunc store_temp_box_felt = store_temp<BoxFelt>;
                libfunc rename_felt = rename<felt>;
                libfunc call_foo = function_call<user@foo>;

                libfunc call_box_and_back = function_call<user@box_and_back>;

                rename_felt([1]) -> ([1]);                      // #0
                felt_dup([2]) -> ([2], [5]);                    // #1
                felt_add([1], [2]) -> ([3]);                    // #2
                store_temp_felt([3]) -> ([4]);                  // #3
                store_temp_felt([5]) -> ([5]);                  // #4
                store_temp_felt([4]) -> ([4]);                  // #5
                call_foo([5], [4]) -> ([7], [8]);               // #6
                felt_dup([8]) -> ([4], [8]);                    // #7
                store_temp_felt([4]) -> ([4]);                  // #8
                return([7], [8], [4]);                          // #9

                finalize_locals() -> ();                        // #10
                felt_jump_nz([1]) { fallthrough() 16([1]) };    // #11
                felt_dup([2]) -> ([1], [2]);                    // #12
                store_temp_felt([1]) -> ([1]);                  // #13
                store_temp_felt([2]) -> ([2]);                  // #14
                return ([1], [2]);                              // #15

                jump() { 17() };                                // #16
                felt_unwrap_nz([1]) -> ([1]);                   // #17
                felt_dup([2]) -> ([2], [3]);                    // #18
                felt_sub([1], [3]) -> ([1]);                    // #19
                store_temp_felt([1]) -> ([1]);                  // #20
                felt_mul_2([1]) -> ([1]);                       // #21
                store_temp_felt([1]) -> ([1]);                  // #22
                store_temp_felt([2]) -> ([2]);                  // #23
                call_foo([1], [2]) -> ([1], [2]);               // #24
                return ([1], [2]);                              // #25

                felt_into_box([1]) -> ([2]);                    // #26
                store_temp_box_felt([2]) -> ([2]);              // #27
                felt_unbox([2]) -> ([3]);                       // #28
                store_temp_felt([3]) -> ([3]);                  // #29
                return ([3]);                                   // #30

                store_temp_felt([1]) -> ([1]);                  // #31
                call_box_and_back([1]) -> ([1]);                // #32
                return ([1]);                                   // #33

                test_program@0([1]: felt, [2]: felt) -> (felt, felt, felt);
                foo@10([1]: felt, [2]: felt) -> (felt, felt);
                box_and_back@26([1]: felt) -> (felt);
                box_and_back_wrapper@31([1]: felt) -> (felt);
            "},
            &[("box_and_back", 2), ("box_and_back_wrapper", 5)], false,
            indoc! {"
                // test_program:
                [ap + 0] = [fp + -4] + [fp + -3], ap++;
                [ap + 0] = [fp + -3], ap++;
                [ap + 0] = [ap + -2], ap++;
                // call foo
                call rel 4;
                [ap + 0] = [ap + -1], ap++;
                ret;

                // foo:
                ap += 0;
                jmp rel 5 if [fp + -4] != 0;
                [ap + 0] = [fp + -3], ap++;
                [ap + 0] = [fp + -3], ap++;
                ret;
                jmp rel 2;
                [fp + -4] = [ap + 0] + [fp + -3], ap++;
                [ap + 0] = [ap + -1] * 2, ap++;
                [ap + 0] = [fp + -3], ap++;
                call rel -13;
                ret;

                // box_and_back:
                %{ memory[ap + 0] = segments.add() %}
                [fp + -3] = [[ap + 0] + 0], ap++;
                [ap + 0] = [[ap + -1] + 0], ap++;
                ret;

                // box_and_back_wrapper:
                [ap + 0] = [fp + -3], ap++;
                call rel -4;
                ret;
            "};
            "good_flow")]
#[test_case(indoc! {"
                type felt = felt;
                type UninitializedFelt = Uninitialized<felt>;

                libfunc finalize_locals = finalize_locals;
                libfunc alloc_local_felt = alloc_local<felt>;
                libfunc store_local_felt = store_local<felt>;
                libfunc store_temp_felt = store_temp<felt>;

                store_temp_felt([1]) -> ([1]);
                alloc_local_felt() -> ([3]);
                alloc_local_felt() -> ([4]);
                store_local_felt([3], [1]) -> ([3]);
                finalize_locals() -> ();
                store_local_felt([4], [2]) -> ([4]);
                store_temp_felt([3]) -> ([3]);
                store_temp_felt([4]) -> ([4]);
                return ([3], [4]);

                test_program@0([1]: felt, [2]: felt) -> (felt, felt);
            "},
            &[("test_program", 5)], false,
            indoc! {"
                [ap + 0] = [fp + -4], ap++;
                [fp + 1] = [ap + -1];
                ap += 2;
                [fp + 2] = [fp + -3];
                [ap + 0] = [fp + 1], ap++;
                [ap + 0] = [fp + 2], ap++;
                ret;
            "};
            "alloc_local and store_local")]
#[test_case(indoc! {"
                type felt = felt;
                type NonZeroFelt = NonZero<felt>;

                libfunc store_temp_nz_felt = store_temp<NonZeroFelt>;
                libfunc nz_felt_drop = drop<NonZeroFelt>;
                libfunc felt_jump_nz = felt_jump_nz;
                libfunc burn_gas = burn_gas;


                felt_jump_nz([1]) { fallthrough() 3([1]) };
                burn_gas() -> ();
                return ();
                store_temp_nz_felt([1]) -> ([1]);
                nz_felt_drop([1]) -> ();
                return ();

                test_program@0([1]: felt) -> ();
            "},
            &[], true,
            indoc! {"
                jmp rel 3 if [fp + -3] != 0;
                ret;
                [ap + 0] = [fp + -3], ap++;
                ret;
            "};
            "burn gas")]
#[test_case(indoc!{"
                type RangeCheck = RangeCheck;
                type uint128 = uint128;

                libfunc revoke_ap_tracking = revoke_ap_tracking;
                libfunc uint128_lt = uint128_lt;
                libfunc store_uint128 = store_temp<uint128>;
                libfunc store_rc = store_temp<RangeCheck>;

                revoke_ap_tracking() -> ();
                uint128_lt([1], [2], [3]) {fallthrough([1]) 2([1]) };
                store_rc([1]) -> ([1]);
                return ([1]);

                test_program@0([1]: RangeCheck, [2]: uint128, [3]: uint128) -> (RangeCheck);
            "}, &[], false, indoc!{"
                %{ memory[ap + 0] = memory[fp + -4] < memory[fp + -3] %}
                jmp rel 6 if [ap + 0] != 0, ap++;
                // a >= b.
                [fp + -3] = [ap + 0] + [fp + -4], ap++;
                [ap + 0] = [[fp + -5] + 0];
                jmp rel 6;
                // a < b.
                [ap + 0] = [fp + -4] + 1, ap++;
                [fp + -3] = [ap + 0] + [ap + -1], ap++;
                [ap + 0] = [[fp + -5] + 0];
                // Store range_check and return.
                [ap + 0] = [fp + -5] + 1, ap++;
                ret;
            "}; "uint128_lt")]
#[test_case(indoc! {"
                type uint128 = uint128;
                type RangeCheck = RangeCheck;

                libfunc revoke_ap_tracking = revoke_ap_tracking;
                libfunc uint128_add = uint128_add;
                libfunc drop<uint128> = drop<uint128>;
                libfunc store_temp<RangeCheck> = store_temp<RangeCheck>;

                revoke_ap_tracking() -> ();
                uint128_add([1], [2], [3]) {fallthrough([1], [2]) 3([1]) };
                drop<uint128>([2]) -> ();
                store_temp<RangeCheck>([1]) -> ([1]);
                return ([1]);

                test_program@0([1]: RangeCheck, [2]: uint128, [3]: uint128) -> (RangeCheck);
            "},
            &[], false,
            indoc! {"
                [ap + 0] = [fp + -4] + [fp + -3], ap++;
                %{ memory[ap + 0] = memory[ap + -1] < 340282366920938463463374607431768211456 %}
                jmp rel 7 if [ap + 0] != 0, ap++;
                [ap + 0] = [ap + -2] + -340282366920938463463374607431768211456, ap++;
                [ap + -1] = [[fp + -5] + 0];
                jmp rel 3;
                [ap + -2] = [[fp + -5] + 0];
                [ap + 0] = [fp + -5] + 1, ap++;
                ret;
            "};
            "u128")]
#[test_case(read_sierra_example_file("fib_no_gas").as_str(),
            &[], false,
            indoc! {"
                jmp rel 4 if [fp + -3] != 0;
                [ap + 0] = [fp + -5], ap++;
                ret;
                [ap + 0] = [fp + -4], ap++;
                [ap + 0] = [fp + -5] + [fp + -4], ap++;
                [ap + 0] = [fp + -3] + -1, ap++;
                call rel -8;
                ret;
            "};
            "fib_no_gas")]
#[test_case(read_sierra_example_file("fib_jumps").as_str(),
            &[], true,
            indoc! {"
                jmp rel 8 if [fp + -3] != 0;
                [ap + 0] = [fp + -5], ap++;
                [ap + 0] = [fp + -4] + 10, ap++;
                [ap + 0] = 1, ap++;
                ret;

                // Statement #  8 - Calculates n - 1 and tests if n - 1 == 0.
                [fp + -3] = [ap + 0] + 1, ap++;
                jmp rel 8 if [ap + -1] != 0;
                [ap + 0] = [fp + -5], ap++;
                [ap + 0] = [fp + -4] + 8, ap++;
                [ap + 0] = 1, ap++;
                ret;

                // Statement # 18
                // Setting up the latest memory to be of the form [b=1, _, _, n=n-1, rc, gb, a=1].
                [ap + 0] = 1, ap++;
                [ap + 0] = [ap + -2], ap++;
                [ap + 0] = [ap + -1], ap++;
                [ap + 0] = [ap + -1], ap++;
                [ap + 0] = [fp + -5], ap++;
                [ap + 0] = [fp + -4], ap++;
                [ap + 0] = 1, ap++;
                // Statement # 27 - Getting gas for the main loop.
                %{ memory[ap + 0] = 5 < memory[ap + -2] %}
                jmp rel 9 if [ap + 0] != 0, ap++;
                [ap + 0] = [ap + -3] + -5, ap++;
                [ap + 0] = [ap + -1] * -1, ap++;
                [ap + -1] = [[ap + -6] + 0];
                jmp rel 19;

                // Statement # 28
                // The main loop - given [b, _, _, n, rc, gb, a, _, _] - adds [n-1, updated_rc, updated_gb, a+b]
                // Memory cells form is now [b'=a, _, _, n'=n-1, rc'=updated_rc, gb'=updated_gb, a'=a+b]
                [ap + 0] = [ap + -3] + -6, ap++;
                [ap + -1] = [[ap + -5] + 0];
                [ap + -6] = [ap + 0] + 1, ap++;
                [ap + 0] = [ap + -6] + 1, ap++;
                [ap + -6] = [ap + 0] + 6, ap++;
                [ap + 0] = [ap + -6] + [ap + -12], ap++;
                jmp rel -19 if [ap + -4] != 0;
                // Statement # 48 - n == 0, so we can return the latest a.
                [ap + 0] = [ap + -3], ap++;
                [ap + 0] = [ap + -3] + 1, ap++;
                [ap + 0] = [ap + -3], ap++;
                ret;
                [ap + 0] = [ap + -6] + 1, ap++;
                [ap + 0] = [ap + -6], ap++;
                [ap + 0] = -1, ap++;
                ret;
            "};
            "fib_jumps")]
#[test_case(read_sierra_example_file("fib_recursive").as_str(),
            &[], true,
            indoc! {"
                [ap + 0] = 1, ap++;
                jmp rel 7 if [fp + -3] != 0;
                [ap + 0] = [fp + -5], ap++;
                [ap + 0] = [fp + -4] + 3, ap++;
                [ap + 0] = [ap + -3], ap++;
                ret;

                // Statement #  8 - calculating n - 1, and testing if n - 1 == 0.
                [fp + -3] = [ap + 0] + 1, ap++;
                jmp rel 7 if [ap + -1] != 0;
                [ap + 0] = [fp + -5], ap++;
                // Statement # 12 - n == 1, so we return updated gb and 1.
                [ap + 0] = [fp + -4] + 1, ap++;
                [ap + 0] = [ap + -4], ap++;
                ret;

                // Statement # 17 - Get gas for the recursive calls.
                %{ memory[ap + 0] = 30 < memory[fp + -4] %}
                jmp rel 9 if [ap + 0] != 0, ap++;
                [ap + 0] = [fp + -4] + -30, ap++;
                [ap + 0] = [ap + -1] * -1, ap++;
                [ap + -1] = [[fp + -5] + 0];
                jmp rel 26;
                [ap + 0] = [fp + -4] + -31, ap++;
                [ap + -1] = [[fp + -5] + 0];

                // Statement # 21 - Performing both recursive calculations and returning their sum.
                ap += 2;
                [ap + 0] = [fp + -5] + 1, ap++;
                [fp + -4] = [ap + 0] + 31, ap++;
                [ap + -7] = [fp + 4] + 1;
                [ap + 0] = [ap + -7], ap++;
                call rel -39;
                [fp + 5] = [ap + -1];
                [ap + 0] = [ap + -3], ap++;
                [ap + 0] = [ap + -3], ap++;
                [ap + 0] = [fp + 4], ap++;
                call rel -45;
                [ap + 0] = [ap + -3], ap++;
                [ap + 0] = [ap + -3], ap++;
                [ap + 0] = [fp + 5] + [ap + -3], ap++;
                ret;

                // Statement # 41 - Ran out of gas - returning update gb and error value.
                [ap + 0] = [fp + -5] + 1, ap++;
                [ap + 0] = [fp + -4], ap++;
                [ap + 0] = -1, ap++;
                ret;
            "};
            "fib_recursive")]
#[test_case(indoc! {"
                type felt = felt;
                type DictFeltToFelt = DictFeltTo<felt>;
                type UninitializedFelt = Uninitialized<felt>;

                libfunc felt_const<10> = felt_const<10>;
                libfunc felt_const<11> = felt_const<11>;
                libfunc felt_const<12> = felt_const<12>;
                libfunc felt_const<13> = felt_const<13>;
                libfunc store_temp_felt = store_temp<felt>;
                libfunc dict_felt_to_new<felt> = dict_felt_to_new<felt>;
                libfunc dict_felt_to_write<felt> = dict_felt_to_write<felt>;
                libfunc dict_felt_to_read<felt> = dict_felt_to_read<felt>;
                libfunc store_temp_dict_felt_to_felt = store_temp<DictFeltToFelt>;

                felt_const<10>() -> ([0]);
                store_temp_felt([0]) -> ([0]);
                felt_const<11>() -> ([1]);
                store_temp_felt([1]) -> ([1]);
                felt_const<12>() -> ([2]);
                store_temp_felt([2]) -> ([2]);
                dict_felt_to_new<felt>([2]) -> ([3]);
                dict_felt_to_write<felt>([3], [0], [1]) -> ([4]);
                felt_const<10>() -> ([5]);
                store_temp_felt([5]) -> ([5]);
                felt_const<13>() -> ([6]);
                store_temp_felt([6]) -> ([6]);
                dict_felt_to_write<felt>([4], [5], [6]) -> ([7]);
                felt_const<10>() -> ([8]);
                store_temp_felt([8]) -> ([8]);
                dict_felt_to_read<felt>([7], [8]) -> ([9], [10]);
                store_temp_dict_felt_to_felt([9]) -> ([9]);
                store_temp_felt([10]) -> ([10]);
                return ([9], [10]);
                test_program@0() -> (DictFeltToFelt, felt);
            "},
            &[("test_program", 13)], false,
            indoc! {"
                [ap + 0] = 10, ap++;
                [ap + 0] = 11, ap++;
                [ap + 0] = 12, ap++;
                %{ 
                if '__dict_manager' not in globals():
                    from starkware.cairo.common.dict import DictManager
                    __dict_manager = DictManager()
                memory[ap + 0] = __dict_manager.new_default_dict(segments, memory[ap + -1])
                 %}
                ap += 1;
                %{ 
                dict_tracker = __dict_manager.get_tracker(memory[ap + -1] + 0)
                dict_tracker.current_ptr += 3
                memory[ap + 0] = dict_tracker.data[memory[ap + -4]]
                dict_tracker.data[memory[ap + -4]] = memory[ap + -3]
                 %}
                ap += 1;
                [ap + -5] = [[ap + -2] + 0];
                [ap + -1] = [[ap + -2] + 1];
                [ap + -4] = [[ap + -2] + 2];
                [ap + 0] = 10, ap++;
                [ap + 0] = 13, ap++;
                %{ 
                dict_tracker = __dict_manager.get_tracker(memory[ap + -4] + 3)
                dict_tracker.current_ptr += 3
                memory[ap + 0] = dict_tracker.data[memory[ap + -2]]
                dict_tracker.data[memory[ap + -2]] = memory[ap + -1]
                 %}
                ap += 1;
                [ap + -3] = [[ap + -5] + 3];
                [ap + -1] = [[ap + -5] + 4];
                [ap + -2] = [[ap + -5] + 5];
                [ap + 0] = 10, ap++;
                %{ 
                dict_tracker = __dict_manager.get_tracker(memory[ap + -6] + 6)
                dict_tracker.current_ptr += 3
                memory[ap + 0] = dict_tracker.data[memory[ap + -1]]
                 %}
                ap += 1;
                [ap + -2] = [[ap + -7] + 6];
                [ap + -1] = [[ap + -7] + 7];
                [ap + -1] = [[ap + -7] + 8];
                [ap + 0] = [ap + -7], ap++;
                [ap + 0] = [ap + -8] + 9, ap++;
                [ap + 0] = [ap + -3], ap++;
                ret;
            "};
            "dict test")]

fn sierra_to_casm(
    sierra_code: &str,
    ap_change_data: &[(&str, usize)],
    check_gas_usage: bool,
    expected_casm: &str,
) {
    let program = ProgramParser::new().parse(sierra_code).unwrap();
    pretty_assertions::assert_eq!(
        compile(
            &program,
            &build_metadata(&program, ap_change_data, check_gas_usage),
            check_gas_usage
        )
        .expect("Compilation failed.")
        .to_string(),
        strip_comments_and_linebreaks(expected_casm)
    );
}

// TODO(ilya, 10/10/2022): Improve error messages.
#[test_case(indoc! {"
                return([2]);

                test_program@0() -> (felt);
            "}, &[],
            "#0: [2] is undefined.";
            "Missing reference")]
#[test_case(indoc! {"
                type felt = felt;
                libfunc felt_dup = dup<felt>;

                felt_dup([1]) -> ([1], [2]);
                felt_dup([2]) -> ([1], [2]);
                return();

                test_program@0([1]: felt) -> ();
            "}, &[],
            "#1->#2: [1] was overridden.";
            "Reference override")]
#[test_case(indoc! {"
                type felt = felt;

                return([2]);

                test_program@0([2]: felt) -> (felt);
            "}, &[("test_program", 0)],
            "#0: Return arguments are not on the stack.";
            "Invalid return reference")]
#[test_case(indoc! {"
                type felt = felt;

                store_temp_felt([1]) -> ([1]);

                test_program@0([1]: felt) -> ();
            "}, &[],
            "Error from program registry";
            "undeclared libfunc")]
#[test_case(indoc! {"
                type felt = felt;

                libfunc store_temp_felt = store_temp<felt>;
                libfunc store_temp_felt = store_temp<felt>;
            "}, &[],
            "Error from program registry";
            "Concrete libfunc Id used twice")]
#[test_case(indoc! {"
                type uint128 = uint128;
                type RangeCheck = RangeCheck;

                libfunc uint128_wrapping_add = uint128_wrapping_add;

                uint128_wrapping_add([1], [2], [3]) -> ([1], [2]);
                test_program@0([1]: RangeCheck, [2]: uint128, [3]: uint128) -> ();
            "}, &[],
            "#0: The requested functionality is not implemented yet.";
            "Not implemented")]
#[test_case(indoc! {"
                type felt = felt;
                libfunc felt_add = felt_add;

                felt_add([1], [2]) -> ([4]);
                felt_add([3], [4]) -> ([5]);

                test_program@0([1]: felt, [2]: felt, [3]: felt) -> ();
            "}, &[],
            "#1: One of the arguments does not satisfy the requirements of the libfunc.";
            "Invalid reference expression for felt_add")]
#[test_case(indoc! {"
                type felt = felt;
                type uint128 = uint128;
                libfunc felt_add = felt_add;
                felt_add([1], [2]) -> ([3]);
                return([3]);

                test_program@0([1]: uint128, [2]: uint128) -> (felt);
            "}, &[],
            "One of the arguments does not match the expected type of the libfunc or return \
 statement.";
            "Types mismatch")]
#[test_case(indoc! {"
                test_program@25() -> ();
            "}, &[], "InvalidStatementIdx";
            "Invalid entry point")]
#[test_case(indoc! {"
                return();

                foo@0([1]: felt, [1]: felt) -> ();
            "},  &[], "Invalid function declaration.";
            "Bad Declaration")]
#[test_case(indoc! {"
            return();
            "}, &[], "MissingAnnotationsForStatement";
            "Missing references for statement")]
#[test_case(indoc! {"
                type NonZeroFelt = NonZero<felt>;
                type felt = felt;
            "}, &[], "Error from program registry";
            "type ordering bad for building size map")]
#[test_case(indoc! {"
                type felt = felt;
                libfunc felt_add = felt_add;
                felt_add([1], [2], [3]) -> ([4]);
                test_program@0([1]: felt, [2]: felt, [3]: felt) -> ();
            "}, &[], "#0: Invocation mismatched to libfunc";
            "input count mismatch")]
#[test_case(indoc! {"
                type felt = felt;
                libfunc felt_add = felt_add;
                felt_add([1], [2]) -> ([3], [4]);
                test_program@0([1]: felt, [2]: felt) -> ();
            "}, &[], "#0: Invocation mismatched to libfunc";
            "output type mismatch")]
#[test_case(indoc! {"
                type felt = felt;
                libfunc felt_add = felt_add;
                felt_add([1], [2]) { 0([3]) 1([3]) };
                test_program@0([1]: felt, [2]: felt) -> ();
            "}, &[], "#0: Invocation mismatched to libfunc";
            "branch count mismatch")]
#[test_case(indoc! {"
                type felt = felt;
                libfunc felt_add = felt_add;
                felt_add([1], [2]) { 0([3]) };
                test_program@0([1]: felt, [2]: felt) -> ();
            "}, &[], "#0: Invocation mismatched to libfunc";
            "fallthrough mismatch")]
#[test_case(indoc! {"
                type felt = felt;
                libfunc felt_dup = dup<felt>;

                felt_dup([1]) -> ([1], [2]);
                return ([1]);
                test_program@0([1]: felt) -> ();
            "}, &[], "[2] is dangling at #1.";
            "Dangling references")]
#[test_case(indoc! {"
                type felt = felt;

                return();

                foo@0([1]: felt) -> ();
                bar@0([2]: felt) -> ();
            "}, &[], "#0: Inconsistent references annotations.";
            "Failed building type information")]
#[test_case(indoc! {"
                type felt = felt;
                libfunc felt_dup = dup<felt>;

                felt_dup([1]) -> ([1], [2]);
                return ([1]);
                test_program@0([1]: felt) -> ();
                foo@1([1]: felt) -> (felt);
            "}, &[], "#1: Inconsistent references annotations.";
            "Inconsistent return annotations.")]
#[test_case(indoc! {"
                type felt = felt;
                type NonZeroFelt = NonZero<felt>;

                libfunc felt_dup = dup<felt>;
                libfunc felt_drop = drop<felt>;
                libfunc felt_jump_nz = felt_jump_nz;
                libfunc store_temp_felt = store_temp<felt>;
                libfunc store_temp_nz_felt = store_temp<NonZeroFelt>;

                felt_jump_nz([1]) { fallthrough() 3([1]) };
                store_temp_felt([2]) -> ([2]);
                return ([2]);
                felt_drop([2]) -> ();
                store_temp_nz_felt([1]) -> ([1]);
                return ([1]);

                test_program@0([1]: felt, [2]: felt) -> (felt);
            "}, &[("test_program", 1)], "One of the arguments does not match the expected type \
of the libfunc or return statement.";
            "Invalid return type")]
#[test_case(indoc! {"
                type felt = felt;

                libfunc felt_dup = dup<felt>;
                libfunc felt_drop = drop<felt>;
                libfunc store_temp_felt = store_temp<felt>;
                libfunc call_foo = function_call<user@foo>;

                store_temp_felt([1]) -> ([1]);
                felt_dup([1]) -> ([1], [2]);
                call_foo([2]) -> ();
                store_temp_felt([1]) -> ([1]);
                felt_drop([1]) -> ();
                return();

                foo@0([1]: felt) -> ();
            "}, &[], "#2->#3: Got 'Unknown ap change' error while moving [1].";
            "Ap change error")]
#[test_case(indoc! {"
                type felt = felt;
                type NonZeroFelt = NonZero<felt>;

                libfunc revoke_ap_tracking = revoke_ap_tracking;

                libfunc felt_drop = drop<felt>;
                libfunc felt_jump_nz = felt_jump_nz;
                libfunc felt_unwrap_nz = unwrap_nz<felt>;
                libfunc jump = jump;

                felt_jump_nz([1]) { fallthrough() 3([1]) };
                revoke_ap_tracking() -> ();
                jump() { 5() };
                felt_unwrap_nz([1]) -> ([1]);
                felt_drop([1]) -> ();
                return ();

                foo@0([1]: felt) -> ();
            "}, &[], "#5: Inconsistent ap tracking.";
            "Inconsistent ap tracking.")]
#[test_case(indoc! {"
                libfunc finalize_locals = finalize_locals;

                finalize_locals () -> ();
                finalize_locals () -> ();
                return ();

                test_program@0() -> ();
            "}, &[], "#1: finalize_locals is not allowed at this point.";
            "Invalid finalize_locals 1")]
#[test_case(indoc! {"
                type felt = felt;

                libfunc finalize_locals = finalize_locals;
                libfunc store_temp_felt = store_temp<felt>;
                libfunc call_foo = function_call<user@foo>;

                store_temp_felt([1]) -> ([1]);
                call_foo([1]) -> ();
                finalize_locals() -> ();
                return ();

                foo@0([1]: felt) -> ();
            "}, &[], "#2: finalize_locals is not allowed at this point.";
            "Invalid finalize_locals 2")]
#[test_case(indoc! {"
                type felt = felt;
                type UninitializedFelt = Uninitialized<felt>;

                libfunc alloc_local_felt = alloc_local<felt>;
                libfunc store_temp_felt = store_temp<felt>;

                alloc_local_felt() -> ([2]);
                store_temp_felt([1]) -> ([1]);
                alloc_local_felt() -> ([3]);
                return ();

                foo@0([1]: felt) -> ();
            "}, &[], "#2: alloc_local is not allowed at this point.";
            "Invalid alloc_local ")]
#[test_case(indoc! {"
                type felt = felt;
                type UninitializedFelt = Uninitialized<felt>;

                libfunc alloc_local_felt = alloc_local<felt>;
                libfunc store_local_felt = store_local<felt>;
                libfunc felt_drop = drop<felt>;

                alloc_local_felt() -> ([2]);
                store_local_felt([2], [1]) -> ([2]);
                felt_drop([2]) -> ();
                return ();

                foo@0([1]: felt) -> ();
            "}, &[("foo", 0)], "#3: locals were allocated but finalize_locals was not called.";
            "missing finalize_locals ")]
#[test_case(indoc! {"
                type felt = felt;
                type UninitializedFelt = Uninitialized<felt>;

                libfunc alloc_local_felt = alloc_local<felt>;
                libfunc store_temp_felt = store_temp<UninitializedFelt>;

                alloc_local_felt() -> ([1]);
                store_temp_felt([1]) -> ([1]);
                return ();

                foo@0() -> ();
            "}, &[], "#1: The functionality is supported only for sized types.";
            "store_temp<Uninitialized<felt>()")]
#[test_case(indoc! {"
                return ();

                foo@0() -> ();
            "}, &[("foo", 5)], "#0: Invalid Ap change annotation. \
expected: ApChange::Known(5) got: ApChange::Known(0).";
            "bad Ap change")]
fn compiler_errors(sierra_code: &str, ap_change_data: &[(&str, usize)], expected_result: &str) {
    let program = ProgramParser::new().parse(sierra_code).unwrap();
    pretty_assertions::assert_eq!(
        compile(&program, &build_metadata(&program, ap_change_data, false), false)
            .expect_err("Compilation is expected to fail.")
            .to_string(),
        expected_result
    );
}
