use cairo_lang_sierra::ProgramParser;
use indoc::indoc;
use pretty_assertions;
use test_case::test_case;

use crate::compiler::compile;
use crate::test_utils::{build_metadata, read_sierra_example_file, strip_comments_and_linebreaks};

#[test_case(indoc! {"
                type felt = felt;
                type NonZeroFelt = NonZero<felt>;
                type BoxFelt = Box<felt>;

                libfunc branch_align = branch_align;
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
                felt_jump_nz([1]) { fallthrough() 17([1]) };    // #11
                branch_align() -> ();                           // #12
                felt_dup([2]) -> ([1], [2]);                    // #13
                store_temp_felt([1]) -> ([1]);                  // #14
                store_temp_felt([2]) -> ([2]);                  // #15
                return ([1], [2]);                              // #16

                branch_align() -> ();                           // #17
                jump() { 19() };                                // #18
                felt_unwrap_nz([1]) -> ([1]);                   // #19
                felt_dup([2]) -> ([2], [3]);                    // #20
                felt_sub([1], [3]) -> ([1]);                    // #21
                store_temp_felt([1]) -> ([1]);                  // #22
                felt_mul_2([1]) -> ([1]);                       // #23
                store_temp_felt([1]) -> ([1]);                  // #24
                store_temp_felt([2]) -> ([2]);                  // #25
                call_foo([1], [2]) -> ([1], [2]);               // #26
                return ([1], [2]);                              // #27

                felt_into_box([1]) -> ([2]);                    // #28
                store_temp_box_felt([2]) -> ([2]);              // #29
                felt_unbox([2]) -> ([3]);                       // #30
                store_temp_felt([3]) -> ([3]);                  // #31
                return ([3]);                                   // #32

                store_temp_felt([1]) -> ([1]);                  // #33
                call_box_and_back([1]) -> ([1]);                // #34
                return ([1]);                                   // #35

                test_program@0([1]: felt, [2]: felt) -> (felt, felt, felt);
                foo@10([1]: felt, [2]: felt) -> (felt, felt);
                box_and_back@28([1]: felt) -> (felt);
                box_and_back_wrapper@33([1]: felt) -> (felt);
            "},
            false,
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
                %{
                if '__boxed_segment' not in globals():
                    __boxed_segment = segments.add()
                memory[ap + 0] = __boxed_segment
                __boxed_segment += 1
                %}
                [fp + -3] = [[ap + 0] + 0], ap++;
                [ap + 0] = [ap + -1], ap++;
                [ap + 0] = [[ap + -1] + 0], ap++;
                ret;

                // box_and_back_wrapper:
                [ap + 0] = [fp + -3], ap++;
                call rel -5;
                ret;
            "};
            "good_flow")]
#[test_case(indoc! {"
                type felt = felt;
                type UninitializedFelt = Uninitialized<felt>;
                type ArrayFelt = Array<felt>;
                type UninitializedArrayFelt = Uninitialized<ArrayFelt>;

                libfunc finalize_locals = finalize_locals;
                libfunc alloc_local_felt = alloc_local<felt>;
                libfunc store_local_felt = store_local<felt>;
                libfunc alloc_local_array_felt = alloc_local<ArrayFelt>;
                libfunc store_local_array_felt = store_local<ArrayFelt>;
                libfunc store_temp_felt = store_temp<felt>;
                libfunc store_temp_array_felt = store_temp<ArrayFelt>;

                store_temp_felt([1]) -> ([1]);
                alloc_local_felt() -> ([4]);
                alloc_local_felt() -> ([5]);
                alloc_local_array_felt() -> ([6]);
                store_local_felt([4], [1]) -> ([4]);
                finalize_locals() -> ();
                store_local_felt([5], [2]) -> ([5]);
                store_local_array_felt([6], [3]) -> ([6]);
                store_temp_felt([4]) -> ([4]);
                store_temp_felt([5]) -> ([5]);
                store_temp_array_felt([6]) -> ([6]);
                return ([4], [5], [6]);

                test_program@0([1]: felt, [2]: felt, [3]: ArrayFelt) -> (felt, felt, ArrayFelt);
            "},
            false,
            indoc! {"
                [ap + 0] = [fp + -6], ap++;
                [fp + 1] = [ap + -1];
                ap += 4;
                [fp + 2] = [fp + -5];
                [fp + 3] = [fp + -4];
                [fp + 4] = [fp + -3];
                [ap + 0] = [fp + 1], ap++;
                [ap + 0] = [fp + 2], ap++;
                [ap + 0] = [fp + 3], ap++;
                [ap + 0] = [fp + 4], ap++;
                ret;
            "};
            "alloc_local and store_local")]
#[test_case(indoc! {"
                type felt = felt;
                type NonZeroFelt = NonZero<felt>;

                libfunc branch_align = branch_align;
                libfunc jump = jump;
                libfunc store_temp_nz_felt = store_temp<NonZeroFelt>;
                libfunc nz_felt_drop = drop<NonZeroFelt>;
                libfunc felt_jump_nz = felt_jump_nz;

                felt_jump_nz([1]) { fallthrough() 3([1]) };
                branch_align() -> ();
                return ();
                branch_align() -> ();
                store_temp_nz_felt([1]) -> ([1]);
                nz_felt_drop([1]) -> ();
                return ();

                test_program@0([1]: felt) -> ();
            "},
            true,
            indoc! {"
                jmp rel 5 if [fp + -3] != 0;
                ap += 1;
                ret;
                [ap + 0] = [fp + -3], ap++;
                ret;
            "};
            "branch align")]
#[test_case(indoc!{"
                type RangeCheck = RangeCheck;
                type u128 = u128;

                libfunc revoke_ap_tracking = revoke_ap_tracking;
                libfunc branch_align = branch_align;
                libfunc jump = jump;
                libfunc u128_lt = u128_lt;
                libfunc store_u128 = store_temp<u128>;
                libfunc store_rc = store_temp<RangeCheck>;

                revoke_ap_tracking() -> ();
                u128_lt([1], [2], [3]) {fallthrough([1]) 4([1]) };
                branch_align() -> ();
                jump() { 5() };
                branch_align() -> ();

                store_rc([1]) -> ([1]);
                return ([1]);

                test_program@0([1]: RangeCheck, [2]: u128, [3]: u128) -> (RangeCheck);
            "}, true, indoc!{"
                [fp + -4] = [ap + 1] + [fp + -3], ap++;
                %{ memory[ap + -1] = memory[ap + 0] < 340282366920938463463374607431768211456 %}
                jmp rel 7 if [ap + -1] != 0, ap++;
                // a < b.
                [ap + 0] = [ap + -1] + 340282366920938463463374607431768211456, ap++;
                [ap + -1] = [[fp + -5] + 0];
                jmp rel 5;
                // a < b.
                [ap + -1] = [[fp + -5] + 0];
                jmp rel 2;
                // Store range_check and return.
                [ap + 0] = [fp + -5] + 1, ap++;
                ret;
            "}; "u128_lt")]
#[test_case(indoc! {"
                type u128 = u128;
                type RangeCheck = RangeCheck;

                libfunc branch_align = branch_align;
                libfunc revoke_ap_tracking = revoke_ap_tracking;
                libfunc u128_overflowing_add = u128_overflowing_add;
                libfunc drop<u128> = drop<u128>;
                libfunc store_temp<RangeCheck> = store_temp<RangeCheck>;

                revoke_ap_tracking() -> ();
                u128_overflowing_add([1], [2], [3]) {fallthrough([1], [2]) 6([1], [2]) };
                branch_align() -> ();
                drop<u128>([2]) -> ();
                store_temp<RangeCheck>([1]) -> ([1]);
                return ([1]);
                branch_align() -> ();
                drop<u128>([2]) -> ();
                store_temp<RangeCheck>([1]) -> ([1]);
                return ([1]);

                test_program@0([1]: RangeCheck, [2]: u128, [3]: u128) -> (RangeCheck);
            "},
            false,
            indoc! {"
                [ap + 1] = [fp + -4] + [fp + -3], ap++;
                %{ memory[ap + -1] = memory[ap + 0] < 340282366920938463463374607431768211456 %}
                jmp rel 7 if [ap + -1] != 0, ap++;
                [ap + -1] = [ap + 0] + 340282366920938463463374607431768211456, ap++;
                [ap + -1] = [[fp + -5] + 0];
                jmp rel 6;
                [ap + -1] = [[fp + -5] + 0];
                [ap + 0] = [fp + -5] + 1, ap++;
                ret;
                [ap + 0] = [fp + -5] + 1, ap++;
                ret;
            "};
            "u128")]
#[test_case(read_sierra_example_file("fib_no_gas").as_str(),
            false,
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
            true,
            indoc! {"
                jmp rel 7 if [fp + -3] != 0;
                [ap + 0] = [fp + -5], ap++;
                [ap + 0] = [fp + -4], ap++;
                [ap + 0] = 1, ap++;
                ret;

                // Statement # 9
                // Setting up the latest memory to be of the form [b=0, _, _, n, rc, gb, a=1].
                [ap + 0] = 0, ap++;
                [ap + 0] = [fp + -3], ap++;
                [ap + 0] = [ap + -1], ap++;
                [ap + 0] = [ap + -1], ap++;
                [ap + 0] = [fp + -5], ap++;
                [ap + 0] = [fp + -4], ap++;
                [ap + 0] = 1, ap++;

                // Statement #21, check n.
                jmp rel 6 if [ap + -4] != 0;
                // Statement # 22 - n == 0, so we can return the latest a.
                [ap + 0] = [ap + -3], ap++;
                [ap + 0] = [ap + -3], ap++;
                [ap + 0] = [ap + -3], ap++;
                ret;
                %{ memory[ap + 0] = 1100 <= memory[ap + -2] %}

                jmp rel 7 if [ap + 0] != 0, ap++;
                [ap + 0] = [ap + -3] + 340282366920938463463374607431768210356, ap++;
                [ap + -1] = [[ap + -5] + 0];
                jmp rel 13;

                // Statement # 31
                // The main loop - given [b, _, _, n, rc, gb, a, _, _] - adds [n-1, updated_rc, updated_gb, a+b]
                // Memory cells form is now [b'=a, _, _, n'=n-1, rc'=updated_rc, gb'=updated_gb, a'=a+b]
                [ap + -3] = [ap + 0] + 1100, ap++;
                [ap + -1] = [[ap + -5] + 0];
                [ap + -6] = [ap + 0] + 1, ap++;
                [ap + 0] = [ap + -6] + 1, ap++;
                [ap + 0] = [ap + -3], ap++;
                [ap + 0] = [ap + -6] + [ap + -12], ap++;
                jmp rel -22;

                // Statement # 41  - Ran out of gas - returning updated gb and -1.
                [ap + 0] = [ap + -5] + 1, ap++;
                [ap + 0] = [ap + -5], ap++;
                [ap + 0] = -1, ap++;
                ret;
            "};
            "fib_jumps")]
fn sierra_to_casm(sierra_code: &str, check_gas_usage: bool, expected_casm: &str) {
    let program = ProgramParser::new().parse(sierra_code).unwrap();
    pretty_assertions::assert_eq!(
        compile(&program, &build_metadata(&program, check_gas_usage), check_gas_usage)
            .expect("Compilation failed.")
            .to_string(),
        strip_comments_and_linebreaks(expected_casm)
    );
}

// TODO(ilya, 10/10/2022): Improve error messages.
#[test_case(indoc! {"
                return([2]);

                test_program@0() -> (felt);
            "},
            "#0: [2] is undefined.";
            "Missing reference")]
#[test_case(indoc! {"
                type felt = felt;
                libfunc felt_dup = dup<felt>;

                felt_dup([1]) -> ([1], [2]);
                felt_dup([2]) -> ([1], [2]);
                return();

                test_program@0([1]: felt) -> ();
            "},
            "#1->#2: [1] was overridden.";
            "Reference override")]
#[test_case(indoc! {"
                type felt = felt;

                return([2]);

                test_program@0([2]: felt) -> (felt);
            "},
            "#0: Return arguments are not on the stack.";
            "Invalid return reference")]
#[test_case(indoc! {"
                type felt = felt;

                store_temp_felt([1]) -> ([1]);

                test_program@0([1]: felt) -> ();
            "},
            "Error from program registry";
            "undeclared libfunc")]
#[test_case(indoc! {"
                type felt = felt;

                libfunc store_temp_felt = store_temp<felt>;
                libfunc store_temp_felt = store_temp<felt>;
            "},
            "Error from program registry";
            "Concrete libfunc Id used twice")]
#[test_case(indoc! {"
                type felt = felt;
                libfunc felt_add = felt_add;

                felt_add([1], [2]) -> ([4]);
                felt_add([3], [4]) -> ([5]);

                test_program@0([1]: felt, [2]: felt, [3]: felt) -> ();
            "},
            "#1: One of the arguments does not satisfy the requirements of the libfunc.";
            "Invalid reference expression for felt_add")]
#[test_case(indoc! {"
                type felt = felt;
                type u128 = u128;
                libfunc felt_add = felt_add;
                felt_add([1], [2]) -> ([3]);
                return([3]);

                test_program@0([1]: u128, [2]: u128) -> (felt);
            "},
            "#0: One of the arguments does not match the expected type of the libfunc or return \
 statement.";
            "Types mismatch")]
#[test_case(indoc! {"
                test_program@25() -> ();
            "}, "InvalidStatementIdx";
            "Invalid entry point")]
#[test_case(indoc! {"
                return();

                foo@0([1]: felt, [1]: felt) -> ();
            "}, "#0: Invalid function declaration.";
            "Bad Declaration")]
#[test_case(indoc! {"
            return();
            "}, "MissingAnnotationsForStatement";
            "Missing references for statement")]
#[test_case(indoc! {"
                type NonZeroFelt = NonZero<felt>;
                type felt = felt;
            "}, "Error from program registry";
            "type ordering bad for building size map")]
#[test_case(indoc! {"
                type felt = felt;
                libfunc felt_add = felt_add;
                felt_add([1], [2], [3]) -> ([4]);
                return();
                test_program@0([1]: felt, [2]: felt, [3]: felt) -> ();
            "}, "#0: Invocation mismatched to libfunc";
            "input count mismatch")]
#[test_case(indoc! {"
                type felt = felt;
                libfunc felt_add = felt_add;
                felt_add([1], [2]) -> ([3], [4]);
                test_program@0([1]: felt, [2]: felt) -> ();
            "}, "#0: Invocation mismatched to libfunc";
            "output type mismatch")]
#[test_case(indoc! {"
                type felt = felt;
                libfunc felt_add = felt_add;
                felt_add([1], [2]) { 0([3]) 1([3]) };
                test_program@0([1]: felt, [2]: felt) -> ();
            "}, "#0: Invocation mismatched to libfunc";
            "branch count mismatch")]
#[test_case(indoc! {"
                type felt = felt;
                libfunc felt_add = felt_add;
                felt_add([1], [2]) { 0([3]) };
                test_program@0([1]: felt, [2]: felt) -> ();
            "}, "#0: Invocation mismatched to libfunc";
            "fallthrough mismatch")]
#[test_case(indoc! {"
                type felt = felt;
                libfunc felt_dup = dup<felt>;

                felt_dup([1]) -> ([1], [2]);
                return ([1]);
                test_program@0([1]: felt) -> ();
            "}, "[2] is dangling at #1.";
            "Dangling references")]
#[test_case(indoc! {" 
                type felt = felt;

                
                return();

                foo@0([1]: felt) -> ();
                bar@0([2]: felt) -> ();
            "}, "#0: Inconsistent references annotations.";
            "Failed building type information")]
#[test_case(indoc! {"
                type felt = felt;
                libfunc felt_dup = dup<felt>;

                felt_dup([1]) -> ([1], [2]);
                return ([1]);
                test_program@0([1]: felt) -> ();
                foo@1([1]: felt) -> (felt);
            "}, "#1: Inconsistent references annotations.";
            "Inconsistent return annotations.")]
#[test_case(indoc! {"
                type felt = felt;
                type NonZeroFelt = NonZero<felt>;

                libfunc branch_align = branch_align;
                libfunc felt_dup = dup<felt>;
                libfunc felt_drop = drop<felt>;
                libfunc felt_jump_nz = felt_jump_nz;
                libfunc store_temp_felt = store_temp<felt>;
                libfunc store_temp_nz_felt = store_temp<NonZeroFelt>;

                felt_jump_nz([1]) { fallthrough() 4([1]) };
                branch_align() -> ();
                store_temp_felt([2]) -> ([2]);
                return ([2]);
                branch_align() -> ();
                felt_drop([2]) -> ();
                store_temp_nz_felt([1]) -> ([1]);
                return ([1]);

                test_program@0([1]: felt, [2]: felt) -> (felt);
            "}, "#7: One of the arguments does not match the expected type \
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
            "}, "#2->#3: Got 'Unknown ap change' error while moving [1].";
            "Ap change error")]
#[test_case(indoc! {"
                type felt = felt;
                type NonZeroFelt = NonZero<felt>;

                libfunc revoke_ap_tracking = revoke_ap_tracking;
                libfunc branch_align = branch_align;
                libfunc felt_drop = drop<felt>;
                libfunc felt_jump_nz = felt_jump_nz;
                libfunc felt_unwrap_nz = unwrap_nz<felt>;
                libfunc jump = jump;

                felt_jump_nz([1]) { fallthrough() 4([1]) };
                branch_align() -> ();
                revoke_ap_tracking() -> ();
                jump() { 7() };
                branch_align() -> ();
                felt_unwrap_nz([1]) -> ([1]);
                felt_drop([1]) -> ();
                return ();

                foo@0([1]: felt) -> ();
            "}, "#7: Inconsistent ap tracking.";
            "Inconsistent ap tracking.")]
#[test_case(indoc! {"
                libfunc finalize_locals = finalize_locals;

                finalize_locals () -> ();
                finalize_locals () -> ();
                return ();

                test_program@0() -> ();
            "}, "#1: finalize_locals is not allowed at this point.";
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
            "}, "#2: finalize_locals is not allowed at this point.";
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
            "}, "#2: alloc_local is not allowed at this point.";
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
            "}, "#3: locals were allocated but finalize_locals was not called.";
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
            "}, "#1: The functionality is supported only for sized types.";
            "store_temp<Uninitialized<felt>()")]
#[test_case(indoc! {"
                return ();

                foo@0() -> ();
                bar@0() -> ();
            "}, "#0 Belongs to two different functions.";
            "Statement in two functions")]
fn compiler_errors(sierra_code: &str, expected_result: &str) {
    let program = ProgramParser::new().parse(sierra_code).unwrap();
    pretty_assertions::assert_eq!(
        compile(&program, &build_metadata(&program, false), false)
            .expect_err("Compilation is expected to fail.")
            .to_string(),
        expected_result
    );
}
