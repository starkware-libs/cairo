use std::fs;
use std::path::PathBuf;

use casm::ap_change::ApChange;
use indoc::indoc;
use pretty_assertions;
use sierra::ids::FunctionId;
use sierra::ProgramParser;
use test_case::test_case;

use crate::compiler::compile;
use crate::metadata::Metadata;

fn build_metadata(ap_change_data: &[(&str, i16)]) -> Metadata {
    Metadata {
        function_ap_change: ap_change_data
            .iter()
            .map(|(func_name, change)| {
                (FunctionId::from_string(func_name), ApChange::Known(*change))
            })
            .collect(),
    }
}

fn read_sierra_example_file(name: &str) -> String {
    // Pop the "/sierra_to_casm" suffix.
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).parent().unwrap().to_owned();
    path.extend(["sierra", "examples", &format!("{name}.sierra")].into_iter());
    fs::read_to_string(path).unwrap()
}

#[test_case(indoc! {"
                type felt = felt;
                type NonZeroFelt = NonZero<felt>;
                type BoxFelt = Box<felt>;

                libfunc finalize_locals = finalize_locals;
                libfunc felt_add = felt_add;
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
                felt_jump_nz([1]) { 16([1]) fallthrough() };    // #11
                felt_dup([2]) -> ([1], [2]);                    // #12
                store_temp_felt([1]) -> ([1]);                  // #13
                store_temp_felt([2]) -> ([2]);                  // #14
                return ([1], [2]);                              // #15

                jump() { 17() };                                // #16
                felt_unwrap_nz([1]) -> ([1]);                   // #17
                felt_dup([2]) -> ([2], [3]);                    // #18
                felt_sub([1], [3]) -> ([1]);                    // #19
                store_temp_felt([1]) -> ([1]);                  // #20
                store_temp_felt([2]) -> ([2]);                  // #21
                call_foo([1], [2]) -> ([1], [2]);               // #22
                return ([1], [2]);                              // #23

                felt_into_box([1]) -> ([2]);                    // #24
                store_temp_box_felt([2]) -> ([2]);              // #25
                felt_unbox([2]) -> ([3]);                       // #26
                store_temp_felt([3]) -> ([3]);                  // #27
                return ([3]);                                   // #28

                test_program@0([1]: felt, [2]: felt) -> (felt, felt, felt);
                foo@10([1]: felt, [2]: felt) -> (felt, felt);
                box_and_back@24([1]: felt) -> (felt);
            "},
            &build_metadata(&[("box_and_back", 2)]),
            indoc! {"
                [ap + 0] = [fp + -4] + [fp + -3], ap++;
                [ap + 0] = [fp + -3], ap++;
                [ap + 0] = [ap + -2], ap++;
                call rel 4;
                [ap + 0] = [ap + -1], ap++;
                ret;
                ap += 0;
                jmp rel 5 if [fp + -4] != 0;
                [ap + 0] = [fp + -3], ap++;
                [ap + 0] = [fp + -3], ap++;
                ret;
                jmp rel 2;
                [fp + -4] = [ap + 0] + [fp + -3], ap++;
                [ap + 0] = [fp + -3], ap++;
                call rel -11;
                ret;
                %{ memory[ap + 0] = segments.add() %}
                [fp + -3] = [[ap + 0]], ap++;
                [ap + 0] = [[ap + -1]], ap++;
                ret;
            "};
            "good_flow")]
#[test_case(indoc! {"
                type felt = felt;
                type UninitializedFelt = uninitialized<felt>;

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
            &build_metadata(&[("test_program", 5)]),
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
#[test_case(read_sierra_example_file("fib_no_gas").as_str(),
            &build_metadata(&[]),
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
#[test_case(read_sierra_example_file("collatz").as_str(),
            &build_metadata(&[]),
            indoc! {"
            "} => ignore["Gas usage lowering not supported yet."];
            "collatz")]
#[test_case(read_sierra_example_file("fib_jumps").as_str(),
            &build_metadata(&[]),
            indoc! {"
            "} => ignore["Gas usage lowering not supported yet."];
            "fib_jumps")]
#[test_case(read_sierra_example_file("fib_recursive").as_str(),
            &build_metadata(&[]),
            indoc! {"
            "} => ignore["Gas usage lowering not supported yet."];
            "fib_recursive")]
fn sierra_to_casm(sierra_code: &str, metadata: &Metadata, expected_casm: &str) {
    let program = ProgramParser::new().parse(sierra_code).unwrap();
    pretty_assertions::assert_eq!(
        compile(&program, metadata).expect("Compilation failed.").to_string(),
        expected_casm
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
                return([2]);

                test_program@0([2]: felt) -> (felt);
            "}, &[("test_program", 0)],
            "#0: Return arguments are not on the stack.";
            "Invalid return reference")]
#[test_case(indoc! {"
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
                type int = int;

                libfunc int_add = int_add;

                int_add([1], [2]) -> ([1]);
                test_program@0([1]: int, [2]: int) -> ();
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
                type int = int;
                libfunc felt_add = felt_add;
                felt_add([1], [2]) -> ([3]);
                return([3]);

                test_program@0([1]: int, [2]: int) -> (felt);
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

                felt_jump_nz([1]) { 3([1]) fallthrough() };
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

                felt_jump_nz([1]) { 3([1]) fallthrough() };
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
                type UninitializedFelt = uninitialized<felt>;

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
                type UninitializedFelt = uninitialized<felt>;

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
                type UninitializedFelt = uninitialized<felt>;

                libfunc store_temp_felt = store_temp<UninitializedFelt>;

                store_temp_felt([1]) -> ([1]);
                return ();

                foo@0([1]:UninitializedFelt) -> ();
            "}, &[], "#0: The functionality is supported only for sized types.";
            "store_temp<uninitialized<felt>()")]
#[test_case(indoc! {"
                return ();

                foo@0() -> ();
            "}, &[("foo", 5)], "#0: Invalid Ap change annotation. \
expected: ApChange::Known(5) got: ApChange::Known(0).";
            "bad Ap change")]
fn compiler_errors(sierra_code: &str, ap_change_data: &[(&str, i16)], expected_result: &str) {
    let prog = ProgramParser::new().parse(sierra_code).unwrap();
    pretty_assertions::assert_eq!(
        compile(&prog, &build_metadata(ap_change_data))
            .expect_err("Compilation is expected to fail.")
            .to_string(),
        expected_result
    );
}
