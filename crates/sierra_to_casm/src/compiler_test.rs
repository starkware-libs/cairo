use std::fs;
use std::path::PathBuf;

use indoc::indoc;
use pretty_assertions;
use sierra::edit_state::EditStateError::MissingReference;
use sierra::ids::ConcreteLibFuncId;
use sierra::program::{BranchInfo, BranchTarget, Invocation, StatementIdx};
use sierra::program_registry::ProgramRegistryError::{
    LibFuncConcreteIdAlreadyExists, MissingLibFunc,
};
use sierra::ProgramParser;
use test_case::test_case;

use crate::annotations::AnnotationError::{
    self, EditStateError, InconsistentReferencesAnnotation, InvalidStatementIdx,
    MissingAnnotationsForStatement,
};
use crate::compiler::{compile, CompilationError};
use crate::invocations::InvocationError;
use crate::references::ReferencesError::{self, InvalidFunctionDeclaration};

#[test]
fn good_flow() {
    let prog = ProgramParser::new()
        .parse(indoc! {"
            type felt = felt;
            type NonZeroFelt = NonZero<felt>;

            libfunc felt_add = felt_add;
            libfunc felt_sub = felt_sub;
            libfunc felt_dup = felt_dup;
            libfunc felt_jump_nz = felt_jump_nz;
            libfunc jump = jump;
            libfunc felt_unwrap_nz = unwrap_nz<felt>;
            libfunc store_temp_felt = store_temp<felt>;
            libfunc rename_felt = rename<felt>;
            libfunc call_foo = function_call<user@foo>;

            rename_felt([1]) -> ([1]);                      // #0
            felt_dup([2]) -> ([2], [5]);                    // #1
            felt_add([1], [2]) -> ([3]);                    // #2
            store_temp_felt([3]) -> ([4]);                  // #3
            felt_dup([4]) -> ([4], [6]);                    // #4
            store_temp_felt([5]) -> ([5]);                  // #5
            store_temp_felt([6]) -> ([6]);                  // #6
            call_foo([5], [6]) -> ([7], [8]);               // #7
            store_temp_felt([4]) -> ([4]);                  // #8
            return([7], [8], [4]);                          // #9

            felt_jump_nz([1]) { 15([1]) fallthrough() };    // #10
            felt_dup([2]) -> ([1], [2]);                    // #11
            store_temp_felt([1]) -> ([1]);                  // #12
            store_temp_felt([2]) -> ([2]);                  // #13
            return ([1], [2]);                              // #14

            jump() { 16() };                                // #15
            felt_unwrap_nz([1]) -> ([1]);                   // #16
            felt_dup([2]) -> ([2], [3]);                    // #17
            felt_sub([1], [3]) -> ([1]);                    // #18
            store_temp_felt([1]) -> ([1]);                  // #19
            store_temp_felt([2]) -> ([2]);                  // #20
            call_foo([1], [2]) -> ([1], [2]);               // #21
            return ([1], [2]);                              // #22

            test_program@0([1]: felt, [2]: felt) -> (felt, felt, felt);
            foo@10([1]: felt, [2]: felt) -> (felt, felt);
        "})
        .unwrap();
    pretty_assertions::assert_eq!(
        compile(&prog).unwrap().to_string(),
        indoc! {"
            [ap + 0] = [fp + -4] + [fp + -3], ap++;
            [ap + 0] = [fp + -3], ap++;
            [ap + 0] = [ap + -2], ap++;
            call rel 4;
            [ap + 0] = [ap + -3], ap++;
            ret;
            jmp rel 5 if [fp + -4] != 0;
            [ap + 0] = [fp + -3], ap++;
            [ap + 0] = [fp + -3], ap++;
            ret;
            jmp rel 2;
            [fp + -4] = [ap + 0] + [fp + -3], ap++;
            [ap + 0] = [fp + -3], ap++;
            call rel -9;
            ret;
        "}
    );
}

#[test]
fn fib_program() {
    let path: PathBuf =
        [env!("CARGO_MANIFEST_DIR"), "../sierra/examples/fib_no_gas.sierra"].iter().collect();
    let prog = sierra::ProgramParser::new().parse(&fs::read_to_string(path).unwrap()).unwrap();
    pretty_assertions::assert_eq!(
        compile(&prog).unwrap().to_string(),
        indoc! {"
            jmp rel 4 if [fp + -3] != 0;
            [ap + 0] = [fp + -5], ap++;
            ret;
            [ap + 0] = [fp + -4], ap++;
            [ap + 0] = [fp + -5] + [fp + -4], ap++;
            [ap + 0] = [fp + -3] + -1, ap++;
            call rel -8;
            ret;
        "}
    );
}

#[test_case(indoc! {"
                return([2]);

                test_program@0() -> ();
            "} => Err(CompilationError::AnnotationError(EditStateError(MissingReference(
                2.into()
            ))));
            "missing reference")]
#[test_case(indoc! {"
                return([2]);

                test_program@0([2]: felt) -> (felt);
            "} =>
            Err(InvocationError::InvalidReferenceExpressionForArgument.into());
            "Invalid return reference")]
#[test_case(indoc! {"
                store_temp_felt([1]) -> ([1]);

                test_program@0([1]: felt) -> ();
            "} => Err(CompilationError::ProgramRegistryError(MissingLibFunc(
                ConcreteLibFuncId::from_string("store_temp_felt")
            )));
            "undeclared libfunc")]
#[test_case(indoc! {"
                type felt = felt;
                libfunc store_temp_felt = store_temp<felt>;
                libfunc store_temp_felt = store_temp<felt>;
            "} => Err(CompilationError::ProgramRegistryError(LibFuncConcreteIdAlreadyExists(
                ConcreteLibFuncId::from_string("store_temp_felt")
            )));
            "Concrete libfunc Id used twice")]
#[test_case(indoc! {"
                type felt = felt;
                libfunc store_local_felt = store_local<felt>;
                store_local_felt([1]) -> ([1]);

                test_program@0([1]: felt) -> ();
            "} => Err(InvocationError::NotImplemented(
                Invocation{
                    libfunc_id: ConcreteLibFuncId::from_string("store_local_felt"),
                    args: vec![sierra::ids::VarId::new(1)],
                    branches: vec![BranchInfo{
                        target: BranchTarget::Fallthrough,
                        results: vec![sierra::ids::VarId::new(1)],
                    }],
                }).into());
            "Not implemented")]
#[test_case(indoc! {"
                type felt = felt;
                libfunc felt_add = felt_add;

                felt_add([1], [2]) -> ([4]);
                felt_add([3], [4]) -> ([5]);

                test_program@0([1]: felt, [2]: felt, [3]: felt) -> ();
            "} => Err(InvocationError::InvalidReferenceExpressionForArgument.into());
            "Invalid reference expression for felt_add")]
#[test_case(indoc! {"
                type felt = felt;
                type int = int;
                libfunc felt_add = felt_add;
                felt_add([1], [2]) -> ([3]);
                return([3]);

                test_program@0([1]: int, [2]: int) -> (felt);
            "} => Err(ReferencesError::InvalidReferenceTypeForArgument.into());
            "Types mismatch")]
#[test_case(indoc! {"
                test_program@25() -> ();
            "} => Err(InvalidStatementIdx.into());
            "Invalid entry point")]
#[test_case(indoc! {"
                return();

                foo@0([1]: felt, [1]: felt) -> ();
            "} => matches Err(CompilationError::AnnotationError(
                AnnotationError::ReferencesError(InvalidFunctionDeclaration(_))));
            "Bad Declaration")]
#[test_case(indoc! {"
            return();
            "} => Err(CompilationError::AnnotationError(
            MissingAnnotationsForStatement(StatementIdx(0))));
            "Missing references for statement")]
#[test_case(indoc! {"
                type NonZeroFelt = NonZero<felt>;
                type felt = felt;
            "} => Err(CompilationError::FailedBuildingTypeInformation);
            "type ordering bad for building size map")]
#[test_case(indoc! {"
                type felt = felt;
                libfunc felt_add = felt_add;
                felt_add([1], [2], [3]) -> ([4]);
                test_program@0([1]: felt, [2]: felt, [3]: felt) -> ();
            "} => Err(CompilationError::LibFuncInvocationMismatch);
            "input count mismatch")]
#[test_case(indoc! {"
                type felt = felt;
                libfunc felt_add = felt_add;
                felt_add([1], [2]) -> ([3], [4]);
                test_program@0([1]: felt, [2]: felt) -> ();
            "} => Err(CompilationError::LibFuncInvocationMismatch);
            "output type mismatch")]
#[test_case(indoc! {"
                type felt = felt;
                libfunc felt_add = felt_add;
                felt_add([1], [2]) { 0([3]) 1([3]) };
                test_program@0([1]: felt, [2]: felt) -> ();
            "} => Err(CompilationError::LibFuncInvocationMismatch);
            "branch count mismatch")]
#[test_case(indoc! {"
                type felt = felt;
                libfunc felt_add = felt_add;
                felt_add([1], [2]) { 0([3]) };
                test_program@0([1]: felt, [2]: felt) -> ();
            "} => Err(CompilationError::LibFuncInvocationMismatch);
            "fallthrough mismatch")]
#[test_case(indoc! {"
                type felt = felt;
                libfunc felt_dup = felt_dup;

                felt_dup([1]) -> ([1], [2]);
                return ([1]);
                test_program@0([1]: felt) -> ();
            "} => Err(ReferencesError::DanglingReferences(StatementIdx(1)).into());
            "Dangling references")]
#[test_case(indoc! {"
                return();

                foo@0([1]: felt) -> ();
                bar@0([2]: felt) -> ();
            "} => Err(InconsistentReferencesAnnotation(StatementIdx(0)).into());
            "Two different function definitions fo a statement.")]
#[test_case(indoc! {"
                type felt = felt;
                libfunc felt_dup = felt_dup;

                felt_dup([1]) -> ([1], [2]);
                return ([1]);
                test_program@0([1]: felt) -> ();
                foo@1([1]: felt) -> (felt);
            "} => Err(InconsistentReferencesAnnotation(StatementIdx(1)).into());
            "Inconsistent return annotations")]
#[test_case(indoc! {"
                type felt = felt;
                type NonZeroFelt = NonZero<felt>;

                libfunc felt_dup = felt_dup;
                libfunc felt_ignore = felt_ignore;
                libfunc felt_jump_nz = felt_jump_nz;
                libfunc store_temp_felt = store_temp<felt>;
                libfunc store_temp_nz_felt = store_temp<NonZeroFelt>;

                felt_jump_nz([1]) { 3([1]) fallthrough() };
                store_temp_felt([2]) -> ([2]);
                return ([2]);
                felt_ignore([2]) -> ();
                store_temp_nz_felt([1]) -> ([1]);
                return ([1]);

                test_program@0([1]: felt, [2]: felt) -> (felt);
            "} => Err(AnnotationError::ReferencesError(
                ReferencesError::InvalidReferenceTypeForArgument).into());
            "Invalid return type")]
fn compiler_errors(sierra_code: &str) -> Result<(), CompilationError> {
    let prog = ProgramParser::new().parse(sierra_code).unwrap();
    compile(&prog).map(|_| ())
}
