use indoc::indoc;
use pretty_assertions;
use sierra::edit_state::EditStateError;
use sierra::ids::ConcreteLibFuncId;
use sierra::program_registry::ProgramRegistryError;
use sierra::ProgramParser;
use test_case::test_case;

use crate::compiler::{compile, CompilationError};
use crate::invocations::InvocationError;
use crate::references::ReferencesError;

#[test]
fn good_flow() {
    let prog = ProgramParser::new()
        .parse(indoc! {"
            type felt = felt;

            libfunc felt_add = felt_add;
            libfunc felt_dup = felt_dup;
            libfunc store_temp_felt = store_temp<felt>;
            libfunc rename_felt = rename<felt>;
            libfunc call_foo = function_call<user@foo>;

            rename_felt([1]) -> ([1]);          // #0
            felt_dup([2]) -> ([2], [5]);        // #1
            felt_add([1], [2]) -> ([3]);        // #2
            store_temp_felt([3]) -> ([4]);      // #3
            felt_dup([4]) -> ([4], [6]);        // #4
            store_temp_felt([5]) -> ([5]);      // #5
            store_temp_felt([6]) -> ([6]);      // #6
            call_foo([5], [6]) -> ([7], [8]);   // #7
            store_temp_felt([4]) -> ([4]);      // #8
            return([7], [8], [4]);              // #9

            store_temp_felt([1]) -> ([1]);      // #10
            store_temp_felt([2]) -> ([2]);      // #11
            return ([1], [2]);                  // #12

            test_program@0([1]: felt, [2]: felt) -> ();
            foo@10([1]: felt, [2]: felt) -> (felt, felt);
        "})
        .unwrap();
    pretty_assertions::assert_eq!(
        compile(&prog).unwrap().to_string(),
        indoc! {"
            [ap + 0] = [fp + -3] + [fp + -2], ap++;
            [ap + 0] = [fp + -2], ap++;
            [ap + 0] = [ap + -2], ap++;
            call rel 0;
            [ap + 0] = [ap + -3], ap++;
            ret;
            [ap + 0] = [fp + -3], ap++;
            [ap + 0] = [fp + -2], ap++;
            ret;
        "}
    );
}

#[test_case(indoc! {"
            return([2]);

            test_program@0() -> ();
        "} => Err(CompilationError::ReferencesError(ReferencesError::EditStateError(EditStateError::MissingReference(2.into()))));
            "missing reference")]
#[test_case(indoc! {"
            return([2]);

            test_program@0([2]: felt) -> ();
        "} => Err(InvocationError::InvalidReferenceExpressionForArgument.into());
            "Invalid return reference")]
#[test_case(indoc! {"
            store_temp_felt([1]) -> ([1]);

            test_program@0([1]: felt) -> ();
        "} => Err(CompilationError::ProgramRegistryError(
            ProgramRegistryError::MissingLibFunc(ConcreteLibFuncId::from_string("store_temp_felt"))));
            "undeclared libfunc")]
#[test_case(indoc! {"
            type felt = felt;
            libfunc store_temp_felt = store_temp<felt>;
            libfunc store_temp_felt = store_temp<felt>;
        "} => Err(CompilationError::ProgramRegistryError(ProgramRegistryError::LibFuncConcreteIdAlreadyExists(
            ConcreteLibFuncId::from_string("store_temp_felt"))));
            "Concrete libfunc Id used twice")]
#[test_case(indoc! {"
            type felt = felt;
            libfunc store_local_felt = store_local<felt>;
            store_local_felt([1]) -> ([1]);

            test_program@0([1]: felt) -> ();
        "} => Err(InvocationError::NotImplemented.into());
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
        "} => Err(InvocationError::InvalidReferenceTypeForArgument.into());
            "Types mismatch")]
#[test_case(indoc! {"
            test_program@25() -> ();
        "} => Err(ReferencesError::InvalidStatementIdx.into());
            "Invalid entry point")]
#[test_case(indoc! {"
            return();

            foo@0([1]: felt) -> ();
            bar@0([2]: felt) -> ();
        "} => Err(ReferencesError::InconsistentReferences.into());
            "Inconsistent references")]
#[test_case(indoc! {"
            return();
        "} => Err(CompilationError::ReferencesError(
            ReferencesError::MissingReferencesForStatement));
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
fn compiler_errors(sierra_code: &str) -> Result<(), CompilationError> {
    let prog = ProgramParser::new().parse(sierra_code).unwrap();
    compile(&prog)?;
    Ok(())
}
