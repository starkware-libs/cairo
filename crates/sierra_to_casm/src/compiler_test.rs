use indoc::indoc;
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
            // TODO(ilya, 10/10/2022): Remove once store_temp does not depend on it.
            type DeferredFelt = Deferred<felt>;

            libfunc felt_add = felt_add;
            libfunc store_temp_felt = store_temp<felt>;
            libfunc rename_felt = rename<felt>;

            rename_felt([1]) -> ([1]);
            felt_add([1], [2]) -> ([3]);
            store_temp_felt([3]) -> ([4]);
            return([4]);

            test_program@0([1]: felt, [2]: felt) -> ();
        "})
        .unwrap();
    assert_eq!(
        compile(&prog).unwrap().to_string(),
        indoc! {"
            [ap + 0] = [fp + -3] + [fp + -2], ap++;
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
        "} => Err(CompilationError::ReferencesError(ReferencesError::InvalidReturnReference));
            "Invalid return reference")]
#[test_case(indoc! {"
            store_temp_felt([1]) -> ([1]);

            test_program@0([1]: felt) -> ();
        "} => Err(CompilationError::ProgramRegistryError(
            ProgramRegistryError::MissingLibFunc(ConcreteLibFuncId::from_string("store_temp_felt"))));
            "undeclared libfunc")]
#[test_case(indoc! {"
            type felt = felt;
            type DeferredFelt = Deferred<felt>;
            libfunc store_temp_felt = store_temp<felt>;
            libfunc store_temp_felt = store_temp<felt>;
        "} => Err(CompilationError::ProgramRegistryError(ProgramRegistryError::LibFuncConcreteIdAlreadyExists(
            ConcreteLibFuncId::from_string("store_temp_felt"))));
            "Concrete libfunc Id used twice")]
#[test_case(indoc! {"
            type felt = felt;
            type DeferredFelt = Deferred<felt>;
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
fn compiler_errors(sierra_code: &str) -> Result<(), CompilationError> {
    let prog = ProgramParser::new().parse(sierra_code).unwrap();
    compile(&prog)?;
    Ok(())
}
