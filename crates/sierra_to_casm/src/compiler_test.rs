use indoc::indoc;
use sierra::edit_state::EditStateError;
use sierra::extensions::ExtensionError::NotImplemented;
use sierra::ids::ConcreteLibFuncId;
use sierra::program_registry::ProgramRegistryError;
use sierra::ProgramParser;
use test_case::test_case;

use crate::compiler::{compile, CompilationError};
use crate::references::ReferencesError;

#[test]
fn good_flow() {
    let prog = ProgramParser::new()
        .parse(indoc! {"
            libfunc store_temp_felt = store_temp<felt>;

            return();

            test_program@0() -> ();
        "})
        .unwrap();
    assert_eq!(
        compile(&prog).unwrap().to_string(),
        indoc! {"
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
            store_temp_felt([1]) -> ([1]);

            test_program@0([1]: felt) -> ();
        "} => Err(CompilationError::ProgramRegistryError(
            ProgramRegistryError::MissingLibFunc(ConcreteLibFuncId::from_string("store_temp_felt"))));
            "undeclared libfunc")]
#[test_case(indoc! {"
            libfunc store_temp_felt = store_temp<felt>;
            libfunc store_temp_felt = store_temp<felt>;
        "} => Err(CompilationError::ProgramRegistryError(ProgramRegistryError::LibFuncConcreteIdUsedTwice(
            ConcreteLibFuncId::from_string("store_temp_felt"))));
            "Concrete libfunc Id used twice")]
#[test_case(indoc! {"
libfunc store_temp_felt = store_temp<felt>;
            store_temp_felt([1]) -> ([1]);

            test_program@0([1]: felt) -> ();
        "} => Err(NotImplemented.into());
            "Not implemented")]
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
