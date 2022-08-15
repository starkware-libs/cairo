use indoc::indoc;
use test_case::test_case;

use crate::compiler::{compile, CompilationError};
use crate::extensions::ExtensionError::{NotImplemented, UndeclaredExtension};
use crate::ids::ConcreteExtensionId;
use crate::references::ReferencesError;
use crate::ProgramParser;

#[test]
fn good_flow() {
    let prog = ProgramParser::new()
        .parse(indoc! {"
            ext store_temp_felt = store_temp<felt>;

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
        "} => Err(CompilationError::MissingReference(2.into()));
            "missing reference")]
#[test_case(indoc! {"
            store_temp_felt([1]) -> ([1]);

            test_program@0() -> ();
        "} => Err(UndeclaredExtension{
            extension_id: ConcreteExtensionId::from_string("store_temp_felt")}.into());
            "undeclared extension")]
#[test_case(indoc! {"
            ext store_temp_felt = store_temp<felt>;
            store_temp_felt([1]) -> ([1]);

            test_program@0() -> ();
        "} => Err(NotImplemented.into());
            "Not implemented")]
#[test_case(indoc! {"
            test_program@25() -> ();
        "} => Err(ReferencesError::InvalidStatementId.into());
            "Invalid entry point")]
#[test_case(indoc! {"
            return();

            foo@0([1]: felt) -> ();
            bar@0([2]: felt) -> ();
        "} => Err(ReferencesError::InconsistentReferences.into());
            "Inconsistent references")]
#[test_case(indoc! {"
            return();
        "} => Err(CompilationError::MissingReferencesForStatement);
            "Missing references for statement")]
#[test_case(indoc! {"
            ext store_temp_felt = store_temp<felt>;
            ext store_temp_felt = store_temp<felt>;
        "} => Err(CompilationError::ConcreteExtensionAlreadyDeclared(
            ConcreteExtensionId::from_string("store_temp_felt")));
            "Concrete extension already declared")]
fn compiler_errors(sierra_code: &str) -> Result<(), CompilationError> {
    let prog = ProgramParser::new().parse(sierra_code).unwrap();
    compile(&prog)?;
    Ok(())
}
