use indoc::indoc;

use crate::compiler::{compile, CompilationError};
use crate::extensions::ExtensionError::NotImplemented;
use crate::ids::{ConcreteExtensionId, VarId};
use crate::program_registry::ProgramRegistryError::MissingExtension;
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

#[test]
fn missing_ref() {
    let prog = ProgramParser::new()
        .parse(indoc! {"
            return([2]);

            test_program@0() -> ();"
        })
        .unwrap();
    assert_matches!(
        compile(&prog), Err(CompilationError::MissingReference(x)) if x == VarId::new(2));
}

#[test]
fn unimplemented_extension() {
    let prog = ProgramParser::new()
        .parse(indoc! {"
            ext store_temp_felt = store_temp<felt>;

            store_temp_felt([1]) -> ([1]);
            return();

            test_program@0() -> ();
        "})
        .unwrap();

    assert_matches!(compile(&prog), Err(CompilationError::ExtensionError(NotImplemented)));
}

#[test]
fn undeclared_extension() {
    let prog = ProgramParser::new()
        .parse(indoc! {"
            store_temp_felt([1]) -> ([1]);

            test_program@0() -> ();
        "})
        .unwrap();

    assert_matches!(
        compile(&prog), Err(
            CompilationError::ProgramRegistryError(MissingExtension(extension_id)))
                if extension_id == ConcreteExtensionId::from_string("store_temp_felt"));
}
