use indoc::indoc;

use crate::program_registry::{ProgramRegistry, ProgramRegistryError};
use crate::ProgramParser;

#[test]
fn basic_insertion() {
    assert_eq!(
        ProgramRegistry::new(
            &ProgramParser::new()
                .parse(indoc! {"
        type int = int;
        type GasBuiltin = GasBuiltin;
        ext move_int = move<int>;
        ext move_gb = move<GasBuiltin>;
    "})
                .unwrap()
        )
        .map(|_| ()),
        Ok(())
    );
}

#[test]
fn double_type_statement() {
    assert_eq!(
        ProgramRegistry::new(
            &ProgramParser::new()
                .parse(indoc! {"
        type same_id = int;
        type same_id = GasBuiltin;
    "})
                .unwrap()
        )
        .map(|_| ()),
        Err(ProgramRegistryError::TypeConcreteIdUsedTwice("same_id".into()))
    );
}

#[test]
fn double_extension_statement() {
    assert_eq!(
        ProgramRegistry::new(
            &ProgramParser::new()
                .parse(indoc! {"
        ext move = move<int>;
        ext move = move<GasBuiltin>;
    "})
                .unwrap()
        )
        .map(|_| ()),
        Err(ProgramRegistryError::ExtensionConcreteIdUsedTwice("move".into()))
    );
}
