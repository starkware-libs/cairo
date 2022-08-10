use indoc::indoc;

use crate::program_registry::{ProgramRegistry, ProgramRegistryError};
use crate::ProgramParser;

#[test]
fn basic_insertion() {
    assert_eq!(
        ProgramRegistry::new(
            &ProgramParser::new()
                .parse(indoc! {"
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
fn extension_id_double_declaration() {
    assert_eq!(
        ProgramRegistry::new(
            &ProgramParser::new()
                .parse(indoc! {"
        ext used_id = move<int>;
        ext used_id = move<GasBuiltin>;
    "})
                .unwrap()
        )
        .map(|_| ()),
        Err(ProgramRegistryError::ExtensionConcreteIdUsedTwice("used_id".into()))
    );
}
