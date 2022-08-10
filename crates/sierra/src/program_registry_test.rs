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
        Some@1(var: int, gb: GasBuiltin) -> (int, GasBuiltin);
    "})
                .unwrap()
        )
        .map(|_| ()),
        Ok(())
    );
}

#[test]
fn type_id_double_declaration() {
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

#[test]
fn double_function_statement() {
    assert_eq!(
        ProgramRegistry::new(
            &ProgramParser::new()
                .parse(indoc! {"
        SameId@0() -> (int, GasBuiltin);
        SameId@1(var: int, gb: GasBuiltin) -> (int, GasBuiltin);
    "})
                .unwrap()
        )
        .map(|_| ()),
        Err(ProgramRegistryError::FunctionIdUsedTwice("SameId".into()))
    );
}
