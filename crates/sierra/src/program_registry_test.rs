use indoc::indoc;

use crate::program_registry::{ProgramRegistry, ProgramRegistryError};
use crate::ProgramParser;

#[test]
fn basic_insertion() {
    assert_eq!(
        ProgramRegistry::new(
            &ProgramParser::new()
                .parse(indoc! {"
        libfunc move_int = move<int>;
        libfunc move_gb = move<GasBuiltin>;
    "})
                .unwrap()
        )
        .map(|_| ()),
        Ok(())
    );
}

#[test]
fn libfunc_id_double_declaration() {
    assert_eq!(
        ProgramRegistry::new(
            &ProgramParser::new()
                .parse(indoc! {"
        libfunc used_id = move<int>;
        libfunc used_id = move<GasBuiltin>;
    "})
                .unwrap()
        )
        .map(|_| ()),
        Err(ProgramRegistryError::LibFuncConcreteIdUsedTwice("used_id".into()))
    );
}
