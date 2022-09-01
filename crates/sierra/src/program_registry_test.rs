use indoc::indoc;

use crate::extensions::core::{CoreLibFunc, CoreType};
use crate::program::{ConcreteTypeLongId, TypeDeclaration};
use crate::program_registry::{ProgramRegistry, ProgramRegistryError};
use crate::ProgramParser;

#[test]
fn basic_insertion() {
    assert_eq!(
        ProgramRegistry::<CoreType, CoreLibFunc>::new(
            &ProgramParser::new()
                .parse(indoc! {"
                    type int = int;
                    type GasBuiltin = GasBuiltin;
                    type NonZeroInt = NonZero<int>;
                    libfunc rename_int = rename<int>;
                    libfunc rename_gb = rename<GasBuiltin>;
                    Func1@1(a: int, gb: GasBuiltin) -> (GasBuiltin);
                    Func2@6() -> ();
                "})
                .unwrap()
        )
        .map(|_| ()),
        Ok(())
    );
}

#[test]
fn function_id_double_declaration() {
    assert_eq!(
        ProgramRegistry::<CoreType, CoreLibFunc>::new(
            &ProgramParser::new()
                .parse(indoc! {"
                    used_id@1(a: int, gb: GasBuiltin) -> (GasBuiltin);
                    used_id@6() -> ();
                "})
                .unwrap()
        )
        .map(|_| ()),
        Err(ProgramRegistryError::FunctionIdAlreadyExists("used_id".into()))
    );
}

#[test]
fn type_id_double_declaration() {
    assert_eq!(
        ProgramRegistry::<CoreType, CoreLibFunc>::new(
            &ProgramParser::new()
                .parse(indoc! {"
                    type used_id = int;
                    type used_id = GasBuiltin;
                    "})
                .unwrap()
        )
        .map(|_| ()),
        Err(ProgramRegistryError::TypeConcreteIdAlreadyExists("used_id".into()))
    );
}

#[test]
fn concrete_type_double_declaration() {
    assert_eq!(
        ProgramRegistry::<CoreType, CoreLibFunc>::new(
            &ProgramParser::new()
                .parse(indoc! {"
                    type int1 = int;
                    type int2 = int;
                "})
                .unwrap()
        )
        .map(|_| ()),
        Err(ProgramRegistryError::TypeAlreadyDeclared(TypeDeclaration {
            id: "int2".into(),
            long_id: ConcreteTypeLongId { generic_id: "int".into(), args: vec![] },
        }))
    );
}

#[test]
fn libfunc_id_double_declaration() {
    assert_eq!(
        ProgramRegistry::<CoreType, CoreLibFunc>::new(
            &ProgramParser::new()
                .parse(indoc! {"
                    type int = int;
                    type GasBuiltin = GasBuiltin;
                    libfunc used_id = rename<int>;
                    libfunc used_id = rename<GasBuiltin>;
                "})
                .unwrap()
        )
        .map(|_| ()),
        Err(ProgramRegistryError::LibFuncConcreteIdAlreadyExists("used_id".into()))
    );
}
