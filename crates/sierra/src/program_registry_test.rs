use indoc::indoc;
use test_log::test;

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
                    type uint128 = uint128;
                    type GasBuiltin = GasBuiltin;
                    type NonZeroInt = NonZero<uint128>;
                    libfunc rename_uint128 = rename<uint128>;
                    libfunc rename_gb = rename<GasBuiltin>;
                    Func1@1(a: uint128, gb: GasBuiltin) -> (GasBuiltin);
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
        Err(Box::new(ProgramRegistryError::FunctionIdAlreadyExists("used_id".into())))
    );
}

#[test]
fn type_id_double_declaration() {
    assert_eq!(
        ProgramRegistry::<CoreType, CoreLibFunc>::new(
            &ProgramParser::new()
                .parse(indoc! {"
                    type used_id = uint128;
                    type used_id = GasBuiltin;
                    "})
                .unwrap()
        )
        .map(|_| ()),
        Err(Box::new(ProgramRegistryError::TypeConcreteIdAlreadyExists("used_id".into())))
    );
}

#[test]
fn concrete_type_double_declaration() {
    assert_eq!(
        ProgramRegistry::<CoreType, CoreLibFunc>::new(
            &ProgramParser::new()
                .parse(indoc! {"
                    type int1 = uint128;
                    type int2 = uint128;
                "})
                .unwrap()
        )
        .map(|_| ()),
        Err(Box::new(ProgramRegistryError::TypeAlreadyDeclared(Box::new(TypeDeclaration {
            id: "int2".into(),
            long_id: ConcreteTypeLongId { generic_id: "uint128".into(), generic_args: vec![] },
        }))))
    );
}

#[test]
fn libfunc_id_double_declaration() {
    assert_eq!(
        ProgramRegistry::<CoreType, CoreLibFunc>::new(
            &ProgramParser::new()
                .parse(indoc! {"
                    type uint128 = uint128;
                    type GasBuiltin = GasBuiltin;
                    libfunc used_id = rename<uint128>;
                    libfunc used_id = rename<GasBuiltin>;
                "})
                .unwrap()
        )
        .map(|_| ()),
        Err(Box::new(ProgramRegistryError::LibFuncConcreteIdAlreadyExists("used_id".into())))
    );
}
