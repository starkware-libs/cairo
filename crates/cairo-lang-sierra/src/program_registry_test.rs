use cairo_lang_test_utils::test;
use indoc::indoc;
use test_case::test_case;

use crate::ProgramParser;
use crate::extensions::core::{CoreLibfunc, CoreType};
use crate::extensions::{ExtensionError, SpecializationError};
use crate::program::{ConcreteTypeLongId, TypeDeclaration};
use crate::program_registry::{ProgramRegistry, ProgramRegistryError};

#[test]
fn basic_insertion() {
    assert_eq!(
        ProgramRegistry::<CoreType, CoreLibfunc>::new(
            &ProgramParser::new()
                .parse(indoc! {"
                    type u128 = u128;
                    type GasBuiltin = GasBuiltin;
                    type NonZeroInt = NonZero<u128>;

                    libfunc rename_u128 = rename<u128>;
                    libfunc rename_gb = rename<GasBuiltin>;

                    return();
                    return();

                    Func1@0(a: u128, gb: GasBuiltin) -> (GasBuiltin);
                    Func2@1() -> ();
                "})
                .unwrap()
        )
        .map(|_| ()),
        Ok(())
    );
}

#[test_case(
    indoc! {"
        type felt = felt252;
        libfunc d = dummy_function_call<user@[0], 0, 0, 1, [999]>;
        return();
        [0]@0() -> ();
    "},
    SpecializationError::MissingTypeInfo(999.into());
    "missing return type info"
)]
#[test_case(
    indoc! {"
        type felt = felt252;
        libfunc d = dummy_function_call<user@[0], 0, 1, 5, 0>;
        return();
        [0]@0() -> ();
    "},
    SpecializationError::UnsupportedGenericArg;
    "value instead of parameter type"
)]
#[test_case(
    indoc! {"
        type felt = felt252;
        libfunc d = dummy_function_call<user@[0], 0, 0, 1, 5>;
        return();
        [0]@0() -> ();
    "},
    SpecializationError::UnsupportedGenericArg;
    "value instead of return type"
)]
#[test_case(
    indoc! {"
        libfunc d = dummy_function_call;
        return();
        [0]@0() -> ();
    "},
    SpecializationError::WrongNumberOfGenericArgs;
    "missing function argument"
)]
fn invalid_dummy_function_call(program: &str, expected_error: SpecializationError) {
    let error = ProgramRegistry::<CoreType, CoreLibfunc>::new(
        &ProgramParser::new().parse(program).unwrap(),
    )
    .map(|_| ())
    .unwrap_err();
    let ProgramRegistryError::LibfuncSpecialization { concrete_id, error } = *error else {
        panic!("Unexpected program registry error: {error:?}");
    };
    assert_eq!(concrete_id, "d".into());
    let ExtensionError::LibfuncSpecialization { libfunc_id, error, .. } = error else {
        panic!("Unexpected extension error: {error:?}");
    };
    assert_eq!(libfunc_id, "dummy_function_call".into());
    assert_eq!(error, expected_error);
}

#[test]
fn valid_dummy_function_call() {
    let program = ProgramParser::new()
        .parse(indoc! {"
            type felt = felt252;
            libfunc d = dummy_function_call<user@[0], 0, 0, 1, felt>;
            return();
            [0]@0() -> ();
        "})
        .unwrap();
    assert!(ProgramRegistry::<CoreType, CoreLibfunc>::new(&program).is_ok());
}

#[test_case(
    indoc! {"
        type C = Const<B, 3>;
        type B = BoundedInt<10, 0> [storable: true, drop: true, dup: true, zero_sized: false];
    "},
    "C",
    "Const";
    "const of inverted bounded int"
)]
#[test_case(
    indoc! {"
        type R = IntRange<T>;
        type T = u8 [storable: true, drop: false, dup: true, zero_sized: false];
    "},
    "R",
    "IntRange";
    "int range of wrongly declared u8"
)]
#[test_case(
    indoc! {"
        type In = CircuitInput<0>;
        type Outputs = Struct<ut@Tuple, G>;
        type C = Circuit<Outputs>;
        type G = AddModGate<In> [storable: false, drop: false, dup: false, zero_sized: true];
    "},
    "C",
    "Circuit";
    "circuit with gate missing an input"
)]
#[test_case(
    indoc! {"
        type In = CircuitInput<0>;
        type Outputs = Struct<ut@Tuple, G>;
        type C = Circuit<Outputs>;
        type G = AddModGate<In, In, In> [storable: false, drop: false, dup: false, zero_sized: true];
    "},
    "C",
    "Circuit";
    "circuit with gate with extra input"
)]
#[test_case(
    indoc! {"
        type In = CircuitInput<0>;
        type Outputs = Struct<ut@Tuple, G>;
        type C = Circuit<Outputs>;
        type G = AddModGate<In, 5> [storable: false, drop: false, dup: false, zero_sized: true];
    "},
    "C",
    "Circuit";
    "circuit with gate with value input"
)]
#[test_case(
    indoc! {"
        type Outputs = Struct<ut@Tuple, G>;
        type C = Circuit<Outputs>;
        type G = AddModGate<In0, In1> [storable: false, drop: false, dup: false, zero_sized: true];
        type In0 = CircuitInput<0> [storable: false, drop: false, dup: false, zero_sized: true];
        type In1 = CircuitInput<0> [storable: false, drop: false, dup: false, zero_sized: true];
    "},
    "C",
    "Circuit";
    "circuit with duplicate input index"
)]
fn invalid_forward_declared_type(program: &str, concrete_id: &str, type_id: &str) {
    assert_eq!(
        ProgramRegistry::<CoreType, CoreLibfunc>::new(
            &ProgramParser::new().parse(program).unwrap()
        )
        .map(|_| ()),
        Err(Box::new(ProgramRegistryError::TypeSpecialization {
            concrete_id: concrete_id.into(),
            error: ExtensionError::TypeSpecialization {
                type_id: type_id.into(),
                error: SpecializationError::UnsupportedGenericArg,
            },
        }))
    );
}

#[test]
fn function_id_double_declaration() {
    assert_eq!(
        ProgramRegistry::<CoreType, CoreLibfunc>::new(
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
        ProgramRegistry::<CoreType, CoreLibfunc>::new(
            &ProgramParser::new()
                .parse(indoc! {"
                    type used_id = u128;
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
    let long_id = ConcreteTypeLongId { generic_id: "u128".into(), generic_args: vec![] };
    assert_eq!(
        ProgramRegistry::<CoreType, CoreLibfunc>::new(
            &ProgramParser::new()
                .parse(indoc! {"
                    type int1 = u128;
                    type int2 = u128;
                "})
                .unwrap()
        )
        .map(|_| ()),
        Err(Box::new(ProgramRegistryError::TypeAlreadyDeclared(Box::new(TypeDeclaration {
            id: "int2".into(),
            long_id,
            declared_type_info: None
        }))))
    );
}

#[test]
fn libfunc_id_double_declaration() {
    assert_eq!(
        ProgramRegistry::<CoreType, CoreLibfunc>::new(
            &ProgramParser::new()
                .parse(indoc! {"
                    type u128 = u128;
                    type GasBuiltin = GasBuiltin;
                    libfunc used_id = rename<u128>;
                    libfunc used_id = rename<GasBuiltin>;
                "})
                .unwrap()
        )
        .map(|_| ()),
        Err(Box::new(ProgramRegistryError::LibfuncConcreteIdAlreadyExists("used_id".into())))
    );
}
