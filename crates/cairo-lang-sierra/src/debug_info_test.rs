use std::collections::HashMap;

use indoc::indoc;

use super::DebugInfo;
use crate::ProgramParser;

#[test]
fn test_extract_names() {
    assert_eq!(
        DebugInfo::extract(
            &ProgramParser::new()
                .parse(indoc! {"
                    type u128 = u128;
                    type GasBuiltin = GasBuiltin;
                    type NonZeroInt = NonZero<u128>;
                    libfunc rename_u128 = rename<u128>;
                    libfunc rename_gb = rename<GasBuiltin>;
                    Func1@1(a: u128, gb: GasBuiltin) -> (GasBuiltin);
                    Func2@6() -> ();
                "})
                .unwrap(),
        ),
        DebugInfo {
            type_names: HashMap::from([
                ("u128".into(), "u128".into()),
                ("GasBuiltin".into(), "GasBuiltin".into()),
                ("NonZeroInt".into(), "NonZeroInt".into())
            ]),
            libfunc_names: HashMap::from([
                ("rename_u128".into(), "rename_u128".into()),
                ("rename_gb".into(), "rename_gb".into())
            ]),
            user_func_names: HashMap::from([
                ("Func1".into(), "Func1".into()),
                ("Func2".into(), "Func2".into())
            ]),
        }
    );
}

#[test]
fn test_populate_names() {
    let mut program = ProgramParser::new()
        .parse(indoc! {"
        type [0] = u128;
        type [1] = GasBuiltin;
        type [2] = NonZero<[0]>;

        libfunc [0] = rename<[0]>;
        libfunc [1] = rename<[1]>;

        [0](a) -> (a);
        [1](gb) -> (gb);

        Func1@1(a: [0], gb: [1]) -> ([1]);
        Func2@6() -> ();
    "})
        .unwrap();
    DebugInfo {
        type_names: HashMap::from([
            (0.into(), "u128".into()),
            (1.into(), "GasBuiltin".into()),
            (2.into(), "NonZeroInt".into()),
        ]),
        libfunc_names: HashMap::from([
            (0.into(), "rename_u128".into()),
            (1.into(), "rename_gb".into()),
        ]),
        user_func_names: HashMap::from([(0.into(), "Func1".into()), (1.into(), "Func2".into())]),
    }
    .populate(&mut program);

    assert_eq!(
        program.to_string(),
        indoc! {"
            type u128 = u128;
            type GasBuiltin = GasBuiltin;
            type NonZeroInt = NonZero<u128>;

            libfunc rename_u128 = rename<u128>;
            libfunc rename_gb = rename<GasBuiltin>;

            rename_u128(a) -> (a);
            rename_gb(gb) -> (gb);

            Func1@1(a: u128, gb: GasBuiltin) -> (GasBuiltin);
            Func2@6() -> ();
        "}
    );
}
