use indoc::indoc;

use crate::compiler::{compile, CompilationError};
use crate::ids::VarId;
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
#[should_panic]
fn unimplemented_extension() {
    let prog = ProgramParser::new()
        .parse(indoc! {"
            ext store_temp_felt = store_temp<felt>;

            store_temp_felt([1]) -> ([1]);
            return();

            test_program@0() -> ();
        "})
        .unwrap();
    let _res = compile(&prog);
}
// TODO(ilya, 10/10/2022): Add test for UnsupportedStatement.
