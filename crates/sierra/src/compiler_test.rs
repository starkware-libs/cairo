use indoc::indoc;

use crate::compiler::{compile, CompilationError};
use crate::ProgramParser;

#[test]
fn good_flow() {
    let prog = ProgramParser::new()
        .parse(indoc! {"
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
    assert_eq!(compile(&prog), Err(CompilationError::MissingReference(2.into())));
}

// TODO(ilya, 10/10/2022): Add test for UnsupportedStatement.
