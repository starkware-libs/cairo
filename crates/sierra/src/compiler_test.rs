use crate::compiler::{compile, CompilationError};
use crate::program::VarId;
use crate::ProgramParser;

#[test]
fn good_flow() {
    let prog = ProgramParser::new()
        .parse(
            r#"
        return();

        test_program@0() -> ();"#,
        )
        .unwrap();
    assert_eq!(
        compile(&prog).unwrap().to_string(),
        "\
ret;
"
    );
}

#[test]
fn missing_ref() {
    let prog = ProgramParser::new()
        .parse(
            r#"
        return([2]);

        test_program@0() -> ();"#,
        )
        .unwrap();
    assert_eq!(compile(&prog), Err(CompilationError::MissingReference(VarId::Numeric(2))));
}

// TODO(ilya, 10/10/2022): Add test for UnsupportedStatement.
