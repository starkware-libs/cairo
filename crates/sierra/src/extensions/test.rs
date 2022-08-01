use super::{ExtensionError, Extensions};
use crate::extensions::SpecializationError;
use crate::program::{Extension, Identifier, TemplateArg, Type};

fn as_extension(id: &str, tmpl_args: Vec<TemplateArg>) -> Extension {
    Extension { id: Identifier::Name(id.into()), tmpl_args }
}

#[test]
fn specialize() {
    let extensions = Extensions::default();
    let assert_success =
        |extension: Extension| assert_eq!(extensions.specialize(&extension).map(|_| ()), Ok(()));
    let assert_failure = |extension: Extension, error| {
        assert_eq!(
            extensions.specialize(&extension).map(|_| ()),
            Err(ExtensionError::Specialization { extension, error })
        )
    };
    let type_arg =
        |name: &str| TemplateArg::Type(Type { id: Identifier::Name(name.into()), args: vec![] });
    assert_success(as_extension("get_gas", vec![TemplateArg::Value(2)]));
    assert_success(as_extension("refund_gas", vec![TemplateArg::Value(7)]));
    assert_success(as_extension("int_op", vec![type_arg("Add")]));
    assert_success(as_extension("int_op", vec![type_arg("Sub")]));
    assert_success(as_extension("int_op", vec![type_arg("Mul")]));
    assert_success(as_extension("int_op", vec![type_arg("Div")]));
    assert_success(as_extension("int_op", vec![type_arg("Mod")]));
    assert_success(as_extension("int_op", vec![type_arg("Add"), TemplateArg::Value(2)]));
    assert_success(as_extension("int_op", vec![type_arg("Sub"), TemplateArg::Value(5)]));
    assert_success(as_extension("int_op", vec![type_arg("Mul"), TemplateArg::Value(7)]));
    assert_success(as_extension("int_op", vec![type_arg("Div"), TemplateArg::Value(9)]));
    assert_success(as_extension("int_op", vec![type_arg("Mod"), TemplateArg::Value(1)]));
    assert_success(as_extension("int_op", vec![TemplateArg::Value(8)]));
    assert_success(as_extension("int_ignore", vec![]));
    assert_success(as_extension("int_dup", vec![]));
    assert_success(as_extension("int_jump_nz", vec![]));
    assert_success(as_extension("int_unwrap_nz", vec![]));
    assert_success(as_extension("store", vec![type_arg("int")]));
    assert_success(as_extension("rename", vec![type_arg("int")]));
    assert_success(as_extension("move", vec![type_arg("int")]));
    assert_success(as_extension("jump", vec![]));
    assert_failure(
        as_extension("NoneExistent", vec![]),
        SpecializationError::UnsupportedLibCallName,
    );
    assert_failure(as_extension("get_gas", vec![]), SpecializationError::UnsupportedTemplateArg);
    assert_failure(
        as_extension("get_gas", vec![TemplateArg::Value(-2)]),
        SpecializationError::UnsupportedTemplateArg,
    );
    assert_failure(as_extension("refund_gas", vec![]), SpecializationError::UnsupportedTemplateArg);
    assert_failure(
        as_extension("refund_gas", vec![TemplateArg::Value(-7)]),
        SpecializationError::UnsupportedTemplateArg,
    );
    assert_failure(as_extension("int_op", vec![]), SpecializationError::UnsupportedTemplateArg);
    assert_failure(as_extension("store", vec![]), SpecializationError::UnsupportedTemplateArg);
    assert_failure(as_extension("rename", vec![]), SpecializationError::UnsupportedTemplateArg);
    assert_failure(as_extension("move", vec![]), SpecializationError::UnsupportedTemplateArg);
    assert_failure(
        as_extension("int_ignore", vec![type_arg("T")]),
        SpecializationError::WrongNumberOfTemplateArgs,
    );
    assert_failure(
        as_extension("int_dup", vec![type_arg("T")]),
        SpecializationError::WrongNumberOfTemplateArgs,
    );
    assert_failure(
        as_extension("int_jump_nz", vec![type_arg("T")]),
        SpecializationError::WrongNumberOfTemplateArgs,
    );
    assert_failure(
        as_extension("int_unwrap_nz", vec![type_arg("T")]),
        SpecializationError::WrongNumberOfTemplateArgs,
    );
    assert_failure(
        as_extension("jump", vec![type_arg("T")]),
        SpecializationError::WrongNumberOfTemplateArgs,
    );
}
