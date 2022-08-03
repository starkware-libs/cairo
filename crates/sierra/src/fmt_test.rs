use crate::program::{
    BranchInfo, BranchTarget, Extension, Identifier, Invocation, Statement, StatementId,
    TemplateArg, Type,
};

fn as_id(id: &str) -> Identifier {
    Identifier::Name(id.into())
}

#[test]
fn display_type() {
    assert_eq!(Type { id: as_id("type"), args: vec![] }.to_string(), "type");
    assert_eq!(
        Type {
            id: as_id("type"),
            args: vec![TemplateArg::Type(Type { id: as_id("arg"), args: vec![] })]
        }
        .to_string(),
        "type<arg>"
    );
    assert_eq!(
        Type {
            id: as_id("type"),
            args: vec![
                TemplateArg::Type(Type { id: as_id("arg1"), args: vec![] }),
                TemplateArg::Value(4)
            ]
        }
        .to_string(),
        "type<arg1, 4>"
    );
}

#[test]
fn display_extension() {
    assert_eq!(Extension { id: as_id("ext"), tmpl_args: vec![] }.to_string(), "ext");
    assert_eq!(
        Extension {
            id: as_id("ext"),
            tmpl_args: vec![TemplateArg::Type(Type { id: as_id("arg"), args: vec![] })]
        }
        .to_string(),
        "ext<arg>"
    );
    assert_eq!(
        Extension {
            id: as_id("ext"),
            tmpl_args: vec![
                TemplateArg::Type(Type { id: as_id("arg1"), args: vec![] }),
                TemplateArg::Value(4)
            ]
        }
        .to_string(),
        "ext<arg1, 4>"
    );
}

#[test]
fn display_statement() {
    assert_eq!(
        Statement::Invocation(Invocation {
            ext: Extension { id: as_id("ext"), tmpl_args: vec![] },
            args: vec![],
            branches: vec![BranchInfo { target: BranchTarget::Fallthrough, results: vec![] }]
        })
        .to_string(),
        "ext() -> ();"
    );
    assert_eq!(
        Statement::Invocation(Invocation {
            ext: Extension { id: as_id("ext"), tmpl_args: vec![] },
            args: vec![as_id("arg1")],
            branches: vec![BranchInfo {
                target: BranchTarget::Fallthrough,
                results: vec![as_id("res1")]
            }]
        })
        .to_string(),
        "ext(arg1) -> (res1);"
    );
    assert_eq!(
        Statement::Invocation(Invocation {
            ext: Extension { id: as_id("ext"), tmpl_args: vec![] },
            args: vec![as_id("arg1"), as_id("arg2")],
            branches: vec![BranchInfo {
                target: BranchTarget::Fallthrough,
                results: vec![as_id("res1"), as_id("res2")]
            }]
        })
        .to_string(),
        "ext(arg1, arg2) -> (res1, res2);"
    );
    assert_eq!(
        Statement::Invocation(Invocation {
            ext: Extension { id: as_id("ext"), tmpl_args: vec![] },
            args: vec![],
            branches: vec![BranchInfo {
                target: BranchTarget::Statement(StatementId(5)),
                results: vec![]
            }]
        })
        .to_string(),
        "ext() { 5() };"
    );
    assert_eq!(
        Statement::Invocation(Invocation {
            ext: Extension { id: as_id("ext"), tmpl_args: vec![] },
            args: vec![as_id("arg1"), as_id("arg2")],
            branches: vec![
                BranchInfo { target: BranchTarget::Fallthrough, results: vec![] },
                BranchInfo {
                    target: BranchTarget::Statement(StatementId(7)),
                    results: vec![as_id("res1")]
                },
                BranchInfo {
                    target: BranchTarget::Statement(StatementId(5)),
                    results: vec![as_id("res1"), as_id("res2")]
                }
            ]
        })
        .to_string(),
        "ext(arg1, arg2) { fallthrough() 7(res1) 5(res1, res2) };"
    );
    assert_eq!(Statement::Return(vec![]).to_string(), "return ();");
    assert_eq!(Statement::Return(vec![as_id("r")]).to_string(), "return (r);");
    assert_eq!(Statement::Return(vec![as_id("r1"), as_id("r2")]).to_string(), "return (r1, r2);");
}
