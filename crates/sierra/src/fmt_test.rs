use crate::program::{
    BranchInfo, BranchTarget, CalleeId, ConcreteType, ConcreteTypeId, ExtensionId,
    ExtensionSpecialization, Function, Invocation, Statement, StatementId, TemplateArg, TypeId,
    TypeSpecialization, TypedVar, VarId,
};

#[test]
fn display_type_specialization() {
    let as_id = |id: &str| TypeId::Name(id.into());
    assert_eq!(
        TypeSpecialization {
            ty: ConcreteType { id: as_id("TypeId"), args: vec![] },
            id: ConcreteTypeId::Name("ConcreteTypeId".into())
        }
        .to_string(),
        "type ConcreteTypeId = TypeId"
    );
    assert_eq!(
        TypeSpecialization {
            ty: ConcreteType {
                id: as_id("TypeId"),
                args: vec![TemplateArg::Type(ConcreteTypeId::Name("arg".into()))]
            },
            id: ConcreteTypeId::Name("ConcreteTypeId".into())
        }
        .to_string(),
        "type ConcreteTypeId = TypeId<arg>"
    );
    assert_eq!(
        TypeSpecialization {
            ty: ConcreteType {
                id: as_id("TypeId"),
                args: vec![
                    TemplateArg::Type(ConcreteTypeId::Name("arg1".into())),
                    TemplateArg::Value(4)
                ]
            },
            id: ConcreteTypeId::Name("ConcreteTypeId".into())
        }
        .to_string(),
        "type ConcreteTypeId = TypeId<arg1, 4>"
    );
}

#[test]
fn display_extension_specialization() {
    assert_eq!(
        ExtensionSpecialization {
            extension_id: ExtensionId::Name("ExtensionId".into()),
            args: vec![],
            id: CalleeId::Name("CalleeId".into())
        }
        .to_string(),
        "ext CalleeId = ExtensionId"
    );
    assert_eq!(
        ExtensionSpecialization {
            extension_id: ExtensionId::Name("ExtensionId".into()),
            args: vec![
                TemplateArg::Type(ConcreteTypeId::Name("arg".into())),
                TemplateArg::Value(4)
            ],
            id: CalleeId::Name("OtherCalleeId".into())
        }
        .to_string(),
        "ext OtherCalleeId = ExtensionId<arg, 4>"
    );
}

#[test]
fn display_function() {
    assert_eq!(
        Function {
            id: CalleeId::Name("Name".into()),
            args: vec![],
            ret_types: vec![],
            entry: StatementId(5),
        }
        .to_string(),
        "Name@5() -> ()"
    );
    assert_eq!(
        Function {
            id: CalleeId::Name("Other".into()),
            args: vec![TypedVar { id: VarId::Numeric(5), ty: ConcreteTypeId::Name("T1".into()) }],
            ret_types: vec![ConcreteTypeId::Name("T2".into())],
            entry: StatementId(3),
        }
        .to_string(),
        "Other@3([5]: T1) -> (T2)"
    );
}

#[test]
fn display_statement() {
    let as_id = |id: &str| VarId::Name(id.into());
    assert_eq!(
        Statement::Invocation(Invocation {
            callee_id: CalleeId::Name("callee".into()),
            args: vec![],
            branches: vec![BranchInfo { target: BranchTarget::Fallthrough, results: vec![] }]
        })
        .to_string(),
        "callee() -> ()"
    );
    assert_eq!(
        Statement::Invocation(Invocation {
            callee_id: CalleeId::Name("callee".into()),
            args: vec![as_id("arg1")],
            branches: vec![BranchInfo {
                target: BranchTarget::Fallthrough,
                results: vec![as_id("res1")]
            }]
        })
        .to_string(),
        "callee(arg1) -> (res1)"
    );
    assert_eq!(
        Statement::Invocation(Invocation {
            callee_id: CalleeId::Name("callee".into()),
            args: vec![as_id("arg1"), as_id("arg2")],
            branches: vec![BranchInfo {
                target: BranchTarget::Fallthrough,
                results: vec![as_id("res1"), as_id("res2")]
            }]
        })
        .to_string(),
        "callee(arg1, arg2) -> (res1, res2)"
    );
    assert_eq!(
        Statement::Invocation(Invocation {
            callee_id: CalleeId::Name("callee".into()),
            args: vec![],
            branches: vec![BranchInfo {
                target: BranchTarget::Statement(StatementId(5)),
                results: vec![]
            }]
        })
        .to_string(),
        "callee() { 5() }"
    );
    assert_eq!(
        Statement::Invocation(Invocation {
            callee_id: CalleeId::Name("callee".into()),
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
        "callee(arg1, arg2) { fallthrough() 7(res1) 5(res1, res2) }"
    );
    assert_eq!(
        Statement::Invocation(Invocation {
            callee_id: CalleeId::Numeric(12345),
            args: vec![VarId::Numeric(12)],
            branches: vec![
                BranchInfo {
                    target: BranchTarget::Statement(StatementId(2)),
                    results: vec![VarId::Numeric(37)]
                },
                BranchInfo { target: BranchTarget::Fallthrough, results: vec![] }
            ]
        })
        .to_string(),
        "[12345]([12]) { 2([37]) fallthrough() }"
    );
    assert_eq!(Statement::Return(vec![]).to_string(), "return ()");
    assert_eq!(Statement::Return(vec![as_id("r")]).to_string(), "return (r)");
    assert_eq!(Statement::Return(vec![as_id("r1"), as_id("r2")]).to_string(), "return (r1, r2)");
}
