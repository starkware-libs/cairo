use std::fmt;

use crate::program::{
    BranchInfo, BranchTarget, Extension, Identifier, Invocation, Statement, TemplateArg, Type,
};

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)?;
        write_template_args(f, &self.args)
    }
}

impl fmt::Display for TemplateArg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TemplateArg::Type(t) => write!(f, "{}", t),
            TemplateArg::Value(v) => write!(f, "{}", v),
        }
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::Invocation(invc) => write!(f, "{};", invc),
            Statement::Return(ids) => {
                write!(f, "return (")?;
                write_comma_separated(f, ids)?;
                write!(f, ");")
            }
        }
    }
}

impl fmt::Display for Invocation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}(", self.ext)?;
        write_comma_separated(f, &self.args)?;
        match &self.branches[..] {
            [BranchInfo { target: BranchTarget::Fallthrough, results }] => {
                write!(f, ") -> (")?;
                write_comma_separated(f, results)?;
                write!(f, ")")
            }
            brs => {
                write!(f, ") {{ ")?;
                brs.iter().try_for_each(|b| write!(f, "{} ", b))?;
                write!(f, "}}")
            }
        }
    }
}

impl fmt::Display for Extension {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)?;
        write_template_args(f, &self.tmpl_args)
    }
}

impl fmt::Display for BranchInfo {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}(", self.target)?;
        write_comma_separated(f, &self.results)?;
        write!(f, ")")
    }
}

impl fmt::Display for BranchTarget {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BranchTarget::Fallthrough => write!(f, "fallthrough"),
            BranchTarget::Statement(s_id) => write!(f, "{}", s_id.0),
        }
    }
}

fn write_template_args(f: &mut fmt::Formatter, args: &Vec<TemplateArg>) -> fmt::Result {
    let mut iter = args.iter();
    if let Some(arg) = iter.next() {
        write!(f, "<{}", arg)?;
        for arg in iter {
            write!(f, ", {}", arg)?;
        }
        write!(f, ">")?;
    }
    Ok(())
}

fn write_comma_separated(f: &mut fmt::Formatter, ids: &Vec<Identifier>) -> fmt::Result {
    ids.iter().take(1).try_for_each(|n| write!(f, "{}", n.0))?;
    ids.iter().skip(1).try_for_each(|n| write!(f, ", {}", n.0))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::program::StatementId;
    #[test]
    fn display_type() {
        assert_eq!(Type { name: "type".to_string(), args: vec![] }.to_string(), "type".to_string());
        assert_eq!(
            Type {
                name: "type".to_string(),
                args: vec![TemplateArg::Type(Type { name: "arg".to_string(), args: vec![] })]
            }
            .to_string(),
            "type<arg>".to_string()
        );
        assert_eq!(
            Type {
                name: "type".to_string(),
                args: vec![
                    TemplateArg::Type(Type { name: "arg1".to_string(), args: vec![] }),
                    TemplateArg::Value(4)
                ]
            }
            .to_string(),
            "type<arg1, 4>".to_string()
        );
    }

    #[test]
    fn display_extension() {
        assert_eq!(
            Extension { name: "ext".to_string(), tmpl_args: vec![] }.to_string(),
            "ext".to_string()
        );
        assert_eq!(
            Extension {
                name: "ext".to_string(),
                tmpl_args: vec![TemplateArg::Type(Type { name: "arg".to_string(), args: vec![] })]
            }
            .to_string(),
            "ext<arg>".to_string()
        );
        assert_eq!(
            Extension {
                name: "ext".to_string(),
                tmpl_args: vec![
                    TemplateArg::Type(Type { name: "arg1".to_string(), args: vec![] }),
                    TemplateArg::Value(4)
                ]
            }
            .to_string(),
            "ext<arg1, 4>".to_string()
        );
    }

    #[test]
    fn display_statement() {
        assert_eq!(
            Statement::Invocation(Invocation {
                ext: Extension { name: "ext".to_string(), tmpl_args: vec![] },
                args: vec![],
                branches: vec![BranchInfo { target: BranchTarget::Fallthrough, results: vec![] }]
            })
            .to_string(),
            "ext() -> ();".to_string()
        );
        assert_eq!(
            Statement::Invocation(Invocation {
                ext: Extension { name: "ext".to_string(), tmpl_args: vec![] },
                args: vec![Identifier("arg1".to_string())],
                branches: vec![BranchInfo {
                    target: BranchTarget::Fallthrough,
                    results: vec![Identifier("res1".to_string())]
                }]
            })
            .to_string(),
            "ext(arg1) -> (res1);".to_string()
        );
        assert_eq!(
            Statement::Invocation(Invocation {
                ext: Extension { name: "ext".to_string(), tmpl_args: vec![] },
                args: vec![Identifier("arg1".to_string()), Identifier("arg2".to_string())],
                branches: vec![BranchInfo {
                    target: BranchTarget::Fallthrough,
                    results: vec![Identifier("res1".to_string()), Identifier("res2".to_string())]
                }]
            })
            .to_string(),
            "ext(arg1, arg2) -> (res1, res2);".to_string()
        );
        assert_eq!(
            Statement::Invocation(Invocation {
                ext: Extension { name: "ext".to_string(), tmpl_args: vec![] },
                args: vec![],
                branches: vec![BranchInfo {
                    target: BranchTarget::Statement(StatementId(5)),
                    results: vec![]
                }]
            })
            .to_string(),
            "ext() { 5() };".to_string()
        );
        assert_eq!(
            Statement::Invocation(Invocation {
                ext: Extension { name: "ext".to_string(), tmpl_args: vec![] },
                args: vec![Identifier("arg1".to_string()), Identifier("arg2".to_string())],
                branches: vec![
                    BranchInfo { target: BranchTarget::Fallthrough, results: vec![] },
                    BranchInfo {
                        target: BranchTarget::Statement(StatementId(7)),
                        results: vec![Identifier("res1".to_string())]
                    },
                    BranchInfo {
                        target: BranchTarget::Statement(StatementId(5)),
                        results: vec![
                            Identifier("res1".to_string()),
                            Identifier("res2".to_string())
                        ]
                    }
                ]
            })
            .to_string(),
            "ext(arg1, arg2) { fallthrough() 7(res1) 5(res1, res2) };".to_string()
        );
    }
}
