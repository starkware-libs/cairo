use std::fmt;

use crate::program::{
    BranchInfo, BranchTarget, Extension, Identifier, Invocation, Statement, TemplateArg, Type,
};

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.id)?;
        write_template_args(f, &self.args)
    }
}

impl fmt::Display for TemplateArg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TemplateArg::Type(t) => write!(f, "{}", t),
            TemplateArg::Value(v) => write!(f, "{}", v),
        }
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}(", self.ext)?;
        write_comma_separated(f, &self.args)?;
        if let [BranchInfo { target: BranchTarget::Fallthrough, results }] = &self.branches[..] {
            write!(f, ") -> (")?;
            write_comma_separated(f, results)?;
            write!(f, ")")
        } else {
            write!(f, ") {{ ")?;
            self.branches.iter().try_for_each(|b| write!(f, "{} ", b))?;
            write!(f, "}}")
        }
    }
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl fmt::Display for Extension {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.id)?;
        write_template_args(f, &self.tmpl_args)
    }
}

impl fmt::Display for BranchInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}(", self.target)?;
        write_comma_separated(f, &self.results)?;
        write!(f, ")")
    }
}

impl fmt::Display for BranchTarget {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BranchTarget::Fallthrough => write!(f, "fallthrough"),
            BranchTarget::Statement(s_id) => write!(f, "{}", s_id.0),
        }
    }
}

fn write_template_args(f: &mut fmt::Formatter<'_>, args: &Vec<TemplateArg>) -> fmt::Result {
    if args.is_empty() {
        Ok(())
    } else {
        write!(f, "<")?;
        write_comma_separated(f, args)?;
        write!(f, ">")
    }
}

fn write_comma_separated<V: std::fmt::Display>(
    f: &mut fmt::Formatter<'_>,
    values: &[V],
) -> fmt::Result {
    values.iter().take(1).try_for_each(|v| write!(f, "{}", v))?;
    values.iter().skip(1).try_for_each(|v| write!(f, ", {}", v))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::program::StatementId;
    fn as_id(id: &str) -> Identifier {
        Identifier(id.into())
    }
    #[test]
    fn display_type() {
        assert_eq!(Type { id: as_id("type"), args: vec![] }.to_string(), "type".to_string());
        assert_eq!(
            Type {
                id: as_id("type"),
                args: vec![TemplateArg::Type(Type { id: as_id("arg"), args: vec![] })]
            }
            .to_string(),
            "type<arg>".to_string()
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
            "type<arg1, 4>".to_string()
        );
    }

    #[test]
    fn display_extension() {
        assert_eq!(
            Extension { id: as_id("ext"), tmpl_args: vec![] }.to_string(),
            "ext".to_string()
        );
        assert_eq!(
            Extension {
                id: as_id("ext"),
                tmpl_args: vec![TemplateArg::Type(Type { id: as_id("arg"), args: vec![] })]
            }
            .to_string(),
            "ext<arg>".to_string()
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
            "ext<arg1, 4>".to_string()
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
            "ext() -> ();".to_string()
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
            "ext(arg1) -> (res1);".to_string()
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
            "ext(arg1, arg2) -> (res1, res2);".to_string()
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
            "ext() { 5() };".to_string()
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
            "ext(arg1, arg2) { fallthrough() 7(res1) 5(res1, res2) };".to_string()
        );
    }
}
