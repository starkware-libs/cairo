use std::fmt;

use crate::program::{
    BranchInfo, BranchTarget, Extension, Identifier, Invocation, Statement, TemplateArg, Type,
};

#[cfg(test)]
#[path = "fmt_test.rs"]
mod tests;

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
        match &self {
            Self::Name(name) => write!(f, "{}", name),
        }
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
