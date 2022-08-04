use std::fmt;

use crate::program::{
    BranchInfo, BranchTarget, CalleeId, ConcreteType, ConcreteTypeId, ExtensionId,
    ExtensionSpecialization, Function, Invocation, Program, Statement, TemplateArg, TypeId,
    TypeSpecialization, TypedVar, VarId,
};

#[cfg(test)]
#[path = "fmt_test.rs"]
mod tests;

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for specialization in &self.type_specializations {
            writeln!(f, "{};", specialization)?;
        }
        writeln!(f)?;
        for specialization in &self.extension_specializations {
            writeln!(f, "{};", specialization)?;
        }
        writeln!(f)?;
        for statement in &self.statements {
            writeln!(f, "{};", statement)?;
        }
        writeln!(f)?;
        for func in &self.funcs {
            writeln!(f, "{};", func)?;
        }
        Ok(())
    }
}

impl fmt::Display for TypeSpecialization {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "type {} = {}", self.id, self.ty)
    }
}

impl fmt::Display for ExtensionSpecialization {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ext {} = {}", self.id, self.extension_id)?;
        write_template_args(f, &self.args)
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}@{}(", self.id, self.entry.0)?;
        write_comma_separated(f, &self.args)?;
        write!(f, ") -> (")?;
        write_comma_separated(f, &self.ret_types)?;
        write!(f, ")")
    }
}

impl fmt::Display for TypedVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.id, self.ty)
    }
}

impl fmt::Display for ConcreteType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.id)?;
        write_template_args(f, &self.args)
    }
}

macro_rules! display_identity {
    ($type_name:tt) => {
        impl fmt::Display for $type_name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match &self {
                    Self::Name(name) => write!(f, "{}", name),
                    Self::Numeric(id) => write!(f, "[{}]", id),
                }
            }
        }
    };
}

display_identity!(ExtensionId);
display_identity!(CalleeId);
display_identity!(VarId);
display_identity!(TypeId);
display_identity!(ConcreteTypeId);

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
            Statement::Invocation(invc) => write!(f, "{}", invc),
            Statement::Return(ids) => {
                write!(f, "return (")?;
                write_comma_separated(f, ids)?;
                write!(f, ")")
            }
        }
    }
}

impl fmt::Display for Invocation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}(", self.callee_id)?;
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
