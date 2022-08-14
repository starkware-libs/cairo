use std::fmt;

use crate::ids::{
    ConcreteExtensionId, ConcreteTypeId, FunctionId, GenericExtensionId, GenericTypeId, VarId,
};
use crate::program::{
    BranchInfo, BranchTarget, ExtensionDeclaration, Function, GenericArg, Invocation, Param,
    Program, Statement, TypeDeclaration,
};

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for declaration in &self.type_declarations {
            writeln!(f, "{declaration};")?;
        }
        writeln!(f)?;
        for declaration in &self.extension_declarations {
            writeln!(f, "{declaration};")?;
        }
        writeln!(f)?;
        for statement in &self.statements {
            writeln!(f, "{statement};")?;
        }
        writeln!(f)?;
        for func in &self.funcs {
            writeln!(f, "{func};")?;
        }
        Ok(())
    }
}

impl fmt::Display for TypeDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "type {} = {}", self.id, self.generic_id)?;
        write_template_args(f, &self.args)
    }
}

impl fmt::Display for ExtensionDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ext {} = {}", self.id, self.generic_id)?;
        write_template_args(f, &self.args)
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}@{}(", self.id, self.entry.0)?;
        write_comma_separated(f, &self.params)?;
        write!(f, ") -> (")?;
        write_comma_separated(f, &self.ret_types)?;
        write!(f, ")")
    }
}

impl fmt::Display for Param {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.id, self.ty)
    }
}

macro_rules! display_identity {
    ($type_name:tt) => {
        impl fmt::Display for $type_name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match &self.debug_name {
                    Some(name) => write!(f, "{name}"),
                    None => write!(f, "[{}]", self.id),
                }
            }
        }
    };
}

display_identity!(GenericExtensionId);
display_identity!(ConcreteExtensionId);
display_identity!(FunctionId);
display_identity!(VarId);
display_identity!(GenericTypeId);
display_identity!(ConcreteTypeId);

impl fmt::Display for GenericArg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            GenericArg::Type(id) => write!(f, "{id}"),
            GenericArg::Func(id) => write!(f, "&{id}"),
            GenericArg::Value(v) => write!(f, "{v}"),
        }
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Invocation(invocation) => write!(f, "{invocation}"),
            Statement::Return(ids) => {
                write!(f, "return(")?;
                write_comma_separated(f, ids)?;
                write!(f, ")")
            }
        }
    }
}

impl fmt::Display for Invocation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}(", self.extension_id)?;
        write_comma_separated(f, &self.args)?;
        if let [BranchInfo { target: BranchTarget::Fallthrough, results }] = &self.branches[..] {
            write!(f, ") -> (")?;
            write_comma_separated(f, results)?;
            write!(f, ")")
        } else {
            write!(f, ") {{ ")?;
            self.branches.iter().try_for_each(|branch_info| write!(f, "{branch_info} "))?;
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
            BranchTarget::Statement(id) => write!(f, "{}", id.0),
        }
    }
}

fn write_template_args(f: &mut fmt::Formatter<'_>, args: &Vec<GenericArg>) -> fmt::Result {
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
    values.iter().take(1).try_for_each(|v| write!(f, "{v}"))?;
    values.iter().skip(1).try_for_each(|v| write!(f, ", {v}"))
}
