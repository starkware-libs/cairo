use std::fmt;

use utils::write_comma_separated;

use crate::ids::{
    ConcreteLibFuncId, ConcreteTypeId, FunctionId, GenericLibFuncId, GenericTypeId, UserTypeId,
    VarId,
};
use crate::program::{
    ConcreteLibFuncLongId, ConcreteTypeLongId, Function, GenBranchInfo, GenBranchTarget,
    GenInvocation, GenStatement, GenericArg, LibFuncDeclaration, Param, Program, StatementIdx,
    TypeDeclaration,
};

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for declaration in &self.type_declarations {
            writeln!(f, "{declaration};")?;
        }
        writeln!(f)?;
        for declaration in &self.libfunc_declarations {
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
        write!(f, "type {} = {}", self.id, self.long_id)
    }
}

impl fmt::Display for ConcreteTypeLongId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.generic_id)?;
        write_template_args(f, &self.generic_args)
    }
}

impl fmt::Display for LibFuncDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "libfunc {} = {}", self.id, self.long_id)
    }
}

impl fmt::Display for ConcreteLibFuncLongId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.generic_id)?;
        write_template_args(f, &self.generic_args)
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}@{}(", self.id, self.entry_point.0)?;
        write_comma_separated(f, &self.params)?;
        write!(f, ") -> (")?;
        write_comma_separated(f, &self.signature.ret_types)?;
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

display_identity!(GenericLibFuncId);
display_identity!(ConcreteLibFuncId);
display_identity!(FunctionId);
display_identity!(UserTypeId);
display_identity!(VarId);
display_identity!(GenericTypeId);
display_identity!(ConcreteTypeId);

impl fmt::Display for GenericArg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            GenericArg::Type(id) => write!(f, "{id}"),
            GenericArg::UserType(id) => write!(f, "ut@{id}"),
            GenericArg::Value(v) => write!(f, "{v}"),
            GenericArg::UserFunc(id) => write!(f, "user@{id}"),
            GenericArg::LibFunc(id) => write!(f, "lib@{id}"),
        }
    }
}

impl<StatementId: fmt::Display> fmt::Display for GenStatement<StatementId> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            GenStatement::Invocation(invocation) => write!(f, "{invocation}"),
            GenStatement::Return(ids) => {
                write!(f, "return(")?;
                write_comma_separated(f, ids)?;
                write!(f, ")")
            }
        }
    }
}

impl<StatementId: fmt::Display> fmt::Display for GenInvocation<StatementId> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}(", self.libfunc_id)?;
        write_comma_separated(f, &self.args)?;
        if let [GenBranchInfo { target: GenBranchTarget::Fallthrough, results }] =
            &self.branches[..]
        {
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

impl<StatementId: fmt::Display> fmt::Display for GenBranchInfo<StatementId> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}(", self.target)?;
        write_comma_separated(f, &self.results)?;
        write!(f, ")")
    }
}

impl<StatementId: fmt::Display> fmt::Display for GenBranchTarget<StatementId> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            GenBranchTarget::Fallthrough => write!(f, "fallthrough"),
            GenBranchTarget::Statement(id) => write!(f, "{}", id),
        }
    }
}

impl fmt::Display for StatementIdx {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

fn write_template_args(f: &mut fmt::Formatter<'_>, args: &[GenericArg]) -> fmt::Result {
    if args.is_empty() {
        Ok(())
    } else {
        write!(f, "<")?;
        write_comma_separated(f, args)?;
        write!(f, ">")
    }
}
