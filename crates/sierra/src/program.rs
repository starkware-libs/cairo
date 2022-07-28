use std::fmt;

// A full sierra program.
#[derive(Clone, Debug)]
pub struct Program {
    // All code of the program.
    pub stmts: Vec<Statement>,
    // Descriptions of the functions - signature and entry point.
    pub funcs: Vec<Function>,
}

// Descriptor of a function.
#[derive(Clone, Debug)]
pub struct Function {
    // The name of the function.
    pub id: Identifier,
    // The arguments for the function.
    pub args: Vec<TypedVar>,
    // The return types.
    pub res_types: Vec<Type>,
    // The statement id where the function starts.
    pub entry: StatementId,
}

// Descriptor of a variable.
#[derive(Clone, Debug, PartialEq)]
pub struct TypedVar {
    pub id: Identifier,
    pub ty: Type,
}

// A general identifier - mostly used to identify vars and functions.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Identifier(pub String);

// A generic type.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Type {
    pub name: String,
    pub args: Vec<TemplateArg>,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)?;
        let mut iter = self.args.iter();
        if let Some(ta) = iter.next() {
            write!(f, "<{}", ta)?;
            for ta in iter {
                write!(f, ", {}", ta)?;
            }
            write!(f, ">")?;
        }
        Ok(())
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct StatementId(pub usize);

// Possible arguments for templatic type.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum TemplateArg {
    Type(Type),
    Value(i64),
}

impl fmt::Display for TemplateArg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TemplateArg::Type(t) => write!(f, "{}", t),
            TemplateArg::Value(v) => write!(f, "{}", v),
        }
    }
}

// A possible statement.
#[derive(Clone, Debug)]
pub enum Statement {
    Invocation(Invocation),
    Return(Vec<Identifier>),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::Invocation(invc) => writeln!(f, "{};", invc),
            Statement::Return(ids) => {
                write!(f, "return (")?;
                write_comma_separated(f, ids)?;
                writeln!(f, ");")
            }
        }
    }
}

// An invocation statement.
#[derive(Clone, Debug)]
pub struct Invocation {
    // The called extension.
    pub ext: Extension,
    // The argumenst consumed by the extension invocation.
    pub args: Vec<Identifier>,
    // The possibly called branches by the invocation.
    pub branches: Vec<BranchInfo>,
}

impl fmt::Display for Invocation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}(", self.ext)?;
        write_comma_separated(f, &self.args)?;
        if self.branches.len() == 1 && self.branches[0].target == BranchTarget::Fallthrough {
            write!(f, ") -> (")?;
            write_comma_separated(f, &self.branches[0].results)?;
            write!(f, ")")
        } else {
            write!(f, ") {{")?;
            self.branches.iter().try_for_each(|b| write!(f, "{}", b))?;
            write!(f, "}}")
        }
    }
}

// Describes an extension.
#[derive(Clone, Debug)]
pub struct Extension {
    pub name: String,
    pub tmpl_args: Vec<TemplateArg>,
}

impl fmt::Display for Extension {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)?;
        let mut iter = self.tmpl_args.iter();
        if let Some(ta) = iter.next() {
            write!(f, "<{}", ta)?;
            for ta in iter {
                write!(f, ", {}", ta)?;
            }
            write!(f, ">")?;
        }
        Ok(())
    }
}

// Describes the flow of a chosen extension's branch.
#[derive(Clone, Debug)]
pub struct BranchInfo {
    // The target the branch continues the run through.
    pub target: BranchTarget,
    // The resulting identifiers from the extension call.
    pub results: Vec<Identifier>,
}

impl fmt::Display for BranchInfo {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}(", self.target)?;
        write_comma_separated(f, &self.results)?;
        write!(f, ")")
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum BranchTarget {
    // Continues a run to the next statement.
    Fallthrough,
    // Continues the run to provided statement.
    Statement(StatementId),
}

impl fmt::Display for BranchTarget {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BranchTarget::Fallthrough => write!(f, "fallthrough"),
            BranchTarget::Statement(s_id) => write!(f, "{}", s_id.0),
        }
    }
}

fn write_comma_separated(f: &mut fmt::Formatter, ids: &Vec<Identifier>) -> fmt::Result {
    ids.iter().take(1).try_for_each(|n| write!(f, "{}", n.0))?;
    ids.iter().skip(1).try_for_each(|n| write!(f, ", {}", n.0))
}
