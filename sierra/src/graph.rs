use std::fmt;

#[derive(Clone, Debug)]
pub struct Program {
    pub blocks: Vec<Block>,
    pub funcs: Vec<Function>,
}

#[derive(Clone, Debug)]
pub struct Function {
    pub name: String,
    pub args: Vec<TypedVar>,
    pub res_types: Vec<Type>,
    pub entry: BlockId,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedVar {
    pub id: Identifier,
    pub ty: Type,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Identifier(pub String);

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
pub struct BlockId(pub usize);

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

#[derive(Clone, Debug)]
pub struct Block {
    pub invocations: Vec<Invocation>,
    pub exit: BlockExit,
}

#[derive(Clone, Debug)]
pub struct Invocation {
    pub ext: Extension,
    pub args: Vec<Identifier>,
    pub results: Vec<Identifier>,
}

impl fmt::Display for Invocation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}(", self.ext)?;
        self.args
            .iter()
            .take(1)
            .try_for_each(|n| write!(f, "{}", n.0))?;
        self.args
            .iter()
            .skip(1)
            .try_for_each(|n| write!(f, ", {}", n.0))?;
        write!(f, ") -> (")?;
        self.results
            .iter()
            .take(1)
            .try_for_each(|n| write!(f, "{}", n.0))?;
        self.results
            .iter()
            .skip(1)
            .try_for_each(|n| write!(f, ", {}", n.0))?;
        write!(f, ")")
    }
}

#[derive(Clone, Debug)]
pub enum BlockExit {
    Return(Vec<Identifier>),
    Jump(JumpInfo),
}

#[derive(Clone, Debug)]
pub struct JumpInfo {
    pub ext: Extension,
    pub args: Vec<Identifier>,
    pub branches: Vec<BranchInfo>,
}

impl fmt::Display for JumpInfo {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}(", self.ext)?;
        self.args
            .iter()
            .take(1)
            .try_for_each(|n| write!(f, "{}", n.0))?;
        self.args
            .iter()
            .skip(1)
            .try_for_each(|n| write!(f, ", {}", n.0))?;
        writeln!(f, ") {{")?;
        self.branches
            .iter()
            .try_for_each(|b| writeln!(f, "{},", b))?;
        write!(f, "}}")
    }
}

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

#[derive(Clone, Debug)]
pub struct BranchInfo {
    pub block: BlockId,
    pub exports: Vec<Identifier>,
}

impl fmt::Display for BranchInfo {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}(", self.block.0)?;
        self.exports
            .iter()
            .take(1)
            .try_for_each(|n| write!(f, "{}", n.0))?;
        self.exports
            .iter()
            .skip(1)
            .try_for_each(|n| write!(f, ", {}", n.0))?;
        writeln!(f, ")")
    }
}
