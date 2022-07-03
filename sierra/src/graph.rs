use std::fmt;

#[derive(Clone, Debug)]
pub struct Program {
    pub funcs: Vec<Function>,
}

#[derive(Clone, Debug)]
pub struct LibCall {
    pub name: String,
    pub tmpl_args: Vec<TemplateArg>,
}

impl fmt::Display for LibCall {
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

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Type {
    Basic(String),
    Template(String, Vec<TemplateArg>),
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Basic(name) => write!(f, "{}", name),
            Type::Template(name, tmpl_args) => {
                write!(f, "{}<", name)?;
                tmpl_args
                    .iter()
                    .take(1)
                    .try_for_each(|ta| write!(f, "{}", ta))?;
                tmpl_args
                    .iter()
                    .skip(1)
                    .try_for_each(|ta| write!(f, ", {}", ta))?;
                write!(f, ">")
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct Function {
    pub name: String,
    pub args: Vec<TypedVar>,
    pub res_types: Vec<Type>,
    pub blocks: Block,
    pub entry: u64,
    pub cost: i64,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedVar {
    pub name: String,
    pub ty: Type,
}

#[derive(Clone, Debug)]
pub struct Block {
    pub invocations: Vec<Invocation>,
    pub exit: BlockExit,
    pub cost: i64,
}

#[derive(Clone, Debug)]
pub struct Invocation {
    pub libcall: LibCall,
    pub args: Vec<String>,
    pub results: Vec<String>,
}

impl fmt::Display for Invocation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}(", self.libcall)?;
        self.args
            .iter()
            .take(1)
            .try_for_each(|n| write!(f, "{}", n))?;
        self.args
            .iter()
            .skip(1)
            .try_for_each(|n| write!(f, ", {}", n))?;
        write!(f, ") -> (")?;
        self.results
            .iter()
            .take(1)
            .try_for_each(|n| write!(f, "{}", n))?;
        self.results
            .iter()
            .skip(1)
            .try_for_each(|n| write!(f, ", {}", n))?;
        write!(f, ")")
    }
}

#[derive(Clone, Debug)]
pub enum BlockExit {
    Return(Vec<String>),
    Jump(JumpInfo),
    Continue,
}

#[derive(Clone, Debug)]
pub struct JumpInfo {
    pub libcall: LibCall,
    pub args: Vec<String>,
    pub branches: Vec<BranchInfo>,
}

impl fmt::Display for JumpInfo {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}(", self.libcall)?;
        self.args
            .iter()
            .take(1)
            .try_for_each(|n| write!(f, "{}", n))?;
        self.args
            .iter()
            .skip(1)
            .try_for_each(|n| write!(f, ", {}", n))?;
        writeln!(f, ") {{")?;
        self.branches
            .iter()
            .try_for_each(|b| writeln!(f, "{},", b))?;
        write!(f, "}}")
    }
}

#[derive(Clone, Debug)]
pub struct BranchInfo {
    pub block: u64,
    pub exports: Vec<String>,
}

impl fmt::Display for BranchInfo {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}(", self.block)?;
        self.exports
            .iter()
            .take(1)
            .try_for_each(|n| write!(f, "{}", n))?;
        self.exports
            .iter()
            .skip(1)
            .try_for_each(|n| write!(f, ", {}", n))?;
        writeln!(f, ")")
    }
}
