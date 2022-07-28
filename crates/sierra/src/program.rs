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
    pub ret_types: Vec<Type>,
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

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct StatementId(pub usize);

// Possible arguments for templatic type.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum TemplateArg {
    Type(Type),
    Value(i64),
}

// A possible statement.
#[derive(Clone, Debug)]
pub enum Statement {
    Invocation(Invocation),
    Return(Vec<Identifier>),
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

// Describes an extension.
#[derive(Clone, Debug)]
pub struct Extension {
    pub name: String,
    pub tmpl_args: Vec<TemplateArg>,
}

// Describes the flow of a chosen extension's branch.
#[derive(Clone, Debug)]
pub struct BranchInfo {
    // The target the branch continues the run through.
    pub target: BranchTarget,
    // The resulting identifiers from the extension call.
    pub results: Vec<Identifier>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum BranchTarget {
    // Continues a run to the next statement.
    Fallthrough,
    // Continues the run to provided statement.
    Statement(StatementId),
}
