/// A full Sierra program.
#[derive(Clone, Debug)]
pub struct Program {
    /// Declarations for all the used types.
    pub type_declarations: Vec<TypeDeclaration>,
    /// Declarations for all the used extensions.
    pub extension_declarations: Vec<ExtensionDeclaration>,
    /// The code of the program.
    pub statements: Vec<Statement>,
    /// Descriptions of the functions - signatures and entry points.
    pub funcs: Vec<Function>,
}

/// Declaration of a concrete type.
#[derive(Clone, Debug)]
pub struct TypeDeclaration {
    /// The id of the declared concrete type.
    pub id: ConcreteTypeId,
    /// The id of the used generic type.
    pub generic_id: GenericTypeId,
    /// The arguments for the generic type.
    pub args: Vec<GenericArg>,
}

/// Declaration of a callable extension.
#[derive(Clone, Debug)]
pub struct ExtensionDeclaration {
    /// The id of the declared concrete extension.
    pub id: ConcreteExtensionId,
    /// The id of the used generic extension.
    pub generic_id: GenericExtensionId,
    /// The arguments for the specialization.
    pub args: Vec<GenericArg>,
}

/// Descriptor of a function.
#[derive(Clone, Debug)]
pub struct Function {
    /// The name of the function.
    pub id: FunctionId,
    /// The arguments for the function.
    pub params: Vec<Param>,
    /// The return types.
    pub ret_types: Vec<ConcreteTypeId>,
    /// The statement id where the function starts.
    pub entry: StatementId,
}

/// Descriptor of a variable.
#[derive(Clone, Debug, PartialEq)]
pub struct Param {
    pub id: VarId,
    pub ty: ConcreteTypeId,
}

macro_rules! define_identity {
    ($doc:literal, $derives:tt, $type_name:ident) => {
        #[doc=$doc]
        #[derive $derives]
        pub enum $type_name {
            // This variant is for testing.
            Name(String),
            Numeric(u64),
        }
        impl From<&str> for $type_name {
            fn from(name: &str) -> Self {
                Self::Name(name.into())
            }
        }
        impl From<String> for $type_name {
            fn from(name: String) -> Self {
                Self::Name(name)
            }
        }
        impl From<u64> for $type_name {
            fn from(id: u64) -> Self {
                Self::Numeric(id)
            }
        }
    };
}

define_identity!(
    "The identity of a generic extension",
    (Clone, Debug, Eq, Hash, PartialEq),
    GenericExtensionId
);

define_identity!(
    "The identity of a concrete extension.",
    (Clone, Debug, PartialEq),
    ConcreteExtensionId
);

define_identity!("The identity of a user function.", (Clone, Debug, PartialEq), FunctionId);

define_identity!("The identity of a variable.", (Clone, Debug, Eq, Hash, PartialEq), VarId);

define_identity!("The identity of a generic type.", (Clone, Debug, PartialEq), GenericTypeId);

define_identity!("The identity of a concrete type.", (Clone, Debug, PartialEq), ConcreteTypeId);

/// Represents the index of a statement in the Program::statements vector.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct StatementId(pub usize);

/// Possible arguments for generic type.
#[derive(Clone, Debug, PartialEq)]
pub enum GenericArg {
    Type(ConcreteTypeId),
    Func(FunctionId),
    Value(i64),
}

/// A possible statement.
#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    Invocation(Invocation),
    Return(Vec<VarId>),
}

/// An invocation statement.
#[derive(Clone, Debug, PartialEq)]
pub struct Invocation {
    /// The called extension.
    pub extension_id: ConcreteExtensionId,
    /// The arguments consumed by the extension's invocation.
    pub args: Vec<VarId>,
    /// The possible branches to continue to after the invocation.
    /// The extension would continue to exactly one of the branches.
    pub branches: Vec<BranchInfo>,
}

/// Describes the flow of a chosen extension's branch.
#[derive(Clone, Debug, PartialEq)]
pub struct BranchInfo {
    /// The target the branch continues the run through.
    pub target: BranchTarget,
    /// The resulting identifiers from the extension call.
    pub results: Vec<VarId>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum BranchTarget {
    /// Continues a run to the next statement.
    Fallthrough,
    /// Continues the run to provided statement.
    Statement(StatementId),
}
