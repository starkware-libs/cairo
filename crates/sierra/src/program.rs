use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

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
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Param {
    pub id: VarId,
    pub ty: ConcreteTypeId,
}

fn id_from_string(s: &str) -> u64 {
    // TODO(ilya, 10/10/2022): Fix https://github.com/starkware-libs/cairo2/issues/45.
    let mut hasher = DefaultHasher::new();
    s.hash(&mut hasher);
    hasher.finish()
}

macro_rules! define_identity {
    ($doc:literal, $derives:tt, $type_name:ident) => {
        #[doc=$doc]
        #[derive $derives]

        pub struct $type_name {
            pub id: u64,
            /// Optional name for testing and debugging.
            pub debug_name: Option<String>,
        }

        impl $type_name {
            pub fn new(id: u64) -> Self {
                $type_name{id, debug_name: None}
            }

            pub fn from_string(name: impl Into<String>) -> Self {
                let s: String = name.into();
                $type_name{id: id_from_string(&s), debug_name: Some(s)}
            }
        }
        impl From<&str> for $type_name {
            fn from(name: &str) -> Self {
                Self::from_string(name.to_string())
            }
        }
        impl From<String> for $type_name {
            fn from(name: String) -> Self {
                Self::from_string(name)
            }
        }
        impl From<u64> for $type_name {
            fn from(id: u64) -> Self {
                Self::new(id)
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
    (Clone, Debug, Eq, PartialEq),
    ConcreteExtensionId
);

define_identity!("The identity of a user function.", (Clone, Debug, Eq, PartialEq), FunctionId);

define_identity!("The identity of a variable.", (Clone, Debug, Eq, Hash, PartialEq), VarId);

define_identity!("The identity of a generic type.", (Clone, Debug, Eq, PartialEq), GenericTypeId);

define_identity!("The identity of a concrete type.", (Clone, Debug, Eq, PartialEq), ConcreteTypeId);

/// Represents the index of a statement in the Program::statements vector.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct StatementId(pub usize);

/// Possible arguments for generic type.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum GenericArg {
    Type(ConcreteTypeId),
    Func(FunctionId),
    Value(i64),
}

/// A possible statement.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Statement {
    Invocation(Invocation),
    Return(Vec<VarId>),
}

/// An invocation statement.
#[derive(Clone, Debug, Eq, PartialEq)]
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
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct BranchInfo {
    /// The target the branch continues the run through.
    pub target: BranchTarget,
    /// The resulting identifiers from the extension call.
    pub results: Vec<VarId>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum BranchTarget {
    /// Continues a run to the next statement.
    Fallthrough,
    /// Continues the run to provided statement.
    Statement(StatementId),
}
