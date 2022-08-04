/// A full Sierra program.
#[derive(Clone, Debug)]
pub struct Program {
    /// Specializations for all the used types.
    pub type_specializations: Vec<TypeSpecialization>,
    /// Specializations for all the used extensions.
    pub extension_specializations: Vec<ExtensionSpecialization>,
    /// The code of the program.
    pub statements: Vec<Statement>,
    /// Descriptions of the functions - signatures and entry points.
    pub funcs: Vec<Function>,
}

/// Declaration of a specialization.
#[derive(Clone, Debug)]
pub struct TypeSpecialization {
    /// The specialized type.
    pub ty: ConcreteType,
    /// The identification of the specialization.
    pub id: ConcreteTypeId,
}

/// Declaration of a specialization.
#[derive(Clone, Debug)]
pub struct ExtensionSpecialization {
    /// The id of the specialized extension.
    pub extension_id: ExtensionId,
    /// The arguments the specialization.
    pub args: Vec<TemplateArg>,
    /// The identification of the specialization.
    pub id: CalleeId,
}

/// Descriptor of a function.
#[derive(Clone, Debug)]
pub struct Function {
    // The name of the function.
    pub id: CalleeId,
    // The arguments for the function.
    pub args: Vec<TypedVar>,
    // The return types.
    pub ret_types: Vec<ConcreteTypeId>,
    // The statement id where the function starts.
    pub entry: StatementId,
}

/// Descriptor of a variable.
#[derive(Clone, Debug, PartialEq)]
pub struct TypedVar {
    pub id: VarId,
    pub ty: ConcreteTypeId,
}

/// A concrete type.
#[derive(Clone, Debug, PartialEq)]
pub struct ConcreteType {
    pub id: TypeId,
    pub args: Vec<TemplateArg>,
}

macro_rules! define_identity {
    ($doc:literal, $derives:tt, $type_name:ident) => {
        #[doc=$doc]
        #[derive $derives]
        pub enum $type_name {
            // This variant is for testing.
            Name(String),
            Numeric(i64),
        }
    };
}

define_identity!("The identity of an extension", (Clone, Debug), ExtensionId);

define_identity!(
    "The identity for a concrete extension, or a user function.",
    (Clone, Debug),
    CalleeId
);

define_identity!("The identity for a variable.", (Clone, Debug, Eq, Hash, PartialEq), VarId);

define_identity!("The identity for a generic type.", (Clone, Debug, PartialEq), TypeId);

define_identity!("The identity for a concrete type.", (Clone, Debug, PartialEq), ConcreteTypeId);

/// Represents the index of a statement in the Program::statements vector.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct StatementId(pub usize);

/// Possible arguments for templatic type.
#[derive(Clone, Debug, PartialEq)]
pub enum TemplateArg {
    Type(ConcreteTypeId),
    Value(i64),
}

/// A possible statement.
#[derive(Clone, Debug)]
pub enum Statement {
    Invocation(Invocation),
    Return(Vec<VarId>),
}

/// An invocation statement.
#[derive(Clone, Debug)]
pub struct Invocation {
    /// The called extension.
    pub callee_id: CalleeId,
    /// The arguments consumed by the extension's invocation.
    pub args: Vec<VarId>,
    /// The possible branches to continue to after the invocation.
    /// The extension would continue to exactly one of the branches.
    pub branches: Vec<BranchInfo>,
}

/// Describes the flow of a chosen extension's branch.
#[derive(Clone, Debug)]
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
