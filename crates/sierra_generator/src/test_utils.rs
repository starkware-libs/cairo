use db_utils::Upcast;
use defs::db::{DefsDatabase, DefsGroup};
use filesystem::db::{init_files_group, AsFilesGroupMut, FilesDatabase, FilesGroup};
use parser::db::ParserDatabase;
use salsa::{InternId, InternKey};
use semantic::db::{AsSemanticGroup, SemanticDatabase};
use sierra::ids::ConcreteLibFuncId;
use syntax::node::db::{SyntaxDatabase, SyntaxGroup};

use crate::db::{SierraGenDatabase, SierraGenGroup};
use crate::pre_sierra;
use crate::utils::{return_statement, simple_statement};

#[salsa::database(
    DefsDatabase,
    SemanticDatabase,
    SierraGenDatabase,
    ParserDatabase,
    SyntaxDatabase,
    FilesDatabase
)]
pub struct SierraGenDatabaseForTesting {
    storage: salsa::Storage<SierraGenDatabaseForTesting>,
}
impl salsa::Database for SierraGenDatabaseForTesting {}
impl Default for SierraGenDatabaseForTesting {
    fn default() -> Self {
        let mut res = Self { storage: Default::default() };
        init_files_group(&mut res);
        res
    }
}
impl AsFilesGroupMut for SierraGenDatabaseForTesting {
    fn as_files_group_mut(&mut self) -> &mut (dyn FilesGroup + 'static) {
        self
    }
}
impl Upcast<dyn FilesGroup> for SierraGenDatabaseForTesting {
    fn upcast(&self) -> &(dyn FilesGroup + 'static) {
        self
    }
}
impl Upcast<dyn SyntaxGroup> for SierraGenDatabaseForTesting {
    fn upcast(&self) -> &(dyn SyntaxGroup + 'static) {
        self
    }
}
impl Upcast<dyn DefsGroup> for SierraGenDatabaseForTesting {
    fn upcast(&self) -> &(dyn defs::db::DefsGroup + 'static) {
        self
    }
}
impl AsSemanticGroup for SierraGenDatabaseForTesting {
    fn as_semantic_group(&self) -> &(dyn semantic::db::SemanticGroup + 'static) {
        self
    }
}

/// Replaces `ConcreteLibFuncId` with a dummy `ConcreteLibFuncId` whose debug string is the string
/// representing the original `ConcreteLibFuncLongId`.
/// For example, while the original debug string may be `[6]`, the resulting debug string may be
/// `felt_const<2>`.
pub fn replace_libfunc_ids(
    db: &dyn SierraGenGroup,
    statement: &pre_sierra::Statement,
) -> pre_sierra::Statement {
    match statement {
        pre_sierra::Statement::Sierra(sierra::program::GenStatement::Invocation(p)) => {
            pre_sierra::Statement::Sierra(sierra::program::GenStatement::Invocation(
                sierra::program::GenInvocation {
                    libfunc_id: db
                        .lookup_intern_concrete_lib_func(p.libfunc_id.clone())
                        .to_string()
                        .into(),
                    ..p.clone()
                },
            ))
        }
        _ => statement.clone(),
    }
}

/// Replaces `ConcreteLibFuncId` with a dummy `ConcreteLibFuncId` whose debug string is the string
/// representing the original `ConcreteLibFuncLongId`.
/// For example, while the original debug string may be `[6]`, the resulting debug string may be
/// `felt_const<2>`.
///
/// Similar to [replace_libfunc_ids] except that it acts on [sierra::program::Program].
pub fn replace_libfunc_ids_in_program(
    db: &dyn SierraGenGroup,
    program: &sierra::program::Program,
) -> sierra::program::Program {
    let mut program = program.clone();
    let replace_id = |libfunc_id: &ConcreteLibFuncId| -> ConcreteLibFuncId {
        db.lookup_intern_concrete_lib_func(libfunc_id.clone()).to_string().into()
    };
    for statement in program.statements.iter_mut() {
        if let sierra::program::GenStatement::Invocation(p) = statement {
            p.libfunc_id = replace_id(&p.libfunc_id);
        }
    }
    for libfunc_declaration in program.libfunc_declarations.iter_mut() {
        libfunc_declaration.id = replace_id(&libfunc_declaration.id);
    }
    program
}

/// Generates a dummy statement with the given name, inputs and outputs.
pub fn dummy_simple_statement(
    name: &str,
    inputs: &[usize],
    outputs: &[usize],
) -> pre_sierra::Statement {
    let inputs_vec: Vec<sierra::ids::VarId> =
        inputs.iter().map(|x| sierra::ids::VarId::from_usize(*x)).collect();
    let outputs_vec: Vec<sierra::ids::VarId> =
        outputs.iter().map(|x| sierra::ids::VarId::from_usize(*x)).collect();
    simple_statement(ConcreteLibFuncId::from_string(name), &inputs_vec, &outputs_vec)
}

/// Generates a dummy return statement.
pub fn dummy_return_statement(args: &[usize]) -> pre_sierra::Statement {
    let args_vec: Vec<sierra::ids::VarId> =
        args.iter().map(|x| sierra::ids::VarId::from_usize(*x)).collect();
    return_statement(args_vec)
}

/// Generates a dummy label.
pub fn dummy_label(id: usize) -> pre_sierra::Statement {
    pre_sierra::Statement::Label(pre_sierra::Label {
        id: pre_sierra::LabelId::from_intern_id(InternId::from(id)),
    })
}
