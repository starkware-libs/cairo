use defs::db::{AsDefsGroup, DefsDatabase};
use filesystem::db::{init_files_group, AsFilesGroup, FilesDatabase, FilesGroup};
use parser::db::ParserDatabase;
use semantic::db::{AsSemanticGroup, SemanticDatabase};
use sierra::ids::ConcreteLibFuncId;
use syntax::node::db::{AsSyntaxGroup, SyntaxDatabase, SyntaxGroup};

use crate::db::{SierraGenDatabase, SierraGenGroup};
use crate::pre_sierra;

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
impl AsFilesGroup for SierraGenDatabaseForTesting {
    fn as_files_group(&self) -> &(dyn FilesGroup + 'static) {
        self
    }
    fn as_files_group_mut(&mut self) -> &mut (dyn FilesGroup + 'static) {
        self
    }
}
impl AsSyntaxGroup for SierraGenDatabaseForTesting {
    fn as_syntax_group(&self) -> &(dyn SyntaxGroup + 'static) {
        self
    }
}
impl AsDefsGroup for SierraGenDatabaseForTesting {
    fn as_defs_group(&self) -> &(dyn defs::db::DefsGroup + 'static) {
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
