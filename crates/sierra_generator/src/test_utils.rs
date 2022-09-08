use defs::db::{AsDefsGroup, DefsDatabase};
use filesystem::db::{AsFilesGroup, FilesDatabase, FilesGroup};
use parser::db::ParserDatabase;
use semantic::db::SemanticDatabase;
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
#[derive(Default)]
pub struct SierraGenDatabaseForTesting {
    storage: salsa::Storage<SierraGenDatabaseForTesting>,
}
impl salsa::Database for SierraGenDatabaseForTesting {}
impl AsFilesGroup for SierraGenDatabaseForTesting {
    fn as_files_group(&self) -> &(dyn FilesGroup + 'static) {
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
