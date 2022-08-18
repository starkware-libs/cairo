use std::path::PathBuf;

use defs::db::DefsGroup;
use defs::ids::{
    ExternFunctionLongId, FreeFunctionId, FreeFunctionLongId, ModuleId, ModuleItemId, StructId,
    StructLongId, SubmoduleId, SubmoduleLongId,
};
use filesystem::ids::{FileId, FileLongId};
use parser::db::ParserGroup;
use syntax::node::ast::Item;

use crate::ids::{
    ConcreteFunctionId, ConcreteFunctionLongId, ExprId, StatementId, TypeId, TypeLongId,
};
use crate::semantic;

// Salsa database interface.
#[salsa::query_group(SemanticDatabase)]
pub trait SemanticGroup: DefsGroup + ParserGroup {
    #[salsa::interned]
    fn function_instance(&self, id: ConcreteFunctionLongId) -> ConcreteFunctionId;
    #[salsa::interned]
    fn type_instance(&self, id: TypeLongId) -> TypeId;
    #[salsa::interned]
    fn expr(&self, expr: semantic::Expr) -> ExprId;
    #[salsa::interned]
    fn statement(&self, statement: semantic::Statement) -> StatementId;
    #[salsa::interned]
    fn intern_submodule(&self, id: SubmoduleLongId) -> SubmoduleId;

    // Queries to compute the semantic model for definitions.
    fn module_semantic(&self, item: ModuleId) -> semantic::Module;
    fn struct_semantic(&self, item: StructId) -> semantic::Struct;
    fn function_semantic(&self, item: FreeFunctionId) -> semantic::FreeFunction;

    fn module_items(&self, item: ModuleId) -> Vec<ModuleItemId>;
}

fn module_semantic(_db: &dyn SemanticGroup, _item: ModuleId) -> semantic::Module {
    todo!()
}

fn struct_semantic(_db: &dyn SemanticGroup, _item: StructId) -> semantic::Struct {
    todo!()
}

fn function_semantic(_db: &dyn SemanticGroup, _item: FreeFunctionId) -> semantic::FreeFunction {
    todo!()
}

#[allow(dead_code)] // TODO(yg): remove
fn module_items(db: &dyn SemanticGroup, module_id: ModuleId) -> Vec<ModuleItemId> {
    let green_interner = db.as_green_interner();
    let syntax_file = db.file_syntax(module_to_file(db, module_id.clone()));
    let item_list = syntax_file.items(green_interner);
    let mut module_items = Vec::new();
    for item in item_list.elements(green_interner) {
        match item {
            Item::Module(_module) => todo!(),
            Item::Function(function) => {
                module_items.push(ModuleItemId::FreeFunction(
                    db.intern_free_function(FreeFunctionLongId {
                        parent: module_id.clone(),
                        name: function
                            .signature(green_interner)
                            .name(green_interner)
                            .text(green_interner),
                    }),
                ));
            }
            Item::FunctionSignature(sig) => {
                module_items.push(ModuleItemId::ExternFunction(db.intern_extern_function(
                    ExternFunctionLongId {
                        parent: module_id.clone(),
                        name:
                            sig.signature(green_interner).name(green_interner).text(green_interner),
                    },
                )));
            }
            Item::Trait(_tr) => todo!(),
            Item::Impl(_imp) => todo!(),
            Item::Struct(strct) => {
                module_items.push(ModuleItemId::Struct(db.intern_struct(StructLongId {
                    parent: module_id.clone(),
                    name: strct.name(db.as_green_interner()).text(db.as_green_interner()),
                })));
            }
            Item::Enum(_en) => todo!(),
            Item::Use(_us) => todo!(),
        }
    }
    module_items
}

fn module_to_file(db: &dyn SemanticGroup, module_id: ModuleId) -> FileId {
    let filename = match module_id {
        ModuleId::CrateRoot(crate_id) => db.lookup_intern_crate(crate_id).0,
        ModuleId::Submodule(submodule_id) => {
            let _submodule_long_id = db.lookup_intern_submodule(submodule_id);
            // TODO(yuval): support submodules.
            todo!()
        }
    };
    db.intern_file(FileLongId::OnDisk(PathBuf::from(filename.as_str())))
}
