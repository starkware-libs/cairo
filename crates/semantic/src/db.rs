use defs::db::DefsGroup;
use defs::ids::{
    ExternFunctionLongId, FreeFunctionId, FreeFunctionLongId, ModuleId, ModuleItemId, StructId,
    StructLongId,
};
use filesystem::ids::FileId;
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
    fn intern_function_instance(&self, id: ConcreteFunctionLongId) -> ConcreteFunctionId;
    #[salsa::interned]
    fn intern_type_instance(&self, id: TypeLongId) -> TypeId;
    #[salsa::interned]
    fn intern_expr(&self, expr: semantic::Expr) -> ExprId;
    #[salsa::interned]
    fn intern_statement(&self, statement: semantic::Statement) -> StatementId;

    // Queries to compute the semantic model for definitions.
    fn module_semantic(&self, item: ModuleId) -> semantic::Module;
    fn struct_semantic(&self, item: StructId) -> semantic::Struct;
    fn free_function_semantic(&self, item: FreeFunctionId) -> semantic::FreeFunction;

    fn module_file(&self, module_id: ModuleId) -> Option<FileId>;
    fn module_items(&self, item: ModuleId) -> Vec<ModuleItemId>;
}

fn module_semantic(_db: &dyn SemanticGroup, _item: ModuleId) -> semantic::Module {
    todo!()
}

fn struct_semantic(_db: &dyn SemanticGroup, _item: StructId) -> semantic::Struct {
    todo!()
}

fn free_function_semantic(
    _db: &dyn SemanticGroup,
    _item: FreeFunctionId,
) -> semantic::FreeFunction {
    todo!()
}

#[allow(dead_code)] // TODO(yg): remove
fn module_items(db: &dyn SemanticGroup, module_id: ModuleId) -> Vec<ModuleItemId> {
    let green_interner = db.as_green_interner();
    let mut module_items = Vec::new();

    let syntax_file = if let Some(file_id) = module_file(db, module_id) {
        db.file_syntax(file_id)
    } else {
        return module_items;
    };
    let item_list = syntax_file.items(green_interner);
    for item in item_list.elements(green_interner) {
        match item {
            Item::Module(_module) => todo!(),
            Item::Function(function) => {
                module_items.push(ModuleItemId::FreeFunction(
                    db.intern_free_function(FreeFunctionLongId {
                        parent: module_id,
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
                        parent: module_id,
                        name:
                            sig.signature(green_interner).name(green_interner).text(green_interner),
                    },
                )));
            }
            Item::Trait(_tr) => todo!(),
            Item::Impl(_imp) => todo!(),
            Item::Struct(strct) => {
                module_items.push(ModuleItemId::Struct(db.intern_struct(StructLongId {
                    parent: module_id,
                    name: strct.name(db.as_green_interner()).text(db.as_green_interner()),
                })));
            }
            Item::Enum(_en) => todo!(),
            Item::Use(_us) => todo!(),
        }
    }
    module_items
}

fn module_file(db: &dyn SemanticGroup, module_id: ModuleId) -> Option<FileId> {
    match module_id {
        ModuleId::CrateRoot(crate_id) => db.crate_root_file(crate_id),
        ModuleId::Submodule(submodule_id) => {
            let _submodule_long_id = db.lookup_intern_submodule(submodule_id);
            // TODO(yuval): support submodules.
            todo!()
        }
    }
}
