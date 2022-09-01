use defs::db::{AsDefsGroup, DefsGroup};
use defs::ids::{FreeFunctionId, StructId};
use diagnostics::{Diagnostics, WithDiagnostics};
use diagnostics_proc_macros::with_diagnostics;
use filesystem::ids::ModuleId;
use parser::db::ParserGroup;
use parser::parser::ParserDiagnostic;
use syntax::node::ast;

use crate::corelib::unit_ty;
use crate::ids::{
    ConcreteFunctionId, ConcreteFunctionLongId, ExprId, StatementId, TypeId, TypeLongId,
};
use crate::{corelib, semantic, ConcreteType, GenericFunctionId};

// Salsa database interface.
#[salsa::query_group(SemanticDatabase)]
pub trait SemanticGroup: DefsGroup + AsDefsGroup + ParserGroup {
    #[salsa::interned]
    fn intern_concrete_function(&self, id: ConcreteFunctionLongId) -> ConcreteFunctionId;
    #[salsa::interned]
    fn intern_type(&self, id: TypeLongId) -> TypeId;
    #[salsa::interned]
    fn intern_expr(&self, expr: semantic::Expr) -> ExprId;
    #[salsa::interned]
    fn intern_statement(&self, statement: semantic::Statement) -> StatementId;

    // Queries to compute the semantic model for definitions.
    fn struct_semantic(&self, item: StructId) -> semantic::Struct;
    fn expr_semantic(&self, item: ExprId) -> semantic::Expr;
    fn statement_semantic(&self, item: StatementId) -> semantic::Statement;

    /// Gets the semantic signature of a function from the given function_id.
    fn generic_function_signature_semantic(
        &self,
        function_id: GenericFunctionId,
    ) -> WithDiagnostics<Option<semantic::Signature>, ParserDiagnostic>;
    /// Gets the semantic function from the given function_id.
    fn free_function_semantic(
        &self,
        function_id: FreeFunctionId,
    ) -> WithDiagnostics<Option<semantic::FreeFunction>, ParserDiagnostic>;

    // Corelib.
    #[salsa::invoke(corelib::core_module)]
    fn core_module(&self) -> ModuleId;
    #[salsa::invoke(corelib::core_felt_ty)]
    fn core_felt_ty(&self) -> TypeId;
}

// ----------------------- Queries -----------------------

fn struct_semantic(_db: &dyn SemanticGroup, _struct_id: StructId) -> semantic::Struct {
    todo!()
}

/// Computes the semantic model of the signature of a GenericFunction (e.g. Free / Extern).
#[with_diagnostics]
fn generic_function_signature_semantic(
    diagnostics: &mut Diagnostics<ParserDiagnostic>,
    db: &dyn SemanticGroup,
    function_id: GenericFunctionId,
) -> Option<semantic::Signature> {
    let free_function_id = match function_id {
        GenericFunctionId::Free(free_function_id) => free_function_id,
        GenericFunctionId::Extern(_) => todo!("Unsupported"),
    };
    let function = lookup_ast_free_function(db, free_function_id).unwrap(diagnostics)?;
    Some(function_signature_from_ast_function(db, &function))
}

#[with_diagnostics]
fn free_function_semantic(
    diagnostics: &mut Diagnostics<ParserDiagnostic>,
    db: &dyn SemanticGroup,
    function_id: FreeFunctionId,
) -> Option<semantic::FreeFunction> {
    let function = lookup_ast_free_function(db, function_id).unwrap(diagnostics)?;
    Some(semantic::FreeFunction {
        signature: function_signature_from_ast_function(db, &function),
        body: free_function_body(db, &function),
    })
}

fn expr_semantic(db: &dyn SemanticGroup, item: ExprId) -> semantic::Expr {
    db.lookup_intern_expr(item)
}

fn statement_semantic(db: &dyn SemanticGroup, item: StatementId) -> semantic::Statement {
    db.lookup_intern_statement(item)
}

// ----------------------- Helper functions -----------------------

/// Gets the AST for a function from its ID.
// TODO(yuval/spapini): replace this logic
// (1) It's a linear search, and used in a linear number of items => qudratic.
// (2) We will have to rewrite this code for each element (struct, enum, etc...)
// (3) When unrelated functions change in the AST of the same file, we will recompute the semantic
// model of the function. This is because of the call to the query db.file_syntax().
//
// To solve 3 we could, for example, use an AST rooted at the Function AST, and not at
// SyntaxFile, or maybe depend on GreenId.
//
// To solve 1 and 2, we can make a single query (ModuleId) -> (HashMap<ItemId, AST>), or something
// like that. It is linear. and this function could query its hashmap.
#[with_diagnostics]
fn lookup_ast_free_function(
    diagnostics: &mut Diagnostics<ParserDiagnostic>,
    db: &dyn SemanticGroup,
    function_id: FreeFunctionId,
) -> Option<ast::ItemFunction> {
    let syntax_db = db.as_syntax_group();
    let function = db.lookup_intern_free_function(function_id);
    let module_id = function.parent;
    let func_name = function.name;
    let syntax_file = db.file_syntax(db.module_file(module_id)?).unwrap(diagnostics)?;
    for item in syntax_file.items(syntax_db).elements(syntax_db) {
        if let ast::Item::Function(function) = item {
            if function.signature(syntax_db).name(syntax_db).text(syntax_db) == func_name {
                return Some(function);
            }
        }
    }
    None
}

/// Gets the semantic signature of the given function's AST.
fn function_signature_from_ast_function(
    db: &dyn SemanticGroup,
    function: &ast::ItemFunction,
) -> semantic::Signature {
    semantic::Signature {
        params: function_signature_params(db, &function.signature(db.as_syntax_group())),
        return_type: function_signature_return_type(db, &function.signature(db.as_syntax_group())),
    }
}

/// Gets the return type of the given function's AST.
fn function_signature_return_type(db: &dyn SemanticGroup, sig: &ast::FunctionSignature) -> TypeId {
    let type_path = match sig.ret_ty(db.as_syntax_group()) {
        ast::OptionReturnTypeClause::Empty(_) => {
            return unit_ty(db);
        }
        ast::OptionReturnTypeClause::ReturnTypeClause(ret_type_clause) => {
            ret_type_clause.ty(db.as_syntax_group())
        }
    };
    resolve_type(db, type_path)
}

// TODO(spapini): add a query wrapper.
/// Helper function to resolve a type from its path.
#[allow(unreachable_code)]
fn resolve_type(db: &dyn SemanticGroup, _type_path: ast::ExprPath) -> TypeId {
    // TODO(yuval/spapini): implement.
    db.intern_type(TypeLongId::Concrete(ConcreteType {
        generic_type: todo!(),
        generic_args: Vec::new(), // TODO(yuval): support generics.
    }))
}

/// Gets the parameters of the given function signature's AST.
fn function_signature_params(
    db: &dyn SemanticGroup,
    sig: &ast::FunctionSignature,
) -> Vec<semantic::Parameter> {
    let syntax_db = db.as_syntax_group();
    sig.parameters(syntax_db)
        .elements(syntax_db)
        .iter()
        .map(|param| {
            let name = param.identifier(syntax_db).text(syntax_db);
            semantic::Parameter {
                name,
                ty: resolve_type(db, param.type_clause(syntax_db).ty(syntax_db)),
            }
        })
        .collect()
}

// TODO(spapini): add a query wrapper.
/// Gets the body of the given free function's AST.
fn free_function_body(_db: &dyn SemanticGroup, _function: &ast::ItemFunction) -> ExprId {
    // TODO(spapini)
    todo!()
}
