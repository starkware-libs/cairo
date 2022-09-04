use defs::db::{AsDefsGroup, DefsGroup};
use defs::ids::{
    FreeFunctionId, GenericFunctionId, GenericTypeId, ParamContainerId, ParamId, ParamLongId,
    StructId, VarId,
};
use diagnostics::{Diagnostics, WithDiagnostics};
use diagnostics_proc_macros::with_diagnostics;
use filesystem::ids::ModuleId;
use parser::db::ParserGroup;
use parser::parser::ParserDiagnostic;
use syntax::node::ast;

use crate::corelib::unit_ty;
use crate::expr::{compute_expr_semantic, ComputationContext};
use crate::ids::{
    ConcreteFunctionId, ConcreteFunctionLongId, ExprId, StatementId, TypeId, TypeLongId,
};
use crate::{corelib, semantic, ConcreteType};

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

    /// Returns the semantic signature of a function given the function_id.
    fn generic_function_signature_semantic(
        &self,
        function_id: GenericFunctionId,
    ) -> WithDiagnostics<Option<semantic::Signature>, ParserDiagnostic>;

    /// Returns the semantic function given the function_id.
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
    let module_id = db.lookup_intern_free_function(free_function_id).parent;
    let syntax = db
        .module_data(module_id)
        .unwrap(diagnostics)?
        .free_functions
        .get(&free_function_id)?
        .clone();
    Some(function_signature_from_ast_function(
        db,
        ParamContainerId::FreeFunction(free_function_id),
        &syntax,
    ))
}

#[with_diagnostics]
fn free_function_semantic(
    diagnostics: &mut Diagnostics<ParserDiagnostic>,
    db: &dyn SemanticGroup,
    free_function_id: FreeFunctionId,
) -> Option<semantic::FreeFunction> {
    let module_id = db.lookup_intern_free_function(free_function_id).parent;
    let syntax = db
        .module_data(module_id)
        .unwrap(diagnostics)?
        .free_functions
        .get(&free_function_id)?
        .clone();

    // Compute signature semantic.
    let signature = function_signature_from_ast_function(
        db,
        ParamContainerId::FreeFunction(free_function_id),
        &syntax,
    );

    // Compute body semantic expr.
    let variables = signature
        .params
        .iter()
        .copied()
        .map(|param_id| (db.lookup_intern_param(param_id).name, VarId::Param(param_id)))
        .collect();
    let mut ctx = ComputationContext::new(db, module_id, variables);
    let body = compute_expr_semantic(&mut ctx, ast::Expr::Block(syntax.body(db.as_syntax_group())));

    Some(semantic::FreeFunction { signature, body })
}

fn expr_semantic(db: &dyn SemanticGroup, item: ExprId) -> semantic::Expr {
    db.lookup_intern_expr(item)
}

fn statement_semantic(db: &dyn SemanticGroup, item: StatementId) -> semantic::Statement {
    db.lookup_intern_statement(item)
}

// ----------------------- Helper functions -----------------------

/// Gets the semantic signature of the given function's AST.
fn function_signature_from_ast_function(
    db: &dyn SemanticGroup,
    sig_id: ParamContainerId,
    function: &ast::ItemFunction,
) -> semantic::Signature {
    semantic::Signature {
        params: function_signature_params(db, sig_id, &function.signature(db.as_syntax_group())),
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
    db.intern_type(TypeLongId::Concrete(ConcreteType {
        generic_type: resolve_type(type_path),
        generic_args: vec![], // TODO(yuval): support generics.
    }))
}

// TODO(spapini): add a query wrapper.
/// Helper function to resolve a type from its path.
fn resolve_type(_type_path: ast::ExprPath) -> GenericTypeId {
    // TODO(yuval/spapini): implement.
    todo!()
}

/// Gets the parameters of the given function signature's AST.
fn function_signature_params(
    db: &dyn SemanticGroup,
    sig_id: ParamContainerId,
    sig: &ast::FunctionSignature,
) -> Vec<ParamId> {
    let syntax_db = db.as_syntax_group();
    sig.parameters(syntax_db)
        .elements(syntax_db)
        .iter()
        .map(|param| {
            db.intern_param(ParamLongId {
                parent: sig_id,
                name: param.identifier(syntax_db).text(syntax_db),
            })
        })
        .collect()
}
