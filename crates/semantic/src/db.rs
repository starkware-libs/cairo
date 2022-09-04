use defs::db::{AsDefsGroup, DefsGroup};
use defs::ids::{
    FreeFunctionId, GenericFunctionId, GenericTypeId, ModuleItemId, ParamContainerId, ParamLongId,
    StructId, VarId,
};
use diagnostics::{Diagnostics, WithDiagnostics};
use diagnostics_proc_macros::with_diagnostics;
use filesystem::db::AsFilesGroup;
use filesystem::ids::ModuleId;
use parser::db::ParserGroup;
use parser::parser::ParserDiagnostic;
use smol_str::SmolStr;
use syntax::node::ast;

use crate::corelib::{core_module, unit_ty};
use crate::diagnostic::Diagnostic;
use crate::expr::{compute_expr_semantic, ComputationContext};
use crate::ids::{
    ConcreteFunctionId, ConcreteFunctionLongId, ExprId, StatementId, TypeId, TypeLongId,
};
use crate::{corelib, semantic, ConcreteType};

// Salsa database interface.
#[salsa::query_group(SemanticDatabase)]
pub trait SemanticGroup: DefsGroup + AsDefsGroup + ParserGroup + AsFilesGroup {
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
    ) -> WithDiagnostics<Option<semantic::Signature>, Diagnostic>;

    /// Returns the semantic function given the function_id.
    fn free_function_semantic(
        &self,
        function_id: FreeFunctionId,
    ) -> WithDiagnostics<Option<semantic::FreeFunction>, Diagnostic>;

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
    diagnostics: &mut Diagnostics<Diagnostic>,
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
    function_signature_from_ast_function(db, module_id, &syntax).unwrap(diagnostics)
}

#[with_diagnostics]
fn free_function_semantic(
    diagnostics: &mut Diagnostics<Diagnostic>,
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
    let signature =
        function_signature_from_ast_function(db, module_id, &syntax).unwrap(diagnostics)?;

    // Compute body semantic expr.
    let variables = signature
        .params
        .iter()
        .map(|param| {
            (
                param.name.clone(),
                VarId::Param(db.intern_param(ParamLongId {
                    parent: ParamContainerId::FreeFunction(free_function_id),
                    name: param.name.clone(),
                })),
            )
        })
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
#[with_diagnostics]
fn function_signature_from_ast_function(
    diagnostics: &mut Diagnostics<ParserDiagnostic>,
    db: &dyn SemanticGroup,
    module_id: ModuleId,
    function: &ast::ItemFunction,
) -> Option<semantic::Signature> {
    let return_type =
        function_signature_return_type(db, module_id, &function.signature(db.as_syntax_group()))
            .unwrap(diagnostics)?;

    let params =
        function_signature_params(db, module_id, &function.signature(db.as_syntax_group()))
            .unwrap(diagnostics)?;

    Some(semantic::Signature { params, return_type })
}

/// Gets the return type of the given function's AST.
#[with_diagnostics]
fn function_signature_return_type(
    diagnostics: &mut Diagnostics<ParserDiagnostic>,
    db: &dyn SemanticGroup,
    module_id: ModuleId,
    sig: &ast::FunctionSignature,
) -> Option<TypeId> {
    let type_path = match sig.ret_ty(db.as_syntax_group()) {
        ast::OptionReturnTypeClause::Empty(_) => {
            return Some(unit_ty(db));
        }
        ast::OptionReturnTypeClause::ReturnTypeClause(ret_type_clause) => {
            ret_type_clause.ty(db.as_syntax_group())
        }
    };
    resolve_type(db, module_id, type_path).unwrap(diagnostics)
}

/// Returns the parameters of the given function signature's AST.
#[with_diagnostics]
fn function_signature_params(
    diagnostics: &mut Diagnostics<ParserDiagnostic>,
    db: &dyn SemanticGroup,
    module_id: ModuleId,
    sig: &ast::FunctionSignature,
) -> Option<Vec<semantic::Parameter>> {
    let syntax_db = db.as_syntax_group();

    let mut semantic_params = Vec::new();
    let ast_params = sig.parameters(syntax_db).elements(syntax_db);
    for ast_param in ast_params.iter() {
        let name = ast_param.identifier(syntax_db).text(syntax_db);
        let ty_path = match ast_param.type_clause(syntax_db) {
            ast::NonOptionTypeClause::TypeClause(type_clause) => type_clause.ty(syntax_db),
            ast::NonOptionTypeClause::NonOptionTypeClauseMissing(_) => {
                // TODO(yuval): return None + Diagnostic.
                panic!("param {name} is missing a type clause")
            }
        };
        // TODO(yuval): Diagnostic?
        let ty = resolve_type(db, module_id, ty_path).unwrap(diagnostics)?;
        semantic_params.push(semantic::Parameter { name, ty });
    }

    Some(semantic_params)
}

// TODO(yuval): move to a separate module "type".
// TODO(spapini): add a query wrapper.
/// Resolves a type given a module and a path.
#[with_diagnostics]
fn resolve_type(
    diagnostics: &mut Diagnostics<ParserDiagnostic>,
    db: &dyn SemanticGroup,
    module_id: ModuleId,
    type_path: ast::ExprPath,
) -> Option<TypeId> {
    let syntax_db = db.as_syntax_group();
    let segments = type_path.elements(syntax_db);
    if segments.len() != 1 {
        // TODO(yuval): return None + Diagnostic.
        panic!("Expected a single identifier");
    }
    let last_segment = &segments[0];
    if let ast::OptionGenericArgs::Some(_) = last_segment.generic_args(syntax_db) {
        todo!("Generics are not supported yet")
    };
    let type_name = last_segment.ident(syntax_db).text(syntax_db);

    if let Some(res) = resolve_type_by_name(db, module_id, &type_name).unwrap(diagnostics) {
        return Some(res);
    }
    resolve_type_by_name(db, core_module(db), &type_name).unwrap(diagnostics)
}

// TODO(yuval): move to a separate module "type".
/// Resolves a type given a module and a simple name.
#[with_diagnostics]
fn resolve_type_by_name(
    diagnostics: &mut Diagnostics<ParserDiagnostic>,
    db: &dyn SemanticGroup,
    module_id: ModuleId,
    type_name: &SmolStr,
) -> Option<TypeId> {
    // TODO(yuval): diagnostic on None?
    let module_item = db.module_item_by_name(module_id, type_name.clone()).unwrap(diagnostics)?;
    let generic_type = match module_item {
        ModuleItemId::Struct(struct_id) => GenericTypeId::Struct(struct_id),
        ModuleItemId::ExternType(extern_type_id) => GenericTypeId::Extern(extern_type_id),
        unexpected_variant => {
            panic!("Unexpected ModuleItemId variant: {unexpected_variant:?}")
        }
    };
    Some(db.intern_type(TypeLongId::Concrete(ConcreteType {
        generic_type,
        generic_args: Vec::new(), // TODO(yuval): support generics.
    })))
}
