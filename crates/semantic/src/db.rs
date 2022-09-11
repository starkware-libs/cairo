use std::collections::HashMap;

use defs::db::{AsDefsGroup, DefsGroup};
use defs::ids::{
    FreeFunctionId, GenericFunctionId, LanguageElementId, ModuleId, ParamLongId, StructId, VarId,
};
use diagnostics::{Diagnostics, WithDiagnostics};
use diagnostics_proc_macros::with_diagnostics;
use filesystem::db::AsFilesGroup;
use parser::db::ParserGroup;
use syntax::node::{ast, TypedSyntaxNode};

use crate::corelib::unit_ty;
use crate::expr::{compute_expr_semantic, resolve_type, ComputationContext, EnvVariables};
use crate::ids::{ExprId, StatementId, TypeId, TypeLongId};
use crate::{corelib, semantic, FunctionId, FunctionLongId, SemanticDiagnostic};

// Salsa database interface.
#[salsa::query_group(SemanticDatabase)]
pub trait SemanticGroup: DefsGroup + AsDefsGroup + ParserGroup + AsFilesGroup {
    #[salsa::interned]
    fn intern_function(&self, id: FunctionLongId) -> FunctionId;
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

    /// Should only be used internally.
    /// Computes semantic data about a signature of a generic function.
    fn priv_generic_function_signature_data(
        &self,
        function_id: GenericFunctionId,
    ) -> WithDiagnostics<Option<GenericFunctionData>, SemanticDiagnostic>;

    /// Returns the semantic signature of a function given the function_id.
    fn generic_function_signature_semantic(
        &self,
        function_id: GenericFunctionId,
    ) -> Option<semantic::Signature>;

    /// Returns the semantic function given the function_id.
    fn priv_free_function_semantic(
        &self,
        function_id: FreeFunctionId,
    ) -> WithDiagnostics<Option<semantic::FreeFunction>, SemanticDiagnostic>;
    /// Returns the semantic function given the function_id.
    fn free_function_semantic(&self, function_id: FreeFunctionId)
    -> Option<semantic::FreeFunction>;

    // Aggregates module level semantic diagnostics.
    // TODO(spapini): use Arcs to Vec of Arcs of diagnostics.
    fn module_semantic_diagnostics(
        &self,
        module_id: ModuleId,
    ) -> Option<Diagnostics<SemanticDiagnostic>>;

    // Corelib.
    #[salsa::invoke(corelib::core_module)]
    fn core_module(&self) -> ModuleId;
    #[salsa::invoke(corelib::core_felt_ty)]
    fn core_felt_ty(&self) -> TypeId;
}

pub trait AsSemanticGroup {
    fn as_semantic_group(&self) -> &(dyn SemanticGroup + 'static);
}

impl AsSemanticGroup for dyn SemanticGroup {
    fn as_semantic_group(&self) -> &(dyn SemanticGroup + 'static) {
        self
    }
}

// ----------------------- Queries -----------------------

fn struct_semantic(_db: &dyn SemanticGroup, _struct_id: StructId) -> semantic::Struct {
    todo!()
}

/// All the semantic data that can be computed from the signature AST.
/// Used mostly for performance reasons, and other queries should select from, instead of
/// recomputing.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct GenericFunctionData {
    signature: semantic::Signature,
    variables: EnvVariables,
}

/// Fetches the AST of the generic function signature. Computes and returns the
/// [`GenericFunctionData`] struct.
#[with_diagnostics]
fn priv_generic_function_signature_data(
    diagnostics: &mut Diagnostics<SemanticDiagnostic>,
    db: &dyn SemanticGroup,
    function_id: GenericFunctionId,
) -> Option<GenericFunctionData> {
    let module_id = function_id.module(db.as_defs_group());
    let module_data = db.module_data(module_id)?;
    let signature_syntax = match function_id {
        GenericFunctionId::Free(free_function_id) => {
            module_data.free_functions.get(&free_function_id)?.signature(db.as_syntax_group())
        }
        GenericFunctionId::Extern(extern_function_id) => {
            module_data.extern_functions.get(&extern_function_id)?.signature(db.as_syntax_group())
        }
    };

    let return_type = function_signature_return_type(diagnostics, db, module_id, &signature_syntax);

    let (params, variables) =
        function_signature_params(diagnostics, db, module_id, &signature_syntax)?;
    Some(GenericFunctionData { signature: semantic::Signature { params, return_type }, variables })
}

/// Computes the semantic model of the signature of a GenericFunction (e.g. Free / Extern).
fn generic_function_signature_semantic(
    db: &dyn SemanticGroup,
    function_id: GenericFunctionId,
) -> Option<semantic::Signature> {
    let generic_data = db.priv_generic_function_signature_data(function_id).ignore()?;
    Some(generic_data.signature)
}

#[with_diagnostics]
fn priv_free_function_semantic(
    diagnostics: &mut Diagnostics<SemanticDiagnostic>,
    db: &dyn SemanticGroup,
    free_function_id: FreeFunctionId,
) -> Option<semantic::FreeFunction> {
    let module_id = free_function_id.module(db.as_defs_group());
    let syntax = db.module_data(module_id)?.free_functions.get(&free_function_id)?.clone();

    // Compute signature semantic.
    let generic_function_signature_data = db
        .priv_generic_function_signature_data(GenericFunctionId::Free(free_function_id))
        .ignore()?;

    // Compute body semantic expr.
    let mut ctx = ComputationContext::new(
        diagnostics,
        db,
        module_id,
        generic_function_signature_data.variables,
    );
    let expr = compute_expr_semantic(&mut ctx, ast::Expr::Block(syntax.body(db.as_syntax_group())));
    let body = db.intern_expr(expr);

    Some(semantic::FreeFunction { signature: generic_function_signature_data.signature, body })
}

fn free_function_semantic(
    db: &dyn SemanticGroup,
    free_function_id: FreeFunctionId,
) -> Option<semantic::FreeFunction> {
    db.priv_free_function_semantic(free_function_id).ignore()
}

// TODO(spapini): Implement this more efficiently, with Arcs where needed, and not clones.
#[allow(clippy::single_match)]
fn module_semantic_diagnostics(
    db: &dyn SemanticGroup,
    module_id: ModuleId,
) -> Option<Diagnostics<SemanticDiagnostic>> {
    let mut diagnostics = Diagnostics::new();
    for (_name, item) in db.module_items(module_id)?.items.iter() {
        match item {
            // Add signature diagnostics.
            defs::ids::ModuleItemId::FreeFunction(free_function) => {
                diagnostics.0.extend(
                    db.priv_generic_function_signature_data(GenericFunctionId::Free(
                        *free_function,
                    ))
                    .get_diagnostics()
                    .0
                    .clone(),
                );
                // Add body diagnostics.
                diagnostics.0.extend(
                    db.priv_free_function_semantic(*free_function).get_diagnostics().0.clone(),
                );
            }
            defs::ids::ModuleItemId::Submodule(_) => {}
            defs::ids::ModuleItemId::Use(_) => {}
            defs::ids::ModuleItemId::Struct(_) => {}
            defs::ids::ModuleItemId::ExternType(_) => {}
            defs::ids::ModuleItemId::ExternFunction(_) => {}
        }
    }
    Some(diagnostics)
}

fn expr_semantic(db: &dyn SemanticGroup, item: ExprId) -> semantic::Expr {
    db.lookup_intern_expr(item)
}

fn statement_semantic(db: &dyn SemanticGroup, item: StatementId) -> semantic::Statement {
    db.lookup_intern_statement(item)
}

// ----------------------- Helper functions -----------------------
/// Gets the return type of the given function's AST.
fn function_signature_return_type(
    diagnostics: &mut Diagnostics<SemanticDiagnostic>,
    db: &dyn SemanticGroup,
    module_id: ModuleId,
    sig: &ast::FunctionSignature,
) -> TypeId {
    let ty_syntax = match sig.ret_ty(db.as_syntax_group()) {
        ast::OptionReturnTypeClause::Empty(_) => {
            return unit_ty(db);
        }
        ast::OptionReturnTypeClause::ReturnTypeClause(ret_type_clause) => {
            ret_type_clause.ty(db.as_syntax_group())
        }
    };
    resolve_type(diagnostics, db, module_id, ty_syntax)
}

/// Returns the parameters of the given function signature's AST.
fn function_signature_params(
    diagnostics: &mut Diagnostics<SemanticDiagnostic>,
    db: &dyn SemanticGroup,
    module_id: ModuleId,
    sig: &ast::FunctionSignature,
) -> Option<(Vec<semantic::Parameter>, EnvVariables)> {
    let syntax_db = db.as_syntax_group();

    let mut semantic_params = Vec::new();
    let mut variables = HashMap::new();
    let ast_params = sig.parameters(syntax_db).elements(syntax_db);
    for ast_param in ast_params.iter() {
        let name = ast_param.name(syntax_db).text(syntax_db);
        let id = db.intern_param(ParamLongId(module_id, ast_param.stable_ptr()));
        let ty_syntax = match ast_param.type_clause(syntax_db) {
            ast::NonOptionTypeClause::TypeClause(type_clause) => type_clause.ty(syntax_db),
            ast::NonOptionTypeClause::NonOptionTypeClauseMissing(_) => {
                // TODO(yuval): return None + Diagnostic.
                panic!("param {name} is missing a type clause")
            }
        };
        // TODO(yuval): Diagnostic?
        let ty = resolve_type(diagnostics, db, module_id, ty_syntax);
        semantic_params.push(semantic::Parameter { id, ty });
        variables.insert(name, semantic::Variable { id: VarId::Param(id), ty });
    }

    Some((semantic_params, variables))
}
