use std::collections::HashMap;

use db_utils::define_short_id;
use debug::DebugWithDb;
use defs::ids::{GenericFunctionId, ModuleId, ParamLongId, VarId};
use diagnostics::Diagnostics;
use diagnostics_proc_macros::DebugWithDb;
use syntax::node::{ast, TypedSyntaxNode};

use crate::corelib::unit_ty;
use crate::db::SemanticGroup;
use crate::expr::compute::Environment;
use crate::types::resolve_type;
use crate::{semantic, SemanticDiagnostic};

/// Function instance.
/// For example: ImplA::foo<A, B>, or bar<A>.
// TODO(spapini): Add a function pointer variant.
#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(SemanticGroup)]
pub enum FunctionLongId {
    Concrete(ConcreteFunction),
    Missing,
}
define_short_id!(FunctionId, FunctionLongId, SemanticGroup, lookup_intern_function);
impl FunctionId {
    pub fn missing(db: &dyn SemanticGroup) -> Self {
        db.intern_function(FunctionLongId::Missing)
    }

    pub fn return_type(&self, db: &dyn SemanticGroup) -> semantic::TypeId {
        match db.lookup_intern_function(*self) {
            FunctionLongId::Concrete(function) => function.return_type,
            FunctionLongId::Missing => semantic::TypeId::missing(db),
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ConcreteFunction {
    pub generic_function: GenericFunctionId,
    pub generic_args: Vec<semantic::GenericArgumentId>,
    pub return_type: semantic::TypeId,
}
impl DebugWithDb<dyn SemanticGroup> for ConcreteFunction {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        db: &(dyn SemanticGroup + 'static),
    ) -> std::fmt::Result {
        self.generic_function.fmt(f, db)?;
        if !self.generic_args.is_empty() {
            write!(f, "<")?;
            for arg in self.generic_args.iter() {
                write!(f, "{:?},", arg.debug(db))?;
            }
            write!(f, ">")?;
        }
        if self.return_type != unit_ty(db) && !self.generic_args.is_empty() {
            write!(f, " -> {:?}", self.return_type)?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(SemanticGroup)]
pub struct Signature {
    // TODO(spapini): Generics parameters.
    pub params: Vec<semantic::Parameter>,
    pub return_type: semantic::TypeId,
}

pub fn function_signature_return_type(
    diagnostics: &mut Diagnostics<SemanticDiagnostic>,
    db: &dyn SemanticGroup,
    module_id: ModuleId,
    sig: &ast::FunctionSignature,
) -> semantic::TypeId {
    let ty_syntax = match sig.ret_ty(db.upcast()) {
        ast::OptionReturnTypeClause::Empty(_) => {
            return unit_ty(db);
        }
        ast::OptionReturnTypeClause::ReturnTypeClause(ret_type_clause) => {
            ret_type_clause.ty(db.upcast())
        }
    };
    resolve_type(diagnostics, db, module_id, ty_syntax)
}

/// Returns the parameters of the given function signature's AST.
pub fn function_signature_params(
    diagnostics: &mut Diagnostics<SemanticDiagnostic>,
    db: &dyn SemanticGroup,
    module_id: ModuleId,
    sig: &ast::FunctionSignature,
) -> (Vec<semantic::Parameter>, Environment) {
    let syntax_db = db.upcast();

    let mut semantic_params = Vec::new();
    let mut variables = HashMap::new();
    let ast_params = sig.parameters(syntax_db).elements(syntax_db);
    for ast_param in ast_params.iter() {
        let name = ast_param.name(syntax_db).text(syntax_db);
        let id = db.intern_param(ParamLongId(module_id, ast_param.stable_ptr()));
        let ty_syntax = ast_param.type_clause(syntax_db).ty(syntax_db);
        // TODO(yuval): Diagnostic?
        let ty = resolve_type(diagnostics, db, module_id, ty_syntax);
        let param_name_stable_ptr = ast_param.name(syntax_db).stable_ptr().untyped();
        semantic_params.push(semantic::Parameter { id, ty, param_name_stable_ptr });
        variables.insert(name, semantic::Variable { id: VarId::Param(id), ty });
    }

    (semantic_params, Environment::new(variables))
}

/// Query implementation of [crate::db::SemanticGroup::generic_function_signature].
pub fn generic_function_signature(
    db: &dyn SemanticGroup,
    generic_function: GenericFunctionId,
) -> Option<Signature> {
    match generic_function {
        GenericFunctionId::Free(free_function) => db.free_function_signature(free_function),
        GenericFunctionId::Extern(extern_function) => {
            db.extern_function_declaration_signature(extern_function)
        }
    }
}
