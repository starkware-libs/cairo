use std::collections::HashMap;

use db_utils::define_short_id;
use debug::DebugWithDb;
use defs::ids::{GenericFunctionId, LanguageElementId, ModuleId, ParamLongId, VarId};
use diagnostics::{Diagnostics, WithDiagnostics};
use diagnostics_proc_macros::{with_diagnostics, DebugWithDb};
use syntax::node::{ast, TypedSyntaxNode};

use crate::corelib::unit_ty;
use crate::db::SemanticGroup;
use crate::expr::compute::EnvVariables;
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
        self.generic_function.fmt(f, db.as_defs_group())?;
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

/// All the semantic data that can be computed from the signature AST.
/// Used mostly for performance reasons, and other queries should select from, instead of
/// recomputing.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct GenericFunctionData {
    pub signature: semantic::Signature,
    pub variables: EnvVariables,
}

#[with_diagnostics]
/// Query implementation of [crate::db::SemanticGroup::priv_generic_function_signature_data].
pub fn priv_generic_function_signature_data(
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

/// Query implementation of [crate::db::SemanticGroup::generic_function_signature_semantic].
pub fn generic_function_signature_semantic(
    db: &dyn SemanticGroup,
    function_id: GenericFunctionId,
) -> Option<semantic::Signature> {
    let generic_data = db.priv_generic_function_signature_data(function_id).ignore()?;
    Some(generic_data.signature)
}
