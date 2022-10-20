use std::collections::HashMap;

use db_utils::define_short_id;
use debug::DebugWithDb;
use defs::ids::{GenericFunctionId, GenericParamId, ParamLongId};
use diagnostics_proc_macros::DebugWithDb;
use syntax::node::{ast, Terminal, TypedSyntaxNode};

use crate::corelib::unit_ty;
use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnostics;
use crate::expr::compute::Environment;
use crate::items::modifiers::compute_modifiers;
use crate::resolve_path::Resolver;
use crate::types::{resolve_type, substitute_generics};
use crate::{semantic, Parameter};

/// Function instance.
/// For example: ImplA::foo<A, B>, or bar<A>.
// TODO(spapini): Add a function pointer variant.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum FunctionLongId {
    Concrete(ConcreteFunction),
    Missing,
}
impl DebugWithDb<dyn SemanticGroup> for FunctionLongId {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        db: &(dyn SemanticGroup + 'static),
    ) -> std::fmt::Result {
        match self {
            FunctionLongId::Concrete(concrete) => write!(f, "{:?}", concrete.debug(db)),
            FunctionLongId::Missing => write!(f, "<missing>"),
        }
    }
}

define_short_id!(FunctionId, FunctionLongId, SemanticGroup, lookup_intern_function);
impl FunctionId {
    pub fn missing(db: &dyn SemanticGroup) -> Self {
        db.intern_function(FunctionLongId::Missing)
    }
}

// TODO(spapini): Refactor to an enum.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ConcreteFunction {
    pub generic_function: GenericFunctionId,
    pub generic_args: Vec<semantic::GenericArgumentId>,
}
impl DebugWithDb<dyn SemanticGroup> for ConcreteFunction {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        db: &(dyn SemanticGroup + 'static),
    ) -> std::fmt::Result {
        write!(f, "{}", self.generic_function.format(db.upcast()))?;
        if !self.generic_args.is_empty() {
            write!(f, "<")?;
            for arg in self.generic_args.iter() {
                write!(f, "{:?},", arg.debug(db))?;
            }
            write!(f, ">")?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct Signature {
    pub params: Vec<semantic::Parameter>,
    pub return_type: semantic::TypeId,
}

pub fn function_signature_return_type(
    diagnostics: &mut SemanticDiagnostics,
    db: &dyn SemanticGroup,
    resolver: &mut Resolver<'_>,
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
    resolve_type(db, diagnostics, resolver, &ty_syntax)
}

/// Returns the parameters of the given function signature's AST.
pub fn function_signature_params(
    diagnostics: &mut SemanticDiagnostics,
    db: &dyn SemanticGroup,
    resolver: &mut Resolver<'_>,
    sig: &ast::FunctionSignature,
) -> (Vec<semantic::Parameter>, Environment) {
    let syntax_db = db.upcast();

    let mut semantic_params = Vec::new();
    let mut variables = HashMap::new();
    let ast_params = sig.parameters(syntax_db).elements(syntax_db);
    for ast_param in ast_params.iter() {
        let name = ast_param.name(syntax_db).text(syntax_db);
        let id = db.intern_param(ParamLongId(resolver.module_id, ast_param.stable_ptr()));
        let ty_syntax = ast_param.type_clause(syntax_db).ty(syntax_db);
        // TODO(yuval): Diagnostic?
        let ty = resolve_type(db, diagnostics, resolver, &ty_syntax);
        let param = semantic::Parameter {
            id,
            ty,
            modifiers: compute_modifiers(
                diagnostics,
                syntax_db,
                &ast_param.modifiers(syntax_db).elements(syntax_db),
            ),
        };
        semantic_params.push(param.clone());
        variables.insert(name, semantic::Variable::Param(param));
    }

    (semantic_params, Environment::new(variables))
}

/// Query implementation of [crate::db::SemanticGroup::generic_function_signature].
pub fn generic_function_signature(
    db: &dyn SemanticGroup,
    generic_function: GenericFunctionId,
) -> Option<Signature> {
    match generic_function {
        GenericFunctionId::Free(free_function) => {
            db.free_function_declaration_signature(free_function)
        }
        GenericFunctionId::Extern(extern_function) => {
            db.extern_function_declaration_signature(extern_function)
        }
    }
}
/// Query implementation of [crate::db::SemanticGroup::generic_function_generic_params].
pub fn generic_function_generic_params(
    db: &dyn SemanticGroup,
    generic_function: GenericFunctionId,
) -> Option<Vec<GenericParamId>> {
    match generic_function {
        GenericFunctionId::Free(free_function) => {
            db.free_function_declaration_generic_params(free_function)
        }
        GenericFunctionId::Extern(extern_function) => {
            db.extern_function_declaration_generic_params(extern_function)
        }
    }
}

pub fn concrete_function_signature(
    db: &dyn SemanticGroup,
    function_id: FunctionId,
) -> Option<Signature> {
    match db.lookup_intern_function(function_id) {
        FunctionLongId::Concrete(ConcreteFunction { generic_function, generic_args, .. }) => {
            let generic_params = db.generic_function_generic_params(generic_function)?;
            if generic_params.len() != generic_args.len() {
                // TODO(spapini): Uphold the invariant that constructed ConcreteFunction instances
                //   always have the correct number of generic arguemnts.
                return None;
            }
            // TODO(spapini): When trait generics are supported, they need to be substituted
            //   one by one, not together.
            let substitution = generic_params.into_iter().zip(generic_args.into_iter()).collect();
            let generic_signature = db.generic_function_signature(generic_function)?;
            Some(Signature {
                params: generic_signature
                    .params
                    .into_iter()
                    .map(|param| Parameter {
                        id: param.id,
                        ty: substitute_generics(db, &substitution, param.ty),
                        modifiers: param.modifiers,
                    })
                    .collect(),
                return_type: substitute_generics(db, &substitution, generic_signature.return_type),
            })
        }
        FunctionLongId::Missing => None,
    }
}
