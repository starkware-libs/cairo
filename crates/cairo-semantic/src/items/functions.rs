use db_utils::define_short_id;
use debug::DebugWithDb;
use defs::ids::{ExternFunctionId, GenericFunctionId, GenericParamId, ParamLongId};
use diagnostics::{skip_diagnostic, Maybe};
use diagnostics_proc_macros::DebugWithDb;
use smol_str::SmolStr;
use syntax::node::{ast, Terminal, TypedSyntaxNode};
use utils::try_extract_matches;

use super::modifiers;
use crate::corelib::unit_ty;
use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnostics;
use crate::expr::compute::Environment;
use crate::resolve_path::Resolver;
use crate::types::{resolve_type, substitute_generics};
use crate::{semantic, Parameter};

/// Function instance.
/// For example: `ImplA::foo<A, B>`, or `bar<A>`.
// TODO(spapini): Make it an enum and add a function pointer variant.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct FunctionLongId {
    pub function: ConcreteFunction,
}
impl DebugWithDb<dyn SemanticGroup> for FunctionLongId {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        db: &(dyn SemanticGroup + 'static),
    ) -> std::fmt::Result {
        write!(f, "{:?}", self.function.debug(db))
    }
}

define_short_id!(FunctionId, FunctionLongId, SemanticGroup, lookup_intern_function);
impl FunctionId {
    /// Returns the ExternFunctionId if this is an extern function. Otherwise returns none.
    pub fn try_get_extern_function_id(
        &self,
        db: &(dyn SemanticGroup + 'static),
    ) -> Option<ExternFunctionId> {
        try_extract_matches!(
            db.lookup_intern_function(*self).function.generic_function,
            GenericFunctionId::Extern
        )
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
    /// implicit parameters
    pub implicits: Vec<semantic::TypeId>,
    pub panicable: bool,
}

impl Signature {
    pub fn from_ast(
        diagnostics: &mut SemanticDiagnostics,
        db: &dyn SemanticGroup,
        resolver: &mut Resolver<'_>,
        signature_syntax: &ast::FunctionSignature,
        function_id: GenericFunctionId,
        environment: &mut Environment,
    ) -> Self {
        let return_type =
            function_signature_return_type(diagnostics, db, resolver, signature_syntax);
        let params = function_signature_params(
            diagnostics,
            db,
            resolver,
            signature_syntax,
            function_id,
            environment,
        );
        let implicits =
            function_signature_implicit_parameters(diagnostics, db, resolver, signature_syntax);
        let panicable = match signature_syntax.optional_no_panic(db.upcast()) {
            ast::OptionTerminalNoPanic::Empty(_) => true,
            ast::OptionTerminalNoPanic::TerminalNoPanic(_) => false,
        };
        semantic::Signature { params, return_type, implicits, panicable }
    }
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

/// Returns the implicit parameters of the given function signature's AST.
pub fn function_signature_implicit_parameters(
    diagnostics: &mut SemanticDiagnostics,
    db: &dyn SemanticGroup,
    resolver: &mut Resolver<'_>,
    sig: &ast::FunctionSignature,
) -> Vec<semantic::TypeId> {
    let syntax_db = db.upcast();

    let ast_implicits = match sig.implicits_clause(syntax_db) {
        ast::OptionImplicitsClause::Empty(_) => Vec::new(),
        ast::OptionImplicitsClause::ImplicitsClause(implicits_clause) => {
            implicits_clause.implicits(syntax_db).elements(syntax_db)
        }
    };

    let mut implicits = Vec::new();
    for implicit in ast_implicits {
        implicits.push(resolve_type(
            db,
            diagnostics,
            resolver,
            &syntax::node::ast::Expr::Path(implicit),
        ));
    }
    implicits
}

/// Returns the parameters of the given function signature's AST.
pub fn function_signature_params(
    diagnostics: &mut SemanticDiagnostics,
    db: &dyn SemanticGroup,
    resolver: &mut Resolver<'_>,
    sig: &ast::FunctionSignature,
    function_id: GenericFunctionId,
    env: &mut Environment,
) -> Vec<semantic::Parameter> {
    let syntax_db = db.upcast();
    update_env_with_ast_params(
        diagnostics,
        db,
        resolver,
        &sig.parameters(syntax_db).elements(syntax_db),
        function_id,
        env,
    )
}

/// Query implementation of [crate::db::SemanticGroup::generic_function_signature].
pub fn generic_function_signature(
    db: &dyn SemanticGroup,
    generic_function: GenericFunctionId,
) -> Maybe<Signature> {
    match generic_function {
        GenericFunctionId::Free(free_function) => {
            db.free_function_declaration_signature(free_function)
        }
        GenericFunctionId::Extern(extern_function) => {
            db.extern_function_declaration_signature(extern_function)
        }
        GenericFunctionId::TraitFunction(trait_function) => {
            db.trait_function_signature(trait_function)
        }
        GenericFunctionId::ImplFunction(impl_function) => db.impl_function_signature(impl_function),
    }
}
/// Query implementation of [crate::db::SemanticGroup::generic_function_generic_params].
pub fn generic_function_generic_params(
    db: &dyn SemanticGroup,
    generic_function: GenericFunctionId,
) -> Maybe<Vec<GenericParamId>> {
    match generic_function {
        GenericFunctionId::Free(free_function) => {
            db.free_function_declaration_generic_params(free_function)
        }
        GenericFunctionId::Extern(extern_function) => {
            db.extern_function_declaration_generic_params(extern_function)
        }
        GenericFunctionId::TraitFunction(trait_function) => {
            db.trait_function_generic_params(trait_function)
        }
        GenericFunctionId::ImplFunction(impl_function) => {
            db.impl_function_generic_params(impl_function)
        }
    }
}

/// Query implementation of [crate::db::SemanticGroup::concrete_function_signature].
pub fn concrete_function_signature(
    db: &dyn SemanticGroup,
    function_id: FunctionId,
) -> Maybe<Signature> {
    let ConcreteFunction { generic_function, generic_args, .. } =
        db.lookup_intern_function(function_id).function;
    let generic_params = db.generic_function_generic_params(generic_function)?;
    if generic_params.len() != generic_args.len() {
        // TODO(spapini): Uphold the invariant that constructed ConcreteFunction instances
        //   always have the correct number of generic arguemnts.
        return Err(skip_diagnostic());
    }
    // TODO(spapini): When trait generics are supported, they need to be substituted
    //   one by one, not together.
    let substitution_map = generic_params.into_iter().zip(generic_args.into_iter()).collect();
    let generic_signature = db.generic_function_signature(generic_function)?;
    let concretize_param = |param: semantic::Parameter| Parameter {
        id: param.id,
        ty: substitute_generics(db, &substitution_map, param.ty),
        mutability: param.mutability,
    };
    Ok(Signature {
        params: generic_signature.params.into_iter().map(concretize_param).collect(),
        return_type: substitute_generics(db, &substitution_map, generic_signature.return_type),
        implicits: generic_signature.implicits,
        panicable: generic_signature.panicable,
    })
}

/// For a given list of AST parameters, returns the list of semantic parameters along with the
/// corresponding environment.
fn update_env_with_ast_params(
    diagnostics: &mut SemanticDiagnostics,
    db: &dyn SemanticGroup,
    resolver: &mut Resolver<'_>,
    ast_params: &[ast::Param],
    function_id: GenericFunctionId,
    env: &mut Environment,
) -> Vec<semantic::Parameter> {
    let mut semantic_params = Vec::new();
    for ast_param in ast_params.iter() {
        let (name, semantic_param) = ast_param_to_semantic(diagnostics, db, resolver, ast_param);
        if env.add_param(diagnostics, &name, semantic_param.clone(), ast_param, function_id).is_ok()
        {
            semantic_params.push(semantic_param);
        }
    }
    semantic_params
}

/// Returns a semantic parameter (and its name) for the given AST parameter.
fn ast_param_to_semantic(
    diagnostics: &mut SemanticDiagnostics,
    db: &dyn SemanticGroup,
    resolver: &mut Resolver<'_>,
    ast_param: &ast::Param,
) -> (Option<SmolStr>, semantic::Parameter) {
    let syntax_db = db.upcast();

    let name = match ast_param.name(syntax_db) {
        ast::ParamName::Underscore(_) => None,
        ast::ParamName::Name(name) => Some(name.text(syntax_db)),
    };

    let id = db.intern_param(ParamLongId(resolver.module_file_id, ast_param.stable_ptr()));
    let ty_syntax = ast_param.type_clause(syntax_db).ty(syntax_db);
    let ty = resolve_type(db, diagnostics, resolver, &ty_syntax);

    let mutability = modifiers::compute_mutability(
        diagnostics,
        syntax_db,
        &ast_param.modifiers(syntax_db).elements(syntax_db),
    );

    let semantic_param = semantic::Parameter { id, ty, mutability };
    (name, semantic_param)
}
