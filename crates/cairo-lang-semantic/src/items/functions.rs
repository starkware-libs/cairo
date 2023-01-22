use std::sync::Arc;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::{
    ExternFunctionId, FreeFunctionId, FunctionSignatureId, FunctionWithBodyId, GenericParamId,
    ImplFunctionId, ModuleItemId, ParamLongId,
};
use cairo_lang_diagnostics::{skip_diagnostic, Diagnostics, Maybe};
use cairo_lang_proc_macros::DebugWithDb;
use cairo_lang_syntax as syntax;
use cairo_lang_syntax::node::{ast, Terminal, TypedSyntaxNode};
use cairo_lang_utils::{define_short_id, try_extract_matches, OptionFrom};
use itertools::chain;

use super::attribute::Attribute;
use super::modifiers;
use super::trt::ConcreteTraitFunctionId;
use crate::corelib::unit_ty;
use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnostics;
use crate::expr::compute::Environment;
use crate::resolve_path::{ResolvedLookback, Resolver};
use crate::types::{resolve_type, substitute_generics, GenericSubstitution};
use crate::{semantic, ConcreteImplId, Parameter, SemanticDiagnostic};

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct ConcreteImplGenericFunctionId {
    pub concrete_impl: ConcreteImplId,
    pub function: ImplFunctionId,
}

/// The ID of a generic function that can be concretized.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum GenericFunctionId {
    /// A generic free function.
    Free(FreeFunctionId),
    /// A generic extern function.
    Extern(ExternFunctionId),
    /// A generic function of a concrete impl.
    Impl(ConcreteImplGenericFunctionId),
    // TODO(spapini): Remove when we separate semantic representations.
    Trait(ConcreteTraitFunctionId),
}
impl GenericFunctionId {
    pub fn format(&self, db: &dyn SemanticGroup) -> String {
        self.signature(db).format(db.elongate().upcast())
    }
    /// Gets the FunctionSignatureId of the generic function.
    pub fn signature(&self, db: &dyn SemanticGroup) -> FunctionSignatureId {
        match *self {
            GenericFunctionId::Free(id) => FunctionSignatureId::Free(id),
            GenericFunctionId::Extern(id) => FunctionSignatureId::Extern(id),
            GenericFunctionId::Impl(id) => FunctionSignatureId::Impl(id.function),
            GenericFunctionId::Trait(id) => FunctionSignatureId::Trait(id.function_id(db)),
        }
    }
}
/// Conversion from ModuleItemId to GenericFunctionId.
impl OptionFrom<ModuleItemId> for GenericFunctionId {
    fn option_from(item: ModuleItemId) -> Option<Self> {
        match item {
            ModuleItemId::FreeFunction(id) => Some(GenericFunctionId::Free(id)),
            ModuleItemId::ExternFunction(id) => Some(GenericFunctionId::Extern(id)),
            ModuleItemId::Constant(_)
            | ModuleItemId::Submodule(_)
            | ModuleItemId::Use(_)
            | ModuleItemId::Trait(_)
            | ModuleItemId::Impl(_)
            | ModuleItemId::Struct(_)
            | ModuleItemId::Enum(_)
            | ModuleItemId::TypeAlias(_)
            | ModuleItemId::ExternType(_) => None,
        }
    }
}

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

    /// Returns the FunctionWithBodyId if this is a function with body, otherwise returns None.
    pub fn try_get_function_with_body_id(
        &self,
        db: &(dyn SemanticGroup + 'static),
    ) -> Option<FunctionWithBodyId> {
        match db.lookup_intern_function(*self).function.generic_function {
            GenericFunctionId::Free(free_function_id) => {
                Some(FunctionWithBodyId::Free(free_function_id))
            }
            GenericFunctionId::Impl(impl_function_id) => {
                Some(FunctionWithBodyId::Impl(impl_function_id.function))
            }
            GenericFunctionId::Trait(_) => None,
            GenericFunctionId::Extern(_) => None,
        }
    }
}

/// The ID of a generic function with body that can be concretized.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum GenericFunctionWithBodyId {
    Free(FreeFunctionId),
    Impl(ConcreteImplGenericFunctionId),
}
impl From<GenericFunctionWithBodyId> for GenericFunctionId {
    fn from(val: GenericFunctionWithBodyId) -> Self {
        match val {
            GenericFunctionWithBodyId::Free(id) => GenericFunctionId::Free(id),
            GenericFunctionWithBodyId::Impl(id) => GenericFunctionId::Impl(id),
        }
    }
}
impl OptionFrom<GenericFunctionId> for GenericFunctionWithBodyId {
    fn option_from(other: GenericFunctionId) -> Option<Self> {
        Some(match other {
            GenericFunctionId::Free(id) => GenericFunctionWithBodyId::Free(id),
            GenericFunctionId::Impl(id) => GenericFunctionWithBodyId::Impl(id),
            _ => return None,
        })
    }
}

/// A long Id of a concrete function with body.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ConcreteFunctionWithBody {
    pub generic_function: GenericFunctionWithBodyId,
    pub generic_args: Vec<semantic::GenericArgumentId>,
}
impl ConcreteFunctionWithBody {
    pub fn function_with_body_id(&self) -> FunctionWithBodyId {
        match self.generic_function {
            GenericFunctionWithBodyId::Free(id) => FunctionWithBodyId::Free(id),
            GenericFunctionWithBodyId::Impl(id) => FunctionWithBodyId::Impl(id.function),
        }
    }
    pub fn substitution(&self, db: &dyn SemanticGroup) -> Maybe<GenericSubstitution> {
        Ok(match self.generic_function {
            GenericFunctionWithBodyId::Free(f) => GenericSubstitution(
                db.free_function_generic_params(f)?
                    .into_iter()
                    .zip(self.generic_args.iter().copied())
                    .collect(),
            ),
            GenericFunctionWithBodyId::Impl(f) => {
                let concrete_impl = db.lookup_intern_concrete_impl(f.concrete_impl);
                GenericSubstitution(
                    chain!(
                        db.impl_function_generic_params(f.function)?
                            .into_iter()
                            .zip(self.generic_args.iter().copied()),
                        db.impl_generic_params(concrete_impl.impl_id)?
                            .into_iter()
                            .zip(concrete_impl.generic_args.iter().copied())
                    )
                    .collect(),
                )
            }
        })
    }
    pub fn from_no_generics_free(
        db: &dyn SemanticGroup,
        free_function_id: FreeFunctionId,
    ) -> Option<Self> {
        if !db.free_function_generic_params(free_function_id).ok()?.is_empty() {
            return None;
        }
        Some(ConcreteFunctionWithBody {
            generic_function: GenericFunctionWithBodyId::Free(free_function_id),
            generic_args: vec![],
        })
    }
    pub fn concrete(&self) -> ConcreteFunction {
        ConcreteFunction {
            generic_function: self.generic_function.into(),
            generic_args: self.generic_args.clone(),
        }
    }
    pub fn function_id(&self, db: &dyn SemanticGroup) -> FunctionId {
        db.intern_function(FunctionLongId { function: self.concrete() })
    }
}

define_short_id!(
    ConcreteFunctionWithBodyId,
    ConcreteFunctionWithBody,
    SemanticGroup,
    lookup_intern_concrete_function_with_body
);
impl ConcreteFunctionWithBodyId {
    fn get(&self, db: &dyn SemanticGroup) -> ConcreteFunctionWithBody {
        db.lookup_intern_concrete_function_with_body(*self)
    }
    pub fn function_with_body_id(&self, db: &dyn SemanticGroup) -> FunctionWithBodyId {
        self.get(db).function_with_body_id()
    }
    pub fn substitution(&self, db: &dyn SemanticGroup) -> Maybe<GenericSubstitution> {
        self.get(db).substitution(db)
    }
    pub fn from_no_generics_free(
        db: &dyn SemanticGroup,
        free_function_id: FreeFunctionId,
    ) -> Option<Self> {
        Some(db.intern_concrete_function_with_body(
            ConcreteFunctionWithBody::from_no_generics_free(db, free_function_id)?,
        ))
    }
    pub fn concrete(&self, db: &dyn SemanticGroup) -> ConcreteFunction {
        self.get(db).concrete()
    }
    pub fn function_id(&self, db: &dyn SemanticGroup) -> FunctionId {
        self.get(db).function_id(db)
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ConcreteFunction {
    pub generic_function: GenericFunctionId,
    pub generic_args: Vec<semantic::GenericArgumentId>,
}
impl ConcreteFunction {
    pub fn get_body(&self, db: &dyn SemanticGroup) -> Option<ConcreteFunctionWithBodyId> {
        Some(db.intern_concrete_function_with_body(ConcreteFunctionWithBody {
            generic_function: OptionFrom::option_from(self.generic_function)?,
            generic_args: self.generic_args.clone(),
        }))
    }
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
    #[hide_field_debug_with_db]
    pub stable_ptr: ast::FunctionSignaturePtr,
}

impl Signature {
    pub fn from_ast(
        diagnostics: &mut SemanticDiagnostics,
        db: &dyn SemanticGroup,
        resolver: &mut Resolver<'_>,
        signature_syntax: &ast::FunctionSignature,
        function_signature_id: FunctionSignatureId,
        environment: &mut Environment,
    ) -> Self {
        let return_type =
            function_signature_return_type(diagnostics, db, resolver, signature_syntax);
        let params = function_signature_params(
            diagnostics,
            db,
            resolver,
            signature_syntax,
            function_signature_id,
            environment,
        );
        let implicits =
            function_signature_implicit_parameters(diagnostics, db, resolver, signature_syntax);
        let panicable = match signature_syntax.optional_no_panic(db.upcast()) {
            ast::OptionTerminalNoPanic::Empty(_) => true,
            ast::OptionTerminalNoPanic::TerminalNoPanic(_) => false,
        };
        let stable_ptr = signature_syntax.stable_ptr();
        semantic::Signature { params, return_type, implicits, panicable, stable_ptr }
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
    function_signature_id: FunctionSignatureId,
    env: &mut Environment,
) -> Vec<semantic::Parameter> {
    let syntax_db = db.upcast();
    update_env_with_ast_params(
        diagnostics,
        db,
        resolver,
        &sig.parameters(syntax_db).elements(syntax_db),
        function_signature_id,
        env,
    )
}

/// Query implementation of [crate::db::SemanticGroup::function_signature_signature].
pub fn function_signature_signature(
    db: &dyn SemanticGroup,
    function_signature_id: FunctionSignatureId,
) -> Maybe<Signature> {
    match function_signature_id {
        FunctionSignatureId::Free(free_function) => db.free_function_signature(free_function),
        FunctionSignatureId::Extern(extern_function) => {
            db.extern_function_signature(extern_function)
        }
        FunctionSignatureId::Trait(trait_function) => db.trait_function_signature(trait_function),
        FunctionSignatureId::Impl(impl_function) => db.impl_function_signature(impl_function),
    }
}
/// Query implementation of [crate::db::SemanticGroup::function_signature_generic_params].
pub fn function_signature_generic_params(
    db: &dyn SemanticGroup,
    function_signature_id: FunctionSignatureId,
) -> Maybe<Vec<GenericParamId>> {
    match function_signature_id {
        FunctionSignatureId::Free(free_function) => db.free_function_generic_params(free_function),
        FunctionSignatureId::Extern(extern_function) => {
            db.extern_function_declaration_generic_params(extern_function)
        }
        FunctionSignatureId::Trait(trait_function) => {
            db.trait_function_generic_params(trait_function)
        }
        FunctionSignatureId::Impl(impl_function) => db.impl_function_generic_params(impl_function),
    }
}

/// Query implementation of [crate::db::SemanticGroup::concrete_function_signature].
pub fn concrete_function_signature(
    db: &dyn SemanticGroup,
    function_id: FunctionId,
) -> Maybe<Signature> {
    let ConcreteFunction { generic_function, generic_args, .. } =
        db.lookup_intern_function(function_id).function;
    let generic_params = db.function_signature_generic_params(generic_function.signature(db))?;
    if generic_params.len() != generic_args.len() {
        eprintln!("{generic_params:?}  {generic_args:?}");
        // TODO(spapini): Uphold the invariant that constructed ConcreteFunction instances
        //   always have the correct number of generic arguments.
        return Err(skip_diagnostic());
    }
    // TODO(spapini): When trait generics are supported, they need to be substituted
    //   one by one, not together.
    let function_subs = generic_params.into_iter().zip(generic_args.into_iter());
    let substitution = match generic_function {
        GenericFunctionId::Free(_) | GenericFunctionId::Extern(_) => {
            GenericSubstitution(function_subs.collect())
        }
        GenericFunctionId::Impl(id) => {
            let long_concrete_impl = db.lookup_intern_concrete_impl(id.concrete_impl);
            let generic_params = db.impl_generic_params(long_concrete_impl.impl_id)?;
            let generic_args = long_concrete_impl.generic_args;
            GenericSubstitution(
                chain!(function_subs, generic_params.into_iter().zip(generic_args.into_iter()))
                    .collect(),
            )
        }
        GenericFunctionId::Trait(id) => {
            let long_concrete_trait = db.lookup_intern_concrete_trait(id.concrete_trait_id(db));
            let generic_params = db.trait_generic_params(long_concrete_trait.trait_id)?;
            let generic_args = long_concrete_trait.generic_args;
            GenericSubstitution(
                chain!(function_subs, generic_params.into_iter().zip(generic_args.into_iter()))
                    .collect(),
            )
        }
    };
    let generic_signature = db.function_signature_signature(generic_function.signature(db))?;
    Ok(substitute_signature(db, substitution, generic_signature))
}

/// Substitutes a generic args in a generic signature.
pub fn substitute_signature(
    db: &dyn SemanticGroup,
    substitution: GenericSubstitution,
    generic_signature: Signature,
) -> Signature {
    let concretize_param = |param: semantic::Parameter| Parameter {
        id: param.id,
        name: param.name,
        ty: substitute_generics(db, &substitution, param.ty),
        mutability: param.mutability,
        stable_ptr: param.stable_ptr,
    };
    Signature {
        params: generic_signature.params.into_iter().map(concretize_param).collect(),
        return_type: substitute_generics(db, &substitution, generic_signature.return_type),
        implicits: generic_signature.implicits,
        panicable: generic_signature.panicable,
        stable_ptr: generic_signature.stable_ptr,
    }
}

/// For a given list of AST parameters, returns the list of semantic parameters along with the
/// corresponding environment.
fn update_env_with_ast_params(
    diagnostics: &mut SemanticDiagnostics,
    db: &dyn SemanticGroup,
    resolver: &mut Resolver<'_>,
    ast_params: &[ast::Param],
    function_signature_id: FunctionSignatureId,
    env: &mut Environment,
) -> Vec<semantic::Parameter> {
    let mut semantic_params = Vec::new();
    for ast_param in ast_params.iter() {
        let semantic_param = ast_param_to_semantic(diagnostics, db, resolver, ast_param);

        if env
            .add_param(diagnostics, semantic_param.clone(), ast_param, function_signature_id)
            .is_ok()
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
) -> semantic::Parameter {
    let syntax_db = db.upcast();

    let name = ast_param.name(syntax_db).text(syntax_db);

    let id = db.intern_param(ParamLongId(resolver.module_file_id, ast_param.stable_ptr()));
    let ty_syntax = ast_param.type_clause(syntax_db).ty(syntax_db);
    let ty = resolve_type(db, diagnostics, resolver, &ty_syntax);

    let mutability = modifiers::compute_mutability(
        diagnostics,
        syntax_db,
        &ast_param.modifiers(syntax_db).elements(syntax_db),
    );

    semantic::Parameter {
        id,
        name,
        ty,
        mutability,
        stable_ptr: ast_param.name(syntax_db).stable_ptr(),
    }
}

// === Function Declaration ===

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct FunctionDeclarationData {
    pub diagnostics: Diagnostics<SemanticDiagnostic>,
    pub signature: semantic::Signature,
    /// The environment induced by the function's signature.
    pub environment: Environment,
    pub generic_params: Vec<GenericParamId>,
    pub attributes: Vec<Attribute>,
    pub resolved_lookback: Arc<ResolvedLookback>,
}
