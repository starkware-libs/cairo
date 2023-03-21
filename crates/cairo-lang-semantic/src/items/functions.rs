use std::sync::Arc;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::{
    ExternFunctionId, FreeFunctionId, FunctionTitleId, FunctionWithBodyId, ImplFunctionId,
    ModuleItemId, ParamLongId, TopLevelLanguageElementId, TraitFunctionId, UnstableSalsaId,
};
use cairo_lang_diagnostics::{skip_diagnostic, Diagnostics, Maybe};
use cairo_lang_proc_macros::{DebugWithDb, SemanticObject};
use cairo_lang_syntax as syntax;
use cairo_lang_syntax::node::{ast, Terminal, TypedSyntaxNode};
use cairo_lang_utils::{define_short_id, try_extract_matches, OptionFrom};
use itertools::{chain, Itertools};
use smol_str::SmolStr;

use super::attribute::Attribute;
use super::imp::ImplId;
use super::modifiers;
use super::trt::ConcreteTraitGenericFunctionId;
use crate::corelib::unit_ty;
use crate::db::SemanticGroup;
use crate::diagnostic::{SemanticDiagnosticKind, SemanticDiagnostics};
use crate::expr::compute::Environment;
use crate::resolve_path::{ResolvedLookback, Resolver};
use crate::substitution::{GenericSubstitution, SemanticRewriter, SubstitutionRewriter};
use crate::types::resolve_type;
use crate::{semantic, semantic_object_for_id, ConcreteImplId, GenericParam, SemanticDiagnostic};

/// A generic function of an impl.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, SemanticObject)]
pub struct ImplGenericFunctionId {
    // TODO(spapini): Consider making these private and enforcing invariants in the ctor.
    pub impl_id: ImplId,
    pub function: TraitFunctionId,
}
impl ImplGenericFunctionId {
    pub fn impl_function(&self, db: &dyn SemanticGroup) -> Maybe<Option<ImplFunctionId>> {
        match self.impl_id {
            ImplId::Concrete(concrete_impl_id) => {
                concrete_impl_id.get_impl_function(db, self.function)
            }
            ImplId::GenericParameter(_) | ImplId::ImplVar(_) => Ok(None),
        }
    }
    /// Converts to ImplGenericFunctionWithBodyId if this is a function of a concrete impl.
    pub fn to_impl_generic_with_body(
        &self,
        db: &dyn SemanticGroup,
    ) -> Maybe<Option<ImplGenericFunctionWithBodyId>> {
        let ImplId::Concrete(concrete_impl_id) = self.impl_id else {
                return Ok(None);
            };
        let Some(impl_function) = concrete_impl_id
                .get_impl_function(db.upcast(), self.function)?
                else {
                    // Trait function not found in impl.
                    return Err(skip_diagnostic());
                };
        Ok(Some(ImplGenericFunctionWithBodyId { concrete_impl_id, function: impl_function }))
    }
    /// Converts to GenericFunctionWithBodyId if this is a function of a concrete impl.
    pub fn to_generic_with_body(
        &self,
        db: &dyn SemanticGroup,
    ) -> Maybe<Option<GenericFunctionWithBodyId>> {
        let Some(impl_generic_with_body) = self.to_impl_generic_with_body(db)? else {
                return Ok(None);
            };
        Ok(Some(GenericFunctionWithBodyId::Impl(impl_generic_with_body)))
    }
    pub fn format(&self, db: &dyn SemanticGroup) -> SmolStr {
        format!("{}::{}", self.impl_id.name(db.upcast()), self.function.name(db.upcast())).into()
    }
}

/// The ID of a generic function that can be concretized.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, SemanticObject)]
pub enum GenericFunctionId {
    /// A generic free function.
    Free(FreeFunctionId),
    /// A generic extern function.
    Extern(ExternFunctionId),
    /// A generic function of an impl.
    Impl(ImplGenericFunctionId),
}
impl GenericFunctionId {
    pub fn from_generic_with_body(
        db: &dyn SemanticGroup,
        val: GenericFunctionWithBodyId,
    ) -> Maybe<Self> {
        Ok(match val {
            GenericFunctionWithBodyId::Free(id) => GenericFunctionId::Free(id),
            GenericFunctionWithBodyId::Impl(id) => GenericFunctionId::Impl(ImplGenericFunctionId {
                impl_id: ImplId::Concrete(id.concrete_impl_id),
                function: db.impl_function_trait_function(id.function)?,
            }),
        })
    }
    pub fn format(&self, db: &dyn SemanticGroup) -> String {
        let defs_db = db.upcast();
        match self {
            GenericFunctionId::Free(id) => id.full_path(defs_db),
            GenericFunctionId::Extern(id) => id.full_path(defs_db),
            GenericFunctionId::Impl(id) => {
                format!("{:?}::{}", id.impl_id.debug(db.elongate()), id.function.name(defs_db))
            }
        }
    }
    pub fn generic_signature(&self, db: &dyn SemanticGroup) -> Maybe<Signature> {
        match *self {
            GenericFunctionId::Free(id) => db.free_function_signature(id),
            GenericFunctionId::Extern(id) => db.extern_function_signature(id),
            GenericFunctionId::Impl(id) => {
                let concrete_trait_id = db.impl_concrete_trait(id.impl_id)?;
                let id = ConcreteTraitGenericFunctionId::new(db, concrete_trait_id, id.function);

                db.concrete_trait_function_signature(id)
            }
        }
    }
    pub fn generic_params(&self, db: &dyn SemanticGroup) -> Maybe<Vec<GenericParam>> {
        match *self {
            GenericFunctionId::Free(id) => db.free_function_generic_params(id),
            GenericFunctionId::Extern(id) => db.extern_function_declaration_generic_params(id),
            GenericFunctionId::Impl(id) => {
                let concrete_trait_id = db.impl_concrete_trait(id.impl_id)?;
                let id = ConcreteTraitGenericFunctionId::new(db, concrete_trait_id, id.function);
                db.concrete_trait_function_generic_params(id)
            }
        }
    }
    pub fn name(&self, db: &dyn SemanticGroup) -> SmolStr {
        match self {
            GenericFunctionId::Free(free_function) => free_function.name(db.upcast()),
            GenericFunctionId::Extern(extern_function) => extern_function.name(db.upcast()),
            GenericFunctionId::Impl(impl_function) => impl_function.format(db.upcast()),
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
#[derive(Clone, Debug, Hash, PartialEq, Eq, SemanticObject)]
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
semantic_object_for_id!(FunctionId, lookup_intern_function, intern_function, FunctionLongId);
impl FunctionId {
    pub fn get_concrete(&self, db: &dyn SemanticGroup) -> ConcreteFunction {
        db.lookup_intern_function(*self).function
    }

    /// Returns the ExternFunctionId if this is an extern function. Otherwise returns none.
    pub fn try_get_extern_function_id(&self, db: &dyn SemanticGroup) -> Option<ExternFunctionId> {
        try_extract_matches!(self.get_concrete(db).generic_function, GenericFunctionId::Extern)
    }

    /// Returns the FunctionWithBodyId if this is a function with body, otherwise returns None.
    pub fn try_get_function_with_body_id(
        &self,
        db: &dyn SemanticGroup,
    ) -> Maybe<Option<FunctionWithBodyId>> {
        Ok(match self.get_concrete(db).generic_function {
            GenericFunctionId::Free(free_function_id) => {
                Some(FunctionWithBodyId::Free(free_function_id))
            }
            GenericFunctionId::Impl(impl_generic_function_id) => {
                impl_generic_function_id.impl_function(db)?.map(FunctionWithBodyId::Impl)
            }
            GenericFunctionId::Extern(_) => None,
        })
    }

    pub fn name(&self, db: &dyn SemanticGroup) -> SmolStr {
        format!("{:?}", self.get_concrete(db)).into()
    }
}

/// A generic function of a concrete impl.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, SemanticObject)]
pub struct ImplGenericFunctionWithBodyId {
    pub concrete_impl_id: ConcreteImplId,
    pub function: ImplFunctionId,
}

/// The ID of a generic function with body that can be concretized.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, SemanticObject)]
pub enum GenericFunctionWithBodyId {
    Free(FreeFunctionId),
    Impl(ImplGenericFunctionWithBodyId),
}
impl GenericFunctionWithBodyId {
    pub fn from_generic(db: &dyn SemanticGroup, other: GenericFunctionId) -> Maybe<Option<Self>> {
        Ok(Some(match other {
            GenericFunctionId::Free(id) => GenericFunctionWithBodyId::Free(id),
            GenericFunctionId::Impl(ImplGenericFunctionId {
                impl_id: ImplId::Concrete(concrete_impl_id),
                function,
            }) => {
                let Some(impl_function) = db.impl_function_by_trait_function(
                    concrete_impl_id.impl_def_id(db),
                    function,
                )? else {
                    return Ok(None);
                };
                GenericFunctionWithBodyId::Impl(ImplGenericFunctionWithBodyId {
                    concrete_impl_id,
                    function: impl_function,
                })
            }
            _ => return Ok(None),
        }))
    }
    pub fn name(&self, db: &dyn SemanticGroup) -> SmolStr {
        match self {
            GenericFunctionWithBodyId::Free(free) => free.name(db.upcast()),
            GenericFunctionWithBodyId::Impl(imp) => {
                format!("{}::{}", imp.concrete_impl_id.name(db), imp.function.name(db.upcast()))
                    .into()
            }
        }
    }
}

/// A long Id of a concrete function with body.
#[derive(Clone, Debug, Hash, PartialEq, Eq, SemanticObject)]
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
            GenericFunctionWithBodyId::Free(f) => {
                GenericSubstitution::new(&db.free_function_generic_params(f)?, &self.generic_args)
            }
            GenericFunctionWithBodyId::Impl(f) => {
                let concrete_impl = db.lookup_intern_concrete_impl(f.concrete_impl_id);
                GenericSubstitution::new(
                    &chain!(
                        db.impl_function_generic_params(f.function)?,
                        db.impl_def_generic_params(concrete_impl.impl_def_id)?
                    )
                    .collect_vec(),
                    &chain!(
                        self.generic_args.iter().copied(),
                        concrete_impl.generic_args.iter().copied()
                    )
                    .collect_vec(),
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
    pub fn concrete(&self, db: &dyn SemanticGroup) -> Maybe<ConcreteFunction> {
        Ok(ConcreteFunction {
            generic_function: GenericFunctionId::from_generic_with_body(db, self.generic_function)?,
            generic_args: self.generic_args.clone(),
        })
    }
    pub fn function_id(&self, db: &dyn SemanticGroup) -> Maybe<FunctionId> {
        Ok(db.intern_function(FunctionLongId { function: self.concrete(db)? }))
    }
    pub fn name(&self, db: &dyn SemanticGroup) -> SmolStr {
        self.function_with_body_id().name(db.upcast())
    }
}

define_short_id!(
    ConcreteFunctionWithBodyId,
    ConcreteFunctionWithBody,
    SemanticGroup,
    lookup_intern_concrete_function_with_body
);
semantic_object_for_id!(
    ConcreteFunctionWithBodyId,
    lookup_intern_concrete_function_with_body,
    intern_concrete_function_with_body,
    ConcreteFunctionWithBody
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
    pub fn concrete(&self, db: &dyn SemanticGroup) -> Maybe<ConcreteFunction> {
        self.get(db).concrete(db)
    }
    pub fn function_id(&self, db: &dyn SemanticGroup) -> Maybe<FunctionId> {
        self.get(db).function_id(db)
    }
    pub fn generic_function(&self, db: &dyn SemanticGroup) -> GenericFunctionWithBodyId {
        self.get(db).generic_function
    }
    pub fn name(&self, db: &dyn SemanticGroup) -> SmolStr {
        self.get(db).name(db)
    }
}

impl UnstableSalsaId for ConcreteFunctionWithBodyId {
    fn get_internal_id(&self) -> &salsa::InternId {
        &self.0
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, SemanticObject)]
pub struct ConcreteFunction {
    pub generic_function: GenericFunctionId,
    pub generic_args: Vec<semantic::GenericArgumentId>,
}
impl ConcreteFunction {
    pub fn get_body(&self, db: &dyn SemanticGroup) -> Maybe<Option<ConcreteFunctionWithBodyId>> {
        let Some(generic_function) = GenericFunctionWithBodyId::from_generic(
            db,
            self.generic_function,
        )? else {
            return Ok(None);
        };
        Ok(Some(db.intern_concrete_function_with_body(ConcreteFunctionWithBody {
            generic_function,
            generic_args: self.generic_args.clone(),
        })))
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
            write!(f, "::<")?;
            for (i, arg) in self.generic_args.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{:?}", arg.debug(db))?;
            }
            write!(f, ">")?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct Signature {
    pub params: Vec<semantic::Parameter>,
    pub return_type: semantic::TypeId,
    /// implicit parameters
    pub implicits: Vec<semantic::TypeId>,
    #[dont_rewrite]
    pub panicable: bool,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::FunctionSignaturePtr,
}

impl Signature {
    pub fn from_ast(
        diagnostics: &mut SemanticDiagnostics,
        db: &dyn SemanticGroup,
        resolver: &mut Resolver<'_>,
        signature_syntax: &ast::FunctionSignature,
        function_title_id: FunctionTitleId,
        environment: &mut Environment,
    ) -> Self {
        let return_type =
            function_signature_return_type(diagnostics, db, resolver, signature_syntax);
        let params = function_signature_params(
            diagnostics,
            db,
            resolver,
            signature_syntax,
            function_title_id,
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
    function_title_id: FunctionTitleId,
    env: &mut Environment,
) -> Vec<semantic::Parameter> {
    let syntax_db = db.upcast();
    update_env_with_ast_params(
        diagnostics,
        db,
        resolver,
        &sig.parameters(syntax_db).elements(syntax_db),
        function_title_id,
        env,
    )
}

/// Query implementation of [crate::db::SemanticGroup::function_title_signature].
pub fn function_title_signature(
    db: &dyn SemanticGroup,
    function_title_id: FunctionTitleId,
) -> Maybe<Signature> {
    match function_title_id {
        FunctionTitleId::Free(free_function) => db.free_function_signature(free_function),
        FunctionTitleId::Extern(extern_function) => db.extern_function_signature(extern_function),
        FunctionTitleId::Trait(trait_function) => db.trait_function_signature(trait_function),
        FunctionTitleId::Impl(impl_function) => db.impl_function_signature(impl_function),
    }
}
/// Query implementation of [crate::db::SemanticGroup::function_title_generic_params].
pub fn function_title_generic_params(
    db: &dyn SemanticGroup,
    function_title_id: FunctionTitleId,
) -> Maybe<Vec<semantic::GenericParam>> {
    match function_title_id {
        FunctionTitleId::Free(free_function) => db.free_function_generic_params(free_function),
        FunctionTitleId::Extern(extern_function) => {
            db.extern_function_declaration_generic_params(extern_function)
        }
        FunctionTitleId::Trait(trait_function) => db.trait_function_generic_params(trait_function),
        FunctionTitleId::Impl(impl_function) => db.impl_function_generic_params(impl_function),
    }
}

/// Query implementation of [crate::db::SemanticGroup::concrete_function_signature].
pub fn concrete_function_signature(
    db: &dyn SemanticGroup,
    function_id: FunctionId,
) -> Maybe<Signature> {
    let ConcreteFunction { generic_function, generic_args, .. } =
        db.lookup_intern_function(function_id).function;
    let generic_params = generic_function.generic_params(db)?;
    let generic_signature = generic_function.generic_signature(db)?;
    // TODO(spapini): When trait generics are supported, they need to be substituted
    //   one by one, not together.
    // Panic shouldn't occur since ConcreteFunction is assumed to be constructed correctly.
    let substitution = GenericSubstitution::new(&generic_params, &generic_args);
    SubstitutionRewriter { db, substitution: &substitution }.rewrite(generic_signature)
}

/// For a given list of AST parameters, returns the list of semantic parameters along with the
/// corresponding environment.
fn update_env_with_ast_params(
    diagnostics: &mut SemanticDiagnostics,
    db: &dyn SemanticGroup,
    resolver: &mut Resolver<'_>,
    ast_params: &[ast::Param],
    function_title_id: FunctionTitleId,
    env: &mut Environment,
) -> Vec<semantic::Parameter> {
    let mut semantic_params = Vec::new();
    for ast_param in ast_params.iter() {
        let semantic_param = ast_param_to_semantic(diagnostics, db, resolver, ast_param);

        if env.add_param(diagnostics, semantic_param.clone(), ast_param, function_title_id).is_ok()
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
    pub generic_params: Vec<semantic::GenericParam>,
    pub attributes: Vec<Attribute>,
    pub resolved_lookback: Arc<ResolvedLookback>,
    pub inline_config: InlineConfiguration,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum InlineConfiguration {
    /// The user did not specify any inlining preferences.
    None,
    Always(Attribute),
    Never(Attribute),
}

/// If a function with impl generic parameters is marked as '#[inline(always)]', raise a diagnostic.
pub fn forbid_inline_always_with_impl_generic_param(
    diagnostics: &mut SemanticDiagnostics,
    generic_params: &[GenericParam],
    inline_config: &InlineConfiguration,
) {
    let has_impl_generic_param = generic_params.iter().any(|p| matches!(p, GenericParam::Impl(_)));
    match &inline_config {
        InlineConfiguration::Always(attr) if has_impl_generic_param => {
            diagnostics.report_by_ptr(
                attr.stable_ptr.untyped(),
                SemanticDiagnosticKind::InlineAlwaysWithImplGenericArgNotAllowed,
            );
        }
        _ => {}
    }
}
