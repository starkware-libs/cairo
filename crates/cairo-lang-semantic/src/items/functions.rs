use std::fmt::Debug;
use std::sync::Arc;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::diagnostic_utils::StableLocation;
use cairo_lang_defs::ids::{
    ExternFunctionId, FreeFunctionId, FunctionTitleId, FunctionWithBodyId, ImplFunctionId,
    LanguageElementId, ModuleFileId, ModuleItemId, NamedLanguageElementId, ParamLongId,
    TopLevelLanguageElementId, TraitFunctionId,
};
use cairo_lang_diagnostics::{skip_diagnostic, Diagnostics, Maybe};
use cairo_lang_filesystem::ids::UnstableSalsaId;
use cairo_lang_proc_macros::{DebugWithDb, SemanticObject};
use cairo_lang_syntax as syntax;
use cairo_lang_syntax::attribute::structured::Attribute;
use cairo_lang_syntax::node::{ast, Terminal, TypedSyntaxNode};
use cairo_lang_utils::{define_short_id, try_extract_matches, OptionFrom};
use itertools::{chain, Itertools};
use smol_str::SmolStr;
use syntax::attribute::consts::{MUST_USE_ATTR, UNSTABLE_ATTR};
use syntax::node::TypedStablePtr;

use super::attribute::SemanticQueryAttrs;
use super::constant::ConstValue;
use super::imp::ImplId;
use super::modifiers;
use super::trt::ConcreteTraitGenericFunctionId;
use crate::corelib::unit_ty;
use crate::db::SemanticGroup;
use crate::diagnostic::{SemanticDiagnosticKind, SemanticDiagnostics};
use crate::expr::compute::Environment;
use crate::resolve::{Resolver, ResolverData};
use crate::substitution::{GenericSubstitution, SemanticRewriter, SubstitutionRewriter};
use crate::types::resolve_type;
use crate::{
    semantic, semantic_object_for_id, ConcreteImplId, ConcreteImplLongId, GenericArgumentId,
    GenericParam, SemanticDiagnostic, TypeId,
};

/// A generic function of an impl.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, SemanticObject)]
pub struct ImplGenericFunctionId {
    // TODO(spapini): Consider making these private and enforcing invariants in the ctor.
    /// The impl the function is in.
    pub impl_id: ImplId,
    /// The trait function this impl function implements.
    pub function: TraitFunctionId,
}
impl ImplGenericFunctionId {
    /// Gets the impl function language element, if self.impl_id is of a concrete impl.
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
        let Some(impl_function) = concrete_impl_id.get_impl_function(db.upcast(), self.function)?
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
impl DebugWithDb<dyn SemanticGroup> for ImplGenericFunctionId {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        db: &(dyn SemanticGroup + 'static),
    ) -> std::fmt::Result {
        write!(f, "{}", self.format(db))
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
    /// Returns the ModuleFileId of the function's definition if possible.
    pub fn module_file_id(&self, db: &dyn SemanticGroup) -> Option<ModuleFileId> {
        match self {
            GenericFunctionId::Free(free_function) => {
                Some(free_function.module_file_id(db.upcast()))
            }
            GenericFunctionId::Extern(extern_function) => {
                Some(extern_function.module_file_id(db.upcast()))
            }
            GenericFunctionId::Impl(impl_generic_function_id) => {
                // Return the module file of the impl containing the function.
                if let ImplId::Concrete(concrete_impl_id) = impl_generic_function_id.impl_id {
                    Some(concrete_impl_id.impl_def_id(db).module_file_id(db.upcast()))
                } else {
                    None
                }
            }
        }
    }
    /// Returns whether the function has the `#[must_use]` attribute.
    pub fn is_must_use(&self, db: &dyn SemanticGroup) -> Maybe<bool> {
        match self {
            GenericFunctionId::Free(id) => id.has_attr(db, MUST_USE_ATTR),
            GenericFunctionId::Impl(id) => id.function.has_attr(db, MUST_USE_ATTR),
            GenericFunctionId::Extern(_) => Ok(false),
        }
    }
    /// Returns the attribute if a function has the `#[unstable(feature: "some-string")]` attribute.
    pub fn unstable_feature(&self, db: &dyn SemanticGroup) -> Maybe<Option<Attribute>> {
        match self {
            GenericFunctionId::Free(id) => id.find_attr(db, UNSTABLE_ATTR),
            GenericFunctionId::Impl(id) => id.function.find_attr(db, UNSTABLE_ATTR),
            GenericFunctionId::Extern(_) => Ok(None),
        }
    }

    /// Returns true if the function does not depend on any generics.
    pub fn is_fully_concrete(&self, db: &dyn SemanticGroup) -> bool {
        match self {
            GenericFunctionId::Free(_) | GenericFunctionId::Extern(_) => true,
            GenericFunctionId::Impl(impl_generic_function) => {
                impl_generic_function.impl_id.is_fully_concrete(db)
            }
        }
    }
    /// Returns true if the function does not depend on impl or type variables.
    pub fn is_var_free(&self, db: &dyn SemanticGroup) -> bool {
        match self {
            GenericFunctionId::Free(_) | GenericFunctionId::Extern(_) => true,
            GenericFunctionId::Impl(impl_generic_function) => {
                impl_generic_function.impl_id.is_var_free(db)
            }
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
            | ModuleItemId::ImplAlias(_)
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
    pub fn lookup(&self, db: &dyn SemanticGroup) -> FunctionLongId {
        db.lookup_intern_function(*self)
    }

    pub fn get_concrete(&self, db: &dyn SemanticGroup) -> ConcreteFunction {
        self.lookup(db).function
    }

    /// Returns the ExternFunctionId if this is an extern function. Otherwise returns none.
    pub fn try_get_extern_function_id(&self, db: &dyn SemanticGroup) -> Option<ExternFunctionId> {
        try_extract_matches!(self.get_concrete(db).generic_function, GenericFunctionId::Extern)
    }

    pub fn name(&self, db: &dyn SemanticGroup) -> SmolStr {
        format!("{:?}", self.get_concrete(db).generic_function.name(db)).into()
    }

    pub fn full_name(&self, db: &dyn SemanticGroup) -> String {
        self.get_concrete(db).full_name(db)
    }

    /// Returns true if the function does not depend on any generics.
    pub fn is_fully_concrete(&self, db: &dyn SemanticGroup) -> bool {
        let func = self.get_concrete(db);
        func.generic_function.is_fully_concrete(db)
            && func
                .generic_args
                .iter()
                .all(|generic_argument_id| generic_argument_id.is_fully_concrete(db))
    }
    /// Returns true if the function does not depend on impl or type variables.
    pub fn is_var_free(&self, db: &dyn SemanticGroup) -> bool {
        let func = self.get_concrete(db);
        func.generic_function.is_var_free(db)
            && func
                .generic_args
                .iter()
                .all(|generic_argument_id| generic_argument_id.is_var_free(db))
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
                let Some(impl_function) =
                    db.impl_function_by_trait_function(concrete_impl_id.impl_def_id(db), function)?
                else {
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

    pub fn full_path(&self, db: &dyn SemanticGroup) -> String {
        let defs_db = db.upcast();
        match self {
            GenericFunctionWithBodyId::Free(free) => free.full_path(defs_db),
            GenericFunctionWithBodyId::Impl(imp) => format!(
                "{}::{}",
                imp.concrete_impl_id.impl_def_id(db).full_path(defs_db),
                imp.function.name(defs_db)
            ),
        }
    }
    pub fn stable_location(&self, db: &dyn SemanticGroup) -> StableLocation {
        match self {
            GenericFunctionWithBodyId::Free(free_function) => {
                free_function.stable_location(db.upcast())
            }
            GenericFunctionWithBodyId::Impl(impl_function) => {
                impl_function.function.stable_location(db.upcast())
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
    pub fn from_generic(db: &dyn SemanticGroup, function_id: FunctionWithBodyId) -> Maybe<Self> {
        Ok(match function_id {
            FunctionWithBodyId::Free(free) => {
                let params = db.free_function_generic_params(free)?;
                let generic_args = generic_params_to_args(params, db)?;
                ConcreteFunctionWithBody {
                    generic_function: GenericFunctionWithBodyId::Free(free),
                    generic_args,
                }
            }
            FunctionWithBodyId::Impl(impl_function_id) => {
                let params = db.impl_function_generic_params(impl_function_id)?;
                let generic_args = generic_params_to_args(params, db)?;
                let impl_def_id = impl_function_id.impl_def_id(db.upcast());
                let impl_def_params = db.impl_def_generic_params(impl_def_id)?;
                let impl_generic_args = generic_params_to_args(impl_def_params, db)?;
                let impl_generic_function = ImplGenericFunctionWithBodyId {
                    concrete_impl_id: db.intern_concrete_impl(ConcreteImplLongId {
                        impl_def_id,
                        generic_args: impl_generic_args,
                    }),
                    function: impl_function_id,
                };
                ConcreteFunctionWithBody {
                    generic_function: GenericFunctionWithBodyId::Impl(impl_generic_function),
                    generic_args,
                }
            }
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
    pub fn full_path(&self, db: &dyn SemanticGroup) -> String {
        self.generic_function.full_path(db)
    }
}

/// Converts each generic param to a generic argument that passes the same generic param.
fn generic_params_to_args(
    params: Vec<GenericParam>,
    db: &dyn SemanticGroup,
) -> Maybe<Vec<GenericArgumentId>> {
    params
        .into_iter()
        .map(|param| match param {
            GenericParam::Type(param) => Ok(GenericArgumentId::Type(
                db.intern_type(crate::TypeLongId::GenericParameter(param.id)),
            )),
            GenericParam::Const(param) => Ok(GenericArgumentId::Constant(
                db.intern_const_value(ConstValue::Generic(param.id)),
            )),
            GenericParam::Impl(param) => {
                Ok(GenericArgumentId::Impl(ImplId::GenericParameter(param.id)))
            }
            GenericParam::NegImpl(_) => Ok(GenericArgumentId::NegImpl),
        })
        .collect::<Maybe<Vec<_>>>()
}

impl DebugWithDb<dyn SemanticGroup> for ConcreteFunctionWithBody {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        db: &(dyn SemanticGroup + 'static),
    ) -> std::fmt::Result {
        write!(f, "{:?}", self.generic_function.name(db.upcast()))?;
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
    pub fn from_generic(db: &dyn SemanticGroup, function_id: FunctionWithBodyId) -> Maybe<Self> {
        Ok(db.intern_concrete_function_with_body(ConcreteFunctionWithBody::from_generic(
            db,
            function_id,
        )?))
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
    pub fn full_path(&self, db: &dyn SemanticGroup) -> String {
        self.get(db).full_path(db)
    }

    pub fn stable_location(&self, db: &dyn SemanticGroup) -> StableLocation {
        self.get(db).generic_function.stable_location(db)
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
    pub fn body(&self, db: &dyn SemanticGroup) -> Maybe<Option<ConcreteFunctionWithBodyId>> {
        let Some(generic_function) =
            GenericFunctionWithBodyId::from_generic(db, self.generic_function)?
        else {
            return Ok(None);
        };
        Ok(Some(db.intern_concrete_function_with_body(ConcreteFunctionWithBody {
            generic_function,
            generic_args: self.generic_args.clone(),
        })))
    }
    pub fn full_name(&self, db: &dyn SemanticGroup) -> String {
        let maybe_generic_part = if !self.generic_args.is_empty() {
            let mut generics = String::new();
            for (i, arg) in self.generic_args.iter().enumerate() {
                if i > 0 {
                    generics.push_str(", ");
                }
                generics.push_str(&arg.format(db));
            }
            format!("::<{generics}>")
        } else {
            "".to_string()
        };
        format!("{}{maybe_generic_part}", self.generic_function.format(db.upcast()))
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
        let params = function_signature_params(
            diagnostics,
            db,
            resolver,
            signature_syntax,
            function_title_id,
            environment,
        );
        let return_type =
            function_signature_return_type(diagnostics, db, resolver, signature_syntax);
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
    pub resolver_data: Arc<ResolverData>,
    pub inline_config: InlineConfiguration,
    /// Order of implicits to follow by this function.
    ///
    /// For example, this can be used to enforce ABI compatibility with Starknet OS.
    pub implicit_precedence: ImplicitPrecedence,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum InlineConfiguration {
    /// The user did not specify any inlining preferences.
    None,
    Always(Attribute),
    Should(Attribute),
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

/// Extra information about sorting of implicit arguments of the function.
///
/// In most of the user written code, the implicits are not stated explicitly, but instead are
/// inferred by the compiler. The order on how these implicit arguments are laid out on Sierra level
/// is unspecified though for the users. Currently, the compiler sorts them alphabetically by name
/// for reproducibility, but it can equally just randomize the order on each compilation.
///
/// Some compilation targets tend to expect that particular functions accept particular implicit
/// arguments at fixed positions. For example, the Starknet OS has such assumptions. By reading the
/// implicit precedence information attached to functions, the compiler can now reliably generate
/// compatible code.
///
/// To set, add the `#[implicit_precedence(...)]` attribute to function declaration. Only free or
/// impl functions can have this information defined. For extern functions, the compiler raises an
/// error. It is recommended to always create this attribute from compiler plugins, and not force
/// users to write it manually.
///
/// Use [ImplicitPrecedence::UNSPECIFIED] to represent lack of information.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct ImplicitPrecedence(Vec<TypeId>);

impl ImplicitPrecedence {
    /// A precedence that does not actually prefer any implicit.
    ///
    /// When applied to a sequence of implicits, they will just be reordered alphabetically.
    pub const UNSPECIFIED: Self = Self(Vec::new());

    /// Sort implicits according to this precedence: first the ones with precedence
    /// (according to it), then the others by their name.
    pub fn apply(&self, implicits: &mut [TypeId], db: &dyn SemanticGroup) {
        implicits.sort_by_cached_key(|implicit| {
            if let Some(idx) = self.0.iter().position(|item| item == implicit) {
                return (idx, "".to_string());
            }

            (self.0.len(), implicit.format(db))
        });
    }
}

impl FromIterator<TypeId> for ImplicitPrecedence {
    fn from_iter<T: IntoIterator<Item = TypeId>>(iter: T) -> Self {
        Self(Vec::from_iter(iter))
    }
}
