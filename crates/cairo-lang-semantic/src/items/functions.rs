use std::fmt::Write;
use std::sync::Arc;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::diagnostic_utils::StableLocation;
use cairo_lang_defs::ids::{
    ExternFunctionId, FreeFunctionId, FunctionTitleId, FunctionWithBodyId, ImplFunctionId,
    LanguageElementId, ModuleId, ModuleItemId, NamedLanguageElementId, ParamLongId,
    TopLevelLanguageElementId, TraitFunctionId,
};
use cairo_lang_diagnostics::{Diagnostics, Maybe, MaybeAsRef};
use cairo_lang_filesystem::ids::{SmolStrId, Tracked, UnstableSalsaId};
use cairo_lang_proc_macros::{DebugWithDb, SemanticObject};
use cairo_lang_syntax as syntax;
use cairo_lang_syntax::attribute::structured::Attribute;
use cairo_lang_syntax::node::{Terminal, TypedSyntaxNode, ast};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::{Intern, OptionFrom, define_short_id, require, try_extract_matches};
use itertools::{Itertools, chain};
use salsa::Database;
use syntax::attribute::consts::MUST_USE_ATTR;
use syntax::node::TypedStablePtr;

use super::attribute::SemanticQueryAttrs;
use super::generics::{fmt_generic_args, generic_params_to_args};
use super::imp::{ImplId, ImplLongId};
use super::modifiers;
use super::trt::ConcreteTraitGenericFunctionId;
use crate::corelib::{CorelibSemantic, fn_traits, unit_ty};
use crate::diagnostic::{SemanticDiagnosticKind, SemanticDiagnostics, SemanticDiagnosticsBuilder};
use crate::expr::compute::Environment;
use crate::expr::fmt::CountingWriter;
use crate::items::extern_function::ExternFunctionSemantic;
use crate::items::free_function::FreeFunctionSemantic;
use crate::items::imp::ImplSemantic;
use crate::items::trt::TraitSemantic;
use crate::resolve::{Resolver, ResolverData};
use crate::substitution::GenericSubstitution;
use crate::types::resolve_type;
use crate::{
    ConcreteImplId, ConcreteImplLongId, ConcreteTraitLongId, GenericArgumentId, GenericParam,
    SemanticDiagnostic, TypeId, semantic, semantic_object_for_id,
};

/// A generic function of an impl.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, SemanticObject, salsa::Update)]
pub struct ImplGenericFunctionId<'db> {
    // TODO(spapini): Consider making these private and enforcing invariants in the ctor.
    /// The impl the function is in.
    pub impl_id: ImplId<'db>,
    /// The trait function this impl function implements.
    pub function: TraitFunctionId<'db>,
}
impl<'db> ImplGenericFunctionId<'db> {
    /// Gets the impl function language element, if self.impl_id is of a concrete impl.
    pub fn impl_function(&self, db: &'db dyn Database) -> Maybe<Option<ImplFunctionId<'db>>> {
        match self.impl_id.long(db) {
            ImplLongId::Concrete(concrete_impl_id) => {
                concrete_impl_id.get_impl_function(db, self.function)
            }
            ImplLongId::GenericParameter(_)
            | ImplLongId::ImplVar(_)
            | ImplLongId::ImplImpl(_)
            | ImplLongId::SelfImpl(_)
            | ImplLongId::GeneratedImpl(_) => Ok(None),
        }
    }
    pub fn format(&self, db: &dyn Database) -> String {
        format!("{}::{}", self.impl_id.name(db), self.function.name(db).long(db))
    }
}
impl<'db> DebugWithDb<'db> for ImplGenericFunctionId<'db> {
    type Db = dyn Database;

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db dyn Database) -> std::fmt::Result {
        write!(f, "{}", self.format(db))
    }
}

/// The ID of a generic function that can be concretized.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, SemanticObject, salsa::Update)]
pub enum GenericFunctionId<'db> {
    /// A generic free function.
    Free(FreeFunctionId<'db>),
    /// A generic extern function.
    Extern(ExternFunctionId<'db>),
    /// A generic function of an impl.
    Impl(ImplGenericFunctionId<'db>),
}
impl<'db> GenericFunctionId<'db> {
    pub fn from_generic_with_body(
        db: &'db dyn Database,
        val: GenericFunctionWithBodyId<'db>,
    ) -> Maybe<Self> {
        Ok(match val {
            GenericFunctionWithBodyId::Free(id) => GenericFunctionId::Free(id),
            GenericFunctionWithBodyId::Impl(id) => {
                let impl_id = ImplLongId::Concrete(id.concrete_impl_id).intern(db);
                let function = match id.function_body {
                    ImplFunctionBodyId::Impl(body_id) => {
                        db.impl_function_trait_function(body_id)?
                    }
                    ImplFunctionBodyId::Trait(body_id) => body_id,
                };
                GenericFunctionId::Impl(ImplGenericFunctionId { impl_id, function })
            }
            GenericFunctionWithBodyId::Trait(id) => {
                GenericFunctionId::Impl(ImplGenericFunctionId {
                    impl_id: ImplLongId::SelfImpl(id.concrete_trait(db)).intern(db),
                    function: id.trait_function(db),
                })
            }
        })
    }
    pub fn format(&self, db: &dyn Database) -> String {
        match self {
            GenericFunctionId::Free(id) => id.full_path(db),
            GenericFunctionId::Extern(id) => id.full_path(db),
            GenericFunctionId::Impl(id) => {
                format!("{:?}::{}", id.impl_id.debug(db), id.function.name(db).long(db))
            }
        }
    }
    pub fn generic_signature(&self, db: &'db dyn Database) -> Maybe<&'db Signature<'db>> {
        match *self {
            GenericFunctionId::Free(id) => db.free_function_signature(id),
            GenericFunctionId::Extern(id) => db.extern_function_signature(id),
            GenericFunctionId::Impl(id) => {
                #[salsa::tracked(returns(ref))]
                fn impl_function_signature_tracked<'db>(
                    db: &'db dyn Database,
                    impl_id: ImplId<'db>,
                    function: TraitFunctionId<'db>,
                ) -> Maybe<Signature<'db>> {
                    let concrete_trait_id = impl_id.concrete_trait(db)?;
                    let signature = db.concrete_trait_function_signature(
                        ConcreteTraitGenericFunctionId::new_from_data(
                            db,
                            concrete_trait_id,
                            function,
                        ),
                    )?;
                    GenericSubstitution::from_impl(impl_id).substitute(db, signature.clone())
                }
                impl_function_signature_tracked(db, id.impl_id, id.function).maybe_as_ref()
            }
        }
    }
    pub fn generic_params(&self, db: &'db dyn Database) -> Maybe<&'db [GenericParam<'db>]> {
        match *self {
            GenericFunctionId::Free(id) => db.free_function_generic_params(id),
            GenericFunctionId::Extern(id) => db.extern_function_declaration_generic_params(id),
            GenericFunctionId::Impl(id) => {
                #[salsa::tracked(returns(ref))]
                fn impl_function_generic_params_tracked<'db>(
                    db: &'db dyn Database,
                    impl_id: ImplId<'db>,
                    trait_function: TraitFunctionId<'db>,
                ) -> Maybe<Vec<GenericParam<'db>>> {
                    let concrete_trait_id = db.impl_concrete_trait(impl_id)?;
                    let concrete_id = ConcreteTraitGenericFunctionId::new_from_data(
                        db,
                        concrete_trait_id,
                        trait_function,
                    );
                    GenericSubstitution::from_impl(impl_id).substitute(
                        db,
                        db.concrete_trait_function_generic_params(concrete_id)?.to_vec(),
                    )
                }
                Ok(impl_function_generic_params_tracked(db, id.impl_id, id.function)
                    .maybe_as_ref()?)
            }
        }
    }
    pub fn name(&self, db: &dyn Database) -> String {
        match self {
            GenericFunctionId::Free(free_function) => free_function.name(db).to_string(db),
            GenericFunctionId::Extern(extern_function) => extern_function.name(db).to_string(db),
            GenericFunctionId::Impl(impl_function) => impl_function.format(db),
        }
    }
    /// Returns the ModuleId of the function's definition if possible.
    pub fn module_id(&self, db: &'db dyn Database) -> Option<ModuleId<'db>> {
        match self {
            GenericFunctionId::Free(free_function) => Some(free_function.module_id(db)),
            GenericFunctionId::Extern(extern_function) => Some(extern_function.module_id(db)),
            GenericFunctionId::Impl(impl_generic_function_id) => {
                // Return the module file of the impl containing the function.
                if let ImplLongId::Concrete(concrete_impl_id) =
                    impl_generic_function_id.impl_id.long(db)
                {
                    Some(concrete_impl_id.impl_def_id(db).module_id(db))
                } else {
                    None
                }
            }
        }
    }
    /// Returns whether the function has the `#[must_use]` attribute.
    pub fn is_must_use(&self, db: &dyn Database) -> Maybe<bool> {
        match self {
            GenericFunctionId::Free(id) => id.has_attr(db, MUST_USE_ATTR),
            GenericFunctionId::Impl(id) => id.function.has_attr(db, MUST_USE_ATTR),
            GenericFunctionId::Extern(_) => Ok(false),
        }
    }
    /// Returns true if the function does not depend on any generics.
    pub fn is_fully_concrete(&self, db: &dyn Database) -> bool {
        match self {
            GenericFunctionId::Free(_) | GenericFunctionId::Extern(_) => true,
            GenericFunctionId::Impl(impl_generic_function) => {
                impl_generic_function.impl_id.is_fully_concrete(db)
            }
        }
    }
    /// Returns true if the function does not depend on impl or type variables.
    pub fn is_var_free(&self, db: &dyn Database) -> bool {
        match self {
            GenericFunctionId::Free(_) | GenericFunctionId::Extern(_) => true,
            GenericFunctionId::Impl(impl_generic_function) => {
                impl_generic_function.impl_id.is_var_free(db)
            }
        }
    }
    /// Returns the concrete function of this generic function with the given generic args.
    pub fn concretize(
        &self,
        db: &'db dyn Database,
        generic_args: Vec<semantic::GenericArgumentId<'db>>,
    ) -> FunctionId<'db> {
        FunctionLongId { function: ConcreteFunction { generic_function: *self, generic_args } }
            .intern(db)
    }
}
/// Conversion from ModuleItemId to GenericFunctionId.
impl<'db> OptionFrom<ModuleItemId<'db>> for GenericFunctionId<'db> {
    fn option_from(item: ModuleItemId<'db>) -> Option<Self> {
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
            | ModuleItemId::ExternType(_)
            | ModuleItemId::MacroDeclaration(_) => None,
        }
    }
}
impl<'db> DebugWithDb<'db> for GenericFunctionId<'db> {
    type Db = dyn Database;

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db dyn Database) -> std::fmt::Result {
        match self {
            GenericFunctionId::Free(func) => write!(f, "{:?}", func.debug(db)),
            GenericFunctionId::Extern(func) => write!(f, "{:?}", func.debug(db)),
            GenericFunctionId::Impl(func) => write!(f, "{:?}", func.debug(db)),
        }
    }
}

/// Function instance.
/// For example: `ImplA::foo<A, B>`, or `bar<A>`.
// TODO(spapini): Make it an enum and add a function pointer variant.
#[derive(Clone, Debug, Hash, PartialEq, Eq, SemanticObject)]
pub struct FunctionLongId<'db> {
    pub function: ConcreteFunction<'db>,
}
impl<'db> DebugWithDb<'db> for FunctionLongId<'db> {
    type Db = dyn Database;

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &dyn Database) -> std::fmt::Result {
        write!(f, "{:?}", self.function.debug(db))
    }
}

define_short_id!(FunctionId, FunctionLongId<'db>);
semantic_object_for_id!(FunctionId, FunctionLongId<'a>);
impl<'db> FunctionId<'db> {
    pub fn get_concrete(&self, db: &'db dyn Database) -> ConcreteFunction<'db> {
        self.long(db).function.clone()
    }

    /// Returns the ExternFunctionId if this is an extern function. Otherwise returns none.
    pub fn try_get_extern_function_id(
        &self,
        db: &'db dyn Database,
    ) -> Option<ExternFunctionId<'db>> {
        try_extract_matches!(self.get_concrete(db).generic_function, GenericFunctionId::Extern)
    }

    pub fn name(&self, db: &dyn Database) -> String {
        format!("{:?}", self.get_concrete(db).generic_function.name(db))
    }

    pub fn full_path(&self, db: &dyn Database) -> String {
        self.get_concrete(db).full_path(db)
    }

    /// Returns true if the function does not depend on any generics.
    pub fn is_fully_concrete(&self, db: &dyn Database) -> bool {
        let func = self.get_concrete(db);
        func.generic_function.is_fully_concrete(db)
            && func
                .generic_args
                .iter()
                .all(|generic_argument_id| generic_argument_id.is_fully_concrete(db))
    }
    /// Returns true if the function does not depend on impl or type variables.
    pub fn is_var_free(&self, db: &dyn Database) -> bool {
        let func = self.get_concrete(db);
        func.generic_function.is_var_free(db)
            && func
                .generic_args
                .iter()
                .all(|generic_argument_id| generic_argument_id.is_var_free(db))
    }
}
impl<'db> FunctionLongId<'db> {
    pub fn from_generic(
        db: &'db dyn Database,
        generic_function: GenericFunctionId<'db>,
    ) -> Maybe<Self> {
        let generic_params = generic_function.generic_params(db)?;

        Ok(FunctionLongId {
            function: ConcreteFunction {
                generic_function,
                generic_args: generic_params_to_args(generic_params, db),
            },
        })
    }
}

/// A generic function of a concrete impl.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, SemanticObject)]
pub struct ImplGenericFunctionWithBodyId<'db> {
    pub concrete_impl_id: ConcreteImplId<'db>,
    pub function_body: ImplFunctionBodyId<'db>,
}

/// The body of an impl function implementation.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, SemanticObject)]
pub enum ImplFunctionBodyId<'db> {
    /// A function that was implemented in the impl.
    Impl(ImplFunctionId<'db>),
    /// The default implementation of a trait function in the trait.
    Trait(TraitFunctionId<'db>),
}
impl<'db> ImplFunctionBodyId<'db> {
    pub fn name(&self, db: &'db dyn Database) -> SmolStrId<'db> {
        match self {
            Self::Impl(body_id) => body_id.name(db),
            Self::Trait(body_id) => body_id.name(db),
        }
    }
    pub fn stable_location(&self, db: &'db dyn Database) -> StableLocation<'db> {
        match self {
            Self::Impl(body_id) => body_id.stable_location(db),
            Self::Trait(body_id) => body_id.stable_location(db),
        }
    }

    pub fn trait_function(&self, db: &'db dyn Database) -> Maybe<TraitFunctionId<'db>> {
        match self {
            Self::Impl(impl_function) => db.impl_function_trait_function(*impl_function),
            Self::Trait(trait_function) => Ok(*trait_function),
        }
    }
}

/// The ID of a generic function with body that can be concretized.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, SemanticObject)]
pub enum GenericFunctionWithBodyId<'db> {
    Free(FreeFunctionId<'db>),
    Impl(ImplGenericFunctionWithBodyId<'db>),
    Trait(ConcreteTraitGenericFunctionId<'db>),
}
impl<'db> GenericFunctionWithBodyId<'db> {
    pub fn from_generic(
        db: &'db dyn Database,
        other: GenericFunctionId<'db>,
    ) -> Maybe<Option<Self>> {
        Ok(Some(match other {
            GenericFunctionId::Free(id) => GenericFunctionWithBodyId::Free(id),
            GenericFunctionId::Impl(ImplGenericFunctionId { impl_id, function }) => {
                let ImplLongId::Concrete(concrete_impl_id) = impl_id.long(db) else {
                    return Ok(None);
                };
                GenericFunctionWithBodyId::Impl(ImplGenericFunctionWithBodyId {
                    concrete_impl_id: *concrete_impl_id,
                    function_body: if let Some(impl_function) =
                        concrete_impl_id.get_impl_function(db, function)?
                    {
                        ImplFunctionBodyId::Impl(impl_function)
                    } else {
                        ImplFunctionBodyId::Trait(function)
                    },
                })
            }
            _ => return Ok(None),
        }))
    }
    pub fn name(&self, db: &dyn Database) -> String {
        match self {
            GenericFunctionWithBodyId::Free(free) => free.name(db).to_string(db),
            GenericFunctionWithBodyId::Impl(imp) => {
                format!(
                    "{}::{}",
                    imp.concrete_impl_id.name(db).long(db),
                    imp.function_body.name(db).long(db)
                )
            }
            GenericFunctionWithBodyId::Trait(trt) => {
                format!(
                    "{}::{}",
                    trt.concrete_trait(db).name(db).long(db),
                    trt.trait_function(db).name(db).long(db)
                )
            }
        }
    }

    pub fn full_path(&self, db: &dyn Database) -> String {
        match self {
            GenericFunctionWithBodyId::Free(free) => free.full_path(db),
            GenericFunctionWithBodyId::Impl(imp) => {
                format!(
                    "{}::{}",
                    imp.concrete_impl_id.impl_def_id(db).full_path(db),
                    imp.function_body.name(db).long(db)
                )
            }
            GenericFunctionWithBodyId::Trait(trt) => format!(
                "{}::{}",
                trt.concrete_trait(db).full_path(db),
                trt.trait_function(db).name(db).long(db)
            ),
        }
    }
    pub fn stable_location(&self, db: &'db dyn Database) -> StableLocation<'db> {
        match self {
            GenericFunctionWithBodyId::Free(free_function) => free_function.stable_location(db),
            GenericFunctionWithBodyId::Impl(impl_function) => {
                impl_function.function_body.stable_location(db)
            }
            GenericFunctionWithBodyId::Trait(trait_function) => {
                trait_function.trait_function(db).stable_location(db)
            }
        }
    }
}

/// A long Id of a concrete function with body.
#[derive(Clone, Debug, Hash, PartialEq, Eq, SemanticObject)]
pub struct ConcreteFunctionWithBody<'db> {
    pub generic_function: GenericFunctionWithBodyId<'db>,
    pub generic_args: Vec<semantic::GenericArgumentId<'db>>,
}
impl<'db> ConcreteFunctionWithBody<'db> {
    pub fn function_with_body_id(&self, db: &'db dyn Database) -> FunctionWithBodyId<'db> {
        match self.generic_function {
            GenericFunctionWithBodyId::Free(id) => FunctionWithBodyId::Free(id),
            GenericFunctionWithBodyId::Impl(id) => match id.function_body {
                ImplFunctionBodyId::Impl(id) => FunctionWithBodyId::Impl(id),
                ImplFunctionBodyId::Trait(id) => FunctionWithBodyId::Trait(id),
            },
            GenericFunctionWithBodyId::Trait(id) => {
                FunctionWithBodyId::Trait(id.trait_function(db))
            }
        }
    }
    pub fn substitution(&self, db: &'db dyn Database) -> Maybe<GenericSubstitution<'db>> {
        Ok(match self.generic_function {
            GenericFunctionWithBodyId::Free(f) => {
                GenericSubstitution::new(db.free_function_generic_params(f)?, &self.generic_args)
            }
            GenericFunctionWithBodyId::Impl(f) => match f.function_body {
                ImplFunctionBodyId::Impl(body_id) => {
                    let concrete_impl = f.concrete_impl_id.long(db);
                    GenericSubstitution::from_impl(
                        ImplLongId::Concrete(f.concrete_impl_id).intern(db),
                    )
                    .concat(GenericSubstitution::new(
                        &chain!(
                            db.impl_function_generic_params(body_id)?,
                            db.impl_def_generic_params(concrete_impl.impl_def_id)?
                        )
                        .cloned()
                        .collect_vec(),
                        &chain!(&self.generic_args, &concrete_impl.generic_args)
                            .cloned()
                            .collect_vec(),
                    ))
                }
                ImplFunctionBodyId::Trait(body_id) => {
                    let concrete_impl_id = ImplLongId::Concrete(f.concrete_impl_id).intern(db);
                    let concrete_trait = concrete_impl_id.concrete_trait(db)?.long(db);
                    GenericSubstitution::from_impl(concrete_impl_id).concat(
                        GenericSubstitution::new(
                            &chain!(
                                db.trait_function_generic_params(body_id)?,
                                db.trait_generic_params(concrete_trait.trait_id)?
                            )
                            .cloned()
                            .collect_vec(),
                            &chain!(&self.generic_args, &concrete_trait.generic_args)
                                .cloned()
                                .collect_vec(),
                        ),
                    )
                }
            },
            GenericFunctionWithBodyId::Trait(f) => {
                let concrete_trait = f.concrete_trait(db).long(db);
                GenericSubstitution::new(
                    &chain!(
                        db.trait_function_generic_params(f.trait_function(db))?,
                        db.trait_generic_params(concrete_trait.trait_id)?
                    )
                    .cloned()
                    .collect_vec(),
                    &chain!(&self.generic_args, &concrete_trait.generic_args)
                        .cloned()
                        .collect_vec(),
                )
            }
        })
    }
    pub fn from_no_generics_free(
        db: &dyn Database,
        free_function_id: FreeFunctionId<'db>,
    ) -> Option<Self> {
        require(db.free_function_generic_params(free_function_id).ok()?.is_empty())?;
        Some(ConcreteFunctionWithBody {
            generic_function: GenericFunctionWithBodyId::Free(free_function_id),
            generic_args: vec![],
        })
    }
    pub fn from_generic(
        db: &'db dyn Database,
        function_id: FunctionWithBodyId<'db>,
    ) -> Maybe<Self> {
        Ok(match function_id {
            FunctionWithBodyId::Free(free) => {
                let params = db.free_function_generic_params(free)?;
                let generic_args = generic_params_to_args(params, db);
                ConcreteFunctionWithBody {
                    generic_function: GenericFunctionWithBodyId::Free(free),
                    generic_args,
                }
            }
            FunctionWithBodyId::Impl(impl_function_id) => {
                let params = db.impl_function_generic_params(impl_function_id)?;
                let generic_args = generic_params_to_args(params, db);
                let impl_def_id = impl_function_id.impl_def_id(db);
                let impl_def_params = db.impl_def_generic_params(impl_def_id)?;
                let impl_generic_args = generic_params_to_args(impl_def_params, db);
                let impl_generic_function = ImplGenericFunctionWithBodyId {
                    concrete_impl_id: ConcreteImplLongId {
                        impl_def_id,
                        generic_args: impl_generic_args,
                    }
                    .intern(db),
                    function_body: ImplFunctionBodyId::Impl(impl_function_id),
                };
                ConcreteFunctionWithBody {
                    generic_function: GenericFunctionWithBodyId::Impl(impl_generic_function),
                    generic_args,
                }
            }
            FunctionWithBodyId::Trait(trait_function_id) => {
                let params = db.trait_function_generic_params(trait_function_id)?;
                let generic_args = generic_params_to_args(params, db);
                let trait_id = trait_function_id.trait_id(db);
                let trait_generic_params = db.trait_generic_params(trait_id)?;
                let trait_generic_args = generic_params_to_args(trait_generic_params, db);
                let concrete_trait_id = ConcreteTraitLongId {
                    generic_args: trait_generic_args,
                    trait_id: trait_function_id.trait_id(db),
                }
                .intern(db);
                let trait_generic_function = ConcreteTraitGenericFunctionId::new_from_data(
                    db,
                    concrete_trait_id,
                    trait_function_id,
                );
                ConcreteFunctionWithBody {
                    generic_function: GenericFunctionWithBodyId::Trait(trait_generic_function),
                    generic_args,
                }
            }
        })
    }
    pub fn concrete(&self, db: &'db dyn Database) -> Maybe<ConcreteFunction<'db>> {
        Ok(ConcreteFunction {
            generic_function: GenericFunctionId::from_generic_with_body(db, self.generic_function)?,
            generic_args: self.generic_args.clone(),
        })
    }
    pub fn function_id(&self, db: &'db dyn Database) -> Maybe<FunctionId<'db>> {
        Ok(FunctionLongId { function: self.concrete(db)? }.intern(db))
    }
    pub fn name(&self, db: &'db dyn Database) -> SmolStrId<'db> {
        self.function_with_body_id(db).name(db)
    }
    pub fn full_path(&self, db: &dyn Database) -> String {
        format!("{:?}", self.debug(db))
    }
}

impl<'db> DebugWithDb<'db> for ConcreteFunctionWithBody<'db> {
    type Db = dyn Database;

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db dyn Database) -> std::fmt::Result {
        let f = &mut CountingWriter::new(f);
        write!(f, "{}", self.generic_function.full_path(db))?;
        fmt_generic_args(&self.generic_args, f, db)
    }
}

define_short_id!(ConcreteFunctionWithBodyId, ConcreteFunctionWithBody<'db>);
semantic_object_for_id!(ConcreteFunctionWithBodyId, ConcreteFunctionWithBody<'a>);
impl<'db> ConcreteFunctionWithBodyId<'db> {
    pub fn function_with_body_id(&self, db: &'db dyn Database) -> FunctionWithBodyId<'db> {
        self.long(db).function_with_body_id(db)
    }
    pub fn substitution(&self, db: &'db dyn Database) -> Maybe<GenericSubstitution<'db>> {
        self.long(db).substitution(db)
    }
    pub fn from_no_generics_free(
        db: &'db dyn Database,
        free_function_id: FreeFunctionId<'db>,
    ) -> Option<Self> {
        Some(ConcreteFunctionWithBody::from_no_generics_free(db, free_function_id)?.intern(db))
    }
    pub fn from_generic(
        db: &'db dyn Database,
        function_id: FunctionWithBodyId<'db>,
    ) -> Maybe<Self> {
        Ok(ConcreteFunctionWithBody::from_generic(db, function_id)?.intern(db))
    }
    pub fn concrete(&self, db: &'db dyn Database) -> Maybe<ConcreteFunction<'db>> {
        self.long(db).concrete(db)
    }
    pub fn function_id(&self, db: &'db dyn Database) -> Maybe<FunctionId<'db>> {
        self.long(db).function_id(db)
    }
    pub fn generic_function(&self, db: &'db dyn Database) -> GenericFunctionWithBodyId<'db> {
        self.long(db).generic_function
    }
    pub fn name(&self, db: &'db dyn Database) -> SmolStrId<'db> {
        self.long(db).name(db)
    }
    pub fn full_path(&self, db: &dyn Database) -> String {
        self.long(db).full_path(db)
    }

    pub fn stable_location(&self, db: &'db dyn Database) -> StableLocation<'db> {
        self.long(db).generic_function.stable_location(db)
    }

    pub fn is_panic_destruct_fn(&self, db: &dyn Database) -> Maybe<bool> {
        let trait_function = match self.generic_function(db) {
            GenericFunctionWithBodyId::Free(_) => return Ok(false),
            GenericFunctionWithBodyId::Impl(impl_func) => {
                impl_func.function_body.trait_function(db)?
            }
            GenericFunctionWithBodyId::Trait(trait_func) => trait_func.trait_function(db),
        };
        Ok(trait_function == db.core_info().panic_destruct_fn)
    }
}

impl<'db> UnstableSalsaId for ConcreteFunctionWithBodyId<'db> {
    fn get_internal_id(&self) -> salsa::Id {
        self.as_intern_id()
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, SemanticObject)]
pub struct ConcreteFunction<'db> {
    pub generic_function: GenericFunctionId<'db>,
    pub generic_args: Vec<semantic::GenericArgumentId<'db>>,
}
impl<'db> ConcreteFunction<'db> {
    pub fn body(&self, db: &'db dyn Database) -> Maybe<Option<ConcreteFunctionWithBodyId<'db>>> {
        let Some(generic_function) =
            GenericFunctionWithBodyId::from_generic(db, self.generic_function)?
        else {
            return Ok(None);
        };
        Ok(Some(
            ConcreteFunctionWithBody { generic_function, generic_args: self.generic_args.clone() }
                .intern(db),
        ))
    }
    pub fn full_path(&self, db: &dyn Database) -> String {
        format!("{:?}", self.debug(db))
    }
}
impl<'db> DebugWithDb<'db> for ConcreteFunction<'db> {
    type Db = dyn Database;

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db dyn Database) -> std::fmt::Result {
        let f = &mut CountingWriter::new(f);
        write!(f, "{}", self.generic_function.format(db))?;
        fmt_generic_args(&self.generic_args, f, db)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb, SemanticObject, salsa::Update)]
#[debug_db(dyn Database)]
pub struct Signature<'db> {
    pub params: Vec<semantic::Parameter<'db>>,
    pub return_type: semantic::TypeId<'db>,
    /// implicit parameters
    pub implicits: Vec<semantic::TypeId<'db>>,
    #[dont_rewrite]
    pub panicable: bool,
    #[dont_rewrite]
    pub is_const: bool,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::FunctionSignaturePtr<'db>,
}

impl<'db> Signature<'db> {
    pub fn from_ast(
        diagnostics: &mut SemanticDiagnostics<'db>,
        db: &'db dyn Database,
        resolver: &mut Resolver<'db>,
        declaration_syntax: &ast::FunctionDeclaration<'db>,
        function_title_id: FunctionTitleId<'db>,
        environment: &mut Environment<'db>,
    ) -> Self {
        let signature_syntax = declaration_syntax.signature(db);
        let params = function_signature_params(
            diagnostics,
            db,
            resolver,
            signature_syntax.parameters(db).elements(db),
            Some(function_title_id),
            environment,
        );
        let return_type =
            function_signature_return_type(diagnostics, db, resolver, &signature_syntax);
        let implicits =
            function_signature_implicit_parameters(diagnostics, db, resolver, &signature_syntax);
        let panicable = match signature_syntax.optional_no_panic(db) {
            ast::OptionTerminalNoPanic::Empty(_) => true,
            ast::OptionTerminalNoPanic::TerminalNoPanic(_) => false,
        };
        let stable_ptr = signature_syntax.stable_ptr(db);
        let is_const = matches!(
            declaration_syntax.optional_const(db),
            ast::OptionTerminalConst::TerminalConst(_)
        );
        semantic::Signature { params, return_type, implicits, panicable, stable_ptr, is_const }
    }
}

pub fn function_signature_return_type<'db>(
    diagnostics: &mut SemanticDiagnostics<'db>,
    db: &'db dyn Database,
    resolver: &mut Resolver<'db>,
    sig: &ast::FunctionSignature<'db>,
) -> semantic::TypeId<'db> {
    let ty_syntax = match sig.ret_ty(db) {
        ast::OptionReturnTypeClause::Empty(_) => {
            return unit_ty(db);
        }
        ast::OptionReturnTypeClause::ReturnTypeClause(ret_type_clause) => ret_type_clause.ty(db),
    };
    resolve_type(db, diagnostics, resolver, &ty_syntax)
}

/// Returns the implicit parameters of the given function signature's AST.
pub fn function_signature_implicit_parameters<'db>(
    diagnostics: &mut SemanticDiagnostics<'db>,
    db: &'db dyn Database,
    resolver: &mut Resolver<'db>,
    sig: &ast::FunctionSignature<'db>,
) -> Vec<semantic::TypeId<'db>> {
    let implicits = match sig.implicits_clause(db) {
        ast::OptionImplicitsClause::Empty(_) => return vec![],
        ast::OptionImplicitsClause::ImplicitsClause(implicits_clause) => {
            implicits_clause.implicits(db)
        }
    };
    let ast_implicits = implicits.elements(db);
    ast_implicits
        .map(|implicit| resolve_type(db, diagnostics, resolver, &ast::Expr::Path(implicit)))
        .collect()
}

/// Returns the parameters of the given function signature's AST.
pub fn function_signature_params<'db>(
    diagnostics: &mut SemanticDiagnostics<'db>,
    db: &'db dyn Database,
    resolver: &mut Resolver<'db>,
    params: impl Iterator<Item = ast::Param<'db>>,
    function_title_id: Option<FunctionTitleId<'db>>,
    env: &mut Environment<'db>,
) -> Vec<semantic::Parameter<'db>> {
    update_env_with_ast_params(diagnostics, db, resolver, params, function_title_id, env)
}

/// Query implementation of [FunctionsSemantic::concrete_function_signature].
#[salsa::tracked(returns(ref))]
fn concrete_function_signature<'db>(
    db: &'db dyn Database,
    function_id: FunctionId<'db>,
) -> Maybe<Signature<'db>> {
    let ConcreteFunction { generic_function, generic_args, .. } = &function_id.long(db).function;
    let generic_params = generic_function.generic_params(db)?;
    let generic_signature = generic_function.generic_signature(db)?;
    // TODO(spapini): When trait generics are supported, they need to be substituted
    //   one by one, not together.
    // Panic shouldn't occur since ConcreteFunction is assumed to be constructed correctly.
    GenericSubstitution::new(generic_params, generic_args).substitute(db, generic_signature.clone())
}

/// Implementation of [FunctionsSemantic::concrete_function_closure_params].
fn concrete_function_closure_params<'db>(
    db: &'db dyn Database,
    function_id: FunctionId<'db>,
) -> Maybe<OrderedHashMap<semantic::TypeId<'db>, semantic::TypeId<'db>>> {
    let ConcreteFunction { generic_function, generic_args, .. } =
        function_id.long(db).function.clone();
    let generic_params = generic_function.generic_params(db)?;
    let mut generic_closure_params = db.get_closure_params(generic_function)?;
    let substitution = GenericSubstitution::new(generic_params, &generic_args);
    let mut changed_keys = vec![];
    for (key, value) in generic_closure_params.iter_mut() {
        *value = substitution.substitute(db, *value)?;
        let updated_key = substitution.substitute(db, *key)?;
        if updated_key != *key {
            changed_keys.push((*key, updated_key));
        }
    }
    for (old_key, new_key) in changed_keys {
        let v = generic_closure_params.swap_remove(&old_key).unwrap();
        generic_closure_params.insert(new_key, v);
    }
    Ok(generic_closure_params)
}

/// Query implementation of [FunctionsSemantic::concrete_function_closure_params].
#[salsa::tracked]
fn concrete_function_closure_params_tracked<'db>(
    db: &'db dyn Database,
    function_id: FunctionId<'db>,
) -> Maybe<OrderedHashMap<semantic::TypeId<'db>, semantic::TypeId<'db>>> {
    concrete_function_closure_params(db, function_id)
}

/// For a given list of AST parameters, returns the list of semantic parameters along with the
/// corresponding environment.
fn update_env_with_ast_params<'db>(
    diagnostics: &mut SemanticDiagnostics<'db>,
    db: &'db dyn Database,
    resolver: &mut Resolver<'db>,
    ast_params: impl Iterator<Item = ast::Param<'db>>,
    function_title_id: Option<FunctionTitleId<'db>>,
    env: &mut Environment<'db>,
) -> Vec<semantic::Parameter<'db>> {
    let mut semantic_params = Vec::new();
    for ast_param in ast_params {
        let semantic_param = ast_param_to_semantic(diagnostics, db, resolver, &ast_param);

        if env
            .add_param(db, diagnostics, semantic_param.clone(), &ast_param, function_title_id)
            .is_ok()
        {
            semantic_params.push(semantic_param);
        }
    }
    semantic_params
}

/// Returns a semantic parameter (and its name) for the given AST parameter.
fn ast_param_to_semantic<'db>(
    diagnostics: &mut SemanticDiagnostics<'db>,
    db: &'db dyn Database,
    resolver: &mut Resolver<'db>,
    ast_param: &ast::Param<'db>,
) -> semantic::Parameter<'db> {
    let name = ast_param.name(db);

    let id = ParamLongId(resolver.module_id, ast_param.stable_ptr(db)).intern(db);

    let ty = match ast_param.type_clause(db) {
        ast::OptionTypeClause::Empty(missing) => {
            resolver.inference().new_type_var(Some(missing.stable_ptr(db).untyped()))
        }
        ast::OptionTypeClause::TypeClause(ty_syntax) => {
            resolve_type(db, diagnostics, resolver, &ty_syntax.ty(db))
        }
    };

    let mutability =
        modifiers::compute_mutability(diagnostics, db, &ast_param.modifiers(db).elements_vec(db));

    semantic::Parameter { id, name: name.text(db), ty, mutability, stable_ptr: name.stable_ptr(db) }
}

// === Function Declaration ===

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb, salsa::Update)]
#[debug_db(dyn Database)]
pub struct FunctionDeclarationData<'db> {
    pub diagnostics: Diagnostics<'db, SemanticDiagnostic<'db>>,
    pub signature: semantic::Signature<'db>,
    /// The environment induced by the function's signature.
    pub environment: Environment<'db>,
    pub attributes: Vec<Attribute<'db>>,
    pub resolver_data: Arc<ResolverData<'db>>,
    pub inline_config: InlineConfiguration<'db>,
    /// Order of implicits to follow by this function.
    ///
    /// For example, this can be used to enforce ABI compatibility with Starknet OS.
    pub implicit_precedence: ImplicitPrecedence<'db>,
}

#[derive(Debug, PartialEq, Eq, Clone, salsa::Update)]
pub enum InlineConfiguration<'db> {
    /// The user did not specify any inlining preferences.
    None,
    Always(ast::AttributePtr<'db>),
    Should(ast::AttributePtr<'db>),
    Never(ast::AttributePtr<'db>),
}

/// If a function with impl generic parameters is marked as '#[inline(always)]', raise a diagnostic.
pub fn forbid_inline_always_with_impl_generic_param<'db>(
    diagnostics: &mut SemanticDiagnostics<'db>,
    generic_params: &[GenericParam<'db>],
    inline_config: &InlineConfiguration<'db>,
) {
    let has_impl_generic_param = generic_params.iter().any(|p| matches!(p, GenericParam::Impl(_)));
    match &inline_config {
        InlineConfiguration::Always(stable_ptr) if has_impl_generic_param => {
            diagnostics.report(
                stable_ptr.untyped(),
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
#[derive(Clone, Debug, Default, Eq, PartialEq, salsa::Update)]
pub struct ImplicitPrecedence<'db>(Vec<TypeId<'db>>);

impl<'db> ImplicitPrecedence<'db> {
    /// A precedence that does not actually prefer any implicit.
    ///
    /// When applied to a sequence of implicits, they will just be reordered alphabetically.
    pub const UNSPECIFIED: Self = Self(Vec::new());

    /// Sort implicits according to this precedence: first the ones with precedence
    /// (according to it), then the others by their name.
    pub fn apply(&self, implicits: &mut [TypeId<'db>], db: &dyn Database) {
        implicits.sort_by_cached_key(|implicit| {
            if let Some(idx) = self.0.iter().position(|item| item == implicit) {
                return (idx, "".to_string());
            }

            (self.0.len(), implicit.format(db))
        });
    }
}

impl<'db> FromIterator<TypeId<'db>> for ImplicitPrecedence<'db> {
    fn from_iter<T: IntoIterator<Item = TypeId<'db>>>(iter: T) -> Self {
        Self(Vec::from_iter(iter))
    }
}

/// Implementation of [FunctionsSemantic::get_closure_params].
fn get_closure_params<'db>(
    db: &'db dyn Database,
    generic_function_id: GenericFunctionId<'db>,
) -> Maybe<OrderedHashMap<TypeId<'db>, TypeId<'db>>> {
    let mut closure_params_map = OrderedHashMap::default();
    let generic_params = generic_function_id.generic_params(db)?;

    for param in generic_params {
        if let GenericParam::Impl(generic_param_impl) = param {
            let concrete_trait = generic_param_impl.concrete_trait?;
            if fn_traits(db).contains(&concrete_trait.trait_id(db)) {
                let [GenericArgumentId::Type(closure_type), GenericArgumentId::Type(params_type)] =
                    *concrete_trait.generic_args(db)
                else {
                    unreachable!(
                        "Fn trait must have exactly two generic arguments: closure type and \
                         parameter type."
                    )
                };

                closure_params_map.insert(closure_type, params_type);
            }
        }
    }
    Ok(closure_params_map)
}

/// Query implementation of [FunctionsSemantic::get_closure_params].
fn get_closure_params_tracked<'db>(
    db: &'db dyn Database,
    generic_function_id: GenericFunctionId<'db>,
) -> Maybe<OrderedHashMap<TypeId<'db>, TypeId<'db>>> {
    get_closure_params_helper(db, (), generic_function_id)
}

#[salsa::tracked]
fn get_closure_params_helper<'db>(
    db: &'db dyn Database,
    _tracked: Tracked,
    generic_function_id: GenericFunctionId<'db>,
) -> Maybe<OrderedHashMap<TypeId<'db>, TypeId<'db>>> {
    get_closure_params(db, generic_function_id)
}

/// Trait for functions-related semantic queries.
pub trait FunctionsSemantic<'db>: Database {
    /// Returns the signature of the given FunctionTitleId. This include free functions, extern
    /// functions, etc...
    fn function_title_signature(
        &'db self,
        function_title_id: FunctionTitleId<'db>,
    ) -> Maybe<&'db semantic::Signature<'db>> {
        match function_title_id {
            FunctionTitleId::Free(id) => self.free_function_signature(id),
            FunctionTitleId::Extern(id) => self.extern_function_signature(id),
            FunctionTitleId::Trait(id) => self.trait_function_signature(id),
            FunctionTitleId::Impl(id) => self.impl_function_signature(id),
        }
    }
    /// Returns the generic parameters of the given FunctionTitleId. This include free
    /// functions, extern functions, etc...
    fn function_title_generic_params(
        &'db self,
        function_title_id: FunctionTitleId<'db>,
    ) -> Maybe<&'db [GenericParam<'db>]> {
        match function_title_id {
            FunctionTitleId::Free(free_function) => {
                self.free_function_generic_params(free_function)
            }
            FunctionTitleId::Extern(extern_function) => {
                self.extern_function_declaration_generic_params(extern_function)
            }
            FunctionTitleId::Trait(trait_function) => {
                self.trait_function_generic_params(trait_function)
            }
            FunctionTitleId::Impl(impl_function) => {
                self.impl_function_generic_params(impl_function)
            }
        }
    }
    /// Returns the signature of a concrete function. This include free functions, extern functions,
    /// etc...
    fn concrete_function_signature(
        &'db self,
        function_id: FunctionId<'db>,
    ) -> Maybe<&'db semantic::Signature<'db>> {
        concrete_function_signature(self.as_dyn_database(), function_id).maybe_as_ref()
    }
    /// Returns a mapping of closure types to their associated parameter types for a concrete
    /// function.
    fn concrete_function_closure_params(
        &'db self,
        function_id: FunctionId<'db>,
    ) -> Maybe<OrderedHashMap<semantic::TypeId<'db>, semantic::TypeId<'db>>> {
        concrete_function_closure_params_tracked(self.as_dyn_database(), function_id)
    }
    /// Returns a mapping of closure types to their associated parameter types for a generic
    /// function.
    fn get_closure_params(
        &'db self,
        generic_function_id: GenericFunctionId<'db>,
    ) -> Maybe<OrderedHashMap<TypeId<'db>, TypeId<'db>>> {
        get_closure_params_tracked(self.as_dyn_database(), generic_function_id)
    }
}
impl<'db, T: Database + ?Sized> FunctionsSemantic<'db> for T {}
