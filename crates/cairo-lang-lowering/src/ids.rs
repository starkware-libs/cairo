use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::{
    NamedLanguageElementId, TopLevelLanguageElementId, TraitFunctionId, UnstableSalsaId,
};
use cairo_lang_diagnostics::{DiagnosticAdded, DiagnosticNote, Maybe};
use cairo_lang_proc_macros::{DebugWithDb, SemanticObject};
use cairo_lang_semantic::items::functions::ImplGenericFunctionId;
use cairo_lang_semantic::items::imp::ImplLongId;
use cairo_lang_semantic::{GenericArgumentId, TypeLongId};
use cairo_lang_syntax::node::ast::ExprPtr;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{TypedStablePtr, ast};
use cairo_lang_utils::{Intern, LookupIntern, define_short_id, try_extract_matches};
use defs::diagnostic_utils::StableLocation;
use defs::ids::{ExternFunctionId, FreeFunctionId};
use semantic::items::functions::GenericFunctionId;
use semantic::substitution::{GenericSubstitution, SubstitutionRewriter};
use semantic::{ExprVar, Mutability};
use {cairo_lang_defs as defs, cairo_lang_semantic as semantic};

use crate::Location;
use crate::db::LoweringGroup;
use crate::ids::semantic::substitution::SemanticRewriter;

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum FunctionWithBodyLongId {
    Semantic(defs::ids::FunctionWithBodyId),
    Generated { parent: defs::ids::FunctionWithBodyId, key: GeneratedFunctionKey },
}
define_short_id!(
    FunctionWithBodyId,
    FunctionWithBodyLongId,
    LoweringGroup,
    lookup_intern_lowering_function_with_body,
    intern_lowering_function_with_body
);
impl FunctionWithBodyLongId {
    pub fn base_semantic_function(
        &self,
        _db: &dyn LoweringGroup,
    ) -> cairo_lang_defs::ids::FunctionWithBodyId {
        match *self {
            FunctionWithBodyLongId::Semantic(id) => id,
            FunctionWithBodyLongId::Generated { parent, .. } => parent,
        }
    }
    pub fn to_concrete(&self, db: &dyn LoweringGroup) -> Maybe<ConcreteFunctionWithBodyLongId> {
        Ok(match *self {
            FunctionWithBodyLongId::Semantic(semantic) => ConcreteFunctionWithBodyLongId::Semantic(
                semantic::ConcreteFunctionWithBodyId::from_generic(db.upcast(), semantic)?,
            ),
            FunctionWithBodyLongId::Generated { parent, key } => {
                ConcreteFunctionWithBodyLongId::Generated(GeneratedFunction {
                    parent: semantic::ConcreteFunctionWithBodyId::from_generic(
                        db.upcast(),
                        parent,
                    )?,
                    key,
                })
            }
        })
    }
}
impl FunctionWithBodyId {
    pub fn base_semantic_function(
        &self,
        db: &dyn LoweringGroup,
    ) -> cairo_lang_defs::ids::FunctionWithBodyId {
        self.lookup_intern(db).base_semantic_function(db)
    }
    pub fn signature(&self, db: &dyn LoweringGroup) -> Maybe<Signature> {
        Ok(db.priv_function_with_body_lowering(*self)?.signature.clone())
    }
    pub fn to_concrete(&self, db: &dyn LoweringGroup) -> Maybe<ConcreteFunctionWithBodyId> {
        Ok(self.lookup_intern(db).to_concrete(db)?.intern(db))
    }
}
pub trait SemanticFunctionWithBodyIdEx {
    fn lowered(&self, db: &dyn LoweringGroup) -> FunctionWithBodyId;
}
impl SemanticFunctionWithBodyIdEx for cairo_lang_defs::ids::FunctionWithBodyId {
    fn lowered(&self, db: &dyn LoweringGroup) -> FunctionWithBodyId {
        FunctionWithBodyLongId::Semantic(*self).intern(db)
    }
}

/// Concrete function with body.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum ConcreteFunctionWithBodyLongId {
    Semantic(semantic::ConcreteFunctionWithBodyId),
    Generated(GeneratedFunction),
}
define_short_id!(
    ConcreteFunctionWithBodyId,
    ConcreteFunctionWithBodyLongId,
    LoweringGroup,
    lookup_intern_lowering_concrete_function_with_body,
    intern_lowering_concrete_function_with_body
);

impl ConcreteFunctionWithBodyId {
    pub fn is_panic_destruct_fn(&self, db: &dyn LoweringGroup) -> Maybe<bool> {
        match db.lookup_intern_lowering_concrete_function_with_body(*self) {
            ConcreteFunctionWithBodyLongId::Semantic(semantic_func) => {
                semantic_func.is_panic_destruct_fn(db.upcast())
            }
            ConcreteFunctionWithBodyLongId::Generated(GeneratedFunction {
                parent: _,
                key: GeneratedFunctionKey::TraitFunc(function, _),
            }) => Ok(function == db.core_info().panic_destruct_fn),
            _ => Ok(false),
        }
    }
}

impl UnstableSalsaId for ConcreteFunctionWithBodyId {
    fn get_internal_id(&self) -> &salsa::InternId {
        &self.0
    }
}
impl ConcreteFunctionWithBodyLongId {
    pub fn function_with_body_id(&self, db: &dyn LoweringGroup) -> FunctionWithBodyId {
        let semantic_db = db.upcast();
        let long_id = match *self {
            ConcreteFunctionWithBodyLongId::Semantic(id) => {
                FunctionWithBodyLongId::Semantic(id.function_with_body_id(semantic_db))
            }
            ConcreteFunctionWithBodyLongId::Generated(GeneratedFunction { parent, key }) => {
                FunctionWithBodyLongId::Generated {
                    parent: parent.function_with_body_id(semantic_db),
                    key,
                }
            }
        };
        long_id.intern(db)
    }
    pub fn substitution(&self, db: &dyn LoweringGroup) -> Maybe<GenericSubstitution> {
        let semantic_db = db.upcast();
        match self {
            ConcreteFunctionWithBodyLongId::Semantic(id) => id.substitution(semantic_db),
            ConcreteFunctionWithBodyLongId::Generated(GeneratedFunction { parent, .. }) => {
                parent.substitution(semantic_db)
            }
        }
    }
    pub fn function_id(&self, db: &dyn LoweringGroup) -> Maybe<FunctionId> {
        let semantic_db = db.upcast();
        let long_id = match self {
            ConcreteFunctionWithBodyLongId::Semantic(id) => {
                FunctionLongId::Semantic(id.function_id(semantic_db)?)
            }
            ConcreteFunctionWithBodyLongId::Generated(generated) => {
                FunctionLongId::Generated(*generated)
            }
        };
        Ok(long_id.intern(db))
    }
    pub fn base_semantic_function(
        &self,
        _db: &dyn LoweringGroup,
    ) -> semantic::ConcreteFunctionWithBodyId {
        match *self {
            ConcreteFunctionWithBodyLongId::Semantic(id) => id,
            ConcreteFunctionWithBodyLongId::Generated(generated) => generated.parent,
        }
    }
    pub fn full_path(&self, db: &dyn LoweringGroup) -> String {
        match self {
            ConcreteFunctionWithBodyLongId::Semantic(semantic) => semantic.full_path(db.upcast()),
            ConcreteFunctionWithBodyLongId::Generated(generated) => generated.full_path(db),
        }
    }
}
impl ConcreteFunctionWithBodyId {
    pub fn from_semantic(
        db: &dyn LoweringGroup,
        semantic: semantic::ConcreteFunctionWithBodyId,
    ) -> Self {
        ConcreteFunctionWithBodyLongId::Semantic(semantic).intern(db)
    }
    pub fn function_with_body_id(&self, db: &dyn LoweringGroup) -> FunctionWithBodyId {
        self.lookup_intern(db).function_with_body_id(db)
    }
    pub fn substitution(&self, db: &dyn LoweringGroup) -> Maybe<GenericSubstitution> {
        self.lookup_intern(db).substitution(db)
    }
    pub fn function_id(&self, db: &dyn LoweringGroup) -> Maybe<FunctionId> {
        self.lookup_intern(db).function_id(db)
    }
    pub fn full_path(&self, db: &dyn LoweringGroup) -> String {
        self.lookup_intern(db).full_path(db)
    }
    pub fn signature(&self, db: &dyn LoweringGroup) -> Maybe<Signature> {
        let generic_signature = self.function_with_body_id(db).signature(db)?;
        self.substitution(db)?.substitute(db.upcast(), generic_signature)
    }
    pub fn from_no_generics_free(
        db: &dyn LoweringGroup,
        free_function_id: FreeFunctionId,
    ) -> Option<Self> {
        let semantic = semantic::ConcreteFunctionWithBodyId::from_no_generics_free(
            db.upcast(),
            free_function_id,
        )?;
        Some(ConcreteFunctionWithBodyLongId::Semantic(semantic).intern(db))
    }
    pub fn base_semantic_function(
        &self,
        db: &dyn LoweringGroup,
    ) -> semantic::ConcreteFunctionWithBodyId {
        self.lookup_intern(db).base_semantic_function(db)
    }
    pub fn stable_location(&self, db: &dyn LoweringGroup) -> Maybe<StableLocation> {
        let semantic_db = db.upcast();
        Ok(match self.lookup_intern(db) {
            ConcreteFunctionWithBodyLongId::Semantic(id) => id.stable_location(semantic_db),
            ConcreteFunctionWithBodyLongId::Generated(generated) => match generated.key {
                GeneratedFunctionKey::Loop(stable_ptr) => StableLocation::new(stable_ptr.untyped()),
                GeneratedFunctionKey::TraitFunc(_, stable_location) => stable_location,
            },
        })
    }
}

/// Function.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum FunctionLongId {
    /// An original function from the user code.
    Semantic(semantic::FunctionId),
    /// A function generated by the compiler.
    Generated(GeneratedFunction),
}
define_short_id!(
    FunctionId,
    FunctionLongId,
    LoweringGroup,
    lookup_intern_lowering_function,
    intern_lowering_function
);
impl FunctionLongId {
    pub fn body(&self, db: &dyn LoweringGroup) -> Maybe<Option<ConcreteFunctionWithBodyId>> {
        let semantic_db = db.upcast();
        Ok(Some(match *self {
            FunctionLongId::Semantic(id) => {
                let concrete_function = id.get_concrete(semantic_db);
                if let GenericFunctionId::Impl(ImplGenericFunctionId { impl_id, function }) =
                    concrete_function.generic_function
                {
                    if let ImplLongId::GeneratedImpl(imp) = db.lookup_intern_impl(impl_id) {
                        let semantic_db = db.upcast();
                        let concrete_trait = imp.concrete_trait(semantic_db);
                        let info = db.core_info();
                        assert!(
                            [
                                info.destruct_fn,
                                info.panic_destruct_fn,
                                info.call_fn,
                                info.call_once_fn
                            ]
                            .contains(&function)
                        );

                        let generic_args = concrete_trait.generic_args(semantic_db);
                        let Some(GenericArgumentId::Type(ty)) = generic_args.first() else {
                            unreachable!("Expected Generated Impl to have a type argument");
                        };
                        let TypeLongId::Closure(ty) = ty.lookup_intern(semantic_db) else {
                            unreachable!("Expected Generated Impl to have a closure type argument");
                        };

                        let Some(parent) =
                            ty.parent_function?.get_concrete(semantic_db).body(semantic_db)?
                        else {
                            return Ok(None);
                        };
                        return Ok(Some(
                            GeneratedFunction {
                                parent,
                                key: GeneratedFunctionKey::TraitFunc(function, ty.wrapper_location),
                            }
                            .body(db),
                        ));
                    }
                }

                let Some(body) = concrete_function.body(semantic_db)? else {
                    return Ok(None);
                };
                ConcreteFunctionWithBodyLongId::Semantic(body).intern(db)
            }
            FunctionLongId::Generated(generated) => generated.body(db),
        }))
    }
    pub fn signature(&self, db: &dyn LoweringGroup) -> Maybe<Signature> {
        match self {
            FunctionLongId::Semantic(semantic) => {
                Ok(Signature::from_semantic(db, db.concrete_function_signature(*semantic)?))
            }
            FunctionLongId::Generated(generated) => generated.body(db).signature(db),
        }
    }
    pub fn full_path(&self, db: &dyn LoweringGroup) -> String {
        format!("{:?}", self.debug(db))
    }
    /// Returns the full path of the relevant semantic function:
    /// - If the function itself is semantic (non generated), its own full path.
    /// - If the function is generated, then its (semantic) parent's full path.
    pub fn semantic_full_path(&self, db: &dyn LoweringGroup) -> String {
        match self {
            FunctionLongId::Semantic(id) => id.full_path(db.upcast()),
            FunctionLongId::Generated(generated) => generated.parent.full_path(db.upcast()),
        }
    }
}
impl FunctionId {
    pub fn body(&self, db: &dyn LoweringGroup) -> Maybe<Option<ConcreteFunctionWithBodyId>> {
        self.lookup_intern(db).body(db)
    }
    pub fn signature(&self, db: &dyn LoweringGroup) -> Maybe<Signature> {
        self.lookup_intern(db).signature(db)
    }
    pub fn full_path(&self, db: &dyn LoweringGroup) -> String {
        self.lookup_intern(db).full_path(db)
    }
    pub fn semantic_full_path(&self, db: &dyn LoweringGroup) -> String {
        self.lookup_intern(db).semantic_full_path(db)
    }
    /// Returns the function as an `ExternFunctionId` and its generic arguments, if it is an
    /// `extern` functions.
    pub fn get_extern(
        &self,
        db: &dyn LoweringGroup,
    ) -> Option<(ExternFunctionId, Vec<GenericArgumentId>)> {
        let semantic = try_extract_matches!(self.lookup_intern(db), FunctionLongId::Semantic)?;
        let concrete = semantic.get_concrete(db.upcast());
        Some((
            try_extract_matches!(concrete.generic_function, GenericFunctionId::Extern)?,
            concrete.generic_args,
        ))
    }
}
pub trait SemanticFunctionIdEx {
    fn lowered(&self, db: &dyn LoweringGroup) -> FunctionId;
}
impl SemanticFunctionIdEx for semantic::FunctionId {
    fn lowered(&self, db: &dyn LoweringGroup) -> FunctionId {
        let ret = FunctionLongId::Semantic(*self).intern(db);
        // If the function is generated, we need to check if it has a body, so we can return its
        // generated function id.
        // TODO(orizi): This is a hack, we should have a better way to do this.
        if let Ok(Some(body)) = ret.body(db) {
            if let Ok(id) = body.function_id(db) {
                return id;
            }
        }
        ret
    }
}
impl<'a> DebugWithDb<dyn LoweringGroup + 'a> for FunctionLongId {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        db: &(dyn LoweringGroup + 'a),
    ) -> std::fmt::Result {
        match self {
            FunctionLongId::Semantic(semantic) => write!(f, "{:?}", semantic.debug(db)),
            FunctionLongId::Generated(generated) => write!(f, "{:?}", generated.debug(db)),
        }
    }
}

/// A key for a generated functions.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum GeneratedFunctionKey {
    /// Generated loop functions are identified by the loop expr_id.
    Loop(ExprPtr),
    TraitFunc(TraitFunctionId, StableLocation),
}

/// Generated function.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct GeneratedFunction {
    pub parent: semantic::ConcreteFunctionWithBodyId,
    pub key: GeneratedFunctionKey,
}
impl GeneratedFunction {
    pub fn body(&self, db: &dyn LoweringGroup) -> ConcreteFunctionWithBodyId {
        let long_id = ConcreteFunctionWithBodyLongId::Generated(*self);
        long_id.intern(db)
    }
    pub fn full_path(&self, db: &dyn LoweringGroup) -> String {
        format!("{:?}", self.debug(db))
    }
}
impl<'a> DebugWithDb<dyn LoweringGroup + 'a> for GeneratedFunction {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        db: &(dyn LoweringGroup + 'a),
    ) -> std::fmt::Result {
        match self.key {
            GeneratedFunctionKey::Loop(expr_ptr) => {
                let mut func_ptr = expr_ptr.untyped();
                while !matches!(
                    func_ptr.kind(db.upcast()),
                    SyntaxKind::FunctionWithBody | SyntaxKind::TraitItemFunction
                ) {
                    func_ptr = func_ptr.parent(db.upcast())
                }

                let span = expr_ptr.0.lookup(db.upcast()).span(db.upcast());
                let function_start = func_ptr.lookup(db.upcast()).span(db.upcast()).start.as_u32();
                write!(
                    f,
                    "{:?}[{}-{}]",
                    self.parent.debug(db),
                    span.start.as_u32() - function_start,
                    span.end.as_u32() - function_start
                )
            }
            GeneratedFunctionKey::TraitFunc(trait_func, loc) => {
                let trait_id = trait_func.trait_id(db.upcast());
                write!(
                    f,
                    "Generated `{}::{}` for {{closure@{:?}}}",
                    trait_id.full_path(db.upcast()),
                    trait_func.name(db.upcast()),
                    loc.debug(db.upcast()),
                )
            }
        }
    }
}

/// Lowered signature of a function.
#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb, SemanticObject, Hash)]
#[debug_db(dyn LoweringGroup + 'a)]
pub struct Signature {
    /// Input params.
    pub params: Vec<semantic::ExprVarMemberPath>,
    /// Extra returns - e.g. ref params
    pub extra_rets: Vec<semantic::ExprVarMemberPath>,
    /// Return type.
    pub return_type: semantic::TypeId,
    /// Explicit implicit requirements.
    pub implicits: Vec<semantic::TypeId>,
    /// Panicable.
    #[dont_rewrite]
    pub panicable: bool,
    /// Location.
    #[dont_rewrite]
    #[hide_field_debug_with_db]
    pub location: LocationId,
}
impl Signature {
    pub fn from_semantic(db: &dyn LoweringGroup, value: semantic::Signature) -> Self {
        let semantic::Signature {
            params,
            return_type,
            implicits,
            panicable,
            stable_ptr,
            is_const: _,
        } = value;
        let ref_params = params
            .iter()
            .filter(|param| param.mutability == Mutability::Reference)
            .map(|param| parameter_as_member_path(param.clone()))
            .collect();
        let params: Vec<semantic::ExprVarMemberPath> =
            params.into_iter().map(parameter_as_member_path).collect();
        Self {
            params,
            extra_rets: ref_params,
            return_type,
            implicits,
            panicable,
            location: LocationId::from_stable_location(
                db,
                StableLocation::new(stable_ptr.untyped()),
            ),
        }
    }
    pub fn is_fully_concrete(&self, db: &dyn LoweringGroup) -> bool {
        let semantic_db = db.upcast();
        self.params.iter().all(|param| param.ty().is_fully_concrete(semantic_db))
            && self.extra_rets.iter().all(|param| param.ty().is_fully_concrete(semantic_db))
            && self.return_type.is_fully_concrete(semantic_db)
            && self.implicits.iter().all(|ty| ty.is_fully_concrete(semantic_db))
    }
}
semantic::add_rewrite!(<'a>, SubstitutionRewriter<'a>, DiagnosticAdded, Signature);

/// Converts a [semantic::Parameter] to a [semantic::ExprVarMemberPath].
pub(crate) fn parameter_as_member_path(param: semantic::Parameter) -> semantic::ExprVarMemberPath {
    let semantic::Parameter { id, ty, stable_ptr, .. } = param;
    semantic::ExprVarMemberPath::Var(ExprVar {
        var: semantic::VarId::Param(id),
        ty,
        stable_ptr: ast::ExprPtr(stable_ptr.0),
    })
}

define_short_id!(LocationId, Location, LoweringGroup, lookup_intern_location, intern_location);
impl LocationId {
    pub fn from_stable_location(
        db: &dyn LoweringGroup,
        stable_location: StableLocation,
    ) -> LocationId {
        Location::new(stable_location).intern(db)
    }

    /// Adds a note to the location.
    pub fn with_note(&self, db: &dyn LoweringGroup, note: DiagnosticNote) -> LocationId {
        self.lookup_intern(db).with_note(note).intern(db)
    }

    /// Adds a note that this location was generated while compiling an auto-generated function.
    pub fn with_auto_generation_note(
        &self,
        db: &dyn LoweringGroup,
        logic_name: &str,
    ) -> LocationId {
        self.with_note(
            db,
            DiagnosticNote::text_only(format!(
                "this error originates in auto-generated {logic_name} logic."
            )),
        )
    }
}
