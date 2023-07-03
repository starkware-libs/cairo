use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::UnstableSalsaId;
use cairo_lang_diagnostics::{DiagnosticAdded, Maybe};
use cairo_lang_proc_macros::{DebugWithDb, SemanticObject};
use cairo_lang_syntax::node::ast;
use cairo_lang_utils::define_short_id;
use defs::diagnostic_utils::StableLocation;
use defs::ids::{FreeFunctionId, LanguageElementId};
use semantic::substitution::{GenericSubstitution, SubstitutionRewriter};
use semantic::{ExprVar, Mutability};
use {cairo_lang_defs as defs, cairo_lang_semantic as semantic};

use crate::db::LoweringGroup;
use crate::ids::semantic::substitution::SemanticRewriter;
use crate::Location;

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum FunctionWithBodyLongId {
    Semantic(defs::ids::FunctionWithBodyId),
    Generated { parent: defs::ids::FunctionWithBodyId, element: semantic::ExprId },
}
define_short_id!(
    FunctionWithBodyId,
    FunctionWithBodyLongId,
    LoweringGroup,
    lookup_intern_lowering_function_with_body
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
            FunctionWithBodyLongId::Generated { parent, element } => {
                ConcreteFunctionWithBodyLongId::Generated(GeneratedFunction {
                    parent: semantic::ConcreteFunctionWithBodyId::from_generic(
                        db.upcast(),
                        parent,
                    )?,
                    element,
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
        db.lookup_intern_lowering_function_with_body(*self).base_semantic_function(db)
    }
    pub fn signature(&self, db: &dyn LoweringGroup) -> Maybe<Signature> {
        Ok(db.priv_function_with_body_lowering(*self)?.signature.clone())
    }
    pub fn to_concrete(&self, db: &dyn LoweringGroup) -> Maybe<ConcreteFunctionWithBodyId> {
        Ok(db.intern_lowering_concrete_function_with_body(
            db.lookup_intern_lowering_function_with_body(*self).to_concrete(db)?,
        ))
    }
}
pub trait SemanticFunctionWithBodyIdEx {
    fn lowered(&self, db: &dyn LoweringGroup) -> FunctionWithBodyId;
}
impl SemanticFunctionWithBodyIdEx for cairo_lang_defs::ids::FunctionWithBodyId {
    fn lowered(&self, db: &dyn LoweringGroup) -> FunctionWithBodyId {
        db.intern_lowering_function_with_body(FunctionWithBodyLongId::Semantic(*self))
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
    lookup_intern_lowering_concrete_function_with_body
);
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
            ConcreteFunctionWithBodyLongId::Generated(GeneratedFunction { parent, element }) => {
                FunctionWithBodyLongId::Generated {
                    parent: parent.function_with_body_id(semantic_db),
                    element,
                }
            }
        };
        db.intern_lowering_function_with_body(long_id)
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
        Ok(db.intern_lowering_function(long_id))
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
}
impl ConcreteFunctionWithBodyId {
    pub fn from_semantic(
        db: &dyn LoweringGroup,
        semantic: semantic::ConcreteFunctionWithBodyId,
    ) -> Self {
        db.intern_lowering_concrete_function_with_body(ConcreteFunctionWithBodyLongId::Semantic(
            semantic,
        ))
    }
    pub fn get(&self, db: &dyn LoweringGroup) -> ConcreteFunctionWithBodyLongId {
        db.lookup_intern_lowering_concrete_function_with_body(*self)
    }
    pub fn function_with_body_id(&self, db: &dyn LoweringGroup) -> FunctionWithBodyId {
        self.get(db).function_with_body_id(db)
    }
    pub fn substitution(&self, db: &dyn LoweringGroup) -> Maybe<GenericSubstitution> {
        self.get(db).substitution(db)
    }
    pub fn function_id(&self, db: &dyn LoweringGroup) -> Maybe<FunctionId> {
        self.get(db).function_id(db)
    }
    pub fn signature(&self, db: &dyn LoweringGroup) -> Maybe<Signature> {
        let generic_signature = self.function_with_body_id(db).signature(db)?;
        let substitution = self.substitution(db)?;
        SubstitutionRewriter { db: db.upcast(), substitution: &substitution }
            .rewrite(generic_signature)
    }
    pub fn from_no_generics_free(
        db: &dyn LoweringGroup,
        free_function_id: FreeFunctionId,
    ) -> Option<Self> {
        let semantic = semantic::ConcreteFunctionWithBodyId::from_no_generics_free(
            db.upcast(),
            free_function_id,
        )?;
        Some(db.intern_lowering_concrete_function_with_body(
            ConcreteFunctionWithBodyLongId::Semantic(semantic),
        ))
    }
    pub fn base_semantic_function(
        &self,
        db: &dyn LoweringGroup,
    ) -> semantic::ConcreteFunctionWithBodyId {
        self.get(db).base_semantic_function(db)
    }
    pub fn stable_location(&self, db: &dyn LoweringGroup) -> Maybe<StableLocation> {
        let semantic_db = db.upcast();
        Ok(match self.get(db) {
            ConcreteFunctionWithBodyLongId::Semantic(id) => id.stable_location(semantic_db),
            ConcreteFunctionWithBodyLongId::Generated(generated) => {
                let parent_id = generated.parent.function_with_body_id(semantic_db);
                StableLocation {
                    module_file_id: parent_id.module_file_id(semantic_db.upcast()),
                    stable_ptr: db.function_body(parent_id)?.exprs[generated.element]
                        .stable_ptr()
                        .untyped(),
                }
            }
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
define_short_id!(FunctionId, FunctionLongId, LoweringGroup, lookup_intern_lowering_function);
impl FunctionLongId {
    pub fn body(&self, db: &dyn LoweringGroup) -> Maybe<Option<ConcreteFunctionWithBodyId>> {
        let semantic_db = db.upcast();
        let long_id = match *self {
            FunctionLongId::Semantic(id) => {
                let Some(body) = id.get_concrete(semantic_db).body(semantic_db)? else {
                    return Ok(None);
                };
                ConcreteFunctionWithBodyLongId::Semantic(body)
            }
            FunctionLongId::Generated(generated) => return Ok(Some(generated.body(db))),
        };
        Ok(Some(db.intern_lowering_concrete_function_with_body(long_id)))
    }
    pub fn signature(&self, db: &dyn LoweringGroup) -> Maybe<Signature> {
        match self {
            FunctionLongId::Semantic(semantic) => {
                Ok(db.concrete_function_signature(*semantic)?.into())
            }
            FunctionLongId::Generated(generated) => generated.body(db).signature(db),
        }
    }
}
impl FunctionId {
    pub fn body(&self, db: &dyn LoweringGroup) -> Maybe<Option<ConcreteFunctionWithBodyId>> {
        db.lookup_intern_lowering_function(*self).body(db)
    }
    pub fn signature(&self, db: &dyn LoweringGroup) -> Maybe<Signature> {
        db.lookup_intern_lowering_function(*self).signature(db)
    }
    pub fn lookup(&self, db: &dyn LoweringGroup) -> FunctionLongId {
        db.lookup_intern_lowering_function(*self)
    }
}
pub trait SemanticFunctionIdEx {
    fn lowered(&self, db: &dyn LoweringGroup) -> FunctionId;
}
impl SemanticFunctionIdEx for semantic::FunctionId {
    fn lowered(&self, db: &dyn LoweringGroup) -> FunctionId {
        db.intern_lowering_function(FunctionLongId::Semantic(*self))
    }
}
impl<'a> DebugWithDb<dyn LoweringGroup + 'a> for FunctionLongId {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        db: &(dyn LoweringGroup + 'a),
    ) -> std::fmt::Result {
        match self {
            FunctionLongId::Semantic(semantic) => semantic.fmt(f, db),
            FunctionLongId::Generated(generated) => {
                write!(
                    f,
                    "{}[expr{}]",
                    generated.parent.full_path(db.upcast()),
                    generated.element.index()
                )
            }
        }
    }
}

/// Generated function.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct GeneratedFunction {
    pub parent: semantic::ConcreteFunctionWithBodyId,
    pub element: semantic::ExprId,
}
impl GeneratedFunction {
    pub fn body(&self, db: &dyn LoweringGroup) -> ConcreteFunctionWithBodyId {
        let GeneratedFunction { parent, element } = *self;
        let long_id =
            ConcreteFunctionWithBodyLongId::Generated(GeneratedFunction { parent, element });
        db.intern_lowering_concrete_function_with_body(long_id)
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
}
impl From<semantic::Signature> for Signature {
    fn from(value: semantic::Signature) -> Self {
        let semantic::Signature { params, return_type, implicits, panicable, .. } = value;
        let ref_params = params
            .iter()
            .filter(|param| param.mutability == Mutability::Reference)
            .map(|param| parameter_as_member_path(param.clone()))
            .collect();
        let params: Vec<semantic::ExprVarMemberPath> =
            params.into_iter().map(parameter_as_member_path).collect();
        Self { params, extra_rets: ref_params, return_type, implicits, panicable }
    }
}
semantic::add_rewrite!(<'a>, SubstitutionRewriter<'a>, DiagnosticAdded, Signature);

/// Converts a [semantic::Parameter] to a [semantic::ExprVarMemberPath].
fn parameter_as_member_path(param: semantic::Parameter) -> semantic::ExprVarMemberPath {
    let semantic::Parameter { id, ty, stable_ptr, .. } = param;
    semantic::ExprVarMemberPath::Var(ExprVar {
        var: semantic::VarId::Param(id),
        ty,
        stable_ptr: ast::ExprPtr(stable_ptr.0),
    })
}

define_short_id!(LocationId, Location, LoweringGroup, lookup_intern_location);
impl LocationId {
    pub fn from_stable_location(
        db: &dyn LoweringGroup,
        stable_location: StableLocation,
    ) -> LocationId {
        db.intern_location(Location::new(stable_location))
    }

    pub fn get(&self, db: &dyn LoweringGroup) -> Location {
        db.lookup_intern_location(*self)
    }

    // Adds a note to the location.
    pub fn with_note(&self, db: &dyn LoweringGroup, note: String) -> LocationId {
        db.intern_location(self.get(db).with_note(note))
    }

    // Adds a note that this location was generated while compiling an auto-generated function.
    pub fn with_auto_generation_note(
        &self,
        db: &dyn LoweringGroup,
        logic_name: &str,
    ) -> LocationId {
        self.with_note(db, format!("while compiling auto-generated {logic_name}"))
    }
}
