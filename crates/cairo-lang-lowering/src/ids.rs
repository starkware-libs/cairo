use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::UnstableSalsaId;
use cairo_lang_diagnostics::Maybe;
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_utils::define_short_id;
use defs::ids::FreeFunctionId;
use semantic::substitution::{GenericSubstitution, SemanticRewriter, SubstitutionRewriter};
use {cairo_lang_defs as defs, cairo_lang_semantic as semantic};

// use {cairo_lang_defs as defs, cairo_lang_semantic as semantic};
use crate::db::LoweringGroup;

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum FunctionWithBodyLongId {
    Semantic(defs::ids::FunctionWithBodyId),
    Generated { parent: defs::ids::FunctionWithBodyId, element: SyntaxStablePtrId },
}
define_short_id!(
    FunctionWithBodyId,
    FunctionWithBodyLongId,
    LoweringGroup,
    lookup_intern_lowering_function_with_body
);
impl FunctionWithBodyLongId {
    pub fn function_with_body_id(
        &self,
        _db: &dyn LoweringGroup,
    ) -> cairo_lang_defs::ids::FunctionWithBodyId {
        match *self {
            FunctionWithBodyLongId::Semantic(id) => id,
            FunctionWithBodyLongId::Generated { parent, .. } => parent,
        }
    }
}
impl FunctionWithBodyId {
    pub fn semantic_function(
        &self,
        db: &dyn LoweringGroup,
    ) -> cairo_lang_defs::ids::FunctionWithBodyId {
        db.lookup_intern_lowering_function_with_body(*self).function_with_body_id(db)
    }
    pub fn signature(&self, db: &dyn LoweringGroup) -> Maybe<semantic::Signature> {
        Ok(db.function_with_body_lowering(*self)?.signature.clone())
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
    pub fn function_with_body_id(&self, db: &dyn LoweringGroup) -> FunctionWithBodyId {
        db.lookup_intern_lowering_concrete_function_with_body(*self).function_with_body_id(db)
    }
    pub fn substitution(&self, db: &dyn LoweringGroup) -> Maybe<GenericSubstitution> {
        db.lookup_intern_lowering_concrete_function_with_body(*self).substitution(db)
    }
    pub fn function_id(&self, db: &dyn LoweringGroup) -> Maybe<FunctionId> {
        db.lookup_intern_lowering_concrete_function_with_body(*self).function_id(db)
    }
    pub fn signature(&self, db: &dyn LoweringGroup) -> Maybe<semantic::Signature> {
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
}

/// Function.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum FunctionLongId {
    Semantic(semantic::FunctionId),
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
    pub fn signature(&self, db: &dyn LoweringGroup) -> Maybe<semantic::Signature> {
        match self {
            FunctionLongId::Semantic(semantic) => db.concrete_function_signature(*semantic),
            FunctionLongId::Generated(generated) => generated.body(db).signature(db),
        }
    }
}
impl FunctionId {
    pub fn body(&self, db: &dyn LoweringGroup) -> Maybe<Option<ConcreteFunctionWithBodyId>> {
        db.lookup_intern_lowering_function(*self).body(db)
    }
    pub fn signature(&self, db: &dyn LoweringGroup) -> Maybe<semantic::Signature> {
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
                // TODO(spapini): Differentiate between the generated functions according to
                // `element`.
                write!(f, "{:?}<Generated>", generated.parent.debug(db))
            }
        }
    }
}

/// Generated function.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct GeneratedFunction {
    pub parent: semantic::ConcreteFunctionWithBodyId,
    pub element: SyntaxStablePtrId,
}
impl GeneratedFunction {
    pub fn body(&self, db: &dyn LoweringGroup) -> ConcreteFunctionWithBodyId {
        let GeneratedFunction { parent, element } = *self;
        let long_id =
            ConcreteFunctionWithBodyLongId::Generated(GeneratedFunction { parent, element });
        db.intern_lowering_concrete_function_with_body(long_id)
    }
}
