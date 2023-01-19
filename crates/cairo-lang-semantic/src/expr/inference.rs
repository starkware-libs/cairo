use std::collections::HashMap;

use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use itertools::zip_eq;

use crate::corelib::never_ty;
use crate::db::SemanticGroup;
use crate::types::ConcreteEnumLongId;
use crate::{
    ConcreteEnumId, ConcreteTypeId, ConcreteVariant, GenericArgumentId, Pattern, TypeId, TypeLongId,
};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct TypeVar {
    pub id: usize,
}

// TODO(spapini): Add to diagnostics.
pub enum InferenceError {
    // TODO(spapini): Cycle.
    Cycle { type_var: TypeVar },
    KindMismatch { ty0: TypeId, ty1: TypeId },
    GenericArgMismatch { garg0: GenericArgumentId, garg1: GenericArgumentId },
}

pub struct Inference<'db> {
    db: &'db dyn SemanticGroup,
    reductions: HashMap<TypeVar, TypeId>,
    vars: Vec<SyntaxStablePtrId>,
    // TODO(spapini): Rank.
    pub enabled: bool,
}
impl<'db> Inference<'db> {
    pub fn disabled(db: &'db dyn SemanticGroup) -> Self {
        Self { db, reductions: Default::default(), vars: vec![], enabled: false }
    }

    pub fn new(db: &'db dyn SemanticGroup) -> Self {
        Self { db, reductions: Default::default(), vars: vec![], enabled: true }
    }

    pub fn new_var(&mut self, stable_ptr: SyntaxStablePtrId) -> TypeId {
        let res = self.db.intern_type(TypeLongId::Var(TypeVar { id: self.vars.len() }));
        self.vars.push(stable_ptr);
        res
    }

    pub fn first_undetermined_variable(&self) -> Option<SyntaxStablePtrId> {
        for (id, stable_ptr) in self.vars.iter().enumerate() {
            if !self.reductions.contains_key(&TypeVar { id }) {
                return Some(*stable_ptr);
            }
        }
        None
    }

    pub fn reduce_ty(&mut self, ty: TypeId) -> TypeId {
        let long_type_id = self.db.lookup_intern_type(ty);
        let long_type_id = match long_type_id {
            TypeLongId::Concrete(concrete) => {
                let generic_args = self.reduce_generic_args(&concrete.generic_args(self.db));
                TypeLongId::Concrete(ConcreteTypeId::new(
                    self.db,
                    concrete.generic_type(self.db),
                    generic_args,
                ))
            }
            TypeLongId::Tuple(tys) => {
                TypeLongId::Tuple(tys.into_iter().map(|ty| self.reduce_ty(ty)).collect())
            }
            TypeLongId::Var(var) => return self.reduce_var(var),
            TypeLongId::GenericParameter(_) | TypeLongId::Missing(_) => return ty,
        };
        self.db.intern_type(long_type_id)
    }

    pub fn reduce_generic_args(
        &mut self,
        generic_args: &[GenericArgumentId],
    ) -> Vec<GenericArgumentId> {
        generic_args.iter().copied().map(|garg| self.reduce_generic_arg(garg)).collect()
    }

    pub fn reduce_concrete_variant(&mut self, concrete_variant: &mut ConcreteVariant) {
        concrete_variant.ty = self.reduce_ty(concrete_variant.ty);
        concrete_variant.concrete_enum_id =
            self.reduce_concrete_enum(concrete_variant.concrete_enum_id);
    }

    pub fn reduce_concrete_enum(&mut self, concrete_enum_id: ConcreteEnumId) -> ConcreteEnumId {
        let concrete_enum = self.db.lookup_intern_concrete_enum(concrete_enum_id);
        let generic_args = self.reduce_generic_args(&concrete_enum.generic_args);

        self.db.intern_concrete_enum(ConcreteEnumLongId { generic_args, ..concrete_enum })
    }

    pub fn reduce_var(&mut self, var: TypeVar) -> TypeId {
        if let Some(new_ty) = self.reductions.get(&var) {
            let new_ty = self.reduce_ty(*new_ty);
            self.reductions.insert(var, new_ty);
            return new_ty;
        }
        self.db.intern_type(TypeLongId::Var(var))
    }

    /// Conform ty0 to ty1. Not symmetric.
    pub fn conform_ty(&mut self, ty0: TypeId, ty1: TypeId) -> Result<TypeId, InferenceError> {
        let ty0 = self.reduce_ty(ty0);
        let ty1 = self.reduce_ty(ty1);
        if ty0 == never_ty(self.db) {
            return Ok(ty1);
        }
        if ty0 == ty1 {
            return Ok(ty0);
        }
        let long_ty0 = self.db.lookup_intern_type(ty0);
        let long_ty1 = self.db.lookup_intern_type(ty1);
        match long_ty1 {
            TypeLongId::Var(var) => return self.assign(var, ty0),
            TypeLongId::Missing(_) => return Ok(ty1),
            _ => {}
        }
        match long_ty0 {
            TypeLongId::Concrete(concrete0) => {
                let TypeLongId::Concrete(concrete1) = long_ty1 else {
                    return Err(InferenceError::KindMismatch { ty0, ty1 });
                };
                if concrete0.generic_type(self.db) != concrete1.generic_type(self.db) {
                    return Err(InferenceError::KindMismatch { ty0, ty1 });
                }
                let gargs0 = concrete0.generic_args(self.db);
                let gargs1 = concrete1.generic_args(self.db);
                let gargs = zip_eq(gargs0, gargs1)
                    .into_iter()
                    .map(|(garg0, garg1)| self.conform_generic_arg(garg0, garg1))
                    .collect::<Result<Vec<_>, _>>()?;
                let long_ty = TypeLongId::Concrete(ConcreteTypeId::new(
                    self.db,
                    concrete0.generic_type(self.db),
                    gargs,
                ));
                Ok(self.db.intern_type(long_ty))
            }
            TypeLongId::Tuple(tys0) => {
                let TypeLongId::Tuple(tys1) = long_ty1 else {
                    return Err(InferenceError::KindMismatch { ty0, ty1 });
                };
                if tys0.len() != tys1.len() {
                    return Err(InferenceError::KindMismatch { ty0, ty1 });
                }
                let tys = zip_eq(tys0, tys1)
                    .into_iter()
                    .map(|(subty0, subty1)| self.conform_ty(subty0, subty1))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(self.db.intern_type(TypeLongId::Tuple(tys)))
            }
            TypeLongId::GenericParameter(_) => Err(InferenceError::KindMismatch { ty0, ty1 }),
            TypeLongId::Var(var) => self.assign(var, ty1),
            TypeLongId::Missing(_) => Ok(ty0),
        }
    }

    pub fn conform_generic_arg(
        &mut self,
        garg0: GenericArgumentId,
        garg1: GenericArgumentId,
    ) -> Result<GenericArgumentId, InferenceError> {
        if garg0 == garg1 {
            return Ok(garg0);
        }
        match garg0 {
            GenericArgumentId::Type(gty0) => {
                let GenericArgumentId::Type(gty1) = garg1 else {
                    return Err(InferenceError::GenericArgMismatch { garg0, garg1 });
                };
                Ok(GenericArgumentId::Type(self.conform_ty(gty0, gty1)?))
            }
            GenericArgumentId::Literal(_) => {
                Err(InferenceError::GenericArgMismatch { garg0, garg1 })
            }
        }
    }

    fn reduce_generic_arg(&mut self, garg: GenericArgumentId) -> GenericArgumentId {
        match garg {
            GenericArgumentId::Type(ty) => GenericArgumentId::Type(self.reduce_ty(ty)),
            GenericArgumentId::Literal(_) => garg,
        }
    }

    fn assign(&mut self, var: TypeVar, ty: TypeId) -> Result<TypeId, InferenceError> {
        assert!(!self.reductions.contains_key(&var), "Cannot reassign variable.");
        if self.contains_var(ty, var) {
            return Err(InferenceError::Cycle { type_var: var });
        }
        self.reductions.insert(var, ty);
        Ok(ty)
    }

    pub fn reduce_pattern(&mut self, pattern: &mut Pattern) {
        match pattern {
            Pattern::Variable(pat) => pat.var.ty = self.reduce_ty(pat.var.ty),
            Pattern::Struct(pat) => {
                pat.ty = self.reduce_ty(pat.ty);
                for (_, pat) in pat.field_patterns.iter_mut() {
                    self.reduce_pattern(pat);
                }
            }
            Pattern::Tuple(pat) => {
                pat.ty = self.reduce_ty(pat.ty);
                for pat in pat.field_patterns.iter_mut() {
                    self.reduce_pattern(pat);
                }
            }
            Pattern::EnumVariant(pat) => {
                self.reduce_concrete_variant(&mut pat.variant);
                pat.ty = self.reduce_ty(pat.ty);
                self.reduce_pattern(&mut pat.inner_pattern);
            }
            Pattern::Literal(_) | Pattern::Otherwise(_) => {}
        }
    }

    pub fn contains_var(&mut self, ty: TypeId, var: TypeVar) -> bool {
        match self.db.lookup_intern_type(self.reduce_ty(ty)) {
            TypeLongId::Concrete(concrete) => {
                concrete.generic_args(self.db).into_iter().any(|garg| match garg {
                    GenericArgumentId::Type(ty) => self.contains_var(ty, var),
                    GenericArgumentId::Literal(_) => false,
                })
            }
            TypeLongId::Tuple(tys) => tys.into_iter().any(|ty| self.contains_var(ty, var)),
            TypeLongId::Var(new_var) => {
                if new_var == var {
                    return true;
                }
                if let Some(ty) = self.reductions.get(&new_var) {
                    return self.contains_var(*ty, var);
                }
                false
            }
            TypeLongId::GenericParameter(_) | TypeLongId::Missing(_) => false,
        }
    }
}
