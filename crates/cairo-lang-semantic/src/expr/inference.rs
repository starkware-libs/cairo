//! Bidirectional type inference.

use std::collections::HashMap;

use cairo_lang_defs::ids::{
    ConstantId, EnumId, ExternFunctionId, ExternTypeId, FreeFunctionId, GenericKind,
    GenericParamId, ImplDefId, ImplFunctionId, LanguageElementId, LocalVarId, MemberId, ParamId,
    StructId, TraitFunctionId, TraitId, VarId, VariantId,
};
use cairo_lang_diagnostics::{skip_diagnostic, DiagnosticAdded, Maybe};
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use itertools::{zip_eq, Itertools};

use crate::corelib::never_ty;
use crate::db::SemanticGroup;
use crate::diagnostic::{SemanticDiagnosticKind, SemanticDiagnostics};
use crate::expr::objects::*;
use crate::expr::pattern::*;
use crate::items::functions::{
    ConcreteFunctionWithBody, GenericFunctionId, GenericFunctionWithBodyId, ImplGenericFunctionId,
    ImplGenericFunctionWithBodyId,
};
use crate::items::generics::{GenericParamConst, GenericParamImpl, GenericParamType};
use crate::items::imp::{find_candidate_impls_at_context, ImplId, ImplLookupContext};
use crate::items::trt::{ConcreteTraitGenericFunctionId, ConcreteTraitGenericFunctionLongId};
use crate::literals::LiteralId;
use crate::substitution::{GenericSubstitution, HasDb, SemanticRewriter, SubstitutionRewriter};
use crate::types::{
    peel_snapshots, ConcreteEnumLongId, ConcreteExternTypeLongId, ConcreteStructLongId,
};
use crate::{
    add_basic_rewrites, add_expr_rewrites, ConcreteEnumId, ConcreteExternTypeId, ConcreteFunction,
    ConcreteImplId, ConcreteImplLongId, ConcreteStructId, ConcreteTraitId, ConcreteTraitLongId,
    ConcreteTypeId, ConcreteVariant, ExprLiteral, FunctionId, FunctionLongId, GenericArgumentId,
    GenericParam, LocalVariable, Member, Parameter, Pattern, SemanticObject, Signature, TypeId,
    TypeLongId,
};
/// A type variable, created when a generic type argument is not passed, and thus is not known
/// yet and needs to be inferred.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct TypeVar {
    pub id: usize,
    pub stable_ptr: SyntaxStablePtrId,
}

/// An impl variable, created when a generic type argument is not passed, and thus is not known
/// yet and needs to be inferred.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct ImplVar {
    pub id: usize,
    pub concrete_trait_id: ConcreteTraitId,
    pub stable_ptr: SyntaxStablePtrId,
}

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub enum InferenceVar {
    Type(usize),
    Impl(usize),
}

// TODO(spapini): Add to diagnostics.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum InferenceError {
    Disabled,
    Failed(DiagnosticAdded),
    Cycle { var: InferenceVar },
    TypeKindMismatch { ty0: TypeId, ty1: TypeId },
    ImplKindMismatch { impl0: ImplId, impl1: ImplId },
    GenericArgMismatch { garg0: GenericArgumentId, garg1: GenericArgumentId },
    TraitMismatch { trt0: TraitId, trt1: TraitId },
    ConstInferenceNotSupported,
    NoImplsFound { concrete_trait_id: ConcreteTraitId },
    MultipleImplsFound { concrete_trait_id: ConcreteTraitId, impls: Vec<ImplId> },
    TypeNotInferred { ty: TypeId },
    AlreadyReported,
}
pub type InferenceResult<T> = Result<T, InferenceError>;

impl From<DiagnosticAdded> for InferenceError {
    fn from(value: DiagnosticAdded) -> Self {
        InferenceError::Failed(value)
    }
}
impl InferenceError {
    pub fn report(
        &self,
        diagnostics: &mut SemanticDiagnostics,
        stable_ptr: SyntaxStablePtrId,
    ) -> DiagnosticAdded {
        match self {
            InferenceError::Failed(diagnostic_added) => *diagnostic_added,
            InferenceError::AlreadyReported => skip_diagnostic(),
            _ => diagnostics.report_by_ptr(
                stable_ptr,
                SemanticDiagnosticKind::InternalInferenceError(self.clone()),
            ),
        }
    }
}

/// State of inference.
#[derive(Clone)]
pub struct Inference<'db> {
    db: &'db dyn SemanticGroup,
    /// Current inferred assignment for type variables.
    type_assignment: HashMap<usize, TypeId>,
    /// Current inferred assignment for impl variables.
    impl_assignment: HashMap<usize, ImplId>,
    /// Stable pointers for each type variable, used for reporting diagnostics properly.
    type_vars: Vec<TypeVar>,
    impl_vars: Vec<ImplVar>,
    impl_candidates: Vec<OrderedHashSet<ImplId>>,
    /// Whether inference is enabled.
    pub enabled: bool,
    pub version: usize,
    // TODO(spapini): Rank.
}
impl<'db> Inference<'db> {
    /// Creates a new [Inference] instance.
    pub fn new(db: &'db dyn SemanticGroup) -> Self {
        Self {
            db,
            enabled: true,
            type_assignment: Default::default(),
            impl_assignment: Default::default(),
            type_vars: Default::default(),
            impl_vars: Default::default(),
            impl_candidates: Default::default(),
            version: 0,
        }
    }

    /// Creates a disabled [Inference] instance, where no inference is being performed.
    pub fn disabled(db: &'db dyn SemanticGroup) -> Self {
        Self {
            db,
            enabled: false,
            type_assignment: Default::default(),
            impl_assignment: Default::default(),
            type_vars: Default::default(),
            impl_vars: Default::default(),
            impl_candidates: Default::default(),
            version: 0,
        }
    }

    /// Allocated a new [TypeVar] for an unknown type that needs to be inferred,
    pub fn new_type_var(&mut self, stable_ptr: SyntaxStablePtrId) -> TypeId {
        let var = TypeVar { id: self.type_vars.len(), stable_ptr };
        self.type_vars.push(var);
        self.version += 1;
        self.db.intern_type(TypeLongId::Var(var))
    }

    /// Allocated a new [ImplVar] for an unknown type that needs to be inferred,
    pub fn new_impl_var(
        &mut self,
        concrete_trait_id: ConcreteTraitId,
        stable_ptr: SyntaxStablePtrId,
        mut lookup_context: ImplLookupContext,
    ) -> Maybe<ImplId> {
        lookup_context
            .extra_modules
            .push(concrete_trait_id.trait_id(self.db).module_file_id(self.db.upcast()).0);

        let candidates = find_candidate_impls_at_context(
            self.db,
            self,
            &lookup_context,
            concrete_trait_id,
            stable_ptr,
        )?;
        self.impl_candidates.push(candidates);

        let var = ImplVar { id: self.impl_vars.len(), concrete_trait_id, stable_ptr };
        self.impl_vars.push(var);
        self.version += 1;
        Ok(ImplId::ImplVar(var))
    }

    /// Retrieves the first variable that is still not inferred, or None, if everything is
    /// inferred.
    pub fn first_undetermined_variable(&mut self) -> Option<(SyntaxStablePtrId, InferenceError)> {
        for (id, var) in self.type_vars.iter().enumerate() {
            if !self.type_assignment.contains_key(&id) {
                let ty = self.db.intern_type(TypeLongId::Var(*var));
                return Some((var.stable_ptr, InferenceError::TypeNotInferred { ty }));
            }
        }
        for (id, var) in self.impl_vars.clone().into_iter().enumerate() {
            self.relax_impl_var(var).ok();
            if !self.impl_assignment.contains_key(&id) {
                let concrete_trait_id = match self.rewrite(var.concrete_trait_id) {
                    Ok(concrete_trait_id) => concrete_trait_id,
                    Err(err) => {
                        return Some((var.stable_ptr, err));
                    }
                };
                if self.impl_candidates[id].is_empty() {
                    return Some((var.stable_ptr, InferenceError::AlreadyReported));
                }
                let impls = self.impl_candidates[id].clone();
                let impls = impls
                    .into_iter()
                    .map(|impl_id| self.rewrite(impl_id).unwrap_or(impl_id))
                    .collect();
                return Some((
                    var.stable_ptr,
                    InferenceError::MultipleImplsFound { concrete_trait_id, impls },
                ));
            }
        }
        None
    }

    /// Returns the number of variables allocated for current inference.
    /// Useful for deciding if new variables were introduced.
    pub fn n_variables(&self) -> usize {
        self.type_vars.len() + self.impl_vars.len()
    }

    /// Conforms ty0 to ty1. Should be called when ty0 should be coerced to ty1. Not symmetric.
    /// Returns the reduced type for ty0, or an error if the type is no coercible.
    pub fn conform_ty(&mut self, ty0: TypeId, ty1: TypeId) -> Result<TypeId, InferenceError> {
        Ok(self.conform_ty_ex(ty0, ty1, false)?.0)
    }

    /// Same as conform_ty but supports adding snapshots to ty0 if `ty0_is_self` is true.
    /// Returns the reduced type for ty0 and the number of snapshots that needs to be added
    /// for the types to conform.
    pub fn conform_ty_ex(
        &mut self,
        ty0: TypeId,
        ty1: TypeId,
        ty0_is_self: bool,
    ) -> Result<(TypeId, usize), InferenceError> {
        let ty0 = self.rewrite(ty0)?;
        let ty1 = self.rewrite(ty1)?;
        if ty0 == never_ty(self.db) {
            return Ok((ty1, 0));
        }
        if ty0 == ty1 {
            return Ok((ty0, 0));
        }
        let long_ty1 = self.db.lookup_intern_type(ty1);
        match long_ty1 {
            TypeLongId::Var(var) => return Ok((self.assign_ty(var, ty0)?, 0)),
            TypeLongId::Missing(_) => return Ok((ty1, 0)),
            TypeLongId::Snapshot(inner_ty) if ty0_is_self && inner_ty == ty0 => {
                return Ok((ty1, 1));
            }
            _ => {}
        }
        let n_snapshots = 0;
        let long_ty0 = self.db.lookup_intern_type(ty0);

        match long_ty0 {
            TypeLongId::Concrete(concrete0) => {
                let (n_snapshots, long_ty1) = self.maybe_peel_snapshots(ty0_is_self, ty1);
                let TypeLongId::Concrete(concrete1) = long_ty1 else {
                    return Err(InferenceError::TypeKindMismatch { ty0, ty1 });
                };
                if concrete0.generic_type(self.db) != concrete1.generic_type(self.db) {
                    return Err(InferenceError::TypeKindMismatch { ty0, ty1 });
                }
                let gargs0 = concrete0.generic_args(self.db);
                let gargs1 = concrete1.generic_args(self.db);
                let gargs = self.conform_generic_args(&gargs0, &gargs1)?;
                let long_ty = TypeLongId::Concrete(ConcreteTypeId::new(
                    self.db,
                    concrete0.generic_type(self.db),
                    gargs,
                ));
                Ok((self.db.intern_type(long_ty), n_snapshots))
            }
            TypeLongId::Tuple(tys0) => {
                let (n_snapshots, long_ty1) = self.maybe_peel_snapshots(ty0_is_self, ty1);
                let TypeLongId::Tuple(tys1) = long_ty1 else {
                    return Err(InferenceError::TypeKindMismatch { ty0, ty1 });
                };
                if tys0.len() != tys1.len() {
                    return Err(InferenceError::TypeKindMismatch { ty0, ty1 });
                }
                let tys = zip_eq(tys0, tys1)
                    .map(|(subty0, subty1)| self.conform_ty(subty0, subty1))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok((self.db.intern_type(TypeLongId::Tuple(tys)), n_snapshots))
            }
            TypeLongId::Snapshot(ty0) => {
                let TypeLongId::Snapshot(ty1) = long_ty1 else {
                    return Err(InferenceError::TypeKindMismatch { ty0, ty1 });
                };
                let (ty, n_snapshots) = self.conform_ty_ex(ty0, ty1, ty0_is_self)?;
                Ok((self.db.intern_type(TypeLongId::Snapshot(ty)), n_snapshots))
            }
            TypeLongId::GenericParameter(_) => Err(InferenceError::TypeKindMismatch { ty0, ty1 }),
            TypeLongId::Var(var) => Ok((self.assign_ty(var, ty1)?, n_snapshots)),
            TypeLongId::Missing(_) => Ok((ty0, n_snapshots)),
        }
    }

    // Conditionally peels snapshots.
    fn maybe_peel_snapshots(&mut self, ty0_is_self: bool, ty1: TypeId) -> (usize, TypeLongId) {
        let (n_snapshots, long_ty1) = if ty0_is_self {
            peel_snapshots(self.db, ty1)
        } else {
            (0, self.db.lookup_intern_type(ty1))
        };
        (n_snapshots, long_ty1)
    }

    /// Conforms generics args. See `conform_ty()`.
    fn conform_generic_args(
        &mut self,
        gargs0: &[GenericArgumentId],
        gargs1: &[GenericArgumentId],
    ) -> Result<Vec<GenericArgumentId>, InferenceError> {
        zip_eq(gargs0, gargs1)
            .map(|(garg0, garg1)| self.conform_generic_arg(*garg0, *garg1))
            .collect::<Result<Vec<_>, _>>()
    }

    /// Conforms a generics arg. See `conform_ty()`.
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
            GenericArgumentId::Impl(impl0) => {
                let GenericArgumentId::Impl(impl1) = garg1 else {
                    return Err(InferenceError::GenericArgMismatch { garg0, garg1 });
                };
                Ok(GenericArgumentId::Impl(self.conform_impl(impl0, impl1)?))
            }
        }
    }

    /// Assigns a value to an [ImplVar]. Return the assigned impl, or an error.
    /// Assumes the variable is not already assigned.
    fn assign_impl(&mut self, var: ImplVar, impl_id: ImplId) -> InferenceResult<ImplId> {
        if !self.enabled {
            return Err(InferenceError::Disabled);
        }
        if let Some(other_impl) = self.impl_assignment.get(&var.id) {
            return self.conform_impl(impl_id, *other_impl);
        }
        assert!(!self.impl_assignment.contains_key(&var.id), "Cannot reassign variable.");
        if self.impl_contains_var(&impl_id, InferenceVar::Impl(var.id))? {
            return Err(InferenceError::Cycle { var: InferenceVar::Impl(var.id) });
        }
        self.impl_assignment.insert(var.id, impl_id);
        self.version += 1;
        Ok(impl_id)
    }

    /// Conforms an impl. See `conform_ty()`.
    pub fn conform_impl(&mut self, impl0: ImplId, impl1: ImplId) -> InferenceResult<ImplId> {
        let impl0 = self.rewrite(impl0)?;
        let impl1 = self.rewrite(impl1)?;
        if impl0 == impl1 {
            return Ok(impl0);
        }
        if let ImplId::ImplVar(var) = impl1 {
            self.conform_traits(var.concrete_trait_id, self.db.impl_concrete_trait(impl0)?)?;
            let impl_id = self.rewrite(impl0)?;
            return self.assign_impl(var, impl_id);
        }
        match impl0 {
            ImplId::ImplVar(var) => {
                self.conform_traits(var.concrete_trait_id, self.db.impl_concrete_trait(impl1)?)?;
                let impl_id = self.rewrite(impl1)?;
                self.assign_impl(var, impl_id)
            }
            ImplId::Concrete(concrete0) => {
                let ImplId::Concrete(concrete1) = impl1 else {
                    return Err(InferenceError::ImplKindMismatch { impl0, impl1 });
                };
                let concrete0 = self.db.lookup_intern_concrete_impl(concrete0);
                let concrete1 = self.db.lookup_intern_concrete_impl(concrete1);
                if concrete0.impl_def_id != concrete1.impl_def_id {
                    return Err(InferenceError::ImplKindMismatch { impl0, impl1 });
                }
                let gargs0 = concrete0.generic_args;
                let gargs1 = concrete1.generic_args;
                let generic_args = self.conform_generic_args(&gargs0, &gargs1)?;
                Ok(ImplId::Concrete(self.db.intern_concrete_impl(ConcreteImplLongId {
                    impl_def_id: concrete0.impl_def_id,
                    generic_args,
                })))
            }
            ImplId::GenericParameter(_) => Err(InferenceError::ImplKindMismatch { impl0, impl1 }),
        }
    }

    /// Conforms generics traits. See `conform_ty()`.
    pub fn conform_traits(
        &mut self,
        trt0: ConcreteTraitId,
        trt1: ConcreteTraitId,
    ) -> Result<ConcreteTraitId, InferenceError> {
        let trt0 = self.db.lookup_intern_concrete_trait(trt0);
        let trt1 = self.db.lookup_intern_concrete_trait(trt1);
        if trt0.trait_id != trt1.trait_id {
            return Err(InferenceError::TraitMismatch { trt0: trt0.trait_id, trt1: trt1.trait_id });
        }
        let generic_args = self.conform_generic_args(&trt0.generic_args, &trt1.generic_args)?;
        Ok(self
            .db
            .intern_concrete_trait(ConcreteTraitLongId { trait_id: trt0.trait_id, generic_args }))
    }

    /// Assigns a value to a [TypeVar]. Return the assigned type, or an error.
    /// Assumes the variable is not already assigned.
    fn assign_ty(&mut self, var: TypeVar, ty: TypeId) -> InferenceResult<TypeId> {
        if !self.enabled {
            return Err(InferenceError::Disabled);
        }
        assert!(!self.type_assignment.contains_key(&var.id), "Cannot reassign variable.");
        let inference_var = InferenceVar::Type(var.id);
        if self.ty_contains_var(ty, inference_var)? {
            return Err(InferenceError::Cycle { var: inference_var });
        }
        self.type_assignment.insert(var.id, ty);
        self.version += 1;
        Ok(ty)
    }

    /// Checks if a type tree contains a certain [InferenceVar] somewhere. Used to avoid inference
    /// cycles.
    pub fn ty_contains_var(&mut self, ty: TypeId, var: InferenceVar) -> InferenceResult<bool> {
        Ok(match self.db.lookup_intern_type(self.rewrite(ty)?) {
            TypeLongId::Concrete(concrete) => {
                let generic_args = concrete.generic_args(self.db);
                self.generic_args_contain_var(&generic_args, var)?
            }
            TypeLongId::Tuple(tys) => tys
                .into_iter()
                .map(|ty| self.ty_contains_var(ty, var))
                .collect::<InferenceResult<Vec<_>>>()?
                .into_iter()
                .any(|x| x),
            TypeLongId::Snapshot(ty) => self.ty_contains_var(ty, var)?,
            TypeLongId::Var(new_var) => {
                if InferenceVar::Type(new_var.id) == var {
                    return Ok(true);
                }
                if let Some(ty) = self.type_assignment.get(&new_var.id) {
                    return self.ty_contains_var(*ty, var);
                }
                false
            }
            TypeLongId::GenericParameter(_) | TypeLongId::Missing(_) => false,
        })
    }

    /// Checks if a slice of generics arguments contain a certain [InferenceVar] somewhere. Used to
    /// avoid inference cycles.
    fn generic_args_contain_var(
        &mut self,
        generic_args: &[GenericArgumentId],
        var: InferenceVar,
    ) -> InferenceResult<bool> {
        for garg in generic_args {
            if match garg {
                GenericArgumentId::Type(ty) => self.ty_contains_var(*ty, var)?,
                GenericArgumentId::Literal(_) => false,
                GenericArgumentId::Impl(impl_id) => self.impl_contains_var(impl_id, var)?,
            } {
                return Ok(true);
            }
        }
        Ok(false)
    }

    /// Checks if an impl contains a certain [InferenceVar] somewhere. Used to avoid inference
    /// cycles.
    fn impl_contains_var(
        &mut self,
        impl_id: &ImplId,
        var: InferenceVar,
    ) -> Result<bool, InferenceError> {
        Ok(match impl_id {
            ImplId::Concrete(concrete_impl_id) => self.generic_args_contain_var(
                &self.db.lookup_intern_concrete_impl(*concrete_impl_id).generic_args,
                var,
            )?,
            ImplId::GenericParameter(_) => false,
            ImplId::ImplVar(new_var) => {
                if InferenceVar::Impl(new_var.id) == var {
                    return Ok(true);
                }
                if let Some(impl_id) = self.impl_assignment.get(&new_var.id).copied() {
                    return self.impl_contains_var(&impl_id, var);
                }
                false
            }
        })
    }

    /// Determines if an assignment to `generic_params` can be chosen s.t. `generic_args` will be
    /// substituted to `expected_generic_args`.
    pub fn can_infer_generics(
        &self,
        generic_params: &[GenericParam],
        generic_args: &[GenericArgumentId],
        expected_generic_args: &[GenericArgumentId],
        lookup_context: &ImplLookupContext,
        stable_ptr: SyntaxStablePtrId,
    ) -> bool {
        if generic_args.len() != expected_generic_args.len() {
            return false;
        }
        if generic_params.iter().any(|param| param.kind() != GenericKind::Type) {
            // Inference for non type generics are not supported yet.
            return false;
        }
        let mut inference = self.clone();
        let res = inference.infer_generics(
            generic_params,
            generic_args,
            expected_generic_args,
            lookup_context,
            stable_ptr,
        );
        res.is_ok()
    }

    /// Determines if a impl (possibly with free generic params) can provide a concrete trait.
    pub fn can_impl_trait(
        &self,
        impl_def_id: ImplDefId,
        concrete_trait_id: ConcreteTraitId,
        lookup_context: &ImplLookupContext,
        stable_ptr: SyntaxStablePtrId,
    ) -> bool {
        let Ok(imp_generic_param) = self.db.impl_def_generic_params(impl_def_id) else {
            return false
        };
        let Ok(imp_concrete_trait) = self.db.impl_def_concrete_trait(impl_def_id) else {
            return false
        };
        if imp_concrete_trait.trait_id(self.db) != concrete_trait_id.trait_id(self.db) {
            return false;
        }

        let long_concrete_trait = self.db.lookup_intern_concrete_trait(concrete_trait_id);
        let long_imp_concrete_trait = self.db.lookup_intern_concrete_trait(imp_concrete_trait);
        self.can_infer_generics(
            &imp_generic_param,
            &long_imp_concrete_trait.generic_args,
            &long_concrete_trait.generic_args,
            lookup_context,
            stable_ptr,
        )
    }

    /// Infers all the variables required to make an impl (possibly with free generic params)
    /// provide a concrete trait.
    pub fn infer_impl_trait(
        &mut self,
        impl_def_id: ImplDefId,
        concrete_trait_id: ConcreteTraitId,
        lookup_context: &ImplLookupContext,
        stable_ptr: SyntaxStablePtrId,
    ) -> Result<ImplId, InferenceError> {
        let imp_generic_params = self.db.impl_def_generic_params(impl_def_id)?;
        let imp_concrete_trait = self.db.impl_def_concrete_trait(impl_def_id)?;
        if imp_concrete_trait.trait_id(self.db) != concrete_trait_id.trait_id(self.db) {
            return Err(InferenceError::TraitMismatch {
                trt0: imp_concrete_trait.trait_id(self.db),
                trt1: concrete_trait_id.trait_id(self.db),
            });
        }

        let long_concrete_trait = self.db.lookup_intern_concrete_trait(concrete_trait_id);
        let long_imp_concrete_trait = self.db.lookup_intern_concrete_trait(imp_concrete_trait);
        let generic_args = self.infer_generics(
            &imp_generic_params,
            &long_imp_concrete_trait.generic_args,
            &long_concrete_trait.generic_args,
            lookup_context,
            stable_ptr,
        )?;
        Ok(ImplId::Concrete(
            self.db.intern_concrete_impl(ConcreteImplLongId { impl_def_id, generic_args }),
        ))
    }

    /// Chooses and assignment to generic_params s.t. generic_args will be substituted to
    /// expected_generic_args.
    /// Returns the generic_params assignment.
    pub fn infer_generics(
        &mut self,
        generic_params: &[GenericParam],
        generic_args: &[GenericArgumentId],
        expected_generic_args: &[GenericArgumentId],
        lookup_context: &ImplLookupContext,
        stable_ptr: SyntaxStablePtrId,
    ) -> InferenceResult<Vec<GenericArgumentId>> {
        let new_generic_args =
            self.infer_generic_args(generic_params, lookup_context, stable_ptr)?;
        let substitution = GenericSubstitution::new(generic_params, &new_generic_args);
        let mut rewriter = SubstitutionRewriter { db: self.db, substitution: &substitution };
        let generic_args = rewriter.rewrite(generic_args.iter().copied().collect_vec())?;
        self.conform_generic_args(&generic_args, expected_generic_args)?;
        self.rewrite(generic_args)
    }

    /// Infers all generic_arguments given the parameters.
    pub fn infer_generic_args(
        &mut self,
        generic_params: &[GenericParam],
        lookup_context: &ImplLookupContext,
        stable_ptr: SyntaxStablePtrId,
    ) -> InferenceResult<Vec<GenericArgumentId>> {
        let mut generic_args = vec![];
        let mut substitution = GenericSubstitution::default();
        for generic_param in generic_params {
            let generic_param = SubstitutionRewriter { db: self.db, substitution: &substitution }
                .rewrite(*generic_param)
                .map_err(InferenceError::Failed)?;
            let generic_arg =
                self.infer_generic_arg(&generic_param, lookup_context.clone(), stable_ptr)?;
            generic_args.push(generic_arg);
            substitution.0.insert(generic_param.id(), generic_arg);
        }
        Ok(generic_args)
    }

    /// Tries to infer a trait function as a method for `self_ty`.
    /// Supports snapshot snapshot coercions.
    ///
    /// Returns the deduced type and the number of snapshots that need to be added to it.
    pub fn infer_concrete_trait_by_self(
        &mut self,
        trait_function: TraitFunctionId,
        self_ty: TypeId,
        lookup_context: &ImplLookupContext,
        stable_ptr: SyntaxStablePtrId,
    ) -> Option<(ConcreteTraitId, usize)> {
        let trait_id = trait_function.trait_id(self.db.upcast());
        let signature = self.db.trait_function_signature(trait_function).ok()?;
        let first_param = signature.params.into_iter().next()?;
        if first_param.name != "self" {
            return None;
        }
        let generic_params = self.db.trait_generic_params(trait_id).ok()?;
        let generic_args =
            self.infer_generic_args(&generic_params, lookup_context, stable_ptr).ok()?;
        let substitution = GenericSubstitution::new(&generic_params, &generic_args);
        let mut rewriter = SubstitutionRewriter { db: self.db, substitution: &substitution };

        let fixed_param_ty = rewriter.rewrite(first_param.ty).ok()?;
        let (_, n_snapshots) = self.conform_ty_ex(self_ty, fixed_param_ty, true).ok()?;
        let generic_args = self.rewrite(generic_args).ok()?;

        Some((
            self.db.intern_concrete_trait(ConcreteTraitLongId { trait_id, generic_args }),
            n_snapshots,
        ))
    }

    /// Infers a generic argument to be passed as a generic paramter.
    /// Allocates a new inference variable of the correct kind, and wraps in a generic argument.
    pub fn infer_generic_arg(
        &mut self,
        param: &GenericParam,
        lookup_context: ImplLookupContext,
        stable_ptr: SyntaxStablePtrId,
    ) -> InferenceResult<GenericArgumentId> {
        match param {
            GenericParam::Type(_) => Ok(GenericArgumentId::Type(self.new_type_var(stable_ptr))),
            GenericParam::Impl(param) => Ok(GenericArgumentId::Impl(self.new_impl_var(
                param.concrete_trait?,
                stable_ptr,
                lookup_context,
            )?)),
            GenericParam::Const(_) => Err(InferenceError::ConstInferenceNotSupported),
        }
    }

    /// Infers the impl to be substituted instead of a trait for a given trait function,
    /// and the generic arguments to be passed to the function.
    /// Returns the resulting impl function.
    pub fn infer_trait_function(
        &mut self,
        concrete_trait_function: ConcreteTraitGenericFunctionId,
        lookup_context: &ImplLookupContext,
        stable_ptr: SyntaxStablePtrId,
    ) -> InferenceResult<FunctionId> {
        let generic_function =
            self.infer_trait_generic_function(concrete_trait_function, lookup_context, stable_ptr)?;
        self.infer_generic_function(generic_function, lookup_context, stable_ptr)
    }

    /// Infers generic arguments to be passed to a generic function.
    /// Returns the resulting specialized function.
    pub fn infer_generic_function(
        &mut self,
        generic_function: GenericFunctionId,
        lookup_context: &ImplLookupContext,
        stable_ptr: SyntaxStablePtrId,
    ) -> InferenceResult<FunctionId> {
        let generic_params = generic_function.generic_params(self.db)?;
        let generic_args = self.infer_generic_args(&generic_params, lookup_context, stable_ptr)?;
        Ok(self.db.intern_function(FunctionLongId {
            function: ConcreteFunction { generic_function, generic_args },
        }))
    }

    /// Infers the impl to be substituted instead of a trait for a given trait function.
    /// Returns the resulting impl generic function.
    pub fn infer_trait_generic_function(
        &mut self,
        trait_function: ConcreteTraitGenericFunctionId,
        lookup_context: &ImplLookupContext,
        stable_ptr: SyntaxStablePtrId,
    ) -> InferenceResult<GenericFunctionId> {
        let impl_id = self.new_impl_var(
            trait_function.concrete_trait_id(self.db),
            stable_ptr,
            lookup_context.clone(),
        )?;
        Ok(GenericFunctionId::Impl(ImplGenericFunctionId {
            impl_id,
            function: trait_function.function_id(self.db),
        }))
    }

    /// Relaxes the information about an [ImplVar]. Prunes the current candidate impls, and assigns
    /// if only a single candidate is left.
    fn relax_impl_var(&mut self, var: ImplVar) -> InferenceResult<ImplId> {
        // TODO(spapini): Beware of cycles.
        if let Some(res) = self.impl_assignment.get(&var.id) {
            return Ok(*res);
        }
        if self.impl_candidates[var.id].is_empty() {
            return Err(InferenceError::AlreadyReported);
        }
        let var_concrete_trait_id = self.rewrite(var.concrete_trait_id)?;
        for candidate in self.impl_candidates[var.id].clone() {
            let candidate_concrete_trait =
                self.db.impl_concrete_trait(candidate).map_err(InferenceError::Failed)?;
            let mut temp_inference = self.clone();
            if temp_inference
                .conform_traits(candidate_concrete_trait, var_concrete_trait_id)
                .is_err()
            {
                self.version += 1;
                self.impl_candidates[var.id].swap_remove(&candidate);
            }
        }
        match self.impl_candidates[var.id].len() {
            0 => Err(InferenceError::NoImplsFound { concrete_trait_id: var_concrete_trait_id }),
            1 => {
                let candidates = std::mem::take(&mut self.impl_candidates[var.id]);
                let candidate = candidates.into_iter().next().unwrap();
                let candidate_concrete_trait =
                    self.db.impl_concrete_trait(candidate).map_err(InferenceError::Failed)?;
                self.conform_traits(candidate_concrete_trait, var_concrete_trait_id)?;
                self.assign_impl(var, candidate)
            }
            _ => Ok(ImplId::ImplVar(var)),
        }
    }
}

impl<'a> HasDb<&'a dyn SemanticGroup> for Inference<'a> {
    fn get_db(&self) -> &'a dyn SemanticGroup {
        self.db
    }
}
add_basic_rewrites!(<'a>, Inference<'a>, InferenceError, @exclude TypeLongId ImplId);
add_expr_rewrites!(<'a>, Inference<'a>, InferenceError, @exclude);
impl<'a> SemanticRewriter<TypeLongId, InferenceError> for Inference<'a> {
    fn rewrite(&mut self, value: TypeLongId) -> Result<TypeLongId, InferenceError> {
        if let TypeLongId::Var(var) = value {
            if let Some(type_id) = self.type_assignment.get(&var.id) {
                return self.rewrite(self.db.lookup_intern_type(*type_id));
            }
        }
        value.default_rewrite(self)
    }
}
impl<'a> SemanticRewriter<ImplId, InferenceError> for Inference<'a> {
    fn rewrite(&mut self, value: ImplId) -> InferenceResult<ImplId> {
        if let ImplId::ImplVar(var) = value {
            // Relax the candidates.
            if let Some(impl_id) = self.impl_assignment.get(&var.id) {
                return self.rewrite(*impl_id);
            } else {
                self.relax_impl_var(var)?;
            }
        }
        value.default_rewrite(self)
    }
}
