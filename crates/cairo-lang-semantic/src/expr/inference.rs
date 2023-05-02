//! Bidirectional type inference.

use std::collections::HashMap;
use std::ops::{Deref, DerefMut};

use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::{
    ConstantId, EnumId, ExternFunctionId, ExternTypeId, FreeFunctionId, GenericParamId,
    ImplAliasId, ImplDefId, ImplFunctionId, LanguageElementId, LocalVarId, MemberId, ParamId,
    StructId, TraitFunctionId, TraitId, VarId, VariantId,
};
use cairo_lang_diagnostics::{skip_diagnostic, DiagnosticAdded, Maybe};
use cairo_lang_proc_macros::DebugWithDb;
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_utils::extract_matches;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use itertools::{zip_eq, Itertools};

use crate::corelib::{core_felt252_ty, get_core_trait};
use crate::db::SemanticGroup;
use crate::diagnostic::{SemanticDiagnosticKind, SemanticDiagnostics};
use crate::expr::objects::*;
use crate::expr::pattern::*;
use crate::items::constant::Constant;
use crate::items::functions::{
    ConcreteFunctionWithBody, ConcreteFunctionWithBodyId, GenericFunctionId,
    GenericFunctionWithBodyId, ImplGenericFunctionId, ImplGenericFunctionWithBodyId,
};
use crate::items::generics::{GenericParamConst, GenericParamImpl, GenericParamType};
use crate::items::imp::{
    find_possible_impls_at_context, ImplId, ImplLookupContext, UninferredImpl,
};
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
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
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
    Failed(DiagnosticAdded),
    Cycle { var: InferenceVar },
    TypeKindMismatch { ty0: TypeId, ty1: TypeId },
    ImplKindMismatch { impl0: ImplId, impl1: ImplId },
    GenericArgMismatch { garg0: GenericArgumentId, garg1: GenericArgumentId },
    TraitMismatch { trt0: TraitId, trt1: TraitId },
    ConstInferenceNotSupported,
    NoImplsFound { concrete_trait_id: ConcreteTraitId },
    MultipleImplsFound { concrete_trait_id: ConcreteTraitId, impls: Vec<UninferredImpl> },
    TypeNotInferred { ty: TypeId },
    WillNotInfer { concrete_trait_id: ConcreteTraitId },
    AlreadyReported,
}
impl InferenceError {
    pub fn format(&self, db: &(dyn SemanticGroup + 'static)) -> String {
        match self {
            InferenceError::Failed(_) => "Inference error occurred".into(),
            InferenceError::AlreadyReported => "Inference error occurred again".into(),
            InferenceError::Cycle { var: _ } => "Inference cycle detected".into(),
            InferenceError::TypeKindMismatch { ty0, ty1 } => {
                format!("Type mismatch: {:?} and {:?}", ty0.debug(db), ty1.debug(db))
            }
            InferenceError::ImplKindMismatch { impl0, impl1 } => {
                format!("Impl mismatch: {:?} and {:?}", impl0.debug(db), impl1.debug(db))
            }
            InferenceError::GenericArgMismatch { garg0, garg1 } => {
                format!("Generic arg mismatch: {:?} and {:?}", garg0.debug(db), garg1.debug(db))
            }
            InferenceError::TraitMismatch { trt0, trt1 } => {
                format!("Trait mismatch: {:?} and {:?}", trt0.debug(db), trt1.debug(db))
            }
            InferenceError::ConstInferenceNotSupported => {
                "Const generic inference not yet supported.".into()
            }
            InferenceError::NoImplsFound { concrete_trait_id } => {
                format!("Trait has no implementation in context: {:?}", concrete_trait_id.debug(db))
            }
            InferenceError::MultipleImplsFound { concrete_trait_id, impls } => {
                let impls_str =
                    impls.iter().map(|imp| format!("{:?}", imp.debug(db.upcast()))).join(", ");
                format!(
                    "Trait `{:?}` has multiple implementations, in: {impls_str}",
                    concrete_trait_id.debug(db)
                )
            }
            InferenceError::TypeNotInferred { ty } => {
                format!("Type annotations needed. Failed to infer {:?}", ty.debug(db))
            }
            InferenceError::WillNotInfer { concrete_trait_id } => format!(
                "Cannot infer trait {:?}. First generic argument must be known.",
                concrete_trait_id.debug(db)
            ),
        }
    }
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
            // TODO(spapini): Better save the DiagnosticAdded on the variable.
            InferenceError::AlreadyReported => skip_diagnostic(),
            _ => diagnostics.report_by_ptr(
                stable_ptr,
                SemanticDiagnosticKind::InternalInferenceError(self.clone()),
            ),
        }
    }
}

/// State of inference for an impl variable.
#[derive(Clone, Debug, PartialEq, Eq)]
enum ImplVarData {
    Inferring(ImplVarInferringData),
    Inferred(ImplId),
}

/// State of a non-inferred impl variable.
#[derive(Clone, Debug, PartialEq, Eq)]
struct ImplVarInferringData {
    lookup_context: ImplLookupContext,
    /// Impl Candidate list. None if inference has no started.
    candidates: Option<Vec<UninferredImpl>>,
}

/// State of inference.
#[derive(Clone, Debug, DebugWithDb, Default, PartialEq, Eq)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct InferenceData {
    /// Current inferred assignment for type variables.
    pub type_assignment: HashMap<usize, TypeId>,
    /// Current inference data for impl variables.
    impl_data: HashMap<usize, ImplVarData>,
    /// Type variables.
    pub type_var_offset: usize,
    pub type_vars: Vec<TypeVar>,
    /// Impl variables.
    pub impl_var_offset: usize,
    pub impl_vars: Vec<ImplVar>,
    pub impl_var_cache: OrderedHashMap<ConcreteTraitId, usize>,
    /// Current version of inference.
    pub version: usize,
    pub remaining_impls: OrderedHashSet<usize>,
}
impl InferenceData {
    pub fn new() -> Self {
        Self::default()
    }
}

/// A stack of inference states, allowing transactional interface using temporary() and rollback().
#[derive(Clone, Debug, DebugWithDb, PartialEq, Eq)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct InferenceStack(Vec<InferenceData>);
impl InferenceStack {
    pub fn new() -> Self {
        Self(vec![InferenceData::default()])
    }
    /// Last inferece table in the stack.
    fn last(&self) -> &InferenceData {
        self.0.last().unwrap()
    }
    /// Last inferece table in the stack.
    fn last_mut(&mut self) -> &mut InferenceData {
        self.0.last_mut().unwrap()
    }
    pub fn version(&self) -> usize {
        self.last().version
    }
    /// Starts a temporary transaction - allowing for rolling back the inference.
    pub fn temporary(&mut self) {
        let last = self.last();
        let data = InferenceData {
            type_assignment: Default::default(),
            impl_data: Default::default(),
            type_var_offset: last.type_var_offset + last.type_vars.len(),
            type_vars: Default::default(),
            impl_var_offset: last.impl_var_offset + last.impl_vars.len(),
            impl_vars: Default::default(),
            impl_var_cache: Default::default(),
            version: last.version,
            remaining_impls: last.remaining_impls.clone(),
        };
        self.0.push(data)
    }
    /// Rolls back inference state from last temporary() call.
    pub fn rollback(&mut self) {
        self.0.pop().unwrap();
    }
    /// Gets a contextual inference object - With database.
    pub fn inference<'db, 'b: 'db>(&'db mut self, db: &'b dyn SemanticGroup) -> Inference<'db> {
        Inference { db, data: self }
    }
    pub fn type_vars_len(&self) -> usize {
        let current = self.last();
        current.type_var_offset + current.type_vars.len()
    }
    pub fn impl_vars_len(&self) -> usize {
        let current = self.last();
        current.impl_var_offset + current.impl_vars.len()
    }
    fn raw_new_type_var(&mut self, stable_ptr: SyntaxStablePtrId) -> TypeVar {
        let var = TypeVar { id: self.type_vars_len(), stable_ptr };
        let current = self.last_mut();
        current.type_vars.push(var);
        current.version += 1;
        var
    }
    fn raw_new_impl_var(
        &mut self,
        concrete_trait_id: ConcreteTraitId,
        stable_ptr: SyntaxStablePtrId,
        lookup_context: ImplLookupContext,
    ) -> ImplVar {
        if let Some(var) = self.find_impl_var_for(concrete_trait_id) {
            return var;
        }
        let id = self.impl_vars_len();
        let current = self.last_mut();
        current.remaining_impls.insert(id);
        current.impl_data.insert(
            id,
            ImplVarData::Inferring(ImplVarInferringData { lookup_context, candidates: None }),
        );

        let var = ImplVar { id, concrete_trait_id, stable_ptr };
        current.impl_vars.push(var);
        current.impl_var_cache.insert(concrete_trait_id, id);
        current.version += 1;
        var
    }
    fn type_var(&self, id: usize) -> &TypeVar {
        for data in self.0.iter().rev() {
            if id >= data.type_var_offset {
                return &data.type_vars[id - data.type_var_offset];
            }
        }
        unreachable!()
    }
    fn impl_var(&self, id: usize) -> &ImplVar {
        for data in self.0.iter().rev() {
            if id >= data.impl_var_offset {
                return &data.impl_vars[id - data.impl_var_offset];
            }
        }
        unreachable!()
    }
    pub fn type_assignment(&self, id: usize) -> Option<TypeId> {
        for data in self.0.iter().rev() {
            if let Some(res) = data.type_assignment.get(&id) {
                return Some(*res);
            }
        }
        None
    }
    fn impl_var_data(&self, id: usize) -> &ImplVarData {
        for data in self.0.iter().rev() {
            if let Some(res) = data.impl_data.get(&id) {
                return res;
            }
        }
        unreachable!()
    }
    fn assign_type_var(&mut self, id: usize, ty: TypeId) {
        let current = self.last_mut();
        if current.type_assignment.insert(id, ty) == Some(ty) {
            return;
        }
        current.version += 1;
    }
    fn update_impl(&mut self, id: usize, data: ImplVarData) {
        let current = self.last_mut();
        match &data {
            ImplVarData::Inferred(_) => {
                current.remaining_impls.swap_remove(&id);
            }
            ImplVarData::Inferring(ImplVarInferringData {
                candidates: Some(candidates), ..
            }) => {
                if candidates.is_empty() {
                    current.remaining_impls.swap_remove(&id);
                }
            }
            _ => {}
        }
        match current.impl_data.entry(id) {
            std::collections::hash_map::Entry::Occupied(mut entry) => {
                if entry.get() == &data {
                    return;
                }
                entry.insert(data);
            }
            std::collections::hash_map::Entry::Vacant(entry) => {
                entry.insert(data);
            }
        }
        current.version += 1;
    }

    fn find_impl_var_for(&self, concrete_trait_id: ConcreteTraitId) -> Option<ImplVar> {
        for data in self.0.iter().rev() {
            if let Some(id) = data.impl_var_cache.get(&concrete_trait_id) {
                return Some(data.impl_vars[*id]);
            }
        }
        None
    }
}
impl Default for InferenceStack {
    fn default() -> Self {
        Self::new()
    }
}

/// State of inference.
pub struct Inference<'db> {
    db: &'db dyn SemanticGroup,
    pub data: &'db mut InferenceStack,
}

impl Deref for Inference<'_> {
    type Target = InferenceStack;

    fn deref(&self) -> &Self::Target {
        self.data
    }
}
impl DerefMut for Inference<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.data
    }
}

impl<'db> std::fmt::Debug for Inference<'db> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let x = self.data.debug(self.db.elongate());
        write!(f, "{x:?}")
    }
}

impl<'db> Inference<'db> {
    /// Creates a new [Inference] instance with the given [InferenceData].
    pub fn with_data(db: &'db dyn SemanticGroup, data: &'db mut InferenceStack) -> Self {
        Self { db, data }
    }

    /// Allocates a new [TypeVar] for an unknown type that needs to be inferred,
    pub fn new_type_var(&mut self, stable_ptr: SyntaxStablePtrId) -> TypeId {
        self.db.intern_type(TypeLongId::Var(self.data.raw_new_type_var(stable_ptr)))
    }

    /// Allocates a new [ImplVar] for an unknown type that needs to be inferred,
    pub fn new_impl_var(
        &mut self,
        concrete_trait_id: ConcreteTraitId,
        stable_ptr: SyntaxStablePtrId,
        lookup_context: ImplLookupContext,
    ) -> InferenceResult<ImplId> {
        let concrete_trait_id = self.rewrite(concrete_trait_id)?;
        let var = self.data.raw_new_impl_var(concrete_trait_id, stable_ptr, lookup_context);
        self.relax_impl_var(var)
    }

    /// Returns the candidates for the given [ImplVar].
    pub fn has_candidates(&self, var: &ImplVar) -> bool {
        match self.impl_var_data(var.id) {
            ImplVarData::Inferring(data) => {
                data.candidates.as_ref().map_or(false, |candidates| !candidates.is_empty())
            }
            ImplVarData::Inferred(_) => true,
        }
    }

    /// Relaxes all the constraints until stable.
    /// Retrieves the first variable that is still not inferred, or None, if everything is
    /// inferred.
    pub fn finalize(&mut self) -> Option<(SyntaxStablePtrId, InferenceError)> {
        // TODO(spapini): Remove the iterative logic in favor of event listeners.
        let numeric_trait_id = get_core_trait(self.db, "NumericLiteral".into());
        let felt_ty = core_felt252_ty(self.db);
        loop {
            let version = self.version();
            for id in self.data.last().remaining_impls.clone() {
                let var = *self.impl_var(id);
                if let Err(err) = self.relax_impl_var(var) {
                    return Some((var.stable_ptr, err));
                }
            }
            // If nothing has changed, try to relax numeric literals.
            // TODO(spapini): Think of a way to generalize this.
            if version != self.version() {
                continue;
            }
            for id in 0..self.impl_vars_len() {
                let var = *self.impl_var(id);
                if matches!(self.impl_var_data(id), ImplVarData::Inferred(_)) {
                    continue;
                }
                if var.concrete_trait_id.trait_id(self.db) != numeric_trait_id {
                    continue;
                }
                // Uninferred numeric trait. Resolve as felt252.
                let ty = extract_matches!(
                    var.concrete_trait_id.generic_args(self.db)[0],
                    GenericArgumentId::Type
                );
                if let Err(err) = self.conform_ty(ty, felt_ty) {
                    return Some((var.stable_ptr, err));
                }
                break;
            }
            if version == self.version() {
                return self.first_undetermined_variable();
            }
        }
    }

    /// Retrieves the first variable that is still not inferred, or None, if everything is
    /// inferred.
    fn first_undetermined_variable(&mut self) -> Option<(SyntaxStablePtrId, InferenceError)> {
        for id in 0..self.type_vars_len() {
            let var = self.type_var(id);
            if self.type_assignment(id).is_none() {
                let ty = self.db.intern_type(TypeLongId::Var(*var));
                return Some((var.stable_ptr, InferenceError::TypeNotInferred { ty }));
            }
        }
        for id in 0..self.impl_vars_len() {
            let var = *self.impl_var(id);
            if let Err(err) = self.relax_impl_var(var) {
                return Some((var.stable_ptr, err));
            }
            if let ImplVarData::Inferring(data) = self.impl_var_data(id) {
                let data = data.clone();
                let Some(candidates) = data.candidates else
                {
                    let concrete_trait_id = self.impl_var(id).concrete_trait_id;
                    let concrete_trait_id = self
                        .rewrite(concrete_trait_id)
                        .unwrap_or(concrete_trait_id);
                    return Some((var.stable_ptr, InferenceError::WillNotInfer{concrete_trait_id}));
                };
                if candidates.is_empty() {
                    return Some((var.stable_ptr, InferenceError::AlreadyReported));
                }
                let impls = candidates;
                let impls = impls
                    .into_iter()
                    .map(|impl_id| self.rewrite(impl_id).unwrap_or(impl_id))
                    .collect();

                // TODO(spapini): Deduplicate impl aliases.
                return Some((
                    var.stable_ptr,
                    InferenceError::MultipleImplsFound {
                        concrete_trait_id: var.concrete_trait_id,
                        impls,
                    },
                ));
            }
        }
        None
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
        let ty0 = self.reduce_ty_head(ty0);
        let ty1 = self.reduce_ty_head(ty1);
        if ty0 == self.db.never_ty() {
            return Ok((ty1, 0));
        }
        if ty0 == ty1 {
            return Ok((ty0, 0));
        }
        let long_ty1 = self.db.lookup_intern_type(ty1);
        match long_ty1 {
            TypeLongId::Var(var) => return Ok((self.assign_ty(var, ty0)?, 0)),
            TypeLongId::Missing(_) => return Ok((ty1, 0)),
            TypeLongId::Snapshot(inner_ty) => {
                if ty0_is_self {
                    if inner_ty == ty0 {
                        return Ok((ty1, 1));
                    }
                    if !matches!(self.db.lookup_intern_type(ty0), TypeLongId::Snapshot(_)) {
                        if let TypeLongId::Var(var) = self.db.lookup_intern_type(inner_ty) {
                            return Ok((self.assign_ty(var, ty0)?, 1));
                        }
                    }
                }
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
        if let ImplVarData::Inferred(other_impl) = self.impl_var_data(var.id) {
            return self.conform_impl(impl_id, *other_impl);
        }
        if self.impl_contains_var(&impl_id, InferenceVar::Impl(var.id))? {
            return Err(InferenceError::Cycle { var: InferenceVar::Impl(var.id) });
        }
        // TODO(spapini): Revisit these.
        let impl_id = self.rewrite(impl_id)?;
        self.update_impl(var.id, ImplVarData::Inferred(impl_id));
        Ok(impl_id)
    }

    /// Conforms an impl. See `conform_ty()`.
    pub fn conform_impl(&mut self, impl0: ImplId, impl1: ImplId) -> InferenceResult<ImplId> {
        let impl0 = self.reduce_impl_head(impl0);
        let impl1 = self.reduce_impl_head(impl1);
        if impl0 == impl1 {
            return Ok(impl0);
        }
        if let ImplId::ImplVar(var) = impl1 {
            self.conform_traits(var.concrete_trait_id, self.db.impl_concrete_trait(impl0)?)?;
            return self.assign_impl(var, impl0);
        }
        match impl0 {
            ImplId::ImplVar(var) => {
                self.conform_traits(var.concrete_trait_id, self.db.impl_concrete_trait(impl1)?)?;
                self.assign_impl(var, impl1)
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
        if let Some(assignment) = self.type_assignment(var.id) {
            return self.conform_ty(ty, assignment);
        }
        let inference_var = InferenceVar::Type(var.id);
        if self.ty_contains_var(ty, inference_var)? {
            return Err(InferenceError::Cycle { var: inference_var });
        }
        self.data.assign_type_var(var.id, ty);
        Ok(ty)
    }

    /// Checks if a type tree contains a certain [InferenceVar] somewhere. Used to avoid inference
    /// cycles.
    pub fn ty_contains_var(&mut self, ty: TypeId, var: InferenceVar) -> InferenceResult<bool> {
        Ok(match self.db.lookup_intern_type(ty) {
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
                if let Some(ty) = self.type_assignment(new_var.id) {
                    return self.ty_contains_var(ty, var);
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
                if let ImplVarData::Inferred(impl_id) = self.impl_var_data(new_var.id) {
                    let impl_id = *impl_id;
                    return self.impl_contains_var(&impl_id, var);
                }
                false
            }
        })
    }

    /// Determines if an assignment to `generic_params` can be chosen s.t. `generic_args` will be
    /// substituted to `expected_generic_args`.
    // TODO(spapini): Fail gracefully on infinite loops.
    pub fn can_infer_generics(
        &mut self,
        generic_params: &[GenericParam],
        generic_args: &[GenericArgumentId],
        expected_generic_args: &[GenericArgumentId],
        lookup_context: &ImplLookupContext,
        stable_ptr: SyntaxStablePtrId,
    ) -> bool {
        if generic_args.len() != expected_generic_args.len() {
            return false;
        }
        self.temporary();
        let res = self.infer_generic_assignment(
            generic_params,
            generic_args,
            expected_generic_args,
            lookup_context,
            stable_ptr,
        );
        let final_res = self.finalize();
        self.rollback();
        res.is_ok() && final_res.is_none()
    }

    /// Infers all the variables required to make an uninferred impl provide a concrete trait.
    fn infer_impl(
        &mut self,
        uninferred_impl: UninferredImpl,
        concrete_trait_id: ConcreteTraitId,
        lookup_context: &ImplLookupContext,
        stable_ptr: SyntaxStablePtrId,
    ) -> InferenceResult<ImplId> {
        let impl_id = match uninferred_impl {
            UninferredImpl::Def(impl_def_id) => {
                self.infer_impl_def(impl_def_id, concrete_trait_id, lookup_context, stable_ptr)?
            }
            UninferredImpl::ImplAlias(impl_alias_id) => {
                self.infer_impl_alias(impl_alias_id, concrete_trait_id, lookup_context, stable_ptr)?
            }
            UninferredImpl::GenericParam(param_id) => {
                let param =
                    self.db.generic_param_semantic(param_id).map_err(InferenceError::Failed)?;
                let param = extract_matches!(param, GenericParam::Impl);
                let imp_concrete_trait_id = param.concrete_trait.unwrap();
                self.conform_traits(concrete_trait_id, imp_concrete_trait_id)?;
                ImplId::GenericParameter(param_id)
            }
        };
        Ok(impl_id)
    }

    /// Check if it possible to infer an impl to provide a concrete trait. See infer_impl.
    pub fn can_infer_impl(
        &mut self,
        uninferred_impl: UninferredImpl,
        concrete_trait_id: ConcreteTraitId,
        lookup_context: &ImplLookupContext,
        stable_ptr: SyntaxStablePtrId,
    ) -> Maybe<bool> {
        self.temporary();
        let res = self.infer_impl(uninferred_impl, concrete_trait_id, lookup_context, stable_ptr);
        self.rollback();
        match res {
            Ok(_) => Ok(true),
            Err(InferenceError::Failed(diag_added)) => Err(diag_added),
            Err(_) => Ok(false),
        }
    }

    /// Infers all the variables required to make an impl (possibly with free generic params)
    /// provide a concrete trait.
    pub fn infer_impl_def(
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
        let generic_args = self.infer_generic_assignment(
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

    /// Infers all the variables required to make an impl alias (possibly with free generic params)
    /// provide a concrete trait.
    pub fn infer_impl_alias(
        &mut self,
        impl_alias_id: ImplAliasId,
        concrete_trait_id: ConcreteTraitId,
        lookup_context: &ImplLookupContext,
        stable_ptr: SyntaxStablePtrId,
    ) -> Result<ImplId, InferenceError> {
        let impl_alias_generic_params = self.db.impl_alias_generic_params(impl_alias_id)?;
        let impl_id = self.db.impl_alias_resolved_impl(impl_alias_id)?;
        let imp_concrete_trait = impl_id.concrete_trait(self.db)?;
        if imp_concrete_trait.trait_id(self.db) != concrete_trait_id.trait_id(self.db) {
            return Err(InferenceError::TraitMismatch {
                trt0: imp_concrete_trait.trait_id(self.db),
                trt1: concrete_trait_id.trait_id(self.db),
            });
        }

        let long_concrete_trait = self.db.lookup_intern_concrete_trait(concrete_trait_id);
        let long_imp_concrete_trait = self.db.lookup_intern_concrete_trait(imp_concrete_trait);
        let generic_args = self.infer_generic_assignment(
            &impl_alias_generic_params,
            &long_imp_concrete_trait.generic_args,
            &long_concrete_trait.generic_args,
            lookup_context,
            stable_ptr,
        )?;

        Ok(SubstitutionRewriter {
            db: self.db,
            substitution: &GenericSubstitution::new(&impl_alias_generic_params, &generic_args),
        }
        .rewrite(impl_id)?)
    }

    /// Chooses and assignment to generic_params s.t. generic_args will be substituted to
    /// expected_generic_args.
    /// Returns the generic_params assignment.
    pub fn infer_generic_assignment(
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
        // Relax all introduced impls.
        for garg in &new_generic_args {
            if let GenericArgumentId::Impl(ImplId::ImplVar(var)) = garg {
                self.relax_impl_var(*var)?;
            }
        }
        Ok(new_generic_args)
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

    /// Enriches the given [ImplLookupContext] with the information from the given
    /// [ConcreteTraitId].
    fn enrich_lookup_context(
        &mut self,
        concrete_trait_id: ConcreteTraitId,
        lookup_context: &mut ImplLookupContext,
        generic_args: &[GenericArgumentId],
    ) {
        lookup_context
            .extra_modules
            .push(concrete_trait_id.trait_id(self.db).module_file_id(self.db.upcast()).0);
        for generic_arg in generic_args {
            if let GenericArgumentId::Type(ty) = generic_arg {
                let ty = self.reduce_ty_head(*ty);
                if let TypeLongId::Concrete(concrete) = self.db.lookup_intern_type(ty) {
                    lookup_context
                        .extra_modules
                        .push(concrete.generic_type(self.db).module_file_id(self.db.upcast()).0);
                }
            }
        }
    }

    /// Reduces a type if it's an already assigned variable.
    pub fn reduce_ty_head(&mut self, mut ty: TypeId) -> TypeId {
        // TODO(spapini): Propagate error to diagnostics.
        let mut long_type_id = self.db.lookup_intern_type(ty);
        loop {
            let TypeLongId::Var(var) = long_type_id else { break; };
            let Some(cur_ty) = self.type_assignment(var.id) else { break; };
            ty = cur_ty;
            long_type_id = self.db.lookup_intern_type(ty);
        }
        ty
    }

    /// Reduces an impl if it's an already assigned variable.
    pub fn reduce_impl_head(&mut self, mut impl_id: ImplId) -> ImplId {
        loop {
            let ImplId::ImplVar(var) = impl_id else { break; };
            let ImplVarData::Inferred(var_impl) = self.impl_var_data(var.id) else { break; };
            impl_id = *var_impl;
        }
        impl_id
    }

    // Resumes inference for an impl var.
    pub fn try_to_resume_impl_var(
        &mut self,
        var: ImplVar,
        concrete_trait_id: ConcreteTraitId,
    ) -> InferenceResult<()> {
        let ImplVarData::Inferring(
            ImplVarInferringData { candidates: None, lookup_context }
        ) = self.impl_var_data(var.id) else {
            return Ok(());
        };
        let mut lookup_context = lookup_context.clone();
        let generic_args = concrete_trait_id.generic_args(self.db);
        // Don't try to resolve impls if the first generic param is a variable.
        match generic_args.get(0) {
            Some(GenericArgumentId::Type(ty)) => {
                let ty = self.reduce_ty_head(*ty);
                if let TypeLongId::Var(_) = self.db.lookup_intern_type(ty) {
                    // Don't try to infer such impls.
                    return Ok(());
                }
            }
            Some(GenericArgumentId::Impl(impl_id)) => {
                let impl_id = self.reduce_impl_head(*impl_id);
                if let ImplId::ImplVar(_) = impl_id {
                    // Don't try to infer such impls.
                    return Ok(());
                }
            }
            _ => {}
        };

        // TODO(spapini): Only reduce a little bit.
        let concrete_trait_id = self.rewrite(concrete_trait_id)?;
        self.enrich_lookup_context(concrete_trait_id, &mut lookup_context, &generic_args);
        let candidates = find_possible_impls_at_context(
            self.db,
            self,
            &lookup_context,
            concrete_trait_id,
            var.stable_ptr,
        )
        .map_err(InferenceError::Failed)?
        .into_iter()
        .collect_vec();
        let are_candidates_empty = candidates.is_empty();
        self.update_impl(
            var.id,
            ImplVarData::Inferring(ImplVarInferringData {
                lookup_context,
                candidates: Some(candidates),
            }),
        );
        if are_candidates_empty {
            let concrete_trait_id = self.rewrite(concrete_trait_id)?;
            return Err(InferenceError::NoImplsFound { concrete_trait_id });
        }
        Ok(())
    }

    pub fn impl_var_has_no_candidates(&self, var: ImplVar) -> bool {
        match self.impl_var_data(var.id) {
            ImplVarData::Inferring(ImplVarInferringData {
                candidates: Some(candidates), ..
            }) => candidates.is_empty(),
            _ => false,
        }
    }

    /// Relaxes the information about an [ImplVar]. Prunes the current candidate impls, and assigns
    /// if only a single candidate is left.
    fn relax_impl_var(&mut self, var: ImplVar) -> InferenceResult<ImplId> {
        // TODO(spapini): Beware of cycles.
        if let ImplVarData::Inferred(res) = self.impl_var_data(var.id) {
            return Ok(*res);
        }
        let var_concrete_trait_id = var.concrete_trait_id;
        self.try_to_resume_impl_var(var, var_concrete_trait_id)?;
        let ImplVarData::Inferring(
            ImplVarInferringData { lookup_context, candidates: Some(candidates), .. }
        ) = self.impl_var_data(var.id) else {
            return Ok(ImplId::ImplVar(var));
        };
        if candidates.is_empty() {
            return Err(InferenceError::AlreadyReported);
        }
        let lookup_context = lookup_context.clone();
        let mut candidates = candidates.clone();
        for i in (0..candidates.len()).rev() {
            let can_infer = self.can_infer_impl(
                candidates[i],
                var_concrete_trait_id,
                &lookup_context,
                var.stable_ptr,
            )?;
            if !can_infer {
                candidates.swap_remove(i);
            }
        }

        match candidates[..] {
            [] => {
                self.update_impl(
                    var.id,
                    ImplVarData::Inferring(ImplVarInferringData {
                        lookup_context,
                        candidates: Some(candidates),
                    }),
                );
                Err(InferenceError::NoImplsFound { concrete_trait_id: var_concrete_trait_id })
            }
            [candidate] => {
                let impl_id = self.infer_impl(
                    candidate,
                    var_concrete_trait_id,
                    &lookup_context,
                    var.stable_ptr,
                )?;

                self.assign_impl(var, impl_id)
            }
            _ => {
                self.update_impl(
                    var.id,
                    ImplVarData::Inferring(ImplVarInferringData {
                        lookup_context,
                        candidates: Some(candidates),
                    }),
                );
                Ok(ImplId::ImplVar(var))
            }
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
            if let Some(type_id) = self.type_assignment(var.id) {
                return self.rewrite(self.db.lookup_intern_type(type_id));
            }
        }
        value.default_rewrite(self)
    }
}
impl<'a> SemanticRewriter<ImplId, InferenceError> for Inference<'a> {
    fn rewrite(&mut self, value: ImplId) -> InferenceResult<ImplId> {
        if let ImplId::ImplVar(var) = value {
            // Relax the candidates.
            if let ImplVarData::Inferred(impl_id) = self.impl_var_data(var.id) {
                return self.rewrite(*impl_id);
            } else {
                self.relax_impl_var(var)?;
            }
        }
        value.default_rewrite(self)
    }
}
