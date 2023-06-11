//! Bidirectional type inference.

use std::collections::{HashMap, VecDeque};
use std::hash::Hash;
use std::ops::{Deref, DerefMut};

use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::{
    ConstantId, EnumId, ExternFunctionId, ExternTypeId, FreeFunctionId, GenericParamId,
    ImplAliasId, ImplDefId, ImplFunctionId, LanguageElementId, LocalVarId, MemberId, ParamId,
    StructId, TraitFunctionId, TraitId, VarId, VariantId,
};
use cairo_lang_diagnostics::DiagnosticAdded;
use cairo_lang_proc_macros::{DebugWithDb, SemanticObject};
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use cairo_lang_utils::{define_short_id, extract_matches};

use self::canonic::{CanonicalImpl, CanonicalMapping, CanonicalTrait, NoError};
use self::solver::{enrich_lookup_context, Ambiguity, SolutionSet};
use crate::corelib::{core_felt252_ty, get_core_trait};
use crate::db::SemanticGroup;
use crate::diagnostic::{SemanticDiagnosticKind, SemanticDiagnostics};
use crate::expr::inference::canonic::ResultNoErrEx;
use crate::expr::inference::conform::InferenceConform;
use crate::expr::objects::*;
use crate::expr::pattern::*;
use crate::items::constant::Constant;
use crate::items::functions::{
    ConcreteFunctionWithBody, ConcreteFunctionWithBodyId, GenericFunctionId,
    GenericFunctionWithBodyId, ImplGenericFunctionId, ImplGenericFunctionWithBodyId,
};
use crate::items::generics::{GenericParamConst, GenericParamImpl, GenericParamType};
use crate::items::imp::{ImplId, ImplLookupContext, UninferredImpl};
use crate::items::trt::{ConcreteTraitGenericFunctionId, ConcreteTraitGenericFunctionLongId};
use crate::literals::LiteralId;
use crate::substitution::{HasDb, SemanticRewriter};
use crate::types::{ConcreteEnumLongId, ConcreteExternTypeLongId, ConcreteStructLongId};
use crate::{
    add_basic_rewrites, add_expr_rewrites, add_rewrite, semantic_object_for_id, ConcreteEnumId,
    ConcreteExternTypeId, ConcreteFunction, ConcreteImplId, ConcreteImplLongId, ConcreteStructId,
    ConcreteTraitId, ConcreteTraitLongId, ConcreteTypeId, ConcreteVariant, ExprLiteral, FunctionId,
    FunctionLongId, GenericArgumentId, GenericParam, LocalVariable, Member, Parameter, Pattern,
    SemanticObject, Signature, TypeId, TypeLongId,
};

pub mod canonic;
pub mod conform;
pub mod infers;
pub mod solver;

/// A type variable, created when a generic type argument is not passed, and thus is not known
/// yet and needs to be inferred.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct TypeVar {
    pub id: LocalTypeVarId,
}

/// An impl variable, created when a generic type argument is not passed, and thus is not known
/// yet and needs to be inferred.
#[derive(Clone, Debug, PartialEq, Eq, Hash, DebugWithDb, SemanticObject)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct ImplVar {
    #[dont_rewrite]
    pub id: LocalImplVarId,
    pub concrete_trait_id: ConcreteTraitId,
    #[dont_rewrite]
    pub lookup_context: ImplLookupContext,
}
impl ImplVar {
    pub fn intern(&self, db: &dyn SemanticGroup) -> ImplVarId {
        db.intern_impl_var(self.clone())
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, SemanticObject)]
pub struct LocalTypeVarId(pub usize);
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, SemanticObject)]
pub struct LocalImplVarId(pub usize);

define_short_id!(ImplVarId, ImplVar, SemanticGroup, lookup_intern_impl_var);
impl ImplVarId {
    pub fn get(&self, db: &dyn SemanticGroup) -> ImplVar {
        db.lookup_intern_impl_var(*self)
    }
    pub fn id(&self, db: &dyn SemanticGroup) -> LocalImplVarId {
        self.get(db).id
    }
    pub fn concrete_trait_id(&self, db: &dyn SemanticGroup) -> ConcreteTraitId {
        self.get(db).concrete_trait_id
    }
    pub fn lookup_context(&self, db: &dyn SemanticGroup) -> ImplLookupContext {
        self.get(db).lookup_context
    }
}
semantic_object_for_id!(ImplVarId, lookup_intern_impl_var, intern_impl_var, ImplVar);

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub enum InferenceVar {
    Type(LocalTypeVarId),
    Impl(LocalImplVarId),
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

    // TODO(spapini): These are only used for external interface. Separate them along with the
    // finalize() function to a wrapper.
    NoImplsFound { concrete_trait_id: ConcreteTraitId },
    Ambiguity(Ambiguity),
    TypeNotInferred { ty: TypeId },
}
impl InferenceError {
    pub fn format(&self, db: &(dyn SemanticGroup + 'static)) -> String {
        match self {
            InferenceError::Failed(_) => "Inference error occurred".into(),
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
            InferenceError::Ambiguity(ambiguity) => ambiguity.format(db),
            InferenceError::TypeNotInferred { ty } => {
                format!("Type annotations needed. Failed to infer {:?}", ty.debug(db))
            }
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
            _ => diagnostics.report_by_ptr(
                stable_ptr,
                SemanticDiagnosticKind::InternalInferenceError(self.clone()),
            ),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct ImplVarData {
    candidates: Option<OrderedHashSet<UninferredImpl>>,
}

/// State of inference.
#[derive(Clone, Debug, DebugWithDb, Default, PartialEq, Eq)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct InferenceData {
    /// Current inferred assignment for type variables.
    pub type_assignment: HashMap<LocalTypeVarId, TypeId>,
    /// Current inferred assignment for impl variables.
    pub impl_assignment: HashMap<LocalImplVarId, ImplId>,
    /// Type variables.
    pub type_vars: Vec<TypeVar>,
    /// Impl variables.
    pub impl_vars: Vec<ImplVar>,
    /// Mapping from variables to stable pointers, if exist.
    pub stable_ptrs: HashMap<InferenceVar, SyntaxStablePtrId>,

    /// Inference variables that are pending to be solved.
    pending: VecDeque<LocalImplVarId>,
    /// Inference variables that have been refuted - no solutions exist.
    refuted: Vec<LocalImplVarId>,
    /// Inference variables that have been solved.
    solved: Vec<LocalImplVarId>,
    /// Inference variables that are currently ambiguous. May be solved later.
    ambiguous: Vec<(LocalImplVarId, Ambiguity)>,
}
impl InferenceData {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn inference<'db, 'b: 'db>(&'db mut self, db: &'b dyn SemanticGroup) -> Inference<'db> {
        Inference { db, data: self }
    }
}

/// State of inference. A system of inference constraints.
pub struct Inference<'db> {
    db: &'db dyn SemanticGroup,
    pub data: &'db mut InferenceData,
}

impl Deref for Inference<'_> {
    type Target = InferenceData;

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
    pub fn with_data(db: &'db dyn SemanticGroup, data: &'db mut InferenceData) -> Self {
        Self { db, data }
    }

    pub fn clone_data(&self) -> InferenceData {
        self.data.clone()
    }

    /// Getter for an [ImplVar].
    fn impl_var(&self, var_id: LocalImplVarId) -> &ImplVar {
        &self.impl_vars[var_id.0]
    }
    /// Getter for an impl var assignment.
    pub fn impl_assignment(&self, var_id: LocalImplVarId) -> Option<ImplId> {
        self.impl_assignment.get(&var_id).copied()
    }
    // Getter for a type var assignment.
    fn type_assignment(&self, var_id: LocalTypeVarId) -> Option<TypeId> {
        self.type_assignment.get(&var_id).copied()
    }

    /// Allocates a new [TypeVar] for an unknown type that needs to be inferred.
    /// Returns a wrapping TypeId.
    pub fn new_type_var(&mut self, stable_ptr: Option<SyntaxStablePtrId>) -> TypeId {
        let var = self.new_type_var_raw(stable_ptr);
        self.db.intern_type(TypeLongId::Var(var))
    }

    /// Allocates a new [TypeVar] for an unknown type that needs to be inferred.
    /// Returns the variable id.
    pub fn new_type_var_raw(&mut self, stable_ptr: Option<SyntaxStablePtrId>) -> TypeVar {
        let var = TypeVar { id: LocalTypeVarId(self.type_vars.len()) };
        if let Some(stable_ptr) = stable_ptr {
            self.stable_ptrs.insert(InferenceVar::Type(var.id), stable_ptr);
        }
        self.type_vars.push(var);
        var
    }

    /// Allocates a new [ImplVar] for an unknown type that needs to be inferred.
    /// Returns a wrapping ImplId.
    pub fn new_impl_var(
        &mut self,
        concrete_trait_id: ConcreteTraitId,
        stable_ptr: Option<SyntaxStablePtrId>,
        mut lookup_context: ImplLookupContext,
    ) -> InferenceResult<ImplId> {
        enrich_lookup_context(self.db, concrete_trait_id, &mut lookup_context);
        let var = self.new_impl_var_raw(lookup_context, concrete_trait_id, stable_ptr);
        Ok(ImplId::ImplVar(self.impl_var(var).intern(self.db)))
    }

    /// Allocates a new [ImplVar] for an unknown type that needs to be inferred.
    /// Returns the variable id.
    fn new_impl_var_raw(
        &mut self,
        lookup_context: ImplLookupContext,
        concrete_trait_id: ConcreteTraitId,
        stable_ptr: Option<SyntaxStablePtrId>,
    ) -> LocalImplVarId {
        let mut lookup_context = lookup_context;
        lookup_context
            .insert_module(concrete_trait_id.trait_id(self.db).module_file_id(self.db.upcast()).0);

        let id = LocalImplVarId(self.impl_vars.len());
        if let Some(stable_ptr) = stable_ptr {
            self.stable_ptrs.insert(InferenceVar::Impl(id), stable_ptr);
        }
        let var = ImplVar { id, concrete_trait_id, lookup_context };
        self.impl_vars.push(var);
        self.pending.push_back(id);
        id
    }

    /// Solves the inference system. After a successful solve, there are no more pending impl
    /// inferences.
    pub fn solve(&mut self) -> InferenceResult<()> {
        let mut ambiguous = std::mem::take(&mut self.ambiguous);
        self.pending.extend(ambiguous.drain(..).map(|(var, _)| var));
        while let Some(var) = self.pending.pop_front() {
            let solution = match self.impl_var_solution_set(var)? {
                SolutionSet::None => {
                    self.refuted.push(var);
                    continue;
                }
                SolutionSet::Ambiguous(ambiguity) => {
                    self.ambiguous.push((var, ambiguity));
                    continue;
                }
                SolutionSet::Unique(solution) => solution,
            };

            // Solution found. Assign it.
            self.assign_impl(var, solution).unwrap();

            // Something changed.
            self.solved.push(var);
            let mut ambiguous = std::mem::take(&mut self.ambiguous);
            self.pending.extend(ambiguous.drain(..).map(|(var, _)| var));
        }
        Ok(())
    }

    /// Returns the solution set status for the inference:
    /// Whether there is a unique solution, multiple solutions, no solutions or an error.
    pub fn solution_set(&mut self) -> InferenceResult<SolutionSet<()>> {
        self.solve()?;
        if !self.refuted.is_empty() {
            return Ok(SolutionSet::None);
        }
        if let Some((_, ambiguity)) = self.ambiguous.first() {
            return Ok(SolutionSet::Ambiguous(ambiguity.clone()));
        }
        assert!(self.pending.is_empty(), "solution() called on an unsolved solver");
        Ok(SolutionSet::Unique(()))
    }

    /// Finalizes an inference by inferring uninferred numeric literals as felt252.
    pub fn finalize(&mut self) -> Option<(Option<SyntaxStablePtrId>, InferenceError)> {
        let numeric_trait_id = get_core_trait(self.db, "NumericLiteral".into());
        let felt_ty = core_felt252_ty(self.db);

        // Conform all uninferred numeric literals to felt252.
        loop {
            let mut changed = false;
            if let Err(err) = self.solve() {
                return Some((None, err));
            }
            for (var, _) in self.ambiguous.clone() {
                let impl_var = self.impl_var(var).clone();
                if impl_var.concrete_trait_id.trait_id(self.db) != numeric_trait_id {
                    continue;
                }
                // Uninferred numeric trait. Resolve as felt252.
                let ty = extract_matches!(
                    impl_var.concrete_trait_id.generic_args(self.db)[0],
                    GenericArgumentId::Type
                );
                if self.rewrite(ty).no_err() == felt_ty {
                    continue;
                }
                if let Err(err) = self.conform_ty(ty, felt_ty) {
                    return Some((
                        self.stable_ptrs.get(&InferenceVar::Impl(impl_var.id)).copied(),
                        err,
                    ));
                }
                changed = true;
                break;
            }
            if !changed {
                break;
            }
        }
        assert!(
            self.pending.is_empty(),
            "pending should all be solved by this point. Guaranteed by solve()."
        );
        let (var, err) = self.first_undetermined_variable()?;
        Some((self.stable_ptrs.get(&var).copied(), err))
    }

    /// Retrieves the first variable that is still not inferred, or None, if everything is
    /// inferred.
    pub fn first_undetermined_variable(&mut self) -> Option<(InferenceVar, InferenceError)> {
        for (id, var) in self.type_vars.iter().enumerate() {
            if self.type_assignment(LocalTypeVarId(id)).is_none() {
                let ty = self.db.intern_type(TypeLongId::Var(*var));
                return Some((
                    InferenceVar::Type(LocalTypeVarId(id)),
                    InferenceError::TypeNotInferred { ty },
                ));
            }
        }
        if let Some(var) = self.refuted.first().copied() {
            let impl_var = self.impl_var(var).clone();
            let concrete_trait_id = impl_var.concrete_trait_id;
            let concrete_trait_id = self.rewrite(concrete_trait_id).no_err();
            return Some((
                InferenceVar::Impl(var),
                InferenceError::NoImplsFound { concrete_trait_id },
            ));
        }
        if let Some((var, ambiguity)) = self.ambiguous.first() {
            let var = *var;
            // Note: do not rewrite `ambiguity`, since it is expressed in canonical variables.
            return Some((InferenceVar::Impl(var), InferenceError::Ambiguity(ambiguity.clone())));
        }
        None
    }

    /// Assigns a value to an [ImplVar]. Return the assigned impl, or an error.
    /// Assumes the variable is not already assigned.
    fn assign_impl(&mut self, var: LocalImplVarId, impl_id: ImplId) -> InferenceResult<ImplId> {
        self.conform_traits(
            self.impl_var(var).concrete_trait_id,
            impl_id.concrete_trait(self.db)?,
        )?;
        if let Some(other_impl) = self.impl_assignment(var) {
            return self.conform_impl(impl_id, other_impl);
        }
        if self.impl_contains_var(&impl_id, InferenceVar::Impl(var))? {
            return Err(InferenceError::Cycle { var: InferenceVar::Impl(var) });
        }
        self.impl_assignment.insert(var, impl_id);
        Ok(impl_id)
    }

    /// Assigns a value to a [TypeVar]. Return the assigned type, or an error.
    /// Assumes the variable is not already assigned.
    fn assign_ty(&mut self, var: TypeVar, ty: TypeId) -> InferenceResult<TypeId> {
        assert!(!self.type_assignment.contains_key(&var.id), "Cannot reassign variable.");
        let inference_var = InferenceVar::Type(var.id);
        if self.ty_contains_var(ty, inference_var)? {
            return Err(InferenceError::Cycle { var: inference_var });
        }
        self.type_assignment.insert(var.id, ty);
        Ok(ty)
    }

    /// Computes the solution set for an impl variable with a recursive query.
    fn impl_var_solution_set(
        &mut self,
        var: LocalImplVarId,
    ) -> InferenceResult<SolutionSet<ImplId>> {
        let impl_var = self.impl_var(var).clone();
        // Update the concrete trait of the impl var.
        let concrete_trait_id = self.rewrite(impl_var.concrete_trait_id).no_err();
        self.impl_vars[impl_var.id.0].concrete_trait_id = concrete_trait_id;

        let solution_set = self.trait_solution_set(concrete_trait_id, impl_var.lookup_context)?;
        Ok(match solution_set {
            SolutionSet::None => SolutionSet::None,
            SolutionSet::Unique((canonical_impl, canonicalizer)) => {
                SolutionSet::Unique(canonical_impl.embed(self, &canonicalizer))
            }
            SolutionSet::Ambiguous(ambiguity) => SolutionSet::Ambiguous(ambiguity),
        })
    }

    /// Computes the solution set for an trait with a recursive query.
    pub fn trait_solution_set(
        &mut self,
        concrete_trait_id: ConcreteTraitId,
        mut lookup_context: ImplLookupContext,
    ) -> InferenceResult<SolutionSet<(CanonicalImpl, CanonicalMapping)>> {
        // TODO(spapini): This is done twice. Consider doing it only here.
        let concrete_trait_id = self.rewrite(concrete_trait_id).no_err();
        enrich_lookup_context(self.db, concrete_trait_id, &mut lookup_context);

        // Don't try to resolve impls if the first generic param is a variable.
        let generic_args = concrete_trait_id.generic_args(self.db);
        match generic_args.get(0) {
            Some(GenericArgumentId::Type(ty)) => {
                if let TypeLongId::Var(_) = self.db.lookup_intern_type(*ty) {
                    // Don't try to infer such impls.
                    return Ok(SolutionSet::Ambiguous(Ambiguity::WillNotInfer {
                        concrete_trait_id,
                    }));
                }
            }
            Some(GenericArgumentId::Impl(ImplId::ImplVar(_))) => {
                // Don't try to infer such impls.
                return Ok(SolutionSet::Ambiguous(Ambiguity::WillNotInfer { concrete_trait_id }));
            }
            _ => {}
        };

        let (canonical_trait, canonicalizer) =
            CanonicalTrait::canonicalize(self.db, concrete_trait_id);
        match self.db.canonic_trait_solutions(canonical_trait, lookup_context)? {
            SolutionSet::None => Ok(SolutionSet::None),
            SolutionSet::Unique(canonical_impl) => {
                Ok(SolutionSet::Unique((canonical_impl, canonicalizer)))
            }
            SolutionSet::Ambiguous(ambiguity) => Ok(SolutionSet::Ambiguous(ambiguity)),
        }
    }
}

impl<'a> HasDb<&'a dyn SemanticGroup> for Inference<'a> {
    fn get_db(&self) -> &'a dyn SemanticGroup {
        self.db
    }
}
add_basic_rewrites!(<'a>, Inference<'a>, NoError, @exclude TypeLongId ImplId);
add_expr_rewrites!(<'a>, Inference<'a>, NoError, @exclude);
add_rewrite!(<'a>, Inference<'a>, NoError, Ambiguity);
impl<'a> SemanticRewriter<TypeLongId, NoError> for Inference<'a> {
    fn rewrite(&mut self, value: TypeLongId) -> Result<TypeLongId, NoError> {
        if let TypeLongId::Var(var) = value {
            if let Some(type_id) = self.type_assignment.get(&var.id) {
                return self.rewrite(self.db.lookup_intern_type(*type_id));
            }
        }
        value.default_rewrite(self)
    }
}
impl<'a> SemanticRewriter<ImplId, NoError> for Inference<'a> {
    fn rewrite(&mut self, value: ImplId) -> Result<ImplId, NoError> {
        if let ImplId::ImplVar(var) = value {
            // Relax the candidates.
            if let Some(impl_id) = self.impl_assignment(var.get(self.db).id) {
                return self.rewrite(impl_id);
            }
        }
        value.default_rewrite(self)
    }
}
