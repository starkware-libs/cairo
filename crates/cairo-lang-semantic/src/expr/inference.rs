//! Bidirectional type inference.

use std::collections::{HashMap, HashSet, VecDeque};
use std::hash::Hash;
use std::ops::{Deref, DerefMut};

use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::{
    ConstantId, EnumId, ExternFunctionId, ExternTypeId, FreeFunctionId, GenericParamId,
    ImplAliasId, ImplDefId, ImplFunctionId, LanguageElementId, LocalVarId, LookupItemId, MemberId,
    ParamId, StructId, TraitFunctionId, TraitId, VarId, VariantId,
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
use crate::items::constant::{ConstValue, ConstValueId};
use crate::items::functions::{
    ConcreteFunctionWithBody, ConcreteFunctionWithBodyId, GenericFunctionId,
    GenericFunctionWithBodyId, ImplGenericFunctionId, ImplGenericFunctionWithBodyId,
};
use crate::items::generics::{GenericParamConst, GenericParamImpl, GenericParamType};
use crate::items::imp::{ImplId, ImplLookupContext, UninferredImpl};
use crate::items::trt::{ConcreteTraitGenericFunctionId, ConcreteTraitGenericFunctionLongId};
use crate::substitution::{HasDb, RewriteResult, SemanticRewriter, SubstitutionRewriter};
use crate::types::{ConcreteEnumLongId, ConcreteExternTypeLongId, ConcreteStructLongId};
use crate::{
    add_basic_rewrites, add_expr_rewrites, add_rewrite, semantic_object_for_id, ConcreteEnumId,
    ConcreteExternTypeId, ConcreteFunction, ConcreteImplId, ConcreteImplLongId, ConcreteStructId,
    ConcreteTraitId, ConcreteTraitLongId, ConcreteTypeId, ConcreteVariant, FunctionId,
    FunctionLongId, GenericArgumentId, GenericParam, LocalVariable, MatchArmSelector, Member,
    Parameter, SemanticObject, Signature, TypeId, TypeLongId, ValueSelectorArm,
};

pub mod canonic;
pub mod conform;
pub mod infers;
pub mod solver;

/// A type variable, created when a generic type argument is not passed, and thus is not known
/// yet and needs to be inferred.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct TypeVar {
    pub inference_id: InferenceId,
    pub id: LocalTypeVarId,
}

/// A const variable, created when a generic const argument is not passed, and thus is not known
/// yet and needs to be inferred.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct ConstVar {
    pub inference_id: InferenceId,
    pub id: LocalConstVarId,
}

/// An id for an inference context. Each inference variable is associated with an inference id.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, DebugWithDb, SemanticObject)]
#[debug_db(dyn SemanticGroup + 'static)]
pub enum InferenceId {
    LookupItemDeclaration(LookupItemId),
    LookupItemGenerics(LookupItemId),
    LookupItemDefinition(LookupItemId),
    ImplDefTrait(ImplDefId),
    ImplAliasImplDef(ImplAliasId),
    GenericParam(GenericParamId),
    GenericImplParamTrait(GenericParamId),
    Canonical,
    /// For resolving that will not be used anywhere in the semantic model.
    NoContext,
}

/// An impl variable, created when a generic type argument is not passed, and thus is not known
/// yet and needs to be inferred.
#[derive(Clone, Debug, PartialEq, Eq, Hash, DebugWithDb, SemanticObject)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct ImplVar {
    pub inference_id: InferenceId,
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

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, SemanticObject)]
pub struct LocalConstVarId(pub usize);

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

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq, SemanticObject)]
pub enum InferenceVar {
    Type(LocalTypeVarId),
    Const(LocalConstVarId),
    Impl(LocalImplVarId),
}

// TODO(spapini): Add to diagnostics.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum InferenceError {
    Failed(DiagnosticAdded),
    Cycle { var: InferenceVar },
    TypeKindMismatch { ty0: TypeId, ty1: TypeId },
    ConstKindMismatch { const0: ConstValueId, const1: ConstValueId },
    ImplKindMismatch { impl0: ImplId, impl1: ImplId },
    GenericArgMismatch { garg0: GenericArgumentId, garg1: GenericArgumentId },
    TraitMismatch { trt0: TraitId, trt1: TraitId },
    GenericFunctionMismatch { func0: GenericFunctionId, func1: GenericFunctionId },
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
                format!("Type mismatch: `{:?}` and `{:?}`", ty0.debug(db), ty1.debug(db))
            }
            InferenceError::ConstKindMismatch { const0, const1 } => {
                format!("Const mismatch: `{:?}` and `{:?}`", const0.debug(db), const1.debug(db))
            }
            InferenceError::ImplKindMismatch { impl0, impl1 } => {
                format!("Impl mismatch: `{:?}` and `{:?}`", impl0.debug(db), impl1.debug(db))
            }
            InferenceError::GenericArgMismatch { garg0, garg1 } => {
                format!("Generic arg mismatch: `{:?}` and `{:?}`", garg0.debug(db), garg1.debug(db))
            }
            InferenceError::TraitMismatch { trt0, trt1 } => {
                format!("Trait mismatch: `{:?}` and `{:?}`", trt0.debug(db), trt1.debug(db))
            }
            InferenceError::ConstInferenceNotSupported => {
                "Const generic inference not yet supported.".into()
            }
            InferenceError::NoImplsFound { concrete_trait_id } => {
                // TODO(yg): consider separating to 3 sub-error kinds.
                let trait_id = concrete_trait_id.trait_id(db);
                if trait_id == get_core_trait(db, "NumericLiteral".into()) {
                    let generic_type = extract_matches!(
                        concrete_trait_id.generic_args(db)[0],
                        GenericArgumentId::Type
                    );
                    return format!(
                        "Mismatched types. The type `{:?}` cannot be created from a numeric \
                         literal.",
                        generic_type.debug(db)
                    );
                } else if trait_id == get_core_trait(db, "StringLiteral".into()) {
                    let generic_type = extract_matches!(
                        concrete_trait_id.generic_args(db)[0],
                        GenericArgumentId::Type
                    );
                    return format!(
                        "Mismatched types. The type `{:?}` cannot be created from a string \
                         literal.",
                        generic_type.debug(db)
                    );
                }
                format!("Trait has no implementation in context: {:?}", concrete_trait_id.debug(db))
            }
            InferenceError::Ambiguity(ambiguity) => ambiguity.format(db),
            InferenceError::TypeNotInferred { ty } => {
                format!("Type annotations needed. Failed to infer {:?}", ty.debug(db))
            }
            InferenceError::GenericFunctionMismatch { func0, func1 } => {
                format!("Function mismatch: `{}` and `{}`", func0.format(db), func1.format(db))
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
#[derive(Debug, DebugWithDb, PartialEq, Eq)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct InferenceData {
    pub inference_id: InferenceId,
    /// Current inferred assignment for type variables.
    pub type_assignment: HashMap<LocalTypeVarId, TypeId>,
    /// Current inferred assignment for const variables.
    pub const_assignment: HashMap<LocalConstVarId, ConstValueId>,
    /// Current inferred assignment for impl variables.
    pub impl_assignment: HashMap<LocalImplVarId, ImplId>,
    /// Type variables.
    pub type_vars: Vec<TypeVar>,
    /// Const variables.
    pub const_vars: Vec<ConstVar>,
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
    pub fn new(inference_id: InferenceId) -> Self {
        Self {
            inference_id,
            type_assignment: HashMap::new(),
            impl_assignment: HashMap::new(),
            const_assignment: HashMap::new(),
            type_vars: Vec::new(),
            impl_vars: Vec::new(),
            const_vars: Vec::new(),
            stable_ptrs: HashMap::new(),
            pending: VecDeque::new(),
            refuted: Vec::new(),
            solved: Vec::new(),
            ambiguous: Vec::new(),
        }
    }
    pub fn inference<'db, 'b: 'db>(&'db mut self, db: &'b dyn SemanticGroup) -> Inference<'db> {
        Inference { db, data: self }
    }
    pub fn clone_with_inference_id(
        &self,
        db: &dyn SemanticGroup,
        inference_id: InferenceId,
    ) -> InferenceData {
        let mut inference_id_replacer =
            InferenceIdReplacer::new(db, self.inference_id, inference_id);
        Self {
            inference_id,
            type_assignment: self
                .type_assignment
                .iter()
                .map(|(k, v)| (*k, inference_id_replacer.rewrite(*v).no_err()))
                .collect(),
            const_assignment: self
                .const_assignment
                .iter()
                .map(|(k, v)| (*k, inference_id_replacer.rewrite(*v).no_err()))
                .collect(),
            impl_assignment: self
                .impl_assignment
                .iter()
                .map(|(k, v)| (*k, inference_id_replacer.rewrite(*v).no_err()))
                .collect(),
            type_vars: inference_id_replacer.rewrite(self.type_vars.clone()).no_err(),
            const_vars: inference_id_replacer.rewrite(self.const_vars.clone()).no_err(),
            impl_vars: inference_id_replacer.rewrite(self.impl_vars.clone()).no_err(),
            stable_ptrs: self.stable_ptrs.clone(),
            pending: inference_id_replacer.rewrite(self.pending.clone()).no_err(),
            refuted: inference_id_replacer.rewrite(self.refuted.clone()).no_err(),
            solved: inference_id_replacer.rewrite(self.solved.clone()).no_err(),
            ambiguous: inference_id_replacer.rewrite(self.ambiguous.clone()).no_err(),
        }
    }
    pub fn temporary_clone(&self) -> InferenceData {
        Self {
            inference_id: self.inference_id,
            type_assignment: self.type_assignment.clone(),
            const_assignment: self.const_assignment.clone(),
            impl_assignment: self.impl_assignment.clone(),
            type_vars: self.type_vars.clone(),
            const_vars: self.const_vars.clone(),
            impl_vars: self.impl_vars.clone(),
            stable_ptrs: self.stable_ptrs.clone(),
            pending: self.pending.clone(),
            refuted: self.refuted.clone(),
            solved: self.solved.clone(),
            ambiguous: self.ambiguous.clone(),
        }
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

#[derive(Debug)]
pub struct InferenceErrors {
    errors: Vec<(Option<SyntaxStablePtrId>, InferenceError)>,
}
impl InferenceErrors {
    pub fn new() -> Self {
        Self { errors: Vec::new() }
    }
    pub fn new_single(stable_ptr: Option<SyntaxStablePtrId>, err: InferenceError) -> Self {
        let mut res = Self { errors: Vec::new() };
        res.push(stable_ptr, err);
        res
    }
    pub fn push(&mut self, stable_ptr: Option<SyntaxStablePtrId>, err: InferenceError) {
        self.errors.push((stable_ptr, err));
    }
    pub fn report_all(
        &self,
        diagnostics: &mut SemanticDiagnostics,
        default_stable_ptr: SyntaxStablePtrId,
    ) {
        for (stable_ptr, err) in &self.errors {
            err.report(diagnostics, stable_ptr.unwrap_or(default_stable_ptr));
        }
    }
    pub fn report_firsts(
        &self,
        diagnostics: &mut SemanticDiagnostics,
        default_stable_ptr: SyntaxStablePtrId,
    ) {
        let mut reported = HashSet::new();
        for (stable_ptr, err) in &self.errors {
            let location = stable_ptr.unwrap_or(default_stable_ptr);
            // TODO(yg): is there a better way?
            let err_kind = match err {
                InferenceError::Failed(_) => 1,
                InferenceError::Cycle { .. } => 2,
                InferenceError::TypeKindMismatch { .. } => 3,
                InferenceError::ConstKindMismatch { .. } => 4,
                InferenceError::ImplKindMismatch { .. } => 5,
                InferenceError::GenericArgMismatch { .. } => 6,
                InferenceError::TraitMismatch { .. } => 7,
                InferenceError::GenericFunctionMismatch { .. } => 8,
                InferenceError::ConstInferenceNotSupported => 9,
                InferenceError::NoImplsFound { .. } => 10,
                InferenceError::Ambiguity(_) => 11, // TODO(yg): Consider not aggregating all
                // ambiguity errors as a single kind.
                InferenceError::TypeNotInferred { .. } => 12,
            };
            if reported.insert(err_kind) {
                err.report(diagnostics, location);
            }
        }
    }
    pub fn is_empty(&self) -> bool {
        self.errors.is_empty()
    }
    pub fn first(&self) -> InferenceError {
        self.errors[0].1.clone()
    }
}

impl<'db> Inference<'db> {
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
        let var =
            TypeVar { inference_id: self.inference_id, id: LocalTypeVarId(self.type_vars.len()) };
        if let Some(stable_ptr) = stable_ptr {
            self.stable_ptrs.insert(InferenceVar::Type(var.id), stable_ptr);
        }
        self.type_vars.push(var);
        var
    }

    /// Allocates a new [ConstVar] for an unknown consts that needs to be inferred.
    /// Returns a wrapping [ConstValueId].
    pub fn new_const_var(&mut self, stable_ptr: Option<SyntaxStablePtrId>) -> ConstValueId {
        let var = self.new_const_var_raw(stable_ptr);
        self.db.intern_const_value(ConstValue::Var(var))
    }

    /// Allocates a new [ConstVar] for an unknown type that needs to be inferred.
    /// Returns the variable id.
    pub fn new_const_var_raw(&mut self, stable_ptr: Option<SyntaxStablePtrId>) -> ConstVar {
        let var = ConstVar {
            inference_id: self.inference_id,
            id: LocalConstVarId(self.const_vars.len()),
        };
        if let Some(stable_ptr) = stable_ptr {
            self.stable_ptrs.insert(InferenceVar::Const(var.id), stable_ptr);
        }
        self.const_vars.push(var);
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
        let var =
            ImplVar { inference_id: self.inference_id, id, concrete_trait_id, lookup_context };
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
            // First inference error stops inference.
            self.solve_single_pending(var)?;
        }
        Ok(())
    }

    fn solve_single_pending(&mut self, var: LocalImplVarId) -> InferenceResult<()> {
        let solution = match self.impl_var_solution_set(var)? {
            SolutionSet::None => {
                self.refuted.push(var);
                return Ok(());
            }
            SolutionSet::Ambiguous(ambiguity) => {
                self.ambiguous.push((var, ambiguity));
                return Ok(());
            }
            SolutionSet::Unique(solution) => solution,
        };

        // Solution found. Assign it.
        self.assign_local_impl(var, solution)?;

        // Something changed.
        self.solved.push(var);
        let mut ambiguous = std::mem::take(&mut self.ambiguous);
        self.pending.extend(ambiguous.drain(..).map(|(var, _)| var));

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
    pub fn finalize(&mut self) -> Option<InferenceErrors> {
        let numeric_trait_id = get_core_trait(self.db, "NumericLiteral".into());
        let felt_ty = core_felt252_ty(self.db);

        // Conform all uninferred numeric literals to felt252.
        loop {
            let mut changed = false;
            if let Err(err) = self.solve() {
                return Some(InferenceErrors::new_single(None, err));
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
                    return Some(InferenceErrors::new_single(
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
        let mut inference_errors = InferenceErrors::new();
        while let Some((var, err)) = self.first_undetermined_variable() {
            inference_errors.push(self.stable_ptrs.get(&var).copied(), err);
        }
        // TODO(yg): change later to vector directly, without option.
        if inference_errors.is_empty() { None } else { Some(inference_errors) }
    }

    /// Retrieves the first variable that is still not inferred, or None, if everything is
    /// inferred.
    pub fn first_undetermined_variable(&mut self) -> Option<(InferenceVar, InferenceError)> {
        // TODO(yg): don't clone.
        for (id, var) in self.type_vars.clone().iter().enumerate() {
            if self.type_assignment(LocalTypeVarId(id)).is_none() {
                let ty = self.db.intern_type(TypeLongId::Var(*var));
                // TODO(yg): do the pops better (all remove(0)s).
                self.type_vars.remove(0);
                return Some((InferenceVar::Type(var.id), InferenceError::TypeNotInferred { ty }));
            }
        }
        if let Some(var) = self.refuted.first().copied() {
            let impl_var = self.impl_var(var).clone();
            let concrete_trait_id = impl_var.concrete_trait_id;
            let concrete_trait_id = self.rewrite(concrete_trait_id).no_err();
            self.refuted.remove(0);
            return Some((
                InferenceVar::Impl(var),
                InferenceError::NoImplsFound { concrete_trait_id },
            ));
        }
        if let Some((var, ambiguity)) = self.ambiguous.first().cloned() {
            let var = var;
            // Note: do not rewrite `ambiguity`, since it is expressed in canonical variables.
            self.ambiguous.remove(0);
            return Some((InferenceVar::Impl(var), InferenceError::Ambiguity(ambiguity.clone())));
        }
        None
    }

    /// Assigns a value to a local impl variable id. See assign_impl().
    fn assign_local_impl(
        &mut self,
        var: LocalImplVarId,
        impl_id: ImplId,
    ) -> InferenceResult<ImplId> {
        self.conform_traits(
            self.impl_var(var).concrete_trait_id,
            impl_id.concrete_trait(self.db)?,
        )?;
        if let Some(other_impl) = self.impl_assignment(var) {
            return self.conform_impl(impl_id, other_impl);
        }
        if self.impl_contains_var(&impl_id, InferenceVar::Impl(var)) {
            return Err(InferenceError::Cycle { var: InferenceVar::Impl(var) });
        }
        self.impl_assignment.insert(var, impl_id);
        Ok(impl_id)
    }

    /// Tries to assigns value to an [ImplVarId]. Return the assigned impl, or an error.
    fn assign_impl(&mut self, var_id: ImplVarId, impl_id: ImplId) -> InferenceResult<ImplId> {
        let var = var_id.get(self.db);
        if var.inference_id != self.inference_id {
            return Err(InferenceError::ImplKindMismatch {
                impl0: ImplId::ImplVar(var_id),
                impl1: impl_id,
            });
        }
        self.assign_local_impl(var.id, impl_id)
    }

    /// Assigns a value to a [TypeVar]. Return the assigned type, or an error.
    /// Assumes the variable is not already assigned.
    fn assign_ty(&mut self, var: TypeVar, ty: TypeId) -> InferenceResult<TypeId> {
        if var.inference_id != self.inference_id {
            return Err(InferenceError::TypeKindMismatch {
                ty0: self.db.intern_type(TypeLongId::Var(var)),
                ty1: ty,
            });
        }
        assert!(!self.type_assignment.contains_key(&var.id), "Cannot reassign variable.");
        let inference_var = InferenceVar::Type(var.id);
        if self.ty_contains_var(ty, inference_var) {
            return Err(InferenceError::Cycle { var: inference_var });
        }
        self.type_assignment.insert(var.id, ty);
        Ok(ty)
    }

    /// Assigns a value to a [ConstVar]. Return the assigned const, or an error.
    /// Assumes the variable is not already assigned.
    fn assign_const(&mut self, var: ConstVar, id: ConstValueId) -> InferenceResult<ConstValueId> {
        if var.inference_id != self.inference_id {
            return Err(InferenceError::ConstKindMismatch {
                const0: self.db.intern_const_value(ConstValue::Var(var)),
                const1: id,
            });
        }

        self.const_assignment.insert(var.id, id);
        Ok(id)
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

    /// Computes the solution set for a trait with a recursive query.
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
        match generic_args.first() {
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
            Some(GenericArgumentId::Constant(const_value)) => {
                if let ConstValue::Var(_) = self.db.lookup_intern_const_value(*const_value) {
                    // Don't try to infer such impls.
                    return Ok(SolutionSet::Ambiguous(Ambiguity::WillNotInfer {
                        concrete_trait_id,
                    }));
                }
            }
            _ => {}
        };

        let (canonical_trait, canonicalizer) =
            CanonicalTrait::canonicalize(self.db, self.inference_id, concrete_trait_id);
        match self.db.canonic_trait_solutions(canonical_trait, lookup_context)? {
            SolutionSet::None => Ok(SolutionSet::None),
            SolutionSet::Unique(canonical_impl) => {
                Ok(SolutionSet::Unique((canonical_impl, canonicalizer)))
            }
            SolutionSet::Ambiguous(ambiguity) => Ok(SolutionSet::Ambiguous(ambiguity)),
        }
    }

    /// Validate that the given impl is valid based on its negative impls arguments.
    /// Returns `SolutionSet::Unique(canonical_impl)` if the impl is valid and
    /// SolutionSet::Ambiguous(...) otherwise.
    fn validate_neg_impls(
        &mut self,
        lookup_context: &ImplLookupContext,
        canonical_impl: CanonicalImpl,
    ) -> InferenceResult<SolutionSet<CanonicalImpl>> {
        let ImplId::Concrete(concrete_impl) = canonical_impl.0 else {
            return Ok(SolutionSet::Unique(canonical_impl));
        };
        let mut rewriter = SubstitutionRewriter {
            db: self.db,
            substitution: &concrete_impl.substitution(self.db)?,
        };

        for garg in self.db.impl_def_generic_params(concrete_impl.impl_def_id(self.db))? {
            let GenericParam::NegImpl(neg_impl) = garg else {
                continue;
            };

            let concrete_trait_id = rewriter.rewrite(neg_impl)?.concrete_trait?;
            for garg in concrete_trait_id.generic_args(self.db) {
                let GenericArgumentId::Type(ty) = garg else {
                    continue;
                };

                // If the negative impl has a generic argument that is not fully
                // concrete we can't tell if we should rule out the candidate impl.
                // For example if we have -TypeEqual<S, T> we can't tell if S and
                // T are going to be assigned the same concrete type.
                // We return `SolutionSet::Ambiguous` here to indicate that more
                // information is needed.
                if let TypeLongId::Var(_) = self.db.lookup_intern_type(ty) {
                    if !ty.is_fully_concrete(self.db) {
                        // TODO(ilya): Try to detect the ambiguity earlier in the
                        // inference process.
                        return Ok(SolutionSet::Ambiguous(
                            Ambiguity::NegativeImplWithUnresolvedGenericArgs {
                                impl_id: ImplId::Concrete(concrete_impl),
                                ty,
                            },
                        ));
                    }
                }
            }

            if !matches!(
                self.trait_solution_set(concrete_trait_id, lookup_context.clone())?,
                SolutionSet::None
            ) {
                // If a negative impl has an impl, then we should skip it.
                return Ok(SolutionSet::None);
            }
        }

        Ok(SolutionSet::Unique(canonical_impl))
    }
}

impl<'a> HasDb<&'a dyn SemanticGroup> for Inference<'a> {
    fn get_db(&self) -> &'a dyn SemanticGroup {
        self.db
    }
}
add_basic_rewrites!(<'a>, Inference<'a>, NoError, @exclude TypeLongId ImplId ConstValue);
add_expr_rewrites!(<'a>, Inference<'a>, NoError, @exclude);
add_rewrite!(<'a>, Inference<'a>, NoError, Ambiguity);
impl<'a> SemanticRewriter<TypeLongId, NoError> for Inference<'a> {
    fn internal_rewrite(&mut self, value: &mut TypeLongId) -> Result<RewriteResult, NoError> {
        if let TypeLongId::Var(var) = value {
            if let Some(type_id) = self.type_assignment.get(&var.id) {
                let mut long_type_id = self.db.lookup_intern_type(*type_id);
                self.internal_rewrite(&mut long_type_id)?;
                *value = long_type_id;
                return Ok(RewriteResult::Modified);
            }
        }
        value.default_rewrite(self)
    }
}
impl<'a> SemanticRewriter<ConstValue, NoError> for Inference<'a> {
    fn internal_rewrite(&mut self, value: &mut ConstValue) -> Result<RewriteResult, NoError> {
        if let ConstValue::Var(var) = value {
            if let Some(const_value_id) = self.const_assignment.get(&var.id) {
                let mut const_value = self.db.lookup_intern_const_value(*const_value_id);
                self.internal_rewrite(&mut const_value)?;
                *value = const_value;
                return Ok(RewriteResult::Modified);
            }
        }
        value.default_rewrite(self)
    }
}
impl<'a> SemanticRewriter<ImplId, NoError> for Inference<'a> {
    fn internal_rewrite(&mut self, value: &mut ImplId) -> Result<RewriteResult, NoError> {
        if let ImplId::ImplVar(var) = value {
            // Relax the candidates.
            if let Some(mut impl_id) = self.impl_assignment(var.get(self.db).id) {
                self.internal_rewrite(&mut impl_id)?;
                *value = impl_id;
                return Ok(RewriteResult::Modified);
            }
        }
        value.default_rewrite(self)
    }
}

struct InferenceIdReplacer<'a> {
    db: &'a dyn SemanticGroup,
    from_inference_id: InferenceId,
    to_inference_id: InferenceId,
}
impl<'a> InferenceIdReplacer<'a> {
    fn new(
        db: &'a dyn SemanticGroup,
        from_inference_id: InferenceId,
        to_inference_id: InferenceId,
    ) -> Self {
        Self { db, from_inference_id, to_inference_id }
    }
}
impl<'a> HasDb<&'a dyn SemanticGroup> for InferenceIdReplacer<'a> {
    fn get_db(&self) -> &'a dyn SemanticGroup {
        self.db
    }
}
add_basic_rewrites!(<'a>, InferenceIdReplacer<'a>, NoError, @exclude InferenceId);
add_expr_rewrites!(<'a>, InferenceIdReplacer<'a>, NoError, @exclude);
add_rewrite!(<'a>, InferenceIdReplacer<'a>, NoError, Ambiguity);
impl<'a> SemanticRewriter<InferenceId, NoError> for InferenceIdReplacer<'a> {
    fn internal_rewrite(&mut self, value: &mut InferenceId) -> Result<RewriteResult, NoError> {
        if value == &self.from_inference_id {
            *value = self.to_inference_id;
            Ok(RewriteResult::Modified)
        } else {
            Ok(RewriteResult::NoChange)
        }
    }
}
