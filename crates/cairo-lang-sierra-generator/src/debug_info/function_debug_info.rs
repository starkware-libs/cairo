#[expect(clippy::disallowed_types)]
use std::collections::HashMap;
use std::iter::{self, zip};
use std::ops::Range;

use cairo_lang_defs::diagnostic_utils::StableLocation;
use cairo_lang_defs::ids::{FunctionWithBodyId, VarId as SemanticVarId};
use cairo_lang_lowering::ids::LocationId;
use cairo_lang_semantic::expr::pattern::QueryPatternVariablesFromDb;
use cairo_lang_semantic::items::function_with_body::FunctionWithBodySemantic;
use cairo_lang_semantic::{
    Condition, Expr, ExprFunctionCallArg, ExprId, Pattern, PatternId, Statement, StatementId,
};
use cairo_lang_sierra::ids::{ConcreteTypeId, FunctionId, VarId};
use cairo_lang_sierra::program::{self, GenStatement, StatementIdx};
use cairo_lang_syntax::node::ast::{BinaryOperator, ExprBinary};
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{TypedStablePtr, TypedSyntaxNode};
use cairo_lang_utils::CloneableDatabase;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use itertools::Itertools;
use rayon::iter::{IntoParallelIterator, ParallelIterator};
use salsa::Database;

use crate::db::SierraGenGroup;
use crate::debug_info::function_debug_info::serializable::{
    CairoVariableName, SerializableAllFunctionsDebugInfo, SerializableFunctionDebugInfo,
    SierraFunctionId, SierraVarId,
};
use crate::debug_info::{
    SourceCodeLocation, SourceCodeSpan, SourceFileFullPath, maybe_code_location,
};
use crate::utils::get_libfunc_signature;

pub mod serializable;

/// The debug info of all Sierra functions in the program.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct AllFunctionsDebugInfo<'db>(OrderedHashMap<FunctionId, FunctionDebugInfo<'db>>);

impl<'db> AllFunctionsDebugInfo<'db> {
    pub fn new(value: OrderedHashMap<FunctionId, FunctionDebugInfo<'db>>) -> Self {
        Self(value)
    }

    /// Returns a corresponding [`SerializableAllFunctionsDebugInfo`].
    pub fn extract_serializable_debug_info(
        &self,
        db: &'db dyn CloneableDatabase,
        program: &program::Program,
    ) -> SerializableAllFunctionsDebugInfo {
        let functions_statement_ranges = collect_functions_with_statement_ranges(program);

        let serializable_debug_info = self
            .0
            .iter()
            .map(|(function_id, function_debug_info)| {
                let (function, statement_range) = &functions_statement_ranges[function_id];
                (function_id.to_owned(), function_debug_info, function, statement_range.to_owned())
            })
            .collect_vec()
            .into_par_iter()
            .map_with(
                db.dyn_clone(),
                |db, (function_id, function_debug_info, function, statement_range)| {
                    Some((
                        SierraFunctionId(function_id.id),
                        function_debug_info.extract_serializable_debug_info(
                            db.as_ref(),
                            function,
                            &program.statements[statement_range],
                        )?,
                    ))
                },
            )
            .flatten()
            .collect();

        SerializableAllFunctionsDebugInfo(serializable_debug_info)
    }
}

/// The debug info of a Sierra function.
/// Contains a signature location and locations of Sierra variables of this function.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FunctionDebugInfo<'db> {
    pub signature_location: LocationId<'db>,
    pub variables_locations: OrderedHashMap<VarId, LocationId<'db>>,
}

impl<'db> FunctionDebugInfo<'db> {
    /// Returns a corresponding [`SerializableFunctionDebugInfo`].
    fn extract_serializable_debug_info(
        &self,
        db: &'db dyn Database,
        program_func: &program::Function,
        function_statements: &[program::Statement],
    ) -> Option<SerializableFunctionDebugInfo> {
        let (function_file_path, function_code_span) = self.extract_location(db)?;
        let sierra_to_cairo_variables =
            self.extract_sierra_to_cairo_variables_map(db, program_func, function_statements)?;

        Some(SerializableFunctionDebugInfo {
            function_file_path,
            function_code_span,
            sierra_to_cairo_variables,
        })
    }

    /// Extracts a mapping from a Sierra variable to all corresponding Cairo variables
    /// (their names and definition spans). A value of a Sierra variable corresponds to the value
    /// of each of the collected Cairo variables at some points during the execution.
    #[expect(clippy::disallowed_types)]
    fn extract_sierra_to_cairo_variables_map(
        &self,
        db: &'db dyn Database,
        function: &program::Function,
        function_statements: &[program::Statement],
    ) -> Option<HashMap<SierraVarId, Vec<(CairoVariableName, SourceCodeSpan)>>> {
        let function_with_body_id = resolve_function_with_body(db, function)?;

        let var_introduction_index = build_var_introduction_index(db, &self.variables_locations);
        let sierra_var_types = build_sierra_var_types_map(db, function, function_statements);

        let mut collector = BindingCollector::new(
            db,
            function_with_body_id,
            &var_introduction_index,
            &sierra_var_types,
        );

        collector.initialize_params(function);

        if let Ok(body_expr) = db.function_body_expr(function_with_body_id) {
            collector.visit_expr(body_expr);
        }

        Some(collector.bindings)
    }

    /// Extracts the location of the function - path to the user file it comes from and its span.
    fn extract_location(
        &self,
        db: &'db dyn Database,
    ) -> Option<(SourceFileFullPath, SourceCodeSpan)> {
        let function_ptr = self
            .signature_location
            .long(db)
            .stable_location
            .syntax_node(db)
            .ancestor_of_kinds(db, &[SyntaxKind::FunctionWithBody, SyntaxKind::TraitItemFunction])?
            .stable_ptr(db);
        let (function_file_path, function_code_span, _) =
            maybe_code_location(db, StableLocation::new(function_ptr))?;

        Some((function_file_path, function_code_span))
    }
}

/// Returns a map of all functions in [`program::Program`], with corresponding ranges of
/// their [`StatementIdx`]. This function relies on a contract of contiguous range of statement
/// indexes throughout the whole program.
#[expect(clippy::disallowed_types)]
fn collect_functions_with_statement_ranges(
    program: &program::Program,
) -> HashMap<&FunctionId, (&program::Function, Range<usize>)> {
    let max_statement_idx = StatementIdx(program.statements.len() - 1);

    let function_ids_by_entrypoints = program
        .funcs
        .iter()
        .sorted_by_key(|function| function.entry_point)
        .map(Option::Some)
        .chain(iter::once(None));

    function_ids_by_entrypoints
        .tuple_windows()
        .map(move |(function1, function2)| match (function1, function2) {
            (Some(function1), Some(function2)) => {
                let range = function1.entry_point.0..function2.entry_point.0;
                (&function1.id, (function1, range))
            }
            (Some(function), None) => {
                let range = function.entry_point.0..max_statement_idx.0;
                (&function.id, (function, range))
            }
            _ => unreachable!("first item is guaranteed to be Some"),
        })
        .collect()
}

/// Returns a [`FunctionWithBodyId`] for the given Sierra function.
fn resolve_function_with_body<'db>(
    db: &'db dyn Database,
    function: &program::Function,
) -> Option<FunctionWithBodyId<'db>> {
    let lowering_fn_id = db.lookup_sierra_function(&function.id);
    let concrete_with_body = lowering_fn_id.body(db).ok().flatten()?;
    let semantic_concrete = match concrete_with_body.long(db) {
        cairo_lang_lowering::ids::ConcreteFunctionWithBodyLongId::Semantic(id) => *id,
        _ => return None,
    };
    Some(semantic_concrete.function_with_body_id(db))
}

/// Builds an index from a syntax node to a list of Sierra variables that were allocated
/// at that node during lowering, in sierra-gen's allocation order.
///
/// For each variable, its original `stable_location` and all `inline_locations`s
/// are collected as introduction points.
#[expect(clippy::disallowed_types)]
fn build_var_introduction_index<'db>(
    db: &'db dyn Database,
    variables_locations: &OrderedHashMap<VarId, LocationId<'db>>,
) -> HashMap<SyntaxStablePtrId<'db>, Vec<VarId>> {
    let mut index: HashMap<SyntaxStablePtrId<'db>, Vec<VarId>> = HashMap::new();

    for (var_id, location_id) in variables_locations.iter() {
        for stable_location in location_id.all_locations(db) {
            let ptr = stable_location.stable_ptr();
            let entries = index.entry(ptr).or_default();

            if !entries.contains(var_id) {
                entries.push(var_id.clone());
            }
        }
    }

    index
}

/// Returns a map of concrete types for each Sierra variable in the function.
#[expect(clippy::disallowed_types)]
fn build_sierra_var_types_map(
    db: &dyn Database,
    function: &program::Function,
    statements: &[program::Statement],
) -> HashMap<VarId, ConcreteTypeId> {
    let mut types: HashMap<VarId, ConcreteTypeId> = HashMap::new();

    for param in &function.params {
        types.insert(param.id.clone(), param.ty.clone());
    }

    for statement in statements {
        let GenStatement::Invocation(invocation) = statement else {
            continue;
        };
        let signature = get_libfunc_signature(db, &invocation.libfunc_id);

        for (branch_info, branch_signature) in
            invocation.branches.iter().zip(signature.branch_signatures.iter())
        {
            for (result_var, output_info) in
                branch_info.results.iter().zip(branch_signature.vars.iter())
            {
                types.insert(result_var.clone(), output_info.ty.clone());
            }
        }
    }
    types
}

/// Returns an originating [`SourceCodeSpan`] for a stable pointer.
fn compute_span<'db>(db: &'db dyn Database, ptr: SyntaxStablePtrId<'db>) -> Option<SourceCodeSpan> {
    let location = StableLocation::new(ptr);
    let user_location = location.span_in_file(db).user_location(db);
    let position = user_location.span.position_in_file(db, user_location.file_id)?;
    Some(SourceCodeSpan {
        start: SourceCodeLocation { line: position.start.line, col: position.start.col },
        end: SourceCodeLocation { line: position.end.line, col: position.end.col },
    })
}

/// Walks the function body's semantic tree, emitting variable-binding events. Each event is tied
/// to the Sierra variable that holds the bound value at runtime. See
/// [`FunctionDebugInfo::extract_sierra_to_cairo_variables_map`] for the strategy summary.
#[expect(clippy::disallowed_types)]
struct BindingCollector<'db, 'a> {
    db: &'db dyn Database,
    function_with_body_id: FunctionWithBodyId<'db>,
    var_introduction_index: &'a HashMap<SyntaxStablePtrId<'db>, Vec<VarId>>,
    sierra_var_types: &'a HashMap<VarId, ConcreteTypeId>,
    /// Map from a Cairo variable to a Sierra variable that currently holds its value.
    alias_map: HashMap<SemanticVarId<'db>, VarId>,
    /// Cairo variable encountered so far.
    cairo_var_info: HashMap<SemanticVarId<'db>, (CairoVariableName, SourceCodeSpan)>,
    /// Output multi-map accumulated so far.
    bindings: HashMap<SierraVarId, Vec<(CairoVariableName, SourceCodeSpan)>>,
}

impl<'db, 'a> BindingCollector<'db, 'a> {
    #[expect(clippy::disallowed_types)]
    fn new(
        db: &'db dyn Database,
        function_with_body_id: FunctionWithBodyId<'db>,
        var_introduction_index: &'a HashMap<SyntaxStablePtrId<'db>, Vec<VarId>>,
        sierra_var_types: &'a HashMap<VarId, ConcreteTypeId>,
    ) -> Self {
        Self {
            db,
            function_with_body_id,
            var_introduction_index,
            sierra_var_types,
            alias_map: HashMap::new(),
            cairo_var_info: HashMap::new(),
            bindings: HashMap::new(),
        }
    }

    /// Initializes the alias map, Cairo var info, and bindings from function parameters.
    fn initialize_params(&mut self, function: &program::Function) {
        let Ok(signature) = self.db.function_with_body_signature(self.function_with_body_id) else {
            return;
        };

        // Params of a Sierra function may contain builtins, that are not representable
        // as Cairo variables and thus not included in `function_param_variables`.
        // Zipping from the end skips the builtins.
        for (sierra_param, cairo_param) in
            zip(function.params.iter().rev(), signature.params.iter().rev())
        {
            let name = cairo_param.name.long(self.db).to_string();
            let Some(span) = compute_span(self.db, cairo_param.stable_ptr.untyped()) else {
                continue;
            };
            self.record_binding(
                SemanticVarId::Param(cairo_param.id),
                name,
                span,
                sierra_param.id.clone(),
            );
        }
    }

    /// Records a binding of a Cairo var (id, name, span) and a Sierra var and updates the alias
    /// map.
    fn record_binding(
        &mut self,
        cairo_var: SemanticVarId<'db>,
        name: CairoVariableName,
        span: SourceCodeSpan,
        sierra_var: VarId,
    ) {
        self.cairo_var_info.insert(cairo_var, (name.clone(), span.clone()));
        self.alias_map.insert(cairo_var, sierra_var.clone());
        self.bindings.entry(SierraVarId(sierra_var.id)).or_default().push((name, span));
    }

    /// Records an additional binding, when we already know the var info.
    fn rebind(&mut self, cairo_var: SemanticVarId<'db>, sierra_var: VarId) {
        let Some((name, span)) = self.cairo_var_info.get(&cairo_var).cloned() else {
            return;
        };

        self.alias_map.insert(cairo_var, sierra_var.clone());
        self.bindings.entry(SierraVarId(sierra_var.id)).or_default().push((name, span));
    }

    fn visit_expr(&mut self, expr_id: ExprId) {
        let expr = self.db.expr_semantic(self.function_with_body_id, expr_id);
        match expr {
            Expr::Block(block) => {
                for stmt_id in block.statements {
                    self.visit_statement(stmt_id);
                }
                if let Some(tail) = block.tail {
                    self.visit_expr(tail);
                }
            }
            Expr::If(if_expr) => {
                for condition in if_expr.conditions {
                    match condition {
                        Condition::BoolExpr(id) => self.visit_expr(id),
                        Condition::Let(rhs_id, patterns) => {
                            self.visit_expr(rhs_id);
                            let rhs_expr =
                                self.db.expr_semantic(self.function_with_body_id, rhs_id);
                            let rhs_sierra = self.resolve_rhs_sierra_var(rhs_id, rhs_expr.ty());
                            for pattern_id in patterns {
                                self.bind_pattern(pattern_id, rhs_sierra.clone());
                            }
                        }
                    }
                }
                self.visit_expr(if_expr.if_block);
                if let Some(else_block) = if_expr.else_block {
                    self.visit_expr(else_block);
                }
            }
            Expr::Match(match_expr) => {
                self.visit_expr(match_expr.matched_expr);
                let matched_ty =
                    self.db.expr_semantic(self.function_with_body_id, match_expr.matched_expr).ty();
                let scrutinee_sierra =
                    self.resolve_rhs_sierra_var(match_expr.matched_expr, matched_ty);
                for arm in match_expr.arms {
                    for pattern_id in arm.patterns {
                        self.bind_pattern(pattern_id, scrutinee_sierra.clone());
                    }
                    self.visit_expr(arm.expression);
                }
            }
            Expr::For(for_expr) => {
                self.visit_expr(for_expr.expr_id);
                // Best-effort: bind pattern variables; their Sierra vars (if any) will be found
                // via the per-leaf var-introduction index lookup inside `bind_pattern`.
                self.bind_pattern(for_expr.pattern, None);
                self.visit_expr(for_expr.body);
            }
            Expr::Loop(loop_expr) => self.visit_expr(loop_expr.body),
            Expr::While(while_expr) => {
                match while_expr.condition {
                    Condition::BoolExpr(id) => self.visit_expr(id),
                    Condition::Let(rhs_id, patterns) => {
                        self.visit_expr(rhs_id);
                        let rhs_expr = self.db.expr_semantic(self.function_with_body_id, rhs_id);
                        let rhs_sierra = self.resolve_rhs_sierra_var(rhs_id, rhs_expr.ty());
                        for pattern_id in patterns {
                            self.bind_pattern(pattern_id, rhs_sierra.clone());
                        }
                    }
                }
                self.visit_expr(while_expr.body);
            }
            Expr::ExprClosure(_) => {
                // Closures lower to separate Sierra functions; their bodies have their own
                // `FunctionDebugInfo` entry. Skip descending here.
            }
            Expr::FunctionCall(call) => {
                for arg in &call.args {
                    match arg {
                        ExprFunctionCallArg::Value(id) | ExprFunctionCallArg::TempReference(id) => {
                            self.visit_expr(*id);
                        }
                        ExprFunctionCallArg::Reference(_) => {}
                    }
                }
                if let Some(coupon_id) = call.coupon_arg {
                    self.visit_expr(coupon_id);
                }
                // Detect compound assignment: the call's syntactic origin is an `ExprBinary` with
                // a compound op. The first arg is a Reference to the LHS variable.
                let call_node = call.stable_ptr.lookup(self.db).as_syntax_node();
                if let Some(binary) = ExprBinary::cast(self.db, call_node) {
                    let is_compound = matches!(
                        binary.op(self.db),
                        BinaryOperator::MulEq(_)
                            | BinaryOperator::DivEq(_)
                            | BinaryOperator::ModEq(_)
                            | BinaryOperator::PlusEq(_)
                            | BinaryOperator::MinusEq(_)
                    );
                    if is_compound
                        && let Some(ExprFunctionCallArg::Reference(member_path)) = call.args.first()
                    {
                        let lhs_var = member_path.base_var();
                        let lhs_ty = member_path.ty();
                        let call_ptr: SyntaxStablePtrId<'db> = call.stable_ptr.into();
                        if let Some(result_var) = self.resolve_at_node(call_ptr, lhs_ty) {
                            self.rebind(lhs_var, result_var);
                        }
                    }
                }
            }
            Expr::Assignment(assignment) => {
                self.visit_expr(assignment.rhs);
                let rhs_ty = assignment.ref_arg.ty();
                if let Some(sierra_var) = self.resolve_rhs_sierra_var(assignment.rhs, rhs_ty) {
                    self.rebind(assignment.ref_arg.base_var(), sierra_var);
                }
            }
            Expr::Tuple(tuple) => {
                for item in tuple.items {
                    self.visit_expr(item);
                }
            }
            Expr::FixedSizeArray(arr) => {
                use cairo_lang_semantic::FixedSizeArrayItems;
                match arr.items {
                    FixedSizeArrayItems::Items(items) => {
                        for item in items {
                            self.visit_expr(item);
                        }
                    }
                    FixedSizeArrayItems::ValueAndSize(item, _) => self.visit_expr(item),
                }
            }
            Expr::StructCtor(ctor) => {
                for (id, _) in ctor.members {
                    self.visit_expr(id);
                }
                if let Some(base) = ctor.base_struct {
                    self.visit_expr(base);
                }
            }
            Expr::EnumVariantCtor(ctor) => self.visit_expr(ctor.value_expr),
            Expr::MemberAccess(access) => self.visit_expr(access.expr),
            Expr::Snapshot(snap) => self.visit_expr(snap.inner),
            Expr::Desnap(desnap) => self.visit_expr(desnap.inner),
            Expr::LogicalOperator(op) => {
                self.visit_expr(op.lhs);
                self.visit_expr(op.rhs);
            }
            Expr::PropagateError(propagate) => self.visit_expr(propagate.inner),
            Expr::Var(_)
            | Expr::Literal(_)
            | Expr::StringLiteral(_)
            | Expr::Constant(_)
            | Expr::Missing(_) => {}
        }
    }

    fn visit_statement(&mut self, stmt_id: StatementId) {
        let stmt = self.db.statement_semantic(self.function_with_body_id, stmt_id);
        match stmt {
            Statement::Let(let_stmt) => {
                self.visit_expr(let_stmt.expr);
                if let Some(else_id) = let_stmt.else_clause {
                    self.visit_expr(else_id);
                }
                let rhs_ty = self.db.expr_semantic(self.function_with_body_id, let_stmt.expr).ty();
                let rhs_sierra = self.resolve_rhs_sierra_var(let_stmt.expr, rhs_ty);
                self.bind_pattern(let_stmt.pattern, rhs_sierra);
            }
            Statement::Expr(stmt_expr) => self.visit_expr(stmt_expr.expr),
            Statement::Return(stmt_return) => {
                if let Some(id) = stmt_return.expr_option {
                    self.visit_expr(id);
                }
            }
            Statement::Break(stmt_break) => {
                if let Some(id) = stmt_break.expr_option {
                    self.visit_expr(id);
                }
            }
            Statement::Continue(_) | Statement::Item(_) => {}
        }
    }

    /// Resolves the Sierra variable that holds the runtime value of `rhs_id` of semantic
    /// type `expected_ty`.
    fn resolve_rhs_sierra_var(
        &self,
        rhs_id: ExprId,
        expected_ty: cairo_lang_semantic::TypeId<'db>,
    ) -> Option<VarId> {
        let expr = self.db.expr_semantic(self.function_with_body_id, rhs_id);
        match expr {
            Expr::Var(var) => self.alias_map.get(&var.var).cloned(),
            Expr::Block(block) => block.tail.and_then(|tail| {
                let tail_ty = self.db.expr_semantic(self.function_with_body_id, tail).ty();
                self.resolve_rhs_sierra_var(tail, tail_ty)
            }),
            _ => {
                let ptr: SyntaxStablePtrId<'db> = expr.stable_ptr().into();
                self.resolve_at_node(ptr, expected_ty)
            }
        }
    }

    /// Looks up the var-introduction index at `ptr` and picks a Sierra variable whose type matches
    /// the bridged Sierra type of `expected_ty`. Returns the first match in allocation order.
    ///
    /// Skips type-filtering (and returns `None`) if `expected_ty` isn't fully concrete — in
    /// generic contexts we can't bridge it to a Sierra `ConcreteTypeId`, and
    /// `get_concrete_type_id` would panic.
    fn resolve_at_node(
        &self,
        ptr: SyntaxStablePtrId<'db>,
        expected_ty: cairo_lang_semantic::TypeId<'db>,
    ) -> Option<VarId> {
        if !expected_ty.is_fully_concrete(self.db) {
            return None;
        }
        let expected_sierra_ty = self.db.get_concrete_type_id(expected_ty).ok()?.clone();
        let candidates = self.var_introduction_index.get(&ptr)?;
        candidates
            .iter()
            .find(|c| {
                self.sierra_var_types.get(*c).map(|ty| ty == &expected_sierra_ty).unwrap_or(false)
            })
            .cloned()
    }

    /// Records bindings for every pattern variable in `pattern_id`, projecting onto `rhs_sierra`
    /// when each leaf doesn't have its own freshly-allocated Sierra var.
    fn bind_pattern(&mut self, pattern_id: PatternId, rhs_sierra: Option<VarId>) {
        let pattern = self.db.pattern_semantic(self.function_with_body_id, pattern_id);
        let is_single_variable = matches!(pattern, Pattern::Variable(_));
        let pattern_vars =
            pattern.variables(&QueryPatternVariablesFromDb(self.db, self.function_with_body_id));
        for pat_var in pattern_vars {
            let leaf_sierra = self
                .resolve_at_node(pat_var.stable_ptr.untyped(), pat_var.var.ty)
                .or_else(|| if is_single_variable { rhs_sierra.clone() } else { None });
            let Some(sierra_var) = leaf_sierra else {
                continue;
            };
            let name = pat_var.name.long(self.db).to_string();
            let Some(span) = compute_span(self.db, pat_var.var.stable_ptr(self.db).untyped())
            else {
                continue;
            };
            self.record_binding(SemanticVarId::Local(pat_var.var.id), name, span, sierra_var);
        }
    }
}
