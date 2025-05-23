use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::NamedLanguageElementId;
use cairo_lang_diagnostics::Maybe;
use cairo_lang_filesystem::flag::Flag;
use cairo_lang_filesystem::ids::FlagId;
use cairo_lang_semantic::{self as semantic, ConcreteVariant, GenericArgumentId, corelib};
use cairo_lang_syntax::node::TypedStablePtr;
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_utils::iterators::zip_eq3;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use cairo_lang_utils::unordered_hash_map::{Entry, UnorderedHashMap};
use cairo_lang_utils::{LookupIntern, try_extract_matches};
use itertools::{Itertools, zip_eq};
use num_traits::ToPrimitive;
use semantic::corelib::unit_ty;
use semantic::items::enm::SemanticEnumEx;
use semantic::types::{peel_snapshots, wrap_in_snapshots};
use semantic::{
    ConcreteTypeId, MatchArmSelector, Pattern, PatternEnumVariant, PatternId, TypeLongId,
    ValueSelectorArm,
};

use super::block_builder::{BlockBuilder, SealedBlockBuilder};
use super::context::{
    LoweredExpr, LoweredExprExternEnum, LoweringContext, LoweringFlowError, LoweringResult,
    lowering_flow_error_to_sealed_block,
};
use super::{
    alloc_empty_block, generators, lower_expr_block, lower_expr_literal, lower_tail_expr,
    lowered_expr_to_block_scope_end, recursively_call_loop_func,
};
use crate::diagnostic::LoweringDiagnosticKind::{self, *};
use crate::diagnostic::{LoweringDiagnosticsBuilder, MatchDiagnostic, MatchError, MatchKind};
use crate::ids::{LocationId, SemanticFunctionIdEx};
use crate::lower::context::VarRequest;
use crate::lower::external::extern_facade_expr;
use crate::lower::{
    create_subscope, lower_expr, lower_single_pattern, match_extern_arm_ref_args_bind,
    match_extern_variant_arm_input_types,
};
use crate::{
    BlockEnd, MatchArm, MatchEnumInfo, MatchEnumValue, MatchExternInfo, MatchInfo, VarUsage,
    VariableId,
};

/// Information about the enum of a match statement. See [extract_concrete_enum].
struct ExtractedEnumDetails {
    concrete_enum_id: semantic::ConcreteEnumId,
    concrete_variants: Vec<semantic::ConcreteVariant>,
    n_snapshots: usize,
}

/// A wrapper enum to provide a unified interface for handling different types of match arms
/// during the lowering phase of the compiler, allowing for consistent pattern matching
/// and expression evaluation across different match-like constructs.
pub enum MatchArmWrapper<'a> {
    /// A match arm. Patterns (non-empty) guard the expression to evaluate.
    Arm(&'a [PatternId], semantic::ExprId),
    /// Else clause (no patterns) and it's expression to evaluate (if-let).
    ElseClause(semantic::ExprId),
    /// Default clause when else clause is not provided (if-let/while-let).
    DefaultClause,
}

impl<'a> From<&'a semantic::MatchArm> for MatchArmWrapper<'a> {
    fn from(arm: &'a semantic::MatchArm) -> Self {
        MatchArmWrapper::Arm(&arm.patterns, arm.expression)
    }
}

impl MatchArmWrapper<'_> {
    /// Returns the expression of the guarded by the match arm.
    pub fn expr(&self) -> Option<semantic::ExprId> {
        match self {
            MatchArmWrapper::Arm(_, expr) => Some(*expr),
            MatchArmWrapper::ElseClause(expr) => Some(*expr),
            MatchArmWrapper::DefaultClause => None,
        }
    }

    /// Returns the patterns of the match arm.
    pub fn patterns(&self) -> Option<&[PatternId]> {
        match self {
            MatchArmWrapper::Arm(patterns, _) => Some(patterns),
            MatchArmWrapper::ElseClause(_) => None,
            MatchArmWrapper::DefaultClause => None,
        }
    }

    /// Try and extract the pattern from the match arm by index.
    /// Returns None if the arm is a missing clause, else clause.
    pub fn pattern<'a>(
        &self,
        ctx: &'a LoweringContext<'_, '_>,
        index: usize,
    ) -> Option<&'a Pattern> {
        self.patterns().map(|patterns| &ctx.function_body.arenas.patterns[patterns[index]])
    }
}

/// Extracts concrete enum and variants from a match expression. Assumes it is indeed a concrete
/// enum.
fn extract_concrete_enum(
    ctx: &mut LoweringContext<'_, '_>,
    stable_ptr: SyntaxStablePtrId,
    ty: semantic::TypeId,
    match_type: MatchKind,
) -> Result<ExtractedEnumDetails, LoweringFlowError> {
    let (n_snapshots, long_ty) = peel_snapshots(ctx.db, ty);

    // Semantic model should have made sure the type is an enum.
    let TypeLongId::Concrete(ConcreteTypeId::Enum(concrete_enum_id)) = long_ty else {
        return Err(LoweringFlowError::Failed(ctx.diagnostics.report(
            stable_ptr,
            MatchError(MatchError {
                kind: match_type,
                error: MatchDiagnostic::UnsupportedMatchedType(long_ty.format(ctx.db)),
            }),
        )));
    };
    let concrete_variants =
        ctx.db.concrete_enum_variants(concrete_enum_id).map_err(LoweringFlowError::Failed)?;

    Ok(ExtractedEnumDetails { concrete_enum_id, concrete_variants, n_snapshots })
}

/// Extracts concrete enums and variants from a match expression on a tuple of enums.
fn extract_concrete_enum_tuple(
    ctx: &mut LoweringContext<'_, '_>,
    stable_ptr: SyntaxStablePtrId,
    types: &[semantic::TypeId],
    match_type: MatchKind,
) -> Result<Vec<ExtractedEnumDetails>, LoweringFlowError> {
    types
        .iter()
        .map(|ty| {
            let (n_snapshots, long_ty) = peel_snapshots(ctx.db, *ty);
            let TypeLongId::Concrete(ConcreteTypeId::Enum(concrete_enum_id)) = long_ty else {
                return Err(LoweringFlowError::Failed(ctx.diagnostics.report(
                    stable_ptr,
                    MatchError(MatchError {
                        kind: match_type,
                        error: MatchDiagnostic::UnsupportedMatchedValueTuple,
                    }),
                )));
            };
            let concrete_variants = ctx
                .db
                .concrete_enum_variants(concrete_enum_id)
                .map_err(LoweringFlowError::Failed)?;
            Ok(ExtractedEnumDetails { concrete_enum_id, concrete_variants, n_snapshots })
        })
        .collect()
}

/// The arm and pattern indices of a pattern in a match arm with an or list.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct PatternPath {
    arm_index: usize,
    pattern_index: Option<usize>,
}

/// Returns an option containing the PatternPath of the underscore pattern, if it exists.
fn get_underscore_pattern_path_and_mark_unreachable(
    ctx: &mut LoweringContext<'_, '_>,
    arms: &[MatchArmWrapper<'_>],
    match_type: MatchKind,
) -> Option<PatternPath> {
    let otherwise_variant = arms
        .iter()
        .enumerate()
        .filter_map(|(arm_index, arm)| {
            let pattern_index = match arm {
                MatchArmWrapper::Arm(patterns, _) => {
                    let position = patterns.iter().position(|pattern| {
                        matches!(
                            ctx.function_body.arenas.patterns[*pattern],
                            semantic::Pattern::Otherwise(_)
                        )
                    })?;
                    Some(position)
                }
                MatchArmWrapper::DefaultClause | MatchArmWrapper::ElseClause(_) => None,
            };
            Some(PatternPath { arm_index, pattern_index })
        })
        .next()?;

    for arm in arms.iter().skip(otherwise_variant.arm_index + 1) {
        match arm {
            MatchArmWrapper::Arm(patterns, _expr) => {
                for pattern in *patterns {
                    let pattern_ptr = ctx.function_body.arenas.patterns[*pattern].stable_ptr();
                    ctx.diagnostics.report(
                        pattern_ptr,
                        MatchError(MatchError {
                            kind: match_type,
                            error: MatchDiagnostic::UnreachableMatchArm,
                        }),
                    );
                }
            }
            MatchArmWrapper::DefaultClause => continue,
            MatchArmWrapper::ElseClause(e) => {
                let expr_ptr = ctx.function_body.arenas.exprs[*e].stable_ptr();
                ctx.diagnostics.report(
                    expr_ptr,
                    MatchError(MatchError {
                        kind: match_type,
                        error: MatchDiagnostic::UnreachableMatchArm,
                    }),
                );
            }
        }
    }

    if let Some(patterns) = arms[otherwise_variant.arm_index].patterns() {
        for pattern in patterns.iter().skip(otherwise_variant.pattern_index.unwrap_or(0) + 1) {
            let pattern = &ctx.function_body.arenas.patterns[*pattern];
            ctx.diagnostics.report(
                pattern.stable_ptr(),
                MatchError(MatchError {
                    kind: match_type,
                    error: MatchDiagnostic::UnreachableMatchArm,
                }),
            );
        }
    }

    Some(otherwise_variant)
}

/// A sparse tree that records which enum‑variants are *already*
/// covered by user code during `match` lowering.
///
/// Each node captures the coverage state for a single enum type or enum variant.
#[derive(Debug, Clone)]
enum VariantMatchTree {
    /// The current variant is itself an enum; a `Vec` entry is kept for
    /// every child variant match tree.
    Mapping(Vec<VariantMatchTree>),
    /// A pattern fully covers this enum type/variant. Additional patterns
    /// reaching here are unreachable (even if current variant is itself an enum, subtrees are
    /// irrelevant).
    Full(PatternPath),
    /// No pattern has covered this enum type/variant. Useful to emit a `MissingMatchArm` diagnostic
    /// later on.
    Empty,
}

impl VariantMatchTree {
    /// Pushes a pattern to the enum paths. Fails if the pattern is unreachable.
    fn push_pattern_path(
        &mut self,
        match_type: MatchKind,
        pattern_path: PatternPath,
    ) -> Result<(), LoweringDiagnosticKind> {
        match self {
            VariantMatchTree::Empty => {
                *self = VariantMatchTree::Full(pattern_path);
                Ok(())
            }
            VariantMatchTree::Full(_) => Err(MatchError(MatchError {
                kind: match_type,
                error: MatchDiagnostic::UnreachableMatchArm,
            })),
            VariantMatchTree::Mapping(mapping) => {
                // Need at least one empty path, but should write to all (pattern covers multiple
                // paths).
                let mut any_ok = false;
                for path in mapping.iter_mut() {
                    if path.push_pattern_path(match_type, pattern_path).is_ok() {
                        any_ok = true;
                    }
                }
                if any_ok {
                    Ok(())
                } else {
                    Err(MatchError(MatchError {
                        kind: match_type,
                        error: MatchDiagnostic::UnreachableMatchArm,
                    }))
                }
            }
        }
    }

    /// Utility to collect every [`PatternPath`] found in `Full` leaves into `leaves`.
    fn collect_leaves(&self, leaves: &mut OrderedHashSet<PatternPath>) {
        match self {
            VariantMatchTree::Empty => {}
            VariantMatchTree::Full(pattern_path) => {
                leaves.insert(*pattern_path);
            }
            VariantMatchTree::Mapping(mapping) => {
                for path in mapping {
                    path.collect_leaves(leaves);
                }
            }
        }
    }

    /// Fails on missing enum in db.
    /// Returns None if path is full otherwise reference the [VariantMatchTree] of appropriate
    /// variant.
    fn get_mapping_or_insert<'a>(
        &'a mut self,
        ctx: &LoweringContext<'_, '_>,
        concrete_variant: ConcreteVariant,
    ) -> cairo_lang_diagnostics::Maybe<Option<&'a mut Self>> {
        match self {
            VariantMatchTree::Empty => {
                let variant_count =
                    ctx.db.concrete_enum_variants(concrete_variant.concrete_enum_id)?.len();
                *self = VariantMatchTree::Mapping(vec![VariantMatchTree::Empty; variant_count]);
                if let VariantMatchTree::Mapping(items) = self {
                    Ok(Some(&mut items[concrete_variant.idx]))
                } else {
                    unreachable!("We just created the mapping.")
                }
            }
            VariantMatchTree::Full(_) => Ok(None),
            VariantMatchTree::Mapping(items) => Ok(Some(&mut items[concrete_variant.idx])),
        }
    }
}

/// Represents a path in a match tree.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
struct MatchingPath {
    /// The variants per member of the tuple matched until this point.
    variants: Vec<semantic::ConcreteVariant>,
}

/// A helper function for [get_variants_to_arm_map_tuple] Inserts the pattern path to the map for
/// each variants list it can match.
fn insert_tuple_path_patterns(
    ctx: &mut LoweringContext<'_, '_>,
    patterns: &[PatternId],
    pattern_path: &PatternPath,
    extracted_enums_details: &[ExtractedEnumDetails],
    mut path: MatchingPath,
    map: &mut UnorderedHashMap<MatchingPath, PatternPath>,
    match_type: MatchKind,
) -> LoweringResult<()> {
    let index = path.variants.len();

    // if the path is the same length as the tuple's patterns, we have reached the end of the path
    if index == patterns.len() {
        match map.entry(path) {
            Entry::Occupied(_) => {}
            Entry::Vacant(entry) => {
                entry.insert(*pattern_path);
            }
        };
        return Ok(());
    }

    let pattern = ctx.function_body.arenas.patterns[patterns[index]].clone();

    match pattern {
        Pattern::EnumVariant(enum_pattern) => {
            if enum_pattern.variant.concrete_enum_id
                != extracted_enums_details[index].concrete_enum_id
            {
                return Err(LoweringFlowError::Failed(ctx.diagnostics.report(
                    enum_pattern.stable_ptr.untyped(),
                    MatchError(MatchError {
                        kind: match_type,
                        error: MatchDiagnostic::UnsupportedMatchArmNotAVariant,
                    }),
                )));
            }
            path.variants.push(enum_pattern.variant);
            insert_tuple_path_patterns(
                ctx,
                patterns,
                pattern_path,
                extracted_enums_details,
                path,
                map,
                match_type,
            )
        }
        Pattern::Otherwise(_) => {
            extracted_enums_details[index].concrete_variants.iter().try_for_each(|variant| {
                // TODO(TomerStarkware): Remove the match on the variant options in this case if
                // there's no other conflicting arm.
                let mut path = path.clone();
                path.variants.push(variant.clone());
                insert_tuple_path_patterns(
                    ctx,
                    patterns,
                    pattern_path,
                    extracted_enums_details,
                    path,
                    map,
                    match_type,
                )
            })
        }
        _ => Err(LoweringFlowError::Failed(ctx.diagnostics.report(
            &pattern,
            MatchError(MatchError {
                kind: match_type,
                error: MatchDiagnostic::UnsupportedMatchArmNotAVariant,
            }),
        ))),
    }
}

/// Returns a map from a matching paths to their corresponding pattern path in a match statement.
fn get_variants_to_arm_map_tuple<'a>(
    ctx: &mut LoweringContext<'_, '_>,
    arms: impl Iterator<Item = &'a MatchArmWrapper<'a>>,
    extracted_enums_details: &[ExtractedEnumDetails],
    match_type: MatchKind,
) -> LoweringResult<UnorderedHashMap<MatchingPath, PatternPath>> {
    let mut map = UnorderedHashMap::default();
    for (arm_index, arm) in arms.enumerate() {
        let Some(patterns) = arm.patterns() else {
            continue;
        };
        for (pattern_index, pattern) in patterns.iter().enumerate() {
            let pattern = ctx.function_body.arenas.patterns[*pattern].clone();
            if let semantic::Pattern::Otherwise(_) = pattern {
                break;
            }
            let patterns =
                try_extract_matches!(&pattern, semantic::Pattern::Tuple).ok_or_else(|| {
                    LoweringFlowError::Failed(ctx.diagnostics.report(
                        &pattern,
                        MatchError(MatchError {
                            kind: match_type,
                            error: MatchDiagnostic::UnsupportedMatchArmNotAVariant,
                        }),
                    ))
                })?;

            let map_size = map.len();
            insert_tuple_path_patterns(
                ctx,
                &patterns.field_patterns,
                &PatternPath { arm_index, pattern_index: Some(pattern_index) },
                extracted_enums_details,
                MatchingPath::default(),
                &mut map,
                match_type,
            )?;
            if map.len() == map_size {
                ctx.diagnostics.report(
                    &pattern,
                    MatchError(MatchError {
                        kind: match_type,
                        error: MatchDiagnostic::UnreachableMatchArm,
                    }),
                );
            }
        }
    }
    Ok(map)
}

/// Information needed to lower a match on tuple expression.
struct LoweringMatchTupleContext {
    /// The location of the match expression.
    match_location: LocationId,
    /// The index of the underscore pattern, if it exists.
    otherwise_variant: Option<PatternPath>,
    /// A map from variants vector to their corresponding pattern path.
    variants_map: UnorderedHashMap<MatchingPath, PatternPath>,
    /// The tuple's destructured inputs.
    match_inputs: Vec<VarUsage>,
    /// The number of snapshots of the tuple.
    n_snapshots_outer: usize,
    /// The current variants path.
    current_path: MatchingPath,
    /// The current variants' variable ids.
    current_var_ids: Vec<VariableId>,
}

/// Lowers the arm of a match on a tuple expression.
fn lower_tuple_match_arm(
    ctx: &mut LoweringContext<'_, '_>,
    mut builder: BlockBuilder,
    arms: &[MatchArmWrapper<'_>],
    match_tuple_ctx: &mut LoweringMatchTupleContext,
    leaves_builders: &mut Vec<MatchLeafBuilder>,
    match_type: MatchKind,
) -> LoweringResult<()> {
    let pattern_path = match_tuple_ctx
        .variants_map
        .get(&match_tuple_ctx.current_path)
        .or(match_tuple_ctx.otherwise_variant.as_ref())
        .ok_or_else(|| {
            let variants_string = format!(
                "({})",
                match_tuple_ctx
                    .current_path
                    .variants
                    .iter()
                    .map(|variant| variant.id.name(ctx.db))
                    .join(", ")
            );
            report_missing_arm_error(
                ctx,
                match_tuple_ctx.match_location,
                match_type,
                variants_string,
            )
        })?;
    let pattern = pattern_path.pattern_index.map(|i| {
        arms[pattern_path.arm_index]
            .pattern(ctx, i)
            .expect("Pattern previously found in arm, but is now missing at index.")
            .clone()
    });
    let lowering_inner_pattern_result = match pattern {
        Some(semantic::Pattern::Tuple(patterns)) => patterns
            .field_patterns
            .iter()
            .enumerate()
            .map(|(index, pattern)| {
                let pattern = &ctx.function_body.arenas.patterns[*pattern];
                match pattern {
                    Pattern::EnumVariant(PatternEnumVariant {
                        inner_pattern: Some(inner_pattern),
                        ..
                    }) => {
                        let inner_pattern = *inner_pattern;
                        let pattern_location = ctx.get_location(
                            ctx.function_body.arenas.patterns[inner_pattern].stable_ptr().untyped(),
                        );

                        let variant_expr = LoweredExpr::AtVariable(VarUsage {
                            var_id: match_tuple_ctx.current_var_ids[index],
                            location: pattern_location,
                        });

                        lower_single_pattern(ctx, &mut builder, inner_pattern, variant_expr)
                    }
                    Pattern::EnumVariant(PatternEnumVariant { inner_pattern: None, .. })
                    | Pattern::Otherwise(_) => Ok(()),
                    _ => unreachable!(
                        "function `get_variant_to_arm_map` should have reported every other \
                         pattern type"
                    ),
                }
            })
            .collect::<LoweringResult<Vec<_>>>()
            .map(|_| ()),
        Some(semantic::Pattern::Otherwise(_)) | None => Ok(()),
        _ => {
            let stable_ptr = pattern.unwrap().stable_ptr();
            return Err(LoweringFlowError::Failed(ctx.diagnostics.report(
                stable_ptr,
                MatchError(MatchError {
                    kind: match_type,
                    error: MatchDiagnostic::UnsupportedMatchArmNotATuple,
                }),
            )));
        }
    };
    leaves_builders.push(MatchLeafBuilder {
        builder,
        arm_index: pattern_path.arm_index,
        lowering_result: lowering_inner_pattern_result,
    });
    Ok(())
}

/// Lowers a full decision tree for a match on a tuple expression.
fn lower_full_match_tree(
    ctx: &mut LoweringContext<'_, '_>,
    builder: &mut BlockBuilder,
    arms: &[MatchArmWrapper<'_>],
    match_tuple_ctx: &mut LoweringMatchTupleContext,
    extracted_enums_details: &[ExtractedEnumDetails],
    leaves_builders: &mut Vec<MatchLeafBuilder>,
    match_type: MatchKind,
) -> LoweringResult<MatchInfo> {
    // Always 0 for initial call as this is default
    let index = match_tuple_ctx.current_path.variants.len();
    let mut arm_var_ids = vec![];
    let block_ids = extracted_enums_details[index]
        .concrete_variants
        .iter()
        .map(|concrete_variant| {
            let mut subscope = create_subscope(ctx, builder);
            let block_id = subscope.block_id;
            let var_id = ctx.new_var(VarRequest {
                ty: wrap_in_snapshots(
                    ctx.db,
                    concrete_variant.ty,
                    extracted_enums_details[index].n_snapshots + match_tuple_ctx.n_snapshots_outer,
                ),
                location: match_tuple_ctx.match_location,
            });
            arm_var_ids.push(vec![var_id]);

            match_tuple_ctx.current_path.variants.push(concrete_variant.clone());
            match_tuple_ctx.current_var_ids.push(var_id);
            let result = if index + 1 == extracted_enums_details.len() {
                lower_tuple_match_arm(
                    ctx,
                    subscope,
                    arms,
                    match_tuple_ctx,
                    leaves_builders,
                    match_type,
                )
            } else {
                lower_full_match_tree(
                    ctx,
                    &mut subscope,
                    arms,
                    match_tuple_ctx,
                    extracted_enums_details,
                    leaves_builders,
                    match_type,
                )
                .map(|match_info| {
                    subscope.finalize(ctx, BlockEnd::Match { info: match_info });
                })
            }
            .map(|_| block_id);
            match_tuple_ctx.current_path.variants.pop();
            match_tuple_ctx.current_var_ids.pop();
            result
        })
        .collect::<Vec<_>>()
        .into_iter()
        .collect::<LoweringResult<Vec<_>>>()?;
    let match_info = MatchInfo::Enum(MatchEnumInfo {
        concrete_enum_id: extracted_enums_details[index].concrete_enum_id,
        input: match_tuple_ctx.match_inputs[index],
        arms: zip_eq(
            zip_eq(&extracted_enums_details[index].concrete_variants, block_ids),
            arm_var_ids,
        )
        .map(|((variant_id, block_id), var_ids)| MatchArm {
            arm_selector: MatchArmSelector::VariantId(variant_id.clone()),
            block_id,
            var_ids,
        })
        .collect(),
        location: match_tuple_ctx.match_location,
    });
    Ok(match_info)
}

/// The types and number of snapshots of a tuple expression in a match statement.
pub struct TupleInfo {
    pub n_snapshots: usize,
    pub types: Vec<semantic::TypeId>,
}

/// Lowers an expression of type [semantic::ExprMatch] where the matched expression is a tuple of
/// enums.
pub(crate) fn lower_expr_match_tuple(
    ctx: &mut LoweringContext<'_, '_>,
    builder: &mut BlockBuilder,
    expr: LoweredExpr,
    matched_expr: semantic::ExprId,
    tuple_info: &TupleInfo,
    arms: &[MatchArmWrapper<'_>],
    match_type: MatchKind,
) -> LoweringResult<LoweredExpr> {
    let location = expr.location();
    let match_inputs_exprs = if let LoweredExpr::Tuple { exprs, .. } = expr {
        exprs
    } else {
        let reqs = tuple_info
            .types
            .iter()
            .map(|ty| VarRequest {
                ty: wrap_in_snapshots(ctx.db, *ty, tuple_info.n_snapshots),
                location,
            })
            .collect();
        generators::StructDestructure { input: expr.as_var_usage(ctx, builder)?, var_reqs: reqs }
            .add(ctx, &mut builder.statements)
            .into_iter()
            .map(|var_id| {
                LoweredExpr::AtVariable(VarUsage {
                    var_id,
                    // The variable is used immediately after the destructure, so the usage
                    // location is the same as the definition location.
                    location: ctx.variables[var_id].location,
                })
            })
            .collect()
    };

    let match_inputs = match_inputs_exprs
        .into_iter()
        .map(|expr| expr.as_var_usage(ctx, builder))
        .collect::<LoweringResult<Vec<_>>>()?;
    let extracted_enums_details = extract_concrete_enum_tuple(
        ctx,
        ctx.function_body.arenas.exprs[matched_expr].stable_ptr().untyped(),
        &tuple_info.types,
        match_type,
    )?;

    let otherwise_variant = get_underscore_pattern_path_and_mark_unreachable(ctx, arms, match_type);

    let variants_map = get_variants_to_arm_map_tuple(
        ctx,
        arms.iter().take(
            otherwise_variant
                .as_ref()
                .map(|PatternPath { arm_index, .. }| *arm_index)
                .unwrap_or(arms.len()),
        ),
        extracted_enums_details.as_slice(),
        match_type,
    )?;

    let mut arms_vec = vec![];
    let mut match_tuple_ctx = LoweringMatchTupleContext {
        match_location: location,
        otherwise_variant,
        variants_map,
        match_inputs,
        n_snapshots_outer: tuple_info.n_snapshots,
        current_path: MatchingPath::default(),
        current_var_ids: vec![],
    };
    let match_info = lower_full_match_tree(
        ctx,
        builder,
        arms,
        &mut match_tuple_ctx,
        &extracted_enums_details,
        &mut arms_vec,
        match_type,
    )?;
    let empty_match_info = MatchInfo::Enum(MatchEnumInfo {
        concrete_enum_id: extracted_enums_details[0].concrete_enum_id,
        input: match_tuple_ctx.match_inputs[0],
        arms: vec![],
        location,
    });
    let sealed_blocks =
        group_match_arms(ctx, &empty_match_info, location, arms, arms_vec, match_type)?;

    builder.merge_and_end_with_match(ctx, match_info, sealed_blocks, location)
}

/// Lowers an expression of type [semantic::ExprMatch].
pub(crate) fn lower_expr_match(
    ctx: &mut LoweringContext<'_, '_>,
    expr: &semantic::ExprMatch,
    builder: &mut BlockBuilder,
) -> LoweringResult<LoweredExpr> {
    log::trace!("Lowering a match expression: {:?}", expr.debug(&ctx.expr_formatter));
    let location = ctx.get_location(expr.stable_ptr.untyped());
    let lowered_expr = lower_expr(ctx, builder, expr.matched_expr)?;

    let ty = ctx.function_body.arenas.exprs[expr.matched_expr].ty();

    if corelib::numeric_upcastable_to_felt252(ctx.db, ty) {
        let match_input = lowered_expr.as_var_usage(ctx, builder)?;
        return lower_expr_match_value(ctx, expr, match_input, builder);
    }

    let arms = expr.arms.iter().map(|arm| arm.into()).collect_vec();

    lower_match_arms(
        ctx,
        builder,
        expr.matched_expr,
        lowered_expr,
        arms,
        location,
        MatchKind::Match,
    )
}

/// Lower the collected match arms according to the matched expression.
/// To be used in multi pattern matching scenarios (if let/while let/match).
pub(crate) fn lower_match_arms(
    ctx: &mut LoweringContext<'_, '_>,
    builder: &mut BlockBuilder,
    matched_expr: semantic::ExprId,
    lowered_expr: LoweredExpr,
    arms: Vec<MatchArmWrapper<'_>>,
    location: LocationId,
    match_type: MatchKind,
) -> Result<LoweredExpr, LoweringFlowError> {
    let ty = ctx.function_body.arenas.exprs[matched_expr].ty();

    let (n_snapshots, long_type_id) = peel_snapshots(ctx.db, ty);

    if let Some(types) = try_extract_matches!(long_type_id, TypeLongId::Tuple) {
        return lower_expr_match_tuple(
            ctx,
            builder,
            lowered_expr,
            matched_expr,
            &TupleInfo { n_snapshots, types },
            &arms,
            match_type,
        );
    }

    // TODO(spapini): Use diagnostics.
    // TODO(spapini): Handle more than just enums.
    if let LoweredExpr::ExternEnum(extern_enum) = lowered_expr {
        return lower_optimized_extern_match(ctx, builder, extern_enum, &arms, match_type);
    }

    lower_concrete_enum_match(ctx, builder, matched_expr, lowered_expr, &arms, location, match_type)
}

/// Lowers a match expression on a concrete enum.
/// This function is used for match expressions on concrete enums, such as
/// `match x { A => 1, B => 2 }` and in if/while let.
pub(crate) fn lower_concrete_enum_match(
    ctx: &mut LoweringContext<'_, '_>,
    builder: &mut BlockBuilder,
    matched_expr: semantic::ExprId,
    lowered_matched_expr: LoweredExpr,
    arms: &[MatchArmWrapper<'_>],
    location: LocationId,
    match_type: MatchKind,
) -> LoweringResult<LoweredExpr> {
    let matched_expr = &ctx.function_body.arenas.exprs[matched_expr];
    let ExtractedEnumDetails { concrete_enum_id, concrete_variants, n_snapshots } =
        extract_concrete_enum(ctx, matched_expr.into(), matched_expr.ty(), match_type)?;

    // TODO(eytan-starkware): Have all the concrete variants down to the lowest level.
    let match_input = lowered_matched_expr.as_var_usage(ctx, builder)?;

    // Merge arm blocks.

    let empty_match_info = MatchInfo::Enum(MatchEnumInfo {
        concrete_enum_id,
        input: match_input,
        arms: vec![],
        location,
    });
    let mut builder_context = MatchArmsLoweringContext::new(builder, arms, empty_match_info);

    let pattern_builder =
        ConcreteEnumVariantPatternBuilder { n_snapshots, default_location: location };

    let variants_block_builders = pattern_builder.build_enum_block_variants(
        ctx,
        &mut builder_context,
        location,
        match_type,
        &concrete_variants,
        concrete_enum_id,
    )?;

    let sealed_blocks = group_match_arms(
        ctx,
        &builder_context.empty_match_info,
        location,
        arms,
        variants_block_builders,
        match_type,
    )?;

    let match_info = MatchInfo::Enum(MatchEnumInfo {
        concrete_enum_id,
        input: match_input,
        arms: builder_context.collect_match_arms(&concrete_variants),
        location,
    });
    builder.merge_and_end_with_match(ctx, match_info, sealed_blocks, location)
}

/// Lowers a match expression on a LoweredExpr::ExternEnum lowered expression.
pub(crate) fn lower_optimized_extern_match(
    ctx: &mut LoweringContext<'_, '_>,
    builder: &mut BlockBuilder,
    extern_enum: LoweredExprExternEnum,
    match_arms: &[MatchArmWrapper<'_>],
    match_type: MatchKind,
) -> LoweringResult<LoweredExpr> {
    log::trace!("Started lowering of an optimized extern match.");
    let location = extern_enum.location;
    let concrete_variants = ctx
        .db
        .concrete_enum_variants(extern_enum.concrete_enum_id)
        .map_err(LoweringFlowError::Failed)?;

    // Merge arm blocks.

    let empty_match_info = MatchInfo::Extern(MatchExternInfo {
        function: extern_enum.function.lowered(ctx.db),
        inputs: vec![],
        arms: vec![],
        location,
    });
    let mut builder_context = MatchArmsLoweringContext::new(builder, match_arms, empty_match_info);

    let pattern_builder = ExternEnumVariantPatternBuilder { extern_enum: extern_enum.clone() };

    let variant_block_builders = pattern_builder.build_enum_block_variants(
        ctx,
        &mut builder_context,
        location,
        match_type,
        &concrete_variants,
        extern_enum.concrete_enum_id,
    )?;

    let sealed_blocks = group_match_arms(
        ctx,
        &builder_context.empty_match_info,
        location,
        match_arms,
        variant_block_builders,
        match_type,
    )?;

    let match_info = MatchInfo::Extern(MatchExternInfo {
        function: extern_enum.function.lowered(ctx.db),
        inputs: extern_enum.inputs,
        arms: builder_context.collect_match_arms(&concrete_variants),
        location,
    });
    builder.merge_and_end_with_match(ctx, match_info, sealed_blocks, location)
}

/// Common trait for preparing per-variant lowering scopes, regardless of
/// whether the enum is *concrete* or *extern*. This trait contains common
/// code for preparing each pattern for lowering, while assuming it is
/// an [Pattern::EnumVariant] or [Pattern::Otherwise].
/// An internal pattern, if exists, is passed to
/// `lower_concrete_enum_variant`.
///
/// Implementors are responsible for implementing `lower_concrete_enum_variant`.
///
/// Call-site entry point: `build_enum_block_variants`.
trait EnumVariantScopeBuilder {
    /// Lowers a concrete enum variant pattern. This should:
    /// * Allocate the arm-introduced variables for each variant.
    /// * Bind those variables to the semantic model.
    fn lower_concrete_enum_variant(
        &self,
        ctx: &mut LoweringContext<'_, '_>,
        builder_context: &mut MatchArmsLoweringContext<'_>,
        subscope: &mut BlockBuilder,
        concrete_variant: &ConcreteVariant,
        inner_pattern: Option<PatternId>,
    ) -> Result<(), LoweringFlowError>;

    /// Orchestrates the lowering of the enum variants.
    /// This function is responsible for traversing the variant match tree
    /// and calling `prepare_and_lower_concrete_variant` for each variant.
    fn build_enum_block_variants(
        &self,
        ctx: &mut LoweringContext<'_, '_>,
        builder_context: &mut MatchArmsLoweringContext<'_>,
        location: LocationId,
        match_type: MatchKind,
        concrete_variants: &[ConcreteVariant],
        concrete_enum_id: semantic::ConcreteEnumId,
    ) -> Result<Vec<MatchLeafBuilder>, LoweringFlowError> {
        // Collect for all patterns into a `VariantMatchTree` to reason about patterns per variant.
        let variant_match_tree = Self::build_variant_match_tree(
            ctx,
            builder_context.arms,
            concrete_enum_id,
            match_type,
        )?;

        match variant_match_tree {
            VariantMatchTree::Empty => concrete_variants
                .iter()
                .map(|concrete_variant| {
                    Err(report_missing_variant_error(ctx, location, match_type, concrete_variant))
                })
                .collect_vec(),
            VariantMatchTree::Full(pattern_path) => {
                // TODO(eytan-starkware) We can actually probably drop the whole match expression
                // after dealing with variable binding, as this case means
                // everything is covered by one pattern.
                // It will probably just happen as part of the multilevel change.
                concrete_variants
                    .iter()
                    .map(|concrete_variant| {
                        self.prepare_and_lower_concrete_variant(
                            ctx,
                            builder_context,
                            concrete_variant,
                            pattern_path,
                        )
                    })
                    .collect_vec()
            }
            VariantMatchTree::Mapping(items) => {
                let mut res = vec![];
                for (variant_idx, subtree) in items.into_iter().enumerate() {
                    match subtree {
                        VariantMatchTree::Mapping(_) => {
                            // Bad pattern we don't support inner enums. The lowering will fail in
                            // `lower_single_pattern`.
                            // TODO(eytan-starkware) Remove these comments when multilevel enum is
                            // done.
                            let mut leaves: OrderedHashSet<_> = Default::default();
                            subtree.collect_leaves(&mut leaves);
                            for pattern_path in leaves {
                                // Error will be generated during lowering right now
                                res.push(self.prepare_and_lower_concrete_variant(
                                    ctx,
                                    builder_context,
                                    &concrete_variants[variant_idx],
                                    pattern_path,
                                ));
                            }
                        }
                        VariantMatchTree::Empty => res.push(Err(report_missing_variant_error(
                            ctx,
                            location,
                            match_type,
                            &concrete_variants[variant_idx],
                        ))),
                        VariantMatchTree::Full(pattern_path) => {
                            // Prepare to lower a single variant
                            res.push(self.prepare_and_lower_concrete_variant(
                                ctx,
                                builder_context,
                                &concrete_variants[variant_idx],
                                pattern_path,
                            ));
                        }
                    }
                }
                res
            }
        }
        .into_iter()
        .collect::<LoweringResult<Vec<_>>>()
    }

    /// Builds a variant match tree from the arms of a match statement.
    /// The tree is used to check for unreachable patterns and represent for each variant it's
    /// corresponding matching pattern.
    fn build_variant_match_tree(
        ctx: &mut LoweringContext<'_, '_>,
        arms: &[MatchArmWrapper<'_>],
        concrete_enum_id: semantic::ConcreteEnumId,
        match_type: MatchKind,
    ) -> LoweringResult<VariantMatchTree> {
        // An Enum might contain nested Enum variants. A good example using either is:
        // ```
        // Either<Either<felt252, Option<felt252>>, Either<Option<Array<felt252>>, Felt252Dict>>
        // ```
        // If we draw it as a tree we can see different branches have different types and depths:
        // ```
        // Either
        // ├── Either
        // │   ├── felt252
        // │   └── Option
        // │       ├── Some<felt252>
        // │       └── None
        // └── Either
        //     ├── Option
        //     |   └── Some<Array<felt252>> <---- Not an enum so no branching
        //     │   └── None
        //     └── Felt252Dict
        // ```
        // This can be generalized to a tree where each enum type intoduces one branch per variant.
        // We use [VariantMatchTree] to check patterns are legal (reachable and all branches end
        // with a pattern).
        let mut variant_match_tree = VariantMatchTree::Empty;
        for (arm_index, arm) in arms.iter().enumerate() {
            let patterns = match arm {
                MatchArmWrapper::DefaultClause => {
                    let _ = variant_match_tree.push_pattern_path(
                        match_type,
                        PatternPath { arm_index, pattern_index: None },
                    );
                    continue;
                }
                MatchArmWrapper::ElseClause(e) => {
                    let ptr = ctx.function_body.arenas.exprs[*e].stable_ptr();
                    try_push(
                        ctx,
                        match_type,
                        ptr,
                        PatternPath { arm_index, pattern_index: None },
                        &mut variant_match_tree,
                    );
                    continue;
                }
                MatchArmWrapper::Arm(patterns, _) => patterns,
            };
            for (pattern_index, pattern) in patterns.iter().copied().enumerate() {
                let pattern_path = PatternPath { arm_index, pattern_index: Some(pattern_index) };
                let pattern_ptr = ctx.function_body.arenas.patterns[pattern].stable_ptr();

                let variant_match_tree = &mut variant_match_tree;
                if !(matches_enum(ctx, pattern) | matches_other(ctx, pattern)) {
                    return Err(LoweringFlowError::Failed(ctx.diagnostics.report(
                        pattern_ptr,
                        MatchError(MatchError {
                            kind: match_type,
                            error: MatchDiagnostic::UnsupportedMatchArmNotAVariant,
                        }),
                    )));
                }
                unfold_pattern_and_push_to_tree(
                    ctx,
                    match_type,
                    pattern,
                    pattern_path,
                    variant_match_tree,
                    concrete_enum_id,
                )?;
            }
        }

        /// Recursively unfolds a pattern and pushes it to a variant match tree.
        /// This function handles nested enum patterns by traversing the enum patterns and updating
        /// the variant match tree accordingly.
        /// The function handles three main cases:
        /// * Otherwise (_) patterns: fills all leaves in the tree
        /// * Enum variant patterns: recursively processes nested patterns if they exist
        /// * Other patterns: reports errors for unsupported pattern types
        ///
        /// # Returns
        /// * `Ok(())` if the pattern was successfully unfolded and pushed to the tree.
        /// * `Err(LoweringFlowError)` if an error occurred during processing (e.g., mismatched enum
        ///   types, or unsupported pattern types).
        fn unfold_pattern_and_push_to_tree(
            ctx: &mut LoweringContext<'_, '_>,
            match_type: MatchKind,
            mut pattern: PatternId,
            pattern_path: PatternPath,
            mut variant_match_tree: &mut VariantMatchTree,
            mut concrete_enum_id: semantic::ConcreteEnumId,
        ) -> Result<(), LoweringFlowError> {
            let pattern_ptr = ctx.function_body.arenas.patterns[pattern].stable_ptr();
            loop {
                match &ctx.function_body.arenas.patterns[pattern] {
                    semantic::Pattern::Otherwise(_) => {
                        // Fill leaves and check for usefulness.
                        try_push(ctx, match_type, pattern_ptr, pattern_path, variant_match_tree);
                        break;
                    }
                    semantic::Pattern::EnumVariant(enum_pattern) => {
                        if concrete_enum_id != enum_pattern.variant.concrete_enum_id {
                            return Err(LoweringFlowError::Failed(ctx.diagnostics.report(
                                pattern_ptr,
                                MatchError(MatchError {
                                    kind: match_type,
                                    error: MatchDiagnostic::UnsupportedMatchArmNotAVariant,
                                }),
                            )));
                        }
                        // Expand paths in map to include all variants of this enum_pattern.
                        if let Some(vmap) = variant_match_tree
                            .get_mapping_or_insert(ctx, enum_pattern.variant.clone())
                            .map_err(LoweringFlowError::Failed)?
                        {
                            variant_match_tree = vmap;
                        } else {
                            ctx.diagnostics.report(
                                pattern_ptr,
                                MatchError(MatchError {
                                    kind: match_type,
                                    error: MatchDiagnostic::UnreachableMatchArm,
                                }),
                            );
                            break;
                        }

                        if let Some(inner_pattern) = enum_pattern.inner_pattern {
                            if !matches_enum(ctx, inner_pattern) {
                                try_push(
                                    ctx,
                                    match_type,
                                    pattern_ptr,
                                    pattern_path,
                                    variant_match_tree,
                                );
                                break;
                            }

                            let ptr = ctx.function_body.arenas.patterns[inner_pattern].stable_ptr();
                            let variant = &ctx
                                .db
                                .concrete_enum_variants(concrete_enum_id)
                                .map_err(LoweringFlowError::Failed)?[enum_pattern.variant.idx];
                            let next_enum =
                                extract_concrete_enum(ctx, ptr.into(), variant.ty, match_type);
                            concrete_enum_id = next_enum?.concrete_enum_id;

                            pattern = inner_pattern;
                        } else {
                            try_push(
                                ctx,
                                match_type,
                                pattern_ptr,
                                pattern_path,
                                variant_match_tree,
                            );
                            break;
                        }
                    }
                    _ => {
                        break;
                    }
                }
            }
            Ok(())
        }

        /// This function attempts to push a pattern onto the [VariantMatchTree] This will fill the
        /// appropriate subtrees as covered (i.e. full). If the pattern is unreachable (i.e.,
        /// the enum variant it represents is already covered), it reports it. Assumes push can
        /// only fail on unreachable patterns.
        fn try_push(
            ctx: &mut LoweringContext<'_, '_>,
            match_type: MatchKind,
            stable_ptr: impl Into<SyntaxStablePtrId>,
            pattern_path: PatternPath,
            variant_map: &mut VariantMatchTree,
        ) {
            let _ = variant_map
                .push_pattern_path(match_type, pattern_path)
                .map_err(|e| ctx.diagnostics.report(stable_ptr, e));
        }

        /// Checks if a pattern matches an enum variant.
        fn matches_enum(ctx: &LoweringContext<'_, '_>, pattern: PatternId) -> bool {
            matches!(ctx.function_body.arenas.patterns[pattern], semantic::Pattern::EnumVariant(_))
        }

        /// Checks if a pattern matches `otherwise` or a variable.
        fn matches_other(ctx: &LoweringContext<'_, '_>, pattern: PatternId) -> bool {
            matches!(
                ctx.function_body.arenas.patterns[pattern],
                semantic::Pattern::Otherwise(_) | semantic::Pattern::Variable(_)
            )
        }

        Ok(variant_match_tree)
    }

    /// Prepares the lowering of a single match arm leaf.
    /// This function is responsible for:
    /// * Creating a subscope for the arm.
    /// * Extracting the inner pattern from the arm.
    /// * Calling `lower_concrete_enum_variant` to lower the arm.
    /// * Returning a `MatchLeafBuilder` that describes the arm.
    fn prepare_and_lower_concrete_variant(
        &self,
        ctx: &mut LoweringContext<'_, '_>,
        builder_context: &mut MatchArmsLoweringContext<'_>,
        concrete_variant: &ConcreteVariant,
        pattern_path: PatternPath,
    ) -> Result<MatchLeafBuilder, LoweringFlowError> {
        let mut subscope = builder_context.add_scope(ctx);
        let pattern = builder_context.pattern(ctx, pattern_path);

        let lowering_inner_pattern_result = match pattern {
            Some(Pattern::EnumVariant(PatternEnumVariant {
                inner_pattern: Some(inner_pattern),
                ..
            })) => {
                let inner_pattern = *inner_pattern;
                self.lower_concrete_enum_variant(
                    ctx,
                    builder_context,
                    &mut subscope,
                    concrete_variant,
                    Some(inner_pattern),
                )
            }
            Some(
                Pattern::EnumVariant(PatternEnumVariant { inner_pattern: None, .. })
                | Pattern::Otherwise(_),
            )
            | None => self.lower_concrete_enum_variant(
                ctx,
                builder_context,
                &mut subscope,
                concrete_variant,
                None,
            ),
            _ => unreachable!(
                "function `get_variant_to_arm_map` should have reported every other pattern type"
            ),
        };

        Ok(MatchLeafBuilder {
            arm_index: pattern_path.arm_index,
            lowering_result: lowering_inner_pattern_result,
            builder: subscope,
        })
    }
}

/// Implements the [EnumVariantScopeBuilder] trait for external enum variants.
/// This struct is used to prepare for lowering match arms for external enums.
struct ExternEnumVariantPatternBuilder {
    extern_enum: LoweredExprExternEnum,
}

impl EnumVariantScopeBuilder for ExternEnumVariantPatternBuilder {
    fn lower_concrete_enum_variant(
        &self,
        ctx: &mut LoweringContext<'_, '_>,
        builder_context: &mut MatchArmsLoweringContext<'_>,
        subscope: &mut BlockBuilder,
        concrete_variant: &ConcreteVariant,
        inner_pattern: Option<PatternId>,
    ) -> Result<(), LoweringFlowError> {
        let location = self.extern_enum.location;
        let input_tys =
            match_extern_variant_arm_input_types(ctx, concrete_variant.ty, &self.extern_enum);
        let mut input_vars =
            input_tys.into_iter().map(|ty| ctx.new_var(VarRequest { ty, location })).collect_vec();
        builder_context.arm_var_ids.push(input_vars.clone());
        // Bind the arm inputs to implicits and semantic variables.
        match_extern_arm_ref_args_bind(ctx, &mut input_vars, &self.extern_enum, subscope);

        let variant_expr = extern_facade_expr(ctx, concrete_variant.ty, input_vars, location);
        match inner_pattern {
            Some(inner_pattern) => lower_single_pattern(ctx, subscope, inner_pattern, variant_expr),
            None => Ok(()),
        }
    }
}

/// Implements the [EnumVariantScopeBuilder] trait for concrete enum variants.
/// This struct is used to prepare for lowering match arms for concrete enums.
struct ConcreteEnumVariantPatternBuilder {
    n_snapshots: usize,
    default_location: LocationId,
}

impl EnumVariantScopeBuilder for ConcreteEnumVariantPatternBuilder {
    fn lower_concrete_enum_variant(
        &self,
        ctx: &mut LoweringContext<'_, '_>,
        builder_context: &mut MatchArmsLoweringContext<'_>,
        subscope: &mut BlockBuilder,
        concrete_variant: &ConcreteVariant,
        inner_pattern: Option<PatternId>,
    ) -> Result<(), LoweringFlowError> {
        if let Some(inner_pattern) = inner_pattern {
            let pattern_location = ctx.get_location(
                ctx.function_body.arenas.patterns[inner_pattern].stable_ptr().untyped(),
            );

            let var_id = ctx.new_var(VarRequest {
                ty: wrap_in_snapshots(ctx.db, concrete_variant.ty, self.n_snapshots),
                location: pattern_location,
            });
            builder_context.arm_var_ids.push(vec![var_id]);
            let variant_expr =
                LoweredExpr::AtVariable(VarUsage { var_id, location: pattern_location });

            lower_single_pattern(ctx, subscope, inner_pattern, variant_expr)
        } else {
            let var_id = ctx.new_var(VarRequest {
                ty: wrap_in_snapshots(ctx.db, concrete_variant.ty, self.n_snapshots),
                location: self.default_location,
            });
            builder_context.arm_var_ids.push(vec![var_id]);
            Ok(())
        }
    }
}

/// A context for lowering match arms and tracking generated blocks and input vars.
struct MatchArmsLoweringContext<'a> {
    /// Outer block builder to generate new subscopes.
    builder: &'a mut BlockBuilder,
    /// The block ids of the generated subscopes.
    block_ids: Vec<crate::BlockId>,
    /// The match arms to lower.
    arms: &'a [MatchArmWrapper<'a>],
    /// For each enum variant, stores `VariableId`s which act as inputs to the arm's subscope.    
    arm_var_ids: Vec<Vec<VariableId>>,
    /// Empty match info to be used for lowering match arms.
    empty_match_info: MatchInfo,
}

impl<'a> MatchArmsLoweringContext<'a> {
    /// Creates a new [MatchArmsLoweringContext] for the given match arms.
    fn new(
        builder: &'a mut BlockBuilder,
        match_arms: &'a [MatchArmWrapper<'_>],
        empty_match_info: MatchInfo,
    ) -> Self {
        Self {
            builder,
            block_ids: Default::default(),
            arms: match_arms,
            arm_var_ids: Default::default(),
            empty_match_info,
        }
    }

    /// Creates a new subscope for the current block, and update used block_ids.
    fn add_scope(&mut self, ctx: &mut LoweringContext<'_, '_>) -> BlockBuilder {
        let subscope = create_subscope(ctx, self.builder);
        self.block_ids.push(subscope.block_id);
        subscope
    }

    /// Get pattern from arm by `pattern_path`. Assumes existence of pattern_index only if pattern
    /// exists.
    fn pattern(
        &self,
        ctx: &'a mut LoweringContext<'_, '_>,
        pattern_path: PatternPath,
    ) -> Option<&'a Pattern> {
        let arm = &self.arms[pattern_path.arm_index];
        pattern_path.pattern_index.map(|pattern_index| {
            arm.pattern(ctx, pattern_index)
                .expect("Pattern was previously found and should be present in the arm.")
        })
    }

    /// Collects the match arms from the context.
    ///
    /// This function converts the internal representation of match arms (block_ids and arm_var_ids)
    /// into the final `MatchArm` structures that will be used in the lowered code.
    ///
    /// Panics if there's a mismatch between the number of concrete variants and
    /// the number of block IDs or arm variable IDs.
    fn collect_match_arms(self, concrete_variants: &[ConcreteVariant]) -> Vec<MatchArm> {
        assert_eq!(
            concrete_variants.len(),
            self.block_ids.len(),
            "Mismatch in number of concrete variants and block ids"
        );
        assert_eq!(
            concrete_variants.len(),
            self.arm_var_ids.len(),
            "Mismatch in number of concrete variants and arm var ids"
        );
        zip_eq3(concrete_variants, self.block_ids, self.arm_var_ids)
            .map(|(variant, block_id, var_ids)| MatchArm {
                arm_selector: MatchArmSelector::VariantId(variant.clone()),
                block_id,
                var_ids,
            })
            .collect()
    }
}

/// Represents a leaf in match tree, with the arm index it belongs to.
struct MatchLeafBuilder {
    arm_index: usize,
    lowering_result: LoweringResult<()>,
    builder: BlockBuilder,
}

/// Groups match arms of different variants to their corresponding arms blocks and lowers
/// the arms expression.
fn group_match_arms(
    ctx: &mut LoweringContext<'_, '_>,
    empty_match_info: &MatchInfo,
    location: LocationId,
    arms: &[MatchArmWrapper<'_>],
    variants_block_builders: Vec<MatchLeafBuilder>,
    kind: MatchKind,
) -> LoweringResult<Vec<SealedBlockBuilder>> {
    let res = variants_block_builders
        .into_iter()
        .sorted_by_key(|MatchLeafBuilder { arm_index, .. }| *arm_index)
        .chunk_by(|MatchLeafBuilder { arm_index, .. }| *arm_index)
        .into_iter()
        .map(|(arm_index, group)| {
            let arm = &arms[arm_index];
            let mut lowering_inner_pattern_results_and_subscopes = group
                .map(|MatchLeafBuilder { lowering_result, builder, .. }| (lowering_result, builder))
                .collect::<Vec<_>>();

            // If the arm has only one pattern, there is no need to create a parent scope.
            if lowering_inner_pattern_results_and_subscopes.len() == 1 {
                let (lowering_inner_pattern_result, subscope) =
                    lowering_inner_pattern_results_and_subscopes.pop().unwrap();

                return match lowering_inner_pattern_result {
                    Ok(_) => {
                        // Lower the arm expression.
                        lower_arm_expr_and_seal(ctx, kind, arm, subscope)
                    }
                    Err(err) => lowering_flow_error_to_sealed_block(ctx, subscope, err),
                }
                .map_err(LoweringFlowError::Failed);
            }

            // A parent block builder where the variables of each pattern are introduced.
            // The parent block should have the same semantics and changed_member_paths as any of
            // the child blocks.
            let mut outer_subscope = lowering_inner_pattern_results_and_subscopes[0]
                .1
                .sibling_block_builder(alloc_empty_block(ctx));

            let sealed_blocks: Vec<_> = lowering_inner_pattern_results_and_subscopes
                .into_iter()
                .map(|(lowering_inner_pattern_result, subscope)| {
                    match lowering_inner_pattern_result {
                        Ok(_) => Ok(subscope.goto_callsite(None)),
                        Err(err) => lowering_flow_error_to_sealed_block(ctx, subscope, err),
                    }
                    .map_err(LoweringFlowError::Failed)
                })
                .collect::<LoweringResult<Vec<_>>>()?;

            outer_subscope.merge_and_end_with_match(
                ctx,
                empty_match_info.clone(),
                sealed_blocks,
                location,
            )?;
            lower_arm_expr_and_seal(ctx, kind, arm, outer_subscope)
                .map_err(LoweringFlowError::Failed)
        })
        .collect();

    /// Lowers the expression of a match arm and seals the block.
    /// This function is responsible for:
    /// * Lowering the expression of the arm in the give subscope.
    /// * Handling the case where the expression is a block and the match arm is a while let.
    fn lower_arm_expr_and_seal(
        ctx: &mut LoweringContext<'_, '_>,
        kind: MatchKind,
        arm: &MatchArmWrapper<'_>,
        mut subscope: BlockBuilder,
    ) -> Maybe<SealedBlockBuilder> {
        match (arm.expr(), kind) {
            (Some(expr), MatchKind::IfLet | MatchKind::Match) => {
                lower_tail_expr(ctx, subscope, expr)
            }
            (Some(expr), MatchKind::WhileLet(loop_expr_id, stable_ptr)) => {
                let semantic::Expr::Block(expr) = ctx.function_body.arenas.exprs[expr].clone()
                else {
                    unreachable!("WhileLet expression should be a block");
                };
                let block_expr = (|| {
                    lower_expr_block(ctx, &mut subscope, &expr)?;
                    recursively_call_loop_func(ctx, &mut subscope, loop_expr_id, stable_ptr)
                })();

                lowered_expr_to_block_scope_end(ctx, subscope, block_expr)
            }
            (None, _) => Ok(subscope.goto_callsite(None)),
        }
    }

    res
}

/// Lowers the [semantic::MatchArm] of an expression of type [semantic::ExprMatch] where the matched
/// expression is a felt252.
fn lower_expr_felt252_arm(
    ctx: &mut LoweringContext<'_, '_>,
    expr: &semantic::ExprMatch,
    match_input: VarUsage,
    builder: &mut BlockBuilder,
    arm_index: usize,
    pattern_index: usize,
    branches_block_builders: &mut Vec<MatchLeafBuilder>,
) -> LoweringResult<MatchInfo> {
    if pattern_index == expr.arms[arm_index].patterns.len() {
        return lower_expr_felt252_arm(
            ctx,
            expr,
            match_input,
            builder,
            arm_index + 1,
            0,
            branches_block_builders,
        );
    }

    let location = ctx.get_location(expr.stable_ptr.untyped());
    let arm = &expr.arms[arm_index];
    let db = ctx.db;

    let main_block = create_subscope(ctx, builder);
    let main_block_id = main_block.block_id;

    let mut else_block = create_subscope(ctx, builder);
    let block_else_id = else_block.block_id;

    let pattern = &ctx.function_body.arenas.patterns[arm.patterns[pattern_index]];
    let semantic::Pattern::Literal(semantic::PatternLiteral { literal, .. }) = pattern else {
        return Err(LoweringFlowError::Failed(ctx.diagnostics.report(
            pattern.stable_ptr().untyped(),
            MatchError(MatchError {
                kind: MatchKind::Match,
                error: MatchDiagnostic::UnsupportedMatchArmNotALiteral,
            }),
        )));
    };

    let felt252_ty = ctx.db.core_info().felt252;
    let if_input = if literal.value == 0.into() {
        match_input
    } else {
        // TODO(TomerStarkware): Use the same type of literal as the input, without the cast to
        // felt252.
        let lowered_arm_val = lower_expr_literal(
            ctx,
            &semantic::ExprLiteral {
                stable_ptr: literal.stable_ptr,
                value: literal.value.clone(),
                ty: felt252_ty,
            },
            builder,
        )?
        .as_var_usage(ctx, builder)?;

        let call_result = generators::Call {
            function: corelib::felt252_sub(db).lowered(db),
            inputs: vec![match_input, lowered_arm_val],
            coupon_input: None,
            extra_ret_tys: vec![],
            ret_tys: vec![felt252_ty],
            location,
        }
        .add(ctx, &mut builder.statements);
        call_result.returns.into_iter().next().unwrap()
    };

    let non_zero_type = corelib::core_nonzero_ty(db, felt252_ty);
    let else_block_input_var_id = ctx.new_var(VarRequest { ty: non_zero_type, location });

    let match_info = MatchInfo::Extern(MatchExternInfo {
        function: corelib::core_felt252_is_zero(db).lowered(db),
        inputs: vec![if_input],
        arms: vec![
            MatchArm {
                arm_selector: MatchArmSelector::VariantId(corelib::jump_nz_zero_variant(
                    db, felt252_ty,
                )),
                block_id: main_block_id,
                var_ids: vec![],
            },
            MatchArm {
                arm_selector: MatchArmSelector::VariantId(corelib::jump_nz_nonzero_variant(
                    db, felt252_ty,
                )),
                block_id: block_else_id,
                var_ids: vec![else_block_input_var_id],
            },
        ],
        location,
    });
    branches_block_builders.push(MatchLeafBuilder {
        arm_index,
        lowering_result: Ok(()),
        builder: main_block,
    });
    if pattern_index + 1 == expr.arms[arm_index].patterns.len() && arm_index == expr.arms.len() - 2
    {
        branches_block_builders.push(MatchLeafBuilder {
            arm_index: arm_index + 1,
            lowering_result: Ok(()),
            builder: else_block,
        });
    } else {
        let match_info = lower_expr_felt252_arm(
            ctx,
            expr,
            match_input,
            &mut else_block,
            arm_index,
            pattern_index + 1,
            branches_block_builders,
        )?;

        // we can use finalize here because the else block is an inner block of the match expression
        // and does not have sibling block it goes to.
        else_block.finalize(ctx, BlockEnd::Match { info: match_info });
    }
    Ok(match_info)
}

/// lowers an expression of type [semantic::ExprMatch] where the matched expression is a felt252,
/// using an index enum.
fn lower_expr_match_index_enum(
    ctx: &mut LoweringContext<'_, '_>,
    expr: &semantic::ExprMatch,
    match_input: VarUsage,
    builder: &BlockBuilder,
    literals_to_arm_map: &UnorderedHashMap<usize, usize>,
    branches_block_builders: &mut Vec<MatchLeafBuilder>,
) -> LoweringResult<MatchInfo> {
    let location = ctx.get_location(expr.stable_ptr.untyped());
    let db = ctx.db;
    let unit_type = unit_ty(db);
    let mut arm_var_ids = vec![];
    let mut block_ids = vec![];

    for index in 0..literals_to_arm_map.len() {
        let subscope = create_subscope(ctx, builder);
        let block_id = subscope.block_id;
        block_ids.push(block_id);

        let arm_index = literals_to_arm_map[&index];

        let var_id = ctx.new_var(VarRequest { ty: unit_type, location });
        arm_var_ids.push(vec![var_id]);

        // Lower the arm expression.
        branches_block_builders.push(MatchLeafBuilder {
            arm_index,
            lowering_result: Ok(()),
            builder: subscope,
        });
    }

    let arms = zip_eq(block_ids, arm_var_ids)
        .enumerate()
        .map(|(value, (block_id, var_ids))| MatchArm {
            arm_selector: MatchArmSelector::Value(ValueSelectorArm { value }),
            block_id,
            var_ids,
        })
        .collect();
    let match_info = MatchInfo::Value(MatchEnumValue {
        num_of_arms: literals_to_arm_map.len(),
        arms,
        input: match_input,
        location,
    });
    Ok(match_info)
}

/// Lowers an expression of type [semantic::ExprMatch] where the matched expression is a felt252.
/// using an index enum to create a jump table.
fn lower_expr_match_value(
    ctx: &mut LoweringContext<'_, '_>,
    expr: &semantic::ExprMatch,
    mut match_input: VarUsage,
    builder: &mut BlockBuilder,
) -> LoweringResult<LoweredExpr> {
    log::trace!("Lowering a match-value expression.");
    if expr.arms.is_empty() {
        return Err(LoweringFlowError::Failed(ctx.diagnostics.report(
            expr.stable_ptr.untyped(),
            MatchError(MatchError {
                kind: MatchKind::Match,
                error: MatchDiagnostic::NonExhaustiveMatchValue,
            }),
        )));
    }
    let mut max = 0;
    let mut literals_to_arm_map = UnorderedHashMap::default();
    let mut otherwise_exist = false;
    for (arm_index, arm) in expr.arms.iter().enumerate() {
        for pattern in arm.patterns.iter() {
            let pattern = &ctx.function_body.arenas.patterns[*pattern];
            if otherwise_exist {
                return Err(LoweringFlowError::Failed(ctx.diagnostics.report(
                    pattern.stable_ptr().untyped(),
                    MatchError(MatchError {
                        kind: MatchKind::Match,
                        error: MatchDiagnostic::UnreachableMatchArm,
                    }),
                )));
            }
            match pattern {
                semantic::Pattern::Literal(semantic::PatternLiteral { literal, .. }) => {
                    let Some(literal) = literal.value.to_usize() else {
                        return Err(LoweringFlowError::Failed(ctx.diagnostics.report(
                            expr.stable_ptr.untyped(),
                            MatchError(MatchError {
                                kind: MatchKind::Match,
                                error: MatchDiagnostic::UnsupportedMatchArmNonSequential,
                            }),
                        )));
                    };
                    if otherwise_exist || literals_to_arm_map.insert(literal, arm_index).is_some() {
                        return Err(LoweringFlowError::Failed(ctx.diagnostics.report(
                            pattern.stable_ptr().untyped(),
                            MatchError(MatchError {
                                kind: MatchKind::Match,
                                error: MatchDiagnostic::UnreachableMatchArm,
                            }),
                        )));
                    }
                    if literal > max {
                        max = literal;
                    }
                }
                semantic::Pattern::Otherwise(_) => otherwise_exist = true,
                _ => {
                    return Err(LoweringFlowError::Failed(ctx.diagnostics.report(
                        pattern.stable_ptr().untyped(),
                        MatchError(MatchError {
                            kind: MatchKind::Match,
                            error: MatchDiagnostic::UnsupportedMatchArmNotALiteral,
                        }),
                    )));
                }
            }
        }
    }

    if !otherwise_exist {
        return Err(LoweringFlowError::Failed(ctx.diagnostics.report(
            expr.stable_ptr.untyped(),
            MatchError(MatchError {
                kind: MatchKind::Match,
                error: MatchDiagnostic::NonExhaustiveMatchValue,
            }),
        )));
    }
    if max + 1 != literals_to_arm_map.len() {
        return Err(LoweringFlowError::Failed(ctx.diagnostics.report(
            expr.stable_ptr.untyped(),
            MatchError(MatchError {
                kind: MatchKind::Match,
                error: MatchDiagnostic::UnsupportedMatchArmNonSequential,
            }),
        )));
    };
    let location = ctx.get_location(expr.stable_ptr.untyped());

    let mut arms_vec = vec![];

    let db = ctx.db;

    let empty_match_info = MatchInfo::Extern(MatchExternInfo {
        function: corelib::core_felt252_is_zero(db).lowered(db),
        inputs: vec![match_input],
        arms: vec![],
        location,
    });

    let info = db.core_info();
    let felt252_ty = info.felt252;
    let ty = ctx.variables[match_input.var_id].ty;

    // max +2 is the number of arms in the match.
    if max + 2 < numeric_match_optimization_threshold(ctx, ty != felt252_ty) {
        if ty != felt252_ty {
            let function = info
                .upcast_fn
                .concretize(
                    db,
                    vec![GenericArgumentId::Type(ty), GenericArgumentId::Type(felt252_ty)],
                )
                .lowered(db);
            let call_result = generators::Call {
                function,
                inputs: vec![match_input],
                coupon_input: None,
                extra_ret_tys: vec![],
                ret_tys: vec![felt252_ty],
                location,
            }
            .add(ctx, &mut builder.statements);

            match_input = call_result.returns.into_iter().next().unwrap();
        }

        let match_info =
            lower_expr_felt252_arm(ctx, expr, match_input, builder, 0, 0, &mut arms_vec)?;

        let sealed_blocks = group_match_arms(
            ctx,
            &empty_match_info,
            location,
            &expr.arms.iter().map(|arm| arm.into()).collect_vec(),
            arms_vec,
            MatchKind::Match,
        )?;

        return builder.merge_and_end_with_match(ctx, match_info, sealed_blocks, location);
    }

    let bounded_int_ty = corelib::bounded_int_ty(db, 0.into(), max.into());

    let in_range_block_input_var_id = ctx.new_var(VarRequest { ty: bounded_int_ty, location });

    let in_range_block = create_subscope(ctx, builder);
    let in_range_block_id = in_range_block.block_id;
    let inner_match_info = lower_expr_match_index_enum(
        ctx,
        expr,
        VarUsage { var_id: in_range_block_input_var_id, location: match_input.location },
        &in_range_block,
        &literals_to_arm_map,
        &mut arms_vec,
    )?;
    in_range_block.finalize(ctx, BlockEnd::Match { info: inner_match_info });

    let otherwise_block = create_subscope(ctx, builder);
    let otherwise_block_id = otherwise_block.block_id;

    arms_vec.push(MatchLeafBuilder {
        arm_index: expr.arms.len() - 1,
        lowering_result: Ok(()),
        builder: otherwise_block,
    });

    let function_id = info
        .downcast_fn
        .concretize(db, vec![GenericArgumentId::Type(ty), GenericArgumentId::Type(bounded_int_ty)])
        .lowered(db);

    let match_info = MatchInfo::Extern(MatchExternInfo {
        function: function_id,
        inputs: vec![match_input],
        arms: vec![
            MatchArm {
                arm_selector: MatchArmSelector::VariantId(corelib::option_some_variant(
                    db,
                    bounded_int_ty,
                )),
                block_id: in_range_block_id,
                var_ids: vec![in_range_block_input_var_id],
            },
            MatchArm {
                arm_selector: MatchArmSelector::VariantId(corelib::option_none_variant(
                    db,
                    bounded_int_ty,
                )),
                block_id: otherwise_block_id,
                var_ids: vec![],
            },
        ],
        location,
    });
    let sealed_blocks = group_match_arms(
        ctx,
        &empty_match_info,
        location,
        &expr.arms.iter().map(|arm| arm.into()).collect_vec(),
        arms_vec,
        MatchKind::Match,
    )?;
    builder.merge_and_end_with_match(ctx, match_info, sealed_blocks, location)
}

/// Returns the threshold for the number of arms for optimising numeric match expressions, by using
/// a jump table instead of an if-else construct.
/// `is_small_type` means the matched type has < 2**128 possible values.
fn numeric_match_optimization_threshold(
    ctx: &mut LoweringContext<'_, '_>,
    is_small_type: bool,
) -> usize {
    // For felt252 the number of steps with if-else is 2 * min(n, number_of_arms) + 2 and 11~13 for
    // jump table for small_types the number of steps with if-else is 2 * min(n, number_of_arms) + 4
    // and 9~12 for jump table.
    let default_threshold = if is_small_type { 8 } else { 10 };
    ctx.db
        .get_flag(FlagId::new(ctx.db, "numeric_match_optimization_min_arms_threshold"))
        .map(|flag| match *flag {
            Flag::NumericMatchOptimizationMinArmsThreshold(threshold) => threshold,
            _ => panic!("Wrong type flag `{flag:?}`."),
        })
        .unwrap_or(default_threshold)
}

/// Reports a missing arm error using `variants` string as part of the message,
/// and returns a [LoweringFlowError].
fn report_missing_arm_error(
    ctx: &mut LoweringContext<'_, '_>,
    location: LocationId,
    match_type: MatchKind,
    variants: String,
) -> LoweringFlowError {
    LoweringFlowError::Failed(ctx.diagnostics.report_by_location(
        location.lookup_intern(ctx.db),
        MatchError(MatchError {
            kind: match_type,
            error: MatchDiagnostic::MissingMatchArm(variants),
        }),
    ))
}

/// Reports a missing arm error and returns a [LoweringFlowError].
fn report_missing_variant_error(
    ctx: &mut LoweringContext<'_, '_>,
    location: LocationId,
    match_type: MatchKind,
    variant: &ConcreteVariant,
) -> LoweringFlowError {
    report_missing_arm_error(ctx, location, match_type, variant.id.name(ctx.db).to_string())
}
