use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::NamedLanguageElementId;
use cairo_lang_filesystem::flag::Flag;
use cairo_lang_filesystem::ids::FlagId;
use cairo_lang_semantic as semantic;
use cairo_lang_semantic::corelib;
use cairo_lang_syntax::node::TypedStablePtr;
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
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
    alloc_empty_block, call_loop_func, generators, lower_expr_block, lower_expr_literal,
    lower_tail_expr, lowered_expr_to_block_scope_end,
};
use crate::diagnostic::LoweringDiagnosticKind::*;
use crate::diagnostic::{LoweringDiagnosticsBuilder, MatchDiagnostic, MatchError, MatchKind};
use crate::ids::{LocationId, SemanticFunctionIdEx};
use crate::lower::context::VarRequest;
use crate::lower::external::extern_facade_expr;
use crate::lower::{
    create_subscope, lower_expr, lower_single_pattern, match_extern_arm_ref_args_bind,
    match_extern_variant_arm_input_types,
};
use crate::{
    FlatBlockEnd, MatchArm, MatchEnumInfo, MatchEnumValue, MatchExternInfo, MatchInfo, VarUsage,
    VariableId,
};

/// Information about the enum of a match statement. See [extract_concrete_enum].
struct ExtractedEnumDetails {
    concrete_enum_id: semantic::ConcreteEnumId,
    concrete_variants: Vec<semantic::ConcreteVariant>,
    n_snapshots: usize,
}

/// MatchArm wrapper that allow for optional expression clause.
/// Used in the case of if-let with missing else clause.
pub struct MatchArmWrapper {
    pub patterns: Vec<PatternId>,
    pub expr: Option<semantic::ExprId>,
}

impl From<&semantic::MatchArm> for MatchArmWrapper {
    fn from(arm: &semantic::MatchArm) -> Self {
        Self { patterns: arm.patterns.clone(), expr: Some(arm.expression) }
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
    let (n_snapshots, long_ty) = peel_snapshots(ctx.db.upcast(), ty);

    // Semantic model should have made sure the type is an enum.
    let TypeLongId::Concrete(ConcreteTypeId::Enum(concrete_enum_id)) = long_ty else {
        return Err(LoweringFlowError::Failed(ctx.diagnostics.report(
            stable_ptr,
            MatchError(MatchError {
                kind: match_type,
                error: MatchDiagnostic::UnsupportedMatchedType(long_ty.format(ctx.db.upcast())),
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
            let (n_snapshots, long_ty) = peel_snapshots(ctx.db.upcast(), *ty);
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
#[derive(Debug, Clone)]
struct PatternPath {
    arm_index: usize,
    pattern_index: Option<usize>,
}

/// Returns an option containing the PatternPath of the underscore pattern, if it exists.
fn get_underscore_pattern_path(
    ctx: &mut LoweringContext<'_, '_>,
    arms: &[MatchArmWrapper],
    match_type: MatchKind,
) -> Option<PatternPath> {
    let otherwise_variant = arms
        .iter()
        .enumerate()
        .map(|(arm_index, arm)| {
            if arm.patterns.is_empty() {
                return Some(PatternPath { arm_index, pattern_index: None });
            }
            arm.patterns
                .iter()
                .position(|pattern| {
                    matches!(
                        ctx.function_body.arenas.patterns[*pattern],
                        semantic::Pattern::Otherwise(_)
                    )
                })
                .map(|pattern_index| PatternPath { arm_index, pattern_index: Some(pattern_index) })
        })
        .find(|option| option.is_some())??;

    for arm in arms.iter().skip(otherwise_variant.arm_index + 1) {
        if arm.patterns.is_empty() && arm.expr.is_some() {
            let expr = ctx.function_body.arenas.exprs[arm.expr.unwrap()].clone();
            ctx.diagnostics.report(
                &expr,
                MatchError(MatchError {
                    kind: match_type,
                    error: MatchDiagnostic::UnreachableMatchArm,
                }),
            );
        }
        for pattern in arm.patterns.iter() {
            let pattern = ctx.function_body.arenas.patterns[*pattern].clone();
            ctx.diagnostics.report(
                &pattern,
                MatchError(MatchError {
                    kind: match_type,
                    error: MatchDiagnostic::UnreachableMatchArm,
                }),
            );
        }
    }
    for pattern in arms[otherwise_variant.arm_index]
        .patterns
        .iter()
        .skip(otherwise_variant.pattern_index.unwrap_or(0) + 1)
    {
        let pattern = ctx.function_body.arenas.patterns[*pattern].clone();
        ctx.diagnostics.report(
            &pattern,
            MatchError(MatchError {
                kind: match_type,
                error: MatchDiagnostic::UnreachableMatchArm,
            }),
        );
    }

    Some(otherwise_variant)
}

/// Returns a map from variants to their corresponding pattern path in a match statement.
fn get_variant_to_arm_map<'a>(
    ctx: &mut LoweringContext<'_, '_>,
    arms: impl Iterator<Item = &'a MatchArmWrapper>,
    concrete_enum_id: semantic::ConcreteEnumId,
    match_type: MatchKind,
) -> LoweringResult<UnorderedHashMap<semantic::ConcreteVariant, PatternPath>> {
    let mut map = UnorderedHashMap::default();
    for (arm_index, arm) in arms.enumerate() {
        for (pattern_index, pattern) in arm.patterns.iter().enumerate() {
            let pattern = ctx.function_body.arenas.patterns[*pattern].clone();

            if let semantic::Pattern::Otherwise(_) = pattern {
                break;
            }

            let enum_pattern = try_extract_matches!(&pattern, semantic::Pattern::EnumVariant)
                .ok_or_else(|| {
                    LoweringFlowError::Failed(ctx.diagnostics.report(
                        &pattern,
                        MatchError(MatchError {
                            kind: match_type,
                            error: MatchDiagnostic::UnsupportedMatchArmNotAVariant,
                        }),
                    ))
                })?
                .clone();

            if enum_pattern.variant.concrete_enum_id != concrete_enum_id {
                return Err(LoweringFlowError::Failed(ctx.diagnostics.report(
                    &pattern,
                    MatchError(MatchError {
                        kind: match_type,
                        error: MatchDiagnostic::UnsupportedMatchArmNotAVariant,
                    }),
                )));
            }

            match map.entry(enum_pattern.variant.clone()) {
                Entry::Occupied(_) => {
                    ctx.diagnostics.report(
                        &pattern,
                        MatchError(MatchError {
                            kind: match_type,
                            error: MatchDiagnostic::UnreachableMatchArm,
                        }),
                    );
                }
                Entry::Vacant(entry) => {
                    entry.insert(PatternPath { arm_index, pattern_index: Some(pattern_index) });
                }
            };
        }
    }
    Ok(map)
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
                entry.insert(pattern_path.clone());
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
    arms: impl Iterator<Item = &'a MatchArmWrapper>,
    extracted_enums_details: &[ExtractedEnumDetails],
    match_type: MatchKind,
) -> LoweringResult<UnorderedHashMap<MatchingPath, PatternPath>> {
    let mut map = UnorderedHashMap::default();
    for (arm_index, arm) in arms.enumerate() {
        for (pattern_index, pattern) in arm.patterns.iter().enumerate() {
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
    arms: &[MatchArmWrapper],
    match_tuple_ctx: &mut LoweringMatchTupleContext,
    leaves_builders: &mut Vec<MatchLeafBuilder>,
    match_type: MatchKind,
) -> LoweringResult<()> {
    let pattern_path = match_tuple_ctx
        .variants_map
        .get(&match_tuple_ctx.current_path)
        .or(match_tuple_ctx.otherwise_variant.as_ref())
        .ok_or_else(|| {
            LoweringFlowError::Failed(ctx.diagnostics.report_by_location(
                match_tuple_ctx.match_location.lookup_intern(ctx.db),
                MatchError(MatchError {
                    kind: match_type,
                    error: MatchDiagnostic::MissingMatchArm(format!(
                        "({})",
                        match_tuple_ctx.current_path.variants
                            .iter()
                            .map(|variant| variant.id.name(ctx.db.upcast()))
                            .join(", ")
                    )),
                }),
            ))
        })?;
    let pattern = pattern_path.pattern_index.map(|pattern_index| {
        ctx.function_body.arenas.patterns[arms[pattern_path.arm_index].patterns[pattern_index]]
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
                        let inner_pattern =
                            ctx.function_body.arenas.patterns[*inner_pattern].clone();
                        let pattern_location =
                            ctx.get_location(inner_pattern.stable_ptr().untyped());

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
            return Err(LoweringFlowError::Failed(ctx.diagnostics.report(
                &pattern.unwrap(),
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
        lowerin_result: lowering_inner_pattern_result,
    });
    Ok(())
}

/// Lowers a full decision tree for a match on a tuple expression.
fn lower_full_match_tree(
    ctx: &mut LoweringContext<'_, '_>,
    builder: &mut BlockBuilder,
    arms: &[MatchArmWrapper],
    match_tuple_ctx: &mut LoweringMatchTupleContext,
    extracted_enums_details: &[ExtractedEnumDetails],
    leaves_builders: &mut Vec<MatchLeafBuilder>,
    match_type: MatchKind,
) -> LoweringResult<MatchInfo> {
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
                    ctx.db.upcast(),
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
                    subscope.finalize(ctx, FlatBlockEnd::Match { info: match_info });
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
    matched_expr: &semantic::Expr,
    tuple_info: &TupleInfo,
    arms: &[MatchArmWrapper],
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
                ty: wrap_in_snapshots(ctx.db.upcast(), *ty, tuple_info.n_snapshots),
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
        matched_expr.stable_ptr().untyped(),
        &tuple_info.types,
        match_type,
    )?;

    let otherwise_variant = get_underscore_pattern_path(ctx, arms, match_type);

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
        group_match_arms(ctx, empty_match_info, location, arms, arms_vec, match_type)?;

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

    let matched_expr = ctx.function_body.arenas.exprs[expr.matched_expr].clone();
    let ty = matched_expr.ty();

    if ty == ctx.db.core_info().felt252 {
        let match_input = lowered_expr.as_var_usage(ctx, builder)?;
        return lower_expr_match_felt252(ctx, expr, match_input, builder, None);
    }
    if let Some(convert_function) =
        corelib::get_convert_to_felt252_libfunc_name_by_type(ctx.db.upcast(), ty)
    {
        let match_input = lowered_expr.as_var_usage(ctx, builder)?;
        return lower_expr_match_felt252(ctx, expr, match_input, builder, Some(convert_function));
    }

    let (n_snapshots, long_type_id) = peel_snapshots(ctx.db.upcast(), ty);

    let arms = expr.arms.iter().map(|arm| arm.into()).collect_vec();
    if let Some(types) = try_extract_matches!(long_type_id, TypeLongId::Tuple) {
        return lower_expr_match_tuple(
            ctx,
            builder,
            lowered_expr,
            &matched_expr,
            &TupleInfo { n_snapshots, types },
            &arms,
            MatchKind::Match,
        );
    }

    // TODO(spapini): Use diagnostics.
    // TODO(spapini): Handle more than just enums.
    if let LoweredExpr::ExternEnum(extern_enum) = lowered_expr {
        return lower_optimized_extern_match(ctx, builder, extern_enum, &arms, MatchKind::Match);
    }
    lower_concrete_enum_match(
        ctx,
        builder,
        &matched_expr,
        lowered_expr,
        &arms,
        location,
        MatchKind::Match,
    )
}

pub(crate) fn lower_concrete_enum_match(
    ctx: &mut LoweringContext<'_, '_>,
    builder: &mut BlockBuilder,
    matched_expr: &semantic::Expr,
    lowered_matched_expr: LoweredExpr,
    arms: &[MatchArmWrapper],
    location: LocationId,
    match_type: MatchKind,
) -> LoweringResult<LoweredExpr> {
    let ExtractedEnumDetails { concrete_enum_id, concrete_variants, n_snapshots } =
        extract_concrete_enum(ctx, matched_expr.into(), matched_expr.ty(), match_type)?;
    let match_input = lowered_matched_expr.as_var_usage(ctx, builder)?;

    // Merge arm blocks.
    let otherwise_variant = get_underscore_pattern_path(ctx, arms, match_type);

    let variant_map = get_variant_to_arm_map(
        ctx,
        arms.iter().take(
            otherwise_variant
                .as_ref()
                .map(|PatternPath { arm_index, .. }| *arm_index)
                .unwrap_or(arms.len()),
        ),
        concrete_enum_id,
        match_type,
    )?;
    let mut arm_var_ids = vec![];
    let mut block_ids = vec![];
    let variants_block_builders = concrete_variants
        .iter()
        .map(|concrete_variant| {
            let PatternPath { arm_index, pattern_index } = variant_map
                .get(concrete_variant)
                .or(otherwise_variant.as_ref())
                .ok_or_else(|| {
                    LoweringFlowError::Failed(ctx.diagnostics.report_by_location(
                        location.lookup_intern(ctx.db),
                        MatchError(MatchError {
                            kind: match_type,
                            error: MatchDiagnostic::MissingMatchArm(format!(
                                "{}",
                                concrete_variant.id.name(ctx.db.upcast())
                            )),
                        }),
                    ))
                })?;
            let arm = &arms[*arm_index];

            let mut subscope = create_subscope(ctx, builder);

            let pattern = pattern_index.map(|pattern_index| {
                &ctx.function_body.arenas.patterns[arm.patterns[pattern_index]]
            });
            let block_id = subscope.block_id;
            block_ids.push(block_id);

            let lowering_inner_pattern_result = match pattern {
                Some(Pattern::EnumVariant(PatternEnumVariant {
                    inner_pattern: Some(inner_pattern),
                    ..
                })) => {
                    let inner_pattern = ctx.function_body.arenas.patterns[*inner_pattern].clone();
                    let pattern_location = ctx.get_location(inner_pattern.stable_ptr().untyped());

                    let var_id = ctx.new_var(VarRequest {
                        ty: wrap_in_snapshots(ctx.db.upcast(), concrete_variant.ty, n_snapshots),
                        location: pattern_location,
                    });
                    arm_var_ids.push(vec![var_id]);
                    let variant_expr =
                        LoweredExpr::AtVariable(VarUsage { var_id, location: pattern_location });

                    lower_single_pattern(ctx, &mut subscope, inner_pattern, variant_expr)
                }
                Some(
                    Pattern::EnumVariant(PatternEnumVariant { inner_pattern: None, .. })
                    | Pattern::Otherwise(_),
                ) => {
                    let var_id = ctx.new_var(VarRequest {
                        ty: wrap_in_snapshots(ctx.db.upcast(), concrete_variant.ty, n_snapshots),
                        location: ctx.get_location(pattern.unwrap().into()),
                    });
                    arm_var_ids.push(vec![var_id]);
                    Ok(())
                }
                None => {
                    let var_id = ctx.new_var(VarRequest {
                        ty: wrap_in_snapshots(ctx.db.upcast(), concrete_variant.ty, n_snapshots),
                        location,
                    });
                    arm_var_ids.push(vec![var_id]);
                    Ok(())
                }
                _ => unreachable!(
                    "function `get_variant_to_arm_map` should have reported every other pattern \
                     type"
                ),
            };
            Ok(MatchLeafBuilder {
                arm_index: *arm_index,
                lowerin_result: lowering_inner_pattern_result,
                builder: subscope,
            })
        })
        .collect::<Vec<_>>()
        .into_iter()
        .collect::<LoweringResult<Vec<_>>>()?;

    let empty_match_info = MatchInfo::Enum(MatchEnumInfo {
        concrete_enum_id,
        input: match_input,
        arms: vec![],
        location,
    });

    let sealed_blocks = group_match_arms(
        ctx,
        empty_match_info,
        location,
        arms,
        variants_block_builders,
        match_type,
    )?;

    let match_info = MatchInfo::Enum(MatchEnumInfo {
        concrete_enum_id,
        input: match_input,
        arms: zip_eq(zip_eq(concrete_variants, block_ids), arm_var_ids)
            .map(|((variant_id, block_id), var_ids)| MatchArm {
                arm_selector: MatchArmSelector::VariantId(variant_id),
                block_id,
                var_ids,
            })
            .collect(),
        location,
    });
    builder.merge_and_end_with_match(ctx, match_info, sealed_blocks, location)
}

/// Lowers a match expression on a LoweredExpr::ExternEnum lowered expression.
pub(crate) fn lower_optimized_extern_match(
    ctx: &mut LoweringContext<'_, '_>,
    builder: &mut BlockBuilder,
    extern_enum: LoweredExprExternEnum,
    match_arms: &[MatchArmWrapper],
    match_type: MatchKind,
) -> LoweringResult<LoweredExpr> {
    log::trace!("Started lowering of an optimized extern match.");
    let location = extern_enum.location;
    let concrete_variants = ctx
        .db
        .concrete_enum_variants(extern_enum.concrete_enum_id)
        .map_err(LoweringFlowError::Failed)?;

    // Merge arm blocks.
    let otherwise_variant = get_underscore_pattern_path(ctx, match_arms, match_type);

    let variant_map = get_variant_to_arm_map(
        ctx,
        match_arms.iter().take(
            otherwise_variant
                .as_ref()
                .map(|PatternPath { arm_index, .. }| *arm_index)
                .unwrap_or(match_arms.len()),
        ),
        extern_enum.concrete_enum_id,
        match_type,
    )?;
    let mut arm_var_ids = vec![];
    let mut block_ids = vec![];

    let variants_block_builders = concrete_variants
        .iter()
        .map(|concrete_variant| {
            let mut subscope = create_subscope(ctx, builder);
            let block_id = subscope.block_id;
            block_ids.push(block_id);

            let input_tys =
                match_extern_variant_arm_input_types(ctx, concrete_variant.ty, &extern_enum);
            let mut input_vars = input_tys
                .into_iter()
                .map(|ty| ctx.new_var(VarRequest { ty, location }))
                .collect_vec();
            arm_var_ids.push(input_vars.clone());

            // Bind the arm inputs to implicits and semantic variables.
            match_extern_arm_ref_args_bind(ctx, &mut input_vars, &extern_enum, &mut subscope);

            let variant_expr = extern_facade_expr(ctx, concrete_variant.ty, input_vars, location);

            let PatternPath { arm_index, pattern_index } = variant_map
                .get(concrete_variant)
                .or(otherwise_variant.as_ref())
                .ok_or_else(|| {
                    LoweringFlowError::Failed(ctx.diagnostics.report_by_location(
                        location.lookup_intern(ctx.db),
                        MatchError(MatchError {
                            kind: match_type,
                            error: MatchDiagnostic::MissingMatchArm(format!(
                                "{}",
                                concrete_variant.id.name(ctx.db.upcast())
                            )),
                        }),
                    ))
                })?;

            let arm = &match_arms[*arm_index];
            let pattern = pattern_index.map(|pattern_index| {
                &ctx.function_body.arenas.patterns[arm.patterns[pattern_index]]
            });

            let lowering_inner_pattern_result = match pattern {
                Some(Pattern::EnumVariant(PatternEnumVariant {
                    inner_pattern: Some(inner_pattern),
                    ..
                })) => lower_single_pattern(
                    ctx,
                    &mut subscope,
                    ctx.function_body.arenas.patterns[*inner_pattern].clone(),
                    variant_expr,
                ),
                Some(
                    Pattern::EnumVariant(PatternEnumVariant { inner_pattern: None, .. })
                    | Pattern::Otherwise(_),
                )
                | None => Ok(()),
                _ => unreachable!(
                    "function `get_variant_to_arm_map` should have reported every other pattern \
                     type"
                ),
            };
            Ok(MatchLeafBuilder {
                arm_index: *arm_index,
                lowerin_result: lowering_inner_pattern_result,
                builder: subscope,
            })
        })
        .collect::<Vec<_>>()
        .into_iter()
        .collect::<LoweringResult<Vec<_>>>()?;

    let empty_match_info = MatchInfo::Extern(MatchExternInfo {
        function: extern_enum.function.lowered(ctx.db),
        inputs: vec![],
        arms: vec![],
        location,
    });
    let sealed_blocks = group_match_arms(
        ctx,
        empty_match_info,
        location,
        match_arms,
        variants_block_builders,
        match_type,
    )?;
    let match_info = MatchInfo::Extern(MatchExternInfo {
        function: extern_enum.function.lowered(ctx.db),
        inputs: extern_enum.inputs,
        arms: zip_eq(zip_eq(concrete_variants, block_ids), arm_var_ids)
            .map(|((variant_id, block_id), var_ids)| MatchArm {
                arm_selector: MatchArmSelector::VariantId(variant_id),
                block_id,
                var_ids,
            })
            .collect(),
        location,
    });
    builder.merge_and_end_with_match(ctx, match_info, sealed_blocks, location)
}

/// Represents a leaf in match tree, with the arm index it belongs to.
struct MatchLeafBuilder {
    arm_index: usize,
    lowerin_result: LoweringResult<()>,
    builder: BlockBuilder,
}
/// Groups match arms of different variants to their corresponding arms blocks and lowers
/// the arms expression.
fn group_match_arms(
    ctx: &mut LoweringContext<'_, '_>,
    empty_match_info: MatchInfo,
    location: LocationId,
    arms: &[MatchArmWrapper],
    variants_block_builders: Vec<MatchLeafBuilder>,
    kind: MatchKind,
) -> LoweringResult<Vec<SealedBlockBuilder>> {
    variants_block_builders
        .into_iter()
        .sorted_by_key(|MatchLeafBuilder { arm_index, .. }| *arm_index)
        .chunk_by(|MatchLeafBuilder { arm_index, .. }| *arm_index)
        .into_iter()
        .map(|(arm_index, group)| {
            let arm = &arms[arm_index];
            let mut lowering_inner_pattern_results_and_subscopes = group
                .map(|MatchLeafBuilder { lowerin_result, builder, .. }| (lowerin_result, builder))
                .collect::<Vec<_>>();

            // If the arm has only one pattern, there is no need to create a parent scope.
            if lowering_inner_pattern_results_and_subscopes.len() == 1 {
                let (lowering_inner_pattern_result, mut subscope) =
                    lowering_inner_pattern_results_and_subscopes.pop().unwrap();

                return match lowering_inner_pattern_result {
                    Ok(_) => {
                        // Lower the arm expression.
                        match (arm.expr, kind) {
                            (Some(expr), MatchKind::IfLet | MatchKind::Match) => {
                                lower_tail_expr(ctx, subscope, expr)
                            }
                            (Some(expr), MatchKind::WhileLet(loop_expr_id, stable_ptr)) => {
                                let semantic::Expr::Block(expr) =
                                    ctx.function_body.arenas.exprs[expr].clone()
                                else {
                                    unreachable!("While Let expression should be a block");
                                };
                                let block_expr = (|| {
                                    lower_expr_block(ctx, &mut subscope, &expr)?;
                                    // Add recursive call.
                                    let signature = ctx.signature.clone();
                                    call_loop_func(
                                        ctx,
                                        signature,
                                        &mut subscope,
                                        loop_expr_id,
                                        stable_ptr,
                                    )
                                })();

                                lowered_expr_to_block_scope_end(ctx, subscope, block_expr)
                            }
                            (None, _) => lowered_expr_to_block_scope_end(
                                ctx,
                                subscope,
                                Ok(LoweredExpr::Tuple { exprs: vec![], location }),
                            ),
                        }
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
                    // Use the first pattern for the location of the for variable assignment block.
                    let location = arm
                        .patterns
                        .first()
                        .map(|pattern| {
                            ctx.get_location(
                                ctx.function_body.arenas.patterns[*pattern].stable_ptr().untyped(),
                            )
                        })
                        .unwrap_or(location);
                    match lowering_inner_pattern_result {
                        Ok(_) => lowered_expr_to_block_scope_end(
                            ctx,
                            subscope,
                            Ok(LoweredExpr::Tuple { exprs: vec![], location }),
                        ),
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
            match (arm.expr, kind) {
                (Some(expr), MatchKind::IfLet | MatchKind::Match) => {
                    lower_tail_expr(ctx, outer_subscope, expr)
                }
                (Some(expr), MatchKind::WhileLet(loop_expr_id, stable_ptr)) => {
                    let semantic::Expr::Block(expr) = ctx.function_body.arenas.exprs[expr].clone()
                    else {
                        unreachable!("WhileLet expression should be a block");
                    };
                    let block_expr = (|| {
                        lower_expr_block(ctx, &mut outer_subscope, &expr)?;
                        // Add recursive call.
                        let signature = ctx.signature.clone();
                        call_loop_func(
                            ctx,
                            signature,
                            &mut outer_subscope,
                            loop_expr_id,
                            stable_ptr,
                        )
                    })();

                    lowered_expr_to_block_scope_end(ctx, outer_subscope, block_expr)
                }
                (None, _) => lowered_expr_to_block_scope_end(
                    ctx,
                    outer_subscope,
                    Ok(LoweredExpr::Tuple { exprs: vec![], location }),
                ),
            }
            .map_err(LoweringFlowError::Failed)
        })
        .collect()
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
    let semantic_db = ctx.db.upcast();

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
            function: corelib::felt252_sub(ctx.db.upcast()).lowered(ctx.db),
            inputs: vec![match_input, lowered_arm_val],
            coupon_input: None,
            extra_ret_tys: vec![],
            ret_tys: vec![felt252_ty],
            location,
        }
        .add(ctx, &mut builder.statements);
        call_result.returns.into_iter().next().unwrap()
    };

    let non_zero_type = corelib::core_nonzero_ty(semantic_db, felt252_ty);
    let else_block_input_var_id = ctx.new_var(VarRequest { ty: non_zero_type, location });

    let match_info = MatchInfo::Extern(MatchExternInfo {
        function: corelib::core_felt252_is_zero(semantic_db).lowered(ctx.db),
        inputs: vec![if_input],
        arms: vec![
            MatchArm {
                arm_selector: MatchArmSelector::VariantId(corelib::jump_nz_zero_variant(
                    semantic_db,
                    felt252_ty,
                )),
                block_id: main_block_id,
                var_ids: vec![],
            },
            MatchArm {
                arm_selector: MatchArmSelector::VariantId(corelib::jump_nz_nonzero_variant(
                    semantic_db,
                    felt252_ty,
                )),
                block_id: block_else_id,
                var_ids: vec![else_block_input_var_id],
            },
        ],
        location,
    });
    branches_block_builders.push(MatchLeafBuilder {
        arm_index,
        lowerin_result: Ok(()),
        builder: main_block,
    });
    if pattern_index + 1 == expr.arms[arm_index].patterns.len() && arm_index == expr.arms.len() - 2
    {
        branches_block_builders.push(MatchLeafBuilder {
            arm_index: arm_index + 1,
            lowerin_result: Ok(()),
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
        else_block.finalize(ctx, FlatBlockEnd::Match { info: match_info });
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
    let semantic_db = ctx.db.upcast();
    let unit_type = unit_ty(semantic_db);
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
            lowerin_result: Ok(()),
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
fn lower_expr_match_felt252(
    ctx: &mut LoweringContext<'_, '_>,
    expr: &semantic::ExprMatch,
    mut match_input: VarUsage,
    builder: &mut BlockBuilder,
    convert_function: Option<semantic::FunctionId>,
) -> LoweringResult<LoweredExpr> {
    log::trace!("Lowering a match-felt252 expression.");
    if expr.arms.is_empty() {
        return Err(LoweringFlowError::Failed(ctx.diagnostics.report(
            expr.stable_ptr.untyped(),
            MatchError(MatchError {
                kind: MatchKind::Match,
                error: MatchDiagnostic::NonExhaustiveMatchFelt252,
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
                error: MatchDiagnostic::NonExhaustiveMatchFelt252,
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

    let empty_match_info = MatchInfo::Extern(MatchExternInfo {
        function: corelib::core_felt252_is_zero(ctx.db.upcast()).lowered(ctx.db),
        inputs: vec![match_input],
        arms: vec![],
        location,
    });

    let felt252_ty = ctx.db.core_info().felt252;

    // max +2 is the number of arms in the match.
    if max + 2 < numeric_match_optimization_threshold(ctx, convert_function.is_some()) {
        if let Some(convert_function) = convert_function {
            let call_result = generators::Call {
                function: convert_function.lowered(ctx.db),
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
            empty_match_info,
            location,
            &expr.arms.iter().map(|arm| arm.into()).collect_vec(),
            arms_vec,
            MatchKind::Match,
        )?;

        return builder.merge_and_end_with_match(ctx, match_info, sealed_blocks, location);
    }

    let semantic_db = ctx.db.upcast();
    let bounded_int_ty = corelib::bounded_int_ty(semantic_db, 0.into(), max.into());

    let ty = ctx.function_body.arenas.exprs[expr.matched_expr].ty();
    let function_id = corelib::core_downcast(semantic_db, ty, bounded_int_ty).lowered(ctx.db);

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
    in_range_block.finalize(ctx, FlatBlockEnd::Match { info: inner_match_info });

    let otherwise_block = create_subscope(ctx, builder);
    let otherwise_block_id = otherwise_block.block_id;

    arms_vec.push(MatchLeafBuilder {
        arm_index: expr.arms.len() - 1,
        lowerin_result: Ok(()),
        builder: otherwise_block,
    });

    let match_info = MatchInfo::Extern(MatchExternInfo {
        function: function_id,
        inputs: vec![match_input],
        arms: vec![
            MatchArm {
                arm_selector: MatchArmSelector::VariantId(corelib::option_some_variant(
                    semantic_db,
                    bounded_int_ty,
                )),
                block_id: in_range_block_id,
                var_ids: vec![in_range_block_input_var_id],
            },
            MatchArm {
                arm_selector: MatchArmSelector::VariantId(corelib::option_none_variant(
                    semantic_db,
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
        empty_match_info,
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
        .get_flag(FlagId::new(ctx.db.upcast(), "numeric_match_optimization_min_arms_threshold"))
        .map(|flag| match *flag {
            Flag::NumericMatchOptimizationMinArmsThreshold(threshold) => threshold,
            _ => panic!("Wrong type flag `{flag:?}`."),
        })
        .unwrap_or(default_threshold)
}
