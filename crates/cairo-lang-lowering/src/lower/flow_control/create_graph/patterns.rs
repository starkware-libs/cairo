use cairo_lang_semantic::items::enm::SemanticEnumEx;
use cairo_lang_semantic::types::{peel_snapshots, wrap_in_snapshots};
use cairo_lang_semantic::{
    self as semantic, ConcreteEnumId, ConcreteTypeId, PatternEnumVariant, TypeLongId,
};
use cairo_lang_utils::iterators::zip_eq3;
use itertools::Itertools;

use super::super::graph::{
    EnumMatch, FlowControlGraphBuilder, FlowControlNode, FlowControlVar, NodeId,
};
use super::filtered_patterns::FilteredPatterns;
use crate::ids::LocationId;
use crate::lower::context::LoweringContext;

/// A callback that gets a [FilteredPatterns] and constructs a node that continues the pattern
/// matching restricted to the filtered patterns.
///
/// For example, consider the following match:
/// ```plain
/// match (x, y) {
///     (_, C) => 0
///     (B, _) => 1
///     _ => 2
/// }
/// ```
///
/// The pattern-matching function that handles the tuple will call the pattern-matching function
/// for `x` with the following three patterns: (0) `_`, (1) `B`, (2) `_`.
///
/// The inner function will compute the filtered patterns for each variant of `x`.
/// For `x=A`, the filtered patterns are `[0, 2]`.
/// Then, the callback will be called with the filter, and it will return a node that
/// checks if `y=C` or not, and returns `0` or `2` respectively.
/// For `x=B`, the filtered patterns are `[0, 1, 2]`, and the callback will return a node that
/// returns `0` if `y=C` and `1` otherwise.
/// Finally, the inner pattern-matching function (for `x`) will construct a [EnumMatch] node
/// that leads to the two nodes returned by the callback.
type BuildNodeCallback<'db, 'a> =
    &'a dyn Fn(&mut FlowControlGraphBuilder<'db>, FilteredPatterns) -> NodeId;

/// A thin wrapper around [semantic::Pattern], where `None` represents the `_` pattern.
pub type Pattern<'a, 'db> = Option<&'a semantic::Pattern<'db>>;

/// Given a list of patterns and the nodes to go to if the pattern matches,
/// returns a new graph node to handle the patterns.
///
/// `location` is the location of the expression initiating the match.
///
/// If `default` is provided, the patterns do not need to be exhaustive, and the default node
/// will be used if no pattern matches.
pub fn create_node_for_patterns<'db>(
    ctx: &LoweringContext<'db, '_>,
    graph: &mut FlowControlGraphBuilder<'db>,
    input_var: FlowControlVar,
    patterns: &[Pattern<'_, 'db>],
    build_node_callback: BuildNodeCallback<'db, '_>,
    location: LocationId<'db>,
) -> NodeId {
    // If all the patterns are catch-all, we do not need to look into `input_var`.
    if patterns.iter().all(|pattern| pattern_is_any(pattern)) {
        // Call the callback with all patterns accepted.
        return build_node_callback(graph, FilteredPatterns::all(patterns.len()));
    }

    let (n_snapshots, long_ty) = peel_snapshots(ctx.db, graph.var_ty(input_var));
    match long_ty {
        TypeLongId::Concrete(ConcreteTypeId::Enum(concrete_enum_id)) => create_node_for_enum(
            ctx,
            graph,
            input_var,
            concrete_enum_id,
            n_snapshots,
            patterns,
            build_node_callback,
            location,
        ),
        _ => todo!("Type {:?} is not supported yet.", long_ty),
    }
}

/// Creates an [EnumMatch] node for the given `input_var` and `patterns`.
#[allow(clippy::too_many_arguments)]
fn create_node_for_enum<'db>(
    ctx: &LoweringContext<'db, '_>,
    graph: &mut FlowControlGraphBuilder<'db>,
    input_var: FlowControlVar,
    concrete_enum_id: ConcreteEnumId<'db>,
    n_snapshots: usize,
    patterns: &[Pattern<'_, 'db>],
    build_node_callback: BuildNodeCallback<'db, '_>,
    location: LocationId<'db>,
) -> NodeId {
    let concrete_variants = ctx.db.concrete_enum_variants(concrete_enum_id).unwrap();

    // Maps variant index to the list of the indices of the patterns that match it.
    let mut variant_to_pattern_indices: Vec<FilteredPatterns> =
        (0..concrete_variants.len()).map(|_| FilteredPatterns::empty()).collect_vec();

    // Maps variant index to the list of the inner patterns.
    // For example, a pattern `A(B(x))` will add the (inner) pattern `B(x)` to the vector at the
    // index of the variant `A`.
    let mut variant_to_inner_patterns: Vec<Vec<Pattern<'_, 'db>>> =
        vec![vec![]; concrete_variants.len()];

    for (idx, pattern) in patterns.iter().enumerate() {
        match pattern {
            Some(semantic::Pattern::EnumVariant(PatternEnumVariant {
                variant,
                inner_pattern,
                ..
            })) => {
                variant_to_pattern_indices[variant.idx].add(idx);
                variant_to_inner_patterns[variant.idx]
                    .push(inner_pattern.map(|inner_pattern| get_pattern(ctx, inner_pattern)));
            }
            Some(semantic::Pattern::Otherwise(..)) | None => {
                // Add `idx` to all the variants.
                for pattern_indices in variant_to_pattern_indices.iter_mut() {
                    pattern_indices.add(idx);
                }
                // Add the `_` pattern (represented by `None`) to all the variants.
                for inner_patterns in variant_to_inner_patterns.iter_mut() {
                    inner_patterns.push(None);
                }
            }
            _ => todo!("Pattern {:?} is not supported yet.", pattern),
        }
    }

    // Create a node in the graph for each variant.
    let variants =
        zip_eq3(concrete_variants, variant_to_pattern_indices, variant_to_inner_patterns)
            .map(|(concrete_variant, pattern_indices, inner_patterns)| {
                let inner_var = graph
                    .new_var(wrap_in_snapshots(ctx.db, concrete_variant.ty, n_snapshots), location);
                let node = create_node_for_patterns(
                    ctx,
                    graph,
                    inner_var,
                    &inner_patterns,
                    &|graph, pattern_indices_inner| {
                        build_node_callback(graph, pattern_indices_inner.lift(&pattern_indices))
                    },
                    location,
                );
                (concrete_variant, node, inner_var)
            })
            .collect_vec();

    // Create a node for the match.
    graph.add_node(FlowControlNode::EnumMatch(EnumMatch {
        matched_var: input_var,
        concrete_enum_id,
        variants,
    }))
}

/// Returns `true` if the pattern accepts any value (`_` or a variable name).
fn pattern_is_any(pattern: &Pattern<'_, '_>) -> bool {
    match pattern {
        Some(semantic_pattern) => match semantic_pattern {
            semantic::Pattern::Otherwise(..) | semantic::Pattern::Variable(..) => true,
            semantic::Pattern::Literal(..)
            | semantic::Pattern::StringLiteral(..)
            | semantic::Pattern::Struct(..)
            | semantic::Pattern::Tuple(..)
            | semantic::Pattern::FixedSizeArray(..)
            | semantic::Pattern::EnumVariant(..)
            | semantic::Pattern::Missing(..) => false,
        },
        None => true,
    }
}

/// Returns a reference to a [semantic::Pattern] from a [semantic::PatternId].
pub fn get_pattern<'db, 'a>(
    ctx: &'a LoweringContext<'db, '_>,
    semantic_pattern: semantic::PatternId,
) -> &'a semantic::Pattern<'db> {
    &ctx.function_body.arenas.patterns[semantic_pattern]
}
