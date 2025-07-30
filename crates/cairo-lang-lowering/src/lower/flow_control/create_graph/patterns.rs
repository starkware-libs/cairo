use cairo_lang_semantic::items::enm::SemanticEnumEx;
use cairo_lang_semantic::types::{peel_snapshots, wrap_in_snapshots};
use cairo_lang_semantic::{
    self as semantic, ConcreteEnumId, ConcreteTypeId, PatternEnumVariant, TypeLongId,
};
use itertools::{Itertools, zip_eq};

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
    patterns: &[&semantic::Pattern<'db>],
    build_node_callback: BuildNodeCallback<'db, '_>,
    location: LocationId<'db>,
) -> NodeId {
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
    patterns: &[&semantic::Pattern<'db>],
    build_node_callback: BuildNodeCallback<'db, '_>,
    location: LocationId<'db>,
) -> NodeId {
    let concrete_variants = ctx.db.concrete_enum_variants(concrete_enum_id).unwrap();

    // Maps variant index to the list of the indices of the patterns that match it.
    let mut variant_to_pattern_indices: Vec<FilteredPatterns> =
        (0..concrete_variants.len()).map(|_| FilteredPatterns::empty()).collect_vec();

    for (idx, pattern) in patterns.iter().enumerate() {
        match pattern {
            semantic::Pattern::EnumVariant(PatternEnumVariant { variant, .. }) => {
                variant_to_pattern_indices[variant.idx].add(idx);
            }
            _ => todo!("Pattern {:?} is not supported yet.", pattern),
        }
    }

    // Create a node in the graph for each variant.
    let variants = zip_eq(concrete_variants, variant_to_pattern_indices)
        .map(|(concrete_variant, pattern_indices)| {
            let inner_var = graph
                .new_var(wrap_in_snapshots(ctx.db, concrete_variant.ty, n_snapshots), location);
            // TODO(lior): Handle the inner patterns by calling `create_node_for_patterns`
            //   recursively on the patterns that match the variant.
            let node = build_node_callback(graph, pattern_indices);
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
