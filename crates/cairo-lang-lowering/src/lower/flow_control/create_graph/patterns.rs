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
use crate::lower::context::LoweringContext;

/// Given a list of patterns and the nodes to go to if the pattern matches,
/// returns a new graph node to handle the patterns.
///
/// `location` is the location of the expression initiating the match.
///
/// If `default` is provided, the patterns do not need to be exhaustive, and the default node
/// will be used if no pattern matches.
///
/// TODO: doc build_node_callback.
pub fn create_node_for_patterns(
    ctx: &LoweringContext<'_, '_>,
    graph: &mut FlowControlGraphBuilder,
    input_var: FlowControlVar,
    patterns: &[&semantic::Pattern],
    build_node_callback: &dyn Fn(&mut FlowControlGraphBuilder, FilteredPatterns) -> NodeId,
) -> NodeId {
    let (n_snapshots, long_ty) = peel_snapshots(ctx.db, input_var.ty());
    match long_ty {
        TypeLongId::Concrete(ConcreteTypeId::Enum(concrete_enum_id)) => create_node_for_enum(
            ctx,
            graph,
            input_var,
            concrete_enum_id,
            n_snapshots,
            patterns,
            build_node_callback,
        ),
        _ => todo!("Type {:?} is not supported yet.", long_ty),
    }
}

fn create_node_for_enum(
    ctx: &LoweringContext<'_, '_>,
    graph: &mut FlowControlGraphBuilder,
    input_var: FlowControlVar,
    concrete_enum_id: ConcreteEnumId,
    n_snapshots: usize,
    patterns: &[&semantic::Pattern],
    build_node_callback: &dyn Fn(&mut FlowControlGraphBuilder, FilteredPatterns) -> NodeId,
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
            let inner_var =
                graph.new_var(wrap_in_snapshots(ctx.db, concrete_variant.ty, n_snapshots));
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
