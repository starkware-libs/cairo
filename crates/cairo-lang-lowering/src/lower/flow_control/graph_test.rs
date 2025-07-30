use cairo_lang_debug::DebugWithDb;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::expr::fmt::ExprFormatter;
use cairo_lang_semantic::test_utils::setup_test_function;
use cairo_lang_semantic::{self as semantic, ExprVarMemberPath};
use cairo_lang_syntax::node::TypedStablePtr;
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_test_utils::verify_diagnostics_expectation;
use cairo_lang_utils::extract_matches;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use itertools::Itertools;

use super::create_graph::create_graph_expr_if;
use super::graph::FlowControlGraph;
use super::lower_graph::lower_graph;
use crate::Lowered;
use crate::lower::block_builder::BlockBuilder;
use crate::lower::context::{LoweringContext, VarRequest};
use crate::lower::test_utils::{create_encapsulating_ctx, create_lowering_context};
use crate::lower::{
    alloc_empty_block, lowered_expr_to_block_scope_end, wrap_sealed_block_as_function,
};
use crate::test_utils::{LoweringDatabaseForTesting, formatted_lowered};

cairo_lang_test_utils::test_file_test!(
    create_graph,
    "src/lower/flow_control/test_data",
    {
        if_: "if",
    },
    test_create_graph
);

fn test_create_graph(
    inputs: &OrderedHashMap<String, String>,
    args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    let db = &mut LoweringDatabaseForTesting::default();
    let (test_function, semantic_diagnostics) = setup_test_function(
        db,
        inputs["function_code"].as_str(),
        "foo",
        inputs.get("module_code").unwrap_or(&"".into()),
    )
    .split();
    let semantic_db: &dyn SemanticGroup = db;

    // Extract the expression from the function's body.
    let semantic::Expr::Block(semantic::ExprBlock { tail: Some(expr_id), .. }) =
        semantic_db.expr_semantic(test_function.function_id, test_function.body)
    else {
        panic!("Expected a block expression.");
    };

    let expr = semantic_db.expr_semantic(test_function.function_id, expr_id);
    let expr_formatter = ExprFormatter { db, function_id: test_function.function_id };

    let mut encapsulating_ctx =
        create_encapsulating_ctx(db, test_function.function_id, &test_function.signature);

    let ctx = create_lowering_context(
        db,
        test_function.function_id,
        &test_function.signature,
        &mut encapsulating_ctx,
    );

    let graph = match &expr {
        semantic::Expr::If(expr) => create_graph_expr_if(&ctx, expr),
        _ => {
            panic!("Unsupported expression: {:?}", expr.debug(&expr_formatter));
        }
    };

    let error = verify_diagnostics_expectation(args, &semantic_diagnostics);

    // Lower the graph.
    let lowered_str = if args.get("skip_lowering").unwrap_or(&"false".into()) == "true" {
        "".into()
    } else {
        let lowered = lower_graph_as_function(ctx, expr_id, &graph);
        formatted_lowered(db, Some(&lowered))
    };

    TestRunnerResult {
        outputs: OrderedHashMap::from([
            ("graph".into(), format!("{graph:?}")),
            ("semantic_diagnostics".into(), semantic_diagnostics),
            ("lowered".into(), lowered_str),
        ]),
        error,
    }
}

/// Calls [lower_graph] on the expression as if it was a function's body. Returns the [Lowered]
/// object.
fn lower_graph_as_function<'db>(
    mut ctx: LoweringContext<'db, '_>,
    expr_id: semantic::ExprId<'db>,
    graph: &FlowControlGraph<'db>,
) -> Lowered<'db> {
    let ctx_ref: &mut LoweringContext<'db, '_> = &mut ctx;
    let expr = ctx_ref.function_body.arenas.exprs[expr_id].clone();
    let location = ctx_ref.get_location(expr.stable_ptr().untyped());

    // Create a new block builder.
    let mut builder = BlockBuilder::root(alloc_empty_block(ctx_ref));

    // Add the function parameters to the block's semantics.
    let parameters = ctx_ref
        .signature
        .params
        .clone()
        .into_iter()
        .map(|param| {
            let location = ctx_ref.get_location(param.stable_ptr().untyped());
            let var = ctx_ref.new_var(VarRequest { ty: param.ty(), location });
            let param_var = extract_matches!(param, ExprVarMemberPath::Var);
            builder.put_semantic(param_var.var, var);
            var
        })
        .collect_vec();

    // Lower the graph into the builder.
    let block_expr = lower_graph(ctx_ref, &mut builder, graph, location);

    let block_sealed = lowered_expr_to_block_scope_end(ctx_ref, builder, block_expr).unwrap();

    let expr = ctx_ref.function_body.arenas.exprs[expr_id].clone();

    wrap_sealed_block_as_function(ctx_ref, block_sealed, expr.stable_ptr().untyped()).unwrap();

    let blocks = std::mem::take(&mut ctx_ref.blocks).build().expect("Root block must exist.");
    Lowered {
        diagnostics: std::mem::take(&mut ctx_ref.diagnostics).build(),
        variables: std::mem::take(&mut ctx_ref.variables.variables),
        blocks,
        signature: ctx_ref.signature.clone(),
        parameters,
    }
}
