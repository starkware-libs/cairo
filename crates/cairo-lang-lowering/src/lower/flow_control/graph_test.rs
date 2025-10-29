use cairo_lang_debug::DebugWithDb;
use cairo_lang_semantic::expr::fmt::ExprFormatter;
use cairo_lang_semantic::items::function_with_body::FunctionWithBodySemantic;
use cairo_lang_semantic::test_utils::setup_test_function;
use cairo_lang_semantic::{self as semantic, ExprVarMemberPath};
use cairo_lang_syntax::node::TypedStablePtr;
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_test_utils::verify_diagnostics_expectation;
use cairo_lang_utils::extract_matches;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use itertools::Itertools;
use salsa::Database;

use super::create_graph::{create_graph_expr_if, create_graph_expr_match};
use super::graph::FlowControlGraph;
use super::lower_graph::lower_graph;
use crate::Lowered;
use crate::lower::block_builder::BlockBuilder;
use crate::lower::context::{LoweringContext, VarRequest};
use crate::lower::test_utils::{create_encapsulating_ctx, create_lowering_context};
use crate::lower::{
    alloc_empty_block, lowered_expr_to_block_scope_end, wrap_sealed_block_as_function,
};
use crate::objects::blocks::Blocks;
use crate::test_utils::{LoweringDatabaseForTesting, formatted_lowered};

cairo_lang_test_utils::test_file_test!(
    create_graph,
    "src/lower/flow_control/test_data",
    {
        if_: "if",
        match_: "match",
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
    let semantic_db: &dyn Database = db;

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

    let mut ctx = create_lowering_context(
        db,
        test_function.function_id,
        &test_function.signature,
        &mut encapsulating_ctx,
    );

    let graph = match &expr {
        semantic::Expr::If(expr) => create_graph_expr_if(&mut ctx, expr),
        semantic::Expr::Match(expr) => create_graph_expr_match(&mut ctx, expr),
        _ => {
            panic!("Unsupported expression: {:?}", expr.debug(&expr_formatter));
        }
    };

    // Lower the graph.
    let (lowered_str, lowering_diagnostics) =
        if args.get("skip_lowering").unwrap_or(&"false".into()) == "true" {
            ("".into(), ctx.diagnostics.build().format(db))
        } else {
            let lowered = lower_graph_as_function(ctx, expr_id, &graph);
            (formatted_lowered(db, Some(&lowered)), lowered.diagnostics.format(db))
        };

    let error = verify_diagnostics_expectation(
        args,
        &format!("{semantic_diagnostics}{lowering_diagnostics}"),
    );

    TestRunnerResult {
        outputs: OrderedHashMap::from([
            ("graph".into(), format!("{graph:?}")),
            ("semantic_diagnostics".into(), semantic_diagnostics),
            ("lowering_diagnostics".into(), lowering_diagnostics),
            ("lowered".into(), lowered_str),
        ]),
        error,
    }
}

/// Calls [lower_graph] on the expression as if it was a function's body. Returns the [Lowered]
/// object.
fn lower_graph_as_function<'db>(
    mut ctx: LoweringContext<'db, '_>,
    expr_id: semantic::ExprId,
    graph: &FlowControlGraph<'db>,
) -> Lowered<'db> {
    let expr = ctx.function_body.arenas.exprs[expr_id].clone();
    let location = ctx.get_location(expr.stable_ptr().untyped());

    // Create a new block builder.
    let mut builder = BlockBuilder::root(alloc_empty_block(&mut ctx));

    // Add the function parameters to the block's semantics.
    let parameters = ctx
        .signature
        .params
        .clone()
        .into_iter()
        .map(|param| {
            let location = ctx.get_location(param.stable_ptr().untyped());
            let var = ctx.new_var(VarRequest { ty: param.ty(), location });
            let param_var = extract_matches!(param, ExprVarMemberPath::Var);
            builder.put_semantic(param_var.var, var);
            var
        })
        .collect_vec();

    // Lower the graph into the builder.
    let block_expr = lower_graph(&mut ctx, &mut builder, graph, location);

    let block_sealed = match lowered_expr_to_block_scope_end(&mut ctx, builder, block_expr) {
        Ok(block_sealed) => block_sealed,
        Err(diag_added) => {
            return Lowered {
                diagnostics: ctx.diagnostics.build(),
                variables: ctx.variables.variables,
                blocks: Blocks::new_errored(diag_added),
                signature: ctx.signature.into(),
                parameters,
            };
        }
    };

    let expr = ctx.function_body.arenas.exprs[expr_id].clone();

    wrap_sealed_block_as_function(&mut ctx, block_sealed, expr.stable_ptr().untyped()).unwrap();

    let blocks = ctx.blocks.build().expect("Root block must exist.");
    Lowered {
        diagnostics: ctx.diagnostics.build(),
        variables: ctx.variables.variables,
        blocks,
        signature: ctx.signature.into(),
        parameters,
    }
}
