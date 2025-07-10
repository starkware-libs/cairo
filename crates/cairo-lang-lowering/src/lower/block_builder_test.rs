use cairo_lang_debug::DebugWithDb;
use cairo_lang_semantic::corelib::unit_ty;
use cairo_lang_semantic::test_utils::{TestFunction, setup_test_function};
use cairo_lang_semantic::usage::MemberPath;
use cairo_lang_semantic::{self as semantic, Expr, ExprVarMemberPath, Statement, StatementId};
use cairo_lang_syntax::node::TypedStablePtr;
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::unordered_hash_set::UnorderedHashSet;
use cairo_lang_utils::{Upcast, extract_matches};
use itertools::Itertools;

use super::block_builder::{BlockBuilder, merge_block_builders};
use super::context::{EncapsulatingLoweringContext, LoweringContext, VarRequest};
use crate::db::LoweringGroup;
use crate::fmt::LoweredFormatter;
use crate::ids::{FunctionWithBodyLongId, Signature};
use crate::test_utils::LoweringDatabaseForTesting;
use crate::{BlockId, VariableId};

const N_LOWERING_VARS: usize = 100;

cairo_lang_test_utils::test_file_test!(
    merge_block_builders,
    "src/lower/test_data",
    {
        merge_block_builders: "merge_block_builders",
    },
    test_merge_block_builders
);

fn create_context<'a, 'db>(
    db: &'db LoweringDatabaseForTesting,
    test_function: &TestFunction,
    encapsulating_ctx: &'a mut EncapsulatingLoweringContext<'db>,
) -> LoweringContext<'a, 'db> {
    let lowering_signature = Signature::from_semantic(db, test_function.signature.clone());
    let return_type = lowering_signature.return_type;

    let lowering_function_id = db.intern_lowering_function_with_body(
        FunctionWithBodyLongId::Semantic(test_function.function_id),
    );
    LoweringContext::new(encapsulating_ctx, lowering_function_id, lowering_signature, return_type)
        .unwrap()
}

fn test_merge_block_builders(
    inputs: &OrderedHashMap<String, String>,
    _args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    let db = LoweringDatabaseForTesting::default();
    let test_function = setup_test_function(
        &db,
        &format!("{} {{ {} }}", &inputs["function_signature"], &inputs["block_definitions"]),
        "foo",
        inputs.get("module_code").unwrap_or(&"".into()),
    )
    .unwrap();
    let mut encapsulating_ctx =
        EncapsulatingLoweringContext::new(&db, test_function.function_id).unwrap();

    for semantic_var in &test_function.signature.params {
        encapsulating_ctx.semantic_defs.insert(
            semantic::VarId::Param(semantic_var.id),
            semantic::Binding::Param(semantic_var.clone()),
        );
    }

    let mut ctx = create_context(&db, &test_function, &mut encapsulating_ctx);

    // Create dummy lowering variables.
    let dummy_location = ctx.get_location(test_function.signature.stable_ptr.untyped());
    let lowering_vars: Vec<VariableId> = (0..N_LOWERING_VARS)
        .map(|_| {
            let var = ctx.new_var(VarRequest { ty: unit_ty(ctx.db), location: dummy_location });
            var
        })
        .collect();

    let input_blocks = create_block_builders(&mut ctx, &test_function, &lowering_vars);
    let input_blocks_str = input_blocks.iter().map(|b| b.to_string()).join("\n");

    let merged_block = merge_block_builders(&mut ctx, input_blocks, dummy_location);

    let lowered_formatter = LoweredFormatter::new(db.upcast(), &ctx.variables.variables);
    let lowered_str = ctx
        .blocks
        .build()
        .unwrap()
        .iter()
        .map(|(block_id, block)| {
            format!(
                "{:?}:\n{:?}\n",
                block_id.debug(&lowered_formatter),
                block.debug(&lowered_formatter)
            )
        })
        .join("");

    TestRunnerResult {
        outputs: OrderedHashMap::from([
            ("input_blocks".into(), input_blocks_str),
            ("lowered".into(), lowered_str),
            ("merged_block_builder".into(), merged_block.to_string()),
        ]),
        error: None,
    }
}

/// Creates a block builder for each semantic "statement" in the function body.
///
/// See [create_block_builder] for more details.
fn create_block_builders(
    ctx: &mut LoweringContext,
    test_function: &TestFunction,
    lowering_vars: &Vec<VariableId>,
) -> Vec<BlockBuilder> {
    let expr = ctx.function_body.arenas.exprs[test_function.body].clone();
    let block_expr = extract_matches!(expr, Expr::Block);

    block_expr
        .statements
        .iter()
        .map(|statement_id| create_block_builder(ctx, *statement_id, lowering_vars))
        .collect()
}

/// Given a semantic "statement" of the form:
///    `((member_path, lower_var_idx), ...)`
/// creates a block builder with a semantic mapping that maps each member path to the corresponding
/// given lowered variable.
///
/// Note that the statement is not a real statement - it is not lowered, and it is only used to
/// define the semantic mapping.
fn create_block_builder(
    ctx: &mut LoweringContext,
    statement_id: StatementId,
    lowering_vars: &Vec<VariableId>,
) -> BlockBuilder {
    let block_id = ctx.blocks.alloc_empty();
    let mut block_builder = BlockBuilder::root(block_id);
    let mut visited_vars: UnorderedHashSet<semantic::VarId> = Default::default();

    let statement_expr =
        extract_matches!(&ctx.function_body.arenas.statements[statement_id], Statement::Expr);
    let external_tuple =
        extract_matches!(&ctx.function_body.arenas.exprs[statement_expr.expr], Expr::Tuple);

    let expr_ids = external_tuple.items.clone();
    for expr_id in expr_ids {
        let inner_tuple = extract_matches!(&ctx.function_body.arenas.exprs[expr_id], Expr::Tuple);
        let lower_var_idx: usize = (&extract_matches!(
            &ctx.function_body.arenas.exprs[inner_tuple.items[1]],
            Expr::Literal
        )
        .value)
            .try_into()
            .unwrap();

        match &ctx.function_body.arenas.exprs[inner_tuple.items[0]] {
            Expr::MemberAccess(member_access) => {
                let member_path: MemberPath = (member_access.member_path.as_ref().unwrap()).into();
                let mut var = &member_path;
                while let MemberPath::Member { parent: v, .. } = var {
                    var = v;
                }
                let var_id = extract_matches!(var, MemberPath::Var);

                if visited_vars.insert(*var_id) {
                    block_builder.put_semantic(*var_id, lowering_vars[lower_var_idx]);
                }

                let location = ctx.get_location(member_access.stable_ptr.untyped());
                block_builder.update_ref_raw(
                    ctx,
                    member_path,
                    lowering_vars[lower_var_idx],
                    location,
                );
                // Remove the statements that were created as part of the `update_ref_raw` call.
                block_builder.statements.statements.clear();
            }
            Expr::Var(var) => {
                block_builder.put_semantic(var.var, lowering_vars[lower_var_idx]);
            }
            expr => {
                panic!("Unexpected expression: {:?}", expr);
            }
        }
    }

    block_builder
}
