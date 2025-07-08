use crate::{db::LoweringGroup, fmt::LoweredFormatter, ids::{FunctionWithBodyLongId, Signature}, test_utils::LoweringDatabaseForTesting, BlockId, VariableId};
use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::ModuleItemId;
use cairo_lang_semantic::{self as semantic, corelib::unit_ty, test_utils::{setup_test_function, TestFunction}, usage::MemberPath, Expr, ExprVarMemberPath, Statement, StatementId};
use cairo_lang_syntax::node::TypedStablePtr;
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::{extract_matches, ordered_hash_map::OrderedHashMap, Upcast};
use itertools::Itertools;
use salsa::{InternId, InternKey};

use super::{block_builder::{merge_block_builders, BlockBuilder}, context::{EncapsulatingLoweringContext, LoweringContext, VarRequest}};

cairo_lang_test_utils::test_file_test!(
    merge_block_builders,
    "src/lower/test_data",
    {
        merge_block_builders: "merge_block_builders",
    },
    test_merge_block_builders
);

fn semantic_param(id: u32) -> semantic::VarId {
    semantic::VarId::Param(semantic::ParamId::from_intern_id(id.into()))
}

fn create_context<'a, 'db>(db: &'db LoweringDatabaseForTesting, test_function: &TestFunction,
    encapsulating_ctx: &'a mut EncapsulatingLoweringContext<'db>) -> LoweringContext<'a, 'db> {

    let lowering_signature = Signature::from_semantic(db, test_function.signature.clone());
    let return_type = lowering_signature.return_type;

    let lowering_function_id = db.intern_lowering_function_with_body(
        FunctionWithBodyLongId::Semantic(test_function.function_id),
    );
    LoweringContext::new(
        encapsulating_ctx,
        lowering_function_id,
        lowering_signature,
        return_type,
    )
    .unwrap()
}

fn test_merge_block_builders(
    inputs: &OrderedHashMap<String, String>,
    _args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    let db = LoweringDatabaseForTesting::default();
    let test_function = setup_test_function(
        &db,
        &inputs["block_definitions"],
        "foo",
        inputs.get("module_code").unwrap_or(&"".into())
    ).unwrap();
    let mut encapsulating_ctx =
         EncapsulatingLoweringContext::new(&db, test_function.function_id).unwrap();

         for semantic_var in &test_function.signature.params {
            encapsulating_ctx.semantic_defs.insert(
                semantic::VarId::Param(semantic_var.id),
                semantic::Binding::Param(semantic_var.clone()),
            );
        }

    let mut ctx = create_context(&db, &test_function, &mut encapsulating_ctx);

    let parameters: Vec<VariableId> = ctx
        .signature
        .params
        .clone()
        .into_iter()
        .map(|param| {
            let location = ctx.get_location(param.stable_ptr().untyped());
            let var = ctx.new_var(VarRequest { ty: param.ty(), location });
            var
        })
        .collect();

    println!("semantic_defs: {:?}", ctx.semantic_defs);

    let dummy_location = ctx.get_location(test_function.signature.stable_ptr.untyped());

    let input_blocks = create_block_builders(&mut ctx, &test_function, &parameters);
    let input_blocks_str = input_blocks.iter().map(|b| b.to_string()).join("\n");

    let merged_block = merge_block_builders(&mut ctx, input_blocks, dummy_location);

    let lowered_formatter = LoweredFormatter::new(db.upcast(), &ctx.variables.variables);
    let lowered_str = ctx.blocks.build().unwrap().iter().map(|(block_id, block)|
        format!("{:?}:\n{:?}\n", block_id.debug(&lowered_formatter),  block.debug(&lowered_formatter))
    ).join("");

    TestRunnerResult {
        outputs: OrderedHashMap::from([
            ("input_blocks".into(), input_blocks_str),
            ("lowered".into(), lowered_str),
            ("merged_block_builder".into(), merged_block.to_string()),
        ]),
        error: None,
    }
}

fn create_block_builders(
    ctx: &mut LoweringContext,
    test_function: &TestFunction,
    parameters: &Vec<VariableId>,
) -> Vec<BlockBuilder> {
    let expr = ctx.function_body.arenas.exprs[test_function.body].clone();
    let block_expr = extract_matches!(expr, Expr::Block);

    block_expr.statements.iter().map(|statement_id| {
        create_block_builder(ctx, test_function, *statement_id, parameters)
    }).collect()
}

fn create_block_builder(
    ctx: &mut LoweringContext,
    test_function: &TestFunction,
    statement_id: StatementId,
    parameters: &Vec<VariableId>,
) -> BlockBuilder {
    let block_id = ctx.blocks.alloc_empty();
    let mut block_builder = BlockBuilder::root(block_id);

    let statement_expr = extract_matches!(
        &ctx.function_body.arenas.statements[statement_id],
        Statement::Expr
    );
    let external_tuple = extract_matches!(
        &ctx.function_body.arenas.exprs[statement_expr.expr],
        Expr::Tuple
    );

    let expr_ids = external_tuple.items.clone();
    for expr_id in expr_ids {
        let inner_tuple = extract_matches!(
            &ctx.function_body.arenas.exprs[expr_id],
            Expr::Tuple
        );
        let lower_var_idx: usize = (&extract_matches!(
            &ctx.function_body.arenas.exprs[inner_tuple.items[1]],
            Expr::Literal
        ).value).try_into().unwrap();

        match &ctx.function_body.arenas.exprs[inner_tuple.items[0]] {
            Expr::MemberAccess(member_access) => {
                let member_path: MemberPath = (member_access.member_path.as_ref().unwrap()).into();
                let mut var = &member_path;
                while let MemberPath::Member{parent: v, ..} = var {
                    var = v;
                };
                let var_id = extract_matches!(var, MemberPath::Var);

                // TODO: Fix [0].
                block_builder.put_semantic(*var_id, parameters[lower_var_idx]);

                let location = ctx.get_location(member_access.stable_ptr.untyped());
                block_builder.get_ref_raw(ctx, &member_path, location);
            }
            Expr::Var(var) => {
                // TODO: Fix [0].
                block_builder.put_semantic(var.var, parameters[lower_var_idx]);
            }
            expr => {
                panic!("Unexpected expression: {:?}", expr);
            }
        }
    }

    block_builder
}
