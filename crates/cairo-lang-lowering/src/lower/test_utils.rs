use {cairo_lang_defs as defs, cairo_lang_semantic as semantic};

use super::context::{EncapsulatingLoweringContext, LoweringContext};
use crate::ids::{EnrichedSemanticSignature, FunctionWithBodyLongId};
use crate::test_utils::LoweringDatabaseForTesting;

/// Creates a [EncapsulatingLoweringContext] for tests.
pub fn create_encapsulating_ctx<'db>(
    db: &'db LoweringDatabaseForTesting,
    function_id: defs::ids::FunctionWithBodyId<'db>,
    signature: &semantic::Signature<'db>,
) -> EncapsulatingLoweringContext<'db> {
    let mut encapsulating_ctx = EncapsulatingLoweringContext::new(db, function_id).unwrap();

    for semantic_var in &signature.params {
        encapsulating_ctx.semantic_defs.insert(
            semantic::VarId::Param(semantic_var.id),
            semantic::Binding::Param(semantic_var.clone()),
        );
    }

    encapsulating_ctx
}

/// Creates a [LoweringContext] for tests.
pub fn create_lowering_context<'a, 'db>(
    db: &'db LoweringDatabaseForTesting,
    function_id: defs::ids::FunctionWithBodyId<'db>,
    signature: &semantic::Signature<'db>,
    encapsulating_ctx: &'a mut EncapsulatingLoweringContext<'db>,
) -> LoweringContext<'db, 'a> {
    let lowering_signature = EnrichedSemanticSignature::from_semantic(db, signature);
    let return_type = lowering_signature.return_type;

    let lowering_function_id =
        crate::ids::FunctionWithBodyId::new(db, FunctionWithBodyLongId::Semantic(function_id));
    LoweringContext::new(encapsulating_ctx, lowering_function_id, lowering_signature, return_type)
        .unwrap()
}
