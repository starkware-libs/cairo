//! Statement generators. Add statements to BlockScope while respecting variable liveness and
//! ownership of OwnedVariable.

use itertools::chain;
use num_bigint::BigInt;
use semantic::{ConcreteEnumId, ConcreteVariant};

use super::{BlockEndInfo, BlockScope, LivingVar};
use crate::lower::context::LoweringContext;
use crate::objects::{
    Statement, StatementCall, StatementLiteral, StatementTupleConstruct, StatementTupleDestructure,
};
use crate::{
    BlockId, StatementCallBlock, StatementEnumConstruct, StatementMatchEnum, StatementMatchExtern,
    VariableId,
};

/// Generator for [StatementLiteral].
pub struct Literal {
    pub value: BigInt,
    pub ty: semantic::TypeId,
}
impl Literal {
    pub fn add(self, ctx: &mut LoweringContext<'_>, scope: &mut BlockScope) -> LivingVar {
        let output = scope.living_variables.introduce_new_var(ctx, self.ty);
        scope.statements.push(Statement::Literal(StatementLiteral {
            value: self.value,
            output: output.var_id(),
        }));
        output
    }
}

/// Generator for [StatementCall].
pub struct Call {
    /// Called function.
    pub function: semantic::FunctionId,
    /// Inputs to function.
    pub inputs: Vec<LivingVar>,
    /// Types for `ref` parameters of the function. An output variable will be introduced for each.
    pub ref_tys: Vec<semantic::TypeId>,
    /// Types for the returns of the function. An output variable will be introduced for each.
    pub ret_tys: Vec<semantic::TypeId>,
}
/// Result of adding a Call statement.
pub struct CallResult {
    /// Output variables for function return value.
    pub returns: Vec<LivingVar>,
    /// Output variables for function `ref` parameters.
    pub ref_outputs: Vec<LivingVar>,
}
impl Call {
    pub fn add(self, ctx: &mut LoweringContext<'_>, scope: &mut BlockScope) -> CallResult {
        let inputs = self
            .inputs
            .into_iter()
            .map(|var| scope.living_variables.use_var(ctx, var).var_id())
            .collect();
        let returns = self
            .ret_tys
            .into_iter()
            .map(|ty| scope.living_variables.introduce_new_var(ctx, ty))
            .collect();
        let ref_outputs = self
            .ref_tys
            .into_iter()
            .map(|ty| scope.living_variables.introduce_new_var(ctx, ty))
            .collect();
        let outputs = chain!(&ref_outputs, &returns).map(|var: &LivingVar| var.var_id()).collect();
        scope.statements.push(Statement::Call(StatementCall {
            function: self.function,
            inputs,
            outputs,
        }));
        CallResult { returns, ref_outputs }
    }
}

/// Generator for [StatementCallBlock].
pub struct CallBlock {
    pub block: BlockId,
    pub end_info: BlockEndInfo,
}
/// Result of adding a CallBlock statement.
pub enum CallBlockResult {
    /// Block returns to call site with output variables.
    Callsite {
        /// Variables for the push (rebind) output variables, that get bound to semantic variables
        /// at the calling scope.
        pushes: Vec<LivingVar>,
        /// Variable for the "block value" output variable if exists.
        maybe_output: Option<LivingVar>,
    },
    /// Block does not return to callsite, and thus the place after the call is unreachable.
    End,
}
impl CallBlock {
    pub fn add(self, ctx: &mut LoweringContext<'_>, scope: &mut BlockScope) -> CallBlockResult {
        let (outputs, res) = process_end_info(ctx, scope, self.end_info);

        // TODO(spapini): Support mut variables.
        scope
            .statements
            .push(Statement::CallBlock(StatementCallBlock { block: self.block, outputs }));
        res
    }
}

/// Generator for [StatementMatchExtern].
pub struct MatchExtern {
    pub function: semantic::FunctionId,
    pub inputs: Vec<LivingVar>,
    pub arms: Vec<BlockId>,
    pub end_info: BlockEndInfo,
}
impl MatchExtern {
    pub fn add(self, ctx: &mut LoweringContext<'_>, scope: &mut BlockScope) -> CallBlockResult {
        let inputs = self
            .inputs
            .into_iter()
            .map(|var| scope.living_variables.use_var(ctx, var).var_id())
            .collect();

        // TODO(lior): Check that each arm has the expected input.

        let (outputs, res) = process_end_info(ctx, scope, self.end_info);
        scope.statements.push(Statement::MatchExtern(StatementMatchExtern {
            function: self.function,
            inputs,
            arms: self.arms,
            outputs,
        }));
        res
    }
}

/// Given a block scope and an end info, extracts output variable ids and a structured
/// representation of them as a [CallBlockResult].
fn process_end_info(
    ctx: &mut LoweringContext<'_>,
    scope: &mut BlockScope,
    end_info: BlockEndInfo,
) -> (Vec<VariableId>, CallBlockResult) {
    let (outputs, res) = match end_info {
        BlockEndInfo::Callsite { maybe_output_ty, push_tys } => {
            let maybe_output =
                maybe_output_ty.map(|ty| scope.living_variables.introduce_new_var(ctx, ty));
            let pushes = push_tys
                .into_iter()
                .map(|ty| scope.living_variables.introduce_new_var(ctx, ty))
                .collect();
            (
                chain!(&pushes, &maybe_output).map(|var: &LivingVar| var.var_id()).collect(),
                CallBlockResult::Callsite { maybe_output, pushes },
            )
        }
        BlockEndInfo::End => (vec![], CallBlockResult::End),
    };
    (outputs, res)
}

/// Generator for [StatementEnumConstruct].
pub struct EnumConstruct {
    pub input: LivingVar,
    pub variant: ConcreteVariant,
}
impl EnumConstruct {
    pub fn add(self, ctx: &mut LoweringContext<'_>, scope: &mut BlockScope) -> LivingVar {
        let input = scope.living_variables.use_var(ctx, self.input).var_id();
        let output = scope.living_variables.introduce_new_var(
            ctx,
            ctx.db.intern_type(semantic::TypeLongId::Concrete(semantic::ConcreteTypeId::Enum(
                self.variant.concrete_enum_id,
            ))),
        );
        scope.statements.push(Statement::EnumConstruct(StatementEnumConstruct {
            variant: self.variant,
            input,
            output: output.var_id(),
        }));
        output
    }
}

/// Generator for [StatementMatchEnum].
pub struct MatchEnum {
    pub input: LivingVar,
    pub concrete_enum_id: ConcreteEnumId,
    pub arms: Vec<(ConcreteVariant, BlockId)>,
    pub end_info: BlockEndInfo,
}
impl MatchEnum {
    pub fn add(self, ctx: &mut LoweringContext<'_>, scope: &mut BlockScope) -> CallBlockResult {
        let input = scope.living_variables.use_var(ctx, self.input).var_id();

        // Check that each arm has a single input of the correct type.
        for (variant, block_id) in &self.arms {
            let input_tys =
                ctx.blocks[*block_id].inputs.iter().map(|var_id| ctx.variables[*var_id].ty);
            itertools::assert_equal([variant.ty].into_iter(), input_tys);
        }

        let (outputs, res) = process_end_info(ctx, scope, self.end_info);
        scope.statements.push(Statement::MatchEnum(StatementMatchEnum {
            concrete_enum: self.concrete_enum_id,
            input,
            arms: self.arms,
            outputs,
        }));
        res
    }
}

/// Generator for [StatementTupleConstruct].
pub struct TupleConstruct {
    pub inputs: Vec<LivingVar>,
    pub ty: semantic::TypeId,
}
impl TupleConstruct {
    pub fn add(self, ctx: &mut LoweringContext<'_>, scope: &mut BlockScope) -> LivingVar {
        let inputs = self
            .inputs
            .into_iter()
            .map(|var| scope.living_variables.use_var(ctx, var).var_id())
            .collect();
        let output = scope.living_variables.introduce_new_var(ctx, self.ty);
        scope.statements.push(Statement::TupleConstruct(StatementTupleConstruct {
            inputs,
            output: output.var_id(),
        }));
        output
    }
}

/// Generator for [StatementTupleDestructure].
pub struct TupleDestructure {
    pub input: LivingVar,
    pub tys: Vec<semantic::TypeId>,
}
impl TupleDestructure {
    pub fn add(self, ctx: &mut LoweringContext<'_>, scope: &mut BlockScope) -> Vec<LivingVar> {
        let input = scope.living_variables.use_var(ctx, self.input).var_id();
        let outputs: Vec<_> = self
            .tys
            .into_iter()
            .map(|ty| scope.living_variables.introduce_new_var(ctx, ty))
            .collect();
        scope.statements.push(Statement::TupleDestructure(StatementTupleDestructure {
            input,
            outputs: outputs.iter().map(|var| var.var_id()).collect(),
        }));
        outputs
    }
}
