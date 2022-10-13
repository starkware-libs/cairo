//! Statement generators. Add statements to BlockScope while respecting variable liveness and
//! ownership of OwnedVariable.

use itertools::chain;
use semantic::{ConcreteEnumId, ConcreteVariant};

use super::{BlockEndInfo, BlockScope, OwnedVariable};
use crate::lower::context::LoweringContext;
use crate::objects::{
    Statement, StatementCall, StatementLiteral, StatementTupleConstruct, StatementTupleDestruct,
};
use crate::{BlockId, StatementCallBlock, StatementMatchEnum, VariableId};

/// Generator for StatementMatchEnum.
pub struct MatchEnum {
    pub input: OwnedVariable,
    pub concrete_enum_id: ConcreteEnumId,
    pub arms: Vec<(ConcreteVariant, BlockId)>,
    pub end_info: BlockEndInfo,
}
impl MatchEnum {
    pub fn add(self, ctx: &mut LoweringContext<'_>, scope: &mut BlockScope) -> CallBlockResult {
        let input = scope.use_var(ctx, self.input);

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

/// Generator for StatementTupleConstruct.
pub struct TupleConstruct {
    pub inputs: Vec<OwnedVariable>,
    pub ty: semantic::TypeId,
}
impl TupleConstruct {
    pub fn add(self, ctx: &mut LoweringContext<'_>, scope: &mut BlockScope) -> OwnedVariable {
        let inputs = self.inputs.into_iter().map(|var| scope.use_var(ctx, var)).collect();
        let output = scope.introduce_variable(ctx, self.ty);
        scope
            .statements
            .push(Statement::TupleConstruct(StatementTupleConstruct { inputs, output: output.0 }));
        output
    }
}

/// Generator for StatementTupleDestruct.
pub struct TupleDestruct {
    pub input: OwnedVariable,
    pub tys: Vec<semantic::TypeId>,
}
impl TupleDestruct {
    pub fn add(self, ctx: &mut LoweringContext<'_>, scope: &mut BlockScope) -> Vec<OwnedVariable> {
        let input = scope.use_var(ctx, self.input);
        let outputs: Vec<_> =
            self.tys.into_iter().map(|ty| scope.introduce_variable(ctx, ty)).collect();
        scope.statements.push(Statement::TupleDestruct(StatementTupleDestruct {
            input,
            outputs: outputs.iter().map(|var| var.0).collect(),
        }));
        outputs
    }
}

/// Generator for StatementCall.
pub struct Call {
    pub function: semantic::FunctionId,
    pub inputs: Vec<OwnedVariable>,
    pub ret_ty: semantic::TypeId,
}
impl Call {
    pub fn add(self, ctx: &mut LoweringContext<'_>, scope: &mut BlockScope) -> OwnedVariable {
        let inputs = self.inputs.into_iter().map(|var| scope.use_var(ctx, var)).collect();
        let output = scope.introduce_variable(ctx, self.ret_ty);
        // TODO(spapini): Support mut variables.
        scope.statements.push(Statement::Call(StatementCall {
            function: self.function,
            inputs,
            outputs: vec![output.0],
        }));
        output
    }
}

/// Generator for StatementCallBlock.
pub struct CallBlock {
    pub block: BlockId,
    pub end_info: BlockEndInfo,
}
/// Result of adding a CallBlock statement.
pub enum CallBlockResult {
    /// Block returns to call site with output variables.
    Callsite {
        /// Variable for the "block value" output variable if exists.
        maybe_output: Option<OwnedVariable>,
        /// Variables for the push (rebind) output variables, that get bound to semantic variables
        /// at the calling scope.
        pushes: Vec<OwnedVariable>,
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

/// Given a block scope and an end info, extracts output variable ids and a structured
/// representation of them as a [CallBlockResult].
fn process_end_info(
    ctx: &mut LoweringContext<'_>,
    scope: &mut BlockScope,
    end_info: BlockEndInfo,
) -> (Vec<VariableId>, CallBlockResult) {
    let (outputs, res) = match end_info {
        BlockEndInfo::Callsite { maybe_output_ty, push_tys } => {
            let maybe_output = maybe_output_ty.map(|ty| scope.introduce_variable(ctx, ty));
            let pushes = push_tys.into_iter().map(|ty| scope.introduce_variable(ctx, ty)).collect();
            (
                chain!(&maybe_output, &pushes).map(|var| var.0).collect(),
                CallBlockResult::Callsite { maybe_output, pushes },
            )
        }
        BlockEndInfo::End => (vec![], CallBlockResult::End),
    };
    (outputs, res)
}

/// Generator for StatementLiteral.
pub struct Literal {
    // TODO(spapini): Fix literal type.
    pub value: usize,
    pub ty: semantic::TypeId,
}
impl Literal {
    pub fn add(self, ctx: &mut LoweringContext<'_>, scope: &mut BlockScope) -> OwnedVariable {
        let output = scope.introduce_variable(ctx, self.ty);
        scope
            .statements
            .push(Statement::Literal(StatementLiteral { value: self.value, output: output.0 }));
        output
    }
}
