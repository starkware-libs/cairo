//! Statement generators. Add statements to BlockScope while respecting variable liveness and
//! ownership of OwnedVariable.

use itertools::chain;

use super::{BlockEndInfo, BlockScope, OwnedVariable};
use crate::lower::context::LoweringContext;
use crate::objects::{
    Statement, StatementCall, StatementLiteral, StatementTupleConstruct, StatementTupleDestruct,
};
use crate::{BlockId, StatementCallBlock};

/// Generator for StatementTupleConstruct.
pub struct TupleConstruct {
    pub inputs: Vec<OwnedVariable>,
    pub ty: semantic::TypeId,
}
impl TupleConstruct {
    pub fn add(self, ctx: &mut LoweringContext<'_>, scope: &mut BlockScope) -> OwnedVariable {
        let inputs = self.inputs.into_iter().map(|var| scope.get_var(ctx, var)).collect();
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
        let input = scope.get_var(ctx, self.input);
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
        let inputs = self.inputs.into_iter().map(|var| scope.get_var(ctx, var)).collect();
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

#[allow(dead_code)]
/// Generator for StatementCallBlock.
pub struct CallBlock {
    pub block: BlockId,
    pub end_info: BlockEndInfo,
}
#[allow(dead_code)]
pub enum CallBlockResult {
    Callsite { maybe_output: Option<OwnedVariable>, pushes: Vec<OwnedVariable> },
    End,
}
#[allow(dead_code)]
impl CallBlock {
    pub fn add(self, ctx: &mut LoweringContext<'_>, scope: &mut BlockScope) -> CallBlockResult {
        let (outputs, res) = match self.end_info {
            BlockEndInfo::Callsite { maybe_output_ty, push_tys } => {
                let maybe_output = maybe_output_ty.map(|ty| scope.introduce_variable(ctx, ty));
                let pushes =
                    push_tys.into_iter().map(|ty| scope.introduce_variable(ctx, ty)).collect();
                (
                    chain!(&maybe_output, &pushes).map(|var| var.0).collect(),
                    CallBlockResult::Callsite { maybe_output, pushes },
                )
            }
            BlockEndInfo::End => (vec![], CallBlockResult::End),
        };

        // TODO(spapini): Support mut variables.
        scope
            .statements
            .push(Statement::CallBlock(StatementCallBlock { block: self.block, outputs }));
        res
    }
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
