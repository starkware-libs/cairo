//! Statement generators. Add statements to BlockBuilder while respecting variable liveness and
//! ownership of OwnedVariable.

use cairo_lang_semantic as semantic;
use cairo_lang_semantic::ConcreteVariant;
use itertools::chain;
use num_bigint::BigInt;

use super::{BlockBuilder, VariableId};
use crate::lower::context::LoweringContext;
use crate::objects::{
    Statement, StatementCall, StatementLiteral, StatementStructConstruct,
    StatementStructDestructure,
};
use crate::StatementEnumConstruct;

/// Generator for [StatementLiteral].
pub struct Literal {
    pub value: BigInt,
    pub ty: semantic::TypeId,
}
impl Literal {
    pub fn add(self, ctx: &mut LoweringContext<'_>, scope: &mut BlockBuilder) -> VariableId {
        let output = ctx.new_var(self.ty);
        scope.push_finalized_statement(Statement::Literal(StatementLiteral {
            value: self.value,
            output,
        }));
        output
    }
}

/// Generator for [StatementCall].
/// Note that scope.finalize_statement() must be called manually after ref bindings.
pub struct Call {
    /// Called function.
    pub function: semantic::FunctionId,
    /// Inputs to function.
    pub inputs: Vec<VariableId>,
    /// Types for `ref` parameters of the function. An output variable will be introduced for each.
    pub ref_tys: Vec<semantic::TypeId>,
    /// Types for the returns of the function. An output variable will be introduced for each.
    pub ret_tys: Vec<semantic::TypeId>,
}
impl Call {
    /// Adds a call statement to the scope.
    pub fn add(self, ctx: &mut LoweringContext<'_>, scope: &mut BlockBuilder) -> CallResult {
        let returns = self.ret_tys.into_iter().map(|ty| ctx.new_var(ty)).collect();
        let implicit_outputs = ctx
            .db
            .function_all_implicits(self.function)
            .unwrap_or_default()
            .into_iter()
            .map(|ty| ctx.new_var(ty))
            .collect();
        let ref_outputs = self.ref_tys.into_iter().map(|ty| ctx.new_var(ty)).collect();
        let outputs = chain!(&implicit_outputs, &ref_outputs, &returns).copied().collect();

        scope.push_statement(Statement::Call(StatementCall {
            function: self.function,
            inputs: self.inputs,
            outputs,
        }));

        CallResult { returns, ref_outputs, implicit_outputs }
    }
}
/// Result of adding a Call statement.
pub struct CallResult {
    /// Output variables for function's return value.
    pub returns: Vec<VariableId>,
    /// Output variables for function's `ref` parameters.
    pub ref_outputs: Vec<VariableId>,
    /// Output variables for function's implicit parameters.
    pub implicit_outputs: Vec<VariableId>,
}

/// Generator for [StatementEnumConstruct].
pub struct EnumConstruct {
    pub input: VariableId,
    pub variant: ConcreteVariant,
}
impl EnumConstruct {
    pub fn add(self, ctx: &mut LoweringContext<'_>, scope: &mut BlockBuilder) -> VariableId {
        let output = ctx.new_var(ctx.db.intern_type(semantic::TypeLongId::Concrete(
            semantic::ConcreteTypeId::Enum(self.variant.concrete_enum_id),
        )));
        scope.push_finalized_statement(Statement::EnumConstruct(StatementEnumConstruct {
            variant: self.variant,
            input: self.input,
            output,
        }));
        output
    }
}

/// Generator for [StatementStructDestructure].
pub struct StructDestructure {
    pub input: VariableId,
    pub tys: Vec<semantic::TypeId>,
}
impl StructDestructure {
    pub fn add(self, ctx: &mut LoweringContext<'_>, scope: &mut BlockBuilder) -> Vec<VariableId> {
        let outputs: Vec<_> = self.tys.into_iter().map(|ty| ctx.new_var(ty)).collect();
        scope.push_finalized_statement(Statement::StructDestructure(StatementStructDestructure {
            input: self.input,
            outputs: outputs.clone(),
        }));
        outputs
    }
}

/// Generator for [StatementStructDestructure] as member access.
pub struct StructMemberAccess {
    pub input: VariableId,
    pub member_tys: Vec<semantic::TypeId>,
    pub member_idx: usize,
}
impl StructMemberAccess {
    pub fn add(self, ctx: &mut LoweringContext<'_>, scope: &mut BlockBuilder) -> VariableId {
        StructDestructure { input: self.input, tys: self.member_tys }
            .add(ctx, scope)
            .remove(self.member_idx)
    }
}

/// Generator for [StatementStructConstruct].
pub struct StructConstruct {
    pub inputs: Vec<VariableId>,
    pub ty: semantic::TypeId,
}
impl StructConstruct {
    pub fn add(self, ctx: &mut LoweringContext<'_>, scope: &mut BlockBuilder) -> VariableId {
        let output = ctx.new_var(self.ty);
        scope.push_finalized_statement(Statement::StructConstruct(StatementStructConstruct {
            inputs: self.inputs,
            output,
        }));
        output
    }
}
