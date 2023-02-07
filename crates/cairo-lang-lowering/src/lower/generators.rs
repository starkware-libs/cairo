//! Statement generators. Add statements to BlockBuilder while respecting variable liveness and
//! ownership of OwnedVariable.

use cairo_lang_defs::diagnostic_utils::StableLocation;
use cairo_lang_semantic as semantic;
use cairo_lang_semantic::ConcreteVariant;
use itertools::chain;
use num_bigint::BigInt;

use super::context::VarRequest;
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
    pub location: StableLocation,
    pub ty: semantic::TypeId,
}
impl Literal {
    pub fn add(self, ctx: &mut LoweringContext<'_>, scope: &mut BlockBuilder) -> VariableId {
        let output = ctx.new_var(VarRequest { ty: self.ty, location: self.location });
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
    /// Location associated with this statement.
    pub location: StableLocation,
}
impl Call {
    /// Adds a call statement to the scope.
    pub fn add(self, ctx: &mut LoweringContext<'_>, scope: &mut BlockBuilder) -> CallResult {
        let returns = self
            .ret_tys
            .into_iter()
            .map(|ty| ctx.new_var(VarRequest { ty, location: self.location }))
            .collect();
        let implicit_outputs = ctx
            .db
            .function_all_implicits(self.function)
            .unwrap_or_default()
            .into_iter()
            .map(|ty| ctx.new_var(VarRequest { ty, location: self.location }))
            .collect();
        let ref_outputs = self
            .ref_tys
            .into_iter()
            .map(|ty| ctx.new_var(VarRequest { ty, location: self.location }))
            .collect();
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
    pub location: StableLocation,
}
impl EnumConstruct {
    pub fn add(self, ctx: &mut LoweringContext<'_>, scope: &mut BlockBuilder) -> VariableId {
        let ty = ctx.db.intern_type(semantic::TypeLongId::Concrete(
            semantic::ConcreteTypeId::Enum(self.variant.concrete_enum_id),
        ));
        let output = ctx.new_var(VarRequest { ty, location: self.location });
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
    /// Variable that holds the struct value.
    pub input: VariableId,
    /// Variable requests for the newly generated member values.
    pub var_reqs: Vec<VarRequest>,
}
impl StructDestructure {
    pub fn add(self, ctx: &mut LoweringContext<'_>, scope: &mut BlockBuilder) -> Vec<VariableId> {
        let outputs: Vec<_> = self.var_reqs.into_iter().map(|req| ctx.new_var(req)).collect();
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
    pub location: StableLocation,
}
impl StructMemberAccess {
    pub fn add(self, ctx: &mut LoweringContext<'_>, scope: &mut BlockBuilder) -> VariableId {
        StructDestructure {
            input: self.input,
            var_reqs: self
                .member_tys
                .into_iter()
                .map(|ty| VarRequest { ty, location: self.location })
                .collect(),
        }
        .add(ctx, scope)
        .remove(self.member_idx)
    }
}

/// Generator for [StatementStructConstruct].
pub struct StructConstruct {
    pub inputs: Vec<VariableId>,
    pub ty: semantic::TypeId,
    pub location: StableLocation,
}
impl StructConstruct {
    pub fn add(self, ctx: &mut LoweringContext<'_>, scope: &mut BlockBuilder) -> VariableId {
        let output = ctx.new_var(VarRequest { ty: self.ty, location: self.location });
        scope.push_finalized_statement(Statement::StructConstruct(StatementStructConstruct {
            inputs: self.inputs,
            output,
        }));
        output
    }
}
