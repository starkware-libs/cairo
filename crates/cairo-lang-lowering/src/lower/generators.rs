//! Statement generators. Add statements to BlockBuilder while respecting variable liveness and
//! ownership of OwnedVariable.

use cairo_lang_defs::diagnostic_utils::StableLocationOption;
use cairo_lang_semantic as semantic;
use cairo_lang_semantic::ConcreteVariant;
use cairo_lang_utils::extract_matches;
use itertools::chain;
use num_bigint::BigInt;

use super::context::VarRequest;
use super::VariableId;
use crate::lower::context::LoweringContext;
use crate::objects::{
    Statement, StatementCall, StatementLiteral, StatementStructConstruct,
    StatementStructDestructure,
};
use crate::{StatementDesnap, StatementEnumConstruct, StatementSnapshot};

#[derive(Clone, Default)]
pub struct StatementsBuilder {
    pub statements: Vec<Statement>,
}
impl StatementsBuilder {
    /// Adds a statement to the block.
    pub fn push_statement(&mut self, statement: Statement) {
        self.statements.push(statement);
    }
}

/// Generator for [StatementLiteral].
pub struct Literal {
    pub value: BigInt,
    pub location: StableLocationOption,
    pub ty: semantic::TypeId,
}
impl Literal {
    pub fn add(
        self,
        ctx: &mut LoweringContext<'_, '_>,
        scope: &mut StatementsBuilder,
    ) -> VariableId {
        let output = ctx.new_var(VarRequest { ty: self.ty, location: self.location });
        scope.push_statement(Statement::Literal(StatementLiteral { value: self.value, output }));
        output
    }
}

/// Generator for [StatementCall].
/// Note that scope.finalize_statement() must be called manually after ref bindings.
pub struct Call {
    /// Called function.
    pub function: crate::ids::FunctionId,
    /// Inputs to function.
    pub inputs: Vec<VariableId>,
    /// Types for `ref` parameters of the function. An output variable will be introduced for each.
    pub extra_ret_tys: Vec<semantic::TypeId>,
    /// Types for the returns of the function. An output variable will be introduced for each.
    pub ret_tys: Vec<semantic::TypeId>,
    /// Location associated with this statement.
    pub location: StableLocationOption,
}
impl Call {
    /// Adds a call statement to the scope.
    pub fn add(
        self,
        ctx: &mut LoweringContext<'_, '_>,
        scope: &mut StatementsBuilder,
    ) -> CallResult {
        let returns = self
            .ret_tys
            .into_iter()
            .map(|ty| ctx.new_var(VarRequest { ty, location: self.location }))
            .collect();
        let extra_outputs = self
            .extra_ret_tys
            .into_iter()
            .map(|ty| ctx.new_var(VarRequest { ty, location: self.location }))
            .collect();
        let outputs = chain!(&extra_outputs, &returns).copied().collect();

        scope.push_statement(Statement::Call(StatementCall {
            function: self.function,
            inputs: self.inputs,
            outputs,
            location: self.location,
        }));

        CallResult { returns, extra_outputs }
    }
}
/// Result of adding a Call statement.
pub struct CallResult {
    /// Output variables for function's return value.
    pub returns: Vec<VariableId>,
    /// Output variables for function's `ref` parameters.
    pub extra_outputs: Vec<VariableId>,
}

/// Generator for [StatementEnumConstruct].
pub struct EnumConstruct {
    pub input: VariableId,
    pub variant: ConcreteVariant,
    pub location: StableLocationOption,
}
impl EnumConstruct {
    pub fn add(
        self,
        ctx: &mut LoweringContext<'_, '_>,
        scope: &mut StatementsBuilder,
    ) -> VariableId {
        let ty = ctx.db.intern_type(semantic::TypeLongId::Concrete(
            semantic::ConcreteTypeId::Enum(self.variant.concrete_enum_id),
        ));
        let output = ctx.new_var(VarRequest { ty, location: self.location });
        scope.push_statement(Statement::EnumConstruct(StatementEnumConstruct {
            variant: self.variant,
            input: self.input,
            output,
        }));
        output
    }
}

/// Generator for [StatementSnapshot].
pub struct Snapshot {
    pub input: VariableId,
    pub location: StableLocationOption,
}
impl Snapshot {
    pub fn add(
        self,
        ctx: &mut LoweringContext<'_, '_>,
        scope: &mut StatementsBuilder,
    ) -> (VariableId, VariableId) {
        let input_ty = ctx.variables[self.input].ty;
        let ty = ctx.db.intern_type(semantic::TypeLongId::Snapshot(input_ty));
        let output_original = ctx.new_var(VarRequest { ty: input_ty, location: self.location });
        let output_snapshot = ctx.new_var(VarRequest { ty, location: self.location });
        scope.push_statement(Statement::Snapshot(StatementSnapshot {
            input: self.input,
            output_original,
            output_snapshot,
        }));
        (output_original, output_snapshot)
    }
}

/// Generator for [StatementDesnap].
pub struct Desnap {
    pub input: VariableId,
    pub location: StableLocationOption,
}
impl Desnap {
    pub fn add(
        self,
        ctx: &mut LoweringContext<'_, '_>,
        scope: &mut StatementsBuilder,
    ) -> VariableId {
        let ty = extract_matches!(
            ctx.db.lookup_intern_type(ctx.variables[self.input].ty),
            semantic::TypeLongId::Snapshot
        );
        let output = ctx.new_var(VarRequest { ty, location: self.location });
        scope.push_statement(Statement::Desnap(StatementDesnap { input: self.input, output }));
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
    pub fn add(
        self,
        ctx: &mut LoweringContext<'_, '_>,
        scope: &mut StatementsBuilder,
    ) -> Vec<VariableId> {
        let outputs: Vec<_> = self.var_reqs.into_iter().map(|req| ctx.new_var(req)).collect();
        scope.push_statement(Statement::StructDestructure(StatementStructDestructure {
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
    pub location: StableLocationOption,
}
impl StructMemberAccess {
    pub fn add(
        self,
        ctx: &mut LoweringContext<'_, '_>,
        scope: &mut StatementsBuilder,
    ) -> VariableId {
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
    pub location: StableLocationOption,
}
impl StructConstruct {
    pub fn add(
        self,
        ctx: &mut LoweringContext<'_, '_>,
        scope: &mut StatementsBuilder,
    ) -> VariableId {
        let output = ctx.new_var(VarRequest { ty: self.ty, location: self.location });
        scope.push_statement(Statement::StructConstruct(StatementStructConstruct {
            inputs: self.inputs,
            output,
        }));
        output
    }
}
