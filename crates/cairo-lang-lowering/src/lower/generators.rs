//! Statement generators. Add statements to BlockBuilder while respecting variable liveness and
//! ownership of OwnedVariable.

use cairo_lang_semantic as semantic;
use cairo_lang_semantic::ConcreteVariant;
use cairo_lang_utils::extract_matches;
use itertools::chain;
use semantic::items::constant::ConstValue;

use super::context::VarRequest;
use super::VariableId;
use crate::ids::LocationId;
use crate::lower::context::LoweringContext;
use crate::objects::{
    Statement, StatementCall, StatementConst, StatementStructConstruct, StatementStructDestructure,
    VarUsage,
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

/// Generator for [StatementConst].
pub struct Const {
    pub value: ConstValue,
    pub location: LocationId,
    pub ty: semantic::TypeId,
}
impl Const {
    pub fn add(
        self,
        ctx: &mut LoweringContext<'_, '_>,
        builder: &mut StatementsBuilder,
    ) -> VarUsage {
        let output = ctx.new_var(VarRequest { ty: self.ty, location: self.location });
        builder.push_statement(Statement::Const(StatementConst { value: self.value, output }));
        VarUsage { var_id: output, location: self.location }
    }
}

/// Generator for [StatementCall].
/// Note that builder.finalize_statement() must be called manually after ref bindings.
pub struct Call {
    /// Called function.
    pub function: crate::ids::FunctionId,
    /// Inputs to function.
    pub inputs: Vec<VarUsage>,
    /// The `__coupon__` input to the function, if exists.
    pub coupon_input: Option<VarUsage>,
    /// Types for `ref` parameters of the function. An output variable will be introduced for each.
    pub extra_ret_tys: Vec<semantic::TypeId>,
    /// Types for the returns of the function. An output variable will be introduced for each.
    pub ret_tys: Vec<semantic::TypeId>,
    /// Location associated with this statement.
    pub location: LocationId,
}
impl Call {
    /// Adds a call statement to the builder.
    pub fn add(
        self,
        ctx: &mut LoweringContext<'_, '_>,
        builder: &mut StatementsBuilder,
    ) -> CallResult {
        let returns = self
            .ret_tys
            .into_iter()
            .map(|ty| ctx.new_var_usage(VarRequest { ty, location: self.location }))
            .collect();
        let extra_outputs = self
            .extra_ret_tys
            .into_iter()
            .map(|ty| ctx.new_var_usage(VarRequest { ty, location: self.location }))
            .collect();
        let outputs =
            chain!(&extra_outputs, &returns).map(|var_usage: &VarUsage| var_usage.var_id).collect();

        let with_coupon = self.coupon_input.is_some();
        let mut inputs = self.inputs;
        inputs.extend(self.coupon_input);
        builder.push_statement(Statement::Call(StatementCall {
            function: self.function,
            inputs,
            with_coupon,
            outputs,
            location: self.location,
        }));

        CallResult { returns, extra_outputs }
    }
}
/// Result of adding a Call statement.
pub struct CallResult {
    /// Output variables for function's return value.
    pub returns: Vec<VarUsage>,
    /// Output variables for function's `ref` parameters.
    pub extra_outputs: Vec<VarUsage>,
}

/// Generator for [StatementEnumConstruct].
pub struct EnumConstruct {
    pub input: VarUsage,
    pub variant: ConcreteVariant,
    pub location: LocationId,
}
impl EnumConstruct {
    pub fn add(
        self,
        ctx: &mut LoweringContext<'_, '_>,
        builder: &mut StatementsBuilder,
    ) -> VarUsage {
        let ty = ctx.db.intern_type(semantic::TypeLongId::Concrete(
            semantic::ConcreteTypeId::Enum(self.variant.concrete_enum_id),
        ));
        let output = ctx.new_var(VarRequest { ty, location: self.location });
        builder.push_statement(Statement::EnumConstruct(StatementEnumConstruct {
            variant: self.variant,
            input: self.input,
            output,
        }));
        VarUsage { var_id: output, location: self.location }
    }
}

/// Generator for [StatementSnapshot].
pub struct Snapshot {
    pub input: VarUsage,
    pub location: LocationId,
}
impl Snapshot {
    pub fn add(
        self,
        ctx: &mut LoweringContext<'_, '_>,
        builder: &mut StatementsBuilder,
    ) -> (VariableId, VariableId) {
        let input_var = &ctx.variables[self.input.var_id];
        let input_ty = input_var.ty;
        let ty = ctx.db.intern_type(semantic::TypeLongId::Snapshot(input_ty));

        // The location of the original input var is likely to be more relevant to the user.
        let output_original =
            ctx.new_var(VarRequest { ty: input_ty, location: input_var.location });
        let output_snapshot = ctx.new_var(VarRequest { ty, location: self.location });
        builder.push_statement(Statement::Snapshot(StatementSnapshot::new(
            self.input,
            output_original,
            output_snapshot,
        )));
        (output_original, output_snapshot)
    }
}

/// Generator for [StatementDesnap].
pub struct Desnap {
    pub input: VarUsage,
    pub location: LocationId,
}
impl Desnap {
    pub fn add(
        self,
        ctx: &mut LoweringContext<'_, '_>,
        builder: &mut StatementsBuilder,
    ) -> VarUsage {
        let ty = extract_matches!(
            ctx.db.lookup_intern_type(ctx.variables[self.input.var_id].ty),
            semantic::TypeLongId::Snapshot
        );
        let output = ctx.new_var(VarRequest { ty, location: self.location });
        builder.push_statement(Statement::Desnap(StatementDesnap { input: self.input, output }));
        VarUsage { var_id: output, location: self.location }
    }
}

/// Generator for [StatementStructDestructure].
///
/// Note that we return `Vec<VariableId>` rather then `Vec<VarUsage>` as the the caller typically
/// has a more accurate location then the one we have in the var requests.
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
        builder: &mut StatementsBuilder,
    ) -> Vec<VariableId> {
        let outputs: Vec<_> = self.var_reqs.into_iter().map(|req| ctx.new_var(req)).collect();
        builder.push_statement(Statement::StructDestructure(StatementStructDestructure {
            // TODO(ilya): Fix to usage location.
            input: VarUsage { var_id: self.input, location: ctx.variables[self.input].location },
            outputs: outputs.clone(),
        }));
        outputs
    }
}

/// Generator for [StatementStructDestructure] as member access.
pub struct StructMemberAccess {
    pub input: VarUsage,
    pub member_tys: Vec<semantic::TypeId>,
    pub member_idx: usize,
    pub location: LocationId,
}
impl StructMemberAccess {
    pub fn add(
        self,
        ctx: &mut LoweringContext<'_, '_>,
        builder: &mut StatementsBuilder,
    ) -> VarUsage {
        VarUsage {
            var_id: StructDestructure {
                input: self.input.var_id,
                var_reqs: self
                    .member_tys
                    .into_iter()
                    .map(|ty| VarRequest { ty, location: self.location })
                    .collect(),
            }
            .add(ctx, builder)
            .remove(self.member_idx),
            location: self.location,
        }
    }
}

/// Generator for [StatementStructConstruct].
pub struct StructConstruct {
    pub inputs: Vec<VarUsage>,
    pub ty: semantic::TypeId,
    pub location: LocationId,
}
impl StructConstruct {
    pub fn add(
        self,
        ctx: &mut LoweringContext<'_, '_>,
        builder: &mut StatementsBuilder,
    ) -> VarUsage {
        let output = ctx.new_var(VarRequest { ty: self.ty, location: self.location });
        builder.push_statement(Statement::StructConstruct(StatementStructConstruct {
            inputs: self.inputs,
            output,
        }));
        VarUsage { var_id: output, location: self.location }
    }
}
