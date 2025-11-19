//! Statement generators. Add statements to BlockBuilder while respecting variable liveness and
//! ownership of OwnedVariable.

use cairo_lang_semantic as semantic;
use cairo_lang_semantic::ConcreteVariant;
use cairo_lang_semantic::items::constant::ConstValueId;
use cairo_lang_utils::{Intern, extract_matches};
use itertools::chain;

use super::VariableId;
use super::context::VarRequest;
use crate::ids::LocationId;
use crate::lower::context::LoweringContext;
use crate::objects::{
    Statement, StatementCall, StatementConst, StatementStructConstruct, StatementStructDestructure,
    VarUsage,
};
use crate::{StatementDesnap, StatementEnumConstruct, StatementSnapshot};

#[derive(Clone, Default)]
pub struct StatementsBuilder<'db> {
    pub statements: Vec<Statement<'db>>,
}
impl<'db> StatementsBuilder<'db> {
    /// Adds a statement to the block.
    pub fn push_statement(&mut self, statement: Statement<'db>) {
        self.statements.push(statement);
    }
}

/// Generator for [StatementConst].
pub struct Const<'db> {
    pub value: ConstValueId<'db>,
    pub location: LocationId<'db>,
    // TODO(TomerStarkware): Remove this field and use the type from value.
    pub ty: semantic::TypeId<'db>,
}
impl<'db> Const<'db> {
    pub fn add(
        self,
        ctx: &mut LoweringContext<'db, '_>,
        builder: &mut StatementsBuilder<'db>,
    ) -> VarUsage<'db> {
        let output = ctx.new_var(VarRequest { ty: self.ty, location: self.location });
        builder.push_statement(Statement::Const(StatementConst::new_flat(self.value, output)));
        VarUsage { var_id: output, location: self.location }
    }
}

/// Generator for [StatementCall].
/// Note that builder.finalize_statement() must be called manually after ref bindings.
pub struct Call<'db> {
    /// Called function.
    pub function: crate::ids::FunctionId<'db>,
    /// Inputs to function.
    pub inputs: Vec<VarUsage<'db>>,
    /// The `__coupon__` input to the function, if exists.
    pub coupon_input: Option<VarUsage<'db>>,
    /// Types for `ref` parameters of the function. An output variable will be introduced for each.
    pub extra_ret_tys: Vec<semantic::TypeId<'db>>,
    /// Types for the returns of the function. An output variable will be introduced for each.
    pub ret_tys: Vec<semantic::TypeId<'db>>,
    /// Location associated with this statement.
    pub location: LocationId<'db>,
}
impl<'db> Call<'db> {
    /// Adds a call statement to the builder.
    pub fn add(
        self,
        ctx: &mut LoweringContext<'db, '_>,
        builder: &mut StatementsBuilder<'db>,
    ) -> CallResult<'db> {
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
        let outputs = chain!(&extra_outputs, &returns)
            .map(|var_usage: &VarUsage<'_>| var_usage.var_id)
            .collect();

        let with_coupon = self.coupon_input.is_some();
        let mut inputs = self.inputs;
        inputs.extend(self.coupon_input);
        builder.push_statement(Statement::Call(StatementCall {
            function: self.function,
            inputs,
            with_coupon,
            outputs,
            location: self.location,
            is_specialization_base_call: false,
        }));

        CallResult { returns, extra_outputs }
    }
}
/// Result of adding a Call statement.
pub struct CallResult<'db> {
    /// Output variables for function's return value.
    pub returns: Vec<VarUsage<'db>>,
    /// Output variables for function's `ref` parameters.
    pub extra_outputs: Vec<VarUsage<'db>>,
}

/// Generator for [StatementEnumConstruct].
pub struct EnumConstruct<'db> {
    pub input: VarUsage<'db>,
    pub variant: ConcreteVariant<'db>,
    pub location: LocationId<'db>,
}
impl<'db> EnumConstruct<'db> {
    pub fn add(
        self,
        ctx: &mut LoweringContext<'db, '_>,
        builder: &mut StatementsBuilder<'db>,
    ) -> VarUsage<'db> {
        let ty = semantic::TypeLongId::Concrete(semantic::ConcreteTypeId::Enum(
            self.variant.concrete_enum_id,
        ))
        .intern(ctx.db);
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
pub struct Snapshot<'db> {
    pub input: VarUsage<'db>,
    pub location: LocationId<'db>,
}
impl<'db> Snapshot<'db> {
    pub fn add(
        self,
        ctx: &mut LoweringContext<'db, '_>,
        builder: &mut StatementsBuilder<'db>,
    ) -> (VariableId, VariableId) {
        let input_var = &ctx.variables[self.input.var_id];
        let input_ty = input_var.ty;
        let ty = semantic::TypeLongId::Snapshot(input_ty).intern(ctx.db);

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
pub struct Desnap<'db> {
    pub input: VarUsage<'db>,
    pub location: LocationId<'db>,
}
impl<'db> Desnap<'db> {
    pub fn add(
        self,
        ctx: &mut LoweringContext<'db, '_>,
        builder: &mut StatementsBuilder<'db>,
    ) -> VarUsage<'db> {
        let ty = extract_matches!(
            ctx.variables[self.input.var_id].ty.long(ctx.db),
            semantic::TypeLongId::Snapshot
        );
        let output = ctx.new_var(VarRequest { ty: *ty, location: self.location });
        builder.push_statement(Statement::Desnap(StatementDesnap { input: self.input, output }));
        VarUsage { var_id: output, location: self.location }
    }
}

/// Generator for [StatementStructDestructure].
///
/// Note that we return `Vec<VariableId>` rather than `Vec<VarUsage>` as the caller typically
/// has a more accurate location than the one we have in the var requests.
pub struct StructDestructure<'db> {
    /// Variable that holds the struct value.
    pub input: VarUsage<'db>,
    /// Variable requests for the newly generated member values.
    pub var_reqs: Vec<VarRequest<'db>>,
}
impl<'db> StructDestructure<'db> {
    pub fn add(
        self,
        ctx: &mut LoweringContext<'db, '_>,
        builder: &mut StatementsBuilder<'db>,
    ) -> Vec<VariableId> {
        let outputs: Vec<_> = self.var_reqs.into_iter().map(|req| ctx.new_var(req)).collect();
        builder.push_statement(Statement::StructDestructure(StatementStructDestructure {
            input: self.input,
            outputs: outputs.clone(),
        }));
        outputs
    }
}

/// Generator for [StatementStructDestructure] as member access.
pub struct StructMemberAccess<'db> {
    pub input: VarUsage<'db>,
    pub member_tys: Vec<semantic::TypeId<'db>>,
    pub member_idx: usize,
    pub location: LocationId<'db>,
}
impl<'db> StructMemberAccess<'db> {
    pub fn add(
        self,
        ctx: &mut LoweringContext<'db, '_>,
        builder: &mut StatementsBuilder<'db>,
    ) -> VarUsage<'db> {
        VarUsage {
            var_id: StructDestructure {
                input: self.input,
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
pub struct StructConstruct<'db> {
    pub inputs: Vec<VarUsage<'db>>,
    pub ty: semantic::TypeId<'db>,
    pub location: LocationId<'db>,
}
impl<'db> StructConstruct<'db> {
    pub fn add(
        self,
        ctx: &mut LoweringContext<'db, '_>,
        builder: &mut StatementsBuilder<'db>,
    ) -> VarUsage<'db> {
        let output = ctx.new_var(VarRequest { ty: self.ty, location: self.location });
        builder.push_statement(Statement::StructConstruct(StatementStructConstruct {
            inputs: self.inputs,
            output,
        }));
        VarUsage { var_id: output, location: self.location }
    }
}
