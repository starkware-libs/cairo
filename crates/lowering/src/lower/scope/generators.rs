//! Statement generators. Add statements to BlockScope while respecting variable liveness and
//! ownership of OwnedVariable.

use itertools::chain;
use num_bigint::BigInt;
use semantic::{ConcreteEnumId, ConcreteVariant};

use super::{BlockEndInfo, BlockScope, LivingVar};
use crate::lower::context::LoweringContext;
use crate::lower::external::extern_facade_return_tys;
use crate::objects::{
    Statement, StatementCall, StatementLiteral, StatementStructConstruct,
    StatementStructDestructure,
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
            ty: self.ty,
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
impl Call {
    /// Adds a call statement to the scope.
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
        let implicit_outputs = ctx
            .db
            .function_all_implicits(self.function)
            .unwrap()
            .into_iter()
            .map(|ty| scope.living_variables.introduce_new_var(ctx, ty))
            .collect();
        let ref_outputs = self
            .ref_tys
            .into_iter()
            .map(|ty| scope.living_variables.introduce_new_var(ctx, ty))
            .collect();
        let outputs = chain!(&implicit_outputs, &ref_outputs, &returns)
            .map(|var: &LivingVar| var.var_id())
            .collect();

        scope.statements.push(Statement::Call(StatementCall {
            function: self.function,
            inputs,
            outputs,
        }));

        CallResult { returns, ref_outputs, implicit_outputs }
    }
}
/// Result of adding a Call statement.
pub struct CallResult {
    /// Output variables for function's return value.
    pub returns: Vec<LivingVar>,
    /// Output variables for function's `ref` parameters.
    pub ref_outputs: Vec<LivingVar>,
    /// Output variables for function's implicit parameters.
    pub implicit_outputs: Vec<LivingVar>,
}

/// Generator for [StatementCallBlock].
pub struct CallBlock {
    pub block: BlockId,
    pub end_info: BlockEndInfo,
}
/// Result of adding a CallBlock statement.
#[derive(Debug)]
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
    /// The arms of the match. Order must be identical to the order in the definition of the enum.
    pub arms: Vec<(ConcreteVariant, BlockId)>,
    pub end_info: BlockEndInfo,
}
impl MatchExtern {
    pub fn add(self, ctx: &mut LoweringContext<'_>, scope: &mut BlockScope) -> CallBlockResult {
        let inputs = self
            .inputs
            .into_iter()
            .map(|var| scope.living_variables.use_var(ctx, var).var_id())
            .collect();

        for (variant, block_id) in &self.arms {
            let variant_facade_types = extern_facade_return_tys(ctx, variant.ty);
            let block_input_tys =
                ctx.blocks[*block_id].inputs.iter().map(|var_id| ctx.variables[*var_id].ty);

            // Compare the types of the inputs excluding implicits and args, to the types of the
            // variant.
            let extern_function_id = self
                .function
                .try_get_extern_function_id(ctx.db.upcast())
                .expect("Expected an extern function");
            let num_implicits =
                ctx.db.extern_function_declaration_implicits(extern_function_id).unwrap().len();
            let num_refs =
                ctx.db.extern_function_declaration_refs(extern_function_id).unwrap().len();
            let total_extra_inputs = num_implicits + num_refs;
            itertools::assert_equal(
                variant_facade_types.into_iter(),
                block_input_tys.skip(total_extra_inputs),
            );
        }

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
    /// The arms of the match. Order must be identical to the order in the definition of the enum.
    pub arms: Vec<(ConcreteVariant, BlockId)>,
    pub end_info: BlockEndInfo,
}
impl MatchEnum {
    pub fn add(self, ctx: &mut LoweringContext<'_>, scope: &mut BlockScope) -> CallBlockResult {
        let input = scope.living_variables.use_var(ctx, self.input).var_id();

        // Check that each arm has a single input of the correct type.
        for (variant, block_id) in &self.arms {
            let block_input_tys =
                ctx.blocks[*block_id].inputs.iter().map(|var_id| ctx.variables[*var_id].ty);
            itertools::assert_equal([variant.ty].into_iter(), block_input_tys);
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

/// Generator for [StatementStructDestructure].
pub struct StructDestructure {
    pub input: LivingVar,
    pub tys: Vec<semantic::TypeId>,
}
impl StructDestructure {
    pub fn add(self, ctx: &mut LoweringContext<'_>, scope: &mut BlockScope) -> Vec<LivingVar> {
        let input = scope.living_variables.use_var(ctx, self.input).var_id();
        let outputs: Vec<_> = self
            .tys
            .into_iter()
            .map(|ty| scope.living_variables.introduce_new_var(ctx, ty))
            .collect();
        scope.statements.push(Statement::StructDestructure(StatementStructDestructure {
            input,
            outputs: outputs.iter().map(|var| var.var_id()).collect(),
        }));
        outputs
    }
}

/// Generator for [StatementStructDestructure] as member access.
pub struct StructMemberAccess {
    pub input: LivingVar,
    pub member_tys: Vec<semantic::TypeId>,
    pub member_idx: usize,
}
impl StructMemberAccess {
    pub fn add(self, ctx: &mut LoweringContext<'_>, scope: &mut BlockScope) -> LivingVar {
        StructDestructure { input: self.input, tys: self.member_tys }
            .add(ctx, scope)
            .remove(self.member_idx)
    }
}

/// Generator for [StatementStructConstruct].
pub struct StructConstruct {
    pub inputs: Vec<LivingVar>,
    pub ty: semantic::TypeId,
}
impl StructConstruct {
    pub fn add(self, ctx: &mut LoweringContext<'_>, scope: &mut BlockScope) -> LivingVar {
        let inputs = self
            .inputs
            .into_iter()
            .map(|var| scope.living_variables.use_var(ctx, var).var_id())
            .collect();
        let output = scope.living_variables.introduce_new_var(ctx, self.ty);
        scope.statements.push(Statement::StructConstruct(StatementStructConstruct {
            inputs,
            output: output.var_id(),
        }));
        output
    }
}
