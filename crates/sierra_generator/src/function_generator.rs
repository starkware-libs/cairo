#[cfg(test)]
#[path = "function_generator_test.rs"]
mod test;

use std::collections::{HashMap, HashSet};

use defs::ids::{FreeFunctionId, GenericFunctionId};
use itertools::{chain, Itertools};
use sierra::ids::VarId;
use sierra::program::{GenBranchTarget, Param};
use utils::ordered_hash_set::OrderedHashSet;

use crate::db::SierraGenGroup;
use crate::expr_generator::generate_expression_code;
use crate::expr_generator_context::ExprGeneratorContext;
use crate::pre_sierra::{self, LabelId, Statement};
use crate::utils::{return_statement, simple_statement};

pub fn generate_function_code(
    db: &dyn SierraGenGroup,
    function_id: FreeFunctionId,
    function_semantic: semantic::FreeFunction,
) -> pre_sierra::Function {
    let mut context = ExprGeneratorContext::new(db, function_id);

    // Generate a label for the function's body.
    let (label, label_id) = context.new_label();

    // Generate Sierra variables for the function parameters.
    let parameters: Vec<sierra::program::Param> = function_semantic
        .signature
        .params
        .iter()
        .map(|param| {
            let sierra_var = context.allocate_sierra_variable();
            // TODO(orizi): Propagate the diagnostics or extract `get_concrete_type_id` usage out of
            // this function.
            context.register_variable(defs::ids::VarId::Param(param.id), sierra_var.clone());
            sierra::program::Param {
                id: sierra_var,
                ty: db.get_concrete_type_id(param.ty).expect("got unexpected diagnostics").unwrap(),
            }
        })
        .collect();
    // TODO(orizi): Propagate the diagnostics or extract `get_concrete_type_id` usage out of this
    // function.
    let ret_types = vec![
        db.get_concrete_type_id(function_semantic.signature.return_type)
            .expect("got unexpected diagnostics")
            .unwrap(),
    ];

    let mut statements: Vec<pre_sierra::Statement> = vec![label];

    // Generate the function's body.
    let (body_statements, res) = generate_expression_code(&mut context, function_semantic.body);
    statements.extend(body_statements);

    // Copy the result to the top of the stack before returning.
    let return_variable_on_stack = context.allocate_sierra_variable();
    statements.push(simple_statement(
        // TODO(lior): Use the real type instead of `felt`.
        context.store_temp_libfunc_id(context.get_db().core_felt_ty()),
        &[res],
        &[return_variable_on_stack.clone()],
    ));

    statements.push(return_statement(vec![return_variable_on_stack]));
    let statements = add_dups_and_ignores(&mut context, &parameters, &statements);

    pre_sierra::Function {
        id: db.intern_function(db.intern_concrete_function(semantic::ConcreteFunctionLongId {
            generic_function: GenericFunctionId::Free(function_id),
            // TODO(lior): Add generic arguments.
            generic_args: vec![],
        })),
        body: statements,
        entry_point: label_id,
        parameters,
        ret_types,
    }
}

/// Adds ignores and duplicates of felts to the sierra code.
/// Assumes no VarId is reused in the same line.
// TODO(orizi): Currently only supports felt types.
fn add_dups_and_ignores(
    context: &mut ExprGeneratorContext<'_>,
    params: &[Param],
    statements: &[Statement],
) -> Vec<Statement> {
    let next_statement_index_fetch = NextStatementIndexFetch::new(statements);
    let mut statement_required_vars = vec![OrderedHashSet::<VarId>::default(); statements.len()];
    calculate_required_vars_per_statement(
        context,
        &next_statement_index_fetch,
        &mut statement_required_vars,
        0,
        statements,
    );

    // Maintaining the set of existing vars per original statement, while calcualting the new set of
    // statements including required dups and ignores.
    let mut statement_existing_vars = vec![OrderedHashSet::<VarId>::default(); statements.len()];
    statement_existing_vars[0] = params.iter().map(|p| p.id.clone()).collect();
    statements
        .iter()
        .enumerate()
        .flat_map(|(i, statement)| match statement {
            Statement::Sierra(sierra::program::GenStatement::Invocation(invocation)) => {
                let mut expanded_statement = vec![];
                for var in statement_existing_vars[i].iter() {
                    if !statement_required_vars[i].contains(var) {
                        expanded_statement.push(simple_statement(
                            context.felt_ignore_libfunc_id(),
                            &[var.clone()],
                            &[],
                        ));
                    }
                }
                // Finding the next required vars without the currently inserted results.
                let next_required_vars =
                    OrderedHashSet::from_iter(invocation.branches.iter().flat_map(|branch| {
                        let next_index = next_statement_index_fetch.get(i, &branch.target);
                        let results: HashSet<VarId> = branch.results.iter().cloned().collect();
                        statement_required_vars[next_index]
                            .iter()
                            .filter(move |v| !results.contains(v))
                            .cloned()
                    }));
                // Updating `statement_existing_vars` for the branchs - which are all the
                // `next_required_vars` with the branches inserted results.
                for branch in &invocation.branches {
                    let next_index = next_statement_index_fetch.get(i, &branch.target);
                    statement_existing_vars[next_index] =
                        chain!(next_required_vars.iter(), branch.results.iter()).cloned().collect();
                }
                // Vars needs to be duplicated if they are used in any of the next branches.
                let mut vars_to_dup_if_used = next_required_vars;
                let mut invocation = invocation.clone();
                // Updating all args with duplicates if required.
                for arg in &mut invocation.args {
                    if vars_to_dup_if_used.contains(arg) {
                        let usage_var = context.allocate_sierra_variable();
                        expanded_statement.push(simple_statement(
                            context.felt_dup_libfunc_id(),
                            &[arg.clone()],
                            &[arg.clone(), usage_var.clone()],
                        ));
                        *arg = usage_var;
                    } else {
                        // If this var is used again in this call it must be duplicated.
                        vars_to_dup_if_used.insert(arg.clone());
                    }
                }
                expanded_statement
                    .push(Statement::Sierra(sierra::program::GenStatement::Invocation(invocation)));
                expanded_statement
            }
            Statement::Sierra(sierra::program::GenStatement::Return(_)) => {
                let mut expanded_statement = vec![];
                // Ignoring all variables not part of the return.
                for var in statement_existing_vars[i].iter() {
                    if !statement_required_vars[i].contains(var) {
                        expanded_statement.push(simple_statement(
                            context.felt_ignore_libfunc_id(),
                            &[var.clone()],
                            &[],
                        ));
                    }
                }
                expanded_statement.push(statement.clone());
                expanded_statement
            }
            Statement::Label(_) => {
                // Label is a no-op - so we do no changes to it.
                statement_existing_vars[i + 1] = statement_existing_vars[i].clone();
                vec![statement.clone()]
            }
        })
        .collect()
}

/// Calculates the set of required existing variables per pre-Sierra statement.
fn calculate_required_vars_per_statement(
    context: &mut ExprGeneratorContext<'_>,
    next_statement_index_fetch: &NextStatementIndexFetch,
    required_vars: &mut Vec<OrderedHashSet<VarId>>,
    index: usize,
    statements: &[Statement],
) {
    // TODO(orizi): Use option instead of empty.
    if !required_vars[index].is_empty() {
        return;
    }
    required_vars[index] = match &statements[index] {
        // The required variables for an invocation, is all of its branches required vars, without
        // vars generated by the call, and its arguments.
        Statement::Sierra(sierra::program::GenStatement::Invocation(invocation)) => chain!(
            invocation.branches.iter().flat_map(|branch| {
                let next_index = next_statement_index_fetch.get(index, &branch.target);
                calculate_required_vars_per_statement(
                    context,
                    next_statement_index_fetch,
                    required_vars,
                    next_index,
                    statements,
                );
                let results: HashSet<VarId> = branch.results.iter().cloned().collect();
                required_vars[next_index]
                    .iter()
                    .filter(move |v| !results.contains(v))
                    .cloned()
                    .collect_vec()
            }),
            invocation.args.iter().cloned()
        )
        .collect(),
        Statement::Sierra(sierra::program::GenStatement::Return(ret_vars)) => {
            // At return - the only required variables are the returned variables.
            ret_vars.iter().cloned().collect()
        }
        Statement::Label(_) => {
            // Labels are no-ops - so we just use the same as next line.
            calculate_required_vars_per_statement(
                context,
                next_statement_index_fetch,
                required_vars,
                index + 1,
                statements,
            );
            required_vars[index + 1].clone()
        }
    }
}

/// Helper to fetch the next statement index from a branch target.
struct NextStatementIndexFetch {
    label_to_statement: HashMap<LabelId, usize>,
}
impl NextStatementIndexFetch {
    fn new(statements: &[Statement]) -> Self {
        Self {
            label_to_statement: statements
                .iter()
                .enumerate()
                .filter_map(|(i, s)| match s {
                    Statement::Sierra(_) => None,
                    Statement::Label(label) => Some((label.id, i)),
                })
                .collect(),
        }
    }
    fn get(&self, index: usize, target: &GenBranchTarget<LabelId>) -> usize {
        match target {
            sierra::program::GenBranchTarget::Fallthrough => index + 1,
            sierra::program::GenBranchTarget::Statement(label) => {
                *self.label_to_statement.get(label).unwrap()
            }
        }
    }
}
