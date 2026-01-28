use std::fmt::Display;

use cairo_lang_sierra::extensions::NamedLibfunc;
use cairo_lang_sierra::extensions::branch_align::BranchAlignLibfunc;
use cairo_lang_sierra::extensions::gas::{CostTokenMap, CostTokenType};
use cairo_lang_sierra::ids::FunctionId;
use cairo_lang_sierra::program::{Program, Statement, StatementIdx};
use cairo_lang_utils::collection_arithmetics::SubCollection;
use cairo_lang_utils::ordered_hash_map::{Entry, OrderedHashMap};
use itertools::{Itertools, chain};

/// Gas information for a Sierra program.
#[derive(Debug, Default, Eq, PartialEq)]
pub struct GasInfo {
    /// The values of variables at matching libfuncs at given statements indices.
    pub variable_values: OrderedHashMap<(StatementIdx, CostTokenType), i64>,
    /// The costs of calling the given function.
    pub function_costs: OrderedHashMap<FunctionId, CostTokenMap<i64>>,
}
impl GasInfo {
    /// Combines two GasInfo instances into the sum of their values.
    pub fn combine(mut self, other: GasInfo) -> GasInfo {
        for (key, value) in other.variable_values {
            *self.variable_values.entry(key).or_insert(0) += value;
        }
        for (key, other_val) in other.function_costs {
            match self.function_costs.entry(key) {
                Entry::Occupied(mut e) => {
                    let e = e.get_mut();
                    for (token, value) in other_val {
                        *e.entry(token).or_insert(0) += value;
                    }
                }
                Entry::Vacant(e) => {
                    e.insert(other_val);
                }
            }
        }
        self
    }

    /// Asserts that all non-branch align values in `self.variable_values` are equal to the values
    /// in `other.variable_values`. Panics otherwise, printing the differences.
    /// We allow branch align values to be different, as they do not affect generated code directly.
    pub fn assert_eq_variables(&self, other: &GasInfo, program: &Program) {
        let branch_align_id: Option<_> = program.libfunc_declarations.iter().find_map(|fd| {
            (fd.long_id.generic_id.0 == BranchAlignLibfunc::STR_ID).then_some(&fd.id)
        });
        let mut fail = false;
        for ((idx, token), val) in self
            .variable_values
            .clone()
            .sub_collection(other.variable_values.iter().map(|(k, v)| (*k, *v)))
        {
            if val != 0
                && !matches!(
                    program.get_statement(idx),
                    Some(Statement::Invocation(x))
                    if Some(&x.libfunc_id) == branch_align_id
                )
            {
                println!(
                    "Difference in ({idx:?}, {token:?}): {:?} != {:?}.",
                    self.variable_values.get(&(idx, token)),
                    other.variable_values.get(&(idx, token))
                );
                fail = true;
            }
        }
        assert!(!fail, "Comparison failed.");
    }

    /// Asserts that all the cost of functions in `self` are equal to the costs in `other`.
    /// Panics otherwise, printing the differences.
    pub fn assert_eq_functions(&self, other: &GasInfo) {
        let mut fail = false;
        for key in chain!(self.function_costs.keys(), other.function_costs.keys()) {
            let self_val = self.function_costs.get(key);
            let other_val = other.function_costs.get(key);
            let is_same = match (self_val, other_val) {
                (Some(self_val), Some(other_val)) => self_val
                    .clone()
                    .sub_collection(other_val.iter().map(|(k, v)| (*k, *v)))
                    .into_iter()
                    .all(|(_, val)| val == 0),
                (None, None) => true,
                _ => false,
            };
            if !is_same {
                println!("Difference in {key:?}: {self_val:?} != {other_val:?}.");
                fail = true;
            }
        }
        assert!(!fail, "Comparison failed.");
    }
}

impl Display for GasInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Reorder the variable values by statement index.
        let mut var_values: OrderedHashMap<StatementIdx, CostTokenMap<i64>> = Default::default();
        for ((statement_idx, cost_type), value) in self.variable_values.iter() {
            var_values.entry(*statement_idx).or_default().insert(*cost_type, *value);
        }

        for statement_idx in var_values.keys().sorted_by(|a, b| a.0.cmp(&b.0)) {
            writeln!(f, "#{statement_idx}: {:?}", var_values[statement_idx])?;
        }
        writeln!(f)?;
        for (function_id, costs) in self.function_costs.iter() {
            writeln!(f, "{function_id}: {costs:?}")?;
        }
        Ok(())
    }
}
