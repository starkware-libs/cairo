use std::fmt::Display;

use cairo_lang_sierra::extensions::gas::CostTokenType;
use cairo_lang_sierra::ids::FunctionId;
use cairo_lang_sierra::program::StatementIdx;
use cairo_lang_utils::collection_arithmetics::sub_maps;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use itertools::{chain, Itertools};

/// Gas information for a Sierra program.
#[derive(Debug, Eq, PartialEq)]
pub struct GasInfo {
    /// The values of variables at matching libfuncs at given statements indices.
    pub variable_values: OrderedHashMap<(StatementIdx, CostTokenType), i64>,
    /// The costs of calling the given function.
    pub function_costs: OrderedHashMap<FunctionId, OrderedHashMap<CostTokenType, i64>>,
}
impl GasInfo {
    pub fn combine(mut self, mut other: GasInfo) -> GasInfo {
        let variable_values = chain!(self.variable_values.keys(), other.variable_values.keys())
            .unique()
            .copied()
            .map(|i| {
                (
                    i,
                    self.variable_values.get(&i).copied().unwrap_or_default()
                        + other.variable_values.get(&i).copied().unwrap_or_default(),
                )
            })
            .collect();
        let function_costs = chain!(self.function_costs.keys(), other.function_costs.keys())
            .unique()
            .cloned()
            .collect_vec()
            .into_iter()
            .map(|i| {
                let costs0 = self.function_costs.swap_remove(&i).unwrap_or_default();
                let costs1 = other.function_costs.swap_remove(&i).unwrap_or_default();
                (
                    i,
                    chain!(costs0.keys(), costs1.keys())
                        .unique()
                        .copied()
                        .map(|i| {
                            (
                                i,
                                costs0.get(&i).copied().unwrap_or_default()
                                    + costs1.get(&i).copied().unwrap_or_default(),
                            )
                        })
                        .collect(),
                )
            })
            .collect();

        GasInfo { variable_values, function_costs }
    }

    pub fn assert_eq(&self, other: &GasInfo) {
        for (key, val) in sub_maps(self.variable_values.clone(), other.variable_values.clone()) {
            assert!(
                val == 0,
                "Difference in {key:?}: {:?} != {:?}",
                self.variable_values.get(&key),
                other.variable_values.get(&key)
            );
        }
        for key in chain!(self.function_costs.keys(), other.function_costs.keys()) {
            let self_val = self.function_costs.get(key);
            let other_val = other.function_costs.get(key);
            assert!(
                match (self_val, other_val) {
                    (Some(self_val), Some(other_val)) =>
                        sub_maps(self_val.clone(), other_val.iter().map(|(k, v)| (*k, *v)))
                            .into_iter()
                            .all(|(_, val)| val == 0),
                    (None, None) => true,
                    _ => false,
                },
                "Difference in {key:?}: {self_val:?} != {other_val:?}",
            );
        }
    }
}

impl Display for GasInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Reorder the variable values by statement index.
        let mut var_values: OrderedHashMap<StatementIdx, OrderedHashMap<CostTokenType, i64>> =
            Default::default();
        for ((statement_idx, cost_type), value) in self.variable_values.iter() {
            var_values.entry(*statement_idx).or_default().insert(*cost_type, *value);
        }

        for statement_idx in var_values.keys().sorted_by(|a, b| a.0.cmp(&b.0)) {
            writeln!(f, "#{statement_idx}: {:?}", var_values[*statement_idx])?;
        }
        writeln!(f)?;
        for (function_id, costs) in self.function_costs.iter() {
            writeln!(f, "{function_id}: {costs:?}")?;
        }
        Ok(())
    }
}
