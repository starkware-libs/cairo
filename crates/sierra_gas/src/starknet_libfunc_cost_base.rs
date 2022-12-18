use sierra::extensions::starknet::StarkNetConcreteLibFunc;

use crate::core_libfunc_cost_base::CostOperations;

/// Returns some cost value for a StarkNet libfunc - a helper function to implement costing both for
/// creating gas equations and getting actual gas usage after having a solution.
pub fn starknet_libfunc_cost_base<Ops: CostOperations>(
    ops: &mut Ops,
    libfunc: &StarkNetConcreteLibFunc,
) -> Vec<Ops::CostType> {
    match libfunc {
        // TODO(Ilya): Consider adding a `CostTokenType::StorageRead` or make storage read a branch.
        StarkNetConcreteLibFunc::StorageRead(_) => vec![ops.const_cost(50)],
        // TODO(yuval): Revisit the real cost.
        StarkNetConcreteLibFunc::StorageWrite(_) => vec![ops.const_cost(50), ops.const_cost(50)],
        StarkNetConcreteLibFunc::StorageAddressConst(_) => vec![ops.const_cost(1)],
    }
}
