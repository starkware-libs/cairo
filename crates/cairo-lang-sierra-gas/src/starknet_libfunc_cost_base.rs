use cairo_lang_sierra::extensions::starknet::StarkNetConcreteLibfunc;

use crate::core_libfunc_cost_base::CostOperations;

/// Returns some cost value for a StarkNet libfunc - a helper function to implement costing both for
/// creating gas equations and getting actual gas usage after having a solution.
pub fn starknet_libfunc_cost_base<Ops: CostOperations>(
    ops: &mut Ops,
    libfunc: &StarkNetConcreteLibfunc,
) -> Vec<Ops::CostType> {
    match libfunc {
        // TODO(Ilya): Revisit the real cost.
        StarkNetConcreteLibfunc::CallContract(_) => vec![ops.steps(50), ops.steps(50)],
        StarkNetConcreteLibfunc::ContractAddressConst(_) => vec![ops.steps(0)],
        StarkNetConcreteLibfunc::ContractAddressTryFromFelt(_) => {
            vec![ops.steps(6), ops.steps(7)]
        }
        // TODO(Ilya): Consider adding a `CostTokenType::StorageRead` or make storage read a branch.
        StarkNetConcreteLibfunc::StorageRead(_) => vec![ops.steps(50), ops.steps(50)],
        // TODO(yuval): Revisit the real cost.
        StarkNetConcreteLibfunc::StorageWrite(_) => vec![ops.steps(50), ops.steps(50)],
        StarkNetConcreteLibfunc::StorageBaseAddressConst(_) => vec![ops.steps(0)],
        StarkNetConcreteLibfunc::StorageBaseAddressFromFelt(_) => vec![ops.steps(10)],
        StarkNetConcreteLibfunc::StorageAddressFromBase(_) => vec![ops.steps(0)],
        StarkNetConcreteLibfunc::StorageAddressFromBaseAndOffset(_) => vec![ops.steps(0)],
        StarkNetConcreteLibfunc::EmitEvent(_) => vec![ops.steps(50), ops.steps(50)],
        StarkNetConcreteLibfunc::GetCallerAddress(_) => {
            vec![ops.steps(50), ops.steps(50)]
        }
    }
}
