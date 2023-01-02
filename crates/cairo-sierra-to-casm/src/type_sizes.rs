use std::collections::HashMap;

use cairo_sierra::extensions::core::{CoreLibfunc, CoreType, CoreTypeConcrete};
use cairo_sierra::extensions::starknet::StarkNetTypeConcrete;
use cairo_sierra::extensions::types::InfoAndTypeConcreteType;
use cairo_sierra::ids::ConcreteTypeId;
use cairo_sierra::program::Program;
use cairo_sierra::program_registry::ProgramRegistry;

pub type TypeSizeMap = HashMap<ConcreteTypeId, i16>;

/// Returns a mapping for the sizes of all types for the given program.
pub fn get_type_size_map(
    program: &Program,
    registry: &ProgramRegistry<CoreType, CoreLibfunc>,
) -> Option<TypeSizeMap> {
    let mut type_sizes = TypeSizeMap::new();
    for declaration in &program.type_declarations {
        let ty = registry.get_type(&declaration.id).ok()?;
        let size = match ty {
            CoreTypeConcrete::Felt(_)
            | CoreTypeConcrete::GasBuiltin(_)
            | CoreTypeConcrete::Bitwise(_)
            | CoreTypeConcrete::BuiltinCosts(_)
            | CoreTypeConcrete::Nullable(_)
            | CoreTypeConcrete::Uint128(_)
            | CoreTypeConcrete::RangeCheck(_)
            | CoreTypeConcrete::Box(_)
            | CoreTypeConcrete::StarkNet(StarkNetTypeConcrete::System(_))
            | CoreTypeConcrete::StarkNet(StarkNetTypeConcrete::StorageAddress(_))
            | CoreTypeConcrete::StarkNet(StarkNetTypeConcrete::ContractAddress(_))
            | CoreTypeConcrete::Pedersen(_)
            | CoreTypeConcrete::DictFeltTo(_)
            | CoreTypeConcrete::DictManager(_) => Some(1),
            CoreTypeConcrete::Array(_)
            | CoreTypeConcrete::EcPoint(_)
            | CoreTypeConcrete::SquashedDictFeltTo(_) => Some(2),
            CoreTypeConcrete::NonZero(InfoAndTypeConcreteType { ty, .. }) => {
                type_sizes.get(ty).cloned()
            }
            CoreTypeConcrete::Enum(enum_type) => {
                Some(1 + enum_type.variants.iter().map(|variant| type_sizes[variant]).max()?)
            }
            CoreTypeConcrete::Struct(struct_type) => {
                Some(struct_type.members.iter().map(|member| type_sizes[member]).sum())
            }
            CoreTypeConcrete::Uninitialized(_) => {
                // Any size operations on `Uninitialized` are not supported, so we skip adding them
                // to the map.
                continue;
            }
        }?;
        type_sizes.insert(declaration.id.clone(), size);
    }
    Some(type_sizes)
}
