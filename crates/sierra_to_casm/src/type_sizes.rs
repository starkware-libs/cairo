use std::collections::HashMap;

use sierra::extensions::core::{CoreLibFunc, CoreType, CoreTypeConcrete};
use sierra::extensions::non_zero::NonZeroConcreteType;
use sierra::ids::ConcreteTypeId;
use sierra::program::Program;
use sierra::program_registry::ProgramRegistry;

pub type TypeSizeMap = HashMap<ConcreteTypeId, i16>;

/// Returns a mapping for the sizes of all types for the given program.
pub fn get_type_size_map(
    program: &Program,
    registry: &ProgramRegistry<CoreType, CoreLibFunc>,
) -> Option<TypeSizeMap> {
    let mut type_sizes = TypeSizeMap::new();
    for declaration in &program.type_declarations {
        let ty = registry.get_type(&declaration.id).ok()?;
        let size = match ty {
            CoreTypeConcrete::Felt(_)
            | CoreTypeConcrete::GasBuiltin(_)
            | CoreTypeConcrete::Integer(_)
            | CoreTypeConcrete::Box(_) => Some(1),
            CoreTypeConcrete::Array(_) => Some(1),
            CoreTypeConcrete::NonZero(NonZeroConcreteType { ty, .. }) => {
                type_sizes.get(ty).cloned()
            }
            CoreTypeConcrete::Uninitialized(_) => Some(0),
            CoreTypeConcrete::Enum(enum_type) => {
                Some(1 + enum_type.variants.iter().map(|variant| type_sizes[variant]).max()?)
            }
        }?;
        type_sizes.insert(declaration.id.clone(), size);
    }
    Some(type_sizes)
}
