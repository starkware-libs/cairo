use std::collections::HashMap;

use sierra::extensions::core::{CoreLibFunc, CoreType, CoreTypeConcrete};
use sierra::extensions::non_zero::NonZeroConcreteType;
use sierra::extensions::uninitialized::UninitializedConcreteType;
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
            | CoreTypeConcrete::Ref(_) => Some(1),
            CoreTypeConcrete::NonZero(NonZeroConcreteType { ty }) => type_sizes.get(ty).cloned(),
            // TODO(ilya, 10/10/2022): What should be the size of Uninitialized<T>?
            CoreTypeConcrete::Uninitialized(UninitializedConcreteType { ty }) => {
                type_sizes.get(ty).cloned()
            }
        }?;
        type_sizes.insert(declaration.id.clone(), size);
    }
    Some(type_sizes)
}
