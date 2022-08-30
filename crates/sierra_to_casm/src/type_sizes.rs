use std::collections::HashMap;

use sierra::extensions::core::non_zero::NonZeroConcreteType;
use sierra::extensions::core::CoreTypeConcrete;
use sierra::extensions::{CoreLibFunc, CoreType};
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
        println!("{}", declaration);
        let ty = registry.get_type(&declaration.id).ok()?;
        let size = match ty {
            CoreTypeConcrete::Felt(_)
            | CoreTypeConcrete::GasBuiltin(_)
            | CoreTypeConcrete::Integer(_) => Some(1),
            CoreTypeConcrete::NonZero(NonZeroConcreteType { ty }) => type_sizes.get(ty).cloned(),
        }?;
        type_sizes.insert(declaration.id.clone(), size);
    }
    Some(type_sizes)
}
