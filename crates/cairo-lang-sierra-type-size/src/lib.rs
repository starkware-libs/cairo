use cairo_lang_sierra::extensions::circuit::CircuitTypeConcrete;
use cairo_lang_sierra::extensions::core::{CoreLibfunc, CoreType, CoreTypeConcrete};
use cairo_lang_sierra::extensions::starknet::StarknetTypeConcrete;
use cairo_lang_sierra::ids::ConcreteTypeId;
use cairo_lang_sierra::program::Program;
use cairo_lang_sierra::program_registry::{ProgramRegistry, ProgramRegistryError};
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;

pub type TypeSizeMap = UnorderedHashMap<ConcreteTypeId, i16>;

/// A wrapper that combines a program registry with its corresponding type size map.
/// This is commonly used together throughout the codebase.
pub struct ProgramRegistryInfo {
    pub registry: ProgramRegistry<CoreType, CoreLibfunc>,
    pub type_sizes: TypeSizeMap,
}

impl ProgramRegistryInfo {
    /// Creates a new ProgramRegistryInfo by building both the registry and type size map from a
    /// program.
    pub fn new(program: &Program) -> Result<Self, Box<ProgramRegistryError>> {
        let registry = ProgramRegistry::<CoreType, CoreLibfunc>::new(program)?;
        let type_sizes = get_type_size_map(program, &registry)?;
        Ok(Self { registry, type_sizes })
    }

    /// Get a reference to the program registry.
    pub fn registry(&self) -> &ProgramRegistry<CoreType, CoreLibfunc> {
        &self.registry
    }

    /// Get a reference to the type size map.
    pub fn type_sizes(&self) -> &TypeSizeMap {
        &self.type_sizes
    }
}

/// Returns a mapping for the sizes of all types for the given program.
pub fn get_type_size_map(
    program: &Program,
    registry: &ProgramRegistry<CoreType, CoreLibfunc>,
) -> Result<TypeSizeMap, Box<ProgramRegistryError>> {
    let mut type_sizes = TypeSizeMap::default();
    for declaration in &program.type_declarations {
        let get_dep_size = |dep: &ConcreteTypeId| {
            type_sizes.get(dep).copied().ok_or_else(|| {
                Box::new(ProgramRegistryError::TypeSizeDependencyMissing {
                    ty: declaration.id.clone(),
                    dep: dep.clone(),
                })
            })
        };
        let size_overflow_error =
            || Box::new(ProgramRegistryError::TypeSizeOverflow(declaration.id.clone()));
        let size = match registry.get_type(&declaration.id)? {
            // Size 0.
            CoreTypeConcrete::Coupon(_) => 0,
            // Size 1.
            CoreTypeConcrete::Felt252(_)
            | CoreTypeConcrete::GasBuiltin(_)
            | CoreTypeConcrete::GasReserve(_)
            | CoreTypeConcrete::Bitwise(_)
            | CoreTypeConcrete::BuiltinCosts(_)
            | CoreTypeConcrete::EcOp(_)
            | CoreTypeConcrete::Nullable(_)
            | CoreTypeConcrete::Uint8(_)
            | CoreTypeConcrete::Uint16(_)
            | CoreTypeConcrete::Uint32(_)
            | CoreTypeConcrete::Uint64(_)
            | CoreTypeConcrete::Uint128(_)
            | CoreTypeConcrete::Sint8(_)
            | CoreTypeConcrete::Sint16(_)
            | CoreTypeConcrete::Sint32(_)
            | CoreTypeConcrete::Sint64(_)
            | CoreTypeConcrete::Sint128(_)
            | CoreTypeConcrete::RangeCheck(_)
            | CoreTypeConcrete::RangeCheck96(_)
            | CoreTypeConcrete::Box(_)
            | CoreTypeConcrete::Starknet(StarknetTypeConcrete::System(_))
            | CoreTypeConcrete::Starknet(StarknetTypeConcrete::StorageBaseAddress(_))
            | CoreTypeConcrete::Starknet(StarknetTypeConcrete::StorageAddress(_))
            | CoreTypeConcrete::Starknet(StarknetTypeConcrete::ContractAddress(_))
            | CoreTypeConcrete::Starknet(StarknetTypeConcrete::ClassHash(_))
            | CoreTypeConcrete::Starknet(StarknetTypeConcrete::Secp256Point(_))
            | CoreTypeConcrete::Starknet(StarknetTypeConcrete::Sha256StateHandle(_))
            | CoreTypeConcrete::Pedersen(_)
            | CoreTypeConcrete::Poseidon(_)
            | CoreTypeConcrete::Felt252Dict(_)
            | CoreTypeConcrete::Felt252DictEntry(_)
            | CoreTypeConcrete::SegmentArena(_)
            | CoreTypeConcrete::Bytes31(_)
            | CoreTypeConcrete::BoundedInt(_)
            | CoreTypeConcrete::QM31(_) => 1,
            // Size 2.
            CoreTypeConcrete::Array(_)
            | CoreTypeConcrete::Span(_)
            | CoreTypeConcrete::EcPoint(_)
            | CoreTypeConcrete::SquashedFelt252Dict(_)
            | CoreTypeConcrete::IntRange(_) => 2,
            // Other.
            CoreTypeConcrete::NonZero(wrapped_ty)
            | CoreTypeConcrete::Snapshot(wrapped_ty)
            | CoreTypeConcrete::Uninitialized(wrapped_ty) => get_dep_size(&wrapped_ty.ty)?,
            CoreTypeConcrete::EcState(_) => 3,
            CoreTypeConcrete::Uint128MulGuarantee(_) => 4,
            CoreTypeConcrete::Enum(enum_type) => {
                let mut max_variant_size: i16 = 0;
                for variant in &enum_type.variants {
                    max_variant_size = max_variant_size.max(get_dep_size(variant)?);
                }
                max_variant_size.checked_add(1).ok_or_else(size_overflow_error)?
            }
            CoreTypeConcrete::Struct(struct_type) => {
                if !struct_type.info.storable {
                    // If the struct is not storable, it should not have a size.
                    continue;
                }
                let mut size: i16 = 0;
                for member in &struct_type.members {
                    size =
                        size.checked_add(get_dep_size(member)?).ok_or_else(size_overflow_error)?;
                }
                size
            }

            CoreTypeConcrete::Circuit(CircuitTypeConcrete::CircuitInputAccumulator(_)) => 2,
            CoreTypeConcrete::Circuit(CircuitTypeConcrete::CircuitDescriptor(_)) => 4,
            CoreTypeConcrete::Circuit(CircuitTypeConcrete::CircuitFailureGuarantee(_)) => 8,
            CoreTypeConcrete::Circuit(CircuitTypeConcrete::U96LimbsLessThanGuarantee(g)) => g
                .limb_count
                .checked_mul(2)
                .and_then(|v| v.try_into().ok())
                .ok_or_else(size_overflow_error)?,
            CoreTypeConcrete::Circuit(CircuitTypeConcrete::U96Guarantee(_)) => 1,
            CoreTypeConcrete::Circuit(CircuitTypeConcrete::CircuitOutputs(_)) => 5,
            CoreTypeConcrete::Circuit(CircuitTypeConcrete::CircuitPartialOutputs(_)) => 6,
            CoreTypeConcrete::Circuit(CircuitTypeConcrete::CircuitModulus(_)) => 4,
            CoreTypeConcrete::Circuit(CircuitTypeConcrete::CircuitData(_))
            | CoreTypeConcrete::Circuit(CircuitTypeConcrete::AddMod(_))
            | CoreTypeConcrete::Circuit(CircuitTypeConcrete::MulMod(_)) => 1,

            // Const and circuit types are not moved around and should not have a size.
            CoreTypeConcrete::Const(_)
            | CoreTypeConcrete::Circuit(CircuitTypeConcrete::Circuit(_))
            | CoreTypeConcrete::Circuit(CircuitTypeConcrete::CircuitInput(_))
            | CoreTypeConcrete::Circuit(CircuitTypeConcrete::AddModGate(_))
            | CoreTypeConcrete::Circuit(CircuitTypeConcrete::InverseGate(_))
            | CoreTypeConcrete::Circuit(CircuitTypeConcrete::MulModGate(_))
            | CoreTypeConcrete::Circuit(CircuitTypeConcrete::SubModGate(_)) => continue,
            CoreTypeConcrete::Blake(_) => 1,
        };
        type_sizes.insert(declaration.id.clone(), size);
    }
    Ok(type_sizes)
}
