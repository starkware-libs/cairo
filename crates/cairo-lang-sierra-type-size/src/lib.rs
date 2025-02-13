use cairo_lang_sierra::extensions::circuit::CircuitTypeConcrete;
use cairo_lang_sierra::extensions::core::{CoreLibfunc, CoreType, CoreTypeConcrete};
use cairo_lang_sierra::extensions::starknet::StarknetTypeConcrete;
use cairo_lang_sierra::ids::ConcreteTypeId;
use cairo_lang_sierra::program::Program;
use cairo_lang_sierra::program_registry::ProgramRegistry;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;

pub type TypeSizeMap = UnorderedHashMap<ConcreteTypeId, i16>;

/// Returns a mapping for the sizes of all types for the given program.
pub fn get_type_size_map(
    program: &Program,
    registry: &ProgramRegistry<CoreType, CoreLibfunc>,
) -> Option<TypeSizeMap> {
    let mut type_sizes = TypeSizeMap::default();
    for declaration in &program.type_declarations {
        let size = match registry.get_type(&declaration.id).ok()? {
            CoreTypeConcrete::Coupon(_) => Some(0),
            CoreTypeConcrete::Felt252(_)
            | CoreTypeConcrete::GasBuiltin(_)
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
            | CoreTypeConcrete::BoundedInt(_) => Some(1),
            CoreTypeConcrete::Array(_)
            | CoreTypeConcrete::Span(_)
            | CoreTypeConcrete::EcPoint(_)
            | CoreTypeConcrete::SquashedFelt252Dict(_)
            | CoreTypeConcrete::IntRange(_) => Some(2),
            CoreTypeConcrete::NonZero(wrapped_ty)
            | CoreTypeConcrete::Snapshot(wrapped_ty)
            | CoreTypeConcrete::Uninitialized(wrapped_ty) => {
                type_sizes.get(&wrapped_ty.ty).cloned()
            }
            CoreTypeConcrete::EcState(_) => Some(3),
            CoreTypeConcrete::Uint128MulGuarantee(_) => Some(4),
            CoreTypeConcrete::Enum(enum_type) => {
                let mut size = 1;
                for variant in &enum_type.variants {
                    size = size.max(type_sizes.get(variant).cloned()? + 1);
                }
                Some(size)
            }
            CoreTypeConcrete::Struct(struct_type) => {
                if !struct_type.info.storable {
                    // If the struct is not storable, it should not have a size.
                    continue;
                }
                let mut size = 0;
                for member in &struct_type.members {
                    size += type_sizes.get(member).cloned()?;
                }
                Some(size)
            }

            CoreTypeConcrete::Circuit(CircuitTypeConcrete::CircuitInputAccumulator(_)) => Some(2),
            CoreTypeConcrete::Circuit(CircuitTypeConcrete::CircuitDescriptor(_)) => Some(4),
            CoreTypeConcrete::Circuit(CircuitTypeConcrete::CircuitFailureGuarantee(_)) => Some(8),
            CoreTypeConcrete::Circuit(CircuitTypeConcrete::U96LimbsLessThanGuarantee(g)) => {
                Some((g.limb_count.checked_mul(2)?).try_into().ok()?)
            }
            CoreTypeConcrete::Circuit(CircuitTypeConcrete::U96Guarantee(_)) => Some(1),
            CoreTypeConcrete::Circuit(CircuitTypeConcrete::CircuitOutputs(_)) => Some(5),
            CoreTypeConcrete::Circuit(CircuitTypeConcrete::CircuitPartialOutputs(_)) => Some(6),
            CoreTypeConcrete::Circuit(CircuitTypeConcrete::CircuitModulus(_)) => Some(4),
            CoreTypeConcrete::Circuit(CircuitTypeConcrete::CircuitData(_))
            | CoreTypeConcrete::Circuit(CircuitTypeConcrete::AddMod(_))
            | CoreTypeConcrete::Circuit(CircuitTypeConcrete::MulMod(_)) => Some(1),

            // Const and circuit types are not moved around and should not have a size.
            CoreTypeConcrete::Const(_)
            | CoreTypeConcrete::Circuit(CircuitTypeConcrete::Circuit(_))
            | CoreTypeConcrete::Circuit(CircuitTypeConcrete::CircuitInput(_))
            | CoreTypeConcrete::Circuit(CircuitTypeConcrete::AddModGate(_))
            | CoreTypeConcrete::Circuit(CircuitTypeConcrete::InverseGate(_))
            | CoreTypeConcrete::Circuit(CircuitTypeConcrete::MulModGate(_))
            | CoreTypeConcrete::Circuit(CircuitTypeConcrete::SubModGate(_)) => continue,
            CoreTypeConcrete::Blake(_) => Some(1),
        }?;
        type_sizes.insert(declaration.id.clone(), size);
    }
    Some(type_sizes)
}
