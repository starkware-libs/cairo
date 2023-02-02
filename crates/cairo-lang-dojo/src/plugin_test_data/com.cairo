#[contract]
mod PositionComponent {
    struct Postion {
        x: felt,
        y: felt
    }

    impl StorageAccessPosition of StorageAccess::<Position> {
    fn read(address_domain: felt, base: StorageBaseAddress) -> SyscallResult::<Position> {
        Result::Ok(
            Position {
                x: StorageAccess::<felt>::read(address_domain, base)?,
                y: storage_read_syscall(
                    address_domain, storage_address_from_base_and_offset(base, 1_u8)
                )?
            }
        )
    }
    fn write(
        address_domain: felt, base: StorageBaseAddress, value: Position
    ) -> SyscallResult::<()> {
        StorageAccess::<felt>::write(address_domain, base, value.x)?;
        storage_write_syscall(
            address_domain, storage_address_from_base_and_offset(base, 1_u8), value.y
        )
    }
}

    struct Storage {
        world_address: felt,
        state: Map::<felt, Position>,
    }

    // Initialize PositionComponent.
    #[external]
    fn initialize(world_addr: felt) {
        let world = world_address::read();
        assert(world == 0, 'PositionComponent: Already initialized.');
        world_address::write(world_addr);
    }

    // Set the state of an entity.
    #[external]
    fn set(entity_id: felt, value: Position) {
        state::write(entity_id, value);
    }

    // Get the state of an entity.
    #[view]
    fn get(entity_id: felt) -> Position {
        return state::read(entity_id);
    }

    #[view]
    fn is_zero(entity_id: felt) -> bool {
        let self = state::read(entity_id);
        match self.x - self.y {
            0 => bool::True(()),
            _ => bool::False(()),
        }
    }

    #[view]
    fn is_equal(entity_id: felt, b: Position) -> bool {
        let self = state::read(entity_id);
        self.x == b.x & self.y == b.y
    }
}
