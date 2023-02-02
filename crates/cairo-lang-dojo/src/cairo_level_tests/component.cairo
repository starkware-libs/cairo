#[derive(Component)]
struct Position { x: felt, y: felt }

#[contract]
mod PositionComponent {
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
}

trait IPosition {
    fn is_zero(self: Position) -> bool;
}