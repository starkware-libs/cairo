#[derive(Drop)]
struct Battle {}

#[generate_trait]
impl BattleImpl of BattleTrait {
    fn battle_loop(ref self: Battle) {
        loop {
            let mut entity: u32 = 5;
            if !self.is_ally(entity) {
                break;
            }
        }
    }

    fn is_ally(ref self: Battle, entity_index: u32) -> bool {
        return true;
    }
}
