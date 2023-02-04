#[component]
mod PositionComponent {
    struct Position {
        x: felt,
        y: felt
    }

    #[view]
    fn is_zero(self: Position) -> bool {
        match self.x - self.y {
            0 => bool::True(()),
            _ => bool::False(()),
        }
    }

    #[view]
    fn is_equal(self: Position, b: Position) -> bool {
        self.x == b.x & self.y == b.y
    }
}