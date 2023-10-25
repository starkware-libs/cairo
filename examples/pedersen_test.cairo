use core::pedersen::pedersen;

fn test_pedersen() -> felt252 {
    pedersen(pedersen(pedersen(1, 2), 3), 4)
}
