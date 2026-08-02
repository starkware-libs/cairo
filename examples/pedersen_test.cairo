use core::pedersen::pedersen;

#[allow_attr(target_function)]
#[target_function]
fn test_pedersen() -> felt252 {
    pedersen(pedersen(pedersen(1, 2), 3), 4)
}
