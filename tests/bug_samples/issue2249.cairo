<<<<<<< HEAD
// Core lib imports
use option::OptionTrait;
use box::Box;

#[derive(Drop)]
struct Node {
    value: felt252,
    left: Option::<Box<Node>>,
}

#[test]
#[available_gas(2000000)]
fn simple_test() {
    let bst = Node { value: 12, left: Option::None(()), };

    assert(bst.value == 12, 'value should be 12');
    assert(bst.left.is_none(), 'left should be none');
}
||||||| 0f77760aa
=======
// Core lib imports
use option::OptionTrait;
use box::Box;

#[derive(Drop)]
struct Node {
    value: felt252,
    left: Option::<Box<Node>>,
}

#[test]
#[available_gas(2000000)]
fn simple_test() {
    let bst = Node { value: 12, left: Option::None(()),  };

    assert(bst.value == 12, 'value should be 12');
    assert(bst.left.is_none(), 'left should be none');
}
>>>>>>> origin/dev-v2.1.1
