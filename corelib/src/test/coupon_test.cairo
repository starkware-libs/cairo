use core::test::test_utils::{assert_eq, assert_ne};
use core::debug::print;

extern fn coupon_buy<T>() -> T nopanic;
extern fn drop<T>(c: T) nopanic;

struct NonDroppable {
    x: felt252,
}

impl NonDroppableDestruct of Destruct<NonDroppable> {
    fn destruct(self: NonDroppable) nopanic {
        print(array![self.x]);
        drop(self);
    }
}

impl NonDroppableArrayDestruct of Destruct<Array<(NonDroppable, _destruct::Coupon)>> {
    fn destruct(self: Array<(NonDroppable, _destruct::Coupon)>) nopanic {
        _destruct(self);
    }
}

fn _destruct(arr: Array<(NonDroppable, _destruct::Coupon)>) nopanic {
    match arr.pop_front_consume() {
        Option::Some((rem, (_elm, coupon))) => {
            // NonDroppableDestruct::destruct is called implicitly.
            _destruct(rem, __coupon__: coupon);
        },
        Option::None => {},
    }
}

#[test]
fn test_destruct_arr() {
    let mut arr: Array::<(NonDroppable, _destruct::Coupon)> = array![];
    arr.append((NonDroppable { x: 0x1111 }, coupon_buy()));
    arr.append((NonDroppable { x: 0x2222 }, coupon_buy()));
    arr.append((NonDroppable { x: 0x3333 }, coupon_buy()));
    // _destruct is called implicitly.
    ()
}
