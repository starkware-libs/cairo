fn main() {}

fn free_function<T, impl TPartialEq: PartialEq<T>, +Drop<T>, +Copy<T>>(
    ref self: Array<T>, item: T,
) -> bool {
    return true;
}

fn free_function2(x: felt252, y: felt252) -> felt252 {
    return true;
}

#[derive(Copy, Drop, Serde)]
enum MemberEnum {
    Simple: felt252,
    Complex: Span<MemberStruct>,
}

#[derive(Copy, Drop)]
struct MemberStruct {
    name: felt252,
    ty: MemberEnum,
}

trait Shape<T> {
    const SHAPE_CONST: felt252;
    type ShapePair;

    fn area(self: T) -> u32;
}

struct Circle {
    radius: u32,
}

impl CircleShape of Shape<Circle> {
    type ShapePair = (Circle, Circle);
    const SHAPE_CONST: felt252 = 'xyz';
    fn area(self: Circle) -> u32 {
        3 * self.radius * self.radius
    }
}


trait ATrait {}

struct B<T, G, +ATrait> {
    t: T,
    g: G,
}

impl BCopy<T, G, +ATrait, +Copy<T>, +Copy<G>> of Copy<B<T, G>>;

pub extern type bytes31;

pub extern type Pedersen;

pub extern fn pedersen(a: felt252, b: felt252) -> felt252 implicits(Pedersen) nopanic;
