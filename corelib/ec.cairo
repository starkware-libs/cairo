extern type EcPoint;

#[panic_with('not on EC', ec_point_from_felts)]
extern fn ec_point_try_create(x: felt, y: felt) -> Option::<EcPoint> nopanic;
