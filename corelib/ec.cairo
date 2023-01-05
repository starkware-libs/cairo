#[derive(Copy, Drop)]
extern type EcPoint;
#[derive(Copy, Drop)]
extern type EcState;

#[panic_with('not on EC', ec_point_from_felts)]
extern fn ec_point_try_create(x: felt, y: felt) -> Option::<EcPoint> nopanic;
extern fn ec_point_unwrap(p: EcPoint) -> (felt, felt) nopanic;
extern fn ec_init_state() -> EcState nopanic;
extern fn ec_add_to_state(s: EcState, p: EcPoint) -> EcState nopanic;
