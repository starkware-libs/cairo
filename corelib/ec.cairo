#[derive(Copy, Drop)]
extern type EcPoint;
extern type EcState;

impl EcPointCopy of Copy::<EcPoint>;
impl EcPointDrop of Drop::<EcPoint>;

#[panic_with('not on EC', ec_point_from_felts)]
extern fn ec_point_try_create(x: felt, y: felt) -> Option::<EcPoint> nopanic;
extern fn ec_point_unwrap(p: EcPoint) -> (felt, felt) nopanic;
extern fn ec_init_state() -> EcState nopanic;
extern fn ec_add_to_state(s: EcState, p: EcPoint) -> EcState nopanic;
