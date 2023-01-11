extern type EcOp;
#[derive(Copy, Drop)]
extern type EcPoint;
#[derive(Copy, Drop)]
extern type EcState;

#[panic_with('not on EC', ec_point_from_felts)]
extern fn ec_point_try_create(x: felt, y: felt) -> Option::<EcPoint> nopanic;
extern fn ec_point_unwrap(p: EcPoint) -> (felt, felt) nopanic;
extern fn ec_init_state() -> EcState nopanic;
extern fn ec_add_to_state(s: EcState, p: EcPoint) -> EcState nopanic;
#[panic_with('not on EC', ec_finalize_state)]
extern fn ec_try_finalize_state(s: EcState) -> Option::<EcPoint> nopanic;
extern fn ec_op_builtin(s: EcState, m: felt, p: EcPoint) -> EcState implicits(EcOp) nopanic;

#[panic_with('not on EC', ec_op)]
fn ec_try_op(p: EcPoint, m: felt, q: EcPoint) -> Option::<EcPoint> implicits(EcOp) nopanic {
    let s = ec_init_state();
    let sp = ec_add_to_state(s, p);
    let final_state = ec_op_builtin(sp, m, q);
    ec_try_finalize_state(final_state)
}
