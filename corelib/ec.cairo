extern type EcOp;
#[derive(Copy, Drop)]
extern type EcPoint;

impl OptionEcPointCopy of Copy::<Option::<EcPoint>>;

#[panic_with('not on EC', ec_point_new)]
extern fn ec_point_try_new(x: felt, y: felt) -> Option::<EcPoint> nopanic;
extern fn ec_point_from_x(x: felt) -> Option::<EcPoint> nopanic;
extern fn ec_point_unwrap(p: EcPoint) -> (felt, felt) nopanic;
/// Computes the negation of an elliptic curve point (-p).
extern fn ec_neg(p: EcPoint) -> EcPoint nopanic;

// EC state.

// TODO(lior): Allow explicit clone() for EcState, since we don't allow implicit dup (Copy).
#[derive(Drop)]
extern type EcState;

extern fn ec_state_init() -> EcState nopanic;
extern fn ec_state_add(ref s: EcState, p: EcPoint) nopanic;
extern fn ec_state_finalize(s: EcState) -> Option::<EcPoint> nopanic;
/// Adds the product p * m to the state.
extern fn ec_state_add_mul(ref s: EcState, m: felt, p: EcPoint) implicits(EcOp) nopanic;

/// Computes the product of an EC point `p` by the given scalar `m`.
fn ec_mul(p: EcPoint, m: felt) -> Option::<EcPoint> {
    let mut state = ec_state_init();
    ec_state_add_mul(ref state, m, p);
    ec_state_finalize(state)
}

impl EcPointAdd of Add::<Option::<EcPoint>> {
    /// Computes the sum of two points on the curve.
    // TODO(lior): Implement using a libfunc to make it more efficient.
    fn add(p: Option::<EcPoint>, q: Option::<EcPoint>) -> Option::<EcPoint> {
        let p_nz = match p {
            Option::Some(pt) => pt,
            Option::None(()) => {
                return q;
            }
        };
        let q_nz = match q {
            Option::Some(pt) => pt,
            Option::None(()) => {
                return p;
            }
        };
        let mut state = ec_state_init();
        ec_state_add(ref state, p_nz);
        ec_state_add(ref state, q_nz);
        ec_state_finalize(state)
    }
}

impl EcPointSub of Sub::<Option::<EcPoint>> {
    /// Computes the difference between two points on the curve.
    fn sub(p: Option::<EcPoint>, q: Option::<EcPoint>) -> Option::<EcPoint> {
        let q_nz = match q {
            Option::Some(pt) => pt,
            Option::None(()) => {
                // p - 0 = p.
                return p;
            }
        };
        // p - q = p + (-q).
        p + Option::Some(ec_neg(q_nz))
    }
}
