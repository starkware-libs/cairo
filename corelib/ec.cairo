mod StarkCurve {
    /// The STARK Curve is defined by the equation `y^2 = x^3 + ALPHA*x + BETA`.
    const ALPHA: felt = 1;
    /// The STARK Curve is defined by the equation `y^2 = x^3 + ALPHA*x + BETA`.
    const BETA: felt = 0x6f21413efbe40de150e596d72f7a8c5609ad26c15c915c1f4cdfcb99cee9e89;
    /// The order (number of points) of the STARK Curve.
    const ORDER: felt = 0x800000000000010ffffffffffffffffb781126dcae7b2321e66a241adc64d2f;
    /// The x coordinate of the generator point used in the ECDSA signature.
    const GEN_X: felt = 0x1ef15c18599971b7beced415a40f0c7deacfd9b0d1819e03d723d8bc943cfca;
    /// The y coordinate of the generator point used in the ECDSA signature.
    const GEN_Y: felt = 0x5668060aa49730b7be4801df46ec62de53ecd11abe43a32873000c36e8dc1f;
}

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
