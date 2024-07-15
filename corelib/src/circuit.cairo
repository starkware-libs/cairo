/// Given two circuit elements, returns a new circuit element representing the circuit that applies
/// the `addmod` operation to the two input circuits.
pub fn circuit_add<Lhs, Rhs, +CircuitElementTrait<Lhs>, +CircuitElementTrait<Rhs>>(
    lhs: CircuitElement<Lhs>, rhs: CircuitElement<Rhs>
) -> CircuitElement::<AddModGate<Lhs, Rhs>> {
    CircuitElement::<AddModGate<Lhs, Rhs>> {}
}


/// Given two circuit elements, returns a new circuit element representing the circuit that applies
/// the `submod` operation to the two input circuits.
pub fn circuit_sub<Lhs, Rhs, +CircuitElementTrait<Lhs>, +CircuitElementTrait<Rhs>>(
    lhs: CircuitElement<Lhs>, rhs: CircuitElement<Rhs>
) -> CircuitElement::<SubModGate<Lhs, Rhs>> {
    CircuitElement::<SubModGate<Lhs, Rhs>> {}
}

/// Given a circuit element, returns a new circuit element representing the circuit that applies
/// the inverse operation on the input circuit.
pub fn circuit_inverse<Input, +CircuitElementTrait<Input>>(
    input: CircuitElement<Input>
) -> CircuitElement::<InverseGate<Input>> {
    CircuitElement::<InverseGate<Input>> {}
}

/// Given two circuit elements, returns a new circuit element representing the circuit that applies
/// the `mul` operation to the two input circuits.
pub fn circuit_mul<Lhs, Rhs, +CircuitElementTrait<Lhs>, +CircuitElementTrait<Rhs>,>(
    lhs: CircuitElement<Lhs>, rhs: CircuitElement<Rhs>
) -> CircuitElement::<MulModGate<Lhs, Rhs>> {
    CircuitElement::<MulModGate<Lhs, Rhs>> {}
}

/// A 384-bit unsigned integer, used for circuit values.
#[derive(Copy, Drop, Debug, PartialEq)]
pub struct u384 {
    pub limb0: u96,
    pub limb1: u96,
    pub limb2: u96,
    pub limb3: u96,
}

pub type u96 = core::internal::BoundedInt<0, 79228162514264337593543950335>;
pub extern type RangeCheck96;
pub extern type AddMod;
pub extern type MulMod;


/// A type that can be used as a circuit modulus (a u384 that is not zero or one).
pub extern type CircuitModulus;

extern fn try_into_circuit_modulus(val: [u96; 4]) -> Option<CircuitModulus> nopanic;

impl U384TryIntoCircuitModulus of TryInto<[u96; 4], CircuitModulus> {
    fn try_into(self: [u96; 4]) -> Option<CircuitModulus> {
        try_into_circuit_modulus(self)
    }
}

/// Converts 'T' into a 'U96Guarantee'.
/// 'T' must be a a value that fits inside a u96, for example: u8, u96 or BoundedInt<0, 12>.
extern fn into_u96_guarantee<T>(val: T) -> U96Guarantee nopanic;
extern fn u96_guarantee_verify(guarantee: U96Guarantee) implicits(RangeCheck96) nopanic;


impl DestructU96Guarantee of Destruct<U96Guarantee> {
    fn destruct(self: U96Guarantee) nopanic {
        u96_guarantee_verify(self);
    }
}

/// A value that is guaranteed to fit in a u96.
extern type U96Guarantee;

/// Expose the const required by the libfunc to allow the compiler const reusage.
pub type ConstZero = core::internal::BoundedInt<0, 0>;
pub type ConstOne = core::internal::BoundedInt<1, 1>;

/// A type that creates a circuit from a tuple of outputs.
pub extern type Circuit<Outputs>;

/// Defines an input for a circuit.
#[phantom]
pub extern type CircuitInput<const N: usize>;
/// Represents the action of adding two fields elements in the circuits builtin.
#[phantom]
extern type AddModGate<Lhs, Rhs>;
/// Represents the action of multiplying two fields elements in the circuits builtin.
#[phantom]
extern type MulModGate<Lhs, Rhs>;
/// Represents the action of computing the difference between two fields elements in the circuits
/// builtin.
#[phantom]
extern type SubModGate<Lhs, Rhs>;

/// Represents the action of computing the inverse of a fields element in the circuits builtin.
#[phantom]
extern type InverseGate<Input>;

/// Initializes the input data for running an instance of the circuit.
extern fn init_circuit_data<C>() -> CircuitInputAccumulator<C> implicits(RangeCheck96) nopanic;

/// Returns the descriptor for the circuit.
extern fn get_circuit_descriptor<C>() -> CircuitDescriptor<C> nopanic;

/// The result of filling an input in the circuit instance's data.
type EvalCircuitResult<C> =
    Result<CircuitOutputs<C>, (CircuitPartialOutputs<C>, CircuitFailureGuarantee)>;

extern fn eval_circuit<C>(
    descriptor: CircuitDescriptor<C>,
    data: CircuitData<C>,
    modulus: CircuitModulus,
    zero: ConstZero,
    one: ConstOne,
) -> EvalCircuitResult<C> implicits(AddMod, MulMod) nopanic;

/// Fill an input in the circuit instance's data.
extern fn add_circuit_input<C>(
    accumulator: CircuitInputAccumulator<C>, value: [U96Guarantee; 4]
) -> AddInputResult<C> nopanic;

/// The result of filling an input in the circuit instance's data.
pub enum AddInputResult<C> {
    /// All inputs have been filled.
    Done: CircuitData<C>,
    /// More inputs are needed to fill the circuit instance's data.
    More: CircuitInputAccumulator<C>,
}

mod internal {
    use core::traits::PanicDestructForDestruct;
    impl AddInputResultDrop<C> of Drop<super::AddInputResult<C>>;
    impl CircuitDataDrop<C> of Drop<super::CircuitData<C>>;
    impl CircuitInputAccumulatorDrop<C> of Drop<super::CircuitInputAccumulator<C>>;

    pub impl PanicDestructAddInputResult<C> = PanicDestructForDestruct<super::AddInputResult<C>>;
    pub impl PanicDestructCircuitData<C> = PanicDestructForDestruct<super::CircuitData<C>>;
    pub impl PanicDestructCircuitInputAccumulator<C> =
        PanicDestructForDestruct<super::CircuitInputAccumulator<C>>;
}
impl PanicDestructAddInputResult<C> = internal::PanicDestructAddInputResult<C>;
impl PanicDestructCircuitData<C> = internal::PanicDestructCircuitData<C>;
impl PanicDestructCircuitInputAccumulator<C> = internal::PanicDestructCircuitInputAccumulator<C>;

/// Type for accumulating inputs into the circuit instance's data.
extern type CircuitInputAccumulator<C>;

/// A type representing a circuit instance data with all the inputs addeded.
extern type CircuitData<C>;

/// A type representing a circuit instance where the outputs are filled.
extern type CircuitOutputs<C>;

/// A type representing a circuit instance where the outputs are partially filled as
/// the evaluation of one of the inverse gates failed.
/// The type is defined for future-compatibility, there is currently no libfunc to extract
/// the partial outputs.
extern type CircuitPartialOutputs<C>;

/// A type representing a circuit descriptor.
extern type CircuitDescriptor<C>;

impl CircuitDescriptorDrop<C> of Drop<CircuitDescriptor<C>>;
impl CircuitModulusDrop of Drop<CircuitModulus>;
impl CircuitOutputsDrop<C> of Drop<CircuitOutputs<C>>;
impl CircuitPartialOutputsDrop<C> of Drop<CircuitPartialOutputs<C>>;

impl CircuitOutputsCopy<C> of Copy<CircuitOutputs<C>>;

/// A wrapper for circuit elements, used to construct circuits..
pub struct CircuitElement<T> {}
pub impl CircuitElementDrop<T> of Drop<CircuitElement<T>>;
pub impl CircuitElementCopy<T> of Copy<CircuitElement<T>>;

/// A marker trait for keeping track of which types are circuit elements.
pub trait CircuitElementTrait<T> {}
impl InputCircuitElement<const N: usize> of CircuitElementTrait<CircuitInput<N>> {}
impl AddModCircuitElement<
    Lhs, Rhs, +CircuitElementTrait<Lhs>, +CircuitElementTrait<Rhs>
> of CircuitElementTrait<AddModGate<Lhs, Rhs>> {}
impl SubModCircuitElement<
    Lhs, Rhs, +CircuitElementTrait<Lhs>, +CircuitElementTrait<Rhs>
> of CircuitElementTrait<SubModGate<Lhs, Rhs>> {}
impl InverseCircuitElement<
    Input, +CircuitElementTrait<Input>
> of CircuitElementTrait<InverseGate<Input>> {}
impl MulModCircuitElement<
    Lhs, Rhs, +CircuitElementTrait<Lhs>, +CircuitElementTrait<Rhs>
> of CircuitElementTrait<MulModGate<Lhs, Rhs>> {}

/// A trait for defining a circuit.
trait CircuitDefinition<CES> {
    /// The circuit internal type for a tuple of `CircuitElement`s.
    type CircuitType;
}
impl CircuitDefinitionImpl<
    T, impl Unwrap: UnwrapCircuitElement<T>, +core::metaprogramming::IsTuple<T>
> of CircuitDefinition<T> {
    type CircuitType = Circuit<Unwrap::Unwrapped>;
}

/// Helper trait to get the unwrapped type of a `CircuitElement`, as well as the tuple of unwrapped
/// types from a tuple of `CircuitElement`s.
trait UnwrapCircuitElement<T> {
    type Unwrapped;
}
/// Implementation for unwrapping a single `CircuitElement`.
impl UnwrapCircuitElementDirect<T> of UnwrapCircuitElement<CircuitElement<T>> {
    type Unwrapped = T;
}
/// Implementation for unwrapping a basic tuple of `CircuitElement`s.
impl UnwrapCircuitElementBase<
    T, impl UnwrapT: UnwrapCircuitElement<T>
> of UnwrapCircuitElement<(T,)> {
    type Unwrapped = (UnwrapT::Unwrapped,);
}
/// Implementation for unwrapping a tuple of `CircuitElement`s.
impl UnwrapCircuitElementNext<
    T,
    impl TS: core::metaprogramming::TupleSplit<T>,
    impl UnwrapHead: UnwrapCircuitElement<TS::Head>,
    impl UnwrapRest: UnwrapCircuitElement<TS::Rest>,
    impl TEF: core::metaprogramming::TupleExtendFront<UnwrapRest::Unwrapped, UnwrapHead::Unwrapped>,
> of UnwrapCircuitElement<T> {
    type Unwrapped = TEF::Result;
}

/// A trait for setting up instances of a circuit defined using `CircuitElement`s.
#[generate_trait]
pub impl CircuitInputsImpl<CES> of CircuitInputs<CES> {
    /// calls `init_circuit_data` for the given circuit.
    // Inlining to make sure possibly huge `CES` won't be in a user function name.
    #[inline]
    fn new_inputs<impl CD: CircuitDefinition<CES>, +Drop<CES>>(
        self: CES
    ) -> AddInputResult<CD::CircuitType> {
        AddInputResult::More(init_circuit_data::<CD::CircuitType>())
    }
}

/// A trait for getting the descriptor of a circuit.
#[generate_trait]
impl GetCircuitDescriptorImpl<CES> of GetCircuitDescriptor<CES> {
    /// calls `get_circuit_descriptor` for the given circuit.
    // Inlining to make sure possibly huge `C` won't be in a user function name.
    #[inline]
    fn get_descriptor<impl CD: CircuitDefinition<CES>, +Drop<CES>>(
        self: CES
    ) -> CircuitDescriptor<CD::CircuitType> {
        get_circuit_descriptor::<CD::CircuitType>()
    }
}

/// A trait for filling inputs in a circuit instance's data.
#[generate_trait]
pub impl AddInputResultImpl<C> of AddInputResultTrait<C> {
    /// Adds an input to the accumulator.
    // Inlining to make sure possibly huge `C` won't be in a user function name.
    #[inline]
    fn next<Value, +IntoCircuitInputValue<Value>, +Drop<Value>>(
        self: AddInputResult<C>, value: Value
    ) -> AddInputResult<C> {
        match self {
            AddInputResult::More(accumulator) => add_circuit_input(
                accumulator, value.into_circuit_input_value()
            ),
            AddInputResult::Done(_) => panic!("All inputs have been filled"),
        }
    }
    // Inlining to make sure possibly huge `C` won't be in a user function name.
    #[inline(always)]
    fn done(self: AddInputResult<C>) -> CircuitData<C> {
        match self {
            AddInputResult::Done(data) => data,
            AddInputResult::More(_) => panic!("Not all inputs have been filled"),
        }
    }
}

/// Trait for converting a value to a circuit input value.
trait IntoCircuitInputValue<T> {
    fn into_circuit_input_value(self: T) -> [U96Guarantee; 4];
}
impl U96sIntoCircuitInputValue of IntoCircuitInputValue<[u96; 4]> {
    fn into_circuit_input_value(self: [u96; 4]) -> [U96Guarantee; 4] {
        let [val0, val1, val2, val3] = self;
        [
            into_u96_guarantee(val0),
            into_u96_guarantee(val1),
            into_u96_guarantee(val2),
            into_u96_guarantee(val3),
        ]
    }
}

impl U384IntoCircuitInputValue of IntoCircuitInputValue<u384> {
    fn into_circuit_input_value(self: u384) -> [U96Guarantee; 4] {
        [
            into_u96_guarantee(self.limb0),
            into_u96_guarantee(self.limb1),
            into_u96_guarantee(self.limb2),
            into_u96_guarantee(self.limb3),
        ]
    }
}

/// A trait for evaluating a circuit.
#[generate_trait]
pub impl EvalCircuitImpl<C> of EvalCircuitTrait<C> {
    // Inlining to make sure possibly huge `C` won't be in a user function name.
    #[inline(always)]
    fn eval(self: CircuitData<C>, modulus: CircuitModulus) -> core::circuit::EvalCircuitResult<C> {
        self.eval_ex(get_circuit_descriptor::<C>(), modulus)
    }
    // Inlining to make sure possibly huge `C` won't be in a user function name.
    #[inline(always)]
    fn eval_ex(
        self: CircuitData<C>, descriptor: CircuitDescriptor<C>, modulus: CircuitModulus
    ) -> core::circuit::EvalCircuitResult<C> {
        eval_circuit::<C>(descriptor, self, modulus, 0, 1)
    }
}

/// A trait for evaluating a circuit.
pub trait CircuitOutputsTrait<Outputs, OutputElement> {
    /// Evaluates the circuit with the given data and modulus.
    fn get_output(self: Outputs, output: OutputElement,) -> u384;
}

impl CircuitOutputsImpl<
    C, Output
> of CircuitOutputsTrait<CircuitOutputs<C>, CircuitElement<Output>> {
    // Inlining to make sure possibly huge `C` won't be in a user function name.
    #[inline(always)]
    fn get_output(self: CircuitOutputs<C>, output: CircuitElement<Output>) -> u384 {
        let (res, _) = get_circuit_output::<C, Output>(self);
        res
    }
}


/// A type that contain that is used to guarantee that the circuit evaluation has failed.
///
/// The guarantee is verified by `circuit_failure_guarantee_verify`, which is the only way to
/// destruct this type. This way, one can trust that the guarantee holds although it has not yet
/// been verified.
extern type CircuitFailureGuarantee;

/// A type that contain that is used to guarantee that a u384 is less than another u384.
extern type U96LimbsLtGuarantee<const LIMB_COUNT: usize>;

/// Helper trait for finding the value of a const minus 1.
trait MinusOne<const NUM: usize> {
    const VALUE: usize;
}
impl MinusOneImpl4 of MinusOne<4> {
    const VALUE: usize = 3;
}
impl MinusOneImpl3 of MinusOne<3> {
    const VALUE: usize = 2;
}
impl MinusOneImpl2 of MinusOne<2> {
    const VALUE: usize = 1;
}

/// Trait helper to convert a multi-limb guarantee to a `U96Guarantee` of the first pair of limbs
/// that differ.
trait IntoU96Guarantee<const LIMB_COUNT: usize> {
    fn into_u96_guarantee(self: U96LimbsLtGuarantee<LIMB_COUNT>) -> U96Guarantee nopanic;
}
impl IntoU96GuaranteeImplByNext<
    const LIMB_COUNT: usize, impl MO: MinusOne<LIMB_COUNT>, +IntoU96Guarantee<MO::VALUE>,
> of IntoU96Guarantee<LIMB_COUNT> {
    fn into_u96_guarantee(self: U96LimbsLtGuarantee<LIMB_COUNT>) -> U96Guarantee nopanic {
        match u96_limbs_less_than_guarantee_verify(self) {
            NextU96LessThanGuarantee::Next(next) => next.into_u96_guarantee(),
            NextU96LessThanGuarantee::Final(guarantee) => guarantee,
        }
    }
}

impl IntoU96GuaranteeImplFinal of IntoU96Guarantee<1> {
    fn into_u96_guarantee(self: U96LimbsLtGuarantee<1>) -> U96Guarantee nopanic {
        u96_single_limb_less_than_guarantee_verify(self)
    }
}

/// Enum representing the result of the verification of a multi-limb less than guarantee.
enum NextU96LessThanGuarantee<const LIMB_COUNT: usize> {
    Next: U96LimbsLtGuarantee<LIMB_COUNT>,
    Final: U96Guarantee,
}

extern fn u96_limbs_less_than_guarantee_verify<
    const LIMB_COUNT: usize, impl MO: MinusOne<LIMB_COUNT>
>(
    guarantee: U96LimbsLtGuarantee<LIMB_COUNT>
) -> NextU96LessThanGuarantee<MO::VALUE> nopanic;

extern fn u96_single_limb_less_than_guarantee_verify(
    guarantee: U96LimbsLtGuarantee<1>
) -> U96Guarantee nopanic;

impl DestructDestructU96LimbsLtGuarantee4 of Destruct<U96LimbsLtGuarantee<4>> {
    fn destruct(self: U96LimbsLtGuarantee<4>) nopanic {
        self.into_u96_guarantee().destruct()
    }
}

/// Verifies the guarantee in order to drop it.
extern fn circuit_failure_guarantee_verify(
    guarantee: CircuitFailureGuarantee, zero: ConstZero, one: ConstOne,
) -> U96LimbsLtGuarantee<4> implicits(RangeCheck96, MulMod) nopanic;


pub impl DestructFailureGuarantee of Destruct<CircuitFailureGuarantee> {
    fn destruct(self: CircuitFailureGuarantee) nopanic {
        circuit_failure_guarantee_verify(self, 0, 1);
    }
}

extern fn get_circuit_output<C, Output>(
    outputs: CircuitOutputs<C>
) -> (u384, U96LimbsLtGuarantee<4>) nopanic;

/// Helper module to convert into `u384`.
mod conversions {
    use core::internal::BoundedInt;
    use core::RangeCheck;


    trait AddHelper<Lhs, Rhs> {
        type Result;
    }
    extern fn bounded_int_add<Lhs, Rhs, impl H: AddHelper<Lhs, Rhs>>(
        lhs: Lhs, rhs: Rhs
    ) -> H::Result nopanic;

    trait MulHelper<Lhs, Rhs> {
        type Result;
    }
    extern fn bounded_int_mul<Lhs, Rhs, impl H: MulHelper<Lhs, Rhs>>(
        lhs: Lhs, rhs: Rhs
    ) -> H::Result nopanic;

    trait DivRemHelper<Lhs, Rhs> {
        type DivT;
        type RemT;
    }
    extern fn bounded_int_div_rem<Lhs, Rhs, impl H: DivRemHelper<Lhs, Rhs>>(
        lhs: Lhs, rhs: NonZero<Rhs>,
    ) -> (H::DivT, H::RemT) implicits(RangeCheck) nopanic;

    type ConstValue<const VALUE: felt252> = BoundedInt<VALUE, VALUE>;
    const POW96: felt252 = 0x1000000000000000000000000;
    const NZ_POW96_TYPED: NonZero<ConstValue<POW96>> = 0x1000000000000000000000000;
    const POW64: felt252 = 0x10000000000000000;
    const NZ_POW64_TYPED: NonZero<ConstValue<POW64>> = 0x10000000000000000;
    const POW32: felt252 = 0x100000000;
    const POW32_TYPED: ConstValue<POW32> = 0x100000000;

    impl DivRemU128By96 of DivRemHelper<u128, ConstValue<POW96>> {
        type DivT = BoundedInt<0, { POW32 - 1 }>;
        type RemT = BoundedInt<0, { POW96 - 1 }>;
    }

    impl DivRemU128By64 of DivRemHelper<u128, ConstValue<POW64>> {
        type DivT = BoundedInt<0, { POW64 - 1 }>;
        type RemT = BoundedInt<0, { POW64 - 1 }>;
    }

    impl MulHelperImpl of MulHelper<BoundedInt<0, { POW64 - 1 }>, ConstValue<POW32>> {
        type Result = BoundedInt<0, { POW96 - POW32 }>;
    }

    impl AddHelperImpl of AddHelper<
        BoundedInt<0, { POW96 - POW32 }>, BoundedInt<0, { POW32 - 1 }>
    > {
        type Result = BoundedInt<0, { POW96 - 1 }>;
    }

    pub fn from_u128(value: u128) -> super::u384 {
        let (limb1, limb0) = bounded_int_div_rem(value, NZ_POW96_TYPED);
        core::circuit::u384 { limb0, limb1: core::integer::upcast(limb1), limb2: 0, limb3: 0 }
    }

    pub fn from_u256(value: u256) -> super::u384 {
        let (limb1_low32, limb0) = bounded_int_div_rem(value.low, NZ_POW96_TYPED);
        let (limb2, limb1_high64) = bounded_int_div_rem(value.high, NZ_POW64_TYPED);
        let limb1 = bounded_int_add(bounded_int_mul(limb1_high64, POW32_TYPED), limb1_low32);
        core::circuit::u384 { limb0, limb1, limb2: core::integer::upcast(limb2), limb3: 0 }
    }
}

impl U128IntoU384 of Into<u128, u384> {
    fn into(self: u128) -> u384 {
        conversions::from_u128(self)
    }
}

impl U256IntoU384 of Into<u256, u384> {
    fn into(self: u256) -> u384 {
        conversions::from_u256(self)
    }
}

impl Felt252IntoU384 of Into<felt252, u384> {
    fn into(self: felt252) -> u384 {
        conversions::from_u256(self.into())
    }
}
