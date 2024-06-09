/// Given two circuit elements, returns a new circuit element representing the circuit that applies
/// the `addmod` operation to the two input circuits.
pub fn circuit_add<Lhs, Rhs, +CircuitElementTrait<Lhs>, +CircuitElementTrait<Rhs>>(
    lhs: CircuitElement<Lhs>, rhs: CircuitElement<Rhs>
) -> CircuitElement::<AddModGate<Lhs, Rhs>> {
    CircuitElement::<AddModGate<Lhs, Rhs>> {}
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
#[derive(Copy, Drop)]
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
pub enum EvalCircuitResult<C> {
    /// The circuit was evaluated successfully.
    Success: CircuitOutputs<C>,
    /// The circuit evaluation failed.
    Failure: (CircuitPartialOutputs<C>, CircuitFailureGuarantee),
}

extern fn eval_circuit<C>(
    descriptor: CircuitDescriptor<C>,
    data: CircuitData<C>,
    modulus: CircuitModulus,
    zero: ConstZero,
    one: ConstOne,
) -> EvalCircuitResult<C> implicits(AddMod, MulMod) nopanic;

/// Fill an input in the circuit instance's data.
extern fn fill_circuit_input<C>(
    accumulator: CircuitInputAccumulator<C>, value: [U96Guarantee; 4]
) -> FillInputResult<C> nopanic;

/// The result of filling an input in the circuit instance's data.
pub enum FillInputResult<C> {
    /// All inputs have been filled.
    Done: CircuitData<C>,
    /// More inputs are needed to fill the circuit instance's data.
    More: CircuitInputAccumulator<C>,
}

/// Type for accumulating inputs into the circuit instance's data.
extern type CircuitInputAccumulator<C>;

/// A type representing a circuit instance data with all the inputs filled.
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

impl CircuitDataDrop<C> of Drop<CircuitData<C>>;
impl CircuitDescriptorDrop<C> of Drop<CircuitDescriptor<C>>;
impl CircuitInputAccumulatorDrop<C> of Drop<CircuitInputAccumulator<C>>;
impl CircuitModulusDrop of Drop<CircuitModulus>;
impl CircuitOutputsDrop<C> of Drop<CircuitOutputs<C>>;
impl CircuitPartialOutputsDrop<C> of Drop<CircuitPartialOutputs<C>>;

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
impl InverseCircuitElement<
    Input, +CircuitElementTrait<Input>
> of CircuitElementTrait<InverseGate<Input>> {}
impl MulModCircuitElement<
    Lhs, Rhs, +CircuitElementTrait<Lhs>, +CircuitElementTrait<Rhs>
> of CircuitElementTrait<MulModGate<Lhs, Rhs>> {}


/// A trait for initializtion instances of a circuit defined using CircuitElements.
pub trait CircuitDefinition<CE> {
    type CircuitType;
    /// calls `init_circuit_data` for the given circuit.
    fn init(self: CE) -> CircuitInputAccumulator<Self::CircuitType>;
    /// calls `get_circuit_descriptor` for the given circuit.
    fn get_descriptor(self: CE) -> CircuitDescriptor<Self::CircuitType>;
}

impl SingleOutputCircuit<Out0> of CircuitDefinition<(CircuitElement<Out0>,)> {
    type CircuitType = Circuit<(Out0,)>;

    fn init(self: (CircuitElement<Out0>,)) -> CircuitInputAccumulator<Self::CircuitType> {
        init_circuit_data::<Self::CircuitType>()
    }

    fn get_descriptor(self: (CircuitElement<Out0>,)) -> CircuitDescriptor<Self::CircuitType> {
        get_circuit_descriptor::<Self::CircuitType>()
    }
}


/// A trait for evaluating a circuit.
pub trait InputAccumulatorTrait<InputAccumulator> {
    type CircuitType;

    /// Fills a circuit input with the given value.
    fn fill_input(self: InputAccumulator, value: [u96; 4]) -> FillInputResult<Self::CircuitType>;
}

impl InputAccumulatorTraitImpl<C> of InputAccumulatorTrait<CircuitInputAccumulator<C>> {
    type CircuitType = C;

    fn fill_input(self: CircuitInputAccumulator<C>, value: [u96; 4]) -> FillInputResult<C> {
        let [val0, val1, val2, val3] = value;
        let value = [
            into_u96_guarantee(val0),
            into_u96_guarantee(val1),
            into_u96_guarantee(val2),
            into_u96_guarantee(val3),
        ];
        fill_circuit_input::<C>(self, value)
    }
}

/// A trait for evaluating a circuit.
pub trait CircuitDescriptorTrait<Descriptor> {
    type CircuitType;

    /// Evaluates the circuit with the given data and modulus.
    fn eval(
        self: Descriptor, data: CircuitData<Self::CircuitType>, modulus: CircuitModulus
    ) -> core::circuit::EvalCircuitResult<Self::CircuitType>;
}

impl CircuitDescriptorImpl<C> of CircuitDescriptorTrait<CircuitDescriptor<C>> {
    type CircuitType = C;

    fn eval(
        self: CircuitDescriptor<C>, data: CircuitData<C>, modulus: CircuitModulus
    ) -> core::circuit::EvalCircuitResult<C> {
        eval_circuit::<C>(self, data, modulus, 0, 1)
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
extern type U96LimbsLTGuarantee<const LIMB_COUNT: usize>;

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
    fn into_u96_guarantee(self: U96LimbsLTGuarantee<LIMB_COUNT>) -> U96Guarantee nopanic;
}
impl IntoU96GuaranteeImplByNext<
    const LIMB_COUNT: usize, impl MO: MinusOne<LIMB_COUNT>, +IntoU96Guarantee<MO::VALUE>,
> of IntoU96Guarantee<LIMB_COUNT> {
    fn into_u96_guarantee(self: U96LimbsLTGuarantee<LIMB_COUNT>) -> U96Guarantee nopanic {
        match u96_limbs_less_than_guarantee_verify(self) {
            NextU96LessThanGuarantee::Next(next) => next.into_u96_guarantee(),
            NextU96LessThanGuarantee::Final(guarantee) => guarantee,
        }
    }
}

impl IntoU96GuaranteeImplFinal of IntoU96Guarantee<1> {
    fn into_u96_guarantee(self: U96LimbsLTGuarantee<1>) -> U96Guarantee nopanic {
        u96_single_limb_less_than_guarantee_verify(self)
    }
}

/// Enum representing the result of the verification of a multi-limb less than guarantee.
enum NextU96LessThanGuarantee<const LIMB_COUNT: usize> {
    Next: U96LimbsLTGuarantee<LIMB_COUNT>,
    Final: U96Guarantee,
}

extern fn u96_limbs_less_than_guarantee_verify<
    const LIMB_COUNT: usize, impl MO: MinusOne<LIMB_COUNT>
>(
    guarantee: U96LimbsLTGuarantee<LIMB_COUNT>
) -> NextU96LessThanGuarantee<MO::VALUE> nopanic;

extern fn u96_single_limb_less_than_guarantee_verify(
    guarantee: U96LimbsLTGuarantee<1>
) -> U96Guarantee nopanic;

impl DestructDestructU96LimbsLTGuarantee4 of Destruct<U96LimbsLTGuarantee<4>> {
    fn destruct(self: U96LimbsLTGuarantee<4>) nopanic {
        self.into_u96_guarantee().destruct()
    }
}

/// Verifies the guarantee in order to drop it.
extern fn circuit_failure_guarantee_verify(
    guarantee: CircuitFailureGuarantee, zero: ConstZero, one: ConstOne,
) -> U96LimbsLTGuarantee<4> implicits(RangeCheck96, MulMod) nopanic;


pub impl DestructFailureGuarantee of Destruct<CircuitFailureGuarantee> {
    fn destruct(self: CircuitFailureGuarantee) nopanic {
        circuit_failure_guarantee_verify(self, 0, 1);
    }
}

extern fn get_circuit_output<C, Output>(
    outputs: CircuitOutputs<C>
) -> (u384, U96LimbsLTGuarantee<4>) nopanic;
