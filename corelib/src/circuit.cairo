use core::zeroable;

/// Given two circuit elements, returns a new circuit element representing the circuit that applies
/// the `addmod` operation to the two input circuits.
pub fn circuit_add<Lhs, Rhs, +CircuitElementTrait<Lhs>, +CircuitElementTrait<Rhs>,>(
    lhs: CircuitElement<Lhs>, rhs: CircuitElement<Rhs>
) -> CircuitElement::<AddModGate<Lhs, Rhs>> {
    CircuitElement::<AddModGate<Lhs, Rhs>> {}
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

/// Initializes the input data for running an instance of the circuit.
extern fn init_circuit_data<C>() -> CircuitInputAccumulator<C> implicits(RangeCheck96) nopanic;

/// Returns the descriptor for the circuit.
extern fn get_circuit_descriptor<C>() -> CircuitDescriptor<C> nopanic;

extern fn eval_circuit<C>(
    descriptor: CircuitDescriptor<C>,
    data: CircuitData<C>,
    modulus: NonZero<u384>,
    zero: ConstZero,
    one: ConstOne,
) -> CircuitOutputs<C> implicits(AddMod, MulMod) nopanic;

/// Fill an input in the circuit instance's data.
// TODO(ilya): Consider using RangeCheck96Guarantee for the inputs.
extern fn fill_circuit_input<C>(
    accumulator: CircuitInputAccumulator<C>, value: [u96; 4]
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

/// A type representing a circuit descriptor.
extern type CircuitDescriptor<C>;

impl CircuitInputAccumulatorDrop<C> of Drop<CircuitInputAccumulator<C>>;
impl CircuitDataDrop<C> of Drop<CircuitData<C>>;
impl CircuitDescriptorDrop<C> of Drop<CircuitDescriptor<C>>;
impl CircuitOutputsDrop<C> of Drop<CircuitOutputs<C>>;


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
        fill_circuit_input::<C>(self, value)
    }
}

extern fn u384_is_zero(a: u384) -> zeroable::IsZeroResult<u384> implicits() nopanic;

impl U384TryIntoNonZero of TryInto<u384, NonZero<u384>> {
    fn try_into(self: u384) -> Option<NonZero<u384>> {
        match u384_is_zero(self) {
            zeroable::IsZeroResult::Zero => Option::None,
            zeroable::IsZeroResult::NonZero(x) => Option::Some(x),
        }
    }
}

/// A trait for evaluating a circuit.
pub trait CircuitDescriptorTrait<Descriptor> {
    type CircuitType;

    /// Evaluates the circuit with the given data and modulus.
    fn eval(
        self: Descriptor, data: CircuitData<Self::CircuitType>, modulus: NonZero<u384>
    ) -> core::circuit::CircuitOutputs<Self::CircuitType>;
}

impl CircuitDescriptorImpl<C> of CircuitDescriptorTrait<CircuitDescriptor<C>> {
    type CircuitType = C;

    fn eval(
        self: CircuitDescriptor<C>, data: CircuitData<C>, modulus: NonZero<u384>
    ) -> core::circuit::CircuitOutputs<C> {
        eval_circuit::<C>(self, data, modulus, 0, 1)
    }
}
