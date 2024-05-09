/// Given two circuit elements, returns a new circuit element representing the circuit that applies
/// the `addmod` operation to the two input circuits.
pub fn circuit_add<Lhs, Rhs, +CircuitElementTrait<Lhs>, +CircuitElementTrait<Rhs>,>(
    lhs: CircuitElement<Lhs>, rhs: CircuitElement<Rhs>
) -> CircuitElement::<AddModGate<Lhs, Rhs>> {
    CircuitElement::<AddModGate<Lhs, Rhs>> {}
}

/// A 384-bit unsigned integer, used for circuit values.
#[derive(Copy, Drop)]
struct u384 {
    limb0: u96,
    limb1: u96,
    limb2: u96,
    limb3: u96,
}

pub type u96 = core::internal::BoundedInt<0, 79228162514264337593543950335>;
pub extern type RangeCheck96;
pub extern type AddMod;
pub extern type MulMod;

#[derive(Copy, Drop)]
struct u384 {
    limb0: u96,
    limb1: u96,
    limb2: u96,
    limb3: u96,
}


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
) -> CircuitOutput<C> implicits(AddMod, MulMod) nopanic;

/// Fill an input in the circuit instance's data.
// TODO(ilya): Consider using RangeCheck96Guarantee for the inputs.
extern fn fill_circuit_input<C>(
    accumulator: CircuitInputAccumulator<C>, value: [u96; 4]
) -> FillInputResult<C> nopanic;

/// The result of filling an input in the circuit instance's data.
enum FillInputResult<C> {
    /// More inputs are needed to fill the circuit instance's data.
    More: CircuitInputAccumulator<C>,
    /// All inputs have been filled.
    Done: CircuitData<C>,
}

/// Type for accumulating inputs into the circuit instance's data.
extern type CircuitInputAccumulator<C>;

/// A type representing a circuit instance data with all the inputs filled.
extern type CircuitData<C>;

/// A type representing a circuit instance where the outputs are filled.
extern type CircuitOutput<C>;

/// A type representing a circuit descriptor.
extern type CircuitDescriptor<C>;

impl CircuitInputAccumulatorDrop<C> of Drop<CircuitInputAccumulator<C>>;


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
}

impl SingleOutputCircuit<Out0> of CircuitDefinition<(CircuitElement<Out0>,)> {
    type CircuitType = Circuit<(Out0,)>;

    fn init(self: (CircuitElement<Out0>,)) -> CircuitInputAccumulator<Self::CircuitType> {
        init_circuit_data::<Self::CircuitType>()
    }
}
