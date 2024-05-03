/// Given two circuit elements, returns a new circuit element representing the circuit that applies
/// the `addmod` operation to the two input circuits.
pub fn circuit_add<Lhs, Rhs, +CircuitElementTrait<Lhs>, +CircuitElementTrait<Rhs>,>(
    lhs: CircuitElement<Lhs>, rhs: CircuitElement<Rhs>
) -> CircuitElement::<AddModGate<Lhs, Rhs>> {
    CircuitElement::<AddModGate<Lhs, Rhs>> {}
}


pub type u96 = core::internal::BoundedInt<0, 79228162514264337593543950335>;
pub extern type RangeCheck96;


/// Defines an input for a circuit.
#[phantom]
pub extern type CircuitInput<const N: usize>;
/// Represents the action of adding two fields elements in the circuits builtin.
#[phantom]
extern type AddModGate<Lhs, Rhs>;


/// Initializes the input data for running an instance of the circuit.
extern fn init_circuit_data<C>() -> CircuitInputAccumulator<C> implicits(RangeCheck96) nopanic;

/// Type for accumulating inputs into the circuit instance's data.
extern type CircuitInputAccumulator<C>;

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
    type CircuitType = (Out0,);

    fn init(self: (CircuitElement<Out0>,)) -> CircuitInputAccumulator<Self::CircuitType> {
        init_circuit_data::<(Out0,)>()
    }
}
