pub type u96 = core::internal::BoundedInt<0, 79228162514264337593543950335>;
pub extern type RangeCheck96;


// Defines an input for a circuit.
#[phantom]
pub extern type CircuitInput<const N: usize>;


// Initializes the input data for running an instance of the circuit.
extern fn init_circuit_data<C>() -> CircuitInputAccumulator<C> implicits(RangeCheck96) nopanic;

// Type for accumulating inputs into the circuit instance's data.
extern type CircuitInputAccumulator<C>;

impl CircuitInputAccumulatorDrop<C> of Drop<CircuitInputAccumulator<C>>;
