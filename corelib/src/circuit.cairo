use core::zeroable;

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

/// Converts 'T' into a 'U96Guarantee'.
/// 'T' must be a a value that fits inside a u96, for example: u8, u96 or BoundedInt<0, 12>.
extern fn into_u96_guarantee<T>(val: T) -> U96Guarantee nopanic;

/// A guaranteed that a value can fit in a u96.
pub extern type U96Guarantee;

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
    /// The circuit evaluation failed.
    Failure: (CircuitPartialOutputs<C>, CircuitFailureGuarantee),
    /// The circuit was evaluated successfully.
    Success: CircuitOutputs<C>,
}

extern fn eval_circuit<C>(
    descriptor: CircuitDescriptor<C>,
    data: CircuitData<C>,
    modulus: NonZero<u384>,
    zero: ConstZero,
    one: ConstOne,
) -> EvalCircuitResult<C> implicits(AddMod, MulMod) nopanic;

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

/// A type representing a circuit instance where the outputs are partially filled as
/// the evaluation of one of the inverse gates failed.
/// The type is defined for future-compatibility, there is currently no libfunc to extract
/// the partial outputs.
extern type CircuitPartialOutputs<C>;

/// A type representing a circuit descriptor.
extern type CircuitDescriptor<C>;

impl CircuitInputAccumulatorDrop<C> of Drop<CircuitInputAccumulator<C>>;
impl CircuitDataDrop<C> of Drop<CircuitData<C>>;
impl CircuitDescriptorDrop<C> of Drop<CircuitDescriptor<C>>;
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
    ) -> core::circuit::EvalCircuitResult<Self::CircuitType>;
}

impl CircuitDescriptorImpl<C> of CircuitDescriptorTrait<CircuitDescriptor<C>> {
    type CircuitType = C;

    fn eval(
        self: CircuitDescriptor<C>, data: CircuitData<C>, modulus: NonZero<u384>
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
extern type U384LessThanGuarantee;

// TODO(ilya): Use a destructor.
pub impl DropU384LessThanGuarantee of Drop<U384LessThanGuarantee>;

/// Verifies the guarantee in order to drop it.
extern fn circuit_failure_guarantee_verify(
    guarantee: CircuitFailureGuarantee, zero: ConstZero, one: ConstOne,
) -> U384LessThanGuarantee implicits(RangeCheck96, MulMod) nopanic;


pub impl DestructFailureGuarantee of Destruct<CircuitFailureGuarantee> {
    fn destruct(self: CircuitFailureGuarantee) nopanic {
        circuit_failure_guarantee_verify(self, 0, 1);
    }
}

extern fn get_circuit_output<C, Output>(
    outputs: CircuitOutputs<C>
) -> (u384, U384LessThanGuarantee) nopanic;
