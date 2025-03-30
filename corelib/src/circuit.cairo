//! Efficient modular arithmetic computations using arithmetic circuits.
//!
//! This module provides a type-safe way to perform modular arithmetic operations using
//! arithmetic circuits. It is particularly useful for implementing cryptographic algorithms
//! and other computations that require efficient modular arithmetic with large numbers.
//!
//! # Core Features
//!
//! - Modular arithmetic operations (add, subtract, multiply, inverse)
//! - Support for numbers up to 384 bits
//! - Type-safe circuit construction
//! - Efficient evaluation of complex expressions
//!
//! # Examples
//!
//! ## Basic Arithmetic
//!
//! Here's an example showing basic modular arithmetic operations:
//!
//! ```
//! use core::circuit::{
//!    CircuitElement, EvalCircuitTrait, CircuitOutputsTrait, CircuitInput, CircuitModulus,
//!    AddInputResultTrait, CircuitInputs, circuit_add, circuit_mul,
//! };
//!
//! // Compute (a + b) * c mod p
//! let a = CircuitElement::<CircuitInput<0>> {};
//! let b = CircuitElement::<CircuitInput<1>> {};
//! let c = CircuitElement::<CircuitInput<2>> {};
//!
//! let sum = circuit_add(a, b);
//! let result = circuit_mul(sum, c);
//!
//! // Evaluate with inputs [3, 6, 2] modulo 7
//! let modulus = TryInto::<_, CircuitModulus>::try_into([7, 0, 0, 0]).unwrap();
//! let outputs = (result,)
//!     .new_inputs()
//!     .next([3, 0, 0, 0])
//!     .next([6, 0, 0, 0])
//!     .next([2, 0, 0, 0])
//!     .done()
//!     .eval(modulus)
//!     .unwrap();
//!
//! // Result: (3 + 6) * 2 mod 7 = 4
//! assert!(outputs.get_output(result) == 4.into());
//! ```
//!
//! # How It Works
//!
//! The module uses a type-based approach to construct and evaluate arithmetic circuits:
//!
//! 1. Circuit elements are created using `CircuitElement<T>` where T defines their role (input or
//! gate)
//! 2. Basic operations combine elements into more complex expressions (chaining gates to create a
//! circuit)
//! 3. The final circuit is evaluated with specific input values and a modulus
//!
//! Operations are performed using a multi-limb representation for large numbers,
//! with each number represented as four 96-bit limbs allowing for values up to 384 bits.
//!
//! # Performance Considerations
//!
//! - Circuit evaluation is optimized for large modular arithmetic operations
//! - The multi-limb representation allows efficient handling of large numbers
//! - Circuit construction has zero runtime overhead due to type-based approach
//!
//! # Errors
//!
//! Circuit evaluation can fail in certain cases:
//! - When computing multiplicative inverses of non-invertible elements
//! - When the modulus is 0 or 1
//! In that case the evaluation will return an Error.

/// Creates a new circuit element representing addition modulo p of two input circuits.
///
/// This function combines two circuit elements using modular addition, creating a new circuit
/// element that represents their sum modulo the circuit's modulus.
///
/// # Arguments
///
/// * `lhs` - Left-hand side circuit element
/// * `rhs` - Right-hand side circuit element
///
/// # Returns
///
/// A new circuit element representing `(lhs + rhs) mod p`
///
/// # Examples
///
/// ```
/// let a = CircuitElement::<CircuitInput<0>> {};
/// let b = CircuitElement::<CircuitInput<1>> {};
/// let sum = circuit_add(a, b);
/// ```
pub fn circuit_add<Lhs, Rhs, +CircuitElementTrait<Lhs>, +CircuitElementTrait<Rhs>>(
    lhs: CircuitElement<Lhs>, rhs: CircuitElement<Rhs>,
) -> CircuitElement<AddModGate<Lhs, Rhs>> {
    CircuitElement::<AddModGate<Lhs, Rhs>> {}
}

/// Creates a new circuit element representing subtraction modulo p of two input circuits.
///
/// This function combines two circuit elements using modular subtraction, creating a new circuit
/// element that represents their difference modulo the circuit's modulus.
///
/// # Arguments
///
/// * `lhs` - Left-hand side circuit element (minuend)
/// * `rhs` - Right-hand side circuit element (subtrahend)
///
/// # Returns
///
/// A new circuit element representing `(lhs - rhs) mod p`
///
/// # Examples
///
/// ```
/// let a = CircuitElement::<CircuitInput<0>> {};
/// let b = CircuitElement::<CircuitInput<1>> {};
/// let diff = circuit_sub(a, b);
/// ```
pub fn circuit_sub<Lhs, Rhs, +CircuitElementTrait<Lhs>, +CircuitElementTrait<Rhs>>(
    lhs: CircuitElement<Lhs>, rhs: CircuitElement<Rhs>,
) -> CircuitElement<SubModGate<Lhs, Rhs>> {
    CircuitElement::<SubModGate<Lhs, Rhs>> {}
}

/// Creates a new circuit element representing the multiplicative inverse modulo p of an input
/// circuit.
///
/// This function creates a new circuit element representing the multiplicative inverse of the input
/// element modulo the circuit's modulus. The operation will fail during evaluation if the input
/// is not invertible (not coprime with the modulus).
///
/// # Arguments
///
/// * `input` - Circuit element to compute the inverse of
///
/// # Returns
///
/// A new circuit element representing `input^(-1) mod p`
///
/// # Examples
///
/// ```
/// let a = CircuitElement::<CircuitInput<0>> {};
/// let inv_a = circuit_inverse(a);
/// ```
pub fn circuit_inverse<Input, +CircuitElementTrait<Input>>(
    input: CircuitElement<Input>,
) -> CircuitElement<InverseGate<Input>> {
    CircuitElement::<InverseGate<Input>> {}
}

/// Creates a new circuit element representing multiplication modulo p of two input circuits.
///
/// This function combines two circuit elements using modular multiplication, creating a new circuit
/// element that represents their product modulo the circuit's modulus.
///
/// # Arguments
///
/// * `lhs` - Left-hand side circuit element
/// * `rhs` - Right-hand side circuit element
///
/// # Returns
///
/// A new circuit element representing `(lhs * rhs) mod p`
///
/// # Examples
///
/// ```
/// let a = CircuitElement::<CircuitInput<0>> {};
/// let b = CircuitElement::<CircuitInput<1>> {};
/// let product = circuit_mul(a, b);
/// ```
pub fn circuit_mul<Lhs, Rhs, +CircuitElementTrait<Lhs>, +CircuitElementTrait<Rhs>>(
    lhs: CircuitElement<Lhs>, rhs: CircuitElement<Rhs>,
) -> CircuitElement<MulModGate<Lhs, Rhs>> {
    CircuitElement::<MulModGate<Lhs, Rhs>> {}
}

/// A 384-bit unsigned integer, used for circuit values.
#[derive(Copy, Drop, Debug, PartialEq)]
pub struct u384 {
    /// The least significant 96 bits
    pub limb0: u96,
    /// Bits 96-191
    pub limb1: u96,
    /// Bits 192-287
    pub limb2: u96,
    /// The most significant 96 bits
    pub limb3: u96,
}

/// A 96-bit unsigned integer type used as the basic building block for multi-limb arithmetic.
pub type u96 = crate::internal::bounded_int::BoundedInt<0, 79228162514264337593543950335>;

/// Range check builtin for 96-bit operations.
pub extern type RangeCheck96;

/// Builtin for modular addition operations.
pub extern type AddMod;

/// Builtin for modular multiplication operations.
pub extern type MulMod;

/// A type that can be used as a circuit modulus (a u384 that is not zero or one).
///
/// The modulus defines the finite field over which the circuit operates. It must be:
/// - A 384-bit number (represented as four 96-bit limbs)
/// - Not zero or one
/// - Typically a prime number for cryptographic applications
pub extern type CircuitModulus;

extern fn try_into_circuit_modulus(val: [u96; 4]) -> Option<CircuitModulus> nopanic;

impl U384TryIntoCircuitModulus of TryInto<[u96; 4], CircuitModulus> {
    fn try_into(self: [u96; 4]) -> Option<CircuitModulus> {
        try_into_circuit_modulus(self)
    }
}

/// Converts 'T' into a 'U96Guarantee'.
/// 'T' must be a value that fits inside a u96, for example: u8, u96 or BoundedInt<0, 12>.
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
pub type ConstZero = crate::internal::bounded_int::UnitInt<0>;
pub type ConstOne = crate::internal::bounded_int::UnitInt<1>;

/// A type that creates a circuit from a tuple of outputs.
///
/// This type represents a complete circuit instance, constructed from its output gates.
/// The type parameter `Outputs` defines the structure of the circuit's outputs.
pub extern type Circuit<Outputs>;

/// Defines an input for a circuit.
///
/// Represents an input signal in the circuit, indexed by `N`. Each input must be assigned
/// a value before circuit evaluation.
#[phantom]
pub extern type CircuitInput<const N: usize>;

/// Represents the action of adding two field elements in the circuits builtin.
///
/// This gate computes `(lhs + rhs) mod p` where `p` is the circuit modulus.
#[phantom]
extern type AddModGate<Lhs, Rhs>;

/// Represents the action of multiplying two field elements in the circuits builtin.
///
/// This gate computes `(lhs * rhs) mod p` where `p` is the circuit modulus.
#[phantom]
extern type MulModGate<Lhs, Rhs>;

/// Represents the action of computing the difference between two field elements in the circuits
/// builtin.
///
/// This gate computes `(lhs - rhs) mod p` where `p` is the circuit modulus.
#[phantom]
extern type SubModGate<Lhs, Rhs>;

/// Represents the action of computing the inverse of a field element in the circuits builtin.
///
/// This gate computes `x^(-1) mod p` where `p` is the circuit modulus.
/// The operation will fail if the input is not invertible (not coprime with the modulus).
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
    accumulator: CircuitInputAccumulator<C>, value: [U96Guarantee; 4],
) -> AddInputResult<C> nopanic;

/// The result of filling an input in the circuit instance's data.
///
/// This enum represents the state of input filling process, indicating whether
/// all inputs have been provided or more are needed.
pub enum AddInputResult<C> {
    /// All inputs have been filled and the circuit data is complete.
    Done: CircuitData<C>,
    /// More inputs are needed to complete the circuit instance's data.
    More: CircuitInputAccumulator<C>,
}

mod internal {
    use crate::traits::PanicDestructForDestruct;
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

/// A type representing a circuit instance data with all the inputs added.
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
impl CircuitModulusCopy of Copy<CircuitModulus>;
impl CircuitModulusDrop of Drop<CircuitModulus>;
impl CircuitOutputsDrop<C> of Drop<CircuitOutputs<C>>;
impl CircuitPartialOutputsDrop<C> of Drop<CircuitPartialOutputs<C>>;
impl CircuitOutputsCopy<C> of Copy<CircuitOutputs<C>>;

/// A wrapper for circuit elements, used to construct circuits.
///
/// This type provides a generic wrapper around different circuit components (inputs, gates)
/// and enables composition of circuit elements through arithmetic operations.
/// The type parameter `T` defines the specific role of the element in the circuit.
pub struct CircuitElement<T> {}
pub impl CircuitElementDrop<T> of Drop<CircuitElement<T>>;
pub impl CircuitElementCopy<T> of Copy<CircuitElement<T>>;

/// A marker trait for keeping track of which types are valid circuit elements.
///
/// This trait is implemented for all valid circuit components including inputs and gates.
/// It provides type safety when composing circuit elements.
pub trait CircuitElementTrait<T> {}

impl InputCircuitElement<const N: usize> of CircuitElementTrait<CircuitInput<N>> {}
impl AddModCircuitElement<
    Lhs, Rhs, +CircuitElementTrait<Lhs>, +CircuitElementTrait<Rhs>,
> of CircuitElementTrait<AddModGate<Lhs, Rhs>> {}
impl SubModCircuitElement<
    Lhs, Rhs, +CircuitElementTrait<Lhs>, +CircuitElementTrait<Rhs>,
> of CircuitElementTrait<SubModGate<Lhs, Rhs>> {}
impl InverseCircuitElement<
    Input, +CircuitElementTrait<Input>,
> of CircuitElementTrait<InverseGate<Input>> {}
impl MulModCircuitElement<
    Lhs, Rhs, +CircuitElementTrait<Lhs>, +CircuitElementTrait<Rhs>,
> of CircuitElementTrait<MulModGate<Lhs, Rhs>> {}

/// A trait for defining a circuit's structure and behavior.
///
/// This trait is used to define the structure of a circuit, including its inputs,
/// gates, and outputs. It provides the foundation for circuit evaluation.
/// The `CES` type parameter represents a tuple of `CircuitElement`s that together
/// define the circuit's structure.
pub trait CircuitDefinition<CES> {
    /// The internal circuit type representing a tuple of `CircuitElement`s.
    type CircuitType;
}

impl CircuitDefinitionImpl<
    T, impl Unwrap: UnwrapCircuitElement<T>, +crate::metaprogramming::IsTuple<T>,
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
    T, impl UnwrapT: UnwrapCircuitElement<T>,
> of UnwrapCircuitElement<(T,)> {
    type Unwrapped = (UnwrapT::Unwrapped,);
}

/// Implementation for unwrapping a tuple of `CircuitElement`s.
impl UnwrapCircuitElementNext<
    T,
    impl TS: crate::metaprogramming::TupleSplit<T>,
    impl UnwrapHead: UnwrapCircuitElement<TS::Head>,
    impl UnwrapRest: UnwrapCircuitElement<TS::Rest>,
    impl TEF: crate::metaprogramming::TupleExtendFront<
        UnwrapRest::Unwrapped, UnwrapHead::Unwrapped,
    >,
> of UnwrapCircuitElement<T> {
    type Unwrapped = TEF::Result;
}

/// A trait for setting up instances of a circuit defined using `CircuitElement`s.
///
/// This trait provides functionality to initialize and manage circuit inputs.
/// The `CES` type parameter represents a tuple of circuit elements (e.g., `(output,)`)
/// that define the circuit's structure. It is used in conjunction with
/// `AddInputResultTrait` to build circuit instances.
///
/// # Examples
///
/// ```
/// let a = CircuitElement::<CircuitInput<0>> {};
/// let b = CircuitElement::<CircuitInput<1>> {};
/// let modulus = TryInto::<_, CircuitModulus>::try_into([2, 0, 0, 0]).unwrap();
/// let circuit = (a,b).new_inputs() // returns AddInputResult::More, inputs are not yet filled
///     .next([10, 0, 0, 0])
///     .next([11, 0, 0, 0])
///     .done()
///     .eval(modulus)
///     .unwrap();
/// assert!(circuit.get_output(a) == 0.into());
/// assert!(circuit.get_output(b) == 1.into());
/// ```
#[generate_trait]
pub impl CircuitInputsImpl<CES> of CircuitInputs<CES> {
    /// Initializes a new circuit instance with inputs.
    ///
    /// This function creates a new input accumulator for the circuit, which can then
    /// be used to add input values sequentially.
    ///
    /// # Returns
    ///
    /// An `AddInputResult` that can be used to add input values to the circuit
    #[inline]
    fn new_inputs<impl CD: CircuitDefinition<CES>, +Drop<CES>>(
        self: CES,
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
        self: CES,
    ) -> CircuitDescriptor<CD::CircuitType> {
        get_circuit_descriptor::<CD::CircuitType>()
    }
}

/// A trait for filling inputs in a circuit instance's data.
///
/// This trait provides methods to add input values to a circuit instance and
/// finalize the input process.
///
/// # Examples
///
/// ```
/// let a = CircuitElement::<CircuitInput<0>> {};
/// let b = CircuitElement::<CircuitInput<1>> {};
/// let modulus = TryInto::<_, CircuitModulus>::try_into([2, 0, 0, 0]).unwrap();
/// let circuit = (a,b).new_inputs()
///     .next([10, 0, 0, 0]) // returns AddInputResult::More, inputs are not yet filled
///     .next([11, 0, 0, 0]) // returns AddInputResult::Done, inputs are filled
///     .done() // returns CircuitData<C>, inputs are filled
///     .eval(modulus)
///     .unwrap();
/// assert!(circuit.get_output(a) == 0.into());
/// assert!(circuit.get_output(b) == 1.into());
/// ```
#[generate_trait]
pub impl AddInputResultImpl<C> of AddInputResultTrait<C> {
    /// Adds an input value to the circuit instance.
    ///
    /// # Arguments
    ///
    /// * `value` - The value to add as input, must be convertible to circuit input value
    ///
    /// # Returns
    ///
    /// A new `AddInputResult` that can be used to add more inputs or finalize
    ///
    /// # Panics
    ///
    /// Panics if all inputs have already been filled
    #[inline]
    fn next<Value, +IntoCircuitInputValue<Value>, +Drop<Value>>(
        self: AddInputResult<C>, value: Value,
    ) -> AddInputResult<C> {
        match self {
            AddInputResult::More(accumulator) => add_circuit_input(
                accumulator, value.into_circuit_input_value(),
            ),
            AddInputResult::Done(_) => core::panic_with_felt252('All inputs have been filled'),
        }
    }

    /// Finalizes the input process and returns the circuit data.
    ///
    /// # Returns
    ///
    /// The complete circuit data ready for evaluation
    ///
    /// # Panics
    ///
    /// Panics if not all required inputs have been filled
    // Inlining to make sure possibly huge `C` won't be in a user function name.
    #[inline]
    fn done(self: AddInputResult<C>) -> CircuitData<C> {
        match self {
            AddInputResult::Done(data) => data,
            AddInputResult::More(_) => core::panic_with_felt252('Not all inputs have been filled'),
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
            into_u96_guarantee(val0), into_u96_guarantee(val1), into_u96_guarantee(val2),
            into_u96_guarantee(val3),
        ]
    }
}

impl U384IntoCircuitInputValue of IntoCircuitInputValue<u384> {
    fn into_circuit_input_value(self: u384) -> [U96Guarantee; 4] {
        [
            into_u96_guarantee(self.limb0), into_u96_guarantee(self.limb1),
            into_u96_guarantee(self.limb2), into_u96_guarantee(self.limb3),
        ]
    }
}

/// A trait for evaluating a circuit.
///
/// This trait provides methods to evaluate a circuit with given inputs and modulus.
/// The evaluation can be done with or without an explicit circuit descriptor.
///
/// # Examples
///
/// ```
/// let a = CircuitElement::<CircuitInput<0>> {};
/// let b = CircuitElement::<CircuitInput<1>> {};
/// let modulus = TryInto::<_, CircuitModulus>::try_into([2, 0, 0, 0]).unwrap();
/// let circuit = (a,b).new_inputs()
///     .next([10, 0, 0, 0])
///     .next([11, 0, 0, 0])
///     .done()
///     .eval(modulus) // Performs the circuit evaluation with the given modulus and returns the
///     Result .unwrap();
/// assert!(circuit.get_output(a) == 0.into());
/// assert!(circuit.get_output(b) == 1.into());
/// ```
#[generate_trait]
pub impl EvalCircuitImpl<C> of EvalCircuitTrait<C> {
    /// Evaluates the circuit with the given modulus.
    ///
    /// # Arguments
    ///
    /// * `modulus` - The modulus to use for arithmetic operations
    ///
    /// # Returns
    ///
    /// Result containing either the circuit outputs or a failure indication
    // Inlining to make sure possibly huge `C` won't be in a user function name.
    #[inline]
    fn eval(self: CircuitData<C>, modulus: CircuitModulus) -> crate::circuit::EvalCircuitResult<C> {
        self.eval_ex(get_circuit_descriptor::<C>(), modulus)
    }

    /// Evaluates the circuit with an explicit descriptor and modulus.
    ///
    /// # Arguments
    ///
    /// * `descriptor` - The circuit descriptor
    /// * `modulus` - The modulus to use for arithmetic operations
    ///
    /// # Returns
    ///
    /// Result containing either the circuit outputs or a failure indication
    // Inlining to make sure possibly huge `C` won't be in a user function name.
    #[inline]
    fn eval_ex(
        self: CircuitData<C>, descriptor: CircuitDescriptor<C>, modulus: CircuitModulus,
    ) -> crate::circuit::EvalCircuitResult<C> {
        eval_circuit::<C>(descriptor, self, modulus, 0, 1)
    }
}

/// A trait for retrieving output values from a circuit evaluation.
///
/// This trait provides methods to access the output values of a circuit after
/// successful evaluation.
///
/// # Examples
///
/// ```
/// let a = CircuitElement::<CircuitInput<0>> {};
/// let b = CircuitElement::<CircuitInput<1>> {};
/// let modulus = TryInto::<_, CircuitModulus>::try_into([2, 0, 0, 0]).unwrap();
/// let circuit = (a,b).new_inputs()
///     .next([10, 0, 0, 0])
///     .next([11, 0, 0, 0])
///     .done()
///     .eval(modulus)
///     .unwrap();
/// let a_mod_2 = circuit.get_output(a); // Returns the output value of `a mod 2`
/// let b_mod_2 = circuit.get_output(b); // Returns the output value of `b mod 2`
/// assert!(a_mod_2 == 0.into());
/// assert!(b_mod_2 == 1.into());
/// ```
pub trait CircuitOutputsTrait<Outputs, OutputElement> {
    /// Gets the output value for a specific circuit element.
    ///
    /// # Arguments
    ///
    /// * `output` - The circuit element to get the output for
    ///
    /// # Returns
    ///
    /// The output value as a u384
    fn get_output(self: Outputs, output: OutputElement) -> u384;
}

impl CircuitOutputsImpl<
    C, Output,
> of CircuitOutputsTrait<CircuitOutputs<C>, CircuitElement<Output>> {
    // Inlining to make sure possibly huge `C` won't be in a user function name.
    #[inline]
    fn get_output(self: CircuitOutputs<C>, output: CircuitElement<Output>) -> u384 {
        let (res, _) = get_circuit_output::<C, Output>(self);
        res
    }
}

/// A type that is used to guarantee that the circuit evaluation has failed.
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
    const LIMB_COUNT: usize, impl MO: MinusOne<LIMB_COUNT>,
>(
    guarantee: U96LimbsLtGuarantee<LIMB_COUNT>,
) -> NextU96LessThanGuarantee<MO::VALUE> nopanic;

extern fn u96_single_limb_less_than_guarantee_verify(
    guarantee: U96LimbsLtGuarantee<1>,
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
    outputs: CircuitOutputs<C>,
) -> (u384, U96LimbsLtGuarantee<4>) nopanic;

/// Helper module to convert into `u384`.
mod conversions {
    use crate::integer::{downcast, upcast};
    use crate::internal::bounded_int;
    use crate::internal::bounded_int::{AddHelper, BoundedInt, DivRemHelper, MulHelper, UnitInt};
    use super::{u384, u96};

    const POW128: felt252 = 0x100000000000000000000000000000000;
    const POW96: felt252 = 0x1000000000000000000000000;
    const POW96_TYPED: UnitInt<POW96> = 0x1000000000000000000000000;
    const NZ_POW96_TYPED: NonZero<UnitInt<POW96>> = 0x1000000000000000000000000;
    const POW64: felt252 = 0x10000000000000000;
    const POW64_TYPED: UnitInt<POW64> = 0x10000000000000000;
    const NZ_POW64_TYPED: NonZero<UnitInt<POW64>> = 0x10000000000000000;
    const POW32: felt252 = 0x100000000;
    const POW32_TYPED: UnitInt<POW32> = 0x100000000;
    const NZ_POW32_TYPED: NonZero<UnitInt<POW32>> = 0x100000000;

    impl DivRemU128By96 of DivRemHelper<u128, UnitInt<POW96>> {
        type DivT = BoundedInt<0, { POW32 - 1 }>;
        type RemT = BoundedInt<0, { POW96 - 1 }>;
    }

    impl DivRemU128By64 of DivRemHelper<u128, UnitInt<POW64>> {
        type DivT = BoundedInt<0, { POW64 - 1 }>;
        type RemT = BoundedInt<0, { POW64 - 1 }>;
    }

    impl DivRemU96By32 of DivRemHelper<u96, UnitInt<POW32>> {
        type DivT = BoundedInt<0, { POW64 - 1 }>;
        type RemT = BoundedInt<0, { POW32 - 1 }>;
    }

    impl DivRemU96By64 of DivRemHelper<u96, UnitInt<POW64>> {
        type DivT = BoundedInt<0, { POW32 - 1 }>;
        type RemT = BoundedInt<0, { POW64 - 1 }>;
    }

    impl MulHelper64By32Impl of MulHelper<BoundedInt<0, { POW64 - 1 }>, UnitInt<POW32>> {
        type Result = BoundedInt<0, { POW96 - POW32 }>;
    }

    impl MulHelper32By96Impl of MulHelper<BoundedInt<0, { POW32 - 1 }>, UnitInt<POW96>> {
        type Result = BoundedInt<0, { POW128 - POW96 }>;
    }

    impl MulHelper64By64Impl of MulHelper<BoundedInt<0, { POW64 - 1 }>, UnitInt<POW64>> {
        type Result = BoundedInt<0, { POW128 - POW64 }>;
    }

    impl AddHelperTo96By32Impl of AddHelper<
        BoundedInt<0, { POW96 - POW32 }>, BoundedInt<0, { POW32 - 1 }>,
    > {
        type Result = u96;
    }

    impl AddHelperTo128By64Impl of AddHelper<
        BoundedInt<0, { POW128 - POW64 }>, BoundedInt<0, { POW64 - 1 }>,
    > {
        type Result = BoundedInt<0, { POW128 - 1 }>;
    }

    impl AddHelperTo128By96Impl of AddHelper<BoundedInt<0, { POW128 - POW96 }>, u96> {
        type Result = BoundedInt<0, { POW128 - 1 }>;
    }

    pub fn from_u128(value: u128) -> u384 {
        let (limb1, limb0) = bounded_int::div_rem(value, NZ_POW96_TYPED);
        u384 { limb0, limb1: upcast(limb1), limb2: 0, limb3: 0 }
    }

    pub fn from_u256(value: u256) -> u384 {
        let (limb1_low32, limb0) = bounded_int::div_rem(value.low, NZ_POW96_TYPED);
        let (limb2, limb1_high64) = bounded_int::div_rem(value.high, NZ_POW64_TYPED);
        let limb1 = bounded_int::add(bounded_int::mul(limb1_high64, POW32_TYPED), limb1_low32);
        u384 { limb0, limb1, limb2: upcast(limb2), limb3: 0 }
    }

    pub fn felt252_try_into_two_u96(value: felt252) -> Option<(u96, u96)> {
        let v: u256 = value.into();
        let (limb1_low32, limb0) = bounded_int::div_rem(v.low, NZ_POW96_TYPED);
        let limb1_high64: BoundedInt<0, { POW64 - 1 }> = downcast(v.high)?;
        let limb1 = bounded_int::add(bounded_int::mul(limb1_high64, POW32_TYPED), limb1_low32);
        Some((limb0, limb1))
    }

    pub fn try_into_u128(value: u384) -> Option<u128> {
        if value.limb2 != 0 || value.limb3 != 0 {
            return None;
        }
        let (limb1_high, limb1_low) = bounded_int::div_rem(value.limb1, NZ_POW32_TYPED);
        if limb1_high != 0 {
            return None;
        }
        Some(upcast(bounded_int::add(bounded_int::mul(limb1_low, POW96_TYPED), value.limb0)))
    }

    pub fn try_into_u256(value: u384) -> Option<u256> {
        if value.limb3 != 0 {
            return None;
        }
        let (limb2_high, limb2_low) = bounded_int::div_rem(value.limb2, NZ_POW64_TYPED);
        if limb2_high != 0 {
            return None;
        }
        let (limb1_high, limb1_low) = bounded_int::div_rem(value.limb1, NZ_POW32_TYPED);
        Some(
            u256 {
                high: upcast(
                    bounded_int::add(bounded_int::mul(limb2_low, POW64_TYPED), limb1_high),
                ),
                low: upcast(
                    bounded_int::add(bounded_int::mul(limb1_low, POW96_TYPED), value.limb0),
                ),
            },
        )
    }

    pub fn two_u96_into_felt252(limb0: u96, limb1: u96) -> felt252 {
        limb0.into() + limb1.into() * POW96
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

impl U384TryIntoU128 of TryInto<u384, u128> {
    fn try_into(self: u384) -> Option<u128> {
        conversions::try_into_u128(self)
    }
}

impl U384TryIntoU256 of TryInto<u384, u256> {
    fn try_into(self: u384) -> Option<u256> {
        conversions::try_into_u256(self)
    }
}

impl U384Serde of Serde<u384> {
    fn serialize(self: @u384, ref output: Array<felt252>) {
        output.append(conversions::two_u96_into_felt252(*self.limb0, *self.limb1));
        output.append(conversions::two_u96_into_felt252(*self.limb2, *self.limb3));
    }

    fn deserialize(ref serialized: Span<felt252>) -> Option<u384> {
        let [l01, l23] = (*serialized.multi_pop_front::<2>()?).unbox();
        let (limb0, limb1) = conversions::felt252_try_into_two_u96(l01)?;
        let (limb2, limb3) = conversions::felt252_try_into_two_u96(l23)?;

        return Some(u384 { limb0, limb1, limb2, limb3 });
    }
}

impl U384Zero of crate::num::traits::Zero<u384> {
    fn zero() -> u384 {
        u384 { limb0: 0, limb1: 0, limb2: 0, limb3: 0 }
    }

    fn is_zero(self: @u384) -> bool {
        *self == Self::zero()
    }

    fn is_non_zero(self: @u384) -> bool {
        !self.is_zero()
    }
}

impl U384One of crate::num::traits::One<u384> {
    fn one() -> u384 {
        u384 { limb0: 1, limb1: 0, limb2: 0, limb3: 0 }
    }

    fn is_one(self: @u384) -> bool {
        *self == Self::one()
    }

    fn is_non_one(self: @u384) -> bool {
        !self.is_one()
    }
}
