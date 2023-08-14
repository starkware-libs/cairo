use array::{ArrayTrait, SpanTrait};
use option::OptionTrait;
/// The call context.
#[derive(Destruct)]
struct CallContext {
    /// The bytecode to execute.
    bytecode: Span<u8>,
    /// The call data.
    call_data: Span<u8>,
    /// Amount of native token to transfer.
    value: u256,
}


trait CallContextTrait {
    fn new(bytecode: Span<u8>, call_data: Span<u8>, value: u256) -> CallContext;
    fn bytecode(self: @CallContext) -> Span<u8>;
    fn call_data(self: @CallContext) -> Span<u8>;
    fn value(self: @CallContext) -> u256;
}

impl CallContextImpl of CallContextTrait {
    fn new(bytecode: Span<u8>, call_data: Span<u8>, value: u256) -> CallContext {
        CallContext { bytecode, call_data, value, }
    }
    fn bytecode(self: @CallContext) -> Span<u8> {
        *self.bytecode
    }

    fn call_data(self: @CallContext) -> Span<u8> {
        *self.call_data
    }

    fn value(self: @CallContext) -> u256 {
        *self.value
    }
}
impl SpanPartialEq<T, impl PartialEqImpl: PartialEq<T>> of PartialEq<Span<T>> {
    fn eq(lhs: @Span<T>, rhs: @Span<T>) -> bool {
        if (*lhs).len() != (*rhs).len() {
            return false;
        }
        let mut lhs_span = *lhs;
        let mut rhs_span = *rhs;
        loop {
            match lhs_span.pop_front() {
                Option::Some(lhs_v) => {
                    if lhs_v != rhs_span.pop_front().unwrap() {
                        break false;
                    }
                },
                Option::None => {
                    break true;
                },
            };
        }
    }
    fn ne(lhs: @Span<T>, rhs: @Span<T>) -> bool {
        !SpanPartialEq::eq(lhs, rhs)
    }
}

#[test]
#[available_gas(1000000)]
fn test_call_context_new() {
    // Given
    let bytecode = array![1, 2, 3];
    let call_data = array![4, 5, 6];
    let value: u256 = 100;
    // When
    let call_context = CallContextTrait::new(bytecode.span(), call_data.span(), value);
    // Then
    assert(SpanPartialEq::eq(@call_context.bytecode(), @bytecode.span()), 'wrong bytecode');
    assert(SpanPartialEq::eq(@call_context.call_data(), @call_data.span()), 'wrong call data');
    assert(call_context.value() == value, 'wrong value');
}
