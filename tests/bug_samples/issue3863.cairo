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

#[test]
fn test_call_context_new() {
    // Given
    let bytecode = array![1, 2, 3];
    let call_data = array![4, 5, 6];
    let value: u256 = 100;
    // When
    let call_context = CallContextTrait::new(bytecode.span(), call_data.span(), value);
    // Then
    assert!(call_context.bytecode() == bytecode.span(), "wrong bytecode");
    assert!(call_context.call_data() == call_data.span(), "wrong call data");
    assert!(call_context.value() == value, "wrong value");
}
