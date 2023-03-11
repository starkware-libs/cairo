use array::ArrayTrait;
use array::SpanTrait;
use gas::get_gas;

trait CloneTrait<T> {
    fn clone(self: @T) -> T;
}

impl FeltCloneImpl of CloneTrait::<felt> {
    fn clone(self: @felt) -> felt {
        *self
    }
}

impl ArrayTCloneImpl<T> of CloneTrait::<Array<T>> {
    fn clone(self: @Array<T>) -> Array<T> {
        let mut response = array_new();
        clone_loop(self.span(), ref response);
        response
    }
}

fn clone_loop<T, impl TClone: CloneTrait::<T>>(mut at: Span<T>, ref response: Array<T>) {
    match get_gas() {
        Option::Some(_) => {},
        Option::None(_) => {
            let mut data = array_new();
            array_append(ref data, 'OOG');
            panic(data);
        },
    }
    match at.pop_front() {
        Option::Some(v) => {
            response.append(v.clone());
            clone_loop(at, ref response);
        },
        Option::None(_) => (),
    }
}
