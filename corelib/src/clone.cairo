use array::ArrayTrait;
use array::SpanTrait;

trait CloneTrait<T> {
    fn clone(self: @T) -> T;

}

impl FeltCloneImpl of CloneTrait::<felt> {
    #[inline(always)]
    fn clone(self: @felt) -> felt{
        *self
    }
}

impl ArrayFeltCloneImpl of CloneTrait::<Array<felt>> {
    #[inline(always)]
    fn clone(self: @Array<felt>) -> Array<felt>{
        let mut response = array_new();
        clone_loop(self.span(), ref response);
        response
    }
}

fn clone_loop(mut at: Span<felt>, ref response: Array<felt>) {
    match try_fetch_gas() {
        Option::Some(_) => {},
        Option::None(_) => {
            let mut data = array_new();
            array_append(ref data, 'OOG');
            panic(data);
        },
    }
    match at.pop_front() {
        Option::Some(v) => {
            response.append(*v);
            clone_loop( at, ref response);
        },
        Option::None(_) => (),
    }
}
