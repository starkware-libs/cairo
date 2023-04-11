#[attr(a = 'A', b = 'B')]
mod foo {}

#[derive(Clone, Copy)]
struct S {}

#[inline]
#[post('/', data = '<todo_form>')]
fn string_value() {}
