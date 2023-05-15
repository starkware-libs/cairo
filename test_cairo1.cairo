use array::ArrayTrait;
use protostar_print::PrintTrait;
use result::ResultTrait;

#[test]
fn test_print_basic() {
  1.print();

  'hello'.print();

  let mut array = ArrayTrait::new();
  array.append('veni');
  array.append('vidi');
  array.append('vici');
  array.print();

  (1 == 2).print();

  true.print();

  assert(1 == 1, 'xxx');
}
