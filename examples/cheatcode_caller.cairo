const CONSTANT: felt = 1;

fn test_cheatcode_caller() {
   roll(CONSTANT, 2)
}

fn test_cheatcode_caller_twice() {
   roll(1, 2);
   roll(1, 2)
}

fn test_cheatcode_caller_three() {
   roll(1, 2);
   roll(1, 2);
   roll(1, 2)
}

fn test_warp() {
   match warp(CONSTANT, Option::<felt>::Some(2)) {
      Result::Ok(class_hash) => (),
      Result::Err(x) => {
         let mut data = array_new::<felt>();
         array_append::<felt>(ref data, x);
         panic(data)
      },
   }
}

fn test_warp_with_none() {
   match warp(1, Option::<felt>::None(())) {
      Result::Ok(class_hash) => (),
      Result::Err(x) => {
         let mut data = array_new::<felt>();
         array_append::<felt>(ref data, x);
         panic(data)
      },
   }
}

fn test_declare() {
   match declare('test') {
      Result::Ok(class_hash) => (),
      Result::Err(x) => {
         let mut data = array_new::<felt>();
         array_append::<felt>(ref data, x);
         panic(data)
      },
   }
}

fn test_start_prank() {
   match start_prank(123, 123) {
      Result::Ok(class_hash) => (),
      Result::Err(x) => {
         let mut data = array_new::<felt>();
         array_append::<felt>(ref data, x);
         panic(data)
      },
   }
}
