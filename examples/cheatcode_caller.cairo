use result::ResultTrait;


fn test_roll() {
   match roll(1, 2) {
      Result::Ok(_) => (),
      Result::Err(x) => {
         let mut data = array_new::<felt>();
         array_append::<felt>(ref data, x);
         panic(data)
      },
   }
}

fn test_warp() {
   match warp(1, 2) {
      Result::Ok(_) => (),
      Result::Err(x) => {
         let mut data = array_new::<felt>();
         array_append::<felt>(ref data, x);
         panic(data)
      },
   }
}

fn test_declare() {
   match declare('test') {
      Result::Ok(_) => (),
      Result::Err(x) => {
         let mut data = array_new::<felt>();
         array_append::<felt>(ref data, x);
         panic(data)
      },
   }
}

fn test_start_prank() {
   match start_prank(123, 123) {
      Result::Ok(_) => (),
      Result::Err(x) => {
         let mut data = array_new::<felt>();
         array_append::<felt>(ref data, x);
         panic(data)
      },
   }
}

fn test_start_invoke() {
   match invoke(123, 123, 123) {
      Result::Ok(class_hash) => (),
      Result::Err(x) => {
         let mut data = array_new::<felt>();
         array_append::<felt>(ref data, x);
         panic(data)
      },
   }
}
