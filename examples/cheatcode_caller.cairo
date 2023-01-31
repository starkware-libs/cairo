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


