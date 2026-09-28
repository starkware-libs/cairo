macro add_one {
    ($x:ident) // Matches one identifier.
    => {
        $x + 1
    };
}

macro with_body_comment {
    ($x:expr) => {
        $x
    } // After the body.
    ;
}

struct S {
    #[cairofmt::skip]
    a: u8 // After a skipped member.
    ,
    b: u8,
}
