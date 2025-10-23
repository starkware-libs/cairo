#[feature("byte-span")]
use crate::byte_array::{ByteSpan, ByteSpanTrait, ToByteSpanTrait};
use crate::num::traits::Bounded;
use crate::test::test_utils::{assert_eq, assert_ne};

#[test]
fn test_append_byte() {
    let mut ba = Default::default();
    for c in '0'_u8..='Z'_u8 {
        ba.append_byte(c);
    }
    assert_eq!(ba, "0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ");
}

#[test]
fn test_append_word() {
    let mut ba = Default::default();

    ba.append_word('ABCDEFGHIJKLMNOPQRSTUVWXYZabcd', 30);
    assert_eq!(ba, "ABCDEFGHIJKLMNOPQRSTUVWXYZabcd", "appending word in single bytes31");

    ba.append_word('efg', 3);
    assert_eq!(ba, "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefg", "append word overflowing pending word");

    ba.append_word('hi', 2);
    assert_eq!(ba, "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghi", "append word extending new pending word");

    // Length is 0, so nothing is actually appended.
    ba.append_word('jk', 0);
    assert_eq!(ba, "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghi", "append 0 length error");

    ba.append_word('ABCDEFGHIJKLMNOPQRSTUVWXYZa', 27);
    assert_eq!(
        ba,
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghiABCDEFGHIJKLMNOPQRSTUVWXYZa",
        "append word filling pending to capacity",
    );

    ba.append_word('b', 1);
    assert_eq!(
        ba,
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghiABCDEFGHIJKLMNOPQRSTUVWXYZab",
        "append word starting new pending word",
    );
}

#[test]
fn test_append() {
    let mut ba_32: ByteArray = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcde$";

    ba_32.append(@ba_32);

    assert_eq!(
        ba_32,
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcde$ABCDEFGHIJKLMNOPQRSTUVWXYZabcde$",
        "append bytearray across new pending word",
    );
}

// Same as test_append, but with `+=` instead of `append`.
#[test]
fn test_add_eq() {
    let mut ba_32: ByteArray = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcde$";

    ba_32 += ba_32.clone();

    assert_eq!(
        ba_32,
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcde$ABCDEFGHIJKLMNOPQRSTUVWXYZabcde$",
        "add-eq bytearray across new pending word",
    );
}

// Same as test_append and test add_eq, but with `concat`.
#[test]
fn test_concat() {
    let mut ba_32: ByteArray = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcde$";

    let ba = ByteArrayTrait::concat(@ba_32, @ba_32);

    assert_eq!(
        ba,
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcde$ABCDEFGHIJKLMNOPQRSTUVWXYZabcde$",
        "add-eq bytearray across new pending word",
    );
}

// Same as test_concat, but with `+` instead of `concat`.
#[test]
fn test_add() {
    let mut ba_32: ByteArray = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcde$";

    let ba_32 = ba_32.clone() + ba_32;

    assert_eq!(
        ba_32,
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcde$ABCDEFGHIJKLMNOPQRSTUVWXYZabcde$",
        "add-eq bytearray across new pending word",
    );
}

// Test concat/append, first byte array empty.
#[test]
fn test_concat_first_empty() {
    let ba_32: ByteArray = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdef";
    let ba_concat = ByteArrayTrait::concat(@Default::default(), @ba_32);
    assert_eq!(ba_concat, ba_32, "Error concat empty ba");
}

// Test concat/append, second byte array empty.
#[test]
fn test_concat_second_empty() {
    let ba_32: ByteArray = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdef";
    let ba_concat = ByteArrayTrait::concat(@ba_32, @Default::default());
    assert_eq!(ba_concat, ba_32, "Error concat empty ba");
}

// Test concat/append, first byte array pending word is empty.
#[test]
fn test_concat_first_pending_0() {
    let ba_31: ByteArray = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcde";
    let ba_32: ByteArray = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcde$";

    let ba_concat = ByteArrayTrait::concat(@ba_31, @ba_32);

    assert_eq!(
        ba_concat,
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdeABCDEFGHIJKLMNOPQRSTUVWXYZabcde$",
        "Error concat with overflow into pending word",
    );
}

// Test concat/append, second byte array pending word is empty.
#[test]
fn test_concat_second_pending_0() {
    let ba_32: ByteArray = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcde$";
    let ba_31: ByteArray = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcde";

    let ba_concat = ByteArrayTrait::concat(@ba_32, @ba_31);

    assert_eq!(
        ba_concat,
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcde$ABCDEFGHIJKLMNOPQRSTUVWXYZabcde",
        "Error concat with overflow into pending word",
    );
}

// Test concat/append, split index of the words of the second byte array is 16.
#[test]
fn test_concat_split_index_16() {
    let ba_16: ByteArray = "ABCDEFGHIJKLMNO$";
    let ba_32: ByteArray = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdef";

    let ba_concat = ByteArrayTrait::concat(@ba_16, @ba_32);

    assert_eq!(
        ba_concat,
        "ABCDEFGHIJKLMNO$ABCDEFGHIJKLMNOPQRSTUVWXYZabcdef",
        "Error concat with split index 16",
    );
}

// Test concat/append, split index of the words of the second byte array is < 16, specifically 1.
#[test]
fn test_concat_split_index_lt_16() {
    let ba_1: ByteArray = "$";
    let ba_32: ByteArray = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdef";

    let ba_concat = ByteArrayTrait::concat(@ba_1, @ba_32);

    assert_eq!(
        ba_concat, "$ABCDEFGHIJKLMNOPQRSTUVWXYZabcdef", "Error concat with split index < 16",
    );
}

// Test concat/append, split index of the words of the second byte array is > 16, specifically 30.
#[test]
fn test_concat_split_index_gt_16() {
    let ba_30: ByteArray = "ABCDEFGHIJKLMNOPQRSTUVWXYZabc$";
    let ba_33: ByteArray = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefg";

    let ba_concat = ByteArrayTrait::concat(@ba_30, @ba_33);

    assert_eq!(
        ba_concat,
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabc$ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefg",
        "Error concat with split index > 16",
    );
}

// Sum of the lengths of the pending words of both byte arrays is 31 (a full word).
#[test]
fn test_concat_pending_sum_up_to_full() {
    let ba_32: ByteArray = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcde$";
    let ba_30: ByteArray = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcd";

    let ba_concat = ByteArrayTrait::concat(@ba_32, @ba_30);

    assert_eq!(
        ba_concat,
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcde$ABCDEFGHIJKLMNOPQRSTUVWXYZabcd",
        "Error concat with pending word sum up to full",
    );
}

// Sum of the lengths of the pending words of both byte arrays is 31+16.
// That is, the pending words aggregate to a full word, and the last split index is 16.
#[test]
fn test_concat_pending_sum_up_to_more_than_word_16() {
    let ba_17: ByteArray = "ABCDEFGHIJKLMNOP$";
    let ba_30: ByteArray = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcd";

    let ba_concat = ByteArrayTrait::concat(@ba_17, @ba_30);

    assert_eq!(
        ba_concat,
        "ABCDEFGHIJKLMNOP$ABCDEFGHIJKLMNOPQRSTUVWXYZabcd",
        "Error pending word overflowed concat with split index 16",
    );
}

// Sum of the lengths of the pending words of both byte arrays is in [32, 31+15].
// That is, the pending words aggregate to a full word, and the last split index is <16.
#[test]
fn test_concat_pending_sum_up_to_more_than_word_lt16() {
    let ba_2: ByteArray = "A$";
    let ba_30: ByteArray = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcd";

    let ba_concat = ByteArrayTrait::concat(@ba_2, @ba_30);

    assert_eq!(
        ba_concat,
        "A$ABCDEFGHIJKLMNOPQRSTUVWXYZabcd",
        "Error pending word overflowed concat with split index < 16",
    );
}

// Sum of the lengths of the pending words of both byte arrays is >31+15
// That is, the pending words aggregate to a full word, and the last split index is >16.
#[test]
fn test_concat_pending_sum_up_to_more_than_word_gt16() {
    let ba_30: ByteArray = "ABCDEFGHIJKLMNOPQRSTUVWXYZabc$";

    let ba_concat = ByteArrayTrait::concat(@ba_30, @ba_30);

    assert_eq!(
        ba_concat,
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabc$ABCDEFGHIJKLMNOPQRSTUVWXYZabc$",
        "Error pending word overflowed concat with split index > 16",
    );
}

#[test]
fn test_len() {
    let ba: ByteArray = Default::default();
    assert_eq!(ba.len(), 0, "wrong ByteArray len");

    let mut ba_33: ByteArray = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdef$";
    let mut ba = ba_33;
    assert_eq!(ba.len(), 33, "wrong ByteArray len");

    let ba_30: ByteArray = "ABCDEFGHIJKLMNOPQRSTUVWXYZabc$";
    ba.append(@ba_30);
    assert_eq!(ba.len(), 63, "wrong ByteArray len");
}

#[test]
fn test_at_empty() {
    let ba: ByteArray = Default::default();

    assert_eq!(ba.at(0), None, "index 0 is not out of bounds");
    assert_eq!(ba.at(1), None, "index 1 is not out of bounds");
    assert_eq!(ba.at(30), None, "index 30 is not out of bounds");
    assert_eq!(ba.at(31), None, "index 31 is not out of bounds");
}

#[test]
fn test_at() {
    let ba_31: ByteArray = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcde";
    let mut ba = ba_31.clone();
    ba.append(@ba_31);

    let ba_17: ByteArray = "ABCDEFGHIJKLMNOPQ";
    ba.append(@ba_17);

    assert_eq!(ba.at(0), Some('A'), "wrong byte at index 0");
    assert_eq!(ba.at(1), Some('B'), "wrong byte at index 1");
    assert_eq!(ba.at(2), Some('C'), "wrong byte at index 2");
    assert_eq!(ba.at(14), Some('O'), "wrong byte at index 14");
    assert_eq!(ba.at(15), Some('P'), "wrong byte at index 15");
    assert_eq!(ba.at(16), Some('Q'), "wrong byte at index 16");
    assert_eq!(ba.at(17), Some('R'), "wrong byte at index 17");
    assert_eq!(ba.at(29), Some('d'), "wrong byte at index 29");
    assert_eq!(ba.at(30), Some('e'), "wrong byte at index 30");
    assert_eq!(ba.at(31), Some('A'), "wrong byte at index 31");
    assert_eq!(ba.at(32), Some('B'), "wrong byte at index 32");
    assert_eq!(ba.at(61), Some('e'), "wrong byte at index 61");
    assert_eq!(ba.at(62), Some('A'), "wrong byte at index 62");
    assert_eq!(ba.at(63), Some('B'), "wrong byte at index 63");
    assert_eq!(ba.at(76), Some('O'), "wrong byte at index 76");
    assert_eq!(ba.at(77), Some('P'), "wrong byte at index 77");
    assert_eq!(ba.at(78), Some('Q'), "wrong byte at index 78");
    assert_eq!(ba.at(79), None, "index 79 is not out of bounds");
}

// Same as the previous test, but with [] instead of .at() (and without the out-of-bounds case).
#[test]
fn test_index_view() {
    let ba_31: ByteArray = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcde";
    let mut ba = ba_31.clone();
    ba.append(@ba_31);

    let ba_17: ByteArray = "ABCDEFGHIJKLMNOPQ";
    ba.append(@ba_17);

    assert_eq!(ba[0], 'A', "wrong byte at index 0");
    assert_eq!(ba[1], 'B', "wrong byte at index 1");
    assert_eq!(ba[2], 'C', "wrong byte at index 2");
    assert_eq!(ba[14], 'O', "wrong byte at index 14");
    assert_eq!(ba[15], 'P', "wrong byte at index 15");
    assert_eq!(ba[16], 'Q', "wrong byte at index 16");
    assert_eq!(ba[17], 'R', "wrong byte at index 17");
    assert_eq!(ba[29], 'd', "wrong byte at index 29");
    assert_eq!(ba[30], 'e', "wrong byte at index 30");
    assert_eq!(ba[31], 'A', "wrong byte at index 31");
    assert_eq!(ba[32], 'B', "wrong byte at index 32");
    assert_eq!(ba[61], 'e', "wrong byte at index 61");
    assert_eq!(ba[62], 'A', "wrong byte at index 62");
    assert_eq!(ba[63], 'B', "wrong byte at index 63");
    assert_eq!(ba[76], 'O', "wrong byte at index 76");
    assert_eq!(ba[77], 'P', "wrong byte at index 77");
    assert_eq!(ba[78], 'Q', "wrong byte at index 78");
}

// Test panic with [] in case of out-of-bounds
#[test]
#[should_panic(expected: ('Index out of bounds',))]
fn test_index_view_out_of_bounds() {
    let ba_31: ByteArray = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcde";
    let mut ba = ba_31.clone();
    ba.append(@ba_31);
    let ba_17: ByteArray = "ABCDEFGHIJKLMNOPQ";
    ba.append(@ba_17);

    let _x = ba[79];
}

#[test]
fn test_string_literals() {
    let _ba: ByteArray = "12345"; // len < 16
    let _ba: ByteArray = "1234567890123456"; // len == 16
    let _ba: ByteArray = "123456789012345678"; // 16 < len < 31
    let _ba: ByteArray = "1234567890123456789012345678901"; // len == 31
    let _ba: ByteArray = "123456789012345678901234567890123"; // 31 < len < 47
    let _ba: ByteArray = "12345678901234567890123456789012345678901234567"; // len == 47
    let _ba: ByteArray = "123456789012345678901234567890123456789012345678"; // len > 47
}

#[test]
fn test_equality() {
    let byte_array: ByteArray = "a";
    assert_eq!(byte_array, "a", "Same strings are not equal");
    assert_ne!(byte_array, "b", "Different strings are equal");

    let mut ba1: ByteArray = "01";
    let ba_31: ByteArray = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcde";
    ba1.append(@ba_31);
    let ba_33: ByteArray = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefg";
    let ba2 = ba_33;
    let ba_32: ByteArray = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdef";
    let ba3 = ba_32.clone();
    let mut ba4 = ba_32;
    ba4.append(@"a");

    assert_eq!(ba1, ba1, "Same ByteArrays are not equal");
    assert_eq!(ba2, ba2, "Same ByteArrays are not equal");
    assert_eq!(ba3, ba3, "Same ByteArrays are not equal");
    assert_eq!(ba4, ba4, "Same ByteArrays are not equal");

    // Different data
    assert_ne!(ba1, ba2, "Different ByteArrays are equal");

    // Different pending word length
    assert_ne!(ba2, ba3, "Different ByteArrays are equal");

    // Different pending word
    assert_ne!(ba2, ba4, "Different ByteArrays are equal");
}

#[test]
fn test_reverse() {
    // Arrays of length < 16
    let ba: ByteArray = "abc";
    let ba_rev: ByteArray = "cba";
    let palindrome: ByteArray = "rotator";
    assert_ne!(ba, ba.rev(), "ba == ba.rev()");
    assert_ne!(ba_rev, ba_rev.rev(), "ba_rev == ba_rev.rev()");
    assert_eq!(ba, ba_rev.rev(), "ba != ba_rev.rev()");
    assert_eq!(palindrome, palindrome.rev(), "palindrome is not a palindrome");

    // Arrays of length 16
    let ba: ByteArray = "my length is 16.";
    let ba_rev: ByteArray = ".61 si htgnel ym";
    let palindrome: ByteArray = "nolemon  nomelon";
    assert_ne!(ba, ba.rev(), "ba == ba.rev()");
    assert_ne!(ba_rev, ba_rev.rev(), "ba_rev == ba_rev.rev()");
    assert_eq!(ba, ba_rev.rev(), "ba != ba_rev.rev()");
    assert_eq!(palindrome, palindrome.rev(), "palindrome is not a palindrome");

    // Arrays of 16 < length < 31
    let ba: ByteArray = "I am a medium byte array";
    let ba_rev: ByteArray = "yarra etyb muidem a ma I";
    let palindrome: ByteArray = "nolemon  nomelon";
    assert_ne!(ba, ba.rev(), "ba == ba.rev()");
    assert_ne!(ba_rev, ba_rev.rev(), "ba_rev == ba_rev.rev()");
    assert_eq!(ba, ba_rev.rev(), "ba != ba_rev.rev()");
    assert_eq!(palindrome, palindrome.rev(), "palindrome is not a palindrome");

    // Arrays of length 31
    let ba: ByteArray = "I didn't find a good palindrome";
    let ba_rev: ByteArray = "emordnilap doog a dnif t'ndid I";
    let palindrome: ByteArray = "kayak level rotator level kayak";
    assert_ne!(ba, ba.rev(), "ba == ba.rev()");
    assert_ne!(ba_rev, ba_rev.rev(), "ba_rev == ba_rev.rev()");
    assert_eq!(ba, ba_rev.rev(), "ba != ba_rev.rev()");
    assert_eq!(palindrome, palindrome.rev(), "palindrome is not a palindrome");

    // Arrays of 31 < length < 47 (31+16)
    let ba: ByteArray = "This time I did find a good palindrome!";
    let ba_rev: ByteArray = "!emordnilap doog a dnif did I emit sihT";
    let palindrome: ByteArray = "noitneverpropagatesifisetagaporprevention";
    assert_ne!(ba, ba.rev(), "ba == ba.rev()");
    assert_ne!(ba_rev, ba_rev.rev(), "ba_rev == ba_rev.rev()");
    assert_eq!(ba, ba_rev.rev(), "ba != ba_rev.rev()");
    assert_eq!(palindrome, palindrome.rev(), "palindrome is not a palindrome");

    // Arrays of length 47 (31+16)
    let ba: ByteArray = "I have found a palindrome, exactly 47 in length";
    let ba_rev: ByteArray = "htgnel ni 74 yltcaxe ,emordnilap a dnuof evah I";
    let palindrome: ByteArray = "onacloverifaliveeruptsavastpureevilafirevolcano";
    assert_ne!(ba, ba.rev(), "ba == ba.rev()");
    assert_ne!(ba_rev, ba_rev.rev(), "ba_rev == ba_rev.rev()");
    assert_eq!(ba, ba_rev.rev(), "ba != ba_rev.rev()");
    assert_eq!(palindrome, palindrome.rev(), "palindrome is not a palindrome");

    // Arrays of length > 47 (31+16)
    let ba: ByteArray = "This palindrome is not as good, but at least it's long enough";
    let ba_rev: ByteArray = "hguone gnol s'ti tsael ta tub ,doog sa ton si emordnilap sihT";
    let palindrome: ByteArray = "docnoteidissentafastneverpreventsafatnessidietoncod";
    assert_ne!(ba, ba.rev(), "ba == ba.rev()");
    assert_ne!(ba_rev, ba_rev.rev(), "ba_rev == ba_rev.rev()");
    assert_eq!(ba, ba_rev.rev(), "ba != ba_rev.rev()");
    assert_eq!(palindrome, palindrome.rev(), "palindrome is not a palindrome");
}

#[test]
fn test_serde() {
    let mut serialized = array![];
    let ba: ByteArray = "";
    ba.serialize(ref serialized);
    assert_eq!(serialized.span(), [0, 0, 0].span());

    let mut serialized = array![];
    let ba: ByteArray = "hello";
    ba.serialize(ref serialized);
    assert_eq!(
        serialized.span(),
        [0, // data len
        0x68656c6c6f, // pending_word
        5 // pending_word_len
        ].span(),
    );

    let mut serialized = array![];
    let ba: ByteArray = "Long string, more than 31 characters.";
    ba.serialize(ref serialized);
    assert_eq!(
        serialized.span(),
        [
            1, // data len
            0x4c6f6e6720737472696e672c206d6f7265207468616e203331206368617261, // data
            0x63746572732e, // pending_word
            6 // pending_word_len
        ]
            .span(),
    );
}

#[test]
fn test_into_iterator() {
    let ba: ByteArray = "hello";
    let mut iter = ba.into_iter();
    assert_eq!(iter.next(), Some('h'));
    assert_eq!(iter.next(), Some('e'));
    assert_eq!(iter.next(), Some('l'));
    assert_eq!(iter.next(), Some('l'));
    assert_eq!(iter.next(), Some('o'));
    assert_eq!(iter.next(), None);
}

#[test]
fn test_from_iterator() {
    assert_eq!(FromIterator::<ByteArray>::from_iter(array!['h'_u8, 'e', 'l', 'l', 'o']), "hello");
}

#[test]
fn test_from_collect() {
    let ba: ByteArray = array!['h', 'e', 'l', 'l', 'o'].into_iter().collect();
    assert_eq!(ba, "hello");
}

#[test]
fn test_span_len() {
    // TODO(giladchase): add short string test here once supported cast into span.
    let ba: ByteArray = "A";
    assert_eq!(ba.span().len(), 1);
    assert!(!ba.span().is_empty());

    let ba_31: ByteArray = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcde";
    assert_eq!(ba_31.span().len(), 31);
    assert!(!ba_31.span().is_empty());

    let empty_ba: ByteArray = "";
    assert_eq!(empty_ba.span().len(), 0);
    assert!(empty_ba.span().is_empty());
    assert_eq!(ba_31.span()[0..0], empty_ba.span());

    // First word in the array, second in last word.
    let two_byte31: ByteArray = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefg";
    let single_span = two_byte31.span()[1..=32];
    assert_eq!(single_span.len(), 32, "len error with start offset");
    assert!(!single_span.is_empty());

    // First word in the array, second in the array, third in remainder.
    let three_bytes31: ByteArray =
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789#$"; // 64 chars.
    let three_span = three_bytes31.span().get(1..64);
    assert_eq!(three_span.map(|s| s.len()), Some(63), "len error with size-3 bytearray");
    assert_eq!(three_span.map(|s| s.is_empty()), Some(false));
}

#[test]
fn test_span_slice_is_empty() {
    let ba: ByteArray = "hello";
    let span = ba.span();
    let is_empty = |span: ByteSpan| span.is_empty();

    let empty = span[2..2];
    assert_eq!(empty.len(), 0);
    assert!(empty.is_empty());
    assert_eq!(empty.to_byte_array(), "");

    let ba_31: ByteArray = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcde";
    assert_eq!(ba_31.span().get(30..30).map(is_empty), Some(true));
    assert_eq!(ba_31.span().get(31..31).map(is_empty), Some(true));
    assert_eq!(ba_31.span().get(15..30).map(is_empty), Some(false));

    let ba_30: ByteArray = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcd";
    assert_eq!(ba_30.span().get(29..29).map(is_empty), Some(true));
    assert_eq!(ba_30.span().get(30..30).map(is_empty), Some(true));
    assert_eq!(ba_30.span().get(15..29).map(is_empty), Some(false));
}

#[test]
fn test_span_copy() {
    let ba: ByteArray = "12";
    let span = ba.span();
    assert_eq!(span.len(), 2);

    let other_span = span;
    assert_eq!(other_span.len(), 2);
    assert_eq!(ba, span.to_byte_array());
    assert_eq!(span, other_span);

    let span_again = span.span();
    assert_eq!(ba, span_again.to_byte_array());
    assert_eq!(span, span_again);

    let even_more_span_again = other_span.span();
    assert_eq!(ba, even_more_span_again.to_byte_array());
    assert_eq!(other_span, even_more_span_again);
    assert_eq!(span, other_span);
}

#[test]
fn test_span_slice_empty() {
    let ba: ByteArray = "hello";
    let span = ba.span();

    let empty = span[2..2];
    assert_eq!(empty.len(), 0);
    assert!(empty.is_empty());
    assert_eq!(empty.to_byte_array(), "");
}

#[test]
fn test_span_slice_out_of_bounds() {
    let ba: ByteArray = "hello";
    let span = ba.span();

    assert_eq!(span.get(3..=7), None, "end out of bounds");
    assert_eq!(span.get(6..=6), None, "start out of bounds (inclusive)");

    const MAX_INDEX: usize = Bounded::MAX;
    assert_eq!(
        span.get(2..4).unwrap().get((MAX_INDEX - 1)..MAX_INDEX), None, "start offset overflow",
    );
    assert_eq!(span.get(2..=3).unwrap().get((MAX_INDEX - 1)..MAX_INDEX), None);
    assert_eq!(span.get(2..4).unwrap().get((MAX_INDEX - 1)..=MAX_INDEX), None);
    assert_eq!(span.get(2..=3).unwrap().get((MAX_INDEX - 1)..=MAX_INDEX), None);
    assert_eq!(span.get(MAX_INDEX..0), None, "backwards range");

    let empty_string: ByteArray = "";
    assert_eq!(empty_string.span().get(0..2), None, "empty slice is sliceable");
}

#[test]
fn test_span_slice_under_31_bytes() {
    // Word entirely in remainder word.
    let ba: ByteArray = "abcde";
    let span = ba.span();
    let tba = |ba: ByteSpan| ba.to_byte_array();

    assert_eq!(span.get(0..=2).map(tba), Some("abc"));
    assert_eq!(span.get(2..4).map(tba), Some("cd"));
    assert_eq!(span.get(4..=4).map(tba), Some("e"));
}
#[test]
fn test_span_slice_exactly_31_bytes() {
    // 1 full data word, empty last_word.
    let ba_31: ByteArray = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcde";
    let span = ba_31.span();

    assert_eq!(span.len(), 31);
    assert_eq!(span.get(0..31).unwrap().to_byte_array(), ba_31);
    assert_eq!(span[10..=19].to_byte_array(), "KLMNOPQRST");
}

#[test]
fn test_span_slice_positions() {
    // Two full bytes31 + remainder with 2 bytes.
    let ba_64: ByteArray = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789#$";

    assert_eq!(ba_64.span()[10..=39].to_byte_array(), "KLMNOPQRSTUVWXYZabcdefghijklmn");
    assert_eq!(
        ba_64.span().get(5..64).map(|s| s.to_byte_array()),
        Some("FGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789#$"),
    );
    assert_eq!(ba_64.span().get(29..=48).map(|s| s.to_byte_array()), Some("defghijklmnopqrstuvw"));
}

#[test]
fn test_span_to_bytearray() {
    let empty_ba: ByteArray = "";
    assert_eq!(empty_ba.span().to_byte_array(), empty_ba);

    // Only remainder.
    let small_ba: ByteArray = "hello";
    assert_eq!(small_ba.span().to_byte_array(), small_ba);

    // Data word and remainder.
    let large_ba: ByteArray = "0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVW"; // 40 bytes
    assert_eq!(large_ba.span().to_byte_array(), large_ba);

    // Two data words and remainder.
    let even_larger_ba: ByteArray =
        "abcdeFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789#$"; // 64 bytes
    assert_eq!(even_larger_ba.span().to_byte_array(), even_larger_ba);
}

#[test]
fn test_span_multiple_start_offset_slicing() {
    let ba_6: ByteArray = "abcdef";

    let slice1_inc = ba_6.span().get(1..=5);
    let slice2_inc = slice1_inc.map(|s| s[1..=4]);
    let slice3_inc = slice2_inc.map(|s| s.get(1..4)).flatten();

    let tba = |ba: ByteSpan| ba.to_byte_array();
    assert_eq!(slice1_inc.map(tba), Some("bcdef"));
    assert_eq!(slice2_inc.map(tba), Some("cdef"));
    assert_eq!(slice3_inc.map(tba), Some("def"));
}

#[test]
fn test_span_at_and_index() {
    // Test simple access.
    let ba: ByteArray = "AB";
    let span = ba.span();
    assert_eq!(span[0_usize], 'A');
    assert_eq!(span.get(1_usize), Some('B'));
    assert_eq!(span.get(2_usize), None);

    // Test with offset and two words.
    let ba_33: ByteArray = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefg";
    let mut span = ba_33.span();
    span = span.get(1..33).unwrap();
    assert_eq!(span.get(0_usize), Some('B'));
    assert_eq!(span.get(30_usize), Some('f'));
    assert_eq!(span[31_usize], 'g');
    assert_eq!(span.get(32_usize), None);

    // Test with offset and two words.
    // 64 bytes: 31 + 31 + 2.
    let ba_64: ByteArray = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789#$";
    let mut span = ba_64.span();
    span = span.get(1..64).unwrap();
    assert_eq!(span.get(30_usize), Some('f'));
    assert_eq!(span[31_usize], 'g');
    assert_eq!(span.get(60_usize), Some('9'));
    assert_eq!(span[61_usize], '#');
    assert_eq!(span.get(62_usize), Some('$'));
    assert_eq!(span.get(63_usize), None);

    // Test empty span.
    let empty: ByteArray = Default::default();
    let empty_span = empty.span();
    assert_eq!(empty_span.get(0_usize), None);
}

#[test]
#[should_panic(expected: ('Index out of bounds',))]
fn test_span_index_out_of_bounds() {
    let ba: ByteArray = "AB";
    let span = ba.span();
    let _x = span[2_usize]; // Should panic
}

#[test]
fn test_span_at_overflows() {
    // Test overflow protection with large indices.
    let ba: ByteArray = "test";
    let span = ba.span();
    assert_eq!(span.get(Bounded::<usize>::MAX), None);

    let sliced = ba.span().get(1..3).unwrap();
    assert_eq!(sliced.get(Bounded::<usize>::MAX - 1), None);
    assert_eq!(sliced.get(Bounded::<usize>::MAX), None);
}

#[test]
fn test_byte_span_simple() {
    let empty: ByteArray = "";
    assert_eq!(empty.span().into_iter().collect(), array![]);

    let ba: ByteArray = "A";
    assert_eq!(ba.span().into_iter().collect(), array!['A']);

    let ba: ByteArray = "AB";
    assert_eq!(ba.span().into_iter().collect(), array!['A', 'B']);
}

#[test]
fn test_byte_span_iterator_word_boundaries() {
    // Test 30, 31, 32 bytes (1 word boundary).
    let ba_30: ByteArray = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcd";
    let mut iter = ba_30.span().into_iter();
    iter.advance_by(29).unwrap();
    assert_eq!(iter.collect(), array!['d'], "30 bytes - last byte");

    let ba_31: ByteArray = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcde";
    let mut iter = ba_31.span().into_iter();
    assert_eq!(iter.next(), Some('A'));
    iter.advance_by(29).unwrap();
    assert_eq!(iter.collect(), array!['e'], "31 bytes - last byte");

    let ba_32: ByteArray = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdef";
    let mut iter = ba_32.span().into_iter();
    iter.advance_by(30).unwrap();
    assert_eq!(iter.collect(), array!['e', 'f'], "32 bytes - last two bytes");

    // Test 62, 63, 64 bytes (2 word boundaries).
    let ba_62: ByteArray = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
    let mut iter = ba_62.span().into_iter();
    iter.advance_by(61).unwrap();
    assert_eq!(iter.collect(), array!['9'], "62 bytes - last byte");

    let ba_63: ByteArray = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789!";
    let mut iter = ba_63.span().into_iter();
    iter.advance_by(62).unwrap();
    assert_eq!(iter.collect(), array!['!'], "63 bytes - last byte");

    let ba_64: ByteArray = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789!@";
    let mut iter = ba_64.span().into_iter();
    iter.advance_by(62).unwrap();
    assert_eq!(iter.collect(), array!['!', '@'], "64 bytes - last two bytes");
}

#[test]
fn test_byte_span_iterator_multiple_words() {
    // Test with 3+ words to verify iteration works across multiple word boundaries.
    // 92 bytes: 31 + 31 + 30.
    let ba_92: ByteArray =
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789!@#$%^&*()_+-=[]{}|;':,.<>?/~`";
    let span = ba_92.span();
    assert_eq!(span.into_iter().count(), 92, "should iterate all 92 bytes");

    // Verify correctness at specific positions.
    let mut iter = span.into_iter();
    assert_eq!(iter.next(), Some('A'));

    // Skip to last byte.
    iter.advance_by(90).unwrap();
    assert_eq!(iter.collect(), array!['`'], "90 bytes - last byte");
}

#[test]
fn test_byte_span_iterator_for_loop_collect() {
    let small_ba: ByteArray = "Hello";
    let span = small_ba.span();

    let mut collected = Default::default();
    let mut count = 0;
    for byte in span {
        collected.append_byte(byte);
        count += 1;
    }
    assert_eq!(collected, small_ba);
    assert_eq!(count, 5);
    assert_eq!(span.into_iter().collect(), small_ba);

    // Test with 2 words.
    let ba_40: ByteArray = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmn";
    collected = Default::default();
    count = 0;
    for byte in ba_40.span() {
        collected.append_byte(byte);
        count += 1;
    }
    assert_eq!(collected, ba_40);
    assert_eq!(count, 40);
    assert_eq!(ba_40.span().into_iter().collect(), ba_40);

    // Test with 3 words.
    let ba_70: ByteArray = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789!@#$%^&*";
    collected = Default::default();
    count = 0;
    for byte in ba_70.span() {
        collected.append_byte(byte);
        count += 1;
    }
    assert_eq!(collected, ba_70);
    assert_eq!(count, 70);
    assert_eq!(ba_70.span().into_iter().collect(), ba_70);
}

#[test]
fn test_byte_span_iterator_slices() {
    // Slice within remainder word (< 31 bytes).
    let ba_13: ByteArray = "Hello Shmello";
    let span = ba_13.span().get(2..7).unwrap();
    assert_eq!(
        span.into_iter().collect(), array!['l', 'l', 'o', ' ', 'S'], "slice within remainder word",
    );

    // Iterate slice across 2 words (1 data + remainder).
    let ba_33: ByteArray = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefg";
    let span = ba_33.span().get(27..32).unwrap();
    assert_eq!(span.into_iter().collect(), array!['b', 'c', 'd', 'e', 'f'], "slice across 2 words");

    // Iterate slice across 3 words.
    let ba_66: ByteArray = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789!@#$";
    let span = ba_66.span().get(29..64).unwrap();
    assert_eq!(
        span.into_iter().collect(),
        array![
            'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't',
            'u', 'v', 'w', 'x', 'y', 'z', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '!',
            '@',
        ],
        "slice across 3 words",
    );

    // Test case where data array is exhausted and remainder has exactly 1 byte.
    let ba_32: ByteArray = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdef";
    let span = ba_32.span().get(31..32).unwrap();
    assert_eq!(span.into_iter().collect(), array!['f'], "1 byte in remainder after data exhausted");
}

#[test]
fn test_span_equality() {
    let empty: ByteArray = "";
    let empty_copy: ByteArray = "";
    let a: ByteArray = "A";
    let a_copy: ByteArray = "A";
    let b: ByteArray = "B";
    let hello_world: ByteArray = "Hello World";
    let hello_world_copy: ByteArray = "Hello World";
    let hello_cairo: ByteArray = "Hello Cairo";
    let ba_31: ByteArray = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcde";
    let ba_31_copy: ByteArray = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcde";
    let ba_64: ByteArray = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789!@";
    let ba_64_copy: ByteArray = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789!@";

    assert_eq!(empty.span(), empty_copy.span());
    assert_eq!(a.span(), a_copy.span());
    assert_eq!(hello_world.span(), hello_world_copy.span());
    assert_eq!(ba_31.span(), ba_31_copy.span());
    assert_eq!(ba_64.span(), ba_64_copy.span());

    assert_ne!(a.span(), b.span());
    assert_ne!(hello_world.span(), hello_cairo.span());
    assert_ne!(empty.span(), a.span());
    assert_ne!(ba_31.span(), ba_64.span());

    // Test slice equality.
    let ba_37: ByteArray = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghij";
    let slice_5_15 = ba_37.span()[5..15];
    let slice_5_15_copy = ba_37.span()[5..15];
    let slice_10_20 = ba_37.span()[10..20];
    assert_eq!(slice_5_15, slice_5_15_copy);
    assert_ne!(slice_5_15, slice_10_20);
}
