use num::*;
use nom::{self, IResult};

#[test]
fn test_numlit() {
    let good_inputs = ["13", "0xf5", "0x10abcp-7", "0X302498a.e10", "0.00271828e3"];
    let bad_inputs = ["f5", "13f5", "10.3.1.4", "0xp1", "50e"];
    let good_outputs = vec![
        Numeral::Int(13),
        Numeral::Int(0xf5),
        Numeral::Float(LuaFloat(533.46875)),
        Numeral::Float(LuaFloat(50481546.87890625)),
        Numeral::Float(LuaFloat(2.71828)),
    ];
    // We won't be matching the entirety of the input
    let bad_outputs = vec![
        IResult::Error(nom::ErrorKind::Alt),
        IResult::Done(&b"f5"[..], Numeral::Int(13)),
        IResult::Done(&b".1.4"[..], Numeral::Float(LuaFloat(10.3))),
        IResult::Done(&b"xp1"[..], Numeral::Int(0)),
        IResult::Done(&b"e"[..], Numeral::Int(50)),
    ];

    for (input, expected) in good_inputs.iter().zip(good_outputs.into_iter()) {
        assert_eq!(num_lit(input.as_bytes()), IResult::Done(&b""[..], expected));
    }

    for (input, expected) in bad_inputs.iter().zip(bad_outputs.into_iter()) {
        assert_eq!(num_lit(input.as_bytes()), expected);
    }
}
