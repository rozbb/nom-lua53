use string::*;
use utils::test_utils::EMPTY;

use std::borrow::Cow;
use nom::IResult;

#[test]
fn test_unescape() {
    let inputs = [r"he\u{320}\u{320}llo",
                  r"\thello\z
                    \'world"];
    let outputs = vec!["he̠̠llo", "\thello'world"]
       .into_iter().map(str::as_bytes).map(Cow::from).collect::<Vec<_>>();

    for (input, expected) in inputs.iter().zip(outputs.into_iter()) {
        assert_eq!(unescape(input.as_bytes()), Ok(StringLit(expected)));
    }
}

#[test]
fn test_long_str() {
    let good_inputs = [
        r"[=====[ hello\nworld\'''hi\z    there]=====]",
        r"[=[foo ' bar \t [=[ baz]=]",
    ];
    let good_outputs = vec![
        r" hello\nworld\'''hi\z    there",
        r"foo ' bar \t [=[ baz",
    ].into_iter().map(str::as_bytes).map(Cow::from).collect::<Vec<_>>();

    for (input, expected) in good_inputs.iter().zip(good_outputs.into_iter()) {
        assert_eq!(raw_str(input.as_bytes()), IResult::Done(EMPTY, StringLit(expected)));
    }
}
