use name::{label, varname, VarName};
use utils::test_utils::EMPTY;

use nom::{self, IResult};

#[test]
fn test_label() {
    assert_eq!(label(b"::   Hello   ::"), IResult::Done(EMPTY, VarName(b"Hello")));
    assert!(label(b"::H ello::").is_err());
}

#[test]
fn test_varname() {
    let good_names = ["Hello", "He_llo", "_H2ello", "nothello"];
    let bad_names = ["4ello", "and", "H&ello", "-World", "F--oo"];
    let bad_output = vec![
        IResult::Error(nom::ErrorKind::Alt),
        IResult::Error(nom::ErrorKind::Custom(0)),
        IResult::Done(&b"&ello"[..], VarName(b"H")),
        IResult::Error(nom::ErrorKind::Alt),
        IResult::Done(&b"--oo"[..], VarName(b"F")),
    ];

    for name in &good_names {
        let name = name.as_bytes();
        assert_eq!(varname(name), IResult::Done(EMPTY, VarName(name)));
    }
    for (name, expected) in bad_names.iter().zip(bad_output.into_iter()) {
        assert_eq!(varname(name.as_bytes()), expected);
    }
}
