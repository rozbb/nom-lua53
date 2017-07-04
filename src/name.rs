use std::fmt;

use utils::{alpha_or_underscore, is_alphanum_or_underscore};
use nom::{self, IResult};

#[derive(Copy, Clone, Eq, PartialEq)]
pub struct VarName<'a>(pub &'a [u8]);

impl<'a> fmt::Debug for VarName<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let s = String::from_utf8_lossy(self.0);
        write!(f, "'{}'", s)
    }
}

named!(pub namelist<Vec<VarName>>, separated_nonempty_list_complete!(lua_tag!(","), varname));

named!(pub label<VarName>, ws!(delimited!(tag!("::"), varname, tag!("::"))));

named!(reserved, alt!(
    tag!("and") | tag!("break") | tag!("do") | tag!("else") | tag!("elseif") | tag!("end") |
    tag!("false") | tag!("for") | tag!("function") | tag!("goto") | tag!("if") | tag!("in") |
    tag!("local") | tag!("nil") | tag!("not") | tag!("or") | tag!("repeat")| tag!("return") |
    tag!("then") | tag!("true") | tag!("until") | tag!("while")));

fn varname_(buf: &[u8]) -> IResult<&[u8], VarName> {
    // Match a valid identifier
    let name_res = recognize!(buf, preceded!(
        alpha_or_underscore,
        take_while!(is_alphanum_or_underscore)
    ));

    // Make sure we didn't just match a keyword
    if let IResult::Done(_, name) = name_res {
        if let IResult::Done(rest, _) = reserved(name) {
            // The len==0 part means the whole name is a keyword. Non-example: "andblah".
            if rest.len() == 0 {
                // TODO: make this a real error
                return IResult::Error(error_position!(nom::ErrorKind::Custom(0), buf));
            }
        }
    }

    name_res.map(|n| VarName(n))
}

named!(pub varname<VarName>, eat_lua_sep!(call!(varname_)));

#[cfg(test)]
mod test {
    use super::{label, varname, VarName};
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
}
