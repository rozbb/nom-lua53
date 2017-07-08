use std::fmt;

use utils::{alpha_or_underscore, is_alphanum_or_underscore};
use nom::{self, IResult};

#[derive(Copy, Clone, Eq, PartialEq)]
pub struct VarName<'a>(pub &'a [u8]);

// Print varnames as utf8 strings. After all, they are required to be ASCII
impl<'a> fmt::Debug for VarName<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let s = String::from_utf8_lossy(self.0);
        write!(f, "'{}'", s)
    }
}

named!(pub namelist<Vec<VarName>>, separated_nonempty_list_complete!(lua_tag!(","), varname));

named!(pub label<VarName>, eat_lua_sep!(
    delimited!(lua_tag!("::"), varname, lua_tag!("::"))
));

named!(pub goto<VarName>, eat_lua_sep!(
    preceded!(lua_tag!("goto"), varname)
));

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
