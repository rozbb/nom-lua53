#[allow(dead_code)]
#[macro_use]
extern crate nom;

#[macro_use]
mod utils;

mod comment;
mod name;
mod num;
mod op;
mod stat_expr;
mod stat_expr_types;
mod string;

pub use stat_expr_types::*;

#[cfg(test)]
mod test;

use stat_expr::statement;
use stat_expr_types::Statement;
use utils::lua_sep;
use nom::IResult;

pub enum ParseResult<'a> {
    Done(Vec<Statement<'a>>),
    Error(&'a [u8], Vec<Statement<'a>>),
}

named!(parse_all_<Vec<Statement>>,
    delimited!(lua_sep, many0!(statement), lua_sep)
);

pub fn parse_all(input: &[u8]) -> ParseResult {
    match parse_all_(input) {
        IResult::Done(rest, parsed) => {
            if rest.len() != 0 {
                ParseResult::Error(rest, parsed)
            }
            else {
                ParseResult::Done(parsed)
            }
        }
        e @ _ => panic!("Unexpected error {:?}", e),
    }
}
