// TODO: Wayy more docs

#[macro_use]
extern crate nom;
#[macro_use]
extern crate lazy_static;
extern crate libc;

#[macro_use]
mod utils;

mod comment;
mod name;
mod num;
mod op;
mod stat_expr;
mod stat_expr_types;
mod string;
mod trans;

pub use stat_expr_types::*;
pub use num::LuaFloat;

#[cfg(test)]
mod test;

use stat_expr::block;
use stat_expr_types::Block;
use utils::lua_sep;
use nom::IResult;

pub enum ParseResult<'a> {
    Done(Block<'a>),
    Error(&'a [u8], Block<'a>),
}

named!(parse_all_<Block>, terminated!(block, lua_sep));

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
