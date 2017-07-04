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
mod string;

pub use stat_expr::*;
pub use string::*;
pub use nom::IResult;
