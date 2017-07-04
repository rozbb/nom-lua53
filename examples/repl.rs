extern crate nom_lua53;

use std::io::{self, Read};

use nom_lua53::{statement, IResult};

fn main() {
    let mut input = Vec::new();
    io::stdin().read_to_end(&mut input).expect("couldn't read from stdin");
    match statement(&*input) {
        IResult::Done(rest, s) => {
            println!("rest == '{}'", String::from_utf8_lossy(rest));
            println!("statement == {:#?}", s);
        }
        e @ _ => println!("error: {:?}", e),
    }
}
