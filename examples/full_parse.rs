extern crate nom_lua53;

use std::io::{self, Read};

use nom_lua53::{parse_all, IResult};

fn main() {
    let mut input = Vec::new();
    io::stdin().read_to_end(&mut input).expect("couldn't read from stdin");
    match parse_all(&*input) {
        IResult::Done(rest, ss) => {
            println!("rest == '{}'", String::from_utf8_lossy(rest));
            println!("statements == {:#?}", ss);
        }
        e @ _ => println!("Error: {:?}", e),
    }
}
