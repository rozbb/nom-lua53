extern crate nom_lua53;

use std::io::{self, Read};

use nom_lua53::{parse_all, ParseResult};

fn main() {
    let mut input = Vec::new();
    io::stdin().read_to_end(&mut input).expect("couldn't read from stdin");
    match parse_all(&*input) {
        ParseResult::Done(ss) => {
            println!("Done. statements == {:#?}", ss);
        }
        ParseResult::Error(rest, ss) => {
            println!("Error. statements == {:#?}", ss);
            println!("rest == '{}'", String::from_utf8_lossy(rest));
        }
    }
}
