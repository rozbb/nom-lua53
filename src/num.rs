use std::ffi;
use std::ptr;
use std::str::{self, FromStr};
use nom::{digit, hex_digit};
use libc;

// Define the underlying float type
#[cfg(feature = "single_float")]
pub type LuaFloat = f32;
#[cfg(not(feature = "single_float"))]
pub type LuaFloat = f64;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Numeral {
    Float(LuaFloat),
    Int(isize)
}

impl Numeral {
    fn from_full_dec(digits: &[u8]) -> Numeral {
        let s = str::from_utf8(digits).unwrap();
        if let Ok(n) = isize::from_str_radix(s, 10) {
            Numeral::Int(n)
        }
        else {
            let f = LuaFloat::from_str(s).unwrap();
            Numeral::Float(f)
        }
    }

    fn from_full_hex(digits: &[u8]) -> Numeral {
        let s = str::from_utf8(digits).unwrap();
        // Slice off the '0x' from the string and try parsing as a normal integer
        if let Ok(n) = isize::from_str_radix(&s[2..], 16) {
            Numeral::Int(n)
        }
        // Otherwise, parse as a float
        else {
            let cstr = ffi::CString::new(digits).unwrap();
            let f = unsafe { libc::strtod(cstr.into_raw() as *const _, ptr::null_mut()) };
            Numeral::Float(f as LuaFloat)
        }
    }
}

named!(pub decimal<usize>,
   map_res!(
       map_res!(
           call!(digit),
           str::from_utf8),
       |s| usize::from_str_radix(s, 10)
   )
);

named!(pub hex<usize>,
   map_res!(
       map_res!(
           call!(hex_digit),
           str::from_utf8),
       |s| usize::from_str_radix(s, 16)
   )
);

named!(float_sgn_suffix<i32>,
   map!(
       do_parse!(
           sign: opt!(alt!(tag!("+") | tag!("-"))) >>
           expt: decimal >>
           (sign, expt)
       ),
       |(sign, expt): (Option<&[u8]>, usize)| {
           match sign {
               Some(b"+") | None => expt as i32,
               Some(b"-") => -(expt as i32),
               _ => unreachable!(),
           }
       }
    )
);

named!(float_mag<i32>, preceded!(alt!(tag!("e") | tag!("E")), float_sgn_suffix));
named!(float_pow<i32>, preceded!(alt!(tag!("p") | tag!("P")), float_sgn_suffix));

named!(hex_lit<Numeral>,
    map!(
        recognize!(
            tuple!(
               opt!(tag!("-")),
               alt!(tag!("0x") | tag!("0X")),
               hex_digit,
               opt!(complete!(preceded!(tag!("."), hex_digit))),
               opt!(complete!(float_pow))
            )
        ),
        Numeral::from_full_hex
    )
);

named!(dec_lit<Numeral>,
    map!(
        recognize!(
            tuple!(
               opt!(tag!("-")),
               digit,
               opt!(complete!(preceded!(tag!("."), digit))),
               opt!(complete!(float_mag))
            )
        ),
        Numeral::from_full_dec
    )
);

named!(pub num_lit<Numeral>, eat_lua_sep!(alt!(hex_lit | dec_lit)));
