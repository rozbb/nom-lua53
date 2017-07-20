use std::str::{self, FromStr};
use nom::{digit, hex_digit};

// Define the underlying float type
#[cfg(feature = "single_float")]
#[derive(Clone, Copy, Debug, PartialEq)]
pub type LuaFloatT = f32;
#[cfg(not(feature = "single_float"))]
pub type LuaFloatT = f64;

// Define a newtype over the float type so we can impl helper functions
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct LuaFloat(pub LuaFloatT);

impl LuaFloat {
    // Makes a float as close to the input integer as possible
    fn from_whole_digits(digits: &[u8], radix: Radix) -> LuaFloat {
        if radix == Radix::Hex {
            let mut acc = 0 as LuaFloatT;
            let radix_num = radix.as_u32() as LuaFloatT;
            for &chr in digits.iter().rev() {
                let n = decode_digit(chr, radix);

                // acc = (acc * radix) + n
                acc = acc.mul_add(radix_num, n as LuaFloatT);
            }

            LuaFloat(acc)
        }
        // If it's a decimal float, use the built-in function
        else {
            // This can't fail, since everything is all digits
            let f = LuaFloatT::from_str(str::from_utf8(digits).unwrap()).unwrap();
            LuaFloat(f)
        }
    }

    fn from_frac_digits(digits: &[u8], radix: Radix) -> LuaFloat {
        let radix_inv = (radix.as_u32() as LuaFloatT).powi(-1);
        let mut acc = 0 as LuaFloatT;

        for &chr in digits.iter().rev() {
            let n = decode_digit(chr, radix);

            // acc = (acc * radix^(-1)) + n
            acc = acc.mul_add(radix_inv, n as LuaFloatT);
        }

        LuaFloat(acc * radix_inv)
    }
}

fn decode_digit(c: u8, radix: Radix) -> u8 {
    match c {
        b'0' ... b'9' => c - b'0',
        b'a' ... b'f' => match radix {
                Radix::Dec => panic!("unexpected decimal digit '{}'", c),
                Radix::Hex => c - b'a' + 10,
        },
        _ => panic!("unexpected digit '{}'", c),
    }
}

#[derive(Clone, Copy, Eq, PartialEq)]
enum Radix {
    Dec,
    Hex,
}

impl Radix {
    fn expt_base(self) -> LuaFloatT {
        match self {
            // This is the 'e'/'E' exponent. xey is x * 10^y
            Radix::Dec => 10u8 as LuaFloatT,
            // This is the 'p'/'P' exponent. xpy is x * 2^y
            Radix::Hex => 2u8 as LuaFloatT,
        }
    }

    fn as_u32(self) -> u32 {
        match self {
            Radix::Dec => 10,
            Radix::Hex => 16,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Numeral {
    Float(LuaFloat),
    Int(isize)
}


impl Numeral {
    fn from_whole_digits(digits: &[u8], radix: Radix, sign: isize) -> Numeral {
        let digit_str = str::from_utf8(digits).unwrap();
        match isize::from_str_radix(digit_str, radix.as_u32()) {
            Ok(n) => Numeral::Int(sign * n),
            // This was too big to parse as an isize. Parse it as a float
            Err(_) => {
                let n = LuaFloat::from_whole_digits(digits, radix);
                Numeral::Float(LuaFloat((sign as LuaFloatT) * n.0)) // This is ugly. Deal with it
            }
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

fn parse_num_lit(neg: Option<&[u8]>,
                 whole_digits: &[u8],
                 frac_digits: Option<&[u8]>,
                 suffix: Option<i32>,
                 radix: Radix) -> Numeral {
    // neg can only be None or Some("-")
    let sign = if neg.is_some() { -1isize } else { 1isize };
    // This might be an integer or a float (if it's too big to be a usize)
    let whole_part: Numeral = Numeral::from_whole_digits(whole_digits, radix, sign);
    println!("whole part == {:?}", whole_part);

    // This is a floating point number
    if frac_digits.is_some() || suffix.is_some() {
        let frac_part = LuaFloat::from_frac_digits(frac_digits.unwrap_or(&b""[..]), radix);
        println!("frac part == {:?}", frac_part);

        let mut full_num = match whole_part {
            Numeral::Float(w) => w.0 + frac_part.0,
            Numeral::Int(w) => (w as LuaFloatT) + frac_part.0,
        };
        println!("full_num == {:?}", full_num);

        // This is the exponent suffix ('e'/'E', 'p'/'P')
        if let Some(pow) = suffix {
            full_num *= radix.expt_base().powi(pow);
        }

        Numeral::Float(LuaFloat((sign as LuaFloatT) * full_num))
    }
    // This is a regular integer
    else {
        whole_part
    }
}

named!(hex_lit<Numeral>,
       do_parse!(
           neg: opt!(tag!("-")) >>
           alt!(tag!("0x") | tag!("0X")) >>
           whole_part: hex_digit >>
           frac_part: opt!(complete!(preceded!(tag!("."), hex_digit))) >>
           suffix: opt!(complete!(float_pow)) >>
           (parse_num_lit(neg, whole_part, frac_part, suffix, Radix::Hex))
       )
);

named!(dec_lit<Numeral>,
       do_parse!(
           neg: opt!(tag!("-")) >>
           whole_part: digit >>
           frac_part: opt!(complete!(preceded!(tag!("."), digit))) >>
           suffix: opt!(complete!(float_mag)) >>
           (parse_num_lit(neg, whole_part, frac_part, suffix, Radix::Dec))
       )
);

named!(pub num_lit<Numeral>, eat_lua_sep!(alt!(hex_lit | dec_lit)));
