use utils::truncate;

use nom::{digit, hex_digit};
use std::str;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Numeral {
    Float(f64),
    Int(isize)
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

fn parse_hex_lit(neg: Option<&[u8]>,
                     whole_part: &[u8],
                     frac_part: Option<&[u8]>,
                     suffix: Option<i32>) -> Numeral {
    let is_neg = neg.is_some();
    // This is a floating point number
    if frac_part.is_some() || suffix.is_some() {
        let frac_part = frac_part.unwrap_or(&b""[..]);
        // This is the whole number in hex without a decimal place
        let full_num_bytes = &[whole_part, frac_part].concat();
        // Limit to 30 digits to avoid overflow
        let full_num_str = str::from_utf8(truncate(&*full_num_bytes, 30)).unwrap();
        let full_num = usize::from_str_radix(full_num_str, 16).unwrap();

        // This is base 16, so each sig fig is a multiplier of 1/16. Hence, multiply by 4, since
        // we're raising 2 to this power. We raise 2 to this power because the 'p'/'P' suffix
        // modifies the binary exponent
        let mut expt = -4 * (frac_part.len() as i32);

        // This the 'p'/'P' suffix
        if let Some(pow) = suffix {
            expt += pow;
        }

        let f = (full_num as f64) * 2f64.powi(expt);

        if is_neg { Numeral::Float(-f) }
        else { Numeral::Float(f) }
    }
    // This is a regular integer
    else {
        // Stil truncate to 30 digits
        // TODO: Figure out the actual behavior here
        let truncated = str::from_utf8(truncate(whole_part, 30)).unwrap();
        let full_num = isize::from_str_radix(truncated, 16).unwrap();

        if is_neg { Numeral::Int(-full_num) }
        else { Numeral::Int(full_num) }
    }
}

fn parse_dec_lit(neg: Option<&[u8]>,
                     whole_part: &[u8],
                     frac_part: Option<&[u8]>,
                     suffix: Option<i32>) -> Numeral {

    let is_neg = neg.is_some();
    // This is a floating point number
    if frac_part.is_some() || suffix.is_some() {
        let frac_part = frac_part.unwrap_or(&b""[..]);
        // This is the whole number without a decimal place
        let full_num_bytes = &[whole_part, frac_part].concat();
        // Limit to 30 digits to avoid overflow
        let full_num_str = str::from_utf8(truncate(&*full_num_bytes, 30)).unwrap();
        let full_num = usize::from_str_radix(full_num_str, 10).unwrap();

        let mut expt = -(frac_part.len() as i32);

        // This the 'e'/'E' suffix
        if let Some(pow) = suffix {
            expt += pow;
        }

        let f = (full_num as f64) * 10f64.powi(expt);

        if is_neg { Numeral::Float(-f) }
        else { Numeral::Float(f) }
    }
    // This is a regular integer
    else {
        // Stil truncate to 30 digits
        let truncated = str::from_utf8(truncate(whole_part, 30)).unwrap();
        let full_num = isize::from_str_radix(truncated, 10).unwrap();

        if is_neg { Numeral::Int(-full_num) }
        else { Numeral::Int(full_num) }
    }
}

named!(hex_lit<Numeral>,
       do_parse!(
           neg: opt!(tag!("-")) >>
           alt!(tag!("0x") | tag!("0X")) >>
           whole_part: hex_digit >>
           frac_part: opt!(complete!(preceded!(tag!("."), hex_digit))) >>
           suffix: opt!(complete!(float_pow)) >>
           (parse_hex_lit(neg, whole_part, frac_part, suffix))
       )
);

named!(dec_lit<Numeral>,
       do_parse!(
           neg: opt!(tag!("-")) >>
           whole_part: digit >>
           frac_part: opt!(complete!(preceded!(tag!("."), digit))) >>
           suffix: opt!(complete!(float_mag)) >>
           (parse_dec_lit(neg, whole_part, frac_part, suffix))
       )
);

named!(pub num_lit<Numeral>, eat_lua_sep!(alt!(hex_lit | dec_lit)));

#[cfg(test)]
mod test {
    use nom::{self, IResult};
    use super::*;

    #[test]
    fn test_numlit() {
        let good_inputs = ["13", "0xf5", "0x10abcp-7", "0X302498a.e10", "0.00271828e3"];
        let bad_inputs = ["f5", "13f5", "10.3.1.4", "0xp1", "50e"];
        let good_outputs = vec![
            Numeral::Int(13),
            Numeral::Int(0xf5),
            Numeral::Float(533.46875),
            Numeral::Float(50481546.87890625),
            Numeral::Float(2.71828),
        ];
        // We won't be matching the entirety of the input
        let bad_outputs = vec![
            IResult::Error(nom::ErrorKind::Alt),
            IResult::Done(&b"f5"[..], Numeral::Int(13)),
            IResult::Done(&b".1.4"[..], Numeral::Float(10.3)),
            IResult::Done(&b"xp1"[..], Numeral::Int(0)),
            IResult::Done(&b"e"[..], Numeral::Int(50)),
        ];

        for (input, expected) in good_inputs.iter().zip(good_outputs.into_iter()) {
            assert_eq!(num_lit(input.as_bytes()), IResult::Done(&b""[..], expected));
        }

        for (input, expected) in bad_inputs.iter().zip(bad_outputs.into_iter()) {
            assert_eq!(num_lit(input.as_bytes()), expected);
        }
    }
}
