use nom::{alpha, is_alphanumeric};

pub fn is_alphanum_or_underscore(c: u8) -> bool {
    is_alphanumeric(c) || c == b'_'
}

pub fn is_eq_sign(c: u8) -> bool {
    c == b'='
}

pub fn is_whitespace(c: u8) -> bool {
    match c {
        b' ' | b'\n' | b'\t' | b'\r' => true,
        _ => false
    }
}

pub fn truncate(s: &[u8], n: usize) -> &[u8] {
    if s.len() <= n {
        s
    }
    else {
        &s[..n]
    }
}

// A tag! that can be preceded by whitespace or a comment
macro_rules! lua_tag {
    ($i:expr, $tag:expr) => (
        eat_lua_sep!($i, tag!($tag))
    );
}

// Eats a preceding lua separator, i.e., whitespace and/or a comment till the end of the line
macro_rules! eat_lua_sep {
    ($i:expr, $submac:ident!( $($args:tt)* )) => ({
        use comment::comment;
        use utils::is_ws;
        complete!(
            $i,
            preceded!(
                recognize_many0!(
                    alt!(take_while1!(is_ws) | comment)
                ),
                $submac!($($args)*)
            )
        )
    });
}

pub fn is_ws(c: u8) -> bool {
    match c {
        b' ' | b'\t' | b'\n' | b'\r' => true,
        _ => false
    }
}


// Matches either one underscore, or >= 1 alphabetic characters
named!(pub alpha_or_underscore, alt!(alpha | tag!("_")));

macro_rules! limit {
    ($i:expr, $submac:ident!( $($args:tt)* ), $n:expr) => (
        $submac!(utils::truncate($i, $n), $($args)*)
    );

    ($i:expr, $f:expr, $n:expr) => (
        limit!($i, call!($f), $n)
    );
}

// Matches something like [=====[ and returns how many '='s there are
named!(pub open_long_bracket<usize>,
       do_parse!(
                tag!("[") >>
           eqs: take_while!(is_eq_sign) >>
                tag!("[") >>
           (eqs.len())
       )
);

pub fn new_end_long_bracket(depth: usize) -> Vec<u8> {
    let mut b = vec![b'='; depth];
    b.insert(0, b']');
    b.push(b']');
    b
}

// Equivalent to recognize!(many0!(...)) but without the unnecessary Vec allocation
macro_rules! recognize_many0 {
  ($i:expr, $submac:ident!( $($args:tt)* )) => ({
      use nom::{self, InputLength, Offset, Slice};

      let ret;
      let mut rest = $i;
      let mut offset = 0;

      loop {
        if rest.input_len() == 0 {
          ret = nom::IResult::Done(rest, ($i).slice(..offset));
          break;
        }

        match $submac!(rest, $($args)*) {
          nom::IResult::Error(_) => {
            ret = nom::IResult::Done(rest, ($i).slice(..offset));
            break;
          },
          nom::IResult::Incomplete(nom::Needed::Unknown) => {
            ret = nom::IResult::Incomplete(nom::Needed::Unknown);
            break;
          },
          nom::IResult::Incomplete(nom::Needed::Size(i)) => {
            let (size,overflowed) = i.overflowing_add(($i).input_len() - rest.input_len());
            ret = match overflowed {
                true  => nom::IResult::Incomplete(nom::Needed::Unknown),
                false => nom::IResult::Incomplete(nom::Needed::Size(size)),
            };
            break;
          },
          nom::IResult::Done(i, o) => {
            // loop trip must always consume (otherwise infinite loops)
            if i == rest {
              ret = nom::IResult::Error(error_position!(nom::ErrorKind::Many0, rest));
              break;
            }

            rest = i;
            offset = (&$i).offset(&i);
          }
        }
      }

      ret
    }
  );
  ($i:expr, $f:expr) => (
    many0!($i, call!($f));
  );
}

#[cfg(test)]
pub mod test_utils {
    pub static EMPTY: &'static [u8] = b"";
}
