use nom::IResult;
use utils::{open_long_bracket, new_end_long_bracket};

named!(pub comment,
    recognize!(
        tuple!(
            tag!("--"),
            alt!(long_comment | take_until_and_consume!("\n"))
        )
    )
);

// Matches a long comment after the initial '--'
fn long_comment(input: &[u8]) -> IResult<&[u8], &[u8]> {
    match open_long_bracket(input) {
        IResult::Done(input, depth) => {
            let end_bracket = new_end_long_bracket(depth);
            take_until_and_consume!(input, &*end_bracket)
        }
        IResult::Error(a) => IResult::Error(a),
        IResult::Incomplete(a) => IResult::Incomplete(a),
    }
}
