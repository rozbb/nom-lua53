#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum BinOp {
    Plus, Minus, Mul, Div, IntDiv, Pow, Mod,
    Concat,
    Lt, Leq, Gt, Geq, Eq, Neq,
    BoolAnd, BoolOr,
    BitAnd, BitXor, BitOr, BitShl, BitShr,
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum UnOp {
    Minus,
    BoolNot,
    Length,
    BitNot,
}

// These are ordered such that strings that if tag X is a prefix of tag Y, then X lies below Y.
// This ensures that something like 'a >= b' doesn't get an intermediate parsing of 'a >' and then
// fail to parse '= b'
named!(pub binop<BinOp>, eat_lua_sep!(
    alt!(
        value!(BinOp::Plus, tag!("+")) |
        value!(BinOp::Minus, tag!("-")) |
        value!(BinOp::Mul, tag!("*")) |
        value!(BinOp::Pow, tag!("^")) |
        value!(BinOp::Mod, tag!("%")) |
        value!(BinOp::IntDiv, tag!("//")) |
        value!(BinOp::Div, tag!("/")) |
        value!(BinOp::Concat, tag!("..")) |
        value!(BinOp::Eq, tag!("==")) |
        value!(BinOp::Neq, tag!("~=")) |
        value!(BinOp::BoolAnd, tag!("and")) |
        value!(BinOp::BoolOr, tag!("or")) |
        value!(BinOp::BitAnd, tag!("&")) |
        value!(BinOp::BitXor, tag!("~")) |
        value!(BinOp::BitOr, tag!("|")) |
        value!(BinOp::Leq, tag!("<=")) |
        value!(BinOp::Geq, tag!(">=")) |
        value!(BinOp::BitShl, tag!("<<")) |
        value!(BinOp::BitShr, tag!(">>")) |
        value!(BinOp::Lt, tag!("<")) |
        value!(BinOp::Gt, tag!(">"))
    )
));

named!(pub unop<UnOp>, eat_lua_sep!(
    alt!(
        value!(UnOp::Minus, tag!("-")) |
        value!(UnOp::BoolNot, tag!("not")) |
        value!(UnOp::Length, tag!("#")) |
        value!(UnOp::BitNot, tag!("~"))
    )
));
