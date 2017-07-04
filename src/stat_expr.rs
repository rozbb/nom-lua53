use name::{self, namelist, varname, VarName};
use num::{self, Numeral};
use op::{binop, unop, BinOp, UnOp};
use string::{self, StringLit};

use nom::{self, IResult};

pub type TableLit<'a> = Vec<Field<'a>>;

#[derive(Clone, Debug, PartialEq)]
pub enum Statement<'a> {
    Semicolon,
    Break,
    Assignment(Assignment<'a>),
    FuncCall(PrefixExp<'a>),
    Label(VarName<'a>),
    Do(Block<'a>),
    While(WhileBlock<'a>),
    Repeat(RepeatBlock<'a>),
    Ite(IfThenElse<'a>),
    ForRange(ForRange<'a>),
    ForIter(ForIter<'a>),
    FuncDef(FunctionDef<'a>),
    LFuncDef(FunctionDef<'a>), // Local function definition
    LVarAssign(LAssignment<'a>), // Local variable assignment
}

// binops.len() == binop_operands.len() - 1. Each binop belongs in between two operands, in order
#[derive(Clone, Debug, PartialEq)]
pub struct Exp<'a> {
    binop_operands: Vec<Exp2<'a>>,
    binops: Vec<BinOp>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Exp2<'a> {
    Nil,
    Ellipses,
    Bool(bool),
    Num(Numeral),
    Str(StringLit<'a>),
    Lambda(FunctionBody<'a>),
    FuncCall(FunctionCall<'a>),
    PrefixExp(Box<PrefixExp<'a>>),
    Table(TableLit<'a>),
    UnExp(UnOp, Box<Exp2<'a>>),
}

// while <cond> do <block>
#[derive(Clone, Debug, PartialEq)]
pub struct WhileBlock<'a> {
    cond: Exp<'a>,
    block: Block<'a>,
}

// repeat <block> until <cond>
#[derive(Clone, Debug, PartialEq)]
pub struct RepeatBlock<'a> {
    cond: Exp<'a>,
    block: Block<'a>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct IfThenElse<'a> {
    cond: Exp<'a>,
    then_blk: Block<'a>,
    elseifs: Vec<(Exp<'a>, Block<'a>)>,
    else_blk: Option<Block<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ForRange<'a> {
    var: VarName<'a>,
    start: Exp<'a>,
    end: Exp<'a>,
    step: Option<Exp<'a>>,
    do_blk: Block<'a>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ForIter<'a> {
    vars: Vec<VarName<'a>>,
    exps: Vec<Exp<'a>>,
    do_blk: Block<'a>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionDef<'a> {
    name: FunctionName<'a>,
    body: FunctionBody<'a>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionName<'a> {
    path: Vec<VarName<'a>>,
    method: Option<VarName<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionBody<'a> {
    params: Params<'a>,
    body: Block<'a>,
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Block<'a> {
    stmts: Vec<Statement<'a>>,
    ret_stmt: Option<Vec<Exp<'a>>>
}

#[derive(Clone, Debug, PartialEq)]
pub struct Params<'a> {
    names: Vec<VarName<'a>>,
    variadic: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Assignment<'a> {
    vars: Vec<PrefixExp<'a>>,
    vals: Vec<Exp<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct LAssignment<'a> {
    vars: Vec<VarName<'a>>,
    vals: Option<Vec<Exp<'a>>>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Args<'a> {
    Str(StringLit<'a>),
    Table(TableLit<'a>),
    ExpList(Vec<Exp<'a>>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct PrefixExp<'a> {
    prefix: ExpOrVarName<'a>,
    // TODO: Fix naming conventions here
    arg_chain: Vec<PrefixedArg<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExpOrVarName<'a> {
    Exp(Exp<'a>),
    VarName(VarName<'a>),
}

// TODO: Fix naming conventions here
#[derive(Clone, Debug, PartialEq)]
pub enum PrefixedArg<'a> {
    TableDot(VarName<'a>),
    TableIdx(Exp<'a>),
    FuncCall(FunctionCall<'a>)
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionCall<'a> {
    method: Option<VarName<'a>>,
    args: Args<'a>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Field<'a> {
    NameAssign(VarName<'a>, Exp<'a>), // Assigning by varname, e.g {foo = 10}
    ExpAssign(Exp<'a>, Exp<'a>), // Assigning by expr, e.g {["foo" .. "bar"] = 10}
    PosAssign(Exp<'a>), // Assigning by position, e.g {"foo", "bar"} assigns in positions 1 and 2
}

// TODO: bad naming
fn exp_from_head_and_binop_chain<'a>(head: Exp2<'a>, binop_chain: Vec<(BinOp, Exp2<'a>)>) -> Exp<'a> {
    // Separate the expressions from the binops
    let (binop_operands, binops) = binop_chain.into_iter().fold((vec![head], Vec::new()),
        |(mut exprs, mut ops), (o, e)| {
            ops.push(o);
            exprs.push(e);
            (exprs, ops)
    });

    Exp {
        binop_operands,
        binops,
    }
}

named!(exp<Exp>, eat_lua_sep!(
    do_parse!(
        head: exp2 >>
        binop_chain: many0!(tuple!(binop, exp2)) >>
        (exp_from_head_and_binop_chain(head, binop_chain))
    ))
);

named!(exp2<Exp2>, eat_lua_sep!(
    alt!(
        value!(Exp2::Nil, lua_tag!("nil")) |
        value!(Exp2::Ellipses, lua_tag!("...")) |
        value!(Exp2::Bool(true), lua_tag!("true")) |
        value!(Exp2::Bool(false), lua_tag!("false")) |
        map!(num::num_lit, Exp2::Num) |
        map!(string::string_lit, Exp2::Str) |
        map!(lambdadef, Exp2::Lambda) |
        map!(prefixexp, |e| Exp2::PrefixExp(Box::new(e))) |
        map!(table_lit, Exp2::Table) |
        map!(tuple!(unop, exp2), |(o, e)| Exp2::UnExp(o, Box::new(e)))
    )
));

named!(pub statement<Statement>, eat_lua_sep!(
    alt!(
        value!(Statement::Semicolon, lua_tag!(";")) |
        value!(Statement::Break, lua_tag!("break")) |
        map!(varassign, Statement::Assignment) |
        map!(name::label, Statement::Label) |
        map!(functioncall, Statement::FuncCall) |
        map!(delimited!(lua_tag!("do"), block, lua_tag!("end")), Statement::Do) |
        map!(whileblock, Statement::While) |
        map!(repeatblock, Statement::Repeat) |
        map!(ifthenelse, Statement::Ite) |
        map!(forrange, Statement::ForRange) |
        map!(foriter, Statement::ForIter) |
        map!(functiondef, Statement::FuncDef) |
        map!(localfunctiondef, Statement::LFuncDef) |
        map!(localvarassign, Statement::LVarAssign)
    )
));

named!(pub parse_all<Vec<Statement>>, many0!(statement));

named!(varassign<Assignment>, eat_lua_sep!(
    do_parse!(
        vars: varlist >>
        lua_tag!("=") >>
        vals: explist >>
        (Assignment { vars, vals })
    )
));

named!(whileblock<WhileBlock>, eat_lua_sep!(
    do_parse!(
        lua_tag!("while") >>
        cond: exp >>
        lua_tag!("do") >>
        block: block >>
        lua_tag!("end") >>
        (WhileBlock { cond, block })
    )
));

named!(repeatblock<RepeatBlock>, eat_lua_sep!(
    do_parse!(
        lua_tag!("repeat") >>
        block: block >>
        lua_tag!("until") >>
        cond: exp >>
        (RepeatBlock { cond, block })
    )
));

named!(ifthenelse<IfThenElse>, eat_lua_sep!(
    do_parse!(
        lua_tag!("if") >>
        cond: exp >>
        lua_tag!("then") >>
        then_blk: block >>
        elseifs: many0!(elseif) >>
        else_blk: opt!(complete!(preceded!(lua_tag!("else"), block))) >>
        lua_tag!("end") >>
        (IfThenElse { cond, then_blk, elseifs, else_blk })
    )
));

named!(elseif<(Exp, Block)>, eat_lua_sep!(
    do_parse!(
        lua_tag!("elseif") >>
        cond: exp >>
        lua_tag!("then") >>
        then_blk: block >>
        (cond, then_blk)
    )
));

named!(forrange<ForRange>, eat_lua_sep!(
    do_parse!(
        lua_tag!("for") >>
        var: varname >>
        lua_tag!("=") >>
        start: exp >>
        lua_tag!(",") >>
        end: exp >>
        step: opt!(complete!(preceded!(lua_tag!(","), exp))) >>
        lua_tag!("do") >>
        do_blk: block >>
        lua_tag!("end") >>
        (ForRange { var, start, end, step, do_blk })
    )
));

named!(foriter<ForIter>, eat_lua_sep!(
    do_parse!(
        lua_tag!("for") >>
        vars: namelist >>
        lua_tag!("in") >>
        exps: explist >>
        lua_tag!("do") >>
        do_blk: block >>
        lua_tag!("end") >>
        (ForIter { vars, exps, do_blk })
    )
));

named!(functiondef<FunctionDef>, eat_lua_sep!(
    do_parse!(
        lua_tag!("function") >>
        name: funcname >>
        body: functionbody >>
        lua_tag!("end") >>
        (FunctionDef { name, body })
    )
));

named!(funcname<FunctionName>, eat_lua_sep!(
       do_parse!(
           path: separated_nonempty_list_complete!(lua_tag!("."), varname) >>
           method: opt!(complete!(preceded!(lua_tag!(":"), varname))) >>
           (FunctionName { path, method })
       )
));

named!(localfunctiondef<FunctionDef>, eat_lua_sep!(
    do_parse!(
        lua_tag!("local") >>
        lua_tag!("function") >>
        name: varname >>
        body: functionbody >>
        lua_tag!("end") >>
        (FunctionDef {
             name: FunctionName { path: vec![name], method: None },
             body: body
        })
    )
));

named!(localvarassign<LAssignment>, eat_lua_sep!(
    do_parse!(
        lua_tag!("local") >>
        vars: namelist >>
        vals: opt!(complete!(preceded!(lua_tag!("="), explist))) >>
        (LAssignment { vars, vals })
    )
));


named!(explist<Vec<Exp>>, eat_lua_sep!(
    separated_nonempty_list_complete!(lua_tag!(","), exp)
));

// Returns an empty vec if there's no explist
named!(opt_explist<Vec<Exp>>,
    map!(opt!(explist), |el| el.unwrap_or(Vec::new()))
);

named!(lambdadef<FunctionBody>, eat_lua_sep!(
    delimited!(lua_tag!("function"), functionbody, lua_tag!("end"))
));

named!(functionbody<FunctionBody>, eat_lua_sep!(
    do_parse!(
       params: delimited!(lua_tag!("("), parlist, lua_tag!(")")) >>
       body: block >>
       (FunctionBody { params, body })
    )
));

named!(block<Block>, eat_lua_sep!(
    do_parse!(
        stmts: many0!(statement) >>
        ret_stmt: opt!(complete!(ret_statement)) >>
        (Block { stmts, ret_stmt })
    )
));

named!(ret_statement<Vec<Exp>>, eat_lua_sep!(
    delimited!(lua_tag!("return"), opt_explist, opt!(lua_tag!(";")))
));

named!(prefixexp<PrefixExp>, eat_lua_sep!(
    do_parse!(
        prefix: prefixexp2 >>
        arg_chain: many0!(prefixexp3) >>
        (PrefixExp { prefix, arg_chain })
    )
));

named!(prefixexp2<ExpOrVarName>, eat_lua_sep!(
    alt!(
        map!(delimited!(lua_tag!("("), exp, lua_tag!(")")), ExpOrVarName::Exp) |
        map!(varname, ExpOrVarName::VarName)
    )
));

named!(prefixexp3<PrefixedArg>, eat_lua_sep!(
    alt!(
        map!(preceded!(lua_tag!("."), varname), PrefixedArg::TableDot) |
        map!(delimited!(lua_tag!("["), exp, lua_tag!("]")), PrefixedArg::TableIdx) |
        do_parse!(
            method: opt!(complete!(preceded!(lua_tag!(":"), varname))) >>
            args: args >>
            (PrefixedArg::FuncCall(FunctionCall { method, args }))
        )
    )
));

// A functioncall is just a prefixexp that ends in a funciton call
fn functioncall(input: &[u8]) -> IResult<&[u8], PrefixExp> {
    let res = prefixexp(input);
    // TODO: de-uglify
    let is_funccall = match res {
        IResult::Done(_, ref pe) => match pe.arg_chain.last() {
            Some(ref a) => match a {
                &&PrefixedArg::FuncCall(_) => true,
                _ => false,
            },
            _ => false,
        },
        _ => false,
    };

    if is_funccall {
        res
    }
    else {
        IResult::Error(error_position!(nom::ErrorKind::Verify, input))
    }
}

named!(varlist<Vec<PrefixExp>>, eat_lua_sep!(
    separated_nonempty_list_complete!(lua_tag!(","), prefixexp)
));

named!(args<Args>, eat_lua_sep!(
    alt!(
        map!(string::string_lit, Args::Str) |
        map!(table_lit, Args::Table) |
        map!(delimited!(lua_tag!("("), opt_explist, lua_tag!(")")), Args::ExpList)
    )
));

named!(table_lit<TableLit>, eat_lua_sep!(
    do_parse!(
        lua_tag!("{") >>
        fields: separated_list_complete!(fieldsep, field) >>
        opt!(complete!(fieldsep)) >>
        lua_tag!("}") >>
        (fields)
    )
));

named!(field<Field>, eat_lua_sep!(
    alt!(
        do_parse!(
            key: delimited!(lua_tag!("["), exp, lua_tag!("]")) >>
            lua_tag!("=") >>
            val: exp >>
            (Field::ExpAssign(key, val))
        ) |
        do_parse!(
            name: varname >>
            lua_tag!("=") >>
            val: exp >>
            (Field::NameAssign(name, val))
        ) |
        map!(exp, Field::PosAssign)
    )
));

// Either we get a ... or nothing, or some parameters which can be followed by ',...'
named!(parlist<Params>, eat_lua_sep!(
    alt!(
        do_parse!(
            names: namelist >>
            ellip: opt!(complete!(preceded!(lua_tag!(","), lua_tag!("...")))) >>
            (Params { names: names, variadic: ellip.is_some() })
        ) |
        value!(Params { names: Vec::new(), variadic: true }, lua_tag!("...")) |
        value!(Params { names: Vec::new(), variadic: false })
   )
));

named!(fieldsep, eat_lua_sep!(alt!(lua_tag!(",") | lua_tag!(";"))));

named!(pub toplevel<Vec<Statement>>, many1!(ws!(statement)));

#[cfg(test)]
mod test {
    use name::VarName;
    use super::*;
    use utils::test_utils::EMPTY;

    use nom::IResult;

    fn varname_vec(varnames: &[&'static str]) -> Vec<VarName<'static>> {
        varnames.iter().map(|n| VarName(n.as_bytes())).collect()
    }

    #[test]
    fn test_funcname() {
        let inputs = ["test", "test:blah", "hello.test", "test.f_oo.bar.baz:bl1ah"];
        let outputs = vec![
            FunctionName { path: varname_vec(&["test"]), method: None },
            FunctionName { path: varname_vec(&["test"]), method: Some(VarName(b"blah")) },
            FunctionName { path: varname_vec(&["hello", "test"]), method: None },
            FunctionName {
                path: varname_vec(&["test", "f_oo", "bar", "baz"]),
                method: Some(VarName(b"bl1ah")),
            },
        ];
        for (input, expected) in inputs.iter().zip(outputs.into_iter()) {
            assert_eq!(funcname(input.as_bytes()), IResult::Done(EMPTY, expected));
        }
    }

    #[test]
    fn test_parlist() {
        let good_inputs = ["hello", "...", "hello, world", "hello, world,   ..."];
        let bad_inputs = ["hello.world", "..f", "..., hello", "foo[bar]"];
        let good_outputs = vec![
            Params { names: varname_vec(&["hello"]), variadic: false },
            Params { names: Vec::new(), variadic: true },
            Params { names: varname_vec(&["hello", "world"]), variadic: false },
            Params { names: varname_vec(&["hello", "world"]), variadic: true },
        ];
        let bad_outputs = vec![
            IResult::Done(&b".world"[..],
                          Params { names: varname_vec(&["hello"]), variadic: false }),
            IResult::Done(&b"..f"[..], Params { names: Vec::new(), variadic: false }),
            IResult::Done(&b", hello"[..], Params { names: Vec::new(), variadic: true }),
            IResult::Done(&b"[bar]"[..], Params { names: varname_vec(&["foo"]), variadic: false }),
        ];

        for (input, expected) in good_inputs.iter().zip(good_outputs.into_iter()) {
            assert_eq!(parlist(input.as_bytes()), IResult::Done(EMPTY, expected));
        }
        for (input, expected) in bad_inputs.iter().zip(bad_outputs.into_iter()) {
            assert_eq!(parlist(input.as_bytes()), expected);
        }
    }

    #[test]
    fn test_binop() {
        let x = "nil + nil";
        println!("exp('{}') == {:?}", x, exp(x.as_bytes()));
    }

    #[test]
    fn test_funccall() {
        let x = "foo:bar('baz')(blah + car, ...)";
        //println!("funccall('{}') == {:#?}", x, functioncall(x.as_bytes()));
    }

    #[test]
    fn test_funcdef() {
        let block = br#"
        function foo(bar, baz)
            return 1 + 1;
        end"#;
        let x = statement(block);
        println!("statement == {:#?}", x);
    }

    #[test]
    fn test_varlist() {
        let list = b"foo, bar, baz";
        //let list = b"foo";
        let x = varlist(list);
        println!("varlist == {:#?}", x);
    }

    #[test]
    fn test_explist() {
        let list = b"  1  ";
        let x = explist(list);
        println!("explist == {:#?}", x);
    }

    // TODO: implement this
    fn insert_comments(code: &[u8]) -> Vec<u8> {
        unimplemented!()
    }

    #[test]
    fn test_complex() {
        let tst = br#"
          1
        "#;
        //let tst = &b"  --hello  \nabcy"[..];
        //let x = dbg_dmp!(tst, eat_lua_sep!(tag!("abc")));
        let x = dbg_dmp!(tst, exp);
        match x {
            IResult::Done(rest, s) => {
                println!("rest == '{}'", String::from_utf8_lossy(rest));
                println!("statement == {:#?}", s);
            }
            e @ _ => println!("error: {:?}", e),
        }
    }
}
