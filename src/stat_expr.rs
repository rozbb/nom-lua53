use name::{goto, label, namelist, varname};
use num::num_lit;
use op::{binop, unop, UnOp};
use stat_expr_types::*;
use string::string_lit;
use trans;

use nom::{self, IResult};

named!(unopexp2<(Vec<UnOp>, Exp2)>, eat_lua_sep!(
    tuple!(
        many0!(unop),
        exp2
    )
));

named!(flatexp<FlatExp>, eat_lua_sep!(
    do_parse!(
        head: unopexp2 >>
        binop_chain: many0!(tuple!(binop, unopexp2)) >>
        (trans::flatexp_from_components(head, binop_chain))
    ))
);

named!(pub exp<Exp>, map!(flatexp, Exp::from));

named!(exp2<Exp2>, eat_lua_sep!(
    alt!(
        value!(Exp2::Nil, lua_tag!("nil")) |
        value!(Exp2::Ellipses, lua_tag!("...")) |
        value!(Exp2::Bool(true), lua_tag!("true")) |
        value!(Exp2::Bool(false), lua_tag!("false")) |
        map!(num_lit, Exp2::Num) |
        map!(string_lit, Exp2::Str) |
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
        map!(goto, Statement::Goto) |
        map!(varassign, Statement::Assignment) |
        map!(label, Statement::Label) |
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

named!(pub funcname<FunctionName>, eat_lua_sep!(
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
        suffix_chain: many0!(prefixexp3) >>
        (PrefixExp { prefix, suffix_chain })
    )
));

named!(prefixexp2<ExpOrVarName>, eat_lua_sep!(
    alt!(
        map!(delimited!(lua_tag!("("), exp, lua_tag!(")")), ExpOrVarName::Exp) |
        map!(varname, ExpOrVarName::VarName)
    )
));

named!(prefixexp3<ExpSuffix>, eat_lua_sep!(
    alt!(
        map!(preceded!(lua_tag!("."), varname), ExpSuffix::TableDot) |
        map!(delimited!(lua_tag!("["), exp, lua_tag!("]")), ExpSuffix::TableIdx) |
        do_parse!(
            method: opt!(complete!(preceded!(lua_tag!(":"), varname))) >>
            args: args >>
            (ExpSuffix::FuncCall(FunctionCall { method, args }))
        )
    )
));

// A functioncall is just a prefixexp that ends in a funciton call
fn functioncall(input: &[u8]) -> IResult<&[u8], PrefixExp> {
    let res = prefixexp(input);
    // TODO: de-uglify
    let is_funccall = match res {
        IResult::Done(_, ref pe) => match pe.suffix_chain.last() {
            Some(ref a) => match a {
                &&ExpSuffix::FuncCall(_) => true,
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
        map!(string_lit, Args::Str) |
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
named!(pub parlist<Params>, eat_lua_sep!(
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
