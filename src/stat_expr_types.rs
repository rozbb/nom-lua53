use op::{BinOp, UnOp};
use name::VarName;
use num::Numeral;
use string::StringLit;

// TODO: Make all fields in this file public

pub type TableLit<'a> = Vec<Field<'a>>;

#[derive(Clone, Debug, PartialEq)]
pub enum Statement<'a> {
    Semicolon,
    Break,
    Goto(VarName<'a>),
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

#[derive(Clone, Debug, PartialEq)]
pub enum Exp<'a> {
    Nil,
    Ellipses,
    Bool(bool),
    Num(Numeral),
    Str(StringLit<'a>),
    Lambda(FunctionBody<'a>),
    FuncCall(FunctionCall<'a>),
    PrefixExp(Box<PrefixExp<'a>>),
    Table(TableLit<'a>),
    UnExp(UnOp, Box<Exp<'a>>),
    BinExp(Box<Exp<'a>>, BinOp, Box<Exp<'a>>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum OpOrExp2<'a> {
    Op(UnOrBinOp),
    Exp2(Exp2<'a>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum UnOrBinOp {
    UnOp(UnOp),
    BinOp(BinOp),
}

#[derive(Clone, Debug, PartialEq)]
pub struct FlatExp<'a>(pub Vec<OpOrExp2<'a>>);

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
    pub cond: Exp<'a>,
    pub block: Block<'a>,
}

// repeat <block> until <cond>
#[derive(Clone, Debug, PartialEq)]
pub struct RepeatBlock<'a> {
    pub cond: Exp<'a>,
    pub block: Block<'a>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct IfThenElse<'a> {
    pub cond: Exp<'a>,
    pub then_blk: Block<'a>,
    pub elseifs: Vec<(Exp<'a>, Block<'a>)>,
    pub else_blk: Option<Block<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ForRange<'a> {
    pub var: VarName<'a>,
    pub start: Exp<'a>,
    pub end: Exp<'a>,
    pub step: Option<Exp<'a>>,
    pub do_blk: Block<'a>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ForIter<'a> {
    pub vars: Vec<VarName<'a>>,
    pub exps: Vec<Exp<'a>>,
    pub do_blk: Block<'a>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionDef<'a> {
    pub name: FunctionName<'a>,
    pub body: FunctionBody<'a>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionName<'a> {
    pub path: Vec<VarName<'a>>,
    pub method: Option<VarName<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionBody<'a> {
    pub params: Params<'a>,
    pub body: Block<'a>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Block<'a> {
    pub stmts: Vec<Statement<'a>>,
    pub ret_stmt: Option<Vec<Exp<'a>>>
}

#[derive(Clone, Debug, PartialEq)]
pub struct Params<'a> {
    pub names: Vec<VarName<'a>>,
    pub variadic: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Assignment<'a> {
    pub vars: Vec<PrefixExp<'a>>,
    pub vals: Vec<Exp<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct LAssignment<'a> {
    pub vars: Vec<VarName<'a>>,
    pub vals: Option<Vec<Exp<'a>>>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Args<'a> {
    Str(StringLit<'a>),
    Table(TableLit<'a>),
    ExpList(Vec<Exp<'a>>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct PrefixExp<'a> {
    pub prefix: ExpOrVarName<'a>,
    // TODO: Fix naming conventions here
    pub suffix_chain: Vec<ExpSuffix<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExpOrVarName<'a> {
    Exp(Exp<'a>),
    VarName(VarName<'a>),
}

// TODO: Fix naming conventions here
#[derive(Clone, Debug, PartialEq)]
pub enum ExpSuffix<'a> {
    TableDot(VarName<'a>),
    TableIdx(Exp<'a>),
    FuncCall(FunctionCall<'a>)
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionCall<'a> {
    pub method: Option<VarName<'a>>,
    pub args: Args<'a>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Field<'a> {
    NameAssign(VarName<'a>, Exp<'a>), // Assigning by varname, e.g {foo = 10}
    ExpAssign(Exp<'a>, Exp<'a>), // Assigning by expr, e.g {["foo" .. "bar"] = 10}
    PosAssign(Exp<'a>), // Assigning by position, e.g {"foo", "bar"} assigns in positions 1 and 2
}
