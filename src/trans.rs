use stat_expr_types::{Exp, Exp2, FlatExp, OpOrExp2, UnOrBinOp};
use op::{BinOp, UnOp};

// Lua 5.3 precedence table:
//   or
//   and
//   <     >     <=    >=    ~=    ==
//   |
//   ~
//   &
//   <<    >>
//   ..
//   +     -
//   *     /     //    %
//   unary operators (not   #     -     ~)
//   ^

lazy_static! {
    static ref UNOPS: Vec<UnOp> = {
        let table = &mut [UnOp::Minus, UnOp::BoolNot, UnOp::Length, UnOp::BitNot];
        table.sort();
        table.to_vec()
    };

    static ref BINOP_PRECEDENCE: Vec<Vec<BinOp>> = {
        let table: &mut [&mut [BinOp]] = &mut [
            &mut [BinOp::Pow],
            &mut [BinOp::Mul, BinOp::Div, BinOp::IntDiv, BinOp::Mod],
            &mut [BinOp::Plus, BinOp::Minus],
            &mut [BinOp::Concat],
            &mut [BinOp::BitShl, BinOp::BitShr],
            &mut [BinOp::BitAnd],
            &mut [BinOp::BitXor],
            &mut [BinOp::BitOr],
            &mut [BinOp::Lt, BinOp::Gt, BinOp::Leq, BinOp::Geq, BinOp::Neq, BinOp::Eq],
            &mut [BinOp::BoolAnd],
            &mut [BinOp::BoolOr],
        ];
        let mut acc = Vec::new();
        for s in table.iter_mut() {
            s.sort();
            acc.push(s.to_vec());
        }
        acc
    };
}

pub fn flatexp_from_components<'a>(head: (Vec<UnOp>, Exp2<'a>),
                                   binop_chain: Vec<(BinOp, (Vec<UnOp>, Exp2<'a>))>)
        -> FlatExp<'a> {
    fn tuple_to_flatvec<'a>((unops, e): (Vec<UnOp>, Exp2<'a>)) -> Vec<OpOrExp2<'a>> {
        let mut v: Vec<OpOrExp2<'a>> = unops.into_iter().map(UnOrBinOp::UnOp).map(OpOrExp2::Op).collect();
        v.push(OpOrExp2::Exp2(e));
        v
    }

    let acc = tuple_to_flatvec(head);
    let res = binop_chain.into_iter().fold(acc,
        |mut a, (binop, t)| {
            a.push(OpOrExp2::Op(UnOrBinOp::BinOp(binop)));
            a.extend_from_slice(&*tuple_to_flatvec(t));
            a
    });

    FlatExp(res)
}

// Convenience type for implementing the below conversion function
#[derive(Debug)]
enum OpOrExp<'a> {
    Op(UnOrBinOp),
    Exp(Exp<'a>),
}

impl<'a> OpOrExp<'a> {
    fn is_binop(&self) -> bool {
        if let &OpOrExp::Op(UnOrBinOp::BinOp(_)) = self { true }
        else { false }
    }

    fn is_unop(&self) -> bool {
        if let &OpOrExp::Op(UnOrBinOp::UnOp(_)) = self { true }
        else { false }
    }

    fn is_exp(&self) -> bool {
        if let &OpOrExp::Exp(_) = self { true }
        else { false }
    }
}

impl<'a> From<FlatExp<'a>> for Exp<'a> {
    fn from(fe: FlatExp<'a>) -> Exp<'a> {
        // Helper function. Expects a,b to be Exps and o to be a BinOp
        fn merge_nodes_binop<'a>(a: OpOrExp<'a>, o: OpOrExp<'a>, b: OpOrExp<'a>) -> OpOrExp<'a> {
            match (a, o, b) {
                (OpOrExp::Exp(a), OpOrExp::Op(UnOrBinOp::BinOp(o)), OpOrExp::Exp(b)) => {
                    let merged_exp = Exp::BinExp(Box::new(a), o, Box::new(b));
                    OpOrExp::Exp(merged_exp)
                }
                _ => panic!("unexpected input variants in merge_nodes_binop"),
            }
        }

        // Helper function. Expects o to be a UnOp and a to be an Exp
        fn merge_nodes_unop<'a>(o: OpOrExp, a: OpOrExp<'a>) -> OpOrExp<'a> {
            match (o, a) {
                (OpOrExp::Op(UnOrBinOp::UnOp(o)), OpOrExp::Exp(a)) => {
                    let merged_exp = Exp::UnExp(o, Box::new(a));
                    OpOrExp::Exp(merged_exp)
                }
                _ => panic!("unexpected input variants in merge_nodes_unop"),
            }
        }

        // TODO: make this more efficient
        fn merge_all_binops(explist: &mut Vec<OpOrExp>, binops: &[BinOp]) {
            loop {
                let mut tojoin_idx: Option<usize> = None;
                for (i, oe) in explist.iter().enumerate().filter(|&(_, ref oe)| oe.is_binop()) {
                    match oe {
                        &OpOrExp::Op(UnOrBinOp::BinOp(ref o)) => {
                            // Found something to join
                            if binops.binary_search(o).is_ok() {
                                assert!(i > 0);
                                assert!(explist[i-1].is_exp());
                                assert!(i.checked_add(1).is_some());
                                let next = explist.get(i+1).unwrap();

                                // If UnOps haven't been merged yet, ignore them. If there are two
                                // subsequent binops, that's an error. Otherwise we have a $ b
                                // where a and b are Exps and $ is a BinOp
                                match next {
                                    &OpOrExp::Op(UnOrBinOp::UnOp(_)) => continue,
                                    &OpOrExp::Op(UnOrBinOp::BinOp(_)) => {
                                        panic!("encountered two binops next to each other");
                                    },
                                    &OpOrExp::Exp(_) => {
                                        tojoin_idx = Some(i);
                                        break;
                                    }
                                }
                            }
                        }
                        _ => unreachable!(),
                    }
                }

                if let Some(i) = tojoin_idx {
                    let a = explist.remove(i-1);
                    let o = explist.remove(i-1);
                    let b = explist.remove(i-1);
                    let merged = merge_nodes_binop(a, o, b);
                    explist.insert(i-1, merged);
                }
                // Joined everything we could. Break
                else {
                    break;
                }
            }
        }

        fn merge_all_unops(explist: &mut Vec<OpOrExp>, unops: &[UnOp]) {
            loop {
                let mut tojoin_idx: Option<usize> = None;
                // Reverse iterate, since we want to apply stacked unary operators right-to-left
                for (i, oe) in explist.iter().enumerate().filter(|&(_, ref oe)| oe.is_unop()).rev() {
                    match oe {
                        &OpOrExp::Op(UnOrBinOp::UnOp(ref o)) => {
                            // Found something to join
                            if unops.binary_search(o).is_ok() {
                                assert!(i.checked_add(1).is_some());
                                let next = explist.get(i+1).unwrap();
                                assert!(next.is_exp());

                                tojoin_idx = Some(i);
                                break;
                            }
                        }
                        _ => unreachable!(),
                    }
                }

                if let Some(i) = tojoin_idx {
                    let o = explist.remove(i);
                    let a = explist.remove(i);
                    let merged = merge_nodes_unop(o, a);
                    explist.insert(i, merged);
                }
                // Joined everything we could. Break
                else {
                    break;
                }
            }
        }

        // First apply the highest-precedent binop (^) where applicable. This will miss cases
        // like 10 ^ #"hi" == 100. So then apply all unops, then apply all binops, starting again
        // from the highest-precedent one.

        let mut explist: Vec<OpOrExp> = fe.0.into_iter().map(|oe| {
            match oe {
                OpOrExp2::Op(o) => OpOrExp::Op(o),
                OpOrExp2::Exp2(e) => OpOrExp::Exp(Exp::from(e)),
            }
        }).collect();

        // First pass: find all triplets of the form a $ b where a and b are Exps and $ is a binop
        // of the highest precedence
        merge_all_binops(&mut explist, &*BINOP_PRECEDENCE[0]);
        merge_all_unops(&mut explist, &*UNOPS);

        for binops in BINOP_PRECEDENCE.iter() {
            merge_all_binops(&mut explist, &*binops);
        }

        assert_eq!(explist.len(), 1, "Exp tree construction didn't complete");
        match explist.pop().unwrap() {
            OpOrExp::Exp(e) => e,
            _ => unreachable!(),
        }
    }
}

impl<'a> From<Exp2<'a>> for Exp<'a> {
    fn from(e: Exp2<'a>) -> Exp<'a> {
        match e {
            Exp2::Nil => Exp::Nil,
            Exp2::Ellipses => Exp::Ellipses,
            Exp2::Bool(b) => Exp::Bool(b),
            Exp2::Num(n) => Exp::Num(n),
            Exp2::Str(s) => Exp::Str(s),
            Exp2::Lambda(l) => Exp::Lambda(l),
            Exp2::FuncCall(f) => Exp::FuncCall(f),
            Exp2::PrefixExp(p) => Exp::PrefixExp(p),
            Exp2::Table(t) => Exp::Table(t),
        }
    }
}
