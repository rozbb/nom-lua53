use name::VarName;
use stat_expr::*;
use stat_expr_types::*;
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
