#![allow(dead_code)]

#[derive(Debug, PartialEq)]
enum Number {
    Int(u32),
    Float(f32),
}
#[derive(Debug, PartialEq)]
enum Atom {
    Symbol(String),
    Number(Number),
}
#[derive(Debug, PartialEq)]
enum Expr<T> {
    Atom(Atom),
    List(Vec<T>),
}
#[derive(Debug, PartialEq)]
enum T {
    Symbol(String),
    Number(Number),
    Atom(Atom),
    List(Vec<T>),
    Expr(Expr<T>),
}

fn tokenize(s: &str) -> Vec<String> {
    s.replace("(", " ( ")
        .replace(")", " ) ")
        .split(" ")
        .map(ToOwned::to_owned)
        .collect()
}

fn parse_from_tokens(tokens: Vec<String>) -> T {
    todo!()
}

#[test]
fn expected_to_panic() {
    let s = "(begin (define r 10) (* pi (* r r)))";
    let t = parse_from_tokens(tokenize(s));

    assert_eq!(t, T::Symbol("abc".to_string()))
}
