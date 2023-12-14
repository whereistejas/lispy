#[derive(Debug, PartialEq)]
pub enum Number {
    Int(u32),
    Float(f32),
}
#[derive(Debug, PartialEq)]
pub enum Atom {
    Symbol(String),
    Number(Number),
}
#[derive(Debug, PartialEq)]
pub enum Expr<T> {
    Atom(Atom),
    List(Vec<T>),
}
#[derive(Debug, PartialEq)]
pub enum T {
    Symbol(String),
    Number(Number),
    Atom(Atom),
    List(Vec<T>),
    Expr(Expr<T>),
}

pub fn tokenize(s: &str) -> Vec<String> {
    (s.replace("(", " ( ").replace(")", " ) "))
        .split(" ")
        .filter_map(|s| {
            if s.is_empty() {
                None
            } else {
                Some(s.to_owned())
            }
        })
        .collect()
}

pub fn parse_atom(atom: &str) -> T {
    atom.parse::<u32>()
        .map(|i| T::Number(Number::Int(i)))
        .unwrap_or_else(|_| {
            atom.parse::<f32>()
                .map(|f| T::Number(Number::Float(f)))
                .unwrap_or_else(|_| T::Symbol(atom.to_string()))
        })
}

// Keep recursively reading nested statements.
// Prepare return all tokens when you encounter the first ")".
pub fn parse_from_tokens(tokens: Vec<&str>) -> T {
    match tokens[..] {
        [] => panic!("unexpected EOF"),
        ["(", ..] => {
            let mut tokens = tokens.into_iter().skip(1).collect::<Vec<_>>();
            let mut l = vec![];

            while tokens.first() != Some(&")") {
                let token = parse_from_tokens(tokens.clone());
                l.push(token);
                tokens = tokens.into_iter().skip(1).collect::<Vec<_>>();
            }

            T::Expr(Expr::List(l))
        }
        [token, ..] => {
            // We only convert the first token because the caller of this instance will try to parse the next token.
            parse_atom(token)
        }
    }
}

#[test]
fn tokenize_and_parse() {
    let t = parse_from_tokens(
        tokenize("(begin (define r 10) (* pi (* r r)))")
            .iter()
            .map(|s| &**s)
            .collect(),
    );

    let e = T::Expr(Expr::List(vec![
        T::Symbol("begin".to_string()),
        T::Expr(Expr::List(vec![
            T::Symbol("define".to_string()),
            T::Symbol("r".to_string()),
            T::Number(Number::Int(10)),
        ])),
        T::Symbol("define".to_string()),
        T::Symbol("r".to_string()),
        T::Number(Number::Int(10)),
    ]));

    assert_eq!(t, e, "{t:#?}")
}
