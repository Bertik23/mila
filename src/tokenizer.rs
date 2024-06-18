use std::fmt::Display;

use regex::Regex;

#[derive(Debug)]
pub enum Token {
    Function,
    Var,
    Const,
    Begin,
    End,
    Semicolon,
    Plus,
    Minus,
    Star,
    Div,
    Assign,
    Mod,
    Exit,
    Less,
    LessEq,
    Greater,
    GreaterEq,
    Equal,
    NotEq,
    Or,
    And,
    LParen,
    RParen,
    Program,
    Comma,
    Collon,
    LBracket,
    RBracket,
    DotDot,
    Dot,
    EOF,
    Array,
    Of,
    If,
    While,
    For,
    Then,
    Else,
    Do,
    Break,
    // Text(String),
    Ident(String),
    Integer(i32),
    String(String),
}

fn split_on_pattern(re: Regex, text: &str) -> Option<(&str, &str)> {
    if let Some(mat) = re.find(text) {
        let start_index = mat.end();
        Some((&text[..start_index], &text[start_index..]))
    } else {
        None
    }
}

macro_rules! strip_token_param {
    ($s:ident, $out:ident, $str:expr, $token:expr) => {
        if let Some((m, rest)) = split_on_pattern(Regex::new($str).unwrap(), $s) {
            $out.push($token(m.into()));
            $s = rest;
            continue;
        }
    };
    ($s:ident, $out:ident, $str:expr, $token:expr, $($rstr:expr, $rtoken:expr),+) => {
        if let Some((m, rest)) = split_on_pattern(Regex::new($str).unwrap(), $s) {
            $out.push($token(m.into()));
            $s = rest;
            continue;
        } else {strip_token_param!($s, $out, $($rstr, $rtoken),+)}
    };
}

macro_rules! strip_token {
    ($s:ident, $out:ident, $str:expr, $token:expr) => {
        if let Some((_, rest)) = split_on_pattern(Regex::new($str).unwrap(), $s) {
            $out.push($token);
            $s = rest;
            continue;
        }
    };
    ($s:ident, $out:ident, $str:expr, $token:expr, $($rstr:expr, $rtoken:expr),+) => {
        if let Some((_, rest)) = split_on_pattern(Regex::new($str).unwrap(), $s) {
            $out.push($token);
            $s = rest;
            continue;
        } else {strip_token!($s, $out, $($rstr, $rtoken),+)}
    };
}

fn min<T: std::cmp::PartialOrd>(a: T, b: T) -> T {
    if a < b {
        return a;
    }
    b
}

pub fn tokenize(code: &str) -> Result<Vec<Token>, String> {
    let mut s = code;
    let mut out = vec![];
    use Token::*;
    while !s.is_empty() {
        s = s.trim_start();
        strip_token!(
            s,
            out,
            r"^function\s",
            Function,
            r"^var\s",
            Var,
            r"^const\s",
            Const,
            r"^begin\s",
            Begin,
            r"^end",
            End,
            r"^;",
            Semicolon,
            r"^\+",
            Plus,
            r"^-",
            Minus,
            r"^\*",
            Star,
            r"^/",
            Div,
            r"^:=",
            Assign,
            r"^mod\s",
            Mod,
            r"^exit\s",
            Exit,
            r"^<>",
            NotEq,
            r"^<=",
            LessEq,
            r"^<",
            Less,
            r"^>=",
            GreaterEq,
            r"^>",
            Greater,
            r"^=",
            Equal,
            r"^\(",
            LParen,
            r"^\)",
            RParen,
            r"^,",
            Comma,
            r"^:",
            Collon,
            r"^\[",
            LBracket,
            r"^\]",
            RBracket,
            r"^\.\.",
            DotDot,
            r"^\.",
            Dot
        );

        strip_token_param!(
            s,
            out,
            r"^[a-zA-Z][a-zA-Z0-9_]*",
            |x: &str| {
                match x {
                    "array" => Array,
                    "of" => Of,
                    "while" => While,
                    "if" => If,
                    "for" => For,
                    "then" => Then,
                    "else" => Else,
                    "do" => Do,
                    "or" => Or,
                    "and" => And,
                    "function" => Function,
                    "begin" => Begin,
                    "end" => End,
                    "program" => Program,
                    "exit" => Exit,
                    "break" => Break,
                    id => Ident(id.to_string()),
                }
            },
            r"^($[0-9A-Fa-f]+)|(&[0-7]+)|([0-9]+)",
            |x: &str| {
                if x.starts_with('$') {
                    Integer(
                        i32::from_str_radix(x.trim_start_matches('$'), 16)
                            .unwrap(),
                    )
                } else if x.starts_with('&') {
                    Integer(
                        i32::from_str_radix(x.trim_start_matches('&'), 8)
                            .unwrap(),
                    )
                } else {
                    Integer(x.parse().unwrap())
                }
            },
            "^\".*\"",
            Token::String,
            "^'.*'",
            Token::String
        );

        s = s.trim_start();

        if s.is_empty() {
            out.push(EOF);
        } else {
            return Err(format!(
                "Unrecognized token: {}",
                &(s[..min(s.len(), 100)])
            ));
        };
    }
    Ok(out)
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}
