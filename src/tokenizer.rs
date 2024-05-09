use std::fmt::Display;

use regex::Regex;

#[derive(Debug)]
pub enum Token {
    Function,
    Var,
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
    // Text(String),
    Ident(String),
    Integer(String),
    String(String),
}

fn split_on_pattern<'a>(
    re: Regex,
    text: &'a str,
) -> Option<(&'a str, &'a str)> {
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
    return b;
}

pub fn tokenize(code: &String) -> Result<Vec<Token>, String> {
    let mut s = code.as_str();
    let mut out = vec![];
    use Token::*;
    while s.len() != 0 {
        s = s.trim_start();
        // println!("{:?}", out.last());
        strip_token!(
            s,
            out,
            r"^function\s",
            Function,
            r"^var\s",
            Var,
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
            r"^<",
            Less,
            r"^<=",
            LessEq,
            r"^>",
            Greater,
            r"^>=",
            GreaterEq,
            r"^=",
            Equal,
            r"^\(",
            LParen,
            r"^\)",
            RParen,
            r"^program\s",
            Program,
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
            Ident,
            r"^($[0-9A-Fa-f]+)|(&[0-7]+)|([0-9]+)",
            Integer,
            "^\".*\"",
            Token::String,
            "^'.*'",
            Token::String
        );

        if s.len() == 0 {
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
