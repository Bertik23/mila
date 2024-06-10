use std::collections::VecDeque;

use crate::tokenizer::Token;

#[derive(Debug)]
struct VarDec {
    is_const: bool,
    ident: String,
    typ: Type,
    assign: Option<Assign>,
}

#[derive(Debug)]
struct FnDec {
    ident: String,
    typ: Type,
    params: Vec<Var>,
    locals: Vec<VarDec>,
    expr: Expression,
}

#[derive(Debug)]
struct Var {
    ident: String,
    typ: Type,
}

#[derive(Debug)]
enum Expression {
    Block(Vec<Expression>),
    If {
        cond: Box<Expression>,
        then: Box<Expression>,
        el: Option<Box<Expression>>,
    },
    While,
    For,
    Op(Operation, Box<Expression>, Box<Expression>),
    Nothing,
}

#[derive(Debug)]
enum Operation {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Assign,
    Less,
    Grater,
    Leq,
    Geq,
    Eq,
    Neq,
}

#[derive(Debug)]
enum UnOp {}
#[derive(Debug)]
enum BinOp {}

#[derive(Debug)]
enum Type {
    Array {
        of: Box<Type>,
        start_index: i32,
        end_index: i32,
    },
    Integer,
}

#[derive(Debug)]
struct Assign;

#[derive(Debug)]
pub struct Program {
    globals: Vec<VarDec>,
    functions: Vec<FnDec>,
    main_vars: Vec<VarDec>,
    main_expr: Expression,
}

pub fn parse(mut token_stream: VecDeque<Token>) -> Program {
    println!("{:?}", token_stream.front());
    if let Some(Token::Program) = token_stream.pop_front() {
    } else {
        println!("{:?}", token_stream.front());
        panic!("Syntax error: Expected 'program'")
    }
    if let Some(Token::Ident(_)) = token_stream.pop_front() {
    } else {
        panic!("Syntax error: Expected identifier")
    }
    if let Some(Token::Semicolon) = token_stream.pop_front() {
    } else {
        panic!("Syntax error: Expected ';'")
    };

    let globals = parse_var_declarations(&mut token_stream);
    let functions = parse_function_declarations(&mut token_stream);
    let main_vars = parse_var_declarations(&mut token_stream);
    let main_expr = parse_expression(&mut token_stream);

    Program {
        globals,
        functions,
        main_vars,
        main_expr,
    }
}

fn parse_var_declarations(token_stream: &mut VecDeque<Token>) -> Vec<VarDec> {
    let mut decs = Vec::new();
    while matches!(token_stream.front(), Some(Token::Var) | Some(Token::Const))
    {
        decs.extend(parse_var_declaration(token_stream));
    }
    decs
}

fn parse_var_declaration(token_stream: &mut VecDeque<Token>) -> Vec<VarDec> {
    let is_const = match token_stream.pop_front() {
        Some(Token::Var) => false,
        Some(Token::Const) => true,
        _ => panic!("Syntax error: Expected 'var' or 'const' "),
    };

    let ident = parse_ident(token_stream);

    if !matches!(token_stream.pop_front(), Some(Token::Collon)) {
        panic!("Syntax error: Expected ':'")
    }

    let typ = parse_type(token_stream);

    parse_semicolon(token_stream);

    vec![VarDec {
        is_const,
        ident,
        assign: None,
        typ,
    }]
}

fn parse_function_declarations(
    token_stream: &mut VecDeque<Token>,
) -> Vec<FnDec> {
    let mut decs = Vec::new();
    while matches!(token_stream.front(), Some(Token::Function)) {
        decs.push(parse_fn_declaration(token_stream));
    }
    decs
}

fn parse_fn_declaration(token_stream: &mut VecDeque<Token>) -> FnDec {
    if !matches!(token_stream.pop_front(), Some(Token::Function)) {
        panic!("Syntax error: Expected 'function'");
    }
    let ident = parse_ident(token_stream);
    if !matches!(token_stream.pop_front(), Some(Token::LParen)) {
        panic!("Syntax error: Expected '('");
    };
    let params = if !matches!(token_stream.front(), Some(Token::RParen)) {
        parse_params(token_stream)
    } else {
        Vec::new()
    };
    if !matches!(token_stream.pop_front(), Some(Token::RParen)) {
        panic!("Syntax error: Expected ')'");
    };
    parse_colon(token_stream);

    let typ = parse_type(token_stream);

    parse_semicolon(token_stream);

    let locals = parse_var_declarations(token_stream);

    let expr = parse_expression(token_stream);
    FnDec {
        ident,
        typ,
        params,
        locals,
        expr,
    }
}

fn parse_params(token_stream: &mut VecDeque<Token>) -> Vec<Var> {
    let mut params = Vec::new();
    params.push(parse_param(token_stream));
    while matches!(token_stream.front(), Some(Token::Semicolon)) {
        token_stream.pop_front();
        params.push(parse_param(token_stream));
    }
    params
}
fn parse_param(token_stream: &mut VecDeque<Token>) -> Var {
    let ident = parse_ident(token_stream);
    parse_colon(token_stream);
    let typ = parse_type(token_stream);
    Var { ident, typ }
}

fn parse_expression(token_stream: &mut VecDeque<Token>) -> Expression {
    match token_stream.front() {
        Some(Token::Begin) => parse_block(token_stream),
        Some(Token::If) => parse_if(token_stream),
        _ => Expression::Nothing,
    }
}

fn parse_block(token_stream: &mut VecDeque<Token>) -> Expression {
    if !matches!(token_stream.pop_front(), Some(Token::Begin)) {
        panic!("Syntax error: Expected 'begin'");
    }
    let mut exprs = Vec::new();
    exprs.push(parse_expression(token_stream));
    while match token_stream.pop_front() {
        Some(Token::Semicolon) => true,
        Some(Token::End) => false,
        _ => panic!("Syntax error. Expected 'end' or ';'."),
    } {
        exprs.push(parse_expression(token_stream));
    }
    Expression::Block(exprs)
}

fn parse_if(token_stream: &mut VecDeque<Token>) -> Expression {
    if !matches!(token_stream.pop_front(), Some(Token::If)) {
        panic!("Syntax error: Expected 'if'");
    }
    let cond = Box::new(parse_expression(token_stream));
    if !matches!(token_stream.pop_front(), Some(Token::Then)) {
        panic!("Syntax error: Expected 'then'");
    }
    let then = Box::new(parse_expression(token_stream));
    let el = match token_stream.front() {
        Some(Token::Else) => {
            token_stream.pop_front();
            Some(Box::new(parse_expression(token_stream)))
        }
        _ => None,
    };
    Expression::If { cond, then, el }
}

fn parse_type(token_stream: &mut VecDeque<Token>) -> Type {
    match token_stream.pop_front() {
        Some(Token::Array) => {
            parse_lbracket(token_stream);
            let start = parse_integer(token_stream);
            if !matches!(token_stream.pop_front(), Some(Token::DotDot)) {
                panic!("Syntax error: Expected '..'")
            };
            let end = parse_integer(token_stream);
            parse_rbracket(token_stream);
            if !matches!(token_stream.pop_front(), Some(Token::Of)) {
                panic!("Syntax error: Expected 'of'")
            };
            let typ = parse_type(token_stream);
            Type::Array {
                of: Box::from(typ),
                start_index: start,
                end_index: end,
            }
        }

        Some(Token::Ident(id)) => {
            if id != "integer" {
                panic!("Expected 'integer' found {}", id)
            } else {
                Type::Integer
            }
        }
        something => panic!(
            "Syntax error. 'integer' or 'array' expected, found {:?}",
            something
        ),
    }
}

fn parse_ident(token_stream: &mut VecDeque<Token>) -> String {
    if let Some(Token::Ident(id)) = token_stream.pop_front() {
        id
    } else {
        panic!("Syntax error: Expected identifier.")
    }
}

fn parse_integer(token_stream: &mut VecDeque<Token>) -> i32 {
    if let Some(Token::Integer(id)) = token_stream.pop_front() {
        id
    } else {
        panic!("Syntax error: Expected integer literal.")
    }
}

fn parse_colon(token_stream: &mut VecDeque<Token>) {
    if !matches!(token_stream.pop_front(), Some(Token::Collon)) {
        panic!("Syntax error: Expected ':'");
    };
}

fn parse_semicolon(token_stream: &mut VecDeque<Token>) {
    if !matches!(token_stream.pop_front(), Some(Token::Semicolon)) {
        panic!("Syntax error: Expected ';'");
    };
}

fn parse_lbracket(token_stream: &mut VecDeque<Token>) {
    if !matches!(token_stream.pop_front(), Some(Token::LBracket)) {
        panic!("Syntax error: Expected '['");
    };
}
fn parse_rbracket(token_stream: &mut VecDeque<Token>) {
    if !matches!(token_stream.pop_front(), Some(Token::RBracket)) {
        panic!("Syntax error: Expected ']'");
    };
}
