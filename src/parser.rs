use std::collections::VecDeque;

use crate::tokenizer::Token;

#[derive(Debug, Clone)]
pub struct VarDec {
    is_const: bool,
    pub ident: String,
    pub typ: Type,
    pub assign: Option<Expression>,
}

#[derive(Debug, Clone)]
pub struct FnDec {
    pub ident: String,
    pub typ: Type,
    pub params: Vec<Var>,
    pub locals: Vec<VarDec>,
    pub expr: Expression,
}

#[derive(Debug, Clone)]
pub struct Var {
    pub ident: String,
    pub typ: Type,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Block(Vec<Expression>),
    If {
        cond: Box<Expression>,
        then: Box<Expression>,
        el: Option<Box<Expression>>,
    },
    While {
        cond: Box<Expression>,
        body: Box<Expression>,
    },
    For,
    Op(Operation, Box<Expression>, Box<Expression>),
    Var(String),
    Literal(i32),
    Call(String, Vec<Expression>),
    Exit,
    Nothing,
}

#[derive(Debug, Clone)]
pub enum Operation {
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
    And,
    Or,
}

#[derive(Debug)]
enum UnOp {}
#[derive(Debug)]
enum BinOp {}

#[derive(Debug, Clone)]
pub enum Type {
    Array {
        of: Box<Type>,
        start_index: i32,
        end_index: i32,
    },
    Integer,
}

#[derive(Debug)]
pub struct Program {
    pub name: String,
    pub globals: Vec<VarDec>,
    pub functions: Vec<FnDec>,
    pub main_vars: Vec<VarDec>,
    pub main_expr: Expression,
}

pub fn parse(mut token_stream: VecDeque<Token>) -> Program {
    if let Some(Token::Program) = token_stream.pop_front() {
    } else {
        panic!("Syntax error: Expected 'program'")
    }
    let name = parse_ident(&mut token_stream);
    parse_semicolon(&mut token_stream);

    let globals = parse_var_declarations(&mut token_stream);
    let functions = parse_function_declarations(&mut token_stream);
    let main_vars = parse_var_declarations(&mut token_stream);
    let main_expr = parse_expression(&mut token_stream);

    if let Some(Token::Dot) = token_stream.pop_front() {
    } else {
        panic!("Syntax error: Expected '.'")
    };

    Program {
        name,
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

    let mut out = Vec::new();

    while matches!(token_stream.front(), Some(Token::Ident(_))) {
        let ident = parse_ident(token_stream);
        let assign = if matches!(token_stream.front(), Some(Token::Equal)) {
            token_stream.pop_front();
            Some(parse_expression(token_stream))
        } else {
            None
        };
        let typ = match assign {
            Some(_) => Type::Integer,
            None => {
                parse_colon(token_stream);
                parse_type(token_stream)
            }
        };
        out.push(VarDec {
            is_const,
            ident,
            assign,
            typ,
        })
    }

    // if !matches!(token_stream.pop_front(), Some(Token::Collon)) {
    //     panic!("Syntax error: Expected ':'")
    // }
    //
    // let typ = parse_type(token_stream);

    parse_semicolon(token_stream);

    out
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
    parse_semicolon(token_stream);
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
        Some(Token::While) => parse_while(token_stream),
        Some(Token::For) => parse_for(token_stream),
        _ => parse_operation_l7(token_stream),
        // _ => Expression::Nothing,
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
        something => panic!(
            "Syntax error. Expected 'end' or ';' found {:?}. {:?}",
            something, token_stream
        ),
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
        panic!("Syntax error: Expected 'then' {:?}", token_stream);
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

fn parse_while(token_stream: &mut VecDeque<Token>) -> Expression {
    if !matches!(token_stream.pop_front(), Some(Token::While)) {
        panic!("Syntax error: Expected 'while'");
    }
    let cond = Box::new(parse_expression(token_stream));
    if !matches!(token_stream.pop_front(), Some(Token::Do)) {
        panic!("Syntax error: Expected 'do'");
    }
    let body = Box::new(parse_expression(token_stream));
    Expression::While { cond, body }
}

fn parse_for(token_stream: &mut VecDeque<Token>) -> Expression {
    todo!()
}

fn parse_operation_l7(token_stream: &mut VecDeque<Token>) -> Expression {
    let lhs = parse_operation_l6(token_stream);
    let rhs = if matches!(token_stream.front(), Some(Token::Assign)) {
        token_stream.pop_front();
        parse_expression(token_stream)
    } else {
        return lhs;
    };
    Expression::Op(Operation::Assign, Box::new(lhs), Box::new(rhs))
}

fn parse_operation_l6(token_stream: &mut VecDeque<Token>) -> Expression {
    let mut lhs = parse_operation_l5(token_stream);
    while let Some(op) = match token_stream.front() {
        Some(Token::And) => Some(Operation::And),
        Some(Token::Or) => Some(Operation::Or),
        _ => None,
    } {
        token_stream.pop_front();
        lhs = Expression::Op(
            op,
            Box::new(lhs),
            Box::new(parse_operation_l5(token_stream)),
        );
    }
    lhs
}

fn parse_operation_l5(token_stream: &mut VecDeque<Token>) -> Expression {
    let mut lhs = parse_operation_l4(token_stream);
    while let Some(op) = match token_stream.front() {
        Some(Token::Equal) => Some(Operation::Eq),
        Some(Token::NotEq) => Some(Operation::Neq),
        _ => None,
    } {
        token_stream.pop_front();
        lhs = Expression::Op(
            op,
            Box::new(lhs),
            Box::new(parse_operation_l4(token_stream)),
        );
    }
    lhs
}

fn parse_operation_l4(token_stream: &mut VecDeque<Token>) -> Expression {
    let mut lhs = parse_operation_l3(token_stream);
    while let Some(op) = match token_stream.front() {
        Some(Token::Less) => Some(Operation::Less),
        Some(Token::LessEq) => Some(Operation::Leq),
        Some(Token::Greater) => Some(Operation::Grater),
        Some(Token::GreaterEq) => Some(Operation::Geq),
        _ => None,
    } {
        token_stream.pop_front();
        lhs = Expression::Op(
            op,
            Box::new(lhs),
            Box::new(parse_operation_l3(token_stream)),
        );
    }
    lhs
}

fn parse_operation_l3(token_stream: &mut VecDeque<Token>) -> Expression {
    let mut lhs = parse_operation_l2(token_stream);
    while let Some(op) = match token_stream.front() {
        Some(Token::Plus) => Some(Operation::Add),
        Some(Token::Minus) => Some(Operation::Sub),
        _ => None,
    } {
        token_stream.pop_front();
        lhs = Expression::Op(
            op,
            Box::new(lhs),
            Box::new(parse_operation_l2(token_stream)),
        );
    }
    lhs
}

fn parse_operation_l2(token_stream: &mut VecDeque<Token>) -> Expression {
    let mut lhs = parse_operation_l1(token_stream);
    while let Some(op) = match token_stream.front() {
        Some(Token::Star) => Some(Operation::Mul),
        Some(Token::Div) => Some(Operation::Div),
        Some(Token::Mod) => Some(Operation::Mod),
        _ => None,
    } {
        token_stream.pop_front();
        lhs = Expression::Op(
            op,
            Box::new(lhs),
            Box::new(parse_operation_l1(token_stream)),
        );
    }
    lhs
}
fn parse_operation_l1(token_stream: &mut VecDeque<Token>) -> Expression {
    parse_operation_l0(token_stream)
}

fn parse_operation_l0(token_stream: &mut VecDeque<Token>) -> Expression {
    if matches!(token_stream.front(), Some(Token::Integer(_))) {
        parse_literal(token_stream)
    } else if matches!(token_stream.front(), Some(Token::Ident(_))) {
        let id = parse_ident(token_stream);
        if matches!(token_stream.front(), Some(Token::LParen)) {
            token_stream.pop_front();
            let call_args =
                if !matches!(token_stream.front(), Some(Token::RParen)) {
                    parse_call_args(token_stream)
                } else {
                    Vec::new()
                };
            parse_rparen(token_stream);
            Expression::Call(id, call_args)
        } else {
            Expression::Var(id)
        }
    } else if matches!(token_stream.front(), Some(Token::Exit)) {
        token_stream.pop_front();
        Expression::Exit
    } else if matches!(token_stream.front(), Some(Token::LParen)) {
        token_stream.pop_front();
        let e = parse_expression(token_stream);
        parse_rparen(token_stream);
        e
    } else {
        Expression::Nothing
    }
}

fn parse_call_args(token_stream: &mut VecDeque<Token>) -> Vec<Expression> {
    let mut vec = vec![parse_expression(token_stream)];
    while matches!(token_stream.front(), Some(Token::Comma)) {
        token_stream.pop_front();
        vec.push(parse_expression(token_stream));
    }
    vec
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

fn parse_literal(token_stream: &mut VecDeque<Token>) -> Expression {
    Expression::Literal(parse_integer(token_stream))
}

fn parse_colon(token_stream: &mut VecDeque<Token>) {
    if !matches!(token_stream.pop_front(), Some(Token::Collon)) {
        panic!("Syntax error: Expected ':'");
    };
}

fn parse_semicolon(token_stream: &mut VecDeque<Token>) {
    match token_stream.pop_front() {
        Some(Token::Semicolon) => {}
        e => panic!("Syntax error: Expected ';' found '{:?}'", e),
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

fn parse_lparen(token_stream: &mut VecDeque<Token>) {
    if !matches!(token_stream.pop_front(), Some(Token::LParen)) {
        panic!("Syntax error: Expected '('");
    };
}

fn parse_rparen(token_stream: &mut VecDeque<Token>) {
    if !matches!(token_stream.pop_front(), Some(Token::RParen)) {
        panic!("Syntax error: Expected ')'");
    };
}
