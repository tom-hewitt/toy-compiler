use logos::{Lexer, Logos};
use std::io::{self, Write};
use std::iter::Peekable;

#[derive(Logos, Debug, PartialEq)]
enum Token {
    #[token("def")]
    Def,

    #[token("extern")]
    Extern,

    #[regex(r"\p{Alphabetic}\w*", |lex| lex.slice().to_string())]
    Identifier(String),

    #[regex(r"\d+\.?\d*", |lex| lex.slice().parse())]
    Number(f64),

    #[token("(")]
    OpeningParenthesis,

    #[token(")")]
    ClosingParenthesis,

    #[token(",")]
    Comma,

    #[regex(r"<|\+|-|\*", |lex| lex.slice().to_string())]
    BinaryOp(String),

    // Skip whitespace
    #[error]
    #[regex(r"[ \t\n\f]+", logos::skip)]
    Error,
}

#[derive(PartialEq, Clone, Debug)]
enum Expression {
    NumberExpr(f64),
    VariableExpr(String),
    BinaryExpr(String, Box<Expression>, Box<Expression>),
    CallExpr(String, Vec<Expression>),
}

#[derive(PartialEq, Clone, Debug)]
struct Prototype {
    name: String,
    parameters: Vec<String>,
}

#[derive(PartialEq, Clone, Debug)]
struct Function {
    prototype: Prototype,
    body: Expression,
}

fn parse_primary(lex: &mut Peekable<Lexer<Token>>) -> Option<Expression> {
    match lex.next() {
        Some(token) => match token {
            Token::Number(val) => Some(Expression::NumberExpr(val)),
            Token::OpeningParenthesis => {
                let v = parse_expr(lex);
                match v {
                    Some(expression) => match lex.next() {
                        Some(Token::ClosingParenthesis) => Some(expression),
                        _ => panic!("Expected ')'"),
                    },
                    None => panic!("Expected expression after '('"),
                }
            }
            Token::Identifier(name) => {
                if lex.peek() == Some(&Token::OpeningParenthesis) {
                    let mut args: Vec<Expression> = vec![];

                    // Eat the '('
                    lex.next();

                    if lex.peek() != Some(&Token::ClosingParenthesis) {
                        loop {
                            match parse_expr(lex) {
                                Some(expression) => args.push(expression),
                                None => panic!("Expected ')' or an expression in function call"),
                            }

                            if lex.peek() == Some(&Token::ClosingParenthesis) {
                                break;
                            }

                            if lex.peek() != Some(&Token::Comma) {
                                panic!("Expected ',' or ')' in function call")
                            }
                        }
                    };

                    // Eat the ')'
                    lex.next();

                    return Some(Expression::CallExpr(name, args));
                } else {
                    Some(Expression::VariableExpr(name))
                }
            }
            _ => panic!("Unimplemented"),
        },
        None => None,
    }
}

fn get_binary_op_precedence(op: &str) -> i32 {
    match op {
        "<" => 10,
        "+" => 20,
        "-" => 20,
        "*" => 40,
        s => panic!("{} is not an operator", s),
    }
}

fn parse_binary_op<'a>(
    lex: &mut Peekable<Lexer<Token>>,
    expression_precedence: i32,
    lhs: &Expression,
) -> Expression {
    // Start with lhs value
    let mut result = lhs.clone();

    // Loop until there is not another operator, or there is a less important operator
    loop {
        let (operator, precedence) = match lex.peek() {
            Some(&Token::BinaryOp(ref op)) => match get_binary_op_precedence(op) {
                next_precedence if next_precedence >= expression_precedence => {
                    (op.clone(), next_precedence)
                }
                _ => break,
            },
            _ => break,
        };

        // Eat the operator
        lex.next();

        // Parse the primary expression after the operator
        let mut rhs = match parse_primary(lex) {
            Some(expression) => expression,
            None => panic!("Expected expression after operator"),
        };

        // Merge all expressions until there is a more important one, then give the expression as the lhs
        loop {
            let binary_rhs = match lex.peek() {
                Some(&Token::BinaryOp(ref op)) => match get_binary_op_precedence(op) {
                    next_precedence if next_precedence > precedence => {
                        parse_binary_op(lex, next_precedence, &rhs)
                    }
                    _ => break,
                },
                _ => break,
            };

            rhs = binary_rhs;
        }

        result = Expression::BinaryExpr(operator, Box::new(result), Box::new(rhs));
    }

    result
}

fn parse_expr(lex: &mut Peekable<Lexer<Token>>) -> Option<Expression> {
    match parse_primary(lex) {
        Some(lhs) => Some(parse_binary_op(lex, 0, &lhs)),
        None => None,
    }
}

fn parse_prototype(lex: &mut Peekable<Lexer<Token>>) -> Prototype {
    let name = match lex.next() {
        Some(Token::Identifier(name)) => name,
        _ => panic!("Expected function name after 'def'"),
    };

    let mut parameters: Vec<String> = vec![];

    match lex.next() {
        Some(Token::OpeningParenthesis) => match lex.peek() {
            Some(&Token::ClosingParenthesis) => {
                lex.next();
            }
            _ => loop {
                match lex.next() {
                    Some(Token::Identifier(name)) => parameters.push(name),
                    _ => panic!("Expected parameter name in function definition"),
                }

                match lex.next() {
                    Some(Token::ClosingParenthesis) => break,
                    Some(Token::Comma) => (),
                    _ => panic!("Expected ',' or ')' after parameter name"),
                }
            },
        },
        _ => panic!("Expected '(' after function name"),
    };

    Prototype { name, parameters }
}

fn parse_definition(lex: &mut Peekable<Lexer<Token>>) -> Function {
    // Eat 'def'
    lex.next();

    let prototype = parse_prototype(lex);

    let body = parse_expr(lex).expect("Expected function body after function definition");

    Function { prototype, body }
}

fn parse_top_level_expression(lex: &mut Peekable<Lexer<Token>>) -> Option<Function> {
    match parse_expr(lex) {
        Some(body) => Some(Function {
            prototype: Prototype {
                name: "".to_string(),
                parameters: vec![],
            },
            body,
        }),
        None => None,
    }
}

fn parse(input: &String) -> Option<Function> {
    let mut lex = Token::lexer(&input).peekable();

    match lex.peek() {
        Some(Token::Def) => Some(parse_definition(&mut lex)),
        _ => parse_top_level_expression(&mut lex),
    }
}

fn main() {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut input = String::new();

    loop {
        print!("> ");
        stdout.flush().unwrap();
        input.clear();
        stdin
            .read_line(&mut input)
            .ok()
            .expect("Failed to read line");

        println!("{:?}", parse(&input).unwrap());
    }
}
