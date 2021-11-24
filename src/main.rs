use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::BasicTypeEnum;
use inkwell::values::BasicValue;
use inkwell::values::BasicValueEnum;
use inkwell::values::FloatValue;
use inkwell::values::FunctionValue;
use logos::{Lexer, Logos};
use std::collections::HashMap;
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
    Number(f64),
    Variable(String),
    Binary(String, Box<Expression>, Box<Expression>),
    Call(String, Vec<Expression>),
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
            Token::Number(val) => Some(Expression::Number(val)),

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

                    return Some(Expression::Call(name, args));
                } else {
                    Some(Expression::Variable(name))
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
        _ => unreachable!(),
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

        result = Expression::Binary(operator, Box::new(result), Box::new(rhs));
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

struct Generator<'ctx> {
    // Holds type and constant value tables
    context: &'ctx Context,

    // Contains functions and global variables
    module: Module<'ctx>,

    // Keeps track of the current place to insert instructions
    builder: Builder<'ctx>,

    variables: HashMap<String, BasicValueEnum<'ctx>>,
}

impl<'ctx> Generator<'ctx> {
    fn generate_expression(&self, expression: &Expression) -> FloatValue<'ctx> {
        match *expression {
            Expression::Number(val) => self.context.f64_type().const_float(val),

            Expression::Variable(ref name) => match self.variables.get(name.as_str()) {
                Some(value) => value.into_float_value(),
                None => panic!("Could not find variable {}", name),
            },

            Expression::Binary(ref op, ref lhs, ref rhs) => {
                let l = self.generate_expression(&lhs);
                let r = self.generate_expression(&rhs);

                match op.as_str() {
                    "+" => self.builder.build_float_add(l, r, "tmpadd"),
                    "-" => self.builder.build_float_sub(l, r, "tmpsub"),
                    "*" => self.builder.build_float_mul(l, r, "tmpmul"),
                    "<" => {
                        let int = self.builder.build_float_compare(
                            inkwell::FloatPredicate::ULT,
                            l,
                            r,
                            "tmpcmp",
                        );
                        self.builder.build_unsigned_int_to_float(
                            int,
                            self.context.f64_type(),
                            "tmpbool",
                        )
                    }
                    _ => panic!("{} is not a binary operator", op),
                }
            }

            Expression::Call(ref name, ref args) => match self.module.get_function(&name) {
                Some(function) => {
                    let mut generated_args: Vec<BasicValueEnum> = Vec::with_capacity(args.len());

                    for arg in args {
                        generated_args.push(self.generate_expression(arg).into());
                    }

                    match self
                        .builder
                        .build_call(function, &generated_args, "tmp")
                        .try_as_basic_value()
                        .left()
                    {
                        Some(val) => val.into_float_value(),
                        None => panic!("Can't build function {}", name),
                    }
                }
                None => panic!("Can't find function {}", name),
            },
        }
    }

    fn generate_prototype(&self, prototype: &Prototype) -> FunctionValue<'ctx> {
        // Make the function type
        let float_type = self.context.f64_type();
        let param_types: Vec<BasicTypeEnum> = vec![float_type.into(); prototype.parameters.len()];
        let param_types = param_types.as_slice();
        let function_type = self.context.f64_type().fn_type(param_types, false);

        let function = self
            .module
            .add_function(prototype.name.as_str(), function_type, None);

        // Set names for the parameters
        for (i, param) in function.get_param_iter().enumerate() {
            param
                .into_float_value()
                .set_name(prototype.parameters[i].as_str());
        }

        function
    }

    fn generate_function(&mut self, function: Function) -> FunctionValue<'ctx> {
        let prototype = function.prototype;
        let function_val = self.generate_prototype(&prototype);

        let basic_block = self.context.append_basic_block(function_val, "entry");

        self.builder.position_at_end(basic_block);

        // Record the function arguments in the variables map
        self.variables.clear();
        self.variables.reserve(prototype.parameters.len());

        for (i, param) in function_val.get_param_iter().enumerate() {
            let name = prototype.parameters[i].clone();
            self.variables.insert(name, param);
        }

        let return_val = self.generate_expression(&function.body);

        self.builder.build_return(Some(&return_val));

        if function_val.verify(true) {
            function_val
        } else {
            panic!("Invalid function")
        }
    }

    fn generate(ast: Function) {
        let context = &Context::create();
        let module = context.create_module("repl");
        let builder = context.create_builder();
        let variables = HashMap::new();

        let mut generator = Generator {
            context,
            module,
            builder,
            variables,
        };

        let function = generator.generate_function(ast);

        function.print_to_stderr();
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

        let ast = parse(&input).unwrap();

        println!("-> Lexer Output:");
        println!("{:?}", Token::lexer(&input).collect::<Vec<Token>>());

        println!("-> Parser Ouput:");
        println!("{:?}", ast);

        println!("-> Compiler Output:");
        Generator::generate(ast);
    }
}
