use std::{
    collections::HashMap,
    io::{Error, ErrorKind},
    marker::PhantomData,
};

use once_cell::sync::Lazy;

use crate::{
    ast::{self, Expression},
    error::MError,
    lexer,
    token::{self, Token, TokenType},
};

#[derive(PartialEq, PartialOrd)]
enum Precedence {
    LOWEST = 1,
    EQUALS,      // ==
    LESSGREATER, // > or <
    SUM,         // +
    PRODUCT,     // *
    PREFIX,      // -X or !X
    CALL,        // myFunction(X)
}

static PRECEDENCES: Lazy<HashMap<&'static str, Precedence>> = Lazy::new(|| {
    let mut m = HashMap::new();
    m.insert(token::EQ, Precedence::EQUALS);
    m.insert(token::NOT_EQ, Precedence::EQUALS);
    m.insert(token::LT, Precedence::LESSGREATER);
    m.insert(token::GT, Precedence::LESSGREATER);
    m.insert(token::PLUS, Precedence::SUM);
    m.insert(token::MINUS, Precedence::SUM);
    m.insert(token::SLASH, Precedence::PRODUCT);
    m.insert(token::ASTERISK, Precedence::PRODUCT);
    m
});
pub struct Parser<'a> {
    lexer: &'a mut lexer::Lexer<'a>,

    current_token: token::Token,
    peek_token:    token::Token,

    prefix_parse_fns: HashMap<TokenType, fn(&mut Self) -> ast::Expression>,
    infix_parse_fns:  HashMap<TokenType, fn(&mut Self, ast::Expression) -> ast::Expression>,

    pub errors: Vec<MError>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: &'a mut lexer::Lexer<'a>) -> Parser<'a> {
        let mut parser = Parser {
            lexer,
            current_token: Token::with_str(token::ILLEGAL, String::new()),
            peek_token: Token::with_str(token::ILLEGAL, String::new()),
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),

            errors: Vec::new(),
        };

        parser.register_prefix(token::IDENT, Parser::parse_identifier);
        parser.register_prefix(token::INT, Parser::parse_integer_literal);
        parser.register_prefix(token::BANG, Parser::parse_prefix_expression);
        parser.register_prefix(token::MINUS, Parser::parse_prefix_expression);
        parser.register_infix(token::PLUS, Parser::parse_infix_expression);
        parser.register_infix(token::MINUS, Parser::parse_infix_expression);
        parser.register_infix(token::SLASH, Parser::parse_infix_expression);
        parser.register_infix(token::ASTERISK, Parser::parse_infix_expression);
        parser.register_infix(token::EQ, Parser::parse_infix_expression);
        parser.register_infix(token::NOT_EQ, Parser::parse_infix_expression);
        parser.register_infix(token::LT, Parser::parse_infix_expression);
        parser.register_infix(token::GT, Parser::parse_infix_expression);

        parser.next_token();
        parser.next_token();
        return parser;
    }

    pub fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    pub fn parse_program(&mut self) -> ast::Program {
        let mut program = ast::Program { statements: vec![] };

        while self.current_token.type_ != token::EOF {
            let statement = self.parse_statement();
            program.statements.push(statement);
            self.next_token();
        }

        program
    }

    fn parse_statement(&mut self) -> ast::Statement {
        match self.current_token.type_ {
            token::LET => self.parse_let_statement(),
            token::RETURN => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_expression(&mut self, precedence: Precedence) -> ast::Expression {
        let prefix = self.prefix_parse_fns.get(&self.current_token.type_);

        let prefix = match prefix {
            Some(prefix) => prefix,
            None => {
                self.errors.push(MError::ParseError(format!(
                    "no prefix parse function for {:?}",
                    self.current_token.type_
                )));
                return ast::Expression::Identifier {
                    value: self.current_token.literal.clone(),
                };
            }
        };

        let mut left_expression = prefix(self);

        while !self.peek_token_is(token::SEMICOLON) && precedence < self.peek_precedence() {
            {
                let infix = self.infix_parse_fns.get(self.peek_token.type_.clone());
                if infix.is_none() {
                    return left_expression;
                }
            }
            self.next_token();
            let infix = self.infix_parse_fns.get(self.current_token.type_).unwrap();
            left_expression = infix(self, left_expression);
        }

        left_expression
    }

    fn parse_prefix_expression(&mut self) -> ast::Expression {
        let operator = self.current_token.literal.clone();

        self.next_token();

        let right = self.parse_expression(Precedence::PREFIX);

        ast::Expression::Prefix {
            operator,
            right: Box::new(right),
        }
    }

    fn parse_infix_expression(&mut self, left: ast::Expression) -> ast::Expression {
        let operator = self.current_token.literal.clone();
        let left = left;

        let precedence = self.current_precedence();
        self.next_token();
        let right = self.parse_expression(precedence);

        return ast::Expression::Infix {
            operator,
            left: Box::new(left),
            right: Box::new(right),
        };
    }

    fn parse_identifier(&mut self) -> ast::Expression {
        return ast::Expression::Identifier {
            value: self.current_token.literal.clone(),
        };
    }

    fn parse_integer_literal(&mut self) -> ast::Expression {
        let token = self.current_token.clone();
        let value = i64::from_str_radix(&token.literal, 10);

        let value = match value {
            Ok(value) => value,
            Err(_) => panic!("invalid integer literal"),
        };

        return ast::Expression::Integer { value };
    }

    /// let <identifier> = <expression>;
    fn parse_let_statement(&mut self) -> ast::Statement {
        if !self.expect_peek(token::IDENT) {
            self.errors
                .push(MError::ParseError("expected identifier".to_string()));
        }

        // <identifier>
        let name = ast::Expression::Identifier {
            value: self.current_token.literal.clone(),
        };

        // ASSIGNじゃない場合構文エラー
        if !self.expect_peek(token::ASSIGN) {
            self.errors
                .push(MError::ParseError("expected =".to_string()));
        };

        // =
        self.next_token();

        // <expression>
        let value = ast::Expression::Identifier {
            value: self.current_token.literal.clone(),
        };

        while !self.current_token_is(token::SEMICOLON) {
            self.next_token();
        }

        ast::Statement::Let { name, value }
    }

    fn parse_return_statement(&mut self) -> ast::Statement {
        self.next_token();

        let value = ast::Expression::Identifier {
            value: self.current_token.literal.clone(),
        };

        while !self.current_token_is(token::SEMICOLON) {
            self.next_token();
        }

        return ast::Statement::Return { value };
    }

    fn parse_expression_statement(&mut self) -> ast::Statement {
        let expression = self.parse_expression(Precedence::LOWEST);

        if self.peek_token_is(token::SEMICOLON) {
            self.next_token();
        }
        return ast::Statement::Expression { expression };
    }

    fn current_token_is(&self, token_type: TokenType) -> bool {
        self.current_token.type_ == token_type
    }
    fn peek_token_is(&self, token_type: TokenType) -> bool {
        self.peek_token.type_ == token_type
    }
    fn expect_peek(&mut self, token_type: TokenType) -> bool {
        if self.peek_token_is(token_type) {
            self.next_token();
            return true;
        } else {
            println!(
                "Expected next token to be {}, got {} instead",
                token_type, self.peek_token.type_
            );
            return false;
        }
    }

    fn peek_precedence(&mut self) -> Precedence {
        let p = PRECEDENCES.get(&self.peek_token.type_);
        let p = match p {
            Some(p) => p,
            None => &Precedence::LOWEST,
        };
        match p {
            Precedence::LOWEST => Precedence::LOWEST,
            Precedence::EQUALS => Precedence::EQUALS,
            Precedence::LESSGREATER => Precedence::LESSGREATER,
            Precedence::SUM => Precedence::SUM,
            Precedence::PRODUCT => Precedence::PRODUCT,
            Precedence::PREFIX => Precedence::PREFIX,
            Precedence::CALL => Precedence::CALL,
        }
    }

    fn current_precedence(&mut self) -> Precedence {
        let p = PRECEDENCES.get(&self.current_token.type_);
        let p = match p {
            Some(p) => p,
            None => &Precedence::LOWEST,
        };
        match p {
            Precedence::LOWEST => Precedence::LOWEST,
            Precedence::EQUALS => Precedence::EQUALS,
            Precedence::LESSGREATER => Precedence::LESSGREATER,
            Precedence::SUM => Precedence::SUM,
            Precedence::PRODUCT => Precedence::PRODUCT,
            Precedence::PREFIX => Precedence::PREFIX,
            Precedence::CALL => Precedence::CALL,
        }
    }

    fn register_prefix(&mut self, token_type: TokenType, fn_: fn(&mut Self) -> ast::Expression) {
        self.prefix_parse_fns.insert(token_type, fn_);
    }

    fn register_infix(
        &mut self,
        token_type: TokenType,
        fn_: fn(&mut Self, ast::Expression) -> ast::Expression,
    ) {
        self.infix_parse_fns.insert(token_type, fn_);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        ast::{Expression, Statement},
        lexer::Lexer,
    };

    #[test]
    fn test_let_statements() {
        let input = r#"
        let x = 5;
        let y = 10;
        let foobar = 838383;
        "#;

        let mut lex = Lexer::new(input);
        let mut parser = Parser::new(&mut lex);

        let program = parser.parse_program();
        if parser.errors.len() > 0 {
            println!("{:?}", parser.errors);
        }
        if program.statements.len() != 3 {
            panic!(
                "program.statements does not contain 3 statements. got={}",
                program.statements.len()
            );
        }

        let expected = vec![("x"), ("y"), ("foobar")];

        for (i, statement) in program.statements.iter().enumerate() {
            if !test_let_statement(statement, expected[i]) {
                panic!("Failed to parse let statement");
            };
        }
    }

    fn test_let_statement(statement: &Statement, expected: &str) -> bool {
        match statement {
            Statement::Let { name, .. } => {
                match name {
                    Expression::Identifier { value, .. } => {
                        return value == expected;
                    }
                    _ => {
                        return false;
                    }
                }
            }
            _ => return false,
        }
    }

    #[test]
    fn test_return_statement() {
        let input = r#"
        return 5;
        return 10;
        return 993322;
        "#;

        let mut lex = Lexer::new(input);
        let mut parser = Parser::new(&mut lex);

        let program = parser.parse_program();
        if parser.errors.len() > 0 {
            println!("{:?}", parser.errors);
        }

        if program.statements.len() != 3 {
            panic!(
                "program.statements does not contain 3 statements. got={}",
                program.statements.len()
            );
        }

        for (i, statement) in program.statements.iter().enumerate() {
            println!("{:?}", &statement);
            match statement {
                Statement::Return { value: _ } => {
                    continue;
                }
                _ => {
                    panic!("Expected return statement. got={:?}", statement)
                }
            }
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";

        let mut lex = Lexer::new(input);
        let mut parser = Parser::new(&mut lex);
        let program = parser.parse_program();
        if parser.errors.len() > 0 {
            println!("{:?}", parser.errors);
        }

        if &program.statements.len() != &1 {
            panic!(
                "program has not enough statements. got={}",
                program.statements.len()
            );
        };

        let statement = program.statements.get(0).unwrap();
        match statement {
            Statement::Expression { expression } => {
                match expression {
                    Expression::Identifier { value } => {
                        if value != "foobar" {
                            panic!("identifier is not foobar. got={}", value);
                        }
                    }
                    _ => panic!("Expected identifier. got={:?}", expression),
                }
            }
            Statement::Let { name: _, value: _ } => todo!(),
            Statement::Return { value: _ } => todo!(),
        };
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "5;";

        let mut lex = Lexer::new(input);
        let mut parser = Parser::new(&mut lex);
        let program = parser.parse_program();

        if parser.errors.len() > 0 {
            println!("{:?}", parser.errors);
        }

        if &program.statements.len() != &1 {
            panic!(
                "program has not enough statements. got={}",
                program.statements.len()
            );
        };

        let statement = program.statements.get(0).unwrap();
        match statement {
            Statement::Expression { expression } => {
                match expression {
                    Expression::Integer { value } => {
                        if *value != 5 as i64 {
                            panic!("identifier is not 5. got={}", value);
                        }
                    }
                    Expression::Identifier { value } => {
                        panic!("Expected identifier. got={:?}", value)
                    }
                    Expression::Prefix { operator, right } => {
                        panic!("Expected identifier. got=({}{})", operator, right)
                    }
                    Expression::Infix {
                        left,
                        operator,
                        right,
                    } => {
                        panic!("Expected identifier. got=({} {} {})", left, operator, right)
                    }
                }
            }
            Statement::Let { value, .. } => panic!("Expected identifier. got={:?}", value),
            Statement::Return { value } => panic!("Expected identifier. got={:?}", value),
        }
    }

    #[test]
    fn test_parsing_prefix_expressions() {
        let inputs = vec![("!5;", "!", 5), ("-15;", "-", 15)];

        for (input, operator, value) in inputs {
            let mut lex = Lexer::new(input);
            let mut parser = Parser::new(&mut lex);
            let program = parser.parse_program();

            if parser.errors.len() > 0 {
                println!("{:?}", parser.errors);
            }

            println!("{:?}", &program.statements);
            if &program.statements.len() != &1 {
                panic!(
                    "program has not enough statements. got={}",
                    program.statements.len()
                );
            }

            let statement = program.statements.get(0).unwrap();
            match statement {
                Statement::Expression { expression } => {
                    match expression {
                        Expression::Prefix {
                            operator: op,
                            right,
                        } => {
                            if op != operator {
                                panic!("Expected operator={}. got={}", operator, op);
                            }

                            let right = right.as_ref();
                            match right {
                                Expression::Integer { value: i_value } => {
                                    if i_value != &value {
                                        panic!("Expected value={}. got={}", i_value, value);
                                    }
                                }
                                _ => panic!("Expected integer. got={:?}", right),
                            }
                        }
                        _ => panic!("Expected prefix expression. got={:?}", expression),
                    }
                }
                Statement::Let { value, .. } => panic!("Expected identifier. got={:?}", value),
                Statement::Return { value } => {
                    panic!("Expected identifier. got={:?}", value)
                }
            }
        }
    }

    #[test]
    fn test_parsing_infix_expression() {
        let inputs = vec![
            ("5 + 5;", 5, "+", 5),
            ("5 - 5;", 5, "-", 5),
            ("5 * 5;", 5, "*", 5),
            ("5 / 5;", 5, "/", 5),
            ("5 > 5;", 5, ">", 5),
            ("5 < 5;", 5, "<", 5),
            ("5 == 5;", 5, "==", 5),
            ("5 != 5;", 5, "!=", 5),
        ];

        for (input, left, operator, right) in inputs {
            let mut lex = Lexer::new(input);
            let mut parser = Parser::new(&mut lex);
            let program = parser.parse_program();

            if parser.errors.len() > 0 {
                println!("{:?}", parser.errors);
            }

            println!("{:?}", &program.statements);
            if &program.statements.len() != &1 {
                panic!(
                    "program has not enough statements. got={}",
                    program.statements.len()
                );
            }

            let statement = program.statements.get(0).unwrap();
            match statement {
                Statement::Expression { expression } => {
                    match expression {
                        Expression::Infix {
                            left: l,
                            operator: op,
                            right: r,
                        } => {
                            if op != operator {
                                panic!("Expected operator={}. got={}", operator, op);
                            }

                            let l = l.as_ref();
                            let r = r.as_ref();
                            match l {
                                Expression::Integer { value: i_value } => {
                                    if i_value != &left {
                                        panic!("Expected value={}. got={}", i_value, left);
                                    }
                                }
                                _ => panic!("Expected integer. got={:?}", l),
                            }
                            match r {
                                Expression::Integer { value: i_value } => {
                                    if i_value != &right {
                                        panic!("Expected value={}. got={}", i_value, right);
                                    }
                                }
                                _ => panic!("Expected integer. got={:?}", r),
                            }
                        }
                        _ => panic!("Expected infix expression. got={:?}", expression),
                    }
                }
                Statement::Let { .. } => todo!(),
                Statement::Return { .. } => todo!(),
            }
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        let inputs = vec![
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
        ];

        for (input, expected) in inputs {
            let mut lex = Lexer::new(input);
            let mut parser = Parser::new(&mut lex);
            let program = parser.parse_program();

            if parser.errors.len() > 0 {
                println!("{:?}", parser.errors);
            }

            let actual = program.to_string();
            println!("{}", actual);
            assert_eq!(actual, expected);
        }
    }
}
