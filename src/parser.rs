use std::{
    collections::HashMap,
    io::{Error, ErrorKind},
    marker::PhantomData,
};

use once_cell::sync::Lazy;

use crate::{
    ast::{self, Expression, Statement},
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
    m.insert(token::LPAREN, Precedence::CALL);
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
        parser.register_prefix(token::FUNCTION, Parser::parse_function_literal);
        parser.register_prefix(token::BANG, Parser::parse_prefix_expression);
        parser.register_prefix(token::MINUS, Parser::parse_prefix_expression);
        parser.register_prefix(token::TRUE, Parser::parse_boolean);
        parser.register_prefix(token::FALSE, Parser::parse_boolean);
        parser.register_prefix(token::LPAREN, Parser::parse_grouped_expression);
        parser.register_prefix(token::IF, Parser::parse_if_expression);
        parser.register_infix(token::PLUS, Parser::parse_infix_expression);
        parser.register_infix(token::MINUS, Parser::parse_infix_expression);
        parser.register_infix(token::SLASH, Parser::parse_infix_expression);
        parser.register_infix(token::ASTERISK, Parser::parse_infix_expression);
        parser.register_infix(token::EQ, Parser::parse_infix_expression);
        parser.register_infix(token::NOT_EQ, Parser::parse_infix_expression);
        parser.register_infix(token::LT, Parser::parse_infix_expression);
        parser.register_infix(token::GT, Parser::parse_infix_expression);
        parser.register_infix(token::LPAREN, Parser::parse_call_expression);

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
            Some(prefix) => *prefix,
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

    fn parse_grouped_expression(&mut self) -> ast::Expression {
        self.next_token();

        let exp = self.parse_expression(Precedence::LOWEST);
        if !self.expect_peek(token::RPAREN) {}

        return exp;
    }

    fn parse_if_expression(&mut self) -> ast::Expression {
        if !self.expect_peek(token::LPAREN) {
            self.errors.push(MError::ParseError(format!(
                "no left parenthesis after if: {:?}",
                self.current_token
            )));
        }

        self.next_token();
        let condition = Box::new(self.parse_expression(Precedence::LOWEST));

        if !self.expect_peek(token::RPAREN) {
            self.errors.push(MError::ParseError(format!(
                "Expectted next token to be {}, got={:?}",
                token::RPAREN,
                self.current_token
            )));
        }

        if !self.expect_peek(token::LBRACE) {
            self.errors.push(MError::ParseError(format!(
                "Expected next token to be {}, got={:?}",
                token::LBRACE,
                self.current_token
            )));
        }

        let consequence = Box::new(self.parse_block_statement());

        if self.peek_token_is(token::ELSE) {
            self.next_token();

            if !self.peek_token_is(token::LBRACE) {
                self.errors.push(MError::ParseError(format!(
                    "Expected next token to be {}, got={:?}",
                    token::LBRACE,
                    self.current_token
                )));
            }
            self.next_token();

            let alternative = Some(Box::new(self.parse_block_statement()));
            return Expression::If {
                condition,
                consequence,
                alternative,
            };
        }

        Expression::If {
            condition,
            consequence,
            alternative: None,
        }
    }

    fn parse_call_expression(&mut self, function: ast::Expression) -> ast::Expression {
        let arguments = self.parse_call_arguments();

        return ast::Expression::Call {
            function: Box::new(function),
            arguments,
        };
    }

    fn parse_call_arguments(&mut self) -> Vec<ast::Expression> {
        let mut arguments = vec![];

        if self.peek_token_is(token::RPAREN) {
            self.next_token();
            return arguments;
        }

        self.next_token();
        arguments.push(self.parse_expression(Precedence::LOWEST));

        while self.peek_token_is(token::COMMA) {
            self.next_token();
            self.next_token();
            arguments.push(self.parse_expression(Precedence::LOWEST));
        }

        if !self.expect_peek(token::RPAREN) {
            self.errors.push(MError::ParseError(format!(
                "Expected next token to be {}, got={:?}",
                token::RPAREN,
                self.current_token
            )));
        }

        return arguments;
    }

    fn parse_identifier(&mut self) -> ast::Expression {
        return ast::Expression::Identifier {
            value: self.current_token.literal.clone(),
        };
    }

    fn parse_boolean(&mut self) -> ast::Expression {
        return ast::Expression::Boolean {
            value: self.current_token.literal.clone() == "true",
        };
    }

    fn parse_integer_literal(&mut self) -> ast::Expression {
        let token = self.current_token.clone();
        let value = i64::from_str_radix(&token.literal, 10);

        let value = match value {
            Ok(value) => value,
            Err(_) => {
                self.errors.push(MError::ParseError(format!(
                    "could not parse {} as integer",
                    token.literal
                )));
                return ast::Expression::Integer { value: 0 };
            }
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
        let value = self.parse_expression(Precedence::LOWEST);

        if self.peek_token_is(token::SEMICOLON) {
            self.next_token();
        }

        ast::Statement::Let { name, value }
    }

    fn parse_return_statement(&mut self) -> ast::Statement {
        self.next_token();

        let value = self.parse_expression(Precedence::LOWEST);

        if self.peek_token_is(token::SEMICOLON) {
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

    fn parse_block_statement(&mut self) -> ast::Statement {
        let mut statements = vec![];

        self.next_token();

        while !self.current_token_is(token::RBRACE) && !self.current_token_is(token::EOF) {
            statements.push(self.parse_statement());
            self.next_token();
        }

        return ast::Statement::Block { statements };
    }

    fn parse_function_literal(&mut self) -> ast::Expression {
        if !self.expect_peek(token::LPAREN) {
            self.errors.push(MError::ParseError(format!(
                "expected next token to be {}, got={:?}",
                token::LPAREN,
                self.current_token
            )));
        }

        let parameters = self.parse_function_prameters();

        if !self.expect_peek(token::LBRACE) {
            self.errors.push(MError::ParseError(format!(
                "expected next token to be {}, got={:?}",
                token::LBRACE,
                self.current_token
            )));
        }

        let body = Box::new(self.parse_block_statement());

        return Expression::Function { parameters, body };
    }

    /// 引数をパースする
    /// ( <identifier>, <identifier>, ... )
    fn parse_function_prameters(&mut self) -> Vec<Expression> {
        let mut identifiers = vec![];

        // (
        if self.peek_token_is(token::RPAREN) {
            self.next_token();
            return identifiers;
        }

        self.next_token();

        identifiers.push(ast::Expression::Identifier {
            value: self.current_token.literal.clone(),
        });

        while self.peek_token_is(token::COMMA) {
            self.next_token();
            self.next_token();
            identifiers.push(ast::Expression::Identifier {
                value: self.current_token.literal.clone(),
            });
        }

        // )
        if !self.expect_peek(token::RPAREN) {
            self.errors.push(MError::ParseError(format!(
                "expected next token to be {}, got={:?}",
                token::RPAREN,
                self.current_token
            )));
        }

        return identifiers;
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
            self.errors.push(MError::ParseError(format!(
                "expected next token to be {}",
                token_type
            )));
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

    use std::fmt::Display;

    use super::*;
    use crate::{
        ast::{Expression, Statement},
        lexer::Lexer,
    };

    #[test]
    fn test_let_statements() {
        let inputs = vec![
            (
                "let x = 5;",
                "x",
                Expression::Identifier {
                    value: 5.to_string(),
                },
            ),
            (
                "let y = true;",
                "y",
                Expression::Identifier {
                    value: true.to_string(),
                },
            ),
            (
                "let foobar = y;",
                "foobar",
                Expression::Identifier {
                    value: "y".to_string(),
                },
            ),
        ];

        for (input, ident, value) in inputs {
            let mut l = Lexer::new(input);
            let mut p = Parser::new(&mut l);
            let program = p.parse_program();
            if p.errors.len() > 0 {
                for e in p.errors {
                    println!("{}", e);
                }
            }

            match &program.statements[0] {
                Statement::Let {
                    name: i, value: v, ..
                } => {
                    println!("name:{}", i);
                    println!("value:{}", v);
                    match i {
                        Expression::Identifier { value: n } => assert_eq!(n, ident),
                        _ => panic!("not identifier"),
                    }
                    match v {
                        Expression::Integer { value: n } => {
                            assert_eq!(
                                n,
                                &i64::from_str_radix(value.to_string().as_str(), 10).unwrap()
                            )
                        }
                        Expression::Boolean { value: n } => {
                            assert_eq!(n, &value.to_string().parse::<bool>().unwrap())
                        }
                        Expression::Identifier { value: n } => {
                            assert_eq!(n, &value.to_string())
                        }
                        _ => panic!("not identifier"),
                    }
                }
                _ => panic!("not let statement"),
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
    fn test_if_expression() {
        let input = r#"if (x < y) { x };"#;

        let mut lex = Lexer::new(input);
        let mut parser = Parser::new(&mut lex);
        let program = parser.parse_program();
        if parser.errors.len() > 0 {
            for e in parser.errors.iter() {
                println!("{}", e);
            }
        }

        match &program.statements[0] {
            Statement::Expression { expression } => {
                match expression {
                    Expression::If {
                        condition,
                        consequence,
                        alternative,
                    } => {
                        println!("condition {:?}", condition);
                        println!("consequence {:?}", consequence);
                        println!("alternative {:?}", alternative);

                        if !test_infix_expression(
                            condition,
                            &Expression::Identifier {
                                value: "x".to_string(),
                            },
                            &"<".to_string(),
                            &Expression::Identifier {
                                value: "y".to_string(),
                            },
                        ) {
                            panic!("Failed to parse if condition");
                        }
                        match consequence.as_ref() {
                            Statement::Block { statements } => {
                                if statements.len() != 1 {
                                    panic!("Failed to parse if consequence: {:?}", statements);
                                }
                                match &statements[0] {
                                    Statement::Expression { expression } => {
                                        match expression {
                                            Expression::Identifier { value } => {
                                                if value != "x" {
                                                    panic!(
                                                        "Failed to parse if consequence: {}",
                                                        expression
                                                    );
                                                }
                                            }
                                            _ => {
                                                panic!(
                                                    "Failed to parse if consequence: {}",
                                                    expression
                                                );
                                            }
                                        }
                                    }
                                    _ => {
                                        panic!("Failed to parse if consequence: {:?}", statements);
                                    }
                                }
                            }
                            _ => {
                                panic!("Failed to parse if consequence: {}", expression);
                            }
                        }
                        match alternative {
                            Some(alter) => {
                                panic!("alternative statements was not None. got={:?}", alter);
                            }
                            None => return,
                        }
                    }
                    _ => {
                        panic!("Expected if expression. got={:?}", expression)
                    }
                }
            }
            _ => {
                panic!(
                    "Expected expression statement. got={:?}",
                    program.statements[0]
                )
            }
        }
    }
    #[test]
    fn test_else_expression() {
        let input = r#"if (x < y) { x } else { y };"#;

        let mut lex = Lexer::new(input);
        let mut parser = Parser::new(&mut lex);
        let program = parser.parse_program();
        if parser.errors.len() > 0 {
            for e in parser.errors.iter() {
                println!("{}", e);
            }
        }

        match &program.statements[0] {
            Statement::Expression { expression } => {
                match expression {
                    Expression::If {
                        condition,
                        consequence,
                        alternative,
                    } => {
                        println!("condition {:?}", condition);
                        println!("consequence {:?}", consequence);
                        println!("alternative {:?}", alternative);

                        if !test_infix_expression(
                            condition,
                            &Expression::Identifier {
                                value: "x".to_string(),
                            },
                            &"<".to_string(),
                            &Expression::Identifier {
                                value: "y".to_string(),
                            },
                        ) {
                            panic!("Failed to parse if condition");
                        }
                        match consequence.as_ref() {
                            Statement::Block { statements } => {
                                if statements.len() != 1 {
                                    panic!("Failed to parse if consequence: {:?}", statements);
                                }
                                match &statements[0] {
                                    Statement::Expression { expression } => {
                                        match expression {
                                            Expression::Identifier { value } => {
                                                if value != "x" {
                                                    panic!(
                                                        "Failed to parse if consequence: {}",
                                                        expression
                                                    );
                                                }
                                            }
                                            _ => {
                                                panic!(
                                                    "Failed to parse if consequence: {}",
                                                    expression
                                                );
                                            }
                                        }
                                    }
                                    _ => {
                                        panic!("Failed to parse if consequence: {:?}", statements);
                                    }
                                }
                            }
                            _ => {
                                panic!("Failed to parse if consequence: {}", expression);
                            }
                        }
                        match alternative {
                            Some(alter) => {
                                match alter.as_ref() {
                                    Statement::Block { statements } => {
                                        if statements.len() != 1 {
                                            panic!(
                                                "Failed to parse if alternative: {:?}",
                                                statements
                                            );
                                        }

                                        match &statements[0] {
                                            Statement::Expression { expression } => {
                                                match expression {
                                                    Expression::Identifier { value } => {
                                                        if value != "y" {
                                                            panic!(
                                                                "Failed to parse if alternative: {}",
                                                                expression
                                                            );
                                                        }
                                                    }
                                                    _ => {
                                                        panic!(
                                                            "Failed to parse if alternative: {}",
                                                            expression
                                                        );
                                                    }
                                                }
                                            }
                                            _ => {
                                                panic!(
                                                    "Failed to parse if alternative: {:?}",
                                                    statements
                                                );
                                            }
                                        }
                                    }
                                    _ => {
                                        panic!("Failed to parse if alternative: {:?}", alter);
                                    }
                                }
                            }
                            None => {
                                panic!("alternative statements was None. got={:?}", alternative)
                            }
                        }
                    }
                    _ => {
                        panic!("Expected if expression. got={:?}", expression)
                    }
                }
            }
            _ => {
                panic!(
                    "Expected expression statement. got={:?}",
                    program.statements[0]
                )
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
            _ => panic!("Expected expression. got={:?}", statement),
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
                    _ => panic!("Expected integer. got={:?}", expression),
                }
            }
            _ => panic!("Expected expression. got={:?}", statement),
        }
    }

    #[test]
    fn test_parsing_prefix_expressions() {
        let inputs = vec![
            ("!5;", "!", Expression::Integer { value: 5 }),
            ("-15;", "-", Expression::Integer { value: 15 }),
            ("!true;", "!", Expression::Boolean { value: true }),
            ("!false;", "!", Expression::Boolean { value: false }),
        ];

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
                                    if i_value.to_string() != value.to_string() {
                                        panic!("Expected value={}. got={}", i_value, value);
                                    }
                                }
                                Expression::Boolean { value: b_value } => {
                                    if b_value.to_string() != value.to_string() {
                                        panic!("Expected value={}. got={}", b_value, value);
                                    }
                                }
                                _ => panic!("Expected integer. got={:?}", right),
                            }
                        }
                        _ => panic!("Expected prefix expression. got={:?}", expression),
                    }
                }
                _ => panic!("Expected expression. got={:?}", statement),
            }
        }
    }

    #[test]
    fn test_parsing_infix_expression() {
        let inputs = vec![
            (
                "5 + 5;",
                Expression::Integer { value: 5 },
                "+",
                Expression::Integer { value: 5 },
            ),
            (
                "5 - 5;",
                Expression::Integer { value: 5 },
                "-",
                Expression::Integer { value: 5 },
            ),
            (
                "5 * 5;",
                Expression::Integer { value: 5 },
                "*",
                Expression::Integer { value: 5 },
            ),
            (
                "5 / 5;",
                Expression::Integer { value: 5 },
                "/",
                Expression::Integer { value: 5 },
            ),
            (
                "5 > 5;",
                Expression::Integer { value: 5 },
                ">",
                Expression::Integer { value: 5 },
            ),
            (
                "5 < 5;",
                Expression::Integer { value: 5 },
                "<",
                Expression::Integer { value: 5 },
            ),
            (
                "5 == 5;",
                Expression::Integer { value: 5 },
                "==",
                Expression::Integer { value: 5 },
            ),
            (
                "5 != 5;",
                Expression::Integer { value: 5 },
                "!=",
                Expression::Integer { value: 5 },
            ),
            (
                "true == true",
                Expression::Boolean { value: true },
                "==",
                Expression::Boolean { value: true },
            ),
            (
                "true != false",
                Expression::Boolean { value: true },
                "!=",
                Expression::Boolean { value: false },
            ),
            (
                "false == false",
                Expression::Boolean { value: false },
                "==",
                Expression::Boolean { value: false },
            ),
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
                    if !test_infix_expression(expression, &left, &operator.to_string(), &right) {
                        panic!("Expected infix expression. got={:?}", expression);
                    }
                }
                _ => panic!("Expected identifier. got={:?}", statement),
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
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
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
            assert_eq!(actual, expected);
        }
        return;
    }

    #[test]
    fn test_function_literal_parsing() {
        let input = "fn(x, y) { x + y; }";

        let mut lex = Lexer::new(input);
        let mut parser = Parser::new(&mut lex);
        let program = parser.parse_program();

        if parser.errors.len() > 0 {
            for error in parser.errors {
                println!("{}", error);
            }
        }

        let statement = &program.statements[0];

        let function = match statement {
            Statement::Expression { expression } => {
                match expression {
                    Expression::Function { parameters, body } => (parameters, body),
                    _ => panic!("Expected function. got={:?}", expression),
                }
            }
            _ => panic!("Expected expression. got={:?}", statement),
        };

        if function.0.len() != 2 {
            panic!("Expected 2 parameters. got={}", function.0.len());
        }

        test_literal_expression(function.0.get(0).unwrap(), "x".to_string());
        test_literal_expression(function.0.get(1).unwrap(), "y".to_string());

        let body = &**function.1;
        match body {
            Statement::Block { statements } => {
                if statements.len() != 1 {
                    panic!("Expected 1 statement. got={}", statements.len());
                }
                let statement = statements.get(0).unwrap();
                match statement {
                    Statement::Expression { expression } => {
                        if !test_infix_expression(
                            expression,
                            &Expression::Identifier {
                                value: "x".to_string(),
                            },
                            &"+".to_string(),
                            &Expression::Identifier {
                                value: "y".to_string(),
                            },
                        ) {
                            panic!("Expected infix expression. got={:?}", expression);
                        }
                    }
                    _ => panic!("Expected identifier. got={:?}", statement),
                }
            }
            _ => panic!("Expected block. got={:?}", body),
        }
    }

    fn test_literal_expression(exp: &ast::Expression, value: String) -> bool {
        match exp {
            Expression::Integer { value: i_value } => {
                if i_value.to_string() != value {
                    println!("exp not Expression::Integer. got={:?}", exp);
                    return false;
                }
                return true;
            }
            Expression::Boolean { value: b_value } => {
                if b_value.to_string() != value {
                    println!("exp not Expression::Boolean. got={:?}", exp);
                    return false;
                }
                return true;
            }
            Expression::Identifier { value: i_value } => {
                if i_value != &value {
                    println!("exp not Expression::Identifier got={:?}", exp);
                    return false;
                }
                return true;
            }
            _ => return false,
        }
    }

    #[test]
    fn test_function_parameters_parsing() {
        let inputs = vec![
            ("fn() {};", vec![]),
            ("fn(x) {};", vec!["x".to_string()]),
            (
                "fn(x, y, z) {};",
                vec!["x".to_string(), "y".to_string(), "z".to_string()],
            ),
        ];

        for (e, a) in inputs {
            let mut lex = Lexer::new(e);
            let mut parser = Parser::new(&mut lex);
            let program = parser.parse_program();

            if parser.errors.len() > 0 {
                println!("{:?}", parser.errors);
            }

            let statement = &program.statements[0];

            let function = match statement {
                Statement::Expression { expression } => {
                    match expression {
                        Expression::Function { parameters, body } => (parameters, body),
                        _ => panic!("Expected function. got={:?}", expression),
                    }
                }
                _ => panic!("Expected expression. got={:?}", statement),
            };

            let actual = function
                .0
                .iter()
                .map(|p| p.to_string())
                .collect::<Vec<String>>();
            assert_eq!(actual, a);
        }
    }

    #[test]
    fn test_call_expression_parsing() {
        let input = "add(1, 2 * 3, 4 + 5);";

        let mut lex = Lexer::new(input);
        let mut parser = Parser::new(&mut lex);
        let program = parser.parse_program();

        if parser.errors.len() > 0 {
            for e in parser.errors {
                println!("{}", e);
            }
        }

        let statement = &program.statements[0];

        let exp = match statement {
            Statement::Expression { expression } => {
                match expression {
                    Expression::Call {
                        function,
                        arguments,
                    } => (function, arguments),
                    _ => panic!("Expected call. got={:?}", expression),
                }
            }
            _ => panic!("Expected expression. got={:?}", statement),
        };

        if !test_identifier(exp.0, "add") {
            panic!("Expected identifier. got={:?}", exp.0);
        }

        if exp.1.len() != 3 {
            panic!("Expected 3 arguments. got={}", exp.1.len());
        }

        test_literal_expression(exp.1.get(0).unwrap(), "1".to_string());
        test_infix_expression(
            exp.1.get(1).unwrap(),
            &Expression::Integer { value: 2 },
            &"*".to_string(),
            &Expression::Integer { value: 3 },
        );
        test_infix_expression(
            exp.1.get(2).unwrap(),
            &Expression::Integer { value: 4 },
            &"+".to_string(),
            &Expression::Integer { value: 5 },
        );
    }

    /// helper function of infix_expression
    fn test_infix_expression<T: Display>(
        exp: &ast::Expression,
        left: &T,
        operator: &String,
        right: &T,
    ) -> bool {
        match exp {
            ast::Expression::Infix {
                left: l,
                operator: op,
                right: r,
            } => {
                if !test_literal_expression(l.as_ref(), left.to_string()) {
                    return false;
                }
                if op != operator {
                    println!("exp::Operator is not {}, got={}", operator, op);
                    return false;
                }
                if !test_literal_expression(r.as_ref(), right.to_string()) {
                    return false;
                }
                return true;
            }
            _ => return false,
        };
    }

    fn test_identifier(exp: &ast::Expression, value: &str) -> bool {
        match exp {
            ast::Expression::Identifier { value: v } => {
                if v != value {
                    println!("exp::Identifier is not {}, got={}", value, v);
                    return false;
                }
                return true;
            }
            _ => return false,
        }
    }
}
