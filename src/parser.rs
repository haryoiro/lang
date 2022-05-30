use std::{
    collections::{HashMap, BTreeMap},
};

use once_cell::sync::Lazy;

use crate::{
    ast::{self, Expression, Literal},
    error::MError,
    lexer,
    token::{Token, TokenType, CodePosition},
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
    INDEX,       // array[index]
}

static PRECEDENCES: Lazy<HashMap<TokenType, Precedence>> = Lazy::new(|| {
    let mut m = HashMap::new();
    m.insert(TokenType::EQ, Precedence::EQUALS);
    m.insert(TokenType::NotEq, Precedence::EQUALS);
    m.insert(TokenType::LT, Precedence::LESSGREATER);
    m.insert(TokenType::GT, Precedence::LESSGREATER);
    m.insert(TokenType::PLUS, Precedence::SUM);
    m.insert(TokenType::MINUS, Precedence::SUM);
    m.insert(TokenType::SLASH, Precedence::PRODUCT);
    m.insert(TokenType::ASTERISK, Precedence::PRODUCT);
    m.insert(TokenType::LPAREN, Precedence::CALL);
    m.insert(TokenType::LBRACKET, Precedence::INDEX);
    m
});
pub struct Parser<'a> {
    lexer: &'a mut lexer::Lexer<'a>,

    current_token: Token,
    peek_token:    Token,

    prefix_parse_fns: HashMap<TokenType, fn(&mut Self) -> Expression>,
    infix_parse_fns:  HashMap<TokenType, fn(&mut Self, Expression) -> Expression>,

    pub errors: Vec<MError>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: &'a mut lexer::Lexer<'a>) -> Parser<'a> {
        let mut parser = Parser {
            lexer,
            current_token: Token::new(TokenType::ILLEGAL,CodePosition::default()),
            peek_token: Token::new(TokenType::ILLEGAL, CodePosition::default()),
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),

            errors: Vec::new(),
        };

        parser.register_prefix(TokenType::IDENT, Parser::parse_identifier);
        parser.register_prefix(TokenType::INT, Parser::parse_integer_literal);
        parser.register_prefix(TokenType::FUNCTION, Parser::parse_function_literal);
        parser.register_prefix(TokenType::BANG, Parser::parse_prefix_expression);
        parser.register_prefix(TokenType::MINUS, Parser::parse_prefix_expression);
        parser.register_prefix(TokenType::TRUE, Parser::parse_boolean);
        parser.register_prefix(TokenType::FALSE, Parser::parse_boolean);
        parser.register_prefix(TokenType::LPAREN, Parser::parse_grouped_expression);
        parser.register_prefix(TokenType::IF, Parser::parse_if_expression);
        parser.register_prefix(TokenType::STRING, Parser::parse_string_literal);
        parser.register_prefix(TokenType::LBRACKET, Parser::parse_array_literal);
        parser.register_prefix(TokenType::LBRACE, Parser::parse_hash_literal);
        parser.register_infix(TokenType::PLUS, Parser::parse_infix_expression);
        parser.register_infix(TokenType::MINUS, Parser::parse_infix_expression);
        parser.register_infix(TokenType::SLASH, Parser::parse_infix_expression);
        parser.register_infix(TokenType::ASTERISK, Parser::parse_infix_expression);
        parser.register_infix(TokenType::EQ, Parser::parse_infix_expression);
        parser.register_infix(TokenType::NotEq, Parser::parse_infix_expression);
        parser.register_infix(TokenType::LT, Parser::parse_infix_expression);
        parser.register_infix(TokenType::GT, Parser::parse_infix_expression);
        parser.register_infix(TokenType::LPAREN, Parser::parse_call_expression);
        parser.register_infix(TokenType::LBRACKET, Parser::parse_index_expression);

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

        while self.current_token.token_type != TokenType::EOF {
            let statement = self.parse_statement();
            program.statements.push(statement);
            self.next_token();
        }

        program
    }

    fn parse_statement(&mut self) -> ast::Statement {
        match self.current_token.token_type {
            TokenType::LET => self.parse_let_statement(),
            TokenType::RETURN => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Expression {
        let prefix = self.prefix_parse_fns.get(&self.current_token.token_type);
        let prefix = match prefix {
            Some(prefix) => *prefix,
            None => {
                self.errors.push(MError::ParseError(format!(
                    "no prefix parse function for {:?}",
                    self.current_token.token_type
                )));
                return Expression::Literal(Literal::Identifier(self.current_token.literal.clone().unwrap()))
            }
        };

        let mut left_expression = prefix(self);

        while !self.peek_token_is(&TokenType::SEMICOLON) && precedence < self.peek_precedence() {
            {
                let infix = self.infix_parse_fns.get(&self.peek_token.token_type.clone());
                if infix.is_none() {
                    return left_expression;
                }
            }
            self.next_token();
            let infix = self.infix_parse_fns.get(&self.current_token.token_type).unwrap();
            left_expression = infix(self, left_expression);
        }

        left_expression
    }

    fn parse_prefix_expression(&mut self) -> Expression {
        let operator = self.current_token.to_string();

        self.next_token();

        let right = self.parse_expression(Precedence::PREFIX);

        Expression::Prefix {
            operator,
            right: Box::new(right),
        }
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Expression {
        let operator = &self.current_token.to_string();
        let left = left;

        let precedence = self.current_precedence();
        self.next_token();
        let right = self.parse_expression(precedence);

        return Expression::Infix {
            operator: operator.clone(),
            left: Box::new(left),
            right: Box::new(right),
        };
    }

    fn parse_grouped_expression(&mut self) -> Expression {
        self.next_token();

        let exp = self.parse_expression(Precedence::LOWEST);
        if !self.expect_peek(&TokenType::RPAREN) {}

        return exp;
    }

    fn parse_if_expression(&mut self) -> Expression {
        if !self.expect_peek(&TokenType::LPAREN) {
            self.errors.push(MError::ParseError(format!(
                "no left parenthesis after if: {:?}",
                self.current_token
            )));
        }

        self.next_token();
        let condition = Box::new(self.parse_expression(Precedence::LOWEST));

        if !self.expect_peek(&TokenType::RPAREN) {
            self.errors.push(MError::ParseError(format!(
                "Expectted next token to be {}, got={:?}",
                TokenType::RPAREN,
                self.current_token
            )));
        }

        if !self.expect_peek(&TokenType::LBRACE) {
            self.errors.push(MError::ParseError(format!(
                "Expected next token to be {}, got={:?}",
                TokenType::LBRACE,
                self.current_token
            )));
        }

        let consequence = Box::new(self.parse_block_statement());

        if self.peek_token_is(&TokenType::ELSE) {
            self.next_token();

            if !self.peek_token_is(&TokenType::LBRACE) {
                self.errors.push(MError::ParseError(format!(
                    "Expected next token to be {}, got={:?}",
                    TokenType::LBRACE,
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

    fn parse_call_expression(&mut self, function: Expression) -> Expression {
        let arguments = self.parse_expression_list(TokenType::RPAREN);

        return Expression::Call {
            function: Box::new(function),
            arguments,
        };
    }

    fn parse_index_expression(&mut self, left: Expression) -> Expression {
        let left = Box::new(left);

        self.next_token();
        let index = Box::new(self.parse_expression(Precedence::LOWEST));

        if !self.expect_peek(&TokenType::RBRACKET) {
            self.errors.push(MError::ParseError(format!(
                "Expected next token to be {}, got={:?}",
                TokenType::RBRACKET,
                self.current_token
            )));
        }

        return Expression::Index {
            left,
            index,
        };
    }

    fn parse_expression_list(&mut self, end: TokenType) -> Vec<Expression> {
        let mut list = vec![];

        if self.peek_token_is(&end) {
            self.next_token();
            return list;
        }

        self.next_token();
        list.push(self.parse_expression(Precedence::LOWEST));

        while self.peek_token_is(&TokenType::COMMA) {
            self.next_token();
            self.next_token();
            list.push(self.parse_expression(Precedence::LOWEST));
        }

        if !self.expect_peek(&end) {
            self.errors.push(MError::ParseError(format!(
                "expected next token to be {}, got={:?}",
                end,
                self.current_token
            )));
        }

        return list;

    }

    fn parse_identifier(&mut self) -> Expression {
        return Expression::Literal(Literal::Identifier(self.current_token.literal.clone().unwrap()));
    }


    fn parse_boolean(&mut self) -> Expression {
        return Expression::Literal(Literal::from_token(&self.current_token));
    }

    fn parse_integer_literal(&mut self) -> Expression {
        let lit = self.current_token.literal.clone().unwrap();
        let value = i64::from_str_radix(&lit, 10);

        let value = match value {
            Ok(value) => value,
            Err(_) => {
                self.errors.push(MError::ParseError(format!(
                    "could not parse {} as integer",
                    &lit
                )));
                return Expression::Literal(Literal::from_token(&self.current_token));
            }
        };

        return Expression::Literal(Literal::Integer(value));
    }

    fn parse_string_literal(&mut self) -> Expression {
        return Expression::Literal(Literal::from_token(&self.current_token));
    }

    fn parse_array_literal(&mut self) -> Expression {
        let elements = self.parse_expression_list(TokenType::RBRACKET);

        return Expression::Array { elements };
    }

    fn parse_hash_literal(&mut self) -> Expression {
        let hash = HashMap::new();

        while !self.peek_token_is(&TokenType::RBRACE) {
            self.next_token();
            let key = self.parse_expression(Precedence::LOWEST);

            if !self.expect_peek(&TokenType::COLON) {
                self.errors.push(MError::ParseError(format!(
                    "Expected next token to be {}, got={:?}",
                    TokenType::COLON,
                    self.current_token
                )));
            }

            self.next_token();
            let value = self.parse_expression(Precedence::LOWEST);

            hash.insert(key, value);

            if !self.peek_token_is(&TokenType::RBRACE) && !self.expect_peek(&TokenType::COMMA) {
                self.errors.push(MError::ParseError(format!(
                    "Expected next token to be {}, got={:?}",
                    TokenType::COMMA,
                    self.current_token
                )));
            }
        }

        if !self.expect_peek(&TokenType::RBRACE) {
            self.errors.push(MError::ParseError(format!(
                "Expected next token to be {}, got={:?}",
                TokenType::RBRACE,
                self.current_token
            )));
        }

        return Expression::Literal(Literal::Hash(hash));
    }

    /// let <identifier> = <expression>;
    fn parse_let_statement(&mut self) -> ast::Statement {
        if !self.expect_peek(&TokenType::IDENT) {
            self.errors
                .push(MError::ParseError("expected identifier".to_string()));
        }

        // <identifier>
        let name = Expression::Literal(Literal::Identifier(self.current_token.to_string()));
        // ASSIGNじゃない場合構文エラー
        if !self.expect_peek(&TokenType::ASSIGN) {
            self.errors
                .push(MError::ParseError("expected =".to_string()));
        };

        // =
        self.next_token();

        // <expression>
        let value = self.parse_expression(Precedence::LOWEST);

        if self.peek_token_is(&TokenType::SEMICOLON) {
            self.next_token();
        }

        ast::Statement::Let { name, value }
    }

    fn parse_return_statement(&mut self) -> ast::Statement {
        self.next_token();

        let value = self.parse_expression(Precedence::LOWEST);

        if self.peek_token_is(&TokenType::SEMICOLON) {
            self.next_token();
        }

        return ast::Statement::Return { value };
    }

    fn parse_expression_statement(&mut self) -> ast::Statement {
        let expression = self.parse_expression(Precedence::LOWEST);

        if self.peek_token_is(&TokenType::SEMICOLON) {
            self.next_token();
        }

        return ast::Statement::Expression { expression };
    }

    fn parse_block_statement(&mut self) -> ast::Statement {
        let mut statements = vec![];

        self.next_token();

        while !self.current_token_is(&TokenType::RBRACE) && !self.current_token_is(&TokenType::EOF) {
            statements.push(self.parse_statement());
            self.next_token();
        }

        return ast::Statement::Block { statements };
    }

    fn parse_function_literal(&mut self) -> Expression {
        if !self.expect_peek(&TokenType::LPAREN) {
            self.errors.push(MError::ParseError(format!(
                "expected next token to be {}, got={:?}",
                TokenType::LPAREN,
                self.current_token
            )));
        }

        let parameters = self.parse_function_prameters();

        if !self.expect_peek(&TokenType::LBRACE) {
            self.errors.push(MError::ParseError(format!(
                "expected next token to be {}, got={:?}",
                TokenType::LBRACE,
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
        if self.peek_token_is(&TokenType::RPAREN) {
            self.next_token();
            return identifiers;
        }

        self.next_token();

        identifiers.push(Expression::Literal(Literal::Identifier(self.current_token.to_string())));

        while self.peek_token_is(&TokenType::COMMA) {
            self.next_token();
            self.next_token();
            identifiers.push(Expression::Literal(Literal::Identifier(self.current_token.to_string())));
        }

        // )
        if !self.expect_peek(&TokenType::RPAREN) {
            self.errors.push(MError::ParseError(format!(
                "expected next token to be {}, got={:?}",
                TokenType::RPAREN,
                self.current_token
            )));
        }

        return identifiers;
    }



    fn current_token_is(&self, token_type: &TokenType) -> bool {
        &self.current_token.token_type == token_type
    }
    fn peek_token_is(&self, token_type: &TokenType) -> bool {
        &self.peek_token.token_type == token_type
    }
    fn expect_peek(&mut self, token_type: &TokenType) -> bool {
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
        let p = PRECEDENCES.get(&self.peek_token.token_type);
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
            Precedence::INDEX => Precedence::INDEX,
        }
    }

    fn current_precedence(&mut self) -> Precedence {
        let p = PRECEDENCES.get(&self.current_token.token_type);
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
            Precedence::INDEX => Precedence::INDEX,
        }
    }

    fn register_prefix(&mut self, token_type: TokenType, fn_: fn(&mut Self) -> Expression) {
        self.prefix_parse_fns.insert(token_type, fn_);
    }

    fn register_infix(
        &mut self,
        token_type: TokenType,
        fn_: fn(&mut Self, Expression) -> Expression,
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
                Expression::Literal(Literal::Integer(5)),
            ),
            (
                "let y = true;",
                "y",
                Expression::Literal(Literal::Boolean(true)),
            ),
            (
                "let foobar = y;",
                "foobar",
                Expression::Literal(Literal::Identifier(String::from("y"))),
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
                    match i {
                        Expression::Literal(Literal::Identifier(i)) => {
                            assert_eq!(i, ident);
                        }
                        _ => panic!("not identifier"),
                    }
                    match v {
                        Expression::Literal(Literal::Integer(i)) => {
                            assert_eq!(i.to_string(), value.to_string());
                        }
                        Expression::Literal(Literal::Boolean(b)) => {
                            assert_eq!(b.to_string(), value.to_string());
                        }
                        Expression::Literal(Literal::Identifier(i)) => {
                            assert_eq!(i.to_string(), value.to_string());
                        }
                        _ => panic!("not integer"),
                    }
                }
                _ => panic!("not let statement"),
            };
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

        for (_, statement) in program.statements.iter().enumerate() {
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
                            &Expression::Literal(Literal::Identifier("x".to_string())),
                            &"<".to_string(),
                            &Expression::Literal(Literal::Identifier("y".to_string())),
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
                                            Expression::Literal(Literal::Identifier(n)) => {
                                                assert_eq!(n, "x")
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
                            &Expression::ident("x".to_string()),
                            &"<".to_string(),
                            &Expression::ident("y".to_string()),
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
                                            Expression::Literal(Literal::Identifier(n)) => {
                                                assert_eq!(n, "x")
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
                                                    Expression::Literal(Literal::Identifier(n)) => {
                                                        assert_eq!(n, "y")
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
                    Expression::Literal(Literal::Identifier(n)) => {
                        assert_eq!(n, "foobar");
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
                    Expression::Literal(Literal::Integer(n)) => {
                        assert_eq!(n, &5);
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
            ("!5;", "!", Expression::int(5)),
            ("-15;", "-", Expression::int(15)),
            ("!true;", "!", Expression::bool(true)),
            ("!false;", "!", Expression::bool(false)),
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
                                Expression::Literal(Literal::Integer(n)) => {
                                    assert_eq!(n.to_string(), value.to_string());
                                }
                                Expression::Literal(Literal::Boolean(n)) => {
                                    assert_eq!(n.to_string(), value.to_string());
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
                Expression::int(5),
                "+",
                Expression::int(5),
            ),
            (
                "5 - 5;",
                Expression::int(5),
                "-",
                Expression::int(5),
            ),
            (
                "5 * 5;",
                Expression::int(5),
                "*",
                Expression::int(5),
            ),
            (
                "5 / 5;",
                Expression::int(5),
                "/",
                Expression::int(5),
            ),
            (
                "5 > 5;",
                Expression::int(5),
                ">",
                Expression::int(5),
            ),
            (
                "5 < 5;",
                Expression::int(5),
                "<",
                Expression::int(5),
            ),
            (
                "5 == 5;",
                Expression::int(5),
                "==",
                Expression::int(5),
            ),
            (
                "5 != 5;",
                Expression::int(5),
                "!=",
                Expression::int(5),
            ),
            (
                "true == true",
                Expression::bool(true),
                "==",
                Expression::bool(true),
            ),
            (
                "true != false",
                Expression::bool(true),
                "!=",
                Expression::bool(false),
            ),
            (
                "false == false",
                Expression::bool(false),
                "==",
                Expression::bool(false),
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
            ("a * [1, 2, 3, 4][b * c] * d", "((a * ([1, 2, 3, 4][(b * c)])) * d)"),
            (
                "add(a * b[2], b[1], 2 * [1, 2][1])",
                "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
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
                            &Expression::ident("x".to_string()),
                            &"+".to_string(),
                            &Expression::ident("y".to_string()),
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

    fn test_literal_expression(exp: &Expression, value: String) -> bool {
        match exp {
            Expression::Literal(Literal::Integer(i_value)) => {
                if i_value.to_string() != value {
                    println!("exp not Expression::Integer. got={:?}", exp);
                    return false;
                }
                return true;
            }
            Expression::Literal(Literal::Boolean(b_value)) => {
                if b_value.to_string() != value {
                    println!("exp not Expression::Boolean. got={:?}", exp);
                    return false;
                }
                return true;
            }
            Expression::Literal(Literal::Identifier(i_value)) => {
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
            &Expression::int(2),
            &"*".to_string(),
            &Expression::int(3),
        );
        test_infix_expression(
            exp.1.get(2).unwrap(),
            &Expression::int(4),
            &"+".to_string(),
            &Expression::int(5),
        );
    }

    #[test]
    fn test_string_literal_expression() {
        let input = r#""hello world""#;

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
                    Expression::Literal(Literal::String(value)) => value,
                    _ => panic!("Expected string. got={:?}", expression),
                }
            }
            _ => panic!("Expected expression. got={:?}", statement),
        };

        assert_eq!(exp, "hello world");
    }

    #[test]
    fn test_parsing_index_expression() {
        let input = "myArray[1 + 1];";

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
                    Expression::Index {
                        left,
                        index,
                    } => (left, index),
                    _ => panic!("Expected index. got={:?}", expression),
                }
            }
            _ => panic!("Expected expression. got={:?}", statement),
        };

        if !test_identifier(exp.0, "myArray") {
            panic!("Expected identifier. got={:?}", exp.0);
        }

        if !test_infix_expression(
            exp.1,
            &Expression::int(1),
            &"+".to_string(),
            &Expression::int(1),
        ) {
            panic!("Expected infix expression. got={:?}", exp.1);
        }

    }

    /// helper function of infix_expression
    fn test_infix_expression<T: Display>(
        exp: &Expression,
        left: &T,
        operator: &String,
        right: &T,
    ) -> bool {
        match exp {
            Expression::Infix {
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

    fn test_identifier(exp: &Expression, value: &str) -> bool {
        match exp {
            Expression::Literal(Literal::Identifier(v))=> {
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
