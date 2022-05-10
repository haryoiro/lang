use std::{
    io::{Error, ErrorKind},
    marker::PhantomData,
};

use crate::{
    ast,
    lexer,
    token::{self, Token},
};

struct Parser<'a> {
    lexer: &'a mut lexer::Lexer<'a>,

    current_token: token::Token,
    peek_token:    token::Token,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: &'a mut lexer::Lexer<'a>) -> Parser<'a> {
        let mut parser = Parser {
            lexer,
            current_token: Token::with_str(token::ILLEGAL, String::new()),
            peek_token: Token::with_str(token::ILLEGAL, String::new()),
        };
        parser.next_token();
        parser.next_token();
        return parser;
    }

    pub fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    pub fn parse_program(&mut self) -> Result<ast::Program, Error> {
        let mut program = ast::Program { statements: vec![] };

        while self.current_token.Type != token::EOF {
            let statement = self.parse_statement()?;
            program.statements.push(statement);
            self.next_token();
        }

        Ok(program)
    }

    fn parse_statement(&mut self) -> Result<ast::Statement, Error> {
        match self.current_token.Type {
            token::LET => Ok(self.parse_let_statement()?),
            _ => unimplemented!(),
        }
    }

    fn parse_let_statement(&mut self) -> Result<ast::Statement, Error> {
        if !self.expect_peek(token::IDENT) {
            return Err(Error::new(ErrorKind::InvalidInput, "Expected identifier"));
        }

        let name = ast::Expression::Identifier(self.current_token.Literal.clone());

        if !self.expect_peek(token::ASSIGN) {
            return Err(Error::new(ErrorKind::InvalidInput, "Expected '='"));
        };

        self.next_token();

        let value = ast::Expression::Identifier(self.current_token.Literal.clone());

        while !self.current_token_is(token::SEMICOLON) {
            self.next_token();
        }

        return Ok(ast::Statement::Let { name, value });
    }

    fn current_token_is(&self, token_type: token::TokenType) -> bool {
        self.current_token.Type == token_type
    }
    fn peek_token_is(&self, token_type: token::TokenType) -> bool {
        self.peek_token.Type == token_type
    }
    fn expect_peek(&mut self, token_type: token::TokenType) -> bool {
        if self.peek_token_is(token_type) {
            self.next_token();
            return true;
        } else {
            return false;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        ast::{Node, Statement},
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

        let program = parser.parse_program().unwrap();
        if program.statements.len() != 3 {
            panic!(
                "program.statements does not contain 3 statements. got={}",
                program.statements.len()
            );
        }

        let expected = vec![("x"), ("y"), ("foobar")];

        for (i, statement) in program.statements.iter().enumerate() {
            println!("{:?}", &statement);
            if !test_let_statement(statement, &expected[i]) {
                panic!("Failed to parse let statement");
            };
        }
    }

    fn test_let_statement(statement: &Statement, name: &str) -> bool {
        match statement {
            Statement::Let { name, value } => return true,
        }
        return false;
    }
}
