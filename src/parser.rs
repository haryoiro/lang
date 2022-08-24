use std::collections::HashMap;

use once_cell::sync::Lazy;

use crate::{
    ast::{HashExpr, Node},
    error::MError,
    lexer,
    token::{CodePosition, Token, TokenType},
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

    prefix_parse_fns: HashMap<TokenType, fn(&mut Self) -> Node>,
    infix_parse_fns:  HashMap<TokenType, fn(&mut Self, Node) -> Node>,

    pub errors: Vec<MError>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: &'a mut lexer::Lexer<'a>) -> Parser<'a> {
        let mut parser = Parser {
            lexer,
            current_token: Token::new(TokenType::ILLEGAL, CodePosition::default()),
            peek_token: Token::new(TokenType::ILLEGAL, CodePosition::default()),
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),

            errors: Vec::new(),
        };

        parser.register_prefix(TokenType::IDENT, Parser::parse_identifier);
        parser.register_prefix(TokenType::INT, Parser::parse_integer_lit);
        parser.register_prefix(TokenType::FUNCTION, Parser::parse_function_lit);
        parser.register_prefix(TokenType::BANG, Parser::parse_prefix_expr);
        parser.register_prefix(TokenType::MINUS, Parser::parse_prefix_expr);
        parser.register_prefix(TokenType::TRUE, Parser::parse_boolean);
        parser.register_prefix(TokenType::FALSE, Parser::parse_boolean);
        parser.register_prefix(TokenType::LPAREN, Parser::parse_grouped_expr);
        parser.register_prefix(TokenType::IF, Parser::parse_if_expr);
        parser.register_prefix(TokenType::STRING, Parser::parse_string_lit);
        parser.register_prefix(TokenType::LBRACKET, Parser::parse_array_lit);
        parser.register_prefix(TokenType::LBRACE, Parser::parse_hash_lit);
        parser.register_infix(TokenType::PLUS, Parser::parse_infix_expr);
        parser.register_infix(TokenType::MINUS, Parser::parse_infix_expr);
        parser.register_infix(TokenType::SLASH, Parser::parse_infix_expr);
        parser.register_infix(TokenType::ASTERISK, Parser::parse_infix_expr);
        parser.register_infix(TokenType::EQ, Parser::parse_infix_expr);
        parser.register_infix(TokenType::NotEq, Parser::parse_infix_expr);
        parser.register_infix(TokenType::LT, Parser::parse_infix_expr);
        parser.register_infix(TokenType::GT, Parser::parse_infix_expr);
        parser.register_infix(TokenType::LPAREN, Parser::parse_call_expr);
        parser.register_infix(TokenType::LBRACKET, Parser::parse_index_expr);

        parser.next_token();
        parser.next_token();
        return parser;
    }

    pub fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    pub fn parse_program(&mut self) -> Node {
        let mut program = vec![];

        while self.current_token.token_type != TokenType::EOF {
            let stmt = self.parse_stmt();
            program.push(stmt);
            self.next_token();
        }

        Node::Program(program)
    }

    fn parse_stmt(&mut self) -> Node {
        match self.current_token.token_type {
            TokenType::LET => self.parse_let_stmt(),
            TokenType::RETURN => self.parse_return_stmt(),
            _ => self.parse_expr_stmt(),
        }
    }

    fn parse_expr(&mut self, precedence: Precedence) -> Node {
        let prefix = self.prefix_parse_fns.get(&self.current_token.token_type);
        let prefix = match prefix {
            Some(prefix) => *prefix,
            None => {
                self.errors.push(MError::ParseError(format!(
                    "no prefix parse function for {:?}",
                    self.current_token.token_type
                )));
                return Node::IdentifierLit(self.current_token.lit.clone().unwrap());
            }
        };

        let mut left_expr = prefix(self);

        while !self.peek_token_is(&TokenType::SEMICOLON) && precedence < self.peek_precedence() {
            {
                let infix = self
                    .infix_parse_fns
                    .get(&self.peek_token.token_type.clone());
                if infix.is_none() {
                    return left_expr;
                }
            }
            self.next_token();
            let infix = self
                .infix_parse_fns
                .get(&self.current_token.token_type)
                .unwrap();
            left_expr = infix(self, left_expr);
        }

        left_expr
    }
    fn parse_expr_stmt(&mut self) -> Node {
        let expression = self.parse_expr(Precedence::LOWEST);
        if self.peek_token_is(&TokenType::SEMICOLON) {
            self.next_token();
        }
        Node::ExprStmt(Box::new(expression))
    }

    fn parse_prefix_expr(&mut self) -> Node {
        let operator = self.current_token.to_string();

        self.next_token();

        let right = self.parse_expr(Precedence::PREFIX);

        Node::PrefixExpr(operator, Box::new(right))
    }

    fn parse_infix_expr(&mut self, left: Node) -> Node {
        let operator = &self.current_token.to_string();
        let left = left;

        let precedence = self.current_precedence();
        self.next_token();
        let right = self.parse_expr(precedence);

        Node::InfixExpr(Box::new(left), operator.clone(), Box::new(right))
    }

    fn parse_grouped_expr(&mut self) -> Node {
        self.next_token();

        let exp = self.parse_expr(Precedence::LOWEST);
        if !self.expect_peek(&TokenType::RPAREN) {}

        return exp;
    }

    fn parse_if_expr(&mut self) -> Node {
        if !self.expect_peek(&TokenType::LPAREN) {
            self.errors.push(MError::ParseError(format!(
                "no left parenthesis after if: {:?}",
                self.current_token
            )));
        }

        self.next_token();
        let condition = Box::new(self.parse_expr(Precedence::LOWEST));

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

        let consequence = Box::new(self.parse_block_stmt());

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

            let alternative = Some(Box::new(self.parse_block_stmt()));

            return Node::IfExpr(condition, consequence, alternative);
        }

        Node::IfExpr(condition, consequence, None)
    }

    fn parse_call_expr(&mut self, function: Node) -> Node {
        let arguments = self.parse_expr_list(TokenType::RPAREN);

        Node::CallExpr(Box::new(function), arguments)
    }

    fn parse_index_expr(&mut self, left: Node) -> Node {
        let left = Box::new(left);

        self.next_token();
        let index = Box::new(self.parse_expr(Precedence::LOWEST));

        if !self.expect_peek(&TokenType::RBRACKET) {
            self.errors.push(MError::ParseError(format!(
                "Expected next token to be {}, got={:?}",
                TokenType::RBRACKET,
                self.current_token
            )));
        }

        Node::IndexExpr(left, index)
    }

    fn parse_expr_list(&mut self, end: TokenType) -> Vec<Node> {
        let mut list = vec![];

        if self.peek_token_is(&end) {
            self.next_token();
            return list;
        }

        self.next_token();
        list.push(self.parse_expr(Precedence::LOWEST));

        while self.peek_token_is(&TokenType::COMMA) {
            self.next_token();
            self.next_token();
            list.push(self.parse_expr(Precedence::LOWEST));
        }

        if !self.expect_peek(&end) {
            self.errors.push(MError::ParseError(format!(
                "expected next token to be {}, got={:?}",
                end, self.current_token
            )));
        }

        return list;
    }

    fn parse_identifier(&mut self) -> Node {
        Node::IdentifierLit(self.current_token.lit.clone().unwrap())
    }

    fn parse_boolean(&mut self) -> Node {
        if &self.current_token.lit.clone().unwrap() == "true" {
            Node::BooleanLit(true)
        } else {
            Node::BooleanLit(false)
        }
    }

    fn parse_integer_lit(&mut self) -> Node {
        let lit = self.current_token.lit.clone().unwrap();
        let value = i64::from_str_radix(&lit, 10);

        let value = match value {
            Ok(value) => value,
            Err(_) => {
                self.errors.push(MError::ParseError(format!(
                    "could not parse {} as integer",
                    &lit
                )));
                // return Expr::Lit(Lit::from_token(&self.current_token));
                return Node::from_token(&self.current_token);
            }
        };

        return Node::IntegerLit(value);
    }

    fn parse_string_lit(&mut self) -> Node {
        let lit = self.current_token.lit.clone().unwrap();
        return Node::StringLit(lit);
    }

    fn parse_array_lit(&mut self) -> Node {
        let elements = self.parse_expr_list(TokenType::RBRACKET);

        return Node::ArrayExpr(elements);
    }

    fn parse_hash_lit(&mut self) -> Node {
        let mut pairs = HashMap::new();

        if self.peek_token_is(&TokenType::RBRACE) {
            self.next_token();
            // return Expr::Hash(ast::HashExpr(pairs));
            return Node::HashExpr(HashExpr(pairs));
        }

        self.next_token();
        let key = self.parse_expr(Precedence::LOWEST);

        if !self.expect_peek(&TokenType::COLON) {
            self.errors.push(MError::ParseError(format!(
                "Expected next token to be {}, got={:?}",
                TokenType::COLON,
                self.current_token
            )));
        }

        self.next_token();
        let value = self.parse_expr(Precedence::LOWEST);
        match key {
            Node::IdentifierLit(key) => {
                pairs.insert(key, value);
            }
            _ => {
                self.errors.push(MError::ParseError(format!(
                    "Expected next token to be {}, got={:?}",
                    TokenType::COLON,
                    self.current_token
                )));
            }
        }

        while self.peek_token_is(&TokenType::COMMA) {
            self.next_token();
            self.next_token();
            let key = self.parse_expr(Precedence::LOWEST);

            if !self.expect_peek(&TokenType::COLON) {
                self.errors.push(MError::ParseError(format!(
                    "Expected next token to be {}, got={:?}",
                    TokenType::COLON,
                    self.current_token
                )));
            }

            self.next_token();
            let value = self.parse_expr(Precedence::LOWEST);

            match key {
                Node::IdentifierLit(key) => {
                    pairs.insert(key, value);
                }
                _ => {
                    self.errors.push(MError::ParseError(format!(
                        "Expected next token to be {}, got={:?}",
                        TokenType::COLON,
                        self.current_token
                    )));
                }
            }
        }

        if !self.expect_peek(&TokenType::RBRACE) {
            self.errors.push(MError::ParseError(format!(
                "Expected next token to be {}, got={:?}",
                TokenType::RBRACE,
                self.current_token
            )));
        }

        return Node::HashExpr(HashExpr(pairs));
    }

    /// let <identifier> = <expression>;
    fn parse_let_stmt(&mut self) -> Node {
        if !self.expect_peek(&TokenType::IDENT) {
            self.errors
                .push(MError::ParseError("expected identifier".to_string()));
        }

        // <identifier>
        // let name = Expr::Lit(Lit::Identifier(self.current_token.to_string()));
        let name = Node::IdentifierLit(self.current_token.lit.clone().unwrap());
        // ASSIGNじゃない場合構文エラー
        if !self.expect_peek(&TokenType::ASSIGN) {
            self.errors
                .push(MError::ParseError("expected =".to_string()));
        };

        // =
        self.next_token();

        // <expression>
        let value = self.parse_expr(Precedence::LOWEST);

        if self.peek_token_is(&TokenType::SEMICOLON) {
            self.next_token();
        }
        Node::LetStmt(Box::new(name), Box::new(value))
        // ast::Stmt::Let { name, value }
    }

    fn parse_return_stmt(&mut self) -> Node {
        self.next_token();

        let value = self.parse_expr(Precedence::LOWEST);

        if self.peek_token_is(&TokenType::SEMICOLON) {
            self.next_token();
        }

        Node::ReturnStmt(Box::new(value))
    }

    // fn parse_expr_stmt(&mut self) -> Node {
    //     let expression = self.parse_expr(Precedence::LOWEST);

    //     if self.peek_token_is(&TokenType::SEMICOLON) {
    //         self.next_token();
    //     }
    //     Node::
    // }

    fn parse_block_stmt(&mut self) -> Node {
        let mut stmts = vec![];

        self.next_token();

        while !self.current_token_is(&TokenType::RBRACE) && !self.current_token_is(&TokenType::EOF)
        {
            stmts.push(self.parse_stmt());
            self.next_token();
        }

        Node::BlockStmt(stmts)
    }

    fn parse_function_lit(&mut self) -> Node {
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

        let body = Box::new(self.parse_block_stmt());

        // return Expr::Function { parameters, body };
        Node::FunctionExpr(parameters, body)
    }

    /// 引数をパースする
    /// ( <identifier>, <identifier>, ... )
    fn parse_function_prameters(&mut self) -> Vec<Node> {
        let mut identifiers = vec![];

        // (
        if self.peek_token_is(&TokenType::RPAREN) {
            self.next_token();
            return identifiers;
        }

        self.next_token();

        identifiers.push(Node::IdentifierLit(self.current_token.to_string()));

        while self.peek_token_is(&TokenType::COMMA) {
            self.next_token();
            self.next_token();

            identifiers.push(Node::IdentifierLit(self.current_token.to_string()));
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

    fn register_prefix(&mut self, token_type: TokenType, fn_: fn(&mut Self) -> Node) {
        self.prefix_parse_fns.insert(token_type, fn_);
    }

    fn register_infix(&mut self, token_type: TokenType, fn_: fn(&mut Self, Node) -> Node) {
        self.infix_parse_fns.insert(token_type, fn_);
    }
}

#[cfg(test)]
mod tests {

    use std::fmt::Display;

    use super::*;
    use crate::lexer::Lexer;

    #[test]
    fn test_let_stmts() {
        let inputs = vec![
            ("let x = 5;", "x", Node::IntegerLit(5)),
            ("let y = true;", "y", Node::BooleanLit(true)),
            (
                "let foobar = y;",
                "foobar",
                Node::IdentifierLit(String::from("y")),
            ),
        ];

        for (input, exp_ident, exp_value) in inputs {
            let mut l = Lexer::new(input);
            let mut p = Parser::new(&mut l);
            let program = p.parse_program();
            if p.errors.len() > 0 {
                for e in p.errors {
                    println!("{}", e);
                }
            }
            let program = match program {
                Node::Program(program) => program,
                _ => panic!("not program"),
            };

            match &program[0] {
                Node::LetStmt(name, inner_value) => {
                    let name = *name.to_owned();
                    let inner_value = *inner_value.to_owned();
                    match name {
                        Node::IdentifierLit(inner_ident) => {
                            assert_eq!(exp_ident, inner_ident);
                        }
                        _ => panic!("not identifier"),
                    }
                    assert_eq!(exp_value, inner_value);
                }
                _ => panic!("not let stmt"),
            };
        }
    }

    #[test]
    fn test_return_stmt() {
        let tests = vec![
            ("return 5;", Node::IntegerLit(5)),
            ("return 10;", Node::IntegerLit(10)),
            ("return 993322;", Node::IntegerLit(993322)),
        ];

        for (input, expected) in tests {
            let mut lex = Lexer::new(input);
            let mut parser = Parser::new(&mut lex);

            let program = parser.parse_program();
            if parser.errors.len() > 0 {
                println!("{:?}", parser.errors);
            }

            let program = match program {
                Node::Program(program) => program,
                _ => panic!("not program"),
            };

            for (_, stmt) in program.iter().enumerate() {
                match stmt {
                    Node::ReturnStmt(value) => {
                        let value = *value.to_owned();
                        assert_eq!(expected, value);
                    }
                    _ => {
                        panic!("Expected return stmt. got={:?}", stmt)
                    }
                }
            }
        }
    }

    #[test]
    fn test_if_expr() {
        let input = r#"if (x < y) { x };"#;

        let mut lex = Lexer::new(input);
        let mut parser = Parser::new(&mut lex);
        let program = parser.parse_program();
        if parser.errors.len() > 0 {
            for e in parser.errors.iter() {
                println!("error {}", e);
            }
        }
        let program = match program {
            Node::Program(program) => program,
            _ => panic!("not program"),
        };

        let exp = *program[0].to_owned().unwrap_expr_stmt().to_owned();

        match exp {
            Node::IfExpr(condition, consequence, alternative) => {
                if !test_infix_expr(
                    &condition,
                    &Node::IdentifierLit(String::from("x")),
                    &"<".to_string(),
                    &Node::IdentifierLit(String::from("y")),
                ) {
                    panic!("Failed to parse if condition");
                }
                match consequence.as_ref() {
                    Node::BlockStmt(stmts) => {
                        if stmts.len() != 1 {
                            panic!("Failed to parse if consequence: {:?}", stmts);
                        }
                        let exp = stmts[0].clone().unwrap_expr_stmt();
                        let ident = exp.clone().unwrap_identifier_lit();
                        if ident != "x" {
                            panic!("Failed to parse if consequence: {:?}", stmts);
                        }
                    }
                    _ => {
                        panic!("Failed to parse if consequence: {}", consequence);
                    }
                }
                match alternative {
                    Some(alter) => {
                        panic!("alternative stmts was not None. got={:?}", alter);
                    }
                    None => return,
                }
            }
            _ => panic!("not if stmt"),
        }
    }
    #[test]
    fn test_else_expr() {
        let input = r#"if (x < y) { x } else { y };"#;

        let mut lex = Lexer::new(input);
        let mut parser = Parser::new(&mut lex);
        let program = parser.parse_program();
        if parser.errors.len() > 0 {
            for e in parser.errors.iter() {
                println!("{}", e);
            }
        }

        let program = match program {
            Node::Program(program) => program,
            _ => panic!("not program"),
        };

        let exp = *program[0].to_owned().unwrap_expr_stmt().to_owned();

        match exp {
            Node::IfExpr(condition, consequence, alternative) => {
                if !test_infix_expr(
                    &condition,
                    &Node::IdentifierLit(String::from("x")),
                    &"<".to_string(),
                    &Node::IdentifierLit(String::from("y")),
                ) {
                    panic!("Failed to parse if condition");
                }
                match consequence.as_ref() {
                    Node::BlockStmt(stmts) => {
                        if stmts.len() != 1 {
                            panic!("Failed to parse if consequence: {:?}", stmts);
                        }
                        match &stmts[0] {
                            Node::ExprStmt(exp) => {
                                let exp = *exp.to_owned();
                                match exp {
                                    Node::IdentifierLit(n) => {
                                        assert_eq!(n, "x");
                                    }
                                    _ => {
                                        panic!("Failed to parse if consequence: {:?}", stmts)
                                    }
                                }
                            }
                            _ => {
                                panic!("Failed to parse if consequence: {:?}", stmts)
                            }
                        }
                    }
                    _ => {
                        panic!("Failed to parse if consequence: {}", consequence);
                    }
                }
                match alternative {
                    Some(alter) => {
                        match alter.as_ref() {
                            Node::BlockStmt(stmts) => {
                                if stmts.len() != 1 {
                                    panic!("Failed to parse if alternative: {:?}", stmts);
                                }

                                let exp = stmts[0].clone().unwrap_expr_stmt();
                                let ident = exp.clone().unwrap_identifier_lit();
                                if ident != "y" {
                                    panic!("Failed to parse if alternative: {:?}", stmts);
                                }
                            }
                            _ => {
                                panic!("Failed to parse if alternative: {:?}", alter);
                            }
                        }
                    }
                    None => {
                        panic!("alternative stmts was None. got={:?}", alternative)
                    }
                }
            }
            _ => {
                panic!("Expected if expression. got={:?}", program[0]);
            }
        }
    }

    #[test]
    fn test_identifier_expr() {
        let input = "foobar;";

        let mut lex = Lexer::new(input);
        let mut parser = Parser::new(&mut lex);
        let program = parser.parse_program();
        if parser.errors.len() > 0 {
            println!("{:?}", parser.errors);
        }
        let program = match program {
            Node::Program(program) => program,
            _ => panic!("not program"),
        };

        if &program.len() != &1 {
            panic!("program has not enough stmts. got={}", program.len());
        };
        let exp = *program[0].to_owned().unwrap_expr_stmt().to_owned();

        match exp {
            Node::IdentifierLit(n) => {
                assert_eq!(n, "foobar");
            }
            _ => panic!("Expected expression. got={:?}", exp),
        };
    }

    #[test]
    fn test_integer_lit_expr() {
        let input = "5;";

        let mut lex = Lexer::new(input);
        let mut parser = Parser::new(&mut lex);
        let program = parser.parse_program();

        let program = match program {
            Node::Program(program) => program,
            _ => panic!("not program"),
        };

        if parser.errors.len() > 0 {
            println!("{:?}", parser.errors);
        }

        if &program.len() != &1 {
            panic!("program has not enough stmts. got={}", program.len());
        };
        let exp = *program[0].to_owned().unwrap_expr_stmt().to_owned();

        match exp {
            Node::IntegerLit(n) => {
                assert_eq!(n, 5);
            }
            _ => panic!("Expected expression. got={:?}", exp),
        }
    }

    #[test]
    fn test_parsing_prefix_exprs() {
        let inputs = vec![
            ("!5;", "!", Node::IntegerLit(5)),
            ("-15;", "-", Node::IntegerLit(15)),
            ("!true;", "!", Node::BooleanLit(true)),
            ("!false;", "!", Node::BooleanLit(false)),
        ];

        for (input, operator, value) in inputs {
            let mut lex = Lexer::new(input);
            let mut parser = Parser::new(&mut lex);
            let program = parser.parse_program();

            if parser.errors.len() > 0 {
                println!("{:?}", parser.errors);
            }
            let program = match program {
                Node::Program(program) => program,
                _ => panic!("not program"),
            };

            if &program.len() != &1 {
                panic!("program has not enough stmts. got={}", program.len());
            }
            let exp = *program[0].to_owned().unwrap_expr_stmt().to_owned();

            match exp {
                Node::PrefixExpr(op, right) => {
                    if &op != operator {
                        panic!("Expected operator={}. got={}", op, operator);
                    }

                    let right = right.as_ref();
                    assert_eq!(right, &value);
                }

                _ => panic!("Expected expression. got={:?}", exp),
            }
        }
    }

    #[test]
    fn test_parsing_infix_expr() {
        use Node::*;
        let inputs = vec![
            ("5 + 5;", IntegerLit(5), "+", IntegerLit(5)),
            ("5 - 5;", IntegerLit(5), "-", IntegerLit(5)),
            ("5 * 5;", IntegerLit(5), "*", IntegerLit(5)),
            ("5 / 5;", IntegerLit(5), "/", IntegerLit(5)),
            ("5 > 5;", IntegerLit(5), ">", IntegerLit(5)),
            ("5 < 5;", IntegerLit(5), "<", IntegerLit(5)),
            ("5 == 5;", IntegerLit(5), "==", IntegerLit(5)),
            ("5 != 5;", IntegerLit(5), "!=", IntegerLit(5)),
            ("true == true", BooleanLit(true), "==", BooleanLit(true)),
            ("true != false", BooleanLit(true), "!=", BooleanLit(false)),
            ("false == false", BooleanLit(false), "==", BooleanLit(false)),
        ];

        for (input, left, operator, right) in inputs {
            let mut lex = Lexer::new(input);
            let mut parser = Parser::new(&mut lex);
            let program = parser.parse_program();

            if parser.errors.len() > 0 {
                println!("{:?}", parser.errors);
            }
            let program = match program {
                Node::Program(program) => program,
                _ => panic!("not program"),
            };

            if &program.len() != &1 {
                panic!("program has not enough stmts. got={}", program.len());
            }

            let exp = *program[0].to_owned().unwrap_expr_stmt().to_owned();

            if !test_infix_expr(&exp, &left, &operator.to_string(), &right) {
                panic!("Expected infix expression. got={:?}", &exp);
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
            (
                "a * [1, 2, 3, 4][b * c] * d",
                "((a * ([1, 2, 3, 4][(b * c)])) * d)",
            ),
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
    fn test_function_lit_parsing() {
        let input = "fn(x, y) { x + y; }";

        let mut lex = Lexer::new(input);
        let mut parser = Parser::new(&mut lex);
        let program = parser.parse_program();

        if parser.errors.len() > 0 {
            for error in parser.errors {
                println!("{}", error);
            }
        }
        let program = match program {
            Node::Program(program) => program,
            _ => panic!("not program"),
        };

        let exp = *program[0].to_owned().unwrap_expr_stmt().to_owned();

        let function = match exp {
            Node::FunctionExpr(parameters, body) => (parameters, body),
            _ => panic!("Expected expression. got={:?}", exp),
        };

        if function.0.len() != 2 {
            panic!("Expected 2 parameters. got={}", function.0.len());
        }

        test_lit_expr(function.0.get(0).unwrap(), "x".to_string());
        test_lit_expr(function.0.get(1).unwrap(), "y".to_string());

        let body = *function.1.to_owned();
        match body {
            Node::BlockStmt(stmts) => {
                if stmts.len() != 1 {
                    panic!("Expected 1 stmt. got={}", stmts.len());
                }
                let exp = *stmts[0].to_owned().unwrap_expr_stmt().to_owned();

                if !test_infix_expr(
                    &exp,
                    &Node::IdentifierLit("x".to_string()),
                    &"+".to_string(),
                    &Node::IdentifierLit("y".to_string()),
                ) {
                    panic!("Expected infix expression. got={:?}", exp);
                }
            }
            _ => panic!("Expected block. got={:?}", body),
        }
    }

    fn test_lit_expr(exp: &Node, value: String) -> bool {
        match exp {
            Node::IntegerLit(i) => {
                if i.to_string() != value {
                    println!("exp not IntegerLiteger. got={:?}", exp);
                    return false;
                }
                return true;
            }
            Node::BooleanLit(b) => {
                if b.to_string() != value {
                    println!("exp not BooleanLitean. got={:?}", exp);
                    return false;
                }
                return true;
            }
            Node::IdentifierLit(i) => {
                if i.to_string() != value {
                    println!("exp not Node::IdentifierLitifier. got={:?}", exp);
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
            let program = match program {
                Node::Program(program) => program,
                _ => panic!("not program"),
            };

            let exp = *program[0].to_owned().unwrap_expr_stmt().to_owned();

            let function = match exp {
                Node::FunctionExpr(parameters, body) => (parameters, body),
                _ => panic!("Expected expression. got={:?}", exp),
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
    fn test_call_expr_parsing() {
        let input = "add(1, 2 * 3, 4 + 5);";

        let mut lex = Lexer::new(input);
        let mut parser = Parser::new(&mut lex);
        let program = parser.parse_program();

        if parser.errors.len() > 0 {
            for e in parser.errors {
                println!("{}", e);
            }
        }
        let program = match program {
            Node::Program(program) => program,
            _ => panic!("not program"),
        };

        let exp = *program[0].to_owned().unwrap_expr_stmt().to_owned();

        let exp = match exp {
            Node::CallExpr(function, arguments) => (function, arguments),
            _ => panic!("Expected expression. got={:?}", exp),
        };

        if !test_identifier(&exp.0.to_owned(), "add") {
            panic!("Expected identifier. got={:?}", exp.0);
        }

        if exp.1.len() != 3 {
            panic!("Expected 3 arguments. got={}", exp.1.len());
        }

        test_lit_expr(exp.1.get(0).unwrap(), "1".to_string());
        test_infix_expr(
            exp.1.get(1).unwrap(),
            &Node::IntegerLit(2),
            &"*".to_string(),
            &Node::IntegerLit(3),
        );
        test_infix_expr(
            exp.1.get(2).unwrap(),
            &Node::IntegerLit(4),
            &"+".to_string(),
            &Node::IntegerLit(5),
        );
    }

    #[test]
    fn test_string_lit_expr() {
        let input = r#""hello world""#;

        let mut lex = Lexer::new(input);
        let mut parser = Parser::new(&mut lex);
        let program = parser.parse_program();

        if parser.errors.len() > 0 {
            for e in parser.errors {
                println!("{}", e);
            }
        }

        let program = match program {
            Node::Program(program) => program,
            _ => panic!("not program"),
        };

        let exp = *program[0].to_owned().unwrap_expr_stmt().to_owned();

        let exp = match exp {
            Node::StringLit(s) => s,
            _ => panic!("Expected expression. got={:?}", exp),
        };

        assert_eq!(exp, "hello world");
    }

    #[test]
    fn test_parsing_index_expr() {
        let input = "myArray[1 + 1];";

        let mut lex = Lexer::new(input);
        let mut parser = Parser::new(&mut lex);
        let program = parser.parse_program();

        if parser.errors.len() > 0 {
            for e in parser.errors {
                println!("{}", e);
            }
        }
        let program = match program {
            Node::Program(program) => program,
            _ => panic!("not program"),
        };

        let exp = *program[0].to_owned().unwrap_expr_stmt().to_owned();

        let exp = match exp {
            Node::IndexExpr(left, index) => (left, index),
            _ => panic!("Expected expression. got={:?}", exp),
        };

        if !test_identifier(&*exp.0.to_owned(), "myArray") {
            panic!("Expected identifier. got={:?}", exp.0);
        }

        if !test_infix_expr(
            &*exp.1.to_owned(),
            &Node::IntegerLit(1),
            &"+".to_string(),
            &Node::IntegerLit(1),
        ) {
            panic!("Expected infix expression. got={:?}", exp.1);
        }
    }

    /// helper function of infix_expr
    fn test_infix_expr<T: Display>(exp: &Node, left: &T, operator: &String, right: &T) -> bool {
        // let exp = *exp.clone().unwrap_expr_stmt();
        match exp {
            Node::InfixExpr(l, op, r) => {
                if !test_lit_expr(l.as_ref(), left.to_string()) {
                    return false;
                }
                if op != operator {
                    println!("exp::Operator is not {}, got={}", operator, op);
                    return false;
                }
                if !test_lit_expr(r.as_ref(), right.to_string()) {
                    return false;
                }
                return true;
            }
            _ => return false,
        };
    }

    fn test_identifier(exp: &Node, value: &str) -> bool {
        match exp {
            Node::IdentifierLit(v) => {
                if v != value {
                    println!("identifier is not {}, got={}", value, v);
                    return false;
                }
                return true;
            }
            _ => return false,
        }
    }
}
