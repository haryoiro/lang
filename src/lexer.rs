use crate::token::{self, Token, TokenType};

#[derive(Debug)]
pub struct Lexer<'a> {
    pub input:         &'a str,
    pub position:      usize,
    pub read_position: usize,
    pub ch:            char,
    pub pos:           token::CodePosition,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Lexer {
        let mut l = Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: ' ',
            pos: token::CodePosition::new()
        };
        l.read_char();
        return l;
    }

    pub fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = '\0';
        } else {
            self.ch = self.input.chars().nth(self.read_position).unwrap();
        }
        self.position = self.read_position;
        self.read_position += 1;
        self.pos.inc_column();
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        let token = match self.ch {
            // Operators
            '=' => {
                // Check Equal Operator "=="
                if self.peek_char() == '=' {
                    self.read_char();
                    // Check Deep Equal Operator "==="
                    if self.peek_char() == '=' {
                        self.read_char();
                        Token::new(TokenType::DeepEq, self.pos)
                    } else {
                        // return Equal Operator "=="
                        Token::new(TokenType::EQ, self.pos)
                    }
                } else {
                    Token::new(TokenType::ASSIGN, self.pos)
                }
            }
            '!' => {
                // Check Not Equal Operator "!="
                if self.peek_char() == '=' {
                    self.read_char();
                    // Check Deep Not Equal Operator "!=="
                    if self.peek_char() == '=' {
                        self.read_char();
                        Token::new(TokenType::DeepNotEq,self.pos)
                    } else {
                        // return Not Equal Operator "!="
                        Token::new(TokenType::NotEq, self.pos)
                    }
                } else {
                    Token::new(TokenType::BANG, self.pos)
                }
            }
            '+' => Token::new(TokenType::PLUS, self.pos),
            '-' => Token::new(TokenType::MINUS, self.pos),
            '*' => Token::new(TokenType::ASTERISK, self.pos),
            '/' => Token::new(TokenType::SLASH, self.pos),
            '<' => Token::new(TokenType::LT, self.pos),
            '>' => Token::new(TokenType::GT, self.pos),
            // Delimiters
            ',' => Token::new(TokenType::COMMA, self.pos),
            ';' => Token::new(TokenType::SEMICOLON, self.pos),
            ':' => Token::new(TokenType::COLON, self.pos),
            '(' => Token::new(TokenType::LPAREN, self.pos),
            ')' => Token::new(TokenType::RPAREN, self.pos),
            '[' => Token::new(TokenType::LBRACKET, self.pos),
            ']' => Token::new(TokenType::RBRACKET, self.pos),
            '{' => Token::new(TokenType::LBRACE, self.pos),
            '}' => Token::new(TokenType::RBRACE, self.pos),
            '"' => Token::with_literal(TokenType::STRING, self.read_string(), self.pos),
            '\0' => Token::with_literal(TokenType::EOF, String::new(), self.pos),
            _ => {
                if is_letter(self.ch) {
                    let ident = self.read_identifier();
                    let token_type = TokenType::lookup_ident(&ident);
                    return Token::with_literal(token_type, ident, self.pos);
                } else if is_digit(self.ch) {
                    let literal = self.read_number();
                    return Token::with_literal(TokenType::INT, literal, self.pos);
                } else {
                    Token::new(TokenType::ILLEGAL, self.pos)
                }
            }
        };
        self.read_char();
        token
    }

    fn read_identifier(&mut self) -> String {
        let position = self.position;
        loop {
            if !is_letter(self.ch) {
                break;
            }

            self.read_char();
        }
        self.input
            .chars()
            .skip(position)
            .take(self.position - position)
            .collect()
    }

    fn read_number(&mut self) -> String {
        let position = self.position;
        loop {
            if !is_digit(self.ch) {
                break;
            }
            self.read_char();
        }
        self.input.get(position..self.position).unwrap().to_string()
    }

    fn read_string(&mut self) -> String {
        let mut result = String::new();
        loop {
            self.read_char();
            match self.ch {
                '"' => break,
                '\\' => {
                    self.read_char();
                    match self.ch {
                        'n' => result.push('\n'),
                        'r' => result.push('\r'),
                        't' => result.push('\t'),
                        '\\' => result.push('\\'),
                        '\'' => result.push('\''),
                        '"' => result.push('"'),
                        _ => {
                            result.push('\\');
                            result.push(self.ch);
                        }
                    }
                },
                '\0' => panic!("Unterminated string"),
                _ => result.push(self.ch),
            }
        }
        return result
    }

    fn peek_char(&self) -> char {
        if self.read_position >= self.input.chars().count() {
            return '\0';
        } else {
            return self.input.chars().nth(self.read_position).unwrap();
        }
    }

    fn skip_whitespace(&mut self) {
        while self.ch == ' ' || self.ch == '\t' || self.ch == '\n' || self.ch == '\r' {
            if self.ch == '\n' {
                self.pos.newline();
            }
            self.read_char();
        }
    }
}

fn is_letter(ch: char) -> bool {
    ch.is_ascii_alphabetic() || ch == '_'
}

fn is_digit(ch: char) -> bool {
    ch.is_digit(10)
}

#[cfg(test)]
mod tests {
    use crate::{lexer, token::{self, TokenType}};

    #[test]
    fn test_next_token_2() {
        let input = r#"let five = 5;
        let ten = 10;

        let add = fn(x, y) {
            x + y;
        };

        let result = add(five, ten);
        !-/*5;
        5 < 10 > 5;

        if (5 < 10) {
            return true;
        } else {
            return false;
        }

        10 == 10;
        10 != 9;

        "foobar"
        "foo bar"
        "foo\nbar"
        [1, 2];
        ["foo", "bar"];
        {"foo": "bar"}
        "#;

        let expected = vec![
            (TokenType::LET, "let"),
            (TokenType::IDENT, "five"),
            (TokenType::ASSIGN, "="),
            (TokenType::INT, "5"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::LET, "let"),
            (TokenType::IDENT, "ten"),
            (TokenType::ASSIGN, "="),
            (TokenType::INT, "10"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::LET, "let"),
            (TokenType::IDENT, "add"),
            (TokenType::ASSIGN, "="),
            (TokenType::FUNCTION, "fn"),
            (TokenType::LPAREN, "("),
            (TokenType::IDENT, "x"),
            (TokenType::COMMA, ","),
            (TokenType::IDENT, "y"),
            (TokenType::RPAREN, ")"),
            (TokenType::LBRACE, "{"),
            (TokenType::IDENT, "x"),
            (TokenType::PLUS, "+"),
            (TokenType::IDENT, "y"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::RBRACE, "}"),
            (TokenType::SEMICOLON, ";"),
            // let result = add(five, ten);
            (TokenType::LET, "let"),
            (TokenType::IDENT, "result"),
            (TokenType::ASSIGN, "="),
            (TokenType::IDENT, "add"),
            (TokenType::LPAREN, "("),
            (TokenType::IDENT, "five"),
            (TokenType::COMMA, ","),
            (TokenType::IDENT, "ten"),
            (TokenType::RPAREN, ")"),
            (TokenType::SEMICOLON, ";"),
            // !-/*5;
            (TokenType::BANG, "!"),
            (TokenType::MINUS, "-"),
            (TokenType::SLASH, "/"),
            (TokenType::ASTERISK, "*"),
            (TokenType::INT, "5"),
            (TokenType::SEMICOLON, ";"),
            // 5 < 10 > 5;
            (TokenType::INT, "5"),
            (TokenType::LT, "<"),
            (TokenType::INT, "10"),
            (TokenType::GT, ">"),
            (TokenType::INT, "5"),
            (TokenType::SEMICOLON, ";"),
            // if (5 < 10) {
            //     return true;
            // } else {
            //     return false;
            // }
            (TokenType::IF, "if"),
            (TokenType::LPAREN, "("),
            (TokenType::INT, "5"),
            (TokenType::LT, "<"),
            (TokenType::INT, "10"),
            (TokenType::RPAREN, ")"),
            (TokenType::LBRACE, "{"),
            (TokenType::RETURN, "return"),
            (TokenType::TRUE, "true"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::RBRACE, "}"),
            (TokenType::ELSE, "else"),
            (TokenType::LBRACE, "{"),
            (TokenType::RETURN, "return"),
            (TokenType::FALSE, "false"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::RBRACE, "}"),
            // 10 == 10;
            (TokenType::INT, "10"),
            (TokenType::EQ, "=="),
            (TokenType::INT, "10"),
            (TokenType::SEMICOLON, ";"),
            // 10 != 9;
            (TokenType::INT, "10"),
            (TokenType::NotEq, "!="),
            (TokenType::INT, "9"),
            (TokenType::SEMICOLON, ";"),
            // "foobar"
            (TokenType::STRING, "foobar"),
            // "foo bar"
            (TokenType::STRING, "foo bar"),
            // "foo\nbar"
            (TokenType::STRING, "foo\nbar"),
            // [1, 2];
            (TokenType::LBRACKET, "["),
            (TokenType::INT, "1"),
            (TokenType::COMMA, ","),
            (TokenType::INT, "2"),
            (TokenType::RBRACKET, "]"),
            (TokenType::SEMICOLON, ";"),
            // ["foo", "bar"];
            (TokenType::LBRACKET, "["),
            (TokenType::STRING, "foo"),
            (TokenType::COMMA, ","),
            (TokenType::STRING, "bar"),
            (TokenType::RBRACKET, "]"),
            (TokenType::SEMICOLON, ";"),
            // {"foo": "bar"}
            (TokenType::LBRACE, "{"),
            (TokenType::STRING, "foo"),
            (TokenType::COLON, ":"),
            (TokenType::STRING, "bar"),
            (TokenType::RBRACE, "}"),
            (TokenType::EOF, ""),
        ];

        let mut lexer = lexer::Lexer::new(input);

        for (i, (expected_type, expected_literal)) in expected.iter().enumerate() {
            let token = lexer.next_token();
            println!("{:?}", token);

            if token.token_type != *expected_type {
                panic!(
                    "tests[{}]: expected token type {}, got {}",
                    i, expected_type, token.token_type
                );
            }

            if token.to_string() != expected_literal.to_string(){
                panic!(
                    "tests[{}]: expected token literal {}, got {}",
                    i, expected_literal, token.to_string()
                );
            }
        }
    }
}
