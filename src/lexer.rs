use crate::token::{self, Token, TokenType};

pub struct Lexer<'a> {
    pub input:         &'a str,
    pub position:      usize,
    pub read_position: usize,
    pub ch:            char,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Lexer {
        let mut l = Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: ' ',
        };
        l.read_char();
        let l = l;
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
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let token = match self.ch {
            // Operators
            '=' => {
                // Todo: makeTowCharToken
                if self.peek_char() == '=' {
                    let tmp_c = self.ch;
                    self.read_char();
                    let lit = format!("{}{}", tmp_c, self.ch);
                    Token::with_str(token::EQ, lit)
                } else {
                    Token::new(token::ASSIGN, self.ch)
                }
            }
            '!' => {
                if self.peek_char() == '=' {
                    let tmp_c = self.ch;
                    self.read_char();
                    let lit = format!("{}{}", tmp_c, self.ch);
                    Token::with_str(token::NOT_EQ, lit)
                } else {
                    Token::new(token::BANG, self.ch)
                }
            }
            '+' => Token::new(token::PLUS, self.ch),
            '-' => Token::new(token::MINUS, self.ch),
            '*' => Token::new(token::ASTERISK, self.ch),
            '/' => Token::new(token::SLASH, self.ch),
            '<' => Token::new(token::LT, self.ch),
            '>' => Token::new(token::GT, self.ch),
            // Delimiters
            ',' => Token::new(token::COMMA, self.ch),
            ';' => Token::new(token::SEMICOLON, self.ch),
            '(' => Token::new(token::LPAREN, self.ch),
            ')' => Token::new(token::RPAREN, self.ch),
            '{' => Token::new(token::LBRACE, self.ch),
            '}' => Token::new(token::RBRACE, self.ch),
            '\0' => Token::with_str(token::EOF, String::new()),
            _ => {
                if is_letter(self.ch) {
                    let ident = self.read_identifier();
                    let token_type = token::lookup_ident(&ident);
                    return Token::with_str(token_type, ident);
                } else if is_digit(self.ch) {
                    let literal = self.read_number();
                    return Token::with_str(token::INT, literal);
                } else {
                    Token::new(token::ILLEGAL, self.ch)
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
        self.input
            .chars()
            .skip(position)
            .take(self.position - position)
            .collect()
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
    use crate::{lexer, token};

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

        "#;

        let expected = vec![
            (token::LET, "let"),
            (token::IDENT, "five"),
            (token::ASSIGN, "="),
            (token::INT, "5"),
            (token::SEMICOLON, ";"),
            (token::LET, "let"),
            (token::IDENT, "ten"),
            (token::ASSIGN, "="),
            (token::INT, "10"),
            (token::SEMICOLON, ";"),
            (token::LET, "let"),
            (token::IDENT, "add"),
            (token::ASSIGN, "="),
            (token::FUNCTION, "fn"),
            (token::LPAREN, "("),
            (token::IDENT, "x"),
            (token::COMMA, ","),
            (token::IDENT, "y"),
            (token::RPAREN, ")"),
            (token::LBRACE, "{"),
            (token::IDENT, "x"),
            (token::PLUS, "+"),
            (token::IDENT, "y"),
            (token::SEMICOLON, ";"),
            (token::RBRACE, "}"),
            (token::SEMICOLON, ";"),
            (token::LET, "let"),
            (token::IDENT, "result"),
            (token::ASSIGN, "="),
            (token::IDENT, "add"),
            (token::LPAREN, "("),
            (token::IDENT, "five"),
            (token::COMMA, ","),
            (token::IDENT, "ten"),
            (token::RPAREN, ")"),
            (token::SEMICOLON, ";"),
            (token::BANG, "!"),
            (token::MINUS, "-"),
            (token::SLASH, "/"),
            (token::ASTERISK, "*"),
            (token::INT, "5"),
            (token::SEMICOLON, ";"),
            (token::INT, "5"),
            (token::LT, "<"),
            (token::INT, "10"),
            (token::GT, ">"),
            (token::INT, "5"),
            (token::SEMICOLON, ";"),
            //
            // if (5 < 10) {
            //     return true;
            // } else {
            //     return false;
            // }
            (token::IF, "if"),
            (token::LPAREN, "("),
            (token::INT, "5"),
            (token::LT, "<"),
            (token::INT, "10"),
            (token::RPAREN, ")"),
            (token::LBRACE, "{"),
            (token::RETURN, "return"),
            (token::TRUE, "true"),
            (token::SEMICOLON, ";"),
            (token::RBRACE, "}"),
            (token::ELSE, "else"),
            (token::LBRACE, "{"),
            (token::RETURN, "return"),
            (token::FALSE, "false"),
            (token::SEMICOLON, ";"),
            (token::RBRACE, "}"),
            // if (5 < 10) {
            //     return true;
            // } else {
            //     return false;
            // }
            (token::EOF, ""),
        ];

        let mut lexer = lexer::Lexer::new(input);

        for (i, (expected_type, expected_literal)) in expected.iter().enumerate() {
            let token = lexer.next_token();
            println!("{:?}", token);

            if token.type_ != *expected_type {
                panic!(
                    "tests[{}]: expected token type {}, got {}",
                    i, expected_type, token.type_
                );
            }

            if token.literal != *expected_literal {
                panic!(
                    "tests[{}]: expected token literal {}, got {}",
                    i, expected_literal, token.literal
                );
            }
        }
    }
}
