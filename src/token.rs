use std::fmt::{Display, Formatter, self, Debug};

// pub type TokenType = &'static str;

// pub const ILLEGAL: TokenType = "ILLEGAL";
// pub const EOF: TokenType = "EOF";

// // Identifiers + literals
// pub const IDENT: TokenType = "IDENT";
// pub const INT: TokenType = "INT";
// pub const STRING: TokenType = "STRING";

// // Operators
// pub const ASSIGN: TokenType = "=";
// pub const PLUS: TokenType = "+";
// pub const MINUS: TokenType = "-";
// pub const BANG: TokenType = "!";
// pub const ASTERISK: TokenType = "*";
// pub const SLASH: TokenType = "/";

// pub const LT: TokenType = "<";
// pub const GT: TokenType = ">";

// pub const EQ: TokenType = "==";
// pub const NOT_EQ: TokenType = "!=";
// pub const DEEP_EQ: TokenType = "===";
// pub const DEEP_NOT_EQ: TokenType = "!==";

// // Delimiters
// pub const COMMA: TokenType = ",";
// pub const SEMICOLON: TokenType = ";";

// pub const LPAREN: TokenType = "(";
// pub const RPAREN: TokenType = ")";
// pub const LBRACE: TokenType = "{";
// pub const RBRACE: TokenType = "}";
// pub const LBRACKET: TokenType = "[";
// pub const RBRACKET: TokenType = "]";

// // Keywords
// pub const FUNCTION: TokenType = "FUNCTION";
// pub const LET: TokenType = "LET";
// pub const TRUE: TokenType = "TRUE";
// pub const FALSE: TokenType = "FALSE";
// pub const IF: TokenType = "IF";
// pub const ELSE: TokenType = "ELSE";
// pub const RETURN: TokenType = "RETURN";

#[derive(Hash,Debug, Clone, PartialEq, Eq)]
pub enum TokenType {
    ILLEGAL,
    EOF,
    IDENT,
    INT,
    STRING,
    ASSIGN,
    PLUS,
    MINUS,
    BANG,
    ASTERISK,
    SLASH,
    LT,
    GT,
    EQ,
    NotEq,
    DeepEq,
    DeepNotEq,
    COMMA,
    COLON,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    LBRACKET,
    RBRACKET,
    FUNCTION,
    LET,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,
}

impl Display for TokenType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            TokenType::ILLEGAL => write!(f, "ILLEGAL"),
            TokenType::EOF => write!(f, "EOF"),
            TokenType::IDENT => write!(f, "IDENT"),
            TokenType::INT => write!(f, "INT"),
            TokenType::STRING => write!(f, "STRING"),
            TokenType::ASSIGN => write!(f, "="),
            TokenType::PLUS => write!(f, "+"),
            TokenType::MINUS => write!(f, "-"),
            TokenType::BANG => write!(f, "!"),
            TokenType::ASTERISK => write!(f, "*"),
            TokenType::SLASH => write!(f, "/"),
            TokenType::LT => write!(f, "<"),
            TokenType::GT => write!(f, ">"),
            TokenType::EQ => write!(f, "=="),
            TokenType::NotEq => write!(f, "!="),
            TokenType::DeepEq => write!(f, "==="),
            TokenType::DeepNotEq => write!(f, "!=="),
            TokenType::COMMA => write!(f, ","),
            TokenType::SEMICOLON => write!(f, ";"),
            TokenType::COLON => write!(f, ":"),
            TokenType::LPAREN => write!(f, "("),
            TokenType::RPAREN => write!(f, ")"),
            TokenType::LBRACE => write!(f, "{{"),
            TokenType::RBRACE => write!(f, "}}"),
            TokenType::LBRACKET => write!(f, "["),
            TokenType::RBRACKET => write!(f, "]"),
            TokenType::FUNCTION => write!(f, "FUNCTION"),
            TokenType::LET => write!(f, "LET"),
            TokenType::TRUE => write!(f, "TRUE"),
            TokenType::FALSE => write!(f, "FALSE"),
            TokenType::IF => write!(f, "IF"),
            TokenType::ELSE => write!(f, "ELSE"),
            TokenType::RETURN => write!(f, "RETURN"),
        }
    }
}

impl TokenType {
    pub fn is_operator(&self) -> bool {
        match self {
            | TokenType::ASSIGN
            | TokenType::PLUS
            | TokenType::MINUS
            | TokenType::BANG
            | TokenType::ASTERISK
            | TokenType::SLASH
            | TokenType::LT
            | TokenType::GT
            | TokenType::EQ
            | TokenType::NotEq
            | TokenType::DeepEq
            | TokenType::DeepNotEq => true,
            _ => false,
        }
    }

    pub fn is_equality(&self) -> bool {
        match self {
            TokenType::EQ | TokenType::NotEq | TokenType::DeepEq | TokenType::DeepNotEq => true,
            _ => false,
        }
    }

    pub fn is_comparison(&self) -> bool {
        match self {
            TokenType::LT | TokenType::GT => true,
            _ => false,
        }
    }

    pub fn is_literal(&self) -> bool {
        match self {
            TokenType::INT | TokenType::STRING => true,
            _ => false,
        }
    }

    pub fn lookup_ident(ident: &String) -> TokenType {
        match ident.as_str() {
            "fn" => TokenType::FUNCTION,
            "let" => TokenType::LET,
            "true" => TokenType::TRUE,
            "false" => TokenType::FALSE,
            "if" => TokenType::IF,
            "else" => TokenType::ELSE,
            "return" => TokenType::RETURN,
            _ => TokenType::IDENT,
        }
    }
}




#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CodePosition {
    pub line: u32,
    pub column: u32,
}

impl CodePosition {
    pub fn new() -> CodePosition {
        CodePosition { line: 1, column: 1 }
    }

    pub fn inc_column(&mut self) {
        self.column += 1;
    }
    pub fn newline(&mut self) {
        self.line += 1;
        self.column = 1;
    }
}

impl Default for CodePosition {
    fn default() -> Self {
        CodePosition { line: 1, column: 1 }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: Option<String>,
    pub position: CodePosition,
}

impl Token {
    pub fn new(token:TokenType, pos: CodePosition) -> Token {
        Token {
            token_type: token,
            literal: None,
            position:  pos,
        }
    }

    pub fn with_literal(token_type: TokenType, literal: String, pos: CodePosition) -> Token {
        Token {
            token_type,
            literal: Some(literal),
            position: pos,
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if let Some(literal) = &self.literal {
            write!(f, "{}",literal)
        } else {
            write!(f, "{}", self.token_type)
        }
    }
}

impl Debug for Token {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "[line {}, col {}] {}", self.position.line,self.position.column, self.token_type)
    }
}



#[cfg(test)]
mod tests {
    use super::*;



    #[test]
    fn test_token_string() {
        let pos = CodePosition::new();
        let token = Token::new(TokenType::IDENT , pos);
        assert_eq!(token.to_string(), "IDENT");
        let token  = Token::new(TokenType::COMMA, pos);
        assert_eq!(token.to_string(), ",");
        let token  = Token::new(TokenType::SEMICOLON, pos);
        assert_eq!(token.to_string(), ";");
        let token  = Token::new(TokenType::LPAREN, pos);
        assert_eq!(token.to_string(), "(");
        let token  = Token::new(TokenType::RPAREN, pos);
        assert_eq!(token.to_string(), ")")
    }
}
