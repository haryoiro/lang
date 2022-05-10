pub type TokenType = &'static str;

#[allow(non_snake_case)]
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub Type:    TokenType,
    pub Literal: String,
}

impl Token {
    pub fn with_str(token_type: TokenType, literal: String) -> Token {
        Token {
            Type:    token_type,
            Literal: literal,
        }
    }
    pub fn new(token_type: TokenType, literal: char) -> Token {
        Token {
            Type:    token_type,
            Literal: literal.to_string(),
        }
    }
}

pub const KEYWORDS: &[(&str, TokenType)] = &[
    ("fn", "FUNCTION"),
    ("let", "LET"),
    ("true", "TRUE"),
    ("false", "FALSE"),
    ("if", "IF"),
    ("else", "ELSE"),
    ("retrun", "RETURN"),
];

pub fn lookup_ident(ident: &str) -> TokenType {
    for (keyword, token_type) in KEYWORDS {
        if &ident == keyword {
            return token_type;
        }
    }
    IDENT
}

pub const ILLEGAL: TokenType = "ILLEGAL";
pub const EOF: TokenType = "EOF";

// Identifiers + literals
pub const IDENT: TokenType = "INDENT";
pub const INT: TokenType = "INT";

// Operators
pub const ASSIGN: TokenType = "=";
pub const PLUS: TokenType = "+";
pub const MINUS: TokenType = "-";
pub const BANG: TokenType = "!";
pub const ASTERISK: TokenType = "*";
pub const SLASH: TokenType = "/";

pub const LT: TokenType = "<";
pub const GT: TokenType = ">";

pub const EQ: TokenType = "==";
pub const NOT_EQ: TokenType = "!=";

// Delimiters
pub const COMMA: TokenType = ",";
pub const SEMICOLON: TokenType = ";";

pub const LPAREN: TokenType = "(";
pub const RPAREN: TokenType = ")";
pub const LBRACE: TokenType = "{";
pub const RBRACE: TokenType = "}";

// Keywords
pub const FUNCTION: TokenType = "FUNCTION";
pub const LET: TokenType = "LET";
pub const TRUE: TokenType = "TRUE";
pub const FALSE: TokenType = "FALSE";
pub const IF: TokenType = "IF";
pub const ELSE: TokenType = "ELSE";
pub const RETURN: TokenType = "RETURN";
