use std::{
    collections::{BTreeMap, HashMap},
    fmt::{Debug, Display, Error, Formatter},
};

use crate::{
    object::Object,
    token::{Token, TokenType},
};

pub trait Node {
    fn token_literal(&self) -> String;
}

pub trait IProgram {
    fn program_node(&self) -> bool;
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl IProgram for Program {
    fn program_node(&self) -> bool {
        true
    }
}

impl Node for Program {
    fn token_literal(&self) -> String {
        if self.statements.len() > 0 {
            self.statements[0].token_literal()
        } else {
            String::new()
        }
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        for statement in &self.statements {
            write!(f, "{}", statement.to_string())?;
        }
        Ok(())
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Statement {
    Let {
        name:  Expression,
        value: Expression,
    },
    Return {
        value: Expression,
    },
    Expression {
        expression: Expression,
    },
    Block {
        statements: Vec<Statement>,
    },
}

pub trait IStatement {
    fn statement_node(&self) -> bool;
}

impl IStatement for Statement {
    fn statement_node(&self) -> bool {
        true
    }
}

impl Node for Statement {
    fn token_literal(&self) -> String {
        match self {
            Statement::Let { .. } => "let".to_string(),
            Statement::Return { .. } => "return".to_string(),
            Statement::Expression { .. } => "expression".to_string(),
            Statement::Block { .. } => "block".to_string(),
        }
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Statement::Let { name, value } => {
                write!(f, "let {} = {};", name.to_string(), value.to_string())
            }
            Statement::Return { value } => {
                write!(f, "return {};", value.to_string())
            }
            Statement::Expression { expression } => {
                write!(f, "{}", expression.to_string())
            }
            Statement::Block { statements } => {
                for statement in statements {
                    write!(f, "{}", statement.to_string())?;
                }
                Ok(())
            }
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct HashExpression(pub HashMap<Literal, Expression>);
impl Eq for HashExpression {
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Expression {
    Literal(Literal),
    // MULTIPLE
    Array {
        elements: Vec<Expression>,
    },
    Hash(HashExpression),
    Index {
        left:  Box<Expression>,
        index: Box<Expression>,
    },
    // STATEMENTS
    If {
        condition:   Box<Expression>,
        consequence: Box<Statement>,
        alternative: Option<Box<Statement>>,
    },
    Function {
        parameters: Vec<Expression>,
        body:       Box<Statement>,
    },
    Call {
        function:  Box<Expression>,
        arguments: Vec<Expression>,
    },
    Prefix {
        operator: String,
        right:    Box<Expression>,
    },
    Infix {
        operator: String,
        left:     Box<Expression>,
        right:    Box<Expression>,
    },
}

pub trait IExpression {
    fn expression_node(&self) -> bool;
}

impl IExpression for Expression {
    fn expression_node(&self) -> bool {
        true
    }
}

impl Expression {
    pub fn ident(string: String) -> Expression {
        Expression::Literal(Literal::Identifier(string))
    }
    pub fn int(integer: i64) -> Expression {
        Expression::Literal(Literal::Integer(integer))
    }
    pub fn string(string: String) -> Expression {
        Expression::Literal(Literal::String(string))
    }
    pub fn bool(boolean: bool) -> Expression {
        Expression::Literal(Literal::Boolean(boolean))
    }
}

impl Node for Expression {
    fn token_literal(&self) -> String {
        match self {
            Expression::Literal(literal) => literal.token_literal(),
            Expression::Array { .. } => "array".to_string(),
            Expression::Hash { .. } => "hash".to_string(),
            Expression::Index { .. } => "index".to_string(),
            Expression::If { .. } => "if".to_string(),
            Expression::Function { .. } => "fn".to_string(),
            Expression::Call { .. } => "call".to_string(),
            Expression::Prefix { operator, .. } => operator.to_string(),
            Expression::Infix { operator, .. } => operator.to_string(),
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            Expression::Literal(literal) => write!(f, "{}", literal.to_string()),
            Expression::Array { elements } => {
                write!(f, "[")?;
                for (i, element) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", element.to_string())?;
                }
                write!(f, "]")
            }
            Expression::Hash(hash_map) => {
                write!(f, "{{ ")?;
                for (key, value) in hash_map.0.iter() {
                    write!(f, "{}: {}, ", key.to_string(), value.to_string())?;
                }
                write!(f, " }}")
            }
            Expression::Index { left, index } => {
                write!(f, "({}[{}])", left.to_string(), index.to_string())
            }
            Expression::If {
                condition,
                consequence,
                alternative,
            } => {
                let mut alternative_str = "".to_string();
                if let Some(alt) = alternative {
                    alternative_str = format!("else {}", alt.to_string());
                }

                write!(
                    f,
                    "if {} {} {}",
                    condition.to_string(),
                    consequence.to_string(),
                    alternative_str
                )
            }
            Expression::Function { parameters, body } => {
                let mut params = vec![];
                for param in parameters {
                    params.push(param.to_string());
                }

                write!(f, "fn ({}) {{{}}}", params.join(", "), body.to_string())
            }
            Expression::Call {
                function,
                arguments,
            } => {
                let args = arguments
                    .iter()
                    .map(|arg| arg.to_string())
                    .collect::<Vec<String>>()
                    .join(", ");
                write!(f, "{}({})", function.as_ref().to_string(), args)
            }
            Expression::Prefix {
                operator, right, ..
            } => write!(f, "({}{})", operator, right),
            Expression::Infix {
                operator,
                left,
                right,
                ..
            } => write!(f, "({} {} {})", left, operator, right),
        }
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum Literal {
    Identifier(String),
    Integer(i64),
    String(String),
    Boolean(bool),
}

pub trait ILiteral {
    fn literal_node(&self) -> bool;
}

impl ILiteral for Literal {
    fn literal_node(&self) -> bool {
        true
    }
}

impl Node for Literal {
    fn token_literal(&self) -> String {
        match self {
            Literal::Identifier(value) => value.to_string(),
            Literal::Integer(value) => value.to_string(),
            Literal::String(value) => value.to_string(),
            Literal::Boolean(value) => value.to_string(),
        }
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            Literal::Identifier(value) => write!(f, "{}", value),
            Literal::Integer(value) => write!(f, "{}", value),
            Literal::String(value) => write!(f, "{}", value),
            Literal::Boolean(value) => write!(f, "{}", value),
        }
    }
}

impl Literal {
    pub fn from_token(token: &Token) -> Self {
        match token.token_type {
            TokenType::STRING => Literal::String(token.to_string()),
            TokenType::INT => Literal::Integer(token.to_string().parse::<i64>().unwrap()),
            TokenType::TRUE => Literal::Boolean(true),
            TokenType::FALSE => Literal::Boolean(false),
            TokenType::IDENT => Literal::Identifier(token.to_string()),
            _ => Literal::Identifier(token.to_string()),
        }
    }
}

impl PartialEq<str> for Literal {
    fn eq(&self, other: &str) -> bool {
        match self {
            Literal::Identifier(value) => value == other,
            Literal::Integer(value) => value.to_string() == other,
            Literal::String(value) => value == other,
            Literal::Boolean(value) => value.to_string() == other,
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_string() {
        let program = Program {
            statements: vec![
                Statement::Let {
                    name:  Expression::ident("myVar".to_string()),
                    value: Expression::ident("five".to_string()),
                },
                Statement::Return {
                    value: Expression::ident("five".to_string()),
                },
            ],
        };
        println!("{}", program.to_string());
        if program.to_string() != "let myVar = five;return five;" {
            panic!(
                "expected let myVar = five;return five;, got {}",
                program.to_string()
            );
        }
    }
}
