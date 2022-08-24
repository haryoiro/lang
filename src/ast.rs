use std::{
    collections::{BTreeMap, HashMap},
    fmt::{Debug, Display, Error, Formatter},
};

use derive_more::Unwrap;

use crate::token::{Token, TokenType};

#[derive(Debug, Clone, PartialEq, Eq, Unwrap)]
pub enum Node {
    // pub enum Expr {
    //     Lit(Lit),
    //     // MULTIPLE
    //     Array {
    //         elements: Vec<Expr>,
    //     },
    //     Hash(HashExpr),
    //     Index {
    //         left:  Box<Expr>,
    //         index: Box<Expr>,
    //     },
    //     // STATEMENTS
    //     If {
    //         condition:   Box<Expr>,
    //         consequence: Box<Stmt>,
    //         alternative: Option<Box<Stmt>>,
    //     },
    //     Function {
    //         parameters: Vec<Expr>,
    //         body:       Box<Stmt>,
    //     },
    //     Call {
    //         function:  Box<Expr>,
    //         arguments: Vec<Expr>,
    //     },
    //     Prefix {
    //         operator: String,
    //         right:    Box<Expr>,
    //     },
    //     Infix {
    //         operator: String,
    //         left:     Box<Expr>,
    //         right:    Box<Expr>,
    //     },
    // }
    Program(Vec<Node>),
    ArrayExpr(Vec<Node>),
    HashExpr(HashExpr),
    FunctionExpr(Vec<Node>, Box<Node>),
    IndexExpr(Box<Node>, Box<Node>),
    IfExpr(Box<Node>, Box<Node>, Option<Box<Node>>),
    CallExpr(Box<Node>, Vec<Node>),
    /// operator, node
    PrefixExpr(String, Box<Node>),
    /// left, operator, right
    InfixExpr(Box<Node>, String, Box<Node>),

    BlockStmt(Vec<Node>),
    LetStmt(Box<Node>, Box<Node>),
    ReturnStmt(Box<Node>),
    ExprStmt(Box<Node>),

    IdentifierLit(String),
    IntegerLit(i64),
    StringLit(String),
    BooleanLit(bool),
}

impl Display for Node {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Node::Program(nodes) => {
                for node in nodes {
                    write!(f, "{}", node)?;
                }
                return Ok(());
            }
            Node::ArrayExpr(nodes) => {
                write!(f, "[")?;
                for (i, node) in nodes.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", node)?;
                }
                write!(f, "]")?;
                return Ok(());
            }
            Node::HashExpr(hash_expr) => {
                let hash = hash_expr;
                write!(f, "{{")?;
                for (i, (key, value)) in hash.0.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", key, value)?;
                }
                write!(f, "}}")?;

                return Ok(());
            }
            Node::IndexExpr(left, right) => {
                write!(f, "({}[{}])", left, right)?;
                return Ok(());
            }
            Node::IfExpr(condition, consequence, alternative) => {
                let is_active_alternative = alternative.is_some();
                let alternative = alternative
                    .clone()
                    .unwrap_or(Box::new(Node::BlockStmt(vec![])));

                if is_active_alternative {
                    write!(f, "if ({}) {} else {}", condition, consequence, alternative)?;
                } else {
                    write!(f, "if ({}) {}", condition, consequence)?;
                }

                return Ok(());
            }

            Node::CallExpr(function, arguments) => {
                let mut args = String::new();
                for (i, arg) in arguments.iter().enumerate() {
                    if i > 0 {
                        args.push_str(", ");
                    }
                    args.push_str(&arg.to_string());
                }

                write!(f, "{}({})", function, args)?;
                return Ok(());
            }
            Node::PrefixExpr(operator, right) => {
                write!(f, "({}{})", operator, right)?;
                return Ok(());
            }
            Node::InfixExpr(left, operator, right) => {
                write!(f, "({} {} {})", left, operator, right)?;
                return Ok(());
            }

            Node::BlockStmt(stmts) => {
                write!(f, "{{\n")?;
                for stmt in stmts {
                    write!(f, "{}", stmt)?;
                }
                write!(f, "}}")?;
                return Ok(());
            }
            Node::LetStmt(token, value) => {
                write!(f, "let {} = {};", token, value)?;
                return Ok(());
            }
            Node::ReturnStmt(return_value) => {
                write!(f, "return {};", return_value)?;
                return Ok(());
            }
            Node::ExprStmt(expression) => {
                write!(f, "{}", expression)?;
                return Ok(());
            }
            Node::IdentifierLit(identifier) => {
                write!(f, "{}", identifier)?;
                return Ok(());
            }
            Node::IntegerLit(integer) => {
                write!(f, "{}", integer)?;
                return Ok(());
            }
            Node::StringLit(string) => {
                write!(f, "\"{}\"", string)?;
                return Ok(());
            }
            Node::BooleanLit(boolean) => {
                write!(f, "{}", boolean)?;
                return Ok(());
            }
            Node::FunctionExpr(parameters, body) => {
                let mut params = String::new();
                for (i, param) in parameters.iter().enumerate() {
                    if i > 0 {
                        params.push_str(", ");
                    }
                    params.push_str(&param.to_string());
                }

                write!(f, "fn ({}) {{\n", params)?;
                write!(f, "{}", body)?;
                write!(f, "}}")?;
                return Ok(());
            }
        }
    }
}

impl Node {
    pub fn token_lit(&self) -> String {
        match self {
            Node::Program(nodes) => {
                return nodes[0].token_lit();
            }
            Node::ArrayExpr(nodes) => {
                return nodes[0].token_lit();
            }
            Node::HashExpr(hash_expr) => {
                return "hash".to_string();
            }
            Node::IndexExpr(left, right) => {
                return left.token_lit();
            }
            Node::IfExpr(condition, consequence, alternative) => {
                return condition.token_lit();
            }
            Node::CallExpr(function, arguments) => {
                return function.token_lit();
            }
            Node::PrefixExpr(operator, right) => {
                return operator.to_string();
            }
            Node::InfixExpr(left, operator, right) => {
                return operator.to_string();
            }
            Node::BlockStmt(stmts) => {
                return stmts[0].token_lit();
            }
            Node::LetStmt(token, value) => {
                return token.to_string();
            }
            Node::ReturnStmt(return_value) => {
                return return_value.token_lit();
            }
            Node::ExprStmt(expression) => {
                return expression.token_lit();
            }
            Node::IdentifierLit(identifier) => {
                return identifier.to_string();
            }
            Node::IntegerLit(integer) => {
                return integer.to_string();
            }
            Node::StringLit(string) => {
                return string.to_string();
            }
            Node::BooleanLit(boolean) => {
                return boolean.to_string();
            }
            Node::FunctionExpr(parameters, body) => {
                return parameters[0].token_lit();
            }
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct HashExpr(pub HashMap<String, Node>);
impl Eq for HashExpr {
}

impl Node {
    pub fn from_token(token: &Token) -> Self {
        match token.token_type {
            TokenType::STRING => Node::StringLit(token.to_string()),
            TokenType::INT => Node::IntegerLit(token.to_string().parse::<i64>().unwrap()),
            TokenType::TRUE => Node::BooleanLit(true),
            TokenType::FALSE => Node::BooleanLit(false),
            _ => Node::IdentifierLit(token.to_string()),
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_string() {
        let program = Node::Program(vec![
            Node::LetStmt(
                Box::new(Node::IdentifierLit("myVar".to_string())),
                Box::new(Node::IdentifierLit("five".to_string())),
            ),
            Node::ReturnStmt(Box::new(Node::IdentifierLit("five".to_string()))),
        ]);

        println!("{}", program.to_string());
        if program.to_string() != "let myVar = five;return five;" {
            panic!(
                "expected let myVar = five;return five;, got {}",
                program.to_string()
            );
        }
    }
}
