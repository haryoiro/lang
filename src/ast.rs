use crate::token;

pub trait Node {
    fn token_literal(&self) -> String;
}
#[derive(Debug)]
pub enum Statement {
    Let {
        name:  Expression,
        value: Expression,
    },
}

pub trait IStatement: Node {
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
        }
    }
}

#[derive(Debug)]
pub enum Expression {
    Identifier(String),
}

impl IExpression for Expression {
    fn expression_node(&self) {
    }
}

trait IExpression: Node {
    fn expression_node(&self);
}

impl Node for Expression {
    fn token_literal(&self) -> String {
        match self {
            Expression::Identifier(name) => name.to_string(),
        }
    }
}

pub struct Program {
    pub statements: Vec<Statement>,
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
