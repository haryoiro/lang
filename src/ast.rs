use std::fmt::{Display, Error, Formatter};


pub trait Node {
    fn token_literal(&self) -> String;
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

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Expression {
    Identifier {
        value: String,
    },
    Integer {
        value: i64,
    },
    String {
        value: String,
    },
    Boolean {
        value: bool,
    },
    Array {
        elements: Vec<Expression>,
    },
    Index {
        left: Box<Expression>,
        index: Box<Expression>,
    },
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

impl Node for Expression {
    fn token_literal(&self) -> String {
        match self {
            Expression::Identifier { value } => value.to_string(),
            Expression::Integer { value } => value.to_string(),
            Expression::String { value } => value.to_string(),
            Expression::Boolean { value } => value.to_string(),
            Expression::Array { .. } => "array".to_string(),
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
            Expression::Identifier { value, .. } => write!(f, "{}", value),
            Expression::Integer { value, .. } => write!(f, "{}", value),
            Expression::String { value, .. } => write!(f, "{}", value),
            Expression::Boolean { value, .. } => write!(f, "{}", value),
            Expression::Array { elements } => {
                write!(f, "[")?;
                for (i, element) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", element.to_string())?;
                }
                write!(f, "]")
            },
            Expression::Index { left, index } => {
                write!(f, "{}[{}]", left.to_string(), index.to_string())
            },
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

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_string() {
        let program = Program {
            statements: vec![
                Statement::Let {
                    name:  Expression::Identifier {
                        value: "myVar".to_string(),
                    },
                    value: Expression::Identifier {
                        value: "five".to_string(),
                    },
                },
                Statement::Return {
                    value: Expression::Identifier {
                        value: "five".to_string(),
                    },
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
