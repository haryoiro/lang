use std::{
    fmt::{Display, Formatter},
    hash::{Hash, Hasher},
};

use crate::{
    ast::{Expression, Statement, Literal},
    environment::Environment,
};

pub type ObjectType = &'static str;

pub const INTEGER_TYPE: ObjectType = "INTEGER";
pub const STRING_TYPE: ObjectType = "STRING";
pub const BOOLEAN_TYPE: ObjectType = "BOOLEAN";
pub const ARRAY_TYPE: ObjectType = "ARRAY";
pub const HASH_TYPE: ObjectType = "HASH";
pub const NULL_TYPE: ObjectType = "NULL";
pub const RETURN_VALUE_TYPE: ObjectType = "RETURN";
pub const ERROR_TYPE: ObjectType = "ERROR";
pub const FUNCTION_TYPE: ObjectType = "FUNCTION";
pub const BUILTIN_TYPE: ObjectType = "BUILTIN";

#[derive(PartialEq, Debug, Clone)]
pub enum Object {
    Integer(i64),
    String(String),
    Boolean(bool),
    Array(Vec<Object>),
    Hash(Vec<(Object, Object)>),
    Null,
    ReturnValue(Box<Object>),
    Error(String),
    Function {
        parameters: Vec<Expression>,
        body:       Box<Statement>,
        env:        Environment,
    },
    Builtin {
        name: String,
        func: fn(Vec<Object>) -> Object,
    },
}

pub trait IObject {
    fn typ(&self) -> ObjectType;
}

impl Object {
    pub fn from_literal(literal: &Literal) -> Object {
        match literal {
            Literal::Integer(integer) => Object::Integer(*integer),
            Literal::String(string) => Object::String(string.clone()),
            Literal::Boolean(boolean) => Object::Boolean(*boolean),
            Literal::Identifier(identifier) => Object::String(identifier.clone()),
        }
    }
}

impl IObject for Object {
    fn typ(&self) -> ObjectType {
        match self {
            Object::Integer(_) => INTEGER_TYPE,
            Object::String(_) => STRING_TYPE,
            Object::Boolean(_) => BOOLEAN_TYPE,
            Object::Array(_) => ARRAY_TYPE,
            Object::Hash(_) => HASH_TYPE,
            Object::Null => NULL_TYPE,
            Object::ReturnValue(_) => RETURN_VALUE_TYPE,
            Object::Error(_) => ERROR_TYPE,
            Object::Function { .. } => FUNCTION_TYPE,
            Object::Builtin { .. } => BUILTIN_TYPE,
        }
    }
}

impl Hash for Object {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match *self {
            Object::Integer(ref i) => i.hash(state),
            Object::String(ref s) => s.hash(state),
            Object::Boolean(ref b) => b.hash(state),
            _ => 0.hash(state),
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        match self {
            Object::Integer(value) => write!(f, "{}", value),
            Object::String(value) => write!(f, "{}", value),
            Object::Boolean(value) => write!(f, "{}", value),
            Object::Array(elements) => {
                let mut first = true;
                write!(f, "[")?;
                for element in elements {
                    if first {
                        first = false;
                    } else {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", element)?;
                }
                write!(f, "]")
            }
            Object::Hash(pairs) => {
                let mut first = true;
                write!(f, "{{")?;
                for (key, value) in pairs {
                    if first {
                        first = false;
                    } else {
                        write!(f, ", ")?;
                    }
                    write!(f, " {}: {} ", key, value)?;
                }
                write!(f, "}}")
            }
            Object::Null => write!(f, "null"),
            Object::ReturnValue(value) => write!(f, "{}", value),
            Object::Error(message) => write!(f, "ERROR: {}", message),
            Object::Function {
                parameters, body, ..
            } => {
                let params = parameters
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<String>>()
                    .join(", ");
                write!(f, "fn ({}) {{\n{}\n}}", params, body.to_string())
            }
            Object::Builtin { name, .. } => write!(f, "BUILTIN FUNCTION: {}", name),
        }
    }
}
