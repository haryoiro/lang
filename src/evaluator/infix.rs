use super::{is_error_object, Eval};
pub use crate::ast::Node;
use crate::{environment::Environment, object::Object};

impl Node {
    pub(crate) fn eval_infix_expr(&self, env: &mut Environment) -> Object {
        let (left, op, right) = self.clone().unwrap_infix_expr();
        let left = left.eval(env);
        if is_error_object(&left) {
            return left;
        }
        let right = right.eval(env);
        if is_error_object(&right) {
            return right;
        }

        use Object::*;
        return match (&left, &right) {
            (Integer(left), Integer(right)) => integer_infix_op(left, &op, right),
            (Boolean(left), Boolean(right)) => boolean_infix_op(left, &op, right),
            (
                String(_) | Integer(_) | Boolean(_) | Null,
                String(_) | Integer(_) | Boolean(_) | Null,
            ) => string_infix_op(&left, &op, &right),
            _ => Object::Error(format!("type mismatch: {} {} {}", left, op, right)),
        };
    }
}

fn integer_infix_op(left: &i64, op: &str, right: &i64) -> Object {
    use Object::*;
    match op {
        "+" => Integer(left + right),
        "-" => Integer(left - right),
        "*" => Integer(left * right),
        "/" => Integer(left / right),
        "<" => Boolean(left < right),
        ">" => Boolean(left > right),
        "==" => Boolean(left == right),
        "!=" => Boolean(left != right),
        _ => Object::Error(format!("unknown operator: {}", op)),
    }
}

fn boolean_infix_op(left: &bool, op: &str, right: &bool) -> Object {
    use Object::*;
    match op {
        "==" => Boolean(left == right),
        "!=" => Boolean(left != right),
        _ => Object::Error(format!("unknown operator: {}", op)),
    }
}

fn string_infix_op(left: &Object, op: &str, right: &Object) -> Object {
    use Object::*;
    let left = match &left {
        String(value) => value.clone(),
        Boolean(value) => value.to_string(),
        Integer(value) => value.to_string(),
        Float(value) => value.to_string(),
        Null => "null".to_string(),
        _ => return Object::Error(format!("unknown operator: {}", op)),
    };
    let right = match right {
        String(value) => value.clone(),
        Boolean(value) => value.to_string(),
        Integer(value) => value.to_string(),
        Float(value) => value.to_string(),
        Null => "null".to_string(),
        _ => return Object::Error(format!("unknown operator: {}", op)),
    };

    match op {
        "+" => String(format!("{}{}", left, right)),
        "==" => Boolean(left == right),
        "!=" => Boolean(left != right),
        _ => Object::Error(format!("unknown operator: {}", op)),
    }
}
