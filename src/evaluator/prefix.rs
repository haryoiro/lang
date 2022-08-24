use super::{is_error_object, Eval};
pub use crate::ast::Node;
use crate::{environment::Environment, object::Object};

impl Node {
    pub(crate) fn eval_prefix_expr(&self, env: &mut Environment) -> Object {
        let prefix_exp = self.clone().unwrap_prefix_expr();
        let right = prefix_exp.1.eval(env);
        if is_error_object(&right) {
            return right;
        }

        let operator = prefix_exp.0.as_str();

        match operator {
            "!" => bang_op(right),
            "-" => minus_op(right),
            _ => Object::Error(format!("unknown operator: {}", operator)),
        }
    }
}

fn bang_op(right: Object) -> Object {
    match right {
        Object::Boolean(value) => Object::Boolean(!value),
        Object::Null => Object::Boolean(true),
        _ => Object::Boolean(false),
    }
}
fn minus_op(right: Object) -> Object {
    match right {
        Object::Integer(value) => Object::Integer(-value),
        Object::Float(value) => Object::Float(-value),
        _ => Object::Error(format!("unknown operator: -")),
    }
}
