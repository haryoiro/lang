use super::{is_error_object, Eval};
pub use crate::ast::Node;
use crate::{builtins::BUILTINS, environment::Environment, object::Object};

impl Node {
    pub(crate) fn eval_string_lit(&self, env: &mut Environment) -> Object {
        let value = self.clone().unwrap_string_lit();
        Object::String(value.to_owned())
    }

    pub(crate) fn eval_integer_lit(&self, env: &mut Environment) -> Object {
        let integer_lit = self.clone().unwrap_integer_lit();
        Object::Integer(integer_lit)
    }

    pub(crate) fn eval_boolean_lit(&self, env: &mut Environment) -> Object {
        let boolean_lit = self.clone().unwrap_boolean_lit();
        Object::Boolean(boolean_lit)
    }

    pub(crate) fn eval_identifier_lit(&self, env: &mut Environment) -> Object {
        let identifier = self.clone().unwrap_identifier_lit();
        let builtin = BUILTINS.get(&identifier);
        if let Some(builtin) = builtin {
            return builtin.clone();
        }

        let ident_obj = env.get(&identifier);
        if ident_obj.is_ok() {
            return ident_obj.unwrap().clone();
        }
        Object::Error(format!("identifier not found: {}", identifier))
    }
}
