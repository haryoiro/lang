use super::{is_error_object, Eval};
pub use crate::ast::Node;
use crate::{builtins::BUILTINS, environment::Environment, object::Object};

impl Node {
    pub(crate) fn eval_hash_expr(&self, env: &mut Environment) -> Object {
        let mut pairs = vec![];
        let hash_expr = self.clone().unwrap_hash_expr();
        for (key, value) in hash_expr.0 {
            let key = Self::eval_identifier(key, env);
            if is_error_object(&key) {
                return key;
            }
            let value = value.eval(env);
            if is_error_object(&value) {
                return value;
            }
            pairs.push((key, value));
        }
        Object::Hash(pairs)
    }
    fn eval_identifier(ident: String, env: &mut Environment) -> Object {
        let builtin = BUILTINS.get(ident.as_str());
        if builtin.is_some() {
            return builtin.unwrap().clone();
        }
        let val = env.get(ident.as_str());
        if val.is_ok() {
            return val.unwrap().clone();
        }
        Object::Error(format!("identifier not found: {}", ident))
    }
}
