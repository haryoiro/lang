use super::{is_error_object, Eval};
pub use crate::ast::Node;
use crate::{environment::Environment, object::Object};

impl Node {
    pub(crate) fn eval_index_expr(&self, env: &mut Environment) -> Object {
        let left = self.clone().unwrap_index_expr().0.eval(env);
        if is_error_object(&left) {
            return left;
        }
        let index = self.clone().unwrap_index_expr().1.eval(env);
        if is_error_object(&index) {
            return index;
        }
        match (&left, &index) {
            (Object::Array(array), Object::Integer(index)) => {
                let mut index = *index;
                let len = array.len() as i64;
                if index >= len {
                    return Object::Error(format!("index out of bounds: {} > {}", index, len - 1));
                }
                if index < 0 {
                    let new_index = (len - 1 - !index);
                    println!("not len: {}", !len + 1);
                    println!("new_index: {}", new_index);
                    if new_index < 0 {
                        return Object::Error(format!(
                            "index out of bounds: {} > {}",
                            index,
                            !len + 1
                        ));
                    }
                    index = new_index;
                };
                array[index as usize].clone()
            }
            (Object::Hash(pairs), key) => {
                let key = match key {
                    Object::String(s) => s,
                    _ => return Object::Error(format!("Invalid key type: {}", key)),
                };
                let pair = pairs
                    .iter()
                    .find(|pair| &pair.0.clone().unwrap_string() == key);
                match pair {
                    Some(pair) => pair.1.clone(),
                    None => Object::Null,
                }
            }
            _ => return Object::Error(format!("index operator not supported: {}", left)),
        }
    }
}
