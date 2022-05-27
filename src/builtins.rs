use std::collections::HashMap;

use once_cell::sync::Lazy;

use crate::object::{Object, IObject};

type BuiltinFunc = fn(Vec<Object>) -> Object;

pub static BUILTINS: Lazy<
    HashMap<
        String,
        Object
    >
> = Lazy::new(|| {
    let mut bfb = BuiltinFuncBuilder::new();
    bfb.set("len", builtin_len as BuiltinFunc)
        .set("first", builtin_first as BuiltinFunc)
        .set("last", builtin_last as BuiltinFunc)
        .set("rest", builtin_rest as BuiltinFunc)
        .set("push", builtin_push as BuiltinFunc)
        .set("pop", builtin_pop as BuiltinFunc)
        .set("builtins", builtin_builtins as BuiltinFunc)
    .build()
});

fn builtin_len(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        return Object::Error(format!("wrong number of arguments. got={}, want=1", args.len()));
    }
    let argument = &args[0];
    match argument {
        Object::String(string) => Object::Integer(string.len() as i64),
        Object::Array(array) => Object::Integer(array.len() as i64),
        _ => Object::Error(format!("argument to `len` not supported, got {}", argument.typ())),
    }
}

fn builtin_first(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        return Object::Error(format!("wrong number of arguments. got={}, want=1", args.len()));
    }
    let argument = &args[0];
    match argument {
        Object::Array(array) => {
            if array.len() > 0 {
                array[0].clone()
            } else {
                Object::Null
            }
        }
        _ => Object::Error(format!("argument to `first` not supported, got {}", argument.typ())),
    }
}

fn builtin_last(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        return Object::Error(format!("wrong number of arguments. got={}, want=1", args.len()));
    }
    let argument = &args[0];
    match argument {
        Object::Array(array) => {
            if array.len() > 0 {
                array[array.len() - 1].clone()
            } else {
                Object::Null
            }
        }
        _ => Object::Error(format!("argument to `last` not supported, got {}", argument.typ())),
    }
}

fn builtin_rest(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        return Object::Error(format!("wrong number of arguments. got={}, want=1", args.len()));
    }
    let argument = &args[0];
    match argument {
        Object::Array(array) => {
            if array.len() > 0 {
                let mut new_array = Vec::new();
                for i in 1..array.len() {
                    new_array.push(array[i].clone());
                }
                Object::Array(new_array)
            } else {
                Object::Null
            }
        }
        _ => Object::Error(format!("argument to `rest` not supported, got {}", argument.typ())),
    }
}

fn builtin_push(args: Vec<Object>) -> Object {
    if args.len() != 2 {
        return Object::Error(format!("wrong number of arguments. got={}, want=2", args.len()));
    }
    let argument = &args[0];
    match argument {
        Object::Array(array) => {
            let mut new_array = array.clone();
            new_array.push(args[1].clone());
            Object::Array(new_array)
        }
        _ => Object::Error(format!("argument to `push` not supported, got {}", argument.typ())),
    }
}

fn builtin_pop(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        return Object::Error(format!("wrong number of arguments. got={}, want=1", args.len()));
    }
    let argument = &args[0];
    match argument {
        Object::Array(array) => {
            if array.len() > 0 {
                let mut new_array = array.clone();
                new_array.pop();
                Object::Array(new_array)
            } else {
                Object::Null
            }
        }
        _ => Object::Error(format!("argument to `pop` not supported, got {}", argument.typ())),
    }
}

fn builtin_builtins(args: Vec<Object>) -> Object {
    if args.len() != 0 {
        return Object::Error(format!("wrong number of arguments. got={}, want=0", args.len()));
    }
    Object::Array(BUILTINS.keys().map(|key| Object::String(key.to_string())).collect())
}

struct BuiltinFuncBuilder {
    pub map: HashMap<String, Object>,
}
impl BuiltinFuncBuilder {
    pub fn new() -> BuiltinFuncBuilder  {
        BuiltinFuncBuilder {
            map: HashMap::new(),
        }
    }
    pub fn set(&mut self, name: &str, func: fn(Vec<Object>) -> Object) -> &mut Self {
        self.map.insert(name.to_string(), Object::Builtin { name: name.to_string(), func });
        self
    }
    pub fn build(&mut self) -> HashMap<String, Object> {
        self.map.clone()
    }
}
