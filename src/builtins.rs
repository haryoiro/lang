use std::collections::{BTreeMap, HashMap};

use once_cell::sync::Lazy;

use crate::object::{IObject, Object};

type BuiltinFunc = fn(Vec<Object>) -> Object;

pub static BUILTINS: Lazy<BTreeMap<String, Object>> = Lazy::new(|| {
    let mut bfb = BuiltinFuncBuilder::new();
    bfb.set("len", len as BuiltinFunc)
        .set("first", first as BuiltinFunc)
        .set("last", last as BuiltinFunc)
        .set("rest", rest as BuiltinFunc)
        .set("push", push as BuiltinFunc)
        .set("pop", pop as BuiltinFunc)
        .set("print", print as BuiltinFunc)
        .set("builtins", builtins as BuiltinFunc)
        .build()
});

fn len(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        return Object::Error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        ));
    }
    let argument = &args[0];
    match argument {
        Object::String(string) => Object::Integer(string.len() as i64),
        Object::Array(array) => Object::Integer(array.len() as i64),
        _ => {
            Object::Error(format!(
                "argument to `len` not supported, got {}",
                argument.typ()
            ))
        }
    }
}

fn first(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        return Object::Error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        ));
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
        _ => {
            Object::Error(format!(
                "argument to `first` not supported, got {}",
                argument.typ()
            ))
        }
    }
}

fn last(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        return Object::Error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        ));
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
        _ => {
            Object::Error(format!(
                "argument to `last` not supported, got {}",
                argument.typ()
            ))
        }
    }
}

fn rest(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        return Object::Error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        ));
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
        _ => {
            Object::Error(format!(
                "argument to `rest` not supported, got {}",
                argument.typ()
            ))
        }
    }
}

fn push(args: Vec<Object>) -> Object {
    if args.len() != 2 {
        return Object::Error(format!(
            "wrong number of arguments. got={}, want=2",
            args.len()
        ));
    }
    let argument = &args[0];
    match argument {
        Object::Array(array) => {
            let mut new_array = array.clone();
            new_array.push(args[1].clone());
            Object::Array(new_array)
        }
        _ => {
            Object::Error(format!(
                "argument to `push` not supported, got {}",
                argument.typ()
            ))
        }
    }
}

fn pop(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        return Object::Error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        ));
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
        _ => {
            Object::Error(format!(
                "argument to `pop` not supported, got {}",
                argument.typ()
            ))
        }
    }
}

fn print(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        return Object::Error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        ));
    }
    let argument = &args[0];
    match argument {
        Object::String(string) => {
            println!("{}", string);
            Object::Null
        }
        Object::Integer(integer) => {
            println!("{}", integer);
            Object::Null
        }
        Object::Boolean(boolean) => {
            println!("{}", boolean);
            Object::Null
        }
        Object::Null => {
            println!("null");
            Object::Null
        }
        Object::Array(array) => {
            for element in array {
                print!("{}", element);
            }
            Object::Null
        }
        Object::Hash(hash) => {
            let mut out = String::new();
            out.push_str("{ ");
            for (key, value) in hash {
                out.push_str(&format!("{}: {}", key, value));
                out.push_str(", ");
            }
            out.pop();
            out.pop();
            out.push_str(" }");
            println!("{}", out);
            Object::Null
        }
        Object::Function {
            parameters,
            body,
            env,
        } => {
            let params = parameters
                .iter()
                .map(|p| p.to_string())
                .collect::<Vec<String>>()
                .join(", ");
            println!("fn({}) {{{}}}", params, body);
            Object::Null
        }
        Object::Error(string) => {
            println!("{}", string);
            Object::Null
        }
        _ => {
            Object::Error(format!(
                "argument to `print` not supported, got {}",
                argument.typ()
            ))
        }
    }
}

fn builtins(args: Vec<Object>) -> Object {
    if args.len() != 0 {
        return Object::Error(format!(
            "wrong number of arguments. got={}, want=0",
            args.len()
        ));
    }
    Object::Array(
        BUILTINS
            .keys()
            .map(|key| Object::String(key.to_string()))
            .collect(),
    )
}

struct BuiltinFuncBuilder {
    pub map: BTreeMap<String, Object>,
}
impl BuiltinFuncBuilder {
    pub fn new() -> BuiltinFuncBuilder {
        BuiltinFuncBuilder {
            map: BTreeMap::new(),
        }
    }
    pub fn set(&mut self, name: &str, func: fn(Vec<Object>) -> Object) -> &mut Self {
        self.map.insert(
            name.to_string(),
            Object::Builtin {
                name: name.to_string(),
                func,
            },
        );
        self
    }
    pub fn build(&mut self) -> BTreeMap<String, Object> {
        self.map.clone()
    }
}
