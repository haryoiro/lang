use std::{
    collections::HashMap,
    sync::{Arc, Mutex}, fmt::Display,
};

use crate::{
    object::Object,
};

#[derive(Debug, Clone)]
pub struct Environment {
    /// Environment
    ///
    /// 変数名と関連付けられた値を記憶する
    ///
    /// # Examples
    /// ```
    /// ```
    map: Arc<Mutex<HashMap<String, Object>>>,

    outer: Option<Arc<Mutex<Environment>>>,

    scope_id: u32,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            map: Arc::new(Mutex::new(HashMap::new())),
            outer: None,
            scope_id: 0,
        }
    }
    pub fn new_enclosed(outer: &Environment) -> Environment {
        let mut env = Environment::new();
        env.outer = Some(Arc::new(Mutex::new(outer.clone())));
        env.scope_id = outer.scope_id + 1;
        env
    }

    pub fn get(&self, name: &str) -> Result<Object, String> {
        // 現在の環境から変数を探す
        if let Some(obj) = &self.map.as_ref().lock().unwrap().get(name) {
            return Ok(obj.clone().clone());
        } else {
            // 現在の環境が存在しない場合は、外側の環境を探す
            self.get_in_outer(name)
        }
    }

    fn get_in_outer(&self, name: &str) -> Result<Object, String> {
        // 親環境の存在確認
        match self.outer.as_ref() {
            Some(a) => {
                // outerをロックして、親環境から変数を探す
                if let Ok(o) = a.lock() {
                    match o.get(name) {
                        Ok(obj) => return Ok(obj.clone()),
                        Err(e) => return Err(e),
                    };
                } else {
                    return Err(format!("identifier not found: {}", name))
                }
            }
            None => return Err(format!("identifier not found: {}", name)),
        }
    }

    pub fn set(&mut self, name: String, value: Object) {
        self.map.lock().unwrap().insert(name, value);
    }

    pub fn keys(&self) -> Vec<String> {
        self.map.lock().unwrap().keys().map(|s| s.to_string()).collect()
    }

    pub fn len(&self) -> usize {
        self.map.lock().unwrap().len()
    }

    pub fn all_keys(&self) -> Vec<String> {
        let mut keys = self.keys();
        match &self.outer {
            Some(outer) => {
                let outer_keys = outer.lock().unwrap().all_keys();
                keys.extend(outer_keys);
            }
            None => {}
        }
        keys
    }
}

impl PartialEq for Environment {
    fn eq(&self, other: &Environment) -> bool {
        self.scope_id == other.scope_id
    }
}

impl Display for Environment {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut keys = self.keys();
        keys.sort();
        for key in keys {
            let value = self.get(&key).unwrap();
            write!(f, "\nscope: {}\nkey: {}\nvalue: {}\n", self.scope_id, key, value).unwrap();
        }
        Ok(())
    }
}
