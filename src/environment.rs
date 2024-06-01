use std::collections::HashMap;

use crate::{callable::LoxCallable, interpreter::InterpErr, scanner::{LoxValue, Token}};

pub struct Environment {
	values: Vec<HashMap<String, LoxValue>>,
}

impl Environment {
	pub fn new() -> Self {
		let root = HashMap::new();
		Environment {
			values: vec![root],
		}
	}

	pub fn new_with_globals() -> Self {
		let mut env = Self::new();
		env.define("clock".into(), LoxValue::Callable(LoxCallable::FnClock));

		env
	}

	pub fn define(&mut self, name: String, value: LoxValue) {
		// Note: we always have at least one HashMap in the vec.
		self.values.last_mut().unwrap().insert(name, value);
	}

	pub fn get(&self, name: &Token) -> Result<&LoxValue, InterpErr> {
		for map in self.values.iter().rev() {
			if let Some(value) = map.get(&name.lexeme) {
				return Ok(value)
			}
		}

		Err(InterpErr::new(name, format!("Undefined variable '{}'", name.lexeme)))
	}

	pub fn assign(&mut self, name: &Token, value: LoxValue) -> Result<LoxValue, InterpErr> {
		for map in self.values.iter_mut().rev() {
			if let Some(ptr) = map.get_mut(&name.lexeme) {
				*ptr = value.clone();
				return Ok(value);
			}
		}
		
		Err(InterpErr::new(name, format!("Undefined variable '{}'", name.lexeme)))
	}

	pub fn push_scope(&mut self) {
		self.values.push(HashMap::new())
	}

	pub fn pop_scope(&mut self) {
		assert!(self.values.len() > 1);
		self.values.pop();
	}
}

