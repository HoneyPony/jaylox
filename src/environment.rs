use std::collections::HashMap;

use crate::{interpreter::InterpErr, scanner::{Token, TokenLiteral}};

pub struct Environment {
	values: Vec<HashMap<String, TokenLiteral>>
}

impl Environment {
	pub fn new() -> Self {
		let root = HashMap::new();
		Environment { values: vec![root] }
	}

	pub fn define(&mut self, name: String, value: TokenLiteral) {
		// Note: we always have at least one HashMap in the vec.
		self.values.last_mut().unwrap().insert(name, value);
	}

	pub fn get(&self, name: &Token) -> Result<&TokenLiteral, InterpErr> {
		for map in self.values.iter().rev() {
			if let Some(value) = map.get(&name.lexeme) {
				return Ok(value)
			}
		}

		Err(InterpErr::new(name, format!("Undefined variable '{}'", name.lexeme)))
	}

	pub fn assign(&mut self, name: &Token, value: TokenLiteral) -> Result<TokenLiteral, InterpErr> {
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

