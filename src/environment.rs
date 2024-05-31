use std::collections::HashMap;

use crate::{interpreter::InterpErr, scanner::{Token, TokenLiteral}};



pub struct Environment {
	values: HashMap<String, TokenLiteral>
}

impl Environment {
	pub fn new() -> Self {
		Environment { values: HashMap::new() }
	}

	pub fn define(&mut self, name: String, value: TokenLiteral) {
		let help = name.clone();
		self.values.insert(name, value);
		self.values.get(&help);
	}

	pub fn get(&self, name: &Token) -> Result<&TokenLiteral, InterpErr> {
		if let Some(value) = self.values.get(&name.lexeme) {
			return Ok(value)
		}

		Err(InterpErr::new(name, format!("Undefined variable '{}'", name.lexeme)))
	}
}

