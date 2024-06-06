use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{callable::LoxCallable, interpreter::InterpErr, scanner::{LoxValue, Token}};
use crate::interpreter::InterpUnwind;

pub struct Environment {
	values: HashMap<String, LoxValue>,
	parent: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
	pub fn new() -> Rc<RefCell<Self>> {
		let root = HashMap::new();
		let env = Environment {
			values: root,
			parent: None,
		};

		return Rc::new(RefCell::new(env));
	}

	pub fn new_with_enclosing(enclosing: Rc<RefCell<Environment>>) -> Rc<RefCell<Environment>> {
		let env = Self::new();
		env.borrow_mut().parent = Some(enclosing);

		env
	}

	pub fn new_with_globals() -> Rc<RefCell<Self>> {
		let env = Self::new();
		env.borrow_mut().define("clock".into(), LoxValue::Callable(LoxCallable::FnClock));

		env
	}

	pub fn define(&mut self, name: String, value: LoxValue) {
		self.values.insert(name, value);
	}

	pub fn get(&self, name: &Token) -> Result<LoxValue, InterpUnwind> {
		if let Some(value) = self.values.get(&name.lexeme) {
			return Ok(value.clone())
		}

		if let Some(parent) = &self.parent {
			return parent.borrow().get(name);
		}

		Err(InterpErr::new(name, format!("Undefined variable '{}'", name.lexeme)))
	}

	pub fn assign(&mut self, name: &Token, value: LoxValue) -> Result<LoxValue, InterpUnwind> {
		if let Some(ptr) = self.values.get_mut(&name.lexeme) {
			*ptr = value.clone();
			return Ok(value);
		}

		if let Some(parent) = &mut self.parent {
			return parent.borrow_mut().assign(name, value);
		}

		Err(InterpErr::new(name, format!("Undefined variable '{}'", name.lexeme)))
	}

	//pub fn push_scope(&mut self) {
	//	self.values.push(HashMap::new())
	//}

	//pub fn pop_scope(&mut self) {
	//	assert!(self.values.len() > 1);
	//	self.values.pop();
	//}
}

