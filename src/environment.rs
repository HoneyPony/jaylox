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

	pub fn get_at(&self, name: &Token, depth: Option<u32>) -> Result<LoxValue, InterpUnwind> {
		match depth {
			Some(0) => {
				if let Some(value) = self.values.get(&name.lexeme) {
					return Ok(value.clone())
				}

				return Err(InterpErr::new(name, format!("Undefined variable '{}'", name.lexeme)));
			},
			Some(x) => {
				// The parent must exist, otherwise the Resolver made a mistake.
				return self.parent.as_ref().unwrap().borrow_mut().get_at(name, Some(x - 1));
			},

			// TODO: If we store an explicit reference to the globals, we can skip directly there...
			None => {
				if let Some(parent) = self.parent.as_ref() {
					return parent.borrow_mut().get_at(name, None);
				}
				return self.get_at(name, Some(0));
			}
		}
	}

	pub fn assign_at(&mut self, name: &Token, value: LoxValue, depth: Option<u32>) -> Result<LoxValue, InterpUnwind> {
		match depth {
			Some(0) => {
				if let Some(ptr) = self.values.get_mut(&name.lexeme) {
					*ptr = value.clone();
					return Ok(value);
				}

				return Err(InterpErr::new(name, format!("Undefined variable '{}'", name.lexeme)));
			},
			Some(x) => {
				// The parent must exist, otherwise the Resolver made a mistake.
				return self.parent.as_mut().unwrap().borrow_mut().assign_at(name, value, Some(x - 1));
			},
			None => {
				if let Some(parent) = self.parent.as_mut() {
					return parent.borrow_mut().assign_at(name, value, None);
				}
				return self.assign_at(name, value, Some(0));
			}
		}
	}

	//pub fn push_scope(&mut self) {
	//	self.values.push(HashMap::new())
	//}

	//pub fn pop_scope(&mut self) {
	//	assert!(self.values.len() > 1);
	//	self.values.pop();
	//}
}

