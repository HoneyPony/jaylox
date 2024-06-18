// This file is generated by build.rs
include!(concat!(env!("OUT_DIR"), "/stmt.gen.rs"));

use std::{collections::HashMap, rc::Rc};
use std::cell::RefCell;

use crate::{callable::LoxCallable, environment::Environment, expr::ExprErr};

pub type StmtRes = Result<Stmt, ExprErr>;

// Function info: lots of stuff, so carry it around Rc'd.
pub struct Function {
	pub name: Token,
	pub parameters: Vec<Token>,
	pub body: Vec<Stmt>,
	pub is_initializer: bool,
}

// Similar to functions, we put the class info in a struct that is Rc'd.

pub struct LoxClass {
	pub name: String,
	pub methods: HashMap<String, (Rc<Function>, Rc<RefCell<Environment>>)>,
}

impl Function {
	pub fn new(name: Token, parameters: Vec<Token>, body: Vec<Stmt>, is_initializer: bool) -> Self {
		Function { name, parameters, body, is_initializer }
	}

	pub fn new_as_rc(name: Token, parameters: Vec<Token>, body: Vec<Stmt>, is_initializer: bool) -> Rc<Self> {
		Rc::new(Self::new(name, parameters, body, is_initializer))
	}

	pub fn new_as_stmt(name: Token, parameters: Vec<Token>, body: Vec<Stmt>, is_initializer: bool) -> Stmt {
		Stmt::Function(Rc::new(Self::new(name, parameters, body, is_initializer)))
	}

	pub fn new_as_stmt_res(name: Token, parameters: Vec<Token>, body: Vec<Stmt>, is_initializer: bool) -> StmtRes {
		Ok(Self::new_as_stmt(name, parameters, body, is_initializer))
	}

	pub fn to_lox_value(fun: &Rc<Function>, closure: &Rc<RefCell<Environment>>) -> LoxValue {
		return LoxValue::Callable(LoxCallable::FnLox(
			Rc::clone(fun),
			Rc::clone(closure)
		))
	}
}

impl LoxClass {
	pub fn new(name: String, methods: HashMap<String, (Rc<Function>, Rc<RefCell<Environment>>)>) -> Self {
		LoxClass { name, methods }
	}

	pub fn new_as_rc(name: String, methods: HashMap<String, (Rc<Function>, Rc<RefCell<Environment>>)>) -> Rc<Self> {
		Rc::new(Self::new(name, methods))
	}

	pub fn to_lox_value(class: &Rc<LoxClass>) -> LoxValue {
		return LoxValue::Callable(LoxCallable::FnClass(
			Rc::clone(class)
		))
	}

	pub fn new_as_lox_value(name: String, methods: HashMap<String, (Rc<Function>, Rc<RefCell<Environment>>)>) -> LoxValue {
		return LoxValue::Callable(LoxCallable::FnClass(
			Self::new_as_rc(name, methods)
		))
	}

	pub fn find_raw_method_bound_to(&self, name: &str, this_binding: LoxValue) -> Option<LoxCallable> {
		self.methods.get(name).map(|(fun, env)| {
			let bound_env = Environment::new_with_enclosing(Rc::clone(env));
			bound_env.borrow_mut().define("this".into(), this_binding);
				
			LoxCallable::FnLox(Rc::clone(fun), bound_env)
		})
	}

	pub fn find_method_bound_to(&self, name: &str, this_binding: LoxValue) -> Option<LoxValue> {
		self.find_raw_method_bound_to(name, this_binding).map(|callable|
			LoxValue::Callable(callable)
		)
	}

	pub fn find_method_arity(&self, name: &str) -> Option<usize> {
		self.methods.get(name).map(|(fun, _)| fun.parameters.len())
	}
}