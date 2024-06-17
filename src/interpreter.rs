use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{environment::Environment, expr::Expr, scanner::{LoxValue, Token}, stmt::{Function, LoxClass, Stmt}};

use crate::scanner::TokenType::*;
use crate::Lox;

pub struct InterpErr {
	pub token: Token,
	pub message: String
}

impl InterpErr {
	pub fn new(token: &Token, message: String) -> InterpUnwind {
		let token = token.clone();
		InterpUnwind::Error(InterpErr { token, message })
	}
}

// Use the Result system to implement stack unwinding. This is needed for
// return statements and could also be used for break, etc.
pub enum InterpUnwind {
	Error(InterpErr),
	ReturnValue(LoxValue)
}

pub type InterpRes = Result<LoxValue, InterpUnwind>;

pub struct Interpreter<'a> {
	pub lox: &'a mut Lox,
	pub environment: Rc<RefCell<Environment>>,
}

fn res_to_number(op: &Token, value: LoxValue) -> Result<f64, InterpUnwind> {
	if let LoxValue::Number(x) = value {
		return Ok(x)
	}
	return Err(InterpErr::new(op, format!("Operands should be numbers, when evaluating {0}", value.to_string())))
}

fn number_to_res(value: f64) -> InterpRes {
	return Ok(LoxValue::Number(value))
}

fn bool_to_res(value: bool) -> InterpRes {
	return Ok(LoxValue::Bool(value))
}

fn lox_equals(lhs: LoxValue, rhs: LoxValue, invert: bool) -> InterpRes {
	let result = lhs == rhs;
	if invert { return Ok(LoxValue::Bool(!result)); }

	return Ok(LoxValue::Bool(result));
}

fn is_truthy(value: &LoxValue) -> bool {
	match value {
		LoxValue::Nil => false,
		LoxValue::String(_) => true,
		LoxValue::Number(_) => true,
		LoxValue::Bool(v) => *v,
		LoxValue::Callable(_) => true,
		LoxValue::Instance(_) => true,
	}
}

fn string_to_res(value: String) -> InterpRes {
	return Ok(LoxValue::String(Rc::from(value.into_boxed_str())))
}

impl<'a> Interpreter<'a> {
	pub fn new(lox: &'a mut Lox, environment: Rc<RefCell<Environment>>) -> Self {
		Interpreter { lox, environment }
	}

	fn binary_op(&mut self, left: &Expr, op: &Token, right: &Expr) -> InterpRes {
		let left = self.evaluate(left)?;
		let right = self.evaluate(right)?;

		match op.typ {
			Minus => {
				let lhs = res_to_number(op, left)?;
				let rhs = res_to_number(op, right)?;
				return number_to_res(lhs - rhs);
			},
			Plus => {
				if let LoxValue::String(lhs) = left {
					if let LoxValue::String(rhs) = right {
						return string_to_res(lhs.to_string() + &rhs);
					}
				}
				else if let LoxValue::Number(lhs) = left {
					if let LoxValue::Number(rhs) = right {
						return number_to_res(lhs + rhs);
					}
				}
				return Err(InterpErr::new(op, format!("Operands to '+' should be numbers or strings.")));
			},
			Slash => {
				let lhs = res_to_number(op, left)?;
				let rhs = res_to_number(op, right)?;
				return number_to_res(lhs / rhs);
			},
			Star => {
				let lhs = res_to_number(op, left)?;
				let rhs = res_to_number(op, right)?;
				return number_to_res(lhs * rhs);
			},
			BangEqual => return lox_equals(left, right, true),
			EqualEqual => return lox_equals(left, right, false),
			Greater => {
				let lhs = res_to_number(op, left)?;
				let rhs: f64 = res_to_number(op, right)?;
				return bool_to_res(lhs > rhs);
			},
			GreaterEqual => {
				let lhs = res_to_number(op, left)?;
				let rhs = res_to_number(op, right)?;
				return bool_to_res(lhs >= rhs);
			},
			Less => {
				let lhs = res_to_number(op, left)?;
				let rhs = res_to_number(op, right)?;
				return bool_to_res(lhs < rhs);
			},
			LessEqual => {
				let lhs = res_to_number(op, left)?;
				let rhs = res_to_number(op, right)?;
				return bool_to_res(lhs <= rhs);
			},

			_ => {
				unreachable!()
			}
		}
	}

	fn unary_op(&mut self, operator: &Token, right: &Expr) -> InterpRes {
		let right = self.evaluate(right)?;

		match operator.typ {
			Bang => {
				return bool_to_res(!is_truthy(&right));
			},
			Minus => {
				let rhs = res_to_number(operator, right)?;
				return number_to_res(-rhs);
			}
			_ => unreachable!()
		}
	}

	pub fn evaluate(&mut self, expr: &Expr) -> InterpRes {
		match expr {
			Expr::Binary { left, operator, right } => {
				return self.binary_op(left, operator, right);
			}
			Expr::Grouping(inner) => return self.evaluate(inner),
			Expr::Literal(value) => return Ok(value.clone()),
			Expr::Unary { operator, right } => {
				return self.unary_op(operator, right);
			},
			Expr::Variable{ name, resolved } => {
				// TODO: Consider how to speed this up in the case of strings.
				// Maybe look into Cow?
				// The other option is some kind of String arena
				return Ok(self.environment.borrow().get_at(name, *resolved)?.clone());
			},
			Expr::This { keyword, resolved } => {
				return Ok(self.environment.borrow().get_at(keyword, *resolved)?.clone());
			}
			Expr::Assign { name, value , resolved } => {
				let value = self.evaluate(value)?;
				return self.environment.borrow_mut().assign_at(name, value, *resolved);
			},
			Expr::Logical { left, operator, right } => {
				let left = self.evaluate(left)?;
				
				match operator.typ {
					Or => {
						if is_truthy(&left) { return Ok(left); }
					},
					And => {
						if !is_truthy(&left) { return Ok(left); }
					},
					_ => unreachable!()
				}

				return self.evaluate(right);
			},
			Expr::Call { callee, paren, arguments } => {
				let callee = self.evaluate(callee)?;

				let LoxValue::Callable(mut function) = callee else {
					return Err(InterpErr::new(paren, "Can only call functions and classes.".into()));
				};

				if function.arity() != arguments.len() {
					return Err(InterpErr::new(paren, format!("Expected {} arguments but got {}.", function.arity(), arguments.len())));
				}

				let mut argument_values = vec![];
				for argument in arguments {
					argument_values.push(self.evaluate(argument)?);
				}

				return function.call(self, argument_values)
			},
			Expr::Get { object, name } => {
				let object = self.evaluate(object)?;
				if let LoxValue::Instance(ptr) = object {
					return self.lox.get(ptr).get(name);
				}

				return Err(InterpErr::new(name, "Only instances have properties.".into()));
			},
			Expr::Set { object, name, value } => {
				let object = self.evaluate(object)?;

				let LoxValue::Instance(ptr) = object else {
					return Err(InterpErr::new(name, "Only instances have fields.".into()));
				};

				let value = self.evaluate(value)?;

				self.lox.get_mut(ptr).set(name, value.clone());
				return Ok(value);
			},
		}
	}

	/// Does not handle making a new environment.
	/// Helper function so that we can correctly pop_scope() after running into
	/// an error.
	fn execute_block_loop(&mut self, block: &Vec<Stmt>) -> Result<(), InterpUnwind> {
		for stmt in block {
			self.execute(stmt)?;
		}

		Ok(())
	}

	pub fn execute_block(&mut self, block: &Vec<Stmt>, enviroment: Rc<RefCell<Environment>>) -> Result<(), InterpUnwind> {
		let previous = std::mem::replace(&mut self.environment, enviroment);
		let result = self.execute_block_loop(block);

		self.environment = previous;
		result
	}

	fn execute(&mut self, stmt: &Stmt) -> Result<(), InterpUnwind> {
		match stmt {
			Stmt::Expression(expr) => { self.evaluate(expr)?; },
			Stmt::Print(expr) => {
				let value = self.evaluate(expr)?;
				println!("{}", value.to_printable_string(self.lox));
			},
			Stmt::Var { name, initializer } => {
				let mut value = LoxValue::Nil;
				if let Some(expr) = initializer {
					value = self.evaluate(expr)?;
				}
				self.environment.borrow_mut().define(name.lexeme.clone(), value);
				
			},
			Stmt::Block(statements) => {
				let new_scope = Environment::new_with_enclosing(Rc::clone(&self.environment));
				self.execute_block(statements, new_scope)?;
			},
			Stmt::If { condition, then_branch, else_branch } => {
				let is_true = is_truthy(&self.evaluate(condition)?);
				if is_true {
					self.execute(then_branch)?;
				}
				else {
					if let Some(else_branch) = else_branch {
						self.execute(else_branch)?;
					}
				}
			},
			Stmt::While { condition, body } => {
				while is_truthy(&self.evaluate(condition)?) {
					self.execute(body)?;
				}
			},
			Stmt::Function(func) => {
				// The closure is captured when the function is seen (at least for now)
				self.environment.borrow_mut().define(
					func.name.lexeme.clone(),
					 Function::to_lox_value(func, &self.environment))
			},
			Stmt::Return { keyword: _, value } => {
				let mut return_value = LoxValue::Nil;
				if let Some(value) = value {
					return_value = self.evaluate(value)?;
				}
				// Note: In jlox, the semantic is that a return value with no
				// expression still results in a nil value. This is the same here.
				return Err(InterpUnwind::ReturnValue(return_value));
			},
			Stmt::Class { name, methods } => {
				let mut method_map = HashMap::new();

				// NOTE: This means that a class local to a function, etc,
				// will be very slow -- because every time that function is run, this
				// hash map is created from scratch.
				//
				// It is "fine" for top-level clases as it will only need to be run
				// once, but it is definitely sub-optimal.
				for method in methods {
					// Note: Original Crafting Interpreters stores the Environment
					// inside the Function. Here we store them as tuples... it's a
					// little ugly, but works OK for non-class Functions.
					method_map.insert(method.name.lexeme.clone(),
					 (Rc::clone(method), Rc::clone(&self.environment)));
				}
				
				// Wrap the class value into a Callable
				self.environment.borrow_mut().define(
					name.lexeme.clone(),
					LoxClass::new_as_lox_value(name.lexeme.clone(), method_map)
				);
			}
		}

		Ok(())
	}

	fn interpret_to_result(&mut self, stmts: &Vec<Stmt>) -> Result<(), InterpUnwind> {
		for stmt in stmts {
			self.execute(stmt)?;
		}

		Ok(())
	}

	pub fn interpret(&mut self, stmts: &Vec<Stmt>) {
		match self.interpret_to_result(stmts) {
			Ok(_) => {
				// Do nothing
			}
			Err(err) => {
				self.lox.runtime_error(&err)
			}
		}
	}
}