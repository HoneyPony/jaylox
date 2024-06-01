use std::rc::Rc;

use crate::{environment::Environment, expr::Expr, scanner::{LoxValue, Token}, stmt::{Function, Stmt}};

use crate::scanner::TokenType::*;
use crate::Lox;

pub struct InterpErr {
	pub token: Token,
	pub message: String
}

impl InterpErr {
	pub fn new(token: &Token, message: String) -> InterpErr {
		let token = token.clone();
		InterpErr { token, message }
	}
}

pub type InterpRes = Result<LoxValue, InterpErr>;

pub struct Interpreter<'a, 'b> {
	pub lox: &'a mut Lox,
	pub environment: &'b mut Environment
}

fn res_to_number(op: &Token, value: LoxValue) -> Result<f64, InterpErr> {
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
	}
}

fn string_to_res(value: String) -> InterpRes {
	return Ok(LoxValue::String(Rc::from(value.into_boxed_str())))
}

impl<'a, 'b> Interpreter<'a, 'b> {
	pub fn new(lox: &'a mut Lox, environment: &'b mut Environment) -> Self {
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
			Expr::Variable(var) => {
				// TODO: Consider how to speed this up in the case of strings.
				// Maybe look into Cow?
				// The other option is some kind of String arena
				return self.environment.get(var).cloned();
			},
			Expr::Assign { name, value } => {
				let value = self.evaluate(value)?;
				return self.environment.assign(name, value);
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
			}
		}
	}

	/// Does not handle making a new environment.
	/// Helper function so that we can correctly pop_scope() after running into
	/// an error.
	fn execute_block_loop(&mut self, block: &Vec<Stmt>) -> Result<(), InterpErr> {
		for stmt in block {
			self.execute(stmt)?;
		}

		Ok(())
	}

	pub fn execute_block_then_pop(&mut self, block: &Vec<Stmt>) -> Result<(), InterpErr> {
		let result = self.execute_block_loop(block);
		self.environment.pop_scope();

		result
	}

	fn execute_block(&mut self, block: &Vec<Stmt>) -> Result<(), InterpErr> {
		// TODO: Figure out if the environment scoping should be outside this function
		self.environment.push_scope();
		self.execute_block_then_pop(block)
	}

	fn execute(&mut self, stmt: &Stmt) -> Result<(), InterpErr> {
		match stmt {
			Stmt::Expression(expr) => { self.evaluate(expr)?; },
			Stmt::Print(expr) => {
				let value = self.evaluate(expr)?;
				println!("{}", value.to_printable_string());
			},
			Stmt::Var { name, initializer } => {
				let mut value = LoxValue::Nil;
				if let Some(expr) = initializer {
					value = self.evaluate(expr)?;
				}
				self.environment.define(name.lexeme.clone(), value);
				
			},
			Stmt::Block(statements) => {
				self.execute_block(statements)?;
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
				self.environment.define(func.name.lexeme.clone(), Function::to_lox_value(func))
			}
		}

		Ok(())
	}

	fn interpret_to_result(&mut self, stmts: &Vec<Stmt>) -> Result<(), InterpErr> {
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