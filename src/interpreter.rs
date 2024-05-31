use crate::{environment::{self, Environment}, expr::Expr, scanner::{Token, TokenLiteral}, stmt::Stmt};

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

pub type InterpRes = Result<TokenLiteral, InterpErr>;

pub struct Interpreter<'a, 'b> {
	lox: &'a mut Lox,
	environment: &'b mut Environment
}

fn res_to_number(op: &Token, value: TokenLiteral) -> Result<f64, InterpErr> {
	if let TokenLiteral::Number(x) = value {
		return Ok(x)
	}
	return Err(InterpErr::new(op, format!("Operands should be numbers, when evaluating {0}", value.to_string())))
}

fn number_to_res(value: f64) -> InterpRes {
	return Ok(TokenLiteral::Number(value))
}

fn res_to_bool(op: &Token, value: TokenLiteral) -> Result<bool, InterpErr> {
	if let TokenLiteral::Bool(x) = value {
		return Ok(x)
	}
	return Err(InterpErr::new(op, format!("Operand should be bool")));
}

fn bool_to_res(value: bool) -> InterpRes {
	return Ok(TokenLiteral::Bool(value))
}

fn lox_equals(lhs: TokenLiteral, rhs: TokenLiteral, invert: bool) -> InterpRes {
	let result = lhs == rhs;
	if invert { return Ok(TokenLiteral::Bool(!result)); }

	return Ok(TokenLiteral::Bool(result));
}

fn is_truthy(value: TokenLiteral) -> bool {
	match value {
		TokenLiteral::None => false,
		TokenLiteral::String(_) => true,
		TokenLiteral::Number(_) => true,
		TokenLiteral::Bool(v) => v,
	}
}

fn string_to_res(value: String) -> InterpRes {
	return Ok(TokenLiteral::String(value))
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
				if let TokenLiteral::String(lhs) = left {
					if let TokenLiteral::String(rhs) = right {
						return string_to_res(lhs + &rhs);
					}
				}
				else if let TokenLiteral::Number(lhs) = left {
					if let TokenLiteral::Number(rhs) = right {
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
				return bool_to_res(!is_truthy(right));
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
			}
		}
	}

	fn execute(&mut self, stmt: &Stmt) -> Result<(), InterpErr> {
		match stmt {
			Stmt::Expression(expr) => { self.evaluate(expr)?; },
			Stmt::Print(expr) => {
				let value = self.evaluate(expr)?;
				println!("{}", value.to_string());
			},
			Stmt::Var { name, initializer } => {
				let mut value = TokenLiteral::None;
				if let Some(expr) = initializer {
					value = self.evaluate(expr)?;
				}
				self.environment.define(name.lexeme.clone(), value);
				
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