use crate::{expr::Expr, scanner::{Token, TokenLiteral}};

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

fn binary_op(left: &Expr, op: &Token, right: &Expr) -> InterpRes {
	let left = evaluate(left)?;
	let right = evaluate(right)?;

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

fn unary_op(operator: &Token, right: &Expr) -> InterpRes {
	let right = evaluate(right)?;

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

pub fn evaluate(expr: &Expr) -> InterpRes {
	match expr {
		Expr::Binary { left, operator, right } => {
			return binary_op(left, operator, right);
		}
		Expr::Grouping(inner) => return evaluate(inner),
		Expr::Literal(value) => return Ok(value.clone()),
		Expr::Unary { operator, right } => {
			return unary_op(operator, right);
		},
	}
}

pub fn interpret(expr: &Expr, lox: &mut Lox) {
	match evaluate(expr) {
		Ok(result) => {
			println!("=> {0}", result.to_string());
		}
		Err(err) => {
			lox.runtime_error(&err)
		}
	}
}