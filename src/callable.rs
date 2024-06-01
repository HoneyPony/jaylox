use crate::interpreter::InterpRes;
use crate::{interpreter::Interpreter, scanner::LoxValue};
use crate::stmt::Function;

use std::{rc::Rc, time::{SystemTime, UNIX_EPOCH}};

fn get_epoch_ms() -> f64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_millis() as f64
}

#[derive(Clone)]
pub enum LoxCallable {
	FnClock,

	FnLox(Rc<Function>)
}

impl PartialEq for LoxCallable {
	fn eq(&self, other: &Self) -> bool {
		match (self, other) {
			(Self::FnLox(l0), Self::FnLox(r0)) => Rc::ptr_eq(l0, r0),
			_ => core::mem::discriminant(self) == core::mem::discriminant(other),
		}
	}
}

impl LoxCallable {
	pub fn call(&mut self, interpreter: &mut Interpreter, arguments: Vec<LoxValue>) -> InterpRes {
		match self {
			LoxCallable::FnClock => {
				return Ok(LoxValue::Number(get_epoch_ms()))
			},

			LoxCallable::FnLox(func) => {
				interpreter.environment.push_scope();
				for i in 0..func.parameters.len() {
					interpreter.environment.define(
						func.parameters.get(i).unwrap().lexeme.clone(),
						arguments.get(i).unwrap().clone()
					)
				}

				interpreter.execute_block_then_pop(&func.body)?;
				// TODO: Return value
				return Ok(LoxValue::Nil);
			}
		}
	}

	pub fn arity(&self) -> usize {
		match self {
			LoxCallable::FnClock => 0,
			LoxCallable::FnLox(rc) => rc.parameters.len(),
		}
	}
}

impl ToString for LoxCallable {
	fn to_string(&self) -> String {
		match self {
			LoxCallable::FnClock => "<native fn>".into(),
			LoxCallable::FnLox(func) => func.name.lexeme.clone()
		}
	}
}