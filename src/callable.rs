use crate::{interpreter::Interpreter, scanner::LoxValue};

use std::time::{SystemTime, UNIX_EPOCH};

fn get_epoch_ms() -> f64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_millis() as f64
}

#[derive(Clone, PartialEq, Debug)]
pub enum LoxCallable {
	FnClock,
}

impl LoxCallable {
	pub fn call(&mut self, interpreter: &mut Interpreter, arguments: Vec<LoxValue>) -> LoxValue {
		match self {
			LoxCallable::FnClock => {
				return LoxValue::Number(get_epoch_ms())
			},
		}
	}

	pub fn arity(&self) -> usize {
		match self {
			LoxCallable::FnClock => 0,
		}
	}
}