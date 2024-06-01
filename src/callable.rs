use crate::{interpreter::Interpreter, scanner::LoxValue};

#[derive(Clone, PartialEq, Debug)]
pub enum LoxCallable {

}

impl LoxCallable {
	pub fn call(&mut self, interpreter: &mut Interpreter, arguments: Vec<LoxValue>) -> LoxValue {
		LoxValue::Nil
	}
}