use crate::environment::Environment;
use crate::interpreter::InterpRes;
use crate::interpreter::InterpUnwind;
use crate::stmt::LoxClass;
use crate::{interpreter::Interpreter, scanner::LoxValue};
use crate::stmt::Function;

use std::cell::RefCell;
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

	/// Store the closure inside the LoxCallable object, rather than inside the
	/// Function (as the Function basically lives inside the parser).
	FnLox(Rc<Function>, Rc<RefCell<Environment>>),

	FnClass(Rc<LoxClass>)
}

impl PartialEq for LoxCallable {
	fn eq(&self, other: &Self) -> bool {
		match (self, other) {
			(Self::FnLox(fun_l, env_l), Self::FnLox(fun_r, env_r)) => 
				Rc::ptr_eq(fun_l, fun_r) && Rc::ptr_eq(env_l, env_r),
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

			LoxCallable::FnLox(func, closure) => {
				let fn_scope = Environment::new_with_enclosing(Rc::clone(closure));
				for i in 0..func.parameters.len() {
					fn_scope.borrow_mut().define(
						func.parameters.get(i).unwrap().lexeme.clone(),
						arguments.get(i).unwrap().clone()
					)
				}

				let result = interpreter.execute_block(&func.body, fn_scope);

				// Turn return value unwinds into regular return values here.
				if let Err(InterpUnwind::ReturnValue(return_value)) = result {
					return Ok(return_value);
				}

				// Map any other value to a return value of nil. Or, propogate
				// any error / unwind value that isn't a return.
				return result.map(|_| LoxValue::Nil);
			}
			LoxCallable::FnClass(class) => {
				// TODO: It is possible that we will need to move new_instance to the interpreter when we
				// add constructors. Somehow, then, the entire instance array will have to live in the interpreter..
				// or maybe we can just invoke the constructor right here.
				return Ok(LoxValue::Instance(interpreter.lox.new_instance(Rc::clone(class))));
			},
		}
	}

	pub fn arity(&self) -> usize {
		match self {
			LoxCallable::FnClock => 0,
			LoxCallable::FnLox(rc, _) => rc.parameters.len(),
			// TODO: Real arity implementation.
			LoxCallable::FnClass(_) => 0,
		}
	}
}

impl ToString for LoxCallable {
	fn to_string(&self) -> String {
		match self {
			LoxCallable::FnClock => "<native fn>".into(),
			LoxCallable::FnLox(func, _) => func.name.lexeme.clone(),
			LoxCallable::FnClass(class) => format!("<class {}>", class.name.lexeme),
		}
	}
}