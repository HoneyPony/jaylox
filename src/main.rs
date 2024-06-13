mod scanner;
mod expr;
mod stmt;
mod parser;
mod interpreter;
mod environment;
mod callable;
mod resolver;

use std::cell::RefCell;
use std::collections::HashMap;
use std::{env, rc::Rc};
use std::process::exit;
use std::io;
use std::io::Write;

use interpreter::{InterpErr, InterpUnwind, Interpreter};
use resolver::Resolver;
use scanner::{LoxValue, Scanner, Token, TokenType};
use parser::Parser;
use environment::Environment;
use stmt::LoxClass;

pub struct LoxInstance {
	class: Rc<LoxClass>,

	fields: HashMap<String, LoxValue>,
}

impl LoxInstance {
	pub fn new(class: Rc<LoxClass>) -> Self {
		LoxInstance { class, fields: HashMap::new() }
	}

	pub fn get(&self, name: &Token) -> Result<LoxValue, InterpUnwind> {
		self.fields.get(&name.lexeme).ok_or_else(|| InterpErr::new(name, format!("Undefined property {}.", name.lexeme))).cloned()
	}

	pub fn set(&mut self, name: &Token, value: LoxValue) {
		// TODO: Can we avoid this clone in the case that the
		// item already exists..?
		self.fields.insert(name.lexeme.clone(), value);
	}
}

struct Lox {
	had_error: bool,
	had_runtime_error: bool,

	instances: Vec<LoxInstance>
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct LoxRef(usize);

impl Lox {

	fn new() -> Self {
		Lox {
			had_error: false,
			had_runtime_error: false,

			instances: vec![]
		}
	}

	pub fn new_instance(&mut self, class: Rc<LoxClass>) -> LoxRef {
		let idx = self.instances.len();
		self.instances.push(LoxInstance::new(class));

		LoxRef(idx)
	}

	pub fn get(&self, ptr: LoxRef) -> &LoxInstance {
		let idx = ptr.0;

		// Safety: We only hand out LoxRef's to valid items in the array. And,
		// at least for now... There's no garbage collection... :(
		unsafe { self.instances.get_unchecked(idx) }
	}

	pub fn get_mut(&mut self, ptr: LoxRef) -> &mut LoxInstance {
		let idx = ptr.0;

		// Safety: We only hand out LoxRef's to valid items in the array. And,
		// at least for now... There's no garbage collection... :(
		unsafe { self.instances.get_unchecked_mut(idx) }
	}

	fn report(&mut self, line: i32, where_: &str, message: &str) {
		eprintln!("[line {line}] Error{where_}: {message}");
		self.had_error = true;
	}

	fn error(&mut self, line: i32, message: &str) {
		self.report(line, "", message);
	}

	fn error_token(&mut self, token: &Token, message: &str) {
		if token.typ == TokenType::Eof {
			self.report(token.line, " at end", message);
		}
		else {
			self.report(token.line, &format!(" at '{0}'", token.lexeme), message);
		}
	}

	fn runtime_error(&mut self, err: &InterpUnwind) {
		if let InterpUnwind::Error(err) = err {
			eprintln!("{0}\n[line {1}]", err.message, err.token.line);
			self.had_runtime_error = true;
		}

		// In the case that an InterpUnwind::ReturnValue makes it back to the top
		// of the interpreter, we could consider that a fine thing...
		//
		// We could even use this as a way to implement the "exprs implicitly return
		// in REPL" thing
		if let InterpUnwind::ReturnValue(value) = err {
			println!("=> {}", value.to_string());
		}
	}

	fn run(&mut self, code: String, environment: Rc<RefCell<Environment>>) {
		let tokens = {
			let mut scanner = Scanner::new(code, self);
			scanner.scan_tokens()
		};

		let mut program = {
			let mut parser = Parser::new(tokens, self);
			parser.parse()
		};

		// Don't resolve if we had an error
		if self.had_error { return; }

		{
			let mut resolver = Resolver::new(self);
			resolver.resolve_stmts(&mut program);
		}

		// Don't interpret if we had an error
		if self.had_error { return; }

		{
			let mut interpreter = Interpreter::new(self, environment);
			interpreter.interpret(&program);
		}
	}

	fn run_file(&mut self, path: String) -> std::io::Result<()> {
		let contents = std::fs::read_to_string(path)?;
		let environment = Environment::new_with_globals();

		self.run(contents, Rc::clone(&environment));
		Ok(())
	}

	fn run_prompt(&mut self) {
		let environment = Environment::new_with_globals();

		loop {
			print!("> ");
			let _ = io::stdout().flush();

			let mut line = String::new();
			let Ok(_) = io::stdin().read_line(&mut line) else {
				break;
			};

			self.run(line, Rc::clone(&environment));
			self.had_error = false;
		}
	}

}

fn main() -> io::Result<()> {
	let mut args: Vec<String> = env::args().collect();
	let mut lox = Lox::new();

	if args.len() > 2 {
		println!("Usage: jlox [script]");
		exit(64);
	}
	else if args.len() == 2 {
		lox.run_file(args.remove(1))?;
		if lox.had_error {
			exit(65);
		}
		if lox.had_runtime_error {
			exit(70);
		}
	}
	else {
		lox.run_prompt();
	}

	exit(0);
}
