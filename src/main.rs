mod scanner;
mod expr;
mod stmt;
mod parser;
//mod resolver;
mod compiler;

use std::env;
use std::process::exit;
use std::io;

use compiler::Compiler;
//use resolver::Resolver;
use scanner::{Scanner, Token, TokenType};
use parser::Parser;

#[derive(Clone, Copy, PartialEq)]
pub enum VarType {
	Local,
	Parameter,
	Captured,
	// A function parameter that becomes captured by a closure. Requires somewhat
	// special treatment. Tracks its original idx in u32.
	CapturedParameter(u32),
	Global,
}

pub struct Variable {
	index: u32,
	typ: VarType,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct VarRef(usize);

pub struct Lox {
	had_error: bool,

	variables: Vec<Variable>
}

impl Lox {

	fn new() -> Self {
		Lox {
			had_error: false,

			variables: vec![]
		}
	}

	fn new_var(&mut self) -> VarRef {
		self.variables.push(Variable { index: 0, typ: VarType::Local });
		return VarRef(self.variables.len() - 1);
	}

	fn get_var_mut(&mut self, ptr: VarRef) -> &mut Variable {
		// Safety: we only ever hand out valid VarRefs.
		unsafe { self.variables.get_unchecked_mut(ptr.0) }
	}

	fn get_var_type(&self, ptr: VarRef) -> VarType {
		unsafe { self.variables.get_unchecked(ptr.0).typ }
	}

	fn report(&mut self, line: i32, where_: &str, message: &str) {
		eprintln!("[line {line}] Error{where_}: {message}");
		self.had_error = true;
	}

	// For errors where we aren't tracking any location info.
	fn error_general(&mut self, message: &str) {
		eprintln!("Error: {}", message);
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

	fn run(&mut self, code: String) {
		let tokens = {
			let mut scanner = Scanner::new(code, self);
			scanner.scan_tokens()
		};

		let (program, globals_count) = {
			let mut parser = Parser::new(tokens, self);
			parser.parse()
		};

		// Don't resolve if we had an error
		if self.had_error { return; }

		//{
		//	let mut resolver = Resolver::new(self);
		//	resolver.resolve_stmts(&mut program);
		//}

		// Don't compile if we had an error
		//if self.had_error { return; }

		let mut compiler = Compiler::new(self);
		match compiler.compile(&program, globals_count) {
			Ok(_) => {},
			Err(err) => {
				println!("Compile codegen error: {}", err);
			},
		}
	}

	fn compile(&mut self, path: String) -> std::io::Result<()> {
		let contents = std::fs::read_to_string(path)?;

		self.run(contents);
		Ok(())
	}

}

fn main() -> io::Result<()> {
	let mut args: Vec<String> = env::args().collect();
	let mut lox = Lox::new();

	if args.len() != 2 {
		println!("Usage: jaylox [script]");
		exit(64);
	}

	lox.compile(args.remove(1))?;
	if lox.had_error {
		exit(65);
	}

	exit(0);
}
