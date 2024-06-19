mod scanner;
mod expr;
mod stmt;
mod parser;
mod resolver;
mod compiler;

use std::env;
use std::process::exit;
use std::io;

use compiler::Compiler;
use resolver::Resolver;
use scanner::{Scanner, Token, TokenType};
use parser::Parser;

pub struct Lox {
	had_error: bool,
}

impl Lox {

	fn new() -> Self {
		Lox {
			had_error: false,
		}
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

	fn run(&mut self, code: String) {
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

		// Don't compile if we had an error
		if self.had_error { return; }

		let mut compiler = Compiler::new(self);
		match compiler.compile(&program) {
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
