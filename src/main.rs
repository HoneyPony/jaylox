mod scanner;
mod expr;
mod stmt;
mod parser;
mod interpreter;
mod environment;

use std::env;
use std::process::exit;
use std::io;
use std::io::Write;

use expr::ast_print;
use interpreter::{Interpreter, InterpErr};
use scanner::{Scanner, Token, TokenType};
use parser::Parser;
use environment::Environment;

struct Lox {
	had_error: bool,
	had_runtime_error: bool,
}

impl Lox {

	fn new() -> Self {
		Lox {
			had_error: false,
			had_runtime_error: false
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

	fn runtime_error(&mut self, err: &InterpErr) {
		eprintln!("{0}\n[line {1}]", err.message, err.token.line);
		self.had_runtime_error = true;
	}

	fn run(&mut self, code: String, environment: &mut Environment) {
		let tokens = {
			let mut scanner = Scanner::new(code, self);
			scanner.scan_tokens()
		};

		let program = {
			let mut parser = Parser::new(tokens, self);
			parser.parse()
		};

		{
			let mut interpreter = Interpreter::new(self, environment);
			interpreter.interpret(&program);
		}
	}

	fn run_file(&mut self, path: String) -> std::io::Result<()> {
		let contents = std::fs::read_to_string(path)?;
		let mut environment = Environment::new();
		self.run(contents, &mut environment);
		Ok(())
	}

	fn run_prompt(&mut self) {
		let mut environment = Environment::new();

		loop {
			print!("> ");
			let _ = io::stdout().flush();

			let mut line = String::new();
			let Ok(_) = io::stdin().read_line(&mut line) else {
				break;
			};

			self.run(line, &mut environment);
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
