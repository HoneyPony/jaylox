mod scanner;
mod expr;
mod parser;

use std::env;
use std::process::exit;
use std::io;
use std::io::Write;

use expr::ast_print;
use scanner::{Scanner, Token, TokenType};
use parser::Parser;

struct Lox {
	had_error: bool
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

		let tree = {
			let mut parser = Parser::new(tokens, self);
			parser.parse()
		};

		if let Some(tree) = tree {
			ast_print(&tree);
			println!("");
		}
	}

	fn run_file(&mut self, path: String) -> std::io::Result<()> {
		let contents = std::fs::read_to_string(path)?;
		self.run(contents);
		Ok(())
	}

	fn run_prompt(&mut self) {
		loop {
			print!("> ");
			let _ = io::stdout().flush();

			let mut line = String::new();
			let Ok(_) =io::stdin().read_line(&mut line) else {
				break;
			};

			self.run(line);
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
		if(lox.had_error) {
			exit(65);
		}
	}
	else {
		lox.run_prompt();
	}

	exit(0);
}
