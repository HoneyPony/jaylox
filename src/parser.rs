use crate::expr::*;
use crate::scanner::*;

use crate::scanner::TokenType::*;

pub struct Parser {
	tokens: Vec<Token>,
	current: usize
}

impl Parser {
	pub fn new(tokens: Vec<Token>) -> Self {
		Parser {
			tokens,
			current: 0
		}
	}

	fn is_at_end(&self) -> bool {
		return self.peek().typ == Eof;
	}

	fn previous(&self) -> &Token {
		return &self.tokens[self.current - 1];
	}

	fn peek(&self) -> &Token {
		return &self.tokens[self.current];
	}

	fn advance(&mut self) {
		if !self.is_at_end() {
			self.current += 1;
		}
	}

	fn check(&self, typ: TokenType) -> bool {
		if self.is_at_end() { return false; }
		return self.peek().typ == typ;
	}

	fn match_one(&mut self, typ: TokenType) -> bool {
		if self.check(typ) {
			self.advance();
			return true;
		}

		return false;
	}

	fn match_either(&mut self, a: TokenType, b: TokenType) -> bool {
		if self.match_one(a) { return true; }
		if self.match_one(b) { return true; }
		return false;
	}

	fn expression(&mut self) -> Expr {
		self.equality()
	}

	fn equality(&mut self) -> Expr {
		let mut expr = self.comparison();

		while self.match_either(BangEqual, EqualEqual) {
			let operator = self.previous().clone();
			let right = self.comparison();
			expr = Expr::binary(expr, operator, right);
		}

		expr
	}

	fn comparison(&mut self) -> Expr {
		Expr::literal(TokenLiteral::None)
	}
}