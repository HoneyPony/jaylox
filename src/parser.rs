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

	fn consume(typ: TokenType, message: &str) {
		
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

	fn match_three(&mut self, a: TokenType, b: TokenType, c: TokenType) -> bool {
		if self.match_either(a, b) { return true; }
		if self.match_one(c) { return true; }
		return false;
	}

	fn match_four(&mut self, a: TokenType, b: TokenType, c: TokenType, d: TokenType) -> bool {
		if self.match_three(a, b, c) { return true; }
		if self.match_one(d) { return true; }
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
		let mut expr = self.term();

		while self.match_four(Greater, GreaterEqual, Less, LessEqual) {
			let operator = self.previous().clone();
			let right = self.term();
			expr = Expr::binary(expr, operator, right);
		}

		expr
	}

	fn term(&mut self) -> Expr {
		let mut expr = self.factor();

		while self.match_either(Minus, Plus) {
			let operator = self.previous().clone();
			let right = self.factor();
			expr = Expr::binary(expr, operator, right);
		}

		expr
	}

	fn factor(&mut self) -> Expr {
		let mut expr = self.unary();

		while self.match_either(Slash, Star) {
			let operator = self.previous().clone();
			let right = self.unary();
			expr = Expr::binary(expr, operator, right);
		}

		expr
	}

	fn unary(&mut self) -> Expr {
		if self.match_either(Bang, Minus) {
			let operator = self.previous().clone();
			let right = self.unary();
			return Expr::unary(operator, right);
		}

		primary();
	}

	fn primary(&mut self) -> Expr {
		if self.match_one(False) { return TokenLiteral::Bool(false).into() }
		if self.match_one(True) { return TokenLiteral::Bool(true).into() }
		if self.match_one(Nil) { return TokenLiteral::None.into() }

		if self.match_either(Number, StringTok) {
			return self.previous().literal.clone().into();
		}

		if self.match(LeftParen) {
			let expr = self.expression();
			self.consume(RightParen, "Expect ')' after expression.");
			return Expr::grouping(expr);
		}
	}
}