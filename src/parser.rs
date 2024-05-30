use crate::expr::*;
use crate::stmt::*;
use crate::scanner::*;

use crate::scanner::TokenType::*;
use crate::Lox;

pub struct Parser<'a> {
	tokens: Vec<Token>,
	current: usize,

	lox: &'a mut Lox
}

impl<'a> Parser<'a> {
	pub fn new(tokens: Vec<Token>, lox: &'a mut Lox) -> Self {
		Parser {
			tokens,
			current: 0,
			lox
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

	fn advance(&mut self)  {
		if !self.is_at_end() {
			self.current += 1;
		}
	}

	fn error(&mut self, token: Token, message: &str) -> Result<Token, ExprErr> {
		self.lox.error_token(&token, message);
		return Err(ExprErr);
	}

	fn error_expr(&mut self, token: Token, message: &str) -> ExprRes {
		self.lox.error_token(&token, message);
		return Err(ExprErr);
	}

	fn consume(&mut self, typ: TokenType, message: &str) -> Result<Token, ExprErr> {
		if self.check(typ) { self.advance(); return Ok(self.previous().clone()) }
		return self.error(self.peek().clone(), message);
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

	fn expression(&mut self) -> ExprRes {
		self.equality()
	}

	fn equality(&mut self) -> ExprRes {
		let mut expr = self.comparison()?;

		while self.match_either(BangEqual, EqualEqual) {
			let operator = self.previous().clone();
			let right = self.comparison()?;
			expr = Expr::binary(expr, operator, right);
		}

		Ok(expr)
	}

	fn comparison(&mut self) -> ExprRes {
		let mut expr = self.term()?;

		while self.match_four(Greater, GreaterEqual, Less, LessEqual) {
			let operator = self.previous().clone();
			let right = self.term()?;
			expr = Expr::binary(expr, operator, right);
		}

		Ok(expr)
	}

	fn term(&mut self) -> ExprRes {
		let mut expr = self.factor()?;

		while self.match_either(Minus, Plus) {
			let operator = self.previous().clone();
			let right = self.factor()?;
			expr = Expr::binary(expr, operator, right);
		}

		Ok(expr)
	}

	fn factor(&mut self) -> ExprRes {
		let mut expr = self.unary()?;

		while self.match_either(Slash, Star) {
			let operator = self.previous().clone();
			let right = self.unary()?;
			expr = Expr::binary(expr, operator, right);
		}

		Ok(expr)
	}

	fn unary(&mut self) -> ExprRes {
		if self.match_either(Bang, Minus) {
			let operator = self.previous().clone();
			let right = self.unary()?;
			return Ok(Expr::unary(operator, right));
		}

		self.primary()
	}

	fn primary(&mut self) -> ExprRes {
		if self.match_one(False) { return Ok(TokenLiteral::Bool(false).into()) }
		if self.match_one(True) { return Ok(TokenLiteral::Bool(true).into()) }
		if self.match_one(Nil) { return Ok(TokenLiteral::None.into()) }

		if self.match_either(Number, StringTok) {
			return Ok(self.previous().literal.clone().into());
		}

		if self.match_one(LeftParen) {
			let expr = self.expression()?;
			self.consume(RightParen, "Expect ')' after expression.")?;
			return Ok(Expr::grouping(expr));
		}

		return self.error_expr(self.peek().clone(), "Expect expression.");
	}

	fn statement(&mut self) -> StmtRes {
		return Err(ExprErr)
	}

	fn declaration(&mut self) -> Option<Stmt> {
		let result = {
			self.statement()
		};

		if result.is_err() {
			self.synchronize();
		}

		return result.ok()
	}

	fn synchronize(&mut self) {
		self.advance();

		while !self.is_at_end() {
			if self.previous().typ == Semicolon { return; }

			match self.peek().typ {
				Class | Fun | Var | For | If | While | Print | Return => { return; }
				_ => { }
			}

			self.advance();
		}
	}

	pub fn parse(&mut self) -> Vec<Stmt> {
		let mut result = vec![];

		while !self.is_at_end() {
			if let Some(next) = self.declaration() { result.push(next); }
		}

		result
	}
}