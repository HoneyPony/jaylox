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
		self.assignment()
	}

	fn assignment(&mut self) -> ExprRes {
		let expr = self.or()?;

		if self.match_one(Equal) {
			let equals = self.previous().clone();
			let value = self.assignment()?;

			if let Expr::Variable(name) = expr {
				return Ok(Expr::assign(name, value));
			}

			// Report but don't bubble.
			let _ = self.error(equals, "Invalid assignment target.");
		}

		Ok(expr)
	}

	fn or(&mut self) -> ExprRes {
		let mut expr = self.and()?;

		while self.match_one(Or) {
			let operator = self.previous().clone();
			let right = self.and()?;
			expr = Expr::logical(expr, operator, right);
		}

		Ok(expr)
	}

	fn and(&mut self) -> ExprRes {
		let mut expr = self.equality()?;

		while self.match_one(And) {
			let operator = self.previous().clone();
			let right = self.equality()?;
			expr = Expr::logical(expr, operator, right);
		}

		Ok(expr)
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
		if self.match_one(False) { return Ok(LoxValue::Bool(false).into()) }
		if self.match_one(True) { return Ok(LoxValue::Bool(true).into()) }
		if self.match_one(Nil) { return Ok(LoxValue::Nil.into()) }

		if self.match_either(Number, StringTok) {
			return Ok(self.previous().literal.clone().into());
		}

		if self.match_one(Identifier) {
			return Ok(Expr::variable(self.previous().clone()));
		}

		if self.match_one(LeftParen) {
			let expr = self.expression()?;
			self.consume(RightParen, "Expect ')' after expression.")?;
			return Ok(Expr::grouping(expr));
		}

		return self.error_expr(self.peek().clone(), "Expect expression.");
	}

	fn print_statement(&mut self) -> StmtRes {
		let value = self.expression()?;
		self.consume(Semicolon, "Expect ';' after value.")?;
		return Ok(Stmt::print(value));
	}

	fn expression_statement(&mut self) -> StmtRes {
		let value = self.expression()?;
		self.consume(Semicolon, "Expect ';' after expression.")?;
		return Ok(Stmt::expression(value));
	}

	fn block(&mut self) -> Result<Vec<Stmt>, ExprErr> {
		let mut statements = vec![];

		while !self.check(RightBrace) && !self.is_at_end() {
			statements.push(self.declaration().ok_or(ExprErr)?);
		}

		self.consume(RightBrace, "Expect '}' after block.")?;
		return Ok(statements);
	}

	fn if_statement(&mut self) -> StmtRes {
		self.consume(LeftParen, "Expect '(' after 'if'.")?;
		let condition = self.expression()?;
		self.consume(RightParen, "Expect ')' after if condition.")?;

		let then_branch = self.statement()?;
		let mut else_branch = None;

		if self.match_one(Else) {
			else_branch = Some(self.statement()?);
		}

		return Ok(Stmt::if_(condition, then_branch, else_branch));
	}

	fn statement(&mut self) -> StmtRes {
		if self.match_one(If) { return self.if_statement(); }
		if self.match_one(Print) { return self.print_statement(); }
		if self.match_one(LeftBrace) {
			return Ok(Stmt::Block(self.block()?))
		}

		return self.expression_statement();
	}

	fn var_declaration(&mut self) -> StmtRes {
		let name = self.consume(Identifier, "Expect variable name.")?;

		let mut initializer = None;
		if self.match_one(Equal) {
			initializer = Some(self.expression()?);
		}

		self.consume(Semicolon, "Expect ';' after variable declaration.")?;
		return Ok(Stmt::var(name, initializer));
	}

	fn declaration(&mut self) -> Option<Stmt> {
		let result = if self.match_one(Var) {
			self.var_declaration()
		}
		else { 
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