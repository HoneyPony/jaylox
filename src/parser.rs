use std::collections::HashMap;
use std::collections::HashSet;
use std::hash::Hash;
use std::rc::Rc;

use crate::expr::*;
use crate::stmt::*;
use crate::scanner::*;

use crate::scanner::TokenType::*;
use crate::Lox;
use crate::VarRef;
use crate::VarType;

struct Scope {
	variables: HashMap<String, VarRef>
}

struct FunScope {
	variables: HashSet<VarRef>
}

impl Scope {
	pub fn new() -> Self {
		return Scope { variables: HashMap::new() }
	}
}

impl FunScope {
	pub fn new() -> Self {
		return FunScope { variables: HashSet::new() }
	}
}

pub struct Parser<'a> {
	tokens: Vec<Token>,
	current: usize,

	scopes: Vec<Scope>,
	fun_scopes: Vec<FunScope>,

	lox: &'a mut Lox
}

impl<'a> Parser<'a> {
	pub fn new(tokens: Vec<Token>, lox: &'a mut Lox) -> Self {
		Parser {
			tokens,
			current: 0,
			lox,

			scopes: vec![],
			fun_scopes: vec![],
		}
	}

	fn push_scope(&mut self) {
		self.scopes.push(Scope::new());
	}

	fn pop_scope(&mut self) {
		self.scopes.pop();
	}

	fn push_fun_scope(&mut self) {
		self.fun_scopes.push(FunScope::new());
	}

	fn pop_fun_scope(&mut self) -> FunScope {
		self.fun_scopes.pop().unwrap()
	}

	fn find_variable(&mut self, name: &str) -> Option<VarRef> {
		for scope in self.scopes.iter().rev() {
			if let Some(var) = scope.variables.get(name) {
				return Some(*var);
			}
		}

		return None;
	}

	fn find_variable_previous(&mut self)-> Option<VarRef> {
		for scope in self.scopes.iter().rev() {
			if let Some(var) = scope.variables.get(&self.previous().lexeme) {
				return Some(*var);
			}
		}

		return None;
	}

	fn declare_variable(&mut self, name: String) -> Result<VarRef, ExprErr> {
		// NOTE: We must always have at least one scope.
		let scope = self.scopes.last_mut().unwrap();
		let fun_scope = self.fun_scopes.last_mut().unwrap();

		let variable = self.lox.new_var();
		fun_scope.variables.insert(variable);

		let had = 
			scope.variables
			.insert(name, variable)
			.is_some();

		if had {
			self.error(self.previous().clone(), "Cannot redeclare variable in same scope.")?;
		}

		Ok(variable)
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

			if let Expr::Variable { name, identity } = expr {
				return Ok(Expr::assign(name, value, identity));
			}
			else if let Expr::Get { object, name } = expr {
				// Transform end-of-chain getter into setter. (Others remain as getters)
				return Ok(Expr::set(*object, name, value));
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

		self.call()
	}

	fn finish_call(&mut self, expr: Expr) -> ExprRes {
		let mut arguments = vec![];

		if !self.check(RightParen) {
			arguments.push(self.expression()?);
			while self.match_one(Comma) {
				if arguments.len() >= 255 {
					self.error(self.peek().clone(), "Can't have more than 255 arguments.")?;
				}
				arguments.push(self.expression()?);
			}
		}

		let paren = self.consume(RightParen, "Expect ')' after arguments.")?;

		return Ok(Expr::call(expr, paren, arguments));
	}

	fn call(&mut self) -> ExprRes {
		let mut expr = self.primary()?;
		
		loop {
			if self.match_one(LeftParen) {
				expr = self.finish_call(expr)?;
			}
			else if self.match_one(Dot) {
				let name = self.consume(Identifier,
					"Expect property name after '.'.")?;
				expr = Expr::get(expr, name);
			}
			else {
				break;
			}
		}

		Ok(expr)
	}

	fn primary(&mut self) -> ExprRes {
		if self.match_one(False) { return Ok(LoxValue::Bool(false).into()) }
		if self.match_one(True) { return Ok(LoxValue::Bool(true).into()) }
		if self.match_one(Nil) { return Ok(LoxValue::Nil.into()) }

		if self.match_either(Number, StringTok) {
			return Ok(self.previous().literal.clone().into());
		}

		if self.match_one(This) {
			return Ok(Expr::this(self.previous().clone(), None));
		}

		if self.match_one(Super) {
			let keyword = self.previous().clone();
			self.consume(Dot, "Expect '.' after 'super'.")?;
			let method = self.consume(Identifier,
				"Expect superclass method name.")?;

			// Note to self while writing this:
			// It seems quite possible that, instead of cloning Token's willy-nilly,
			// we could have simply returned either a reference to the token, or
			// perhaps an index into the Token Vec...

			return Ok(Expr::super_(keyword, method, None));
		}

		if self.match_one(Identifier) {
			let identity = self.find_variable_previous();
			// May need to be changed to not error out because the variable might be global...
			// Maybe just add the variable to the global array here...?
			let Some(identity) = identity else {
				return self.error_expr(self.previous().clone(), "Unknown identifier.");
			};
			return Ok(Expr::variable(self.previous().clone(), identity));
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

		// Blocks introduce a new scope.
		self.push_scope();

		while !self.check(RightBrace) && !self.is_at_end() {
			statements.push(self.declaration().ok_or(ExprErr)?);
		}

		self.pop_scope();

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

	fn while_statement(&mut self) -> StmtRes {
		self.consume(LeftParen, "Expect '(' after 'while'.")?;
		let condition = self.expression()?;
		self.consume(RightParen, "Expect ')' after while condition.")?;

		let body = self.statement()?;

		return Ok(Stmt::while_(condition, body));
	}

	fn for_statement(&mut self) -> StmtRes {
		self.consume(LeftParen, "Expect '(' after 'for'.")?;

		let mut initializer = None;
		if self.match_one(Semicolon) {

		}
		else if self.match_one(Var) {
			initializer = Some(self.var_declaration()?);
		}
		else {
			initializer = Some(self.expression_statement()?);
		}

		let mut condition = None;
		if !self.check(Semicolon) {
			condition = Some(self.expression()?);
		}

		self.consume(Semicolon, "Expect ';' after loop condition.")?;

		let mut increment = None;
		if !self.check(RightParen) {
			increment = Some(self.expression()?);
		}

		self.consume(RightParen, "Expect ')' after for clauses.")?;

		let mut body = self.statement()?;

		// Desugar: add increment to end of loop body.
		if let Some(increment) = increment {
			body = Stmt::block(vec![body, Stmt::expression(increment)]);
		}

		// Desugar: add condition to loop.
		let condition = condition.unwrap_or(Expr::literal(LoxValue::Bool(true)));
		body = Stmt::while_(condition, body);

		// Desugar: add initializer.
		if let Some(initializer) = initializer {
			body = Stmt::Block(vec![initializer, body]);
		}

		return Ok(body);
	}

	fn return_statement(&mut self) -> StmtRes {
		let keyword = self.previous().clone();
		let mut value = None;

		if !self.check(Semicolon) {
			value = Some(self.expression()?);
		}

		self.consume(Semicolon, "Expect ';' after return value.")?;
		return Ok(Stmt::return_(keyword, value));
	}

	fn statement(&mut self) -> StmtRes {
		if self.match_one(For) { return self.for_statement(); }
		if self.match_one(If) { return self.if_statement(); }
		if self.match_one(While) { return self.while_statement(); }
		if self.match_one(Print) { return self.print_statement(); }
		if self.match_one(Return) { return self.return_statement(); }
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

		let identity = self.declare_variable(self.previous().lexeme.clone())?;

		return Ok(Stmt::var(name, initializer, identity));
	}

	fn function(&mut self, kind: &str) -> Result<Function, ExprErr> {
		let name = self.consume(Identifier, &format!("Expect {kind} name."))?;

		self.consume(LeftParen, &format!("Expect '(' after {kind} name."))?;

		// The function itself must exist as a variable inside the enclosing scope.
		let identity = self.declare_variable(name.lexeme.clone())?;

		let mut parameters_tokens = vec![];
		
		if !self.check(RightParen) {
			parameters_tokens.push(self.consume(Identifier, "Expect parameter name.")?);
			while self.match_one(Comma) {
				parameters_tokens.push(self.consume(Identifier, "Expect parameter name.")?);
			}
		}

		self.push_fun_scope();
		self.push_scope();

		let mut param_idx = 0;
		// Create parameter variables.
		for param in parameters_tokens {
			let var = self.declare_variable(param.lexeme.clone())?;
			self.lox.get_var_mut(var).typ = VarType::Parameter;
			// Assign parameter indices here.
			self.lox.get_var_mut(var).index = param_idx;
			param_idx += 1;
		}

		self.consume(RightParen, "Expect ')' after parameters.")?;

		self.consume(LeftBrace, &format!("Expect '{{' before {kind} body."))?;
		let body = self.block()?;

		self.pop_scope();

		// Get the fun scope so that variable indices can be assigned.
		let funscope = self.pop_fun_scope();

		let mut locals_idx = 0;
		let mut captures_idx = 0;
		for var in &funscope.variables {
			match self.lox.get_var_mut(*var).typ {
				VarType::Local => {
					self.lox.get_var_mut(*var).index = locals_idx;
					locals_idx += 1;
				},
				VarType::Parameter => { /* already assigned */ },
				VarType::Captured => {
					self.lox.get_var_mut(*var).index = captures_idx;
					captures_idx += 1;
				},
				VarType::Global => {
					/* Either this won't be possible here, or we won't do anything
					 * anyways. */
				}
			}
		}

		return Ok(Function {
			name,
			identity,
			vars: funscope.variables,
			param_count: param_idx,
			local_count: locals_idx,
			capture_count: captures_idx,
			body,
			is_initializer: false
		});
	}

	fn class_declaration(&mut self) -> StmtRes {
		let name = self.consume(Identifier, "Expect class name.")?;
		
		let superclass = if self.match_one(Less) {
			let identifier = self.consume(Identifier, "Expect superclass name.")?;
			Some(identifier)
		} else { None };

		self.consume(LeftBrace, "Expect '{' before class body.")?;

		let mut methods = vec![];
		while !self.check(RightBrace) && !self.is_at_end() {
			methods.push(self.function("method")?);
		}

		self.consume(RightBrace, "Expect '}' after class body.")?;

		self.lox.error_token(&self.previous().clone(), "Classes unimplemented at the momemt");
		return Err(ExprErr);

		//Ok(Stmt::class(name, methods, (superclass, None)))
	}

	fn declaration(&mut self) -> Option<Stmt> {
		let result = if self.match_one(Var) {
			self.var_declaration()
		}
		else if self.match_one(Fun) {
			self.function("function").map(|fun| Stmt::Function(fun))
		}
		else if self.match_one(Class) {
			self.class_declaration()
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

	pub fn parse(&mut self) -> (Vec<Stmt>, u32) {
		let mut result = vec![];

		// Create the initial scopes
		self.push_scope();
		self.push_fun_scope();

		while !self.is_at_end() {
			if let Some(next) = self.declaration() { result.push(next); }
		}

		// Every variable in the top-level fun-scope is marked as global.
		let mut global_index = 0;
		for var in &self.fun_scopes[0].variables {
			self.lox.get_var_mut(*var).typ = VarType::Global;
			self.lox.get_var_mut(*var).index = global_index;
			global_index += 1;
		}

		// Return the globals count
		(result, global_index)
	}
}