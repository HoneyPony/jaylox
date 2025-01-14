use std::collections::HashMap;
use std::collections::HashSet;

use crate::expr::*;
use crate::stmt::Function;
use crate::stmt::Stmt;
use crate::stmt::StmtRes;
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

	undeclared_globals: HashSet<VarRef>,

	// Track if we're in an initializer so we can semantically check the return
	// statements (make sure they're 'return;' and transform them into 'return this;')
	in_initializer: bool,

	global_index: u32,

	// Stores which VarRef is currently the superclass.
	current_superclass: Option<VarRef>,

	// Stores which VarRef is currently the class.
	current_class: Option<VarRef>,

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

			undeclared_globals: HashSet::new(),
			global_index: 0,

			in_initializer: false,
			current_superclass: None,
			current_class: None,
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

	fn check_global(&mut self, ptr: VarRef) {
		// If we're at the global scope, we want to consider our variables "local"
		// if possible to give the compiler more information.
		//
		// So, don't mark them global in that case.
		if self.fun_scopes.len() == 1 {
			return;
		}

		// Otherwise, we know we got here from check_closure, so we know that
		// the variable IS definitely a global, and so it should be marked as
		// such.
		//
		// But, if it is ALREADY a global, we don't want to mark it again.
		if self.lox.get_var_mut(ptr).typ != VarType::Global {
			self.lox.get_var_mut(ptr).typ = VarType::Global;
			self.lox.get_var_mut(ptr).index = self.global_index;
			self.global_index += 1;
		}
	}

	fn check_closure(&mut self, ptr: VarRef) {
		// If it's in the current function scope, it's definitely not a closure.
		if self.fun_scopes.last().unwrap().variables.contains(&ptr) {
			return;
		}

		// Similarly, if it's in the top-level function scope, it's not a closure.
		// However, if it is in the top level function scope, and we're not
		// at that top-level scope, it should be marked as a global.
		if self.fun_scopes.first().unwrap().variables.contains(&ptr) {
			self.check_global(ptr);
			return;
		}

		// Otherwise, this variable should be in a closure. If it's already in a closure,
		// great, otherwise, set it to be in one.
		//
		// IMPORTANT! If it's a CapturedParameter, it should stay the same.
		//
		// In terms of resolving them... I think the main thing to do is, in the compiler,
		// walk down the tree, and keep track of current closure variables. Then, increment
		// their pointer-chase value each time we step into a new function THAT HAS A CLOSURE,
		// and simiarly decrement it afterwards.
		let new_type = match self.lox.get_var_type(ptr) {
			// For parameters, store the original parameter index for later.
			VarType::Parameter => VarType::CapturedParameter(self.lox.get_var_mut(ptr).index),
			VarType::CapturedParameter(idx) => VarType::CapturedParameter(idx),
			VarType::Undefined(name) => VarType::Undefined(name),
			_ => VarType::Captured,
		};
		self.lox.get_var_mut(ptr).typ = new_type;
	}

	fn find_variable(&mut self, name: &str) -> Option<VarRef> {
		for scope in self.scopes.iter().rev() {
			if let Some(&var) = scope.variables.get(name) {
				self.check_closure(var);
				return Some(var);
			}
		}

		return None;
	}

	fn find_variable_previous(&mut self)-> Option<VarRef> {
		for scope in self.scopes.iter().rev() {
			// Must deref var/ref &var so that we can call check_closure. TODO: Why doesn't as_deref() do that..?
			if let Some(&var) = scope.variables.get(&self.previous().lexeme) {
				self.check_closure(var);
				return Some(var);
			}
		}

		return None;
	}

	fn declare_variable(&mut self, name: &Token) -> Result<VarRef, ExprErr> {
		// Special case: If we're at the top-level scope (and implicitly, fun_scope),
		// then we want to check the undeclared variable set. It's possible that
		// we've simply reached the declaration for an undeclared variable, in which
		// case, we just want to remove it from the set, and then return its existing
		// identity.
		if self.scopes.len() == 1 {
			let scope = self.scopes.first_mut().unwrap();
			if let Some(existing) = scope.variables.get(&name.lexeme) {
				// At the global level, unlike the local level, we're allowed to redefine
				// variables (see e.g. test/variable/use_global_in_initializer.lox).
				//
				// So, there are two possibilities:
				// 1. We reached the declaration for an undeclared variable.
				// 2. We are redeclaring some global variable.
				// If we're in case (1), then undeclare_globals.remove(existing) will return
				// true. In that case, we are done--the variable was previously undeclared,
				// now it's declared, so the existing value is the right value.
				//
				// But, if it wasn't undeclared, that means it's a redefinition. So in that
				// case, get a new variable identity, and overwrite the old one.
				if self.undeclared_globals.remove(existing) {
					return Ok(*existing);
				}
				else {
					// Create a new variable.
					// TODO: Maybe deduplicate this logic with the logic below?
					let variable = self.lox.new_var();

					self.scopes.first_mut().unwrap().variables.insert(name.lexeme.clone(), variable);
					self.fun_scopes.first_mut().unwrap().variables.insert(variable);

					// Note that in this case, the variable MUST be global, because
					// we're accessing it from a non-global level (otherwise it would
					// error with undeclared), so immediately mark it as really global.
					self.lox.get_var_mut(variable).typ = VarType::Global;
					self.lox.get_var_mut(variable).index = self.global_index;
					self.global_index += 1;
					return Ok(variable);
				}
			}
		}

		// NOTE: We must always have at least one scope.
		let scope = self.scopes.last_mut().unwrap();
		let fun_scope = self.fun_scopes.last_mut().unwrap();

		let variable = self.lox.new_var();
		fun_scope.variables.insert(variable);

		let had = 
			scope.variables
			.insert(name.lexeme.clone(), variable)
			.is_some();

		if had {
			// Report an error but do not unwind. This is a semantic error not a syntactic error.
			self.error_report(&name, "Already a variable with this name in this scope.");
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

	// TODO: Change this to &Token and fix all call sites
	fn error_expr(&mut self, token: Token, message: &str) -> ExprRes {
		self.lox.error_token(&token, message);
		return Err(ExprErr);
	}

	/// Reports an error, but does not unwind the stack at all. Useful for
	/// reporting semantic errors rather than parse errors (as semantic errors
	/// generally still mean the parse tree is correct).
	fn error_report(&mut self, token: &Token, message: &str) {
		self.lox.error_token(token, message);
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

			// TODO: Consider folding
			expr = Expr::binary(expr, operator, right);
		}

		Ok(expr)
	}

	fn comparison(&mut self) -> ExprRes {
		let mut expr = self.term()?;

		while self.match_four(Greater, GreaterEqual, Less, LessEqual) {
			let operator = self.previous().clone();
			let right = self.term()?;
			expr = Expr::binary_folded(expr, operator, right, self.lox)?;
		}

		Ok(expr)
	}

	fn term(&mut self) -> ExprRes {
		let mut expr = self.factor()?;

		while self.match_either(Minus, Plus) {
			let operator = self.previous().clone();
			let right = self.factor()?;
			
			expr = Expr::binary_folded(expr, operator, right, self.lox)?;
		}

		Ok(expr)
	}

	fn factor(&mut self) -> ExprRes {
		let mut expr = self.unary()?;

		while self.match_either(Slash, Star) {
			let operator = self.previous().clone();
			let right = self.unary()?;
			expr = Expr::binary_folded(expr, operator, right, self.lox)?;
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

	fn get_previous_variable_identity(&mut self) -> Result<VarRef, ExprErr> {
		let identity = self.find_variable_previous();

		let Some(identity) = identity else {
			// If we didn't find the variable, it is undeclared. So, the only
			// possibility that lets the program compile is that it is a global
			// variable that will be declared later.
			//
			// If we're at the top-level, i.e. fun_scopes.len() == 1, then
			// it's not allowed to access a variable before it's defined, so
			// we should error out.
			//
			// Otherwise, we should add the variable to the globals array,
			// but also remember that we want to see a declaration for it at
			// some point. If we never see a declaration, than something is wrong.

			if self.fun_scopes.len() <= 1 {
				if self.lox.full_conformance {
					// In full conformance mode, we just create a global variable
					// with type Undefined.
					let variable = self.lox.new_var();
					// We don't need to bother inserting this variable in any scopes,
					// as it can't meaningfully be referenced again.
					//
					// This does mean that we are quite inefficient if the same undefined
					// variable is referenced a bunch of times. But, for now, that's
					// OK. (TODO: Make that fast..?)
					self.lox.get_var_mut(variable).typ = VarType::Undefined(self.previous().lexeme.clone().leak());
					return Ok(variable);
				}

				// TODO: Get rid of 'clone()' somehow..
				self.error_report(&self.previous().clone(), "At top level: Unknown identifier.\nExtra info: Outside a function, you cannot use a global variable before it's declared.");

				return Err(ExprErr);
			}

			// Okay, the variable should be valid. Create it and add it to the global scopes,
			// plus the undeclared set.
			let variable = self.lox.new_var();
			let varname = self.previous().lexeme.clone();

			self.scopes.first_mut().unwrap().variables.insert(varname, variable);
			self.fun_scopes.first_mut().unwrap().variables.insert(variable);

			self.undeclared_globals.insert(variable);

			// Note that in this case, the variable MUST be global, because
			// we're accessing it from a non-global level (otherwise it would
			// error with undeclared), so immediately mark it as really global.
			self.lox.get_var_mut(variable).typ = VarType::Global;
			self.lox.get_var_mut(variable).index = self.global_index;
			self.global_index += 1;

			return Ok(variable);
		};

		return Ok(identity);
	}

	fn primary(&mut self) -> ExprRes {
		if self.match_one(False) { return Ok(LoxValue::Bool(false).into()) }
		if self.match_one(True) { return Ok(LoxValue::Bool(true).into()) }
		if self.match_one(Nil) { return Ok(LoxValue::Nil.into()) }

		if self.match_either(Number, StringTok) {
			return Ok(self.previous().literal.clone().into());
		}

		if self.match_one(This) {
			let identity = self.find_variable("this");

			// For the "this" variable, it MUST exist in the current scope,
			// otherwise it is located somewhere that is invalid. As such,
			// simply check, otherwise throw an error.
			let Some(identity) = identity else {
				return self.error_expr(self.previous().clone(), 
					"Can't use 'this' outside of a class.");
			};

			return Ok(Expr::this(self.previous().clone(), identity));
		}

		if self.match_one(Super) {
			let keyword = self.previous().clone();
			self.consume(Dot, "Expect '.' after 'super'.")?;
			let method = self.consume(Identifier,
				"Expect superclass method name.")?;


			// We have to track the class that is making use of the 'super' keyword. That class
			// object lets us know what the actual correct superclass value is at runtime.
			// See test/super/reassign_super.lox.
			let Some(class_identity) = self.current_class else {
				self.error_report(&keyword, "Can't use 'super' outside of a class.");
				return Err(ExprErr);
			};

			// Like with 'this', we kind of have to check whether the superclass
			// is valid at parse time..
			let Some(identity) = self.current_superclass else {
				self.error_report(&keyword, "Can't use 'super' in a class with no superclass.");
				return Err(ExprErr);
			};

			// When we access the superclass through super, we have to add the
			// class_identity to the closure so that we can find it later (that is, if that
			// class not a global variable, for example).
			// Usually find_variable() would do this, but because we just look
			// it up in self.current_class, we have to do it manually.
			self.check_closure(class_identity);

			let this_identity = self.find_variable("this")
				.expect("If the super lookup worked, this definitely should have.");

			// Note to self while writing this:
			// It seems quite possible that, instead of cloning Token's willy-nilly,
			// we could have simply returned either a reference to the token, or
			// perhaps an index into the Token Vec...

			return Ok(Expr::super_(keyword, method, identity, this_identity, class_identity));
		}

		if self.match_one(Identifier) {
			let identity = self.get_previous_variable_identity()?;
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

		// If we're in an initializer, we have to make sure that the 'value'
		// is None, then transform value to be a This.
		if self.in_initializer {
			if value.is_some() {
				// Note: not a parse error.
				self.error_report(&keyword, "Can't return a value from an initializer.");
			}

			let identity = self.find_variable("this")
				.expect("Internal error: 'this' should have been defined in an initializer");

			let this_token = Token::new(TokenType::This, "this".into(), LoxValue::Nil,
				keyword.line);
			value = Some(Expr::this(this_token, identity));
		}

		// If we're in the top level code, in full conformance mode, we can't return.
		// (In non-full conformance mode, we'll turn the value into an exit code).
		if self.lox.full_conformance && self.fun_scopes.len() == 1 {
			self.error_report(&keyword, "Can't return from top-level code.");
		}

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

		// Perform the declaration after parsing, because the variable might be
		// referencing a variable with the same name and we can't overwrite the
		// identity yet.
		// 
		// Pass the name token in to the function so that it can accurately report
		// where the error occurred.
		let identity = self.declare_variable(&name)?;

		return Ok(Stmt::var(name, initializer, identity));
	}

	fn function(&mut self, kind: &str, is_method: bool) -> Result<Function, ExprErr> {
		let name = self.consume(Identifier, &format!("Expect {kind} name."))?;

		self.consume(LeftParen, &format!("Expect '(' after {kind} name."))?;

		let identity = if is_method { None }
		else {
			// The function itself must exist as a variable inside the enclosing scope.
			Some(self.declare_variable(&name)?)
		};

		let mut parameters_toks = vec![];
		
		if !self.check(RightParen) {
			parameters_toks.push(
				self.consume(Identifier, "Expect parameter name.")?);
			while self.match_one(Comma) {
				// If we already have 255 parameters, we can't push another one.
				if parameters_toks.len() == 255 {
					self.error_report(&self.peek().clone(), "Can't have more than 255 parameters.");
				}
				parameters_toks.push(
					self.consume(Identifier, "Expect parameter name.")?);
			}
		}

		if is_method {
			// If we're a method, than we need to have a 'this' parameter as our
			// last parameter. Note that, because "this" is lexed into a keyword,
			// we cannot have accidentally already declared it.
			//
			// TODO: We're using a synthesized token here due to error reporting
			// requirements of other users of declare_variable (meaning parameter_toks
			// has to be an array of tokens). Is there a cleaner way to do this?
			let synth = Token::new(TokenType::Identifier, "this".into(), LoxValue::Nil, 0);
			parameters_toks.push(synth);
		}

		// Keep track of when we're in an initializer
		let enclosing_initializer = self.in_initializer;
		self.in_initializer = is_method && name.lexeme == "init";

		self.push_fun_scope();
		self.push_scope();

		let mut param_idx = 0;
		// Create parameter variables.
		for param in parameters_toks {
			let var = self.declare_variable(&param)?;
			self.lox.get_var_mut(var).typ = VarType::Parameter;
			// Assign parameter indices here.
			self.lox.get_var_mut(var).index = param_idx;
			param_idx += 1;
		}

		self.consume(RightParen, "Expect ')' after parameters.")?;

		self.consume(LeftBrace, &format!("Expect '{{' before {kind} body."))?;
		let body = self.block()?;

		// Keep track of any "this" value for compiler.
		let function_this = self.find_variable("this");

		self.pop_scope();

		// Get the fun scope so that variable indices can be assigned.
		let funscope = self.pop_fun_scope();

		// Reset tracked state
		self.in_initializer = enclosing_initializer;

		// Compute var indices
		let mut locals_idx = 0;
		let mut captures_idx = 0;
		let mut captured = vec![];
		for var in &funscope.variables {
			match self.lox.get_var_mut(*var).typ {
				VarType::Local => {
					self.lox.get_var_mut(*var).index = locals_idx;
					locals_idx += 1;
				},
				VarType::Parameter => { /* already assigned */ },

				// Both these are treated the same most places, but require
				// a little bit of extra finagling in the compiler
				VarType::Captured | VarType::CapturedParameter(_)=> {
					self.lox.get_var_mut(*var).index = captures_idx;
					captures_idx += 1;

					captured.push(*var);
				},
				VarType::Global => {
					/* Either this won't be possible here, or we won't do anything
					 * anyways. */
				},
				VarType::Undefined(_) => {
					/* Nothing to do. */
				}
			}
		}

		return Ok(Function {
			name,
			identity,
			vars: funscope.variables,
			param_count: param_idx,
			local_count: locals_idx,
			body,
			this: function_this,
			captured,
		});
	}

	fn class_declaration(&mut self) -> StmtRes {
		let name = self.consume(Identifier, "Expect class name.")?;

		// The class exists as a variable in the surrounding scope.
		// TODO: Possible optimization: hoist classes that are not "dynamic"
		// (e.g. no closure, no dynamic superclass) to global scope
		let identity = self.declare_variable(&name)?;

		let superclass = if self.match_one(Less) {
		 	self.consume(Identifier, "Expect superclass name.")?;
			Some(self.get_previous_variable_identity()?)
		} else { None };

		// Track superclass for 'super' expressions.
		let enclosing_superclass = self.current_superclass;
		self.current_superclass = superclass;

		let enclosing_class = self.current_class;
		self.current_class = Some(identity);

		self.consume(LeftBrace, "Expect '{' before class body.")?;

		// We start with 0 methods.
		// Originally, we created an empty initializer here. But that's not valid--
		// classes without an initializer should defer to their superclass.
		let mut methods = vec![];

		// Push every method into our methods array.
		while !self.check(RightBrace) && !self.is_at_end() {
			let method = self.function("method", true)?;
			methods.push(method);
		}

		self.consume(RightBrace, "Expect '}' after class body.")?;

		// Pop superclass info.
		self.current_superclass = enclosing_superclass;
		self.current_class = enclosing_class;

		if Some(identity) == superclass {
			// Note: It's actually not a problem for us to have a class inherit from itself,
			// because its variable value will be initialized to jay_box_nil() before it is passed
			// as its own superclass, which is supported (at least sort of... we need to get rid
			// of that support in conformance mode I guess).
			//
			// But in any case, that behavior will be unintuitive and useless anyway, so we might
			// as well correctly report the error (as expected by the tester).
			self.error_report(&name, "A class can't inherit from itself.")
		}

		Ok(Stmt::class(crate::stmt::Class {
			name,
			methods,
			superclass,
			identity,
		}))
	}

	fn declaration(&mut self) -> Option<Stmt> {
		let result = if self.match_one(Var) {
			self.var_declaration()
		}
		else if self.match_one(Fun) {
			self.function("function", false).map(|fun| Stmt::Function(fun))
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

	pub fn parse(&mut self) -> (Vec<Stmt>, u32, u32) {
		let mut result = vec![];

		// Create the initial scopes
		self.push_scope();
		self.push_fun_scope();

		// Create default global variables
		self.add_default_globals(&mut result);

		while !self.is_at_end() {
			if let Some(next) = self.declaration() { result.push(next); }
		}

		// For each variable in the global scope that is actually only used
		// in main(), we need to assign it an index.
		let mut global_local_index = 0;
		for var in &self.fun_scopes[0].variables {
			if self.lox.get_var_mut(*var).typ == VarType::Local {
				self.lox.get_var_mut(*var).index = global_local_index;
				global_local_index += 1;
			}
		}

		// If we have any undeclared global variables, report an error.
		for var in &self.undeclared_globals {
			let mut name = "<unknown>";
			// This is an unhappy path, so although this is very inefficient, it
			// should be OK...
			for (k, v) in &self.scopes[0].variables {
				if *v == *var {
					name = &k;
					break;
				}
			}
			if self.lox.full_conformance {
				// In full conformance mode, we simply define the variable with 'Undefined'.
				self.lox.get_var_mut(*var).typ = VarType::Undefined(String::from(name).leak());
			}
			else {
				// Usually though, we want to give a compiler error, as using a variable that
				// hasn't been declared is bad.
				self.lox.error_general(&format!("Global variable '{}' has not been declared.", name));
			}
		}

		// Return the globals counts
		(result, self.global_index, global_local_index)
	}

	// Helper function for creating library functions
	fn std_extern_fun(&mut self, var_name: &str, c_name: &str, arity: u32) -> Stmt {
		// You should not call std_extern_fun in a way where it would fail.
		// TODO: We're using a synthesized token because declare_variable wants
		// to have accurate error reporting. Is this the cleanest way to use
		// it from here?
		let synth_token = Token::new(TokenType::Identifier, var_name.to_string(), LoxValue::Nil, 1);
		let identity = match self.declare_variable(&synth_token) {
			Ok(identity) => identity,
			Err(_) => panic!("std_extern_fun failed to declare variable")
		};

		return Stmt::externfunction(
			var_name.to_string(), 
			c_name.to_string(), 
			arity, 
			identity);
	}

	// Any library functions, etc, must be added here. Of course, this does
	// mean that there is extra overhead for adding globals that are completely
	// unused, which we could try to fix, but for now it's fine...
	fn add_default_globals(&mut self, stmts: &mut Vec<Stmt>) {
		stmts.push(self.std_extern_fun("clock", "jay_std_clock", 0));
	}
}