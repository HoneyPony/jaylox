use std::collections::HashMap;
use std::rc::Rc;

use crate::scanner::Token;
use crate::Lox;

use crate::stmt::*;
use crate::expr::*;

pub struct Resolver<'a> {
	lox: &'a mut Lox,

	scopes: Vec<HashMap<String, bool>>
}

impl<'a> Resolver<'a> {
	pub fn new(lox: &'a mut Lox) -> Self {
		Resolver { lox, scopes: Default::default() }
	}

	pub fn resolve_stmts(&mut self, stmts: &mut Vec<Stmt>) {
		for stmt in stmts {
			self.resolve_stmt(stmt)
		}
	}

	fn begin_scope(&mut self) {
		self.scopes.push(HashMap::new());
	}

	fn end_scope(&mut self) {
		self.scopes.pop();
	}

	fn declare(&mut self, name: &Token) {
		// Note: It is important that we don't assume that scopes has any items,
		// as the variables in the global scope will NOT be in the scopes array
		// (this is why they result in None).
		if let Some(scope) = self.scopes.last_mut() {
			if scope.contains_key(&name.lexeme) {
				self.lox.error_token(name, &format!("Scope already has a variable named '{}'", name.lexeme));
			}
			scope.insert(name.lexeme.clone(), false);
		}
	}

	/// Note: Before calling define, a declare() with the same name
	/// MUST have been called.
	fn define(&mut self, name: &Token) {
		if let Some(scope) = self.scopes.last_mut() {
			// define is only valid to call after declare
			*scope.get_mut(&name.lexeme).unwrap() = true;
		}
	}

	fn resolve_function(&mut self, fun: &mut Rc<Function>) {
		self.begin_scope();

		// Declare/define all parameters
		for param in &fun.parameters {
			self.declare(param);
			self.define(param);
		}
		
		// The question:
		// I have a type Rc<Function> that I'm using so that I can quickly create
		// copies of the Function value, given a fixed version of the AST.
		//
		// This is so that, each time that part of the AST is traversed, the function
		// value is cheaply cloned. 
		//
		// But... that prevents us from mutating it *while building* the AST, which
		// is very problematic.
		//
		// Because at this point in time, it is guaranteed that each of these guys just
		// has one reference.
		//
		// So... can we somehow tell Rust that it is OK to use this specific instance
		// as a regular pointer?
		//
		// What is especially weird is there seems to be no way to do this using
		// Rc::try_unwrap() -- it requires us to use a Rc<Function>, but what we
		// want to do is modify the value inside the pointed-to one...
		//
		// Like, what we want is something that lets us borrow the &mut Function
		// inside the &mut Rc<Function> in the case that the refcount == 1...


		//let raw = Rc::into_raw(fun.clone());
		//unsafe {
		//	let raw = raw as *mut Function;
		//	self.resolve_stmts(&mut (*raw).body);
		//}

		// Oh... it's get_mut...

		if let Some(fun) = Rc::get_mut(fun) {
			self.resolve_stmts(&mut fun.body);
		}
		else {
			panic!("Resolver could not get_mut() when resolving function");
		}

		self.end_scope();
	}

	pub fn resolve_stmt(&mut self, stmt: &mut Stmt) {
		match stmt {
			Stmt::Block(stmts) => {
				self.begin_scope();
				self.resolve_stmts(stmts);
				self.end_scope();
			},
			Stmt::Expression(expr) => self.resolve_expr(expr),
			Stmt::Function(fun) => {
				self.declare(&fun.name);
				self.define(&fun.name);

				self.resolve_function(fun);
			},
			Stmt::If { condition, then_branch, else_branch } => {
				self.resolve_expr(condition);
				self.resolve_stmt(then_branch);
				else_branch.as_mut().map(|branch| self.resolve_stmt(branch));
			},
			Stmt::Print(expr) => self.resolve_expr(expr),
			Stmt::Return { keyword: _, value } => {
				value.as_mut().map(|value| self.resolve_expr(value));
			}
			Stmt::Var { name, initializer } => {
				self.declare(name);
				if let Some(initializer) = initializer {
					self.resolve_expr(initializer);
				}
				self.define(name);
			},
			Stmt::While { condition, body } => {
				self.resolve_expr(condition);
				self.resolve_stmt(body);
			}
		}
	}

	pub fn resolve_expr(&mut self, expr: &mut Expr) {
		match expr {
			Expr::Binary { left, operator: _, right } => {
				self.resolve_expr(left);
				self.resolve_expr(right);
			},
			Expr::Call { callee, paren: _, arguments: _ } => {
				self.resolve_expr(callee);
			},
			Expr::Grouping(inner) => self.resolve_expr(inner),
			Expr::Literal(_literal) => {},
			Expr::Logical { left, operator: _, right } => {
				self.resolve_expr(left);
				self.resolve_expr(right);
			},
			Expr::Unary { operator: _, right } => {
				self.resolve_expr(right);	
			},
			Expr::Variable { name, resolved } => {
				if let Some(scope) = self.scopes.last() {
					if *scope.get(&name.lexeme).unwrap_or(&true) == false {
						self.lox.error_token(name, "Can't read local variable in its own initializer.");
					}

					*resolved = self.resolve_name_now(name);
				}
			},
			Expr::Assign { name, value, resolved } => {
				self.resolve_expr(value);
				*resolved = self.resolve_name_now(name);
			},
		}
	}

	/// Resolves a variable name "right now", i.e., in whatever nested scopes we
	/// are in at the moment. This is the core functionality, as it provides the
	/// resolved values to the Expr::Variable and similar.
	fn resolve_name_now(&self, name: &Token) -> Option<u32> {
		let mut hops: u32 = 0;
		for scope in self.scopes.iter().rev() {
			if scope.contains_key(&name.lexeme) {
				return Some(hops);
			}

			// Directly trace the number of hops up the tree, mirroring how
			// we will actually use this value in the interpreter.
			hops += 1;
		}

		// Global variables get None.
		None
	}
}