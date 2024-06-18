use std::collections::HashMap;
use std::rc::Rc;

use crate::scanner::Token;
use crate::Lox;

use crate::stmt::*;
use crate::expr::*;

#[derive(Clone, Copy, PartialEq)]
enum FunctionType {
	None,
	Function,
	Initializer,
	Method
}

#[derive(Clone, Copy, PartialEq)]
enum ClassType {
	None,
	Class,
	Subclass,
}

pub struct Resolver<'a> {
	lox: &'a mut Lox,

	scopes: Vec<HashMap<String, bool>>,

	current_function: FunctionType,
	current_class: ClassType,
}

impl<'a> Resolver<'a> {
	pub fn new(lox: &'a mut Lox) -> Self {
		Resolver {
			lox,
			scopes: Default::default(),
			current_function: FunctionType::None,
			current_class: ClassType::None,
		}
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

	fn resolve_function(&mut self, fun: &mut Rc<Function>, kind: FunctionType) {
		let enclosing_function = self.current_function;
		self.current_function = kind;
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

			if kind == FunctionType::Initializer {
				fun.is_initializer = true;
			}
		}
		else {
			panic!("Resolver could not get_mut() when resolving function");
		}

		self.end_scope();
		self.current_function = enclosing_function;
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

				self.resolve_function(fun, FunctionType::Function);
			},
			Stmt::If { condition, then_branch, else_branch } => {
				self.resolve_expr(condition);
				self.resolve_stmt(then_branch);
				else_branch.as_mut().map(|branch| self.resolve_stmt(branch));
			},
			Stmt::Print(expr) => self.resolve_expr(expr),
			Stmt::Return { keyword, value } => {
				if self.current_function == FunctionType::None {
					self.lox.error_token(keyword, "Can't return from top-level code.");
				}

				if self.current_function == FunctionType::Initializer && value.is_some() {
					self.lox.error_token(keyword, "Can't return a value from an initializer.");
				}

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
			},
			Stmt::Class { name, methods, superclass } => {
				let enclosing = self.current_class;
				self.current_class = ClassType::Class;
				
				self.declare(&name);
				self.define(&name);

				if let Some(superclass_name) = &superclass.0 {
					if name.lexeme == superclass_name.lexeme {
						self.lox.error_token(superclass_name, "A class can't inherit from itself.");
					}
					self.resolve_local(superclass_name, &mut superclass.1);

					self.current_class = ClassType::Subclass;
				}

				if superclass.0.is_some() {
					self.begin_scope();
					// Safety: We just began the scope, so last must exist.
					self.scopes.last_mut().unwrap().insert("super".into(), true);
				}

				self.begin_scope(); // New scope for "this"
				self.scopes.last_mut().unwrap().insert("this".into(), true);

				for method in methods {
					let mut declaration = FunctionType::Method;
					if method.name.lexeme == "init" {
						declaration = FunctionType::Initializer;
					}
					self.resolve_function(method, declaration);
				}

				self.end_scope();

				// Must close the superclass scope if it existed
				if superclass.0.is_some() {
					self.end_scope();
				}

				self.current_class = enclosing;
			}
		}
	}

	fn resolve_local(&mut self, name: &Token, resolved: &mut Option<u32>) {
		if let Some(scope) = self.scopes.last() {
			if *scope.get(&name.lexeme).unwrap_or(&true) == false {
				self.lox.error_token(name, "Can't read local variable in its own initializer.");
			}

			*resolved = self.resolve_name_now(name);
		}
	}

	pub fn resolve_expr(&mut self, expr: &mut Expr) {
		match expr {
			Expr::Binary { left, operator: _, right } => {
				self.resolve_expr(left);
				self.resolve_expr(right);
			},
			Expr::Call { callee, paren: _, arguments } => {
				self.resolve_expr(callee);
				for arg in arguments {
					self.resolve_expr(arg);
				}
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
				self.resolve_local(name, resolved)
			},
			Expr::Assign { name, value, resolved } => {
				self.resolve_expr(value);
				*resolved = self.resolve_name_now(name);
			},
			Expr::Get { object, .. } => {
				self.resolve_expr(object);
			},
			Expr::Set { object, value, .. } => {
				self.resolve_expr(object);
				self.resolve_expr(value);
			},
			Expr::This { keyword, resolved } => {
				if self.current_class == ClassType::None {
					self.lox.error_token(keyword, "Can't use 'this' keyword outside of a class.");
					return;
				}
				self.resolve_local(keyword, resolved);
			},
			Expr::Super { keyword, resolved, .. } => {
				if self.current_class == ClassType::None {
					self.lox.error_token(keyword, "Can't user 'super' outside of a class.");
				}
				if self.current_class != ClassType::Subclass {
					self.lox.error_token(keyword, "Can't use 'super' in a class with no superclass.");
				}

				self.resolve_local(keyword, resolved);
			}
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