use std::collections::HashMap;

use crate::{expr::Expr, scanner::{LoxValue, TokenType}, stmt::Stmt, VarRef};


#[derive(Clone, Copy, Eq, PartialEq, Hash)]
pub struct Val(usize);

pub enum Ir {
	Binop {
		output: Val,
		left: Val,
		right: Val,
		op: TokenType,
	},

	This {
		output: Val,
	},

	Call {
		output: Val,
		callee: Val,
		arity: usize,
	},

	If {
		input: Val,

		then_branch: Vec<Ir>,
		else_branch: Vec<Ir>,
	},

	Print {
		input: Val
	},

	Literal {
		output: Val,
		literal: LoxValue,
	},

	Assign {
		output: Val,
		identity: VarRef,
		value: Val,
	},

	Block(Vec<Ir>)
}

pub struct IrFunction {
	pub code: Vec<Ir>
}

struct ValInfo {
	location: Location,

	// How many values will be popped off the stack after this operation.
	pop: Option<u32>,

	// Only relevant to Location::Stack. Kept separate for convenience...
	stack_index: u32,
}

pub struct IrCompiler {
	pub main: IrFunction,

	current_stack: u32,

	pub vals: IrVals,
}

pub struct IrVals {
	pub vals: Vec<ValInfo>,
	pub var_vals: HashMap<VarRef, Val>,
}

impl IrVals {
	pub fn new() -> Self {
		return IrVals {
			vals: vec![],
			var_vals: HashMap::new(),
		}
	}

	pub fn get_location(&self, val: Val) -> Location {
		unsafe { self.vals.get_unchecked(val.0).location }
	}
}

#[derive(Clone, Copy, PartialEq)]
pub enum Location {
	// Floating: A completely temporary expression.
	Floating,

	// Stack: Something that can be accessed at jay_stack_ptr[-x]
	Stack,

	// Anchored: Something like locals.at[0]
	Anchored(VarRef),

	// The computed value is unused (only side effects)
	None,
}

impl IrCompiler {
	pub fn new() -> IrCompiler {
		return IrCompiler {
			main: IrFunction { code: vec![] },

			current_stack: 0,
			
			vals: IrVals::new(),
		}
	}

	fn location(&self, val: Val) -> Location {
		return Location::Floating;
	}

	fn location_combine(&self, a: Val, b: Val) -> Location {
		// pattern matching..?
		if self.location(a) == Location::Stack { return Location::Stack; }

		if self.location(b) == Location::Stack { return Location::Stack; }

		return Location::Floating;
	}

	/// If the given location is Stack, then return Stack, otherwise return
	/// Floating. This lets us easily have stack-based operations remain
	/// stack-based.
	fn location_follow(&self, a: Val) -> Location {
		if self.location(a) == Location::Stack { return Location::Stack; }
		return Location::Floating;
	}

	fn new_location(&mut self, location: Location) -> Val {
		let index = self.vals.vals.len();

		let mut info = ValInfo {
			location,
			pop: None,
			stack_index: 0
		};

		if location == Location::Stack {
			self.current_stack += 1;
			info.stack_index = self.current_stack;
		}

		self.vals.vals.push(info);

		return Val(index);
	}

	fn var_location(&mut self, var: VarRef) -> Val {
		if let Some(val) = self.vals.var_vals.get(&var) {
			return *val;
		}

		let val = self.new_location(Location::Anchored(var));
		self.vals.var_vals.insert(var, val);

		return val;
	}

	// Given the specified Val, ensures that its entire evaluation stack is
	// popped and then that value is pushed. Note: MUST be called BEFORE
	// any other locations are generated with new_location.
	fn collapse(&mut self) {
		let info = unsafe { self.vals.vals.last_mut().unwrap_unchecked() };

		if info.location != Location::Stack {
			// Only safe because we're modifying the last generated element.
			info.location = Location::Stack;
			self.current_stack += 1;
			info.stack_index = self.current_stack;
		}

		info.pop = Some(self.current_stack);

		self.current_stack = 0;
	}

	fn remove_output(&mut self, val: Val) {
		let info = unsafe { self.vals.vals.get_unchecked_mut(val.0) };

		info.location = Location::None;
	}

	fn synthesize_nil(&mut self, into: &mut Vec<Ir>) -> Val {
		let output = self.new_location(Location::Floating);
		let literal = LoxValue::Nil;
		into.push(Ir::Literal { output, literal });

		return output;
	}

	fn compile_expr(&mut self, into: &mut Vec<Ir>, expr: &Expr) -> Val {
		match expr {
			Expr::Binary { left, operator, right } => {
				let left = self.compile_expr(into, left);
				let right = self.compile_expr(into, right);

				let location = match operator.typ {
					TokenType::Plus => Location::Stack,
					_ => self.location_combine(left, right)
				};

				let output = self.new_location(location);

				into.push(Ir::Binop { output, left, right, op: operator.typ });

				return output;
			},
			Expr::Call { callee, paren, arguments } => {
				for param in arguments {
					let val = self.compile_expr(into, param);
					self.collapse();
				}

				let callee = self.compile_expr(into, callee);

				let output = self.new_location(Location::Floating);

				into.push(Ir::Call { callee, output, arity: arguments.len() });

				return output;
			},
			Expr::Get { object, name } => todo!(),
			Expr::Grouping(inner) => {
				return self.compile_expr(into, expr);
			},
			Expr::Literal(literal) => {
				let output = self.new_location(Location::Floating);

				into.push(Ir::Literal { output, literal: literal.clone() });

				return output;
			},
			Expr::Logical { left, operator, right } => todo!(),
			Expr::Set { object, name, value } => todo!(),
			Expr::Super { keyword, method, identity, this_identity } => todo!(),
			Expr::This { keyword, identity } => {
				return self.var_location(*identity);
			},
			Expr::Unary { operator, right } => todo!(),
			Expr::Variable { name, identity } => {
				return self.var_location(*identity);
			},
			Expr::Assign { name, value, identity } => {
				let value = self.compile_expr(into, value);

				let output = self.new_location(self.location_follow(value));

				into.push(Ir::Assign { output, identity: *identity, value });

				return output;
			},
		}
	}

	fn compile_stmt_block(&mut self, stmt: Option<&Stmt>) -> Vec<Ir> {
		let Some(stmt) = stmt else { return Vec::new(); };

		let mut output = vec![];

		match stmt {
			// Unwrap blocks here.
			Stmt::Block(inner) => {
				for stmt in inner {
					self.compile_stmt(&mut output, stmt);
				}
			},
			_ => {
				self.compile_stmt(&mut output, stmt);
			}
		}

		return output;
	}

	fn compile_stmt(&mut self, into: &mut Vec<Ir>, stmt: &Stmt) {
		match stmt {
			Stmt::Block(inner) => {
				let mut code = vec![];

				for stmt in inner {
					self.compile_stmt(&mut code, stmt);
				}

				into.push(Ir::Block(code));
			},
			Stmt::Class(_) => todo!(),
			Stmt::Expression(expr) => {
				let input = self.compile_expr(into, expr);

				self.remove_output(input);

				// No IR gen needed.
			},
			Stmt::ExternFunction { var_name, c_name, arity, identity } => todo!(),
			Stmt::Function(_) => todo!(),
			Stmt::If { condition, then_branch, else_branch } => {
				let input = self.compile_expr(into, condition);

				let then_branch = self.compile_stmt_block(Some(&then_branch));
				let else_branch = self.compile_stmt_block(else_branch.as_deref());

				into.push(Ir::If { input, then_branch, else_branch });
			},
			Stmt::Print(expr) => {
				let input = self.compile_expr(into, expr);

				into.push(Ir::Print { input });
			},
			Stmt::Return { keyword, value } => todo!(),
			Stmt::Var { name, initializer, identity } => {
				let value = match initializer {
					Some(init) => self.compile_expr(into, init),
					None => self.synthesize_nil(into)
				};

				let output = self.new_location(Location::None);

				into.push(Ir::Assign { output, identity: *identity, value });
			},
			Stmt::While { condition, body } => todo!(),
		}
	}

	fn compile_function(&mut self, input: &Vec<Stmt>) -> IrFunction {
		let mut code = vec![];

		for stmt in input {
			self.compile_stmt(&mut code, stmt);
		}

		return IrFunction { code }
	}

	pub fn compile(mut self, program: Vec<Stmt>) -> (IrFunction, IrVals) {
		self.main = self.compile_function(&program);

		return (self.main, self.vals);
	}
}