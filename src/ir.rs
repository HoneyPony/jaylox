use crate::{expr::Expr, scanner::TokenType, stmt::Stmt};


#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Val(u64);

enum Ir {
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
	}
}

struct IrFunction {
	code: Vec<Ir>
}

pub struct IrCompiler {
	main: IrFunction,
	others: Vec<IrFunction>,

	current_stack: u32
}

#[derive(Clone, Copy, PartialEq)]
enum Location {
	// Floating: A completely temporary expression.
	Floating,

	// Stack: Something that can be accessed at jay_stack_ptr[-x]
	Stack,

	// Anchored: Something like locals.at[0]
	Anchored,
}

impl IrCompiler {
	fn location(&self, val: Val) -> Location {
		return Location::Floating;
	}

	fn location_combine(&self, a: Val, b: Val) -> Location {
		// pattern matching..?
		if self.location(a) == Location::Stack { return Location::Stack; }

		if self.location(b) == Location::Stack { return Location::Stack; }

		return Location::Floating;
	}

	fn new_location(&mut self, location: Location) -> Val {
		return Val(0);
	}

	// Given the specified Val, ensures that its entire evaluation stack is
	// popped and then that value is pushed.
	fn collapse(&mut self, val: Val) {

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
					self.collapse(val);
				}

				let callee = self.compile_expr(into, callee);

				let output = self.new_location(Location::Floating);

				into.push(Ir::Call { callee, output, arity: arguments.len() });

				return output;
			},
			Expr::Get { object, name } => todo!(),
			Expr::Grouping(_) => todo!(),
			Expr::Literal(_) => todo!(),
			Expr::Logical { left, operator, right } => todo!(),
			Expr::Set { object, name, value } => todo!(),
			Expr::Super { keyword, method, identity, this_identity } => todo!(),
			Expr::This { keyword, identity } => todo!(),
			Expr::Unary { operator, right } => todo!(),
			Expr::Variable { name, identity } => todo!(),
			Expr::Assign { name, value, identity } => todo!(),
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
			Stmt::Block(_) => todo!(),
			Stmt::Class(_) => todo!(),
			Stmt::Expression(_) => todo!(),
			Stmt::ExternFunction { var_name, c_name, arity, identity } => todo!(),
			Stmt::Function(_) => todo!(),
			Stmt::If { condition, then_branch, else_branch } => {
				let input = self.compile_expr(into, condition);

				let then_branch = self.compile_stmt_block(Some(&then_branch));
				let else_branch = self.compile_stmt_block(else_branch.as_deref());

				into.push(Ir::If { input, then_branch, else_branch });
			},
			Stmt::Print(_) => todo!(),
			Stmt::Return { keyword, value } => todo!(),
			Stmt::Var { name, initializer, identity } => todo!(),
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
}