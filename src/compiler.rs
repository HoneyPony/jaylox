use core::fmt;
use std::{cell::RefCell, collections::HashSet, fmt::Write, rc::Rc};

use crate::{environment::Environment, expr::Expr, scanner::Token, stmt::Stmt, Lox};



pub struct Compiler<'a> {
	pub lox: &'a mut Lox,
	pub environment: Rc<RefCell<Environment>>,

	/// Contains the #include and all function forward-declarations.
	prelude: String,

	/// Contains all the function definitions.
	function_defs: Vec<String>,

	/// Contains every string used for names. This is for the fast hashing system.
	name_set: HashSet<String>,

	current_indent: i32,
}

impl<'a> Compiler<'a> {
	pub fn new(lox: &'a mut Lox, environment: Rc<RefCell<Environment>>) -> Self {
		return Compiler {
			lox,
			environment,
			prelude: String::new(),
			function_defs: Vec::new(),
			name_set: HashSet::new(),
			current_indent: 0,
		}
	}

	fn push_indent(&mut self) {
		self.current_indent += 1;
	}

	fn pop_indent(&mut self) {
		self.current_indent -= 1;
	}

	fn indent(&self, into: &mut String) {
		for i in 0..self.current_indent {
			into.push('\t');
		}
	}

	fn add_name(&mut self, name: &Token) {
		if self.name_set.contains(&name.lexeme) { return; }
		self.name_set.insert(name.lexeme.clone());
	}

	fn compile_expr(&mut self, expr: &Expr, into: &mut String) -> fmt::Result {
		match expr {
			Expr::Binary { left, operator, right } => todo!(),
			Expr::Call { callee, paren, arguments } => todo!(),
			Expr::Get { object, name } => todo!(),
			Expr::Grouping(_) => todo!(),
			Expr::Literal(_) => todo!(),
			Expr::Logical { left, operator, right } => todo!(),
			Expr::Set { object, name, value } => todo!(),
			Expr::Super { keyword, method, resolved } => todo!(),
			Expr::This { keyword, resolved } => todo!(),
			Expr::Unary { operator, right } => todo!(),
			Expr::Variable { name, resolved } => todo!(),
			Expr::Assign { name, value, resolved } => todo!(),
		}

		Ok(())
	}

	fn compile_stmt(&mut self, stmt: &Stmt, into: &mut String) -> fmt::Result {
		match stmt {
			Stmt::Block(stmts) => {
				self.indent(into);
				into.push_str("{\n");
				self.push_indent();
				self.compile_stmts(stmts, into)?;
				self.pop_indent();
				self.indent(into);
				into.push_str("}\n");
			},
			Stmt::Class { name, methods, superclass } => todo!(),
			Stmt::Expression(expr) => {
				self.compile_expr(expr, into)?;
			},
			Stmt::Function(fun) => {
				let mut def = String::new();

				// Reset indent for top-level functions
				let enclosing_indent = self.current_indent;
				self.current_indent = 0;

				// TODO: NAME MANGLING
				let mangled_name = fun.name.lexeme.clone();

				writeln!(def, "jay_value\n{}(jay_value *arguments, jay_instance *closure) {{", mangled_name)?;
				self.push_indent();

				// For now, because all args are in the closure, convert any parameters into closure values
				// And, always allocate the "scope" closure for the current fun.
				self.indent(&mut def);
				writeln!(def, "jay_instance *scope = jay_new_scope(closure);")?;

				// Iterate through each parameter and put it into the current scope
				for (idx, arg) in fun.parameters.iter().enumerate() {
					self.indent(&mut def);
					self.add_name(arg);
					writeln!(def, "jay_put(NAME_{}, arguments[{idx}]);", arg.lexeme)?;
				}

				// Finally, put a default return value.
				self.indent(&mut def);
				writeln!(def, "return jay_null();")?;

				// End the function.
				self.pop_indent();
				writeln!(def, "}}")?;

				self.current_indent = enclosing_indent;
				self.function_defs.push(def);

				// In the outer scope, we need to have a reference to this function
				// in our closure. And, that function's closure is our 'scope.' So,
				// create a new value.
				//
				// Because we create this value in the compiled code at the same place
				// that the function is defined in the AST, it should have the same
				// ordering properties (e.g. not being visible beforehand).
				self.indent(into);
				self.add_name(&fun.name);
				writeln!(into, "jay_put(NAME_{}, jay_fun_from({mangled_name}, scope));", fun.name.lexeme)?;
			},
			Stmt::If { condition, then_branch, else_branch } => {
				self.indent(into);
				into.push_str("if(");
				// TODO: Do we need a prelude for calls?
				self.compile_expr(condition, into)?;
				into.push_str(")\n");
				self.compile_stmt(then_branch, into)?;
				if let Some(else_branch) = else_branch {
					// Note: This should be OK because we should always add a '\n' after statements anyways...
					self.indent(into);
					into.push_str("else\n");
					self.compile_stmt(&else_branch, into)?;
				}
			},
			Stmt::Print(expr) => {
				self.indent(into);
				into.push_str("jay_print(");
				self.compile_expr(expr, into)?;
				into.push_str(");\n");
			},
			Stmt::Return { keyword, value } => {
				self.indent(into);
				into.push_str("return ");
				match value {
					Some(value) => { self.compile_expr(value, into)?; },
					None => { into.push_str("jay_null()"); }
				};
				into.push_str(";\n");
			},
			Stmt::Var { name, initializer } => {
				// For now, we always get names from the surrounding closure.
				self.add_name(name);
				self.indent(into);
				write!(into, "jay_put(NAME_{}, ", name.lexeme)?;
				match initializer {
					Some(initializer) => { self.compile_expr(initializer, into)?; },
					None => { into.push_str("jay_null()"); }
				}
				into.push_str(");\n");
			},
			Stmt::While { condition, body } => {
				self.indent(into);
				into.push_str("while(");
				self.compile_expr(condition, into)?;
				into.push_str(")\n");
				self.compile_stmt(body, into)?;
			},
		}

		Ok(())
	}

	fn compile_stmts(&mut self, stmts: &Vec<Stmt>, into: &mut String) -> fmt::Result {
		for stmt in stmts {
			self.compile_stmt(stmt, into)?;
		}

		Ok(())
	}

	pub fn compile(&mut self, stmts: &Vec<Stmt>) -> fmt::Result {
		// Write the first part of the prelude
		writeln!(self.prelude, "/* C file created by jaylox */")?;
		writeln!(self.prelude, "#include <jaylib.h>\n")?;

		let mut main_fn = String::new();

		// TODO: Consider creating an efficient string writing system
		// In particular, each function being compiled will need its own string...
		self.push_indent();
		self.compile_stmts(stmts, &mut main_fn)?;
		self.pop_indent();

		print!("{}", self.prelude);
		for fun in &self.function_defs {
			print!("{}", fun);
		}
		print!("int\nmain(void) {{\n{}}}\n", main_fn);

		Ok(())
	}
}