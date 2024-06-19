use core::fmt;
use std::{collections::HashSet, fmt::Write, rc::Rc};

use crate::stmt::Function;
use crate::VarRef;
use crate::{expr::Expr, scanner::Token, stmt::Stmt, Lox};
use crate::scanner::LoxValue;
use crate::scanner::TokenType::*;


pub struct Compiler<'a> {
	pub lox: &'a mut Lox,

	/// Contains the #include and all function forward-declarations.
	prelude: String,

	/// Contains all the function definitions.
	function_defs: Vec<String>,

	/// Contains every string used for names. This is for the fast hashing system.
	name_set: HashSet<String>,

	current_indent: i32,

	/// Tracks whether we currently have a 'locals' frame (wrt 'return' statements.)
	has_locals_frame: bool,
}

impl<'a> Compiler<'a> {
	pub fn new(lox: &'a mut Lox) -> Self {
		return Compiler {
			lox,
			prelude: String::new(),
			function_defs: Vec::new(),
			name_set: HashSet::new(),
			current_indent: 0,

			// The main function has no locals frame.
			has_locals_frame: false,
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

	fn add_name_str(&mut self, name: &str) {
		if self.name_set.contains(name) { return; }
		self.name_set.insert(name.to_string());
	}

	fn binary_op(&mut self, into: &mut String, left: &Expr, op: &Token, right: &Expr) -> fmt::Result {
		let fun = match op.typ {
			Minus => "jay_op_sub",
			Plus => "jay_op_add",
			Slash => "jay_op_div",
			Star => "jay_op_mul",
			BangEqual => "jay_op_neq",
			EqualEqual => "jay_op_eq",
			Greater => "jay_op_gt",
			GreaterEqual => "jay_op_ge",
			Less => "jay_op_lt",
			LessEqual => "jay_op_le",
			_ => unreachable!()
		};

		self.compile_expr(left, into)?;
		self.compile_expr(right, into)?;
		self.indent(into);
		write!(into, "{fun}();\n")?;

		Ok(())
	}

	fn compile_expr(&mut self, expr: &Expr, into: &mut String) -> fmt::Result {
		match expr {
			Expr::Binary { left, operator, right } => {
				self.binary_op(into, left, operator, right)?;
			},
			Expr::Call { callee, paren, arguments } => {
				// Push all args, push the callee, then do jay_op_call.

				for arg in arguments {
					self.compile_expr(arg, into)?;
				}

				self.compile_expr(callee, into)?;

				self.indent(into);
				write!(into, "jay_op_call({});\n", arguments.len())?;
			},
			Expr::Get { object, name } => {
				todo!();
			},
			Expr::Grouping(inner) => {
				self.compile_expr(inner, into)?;
			},
			Expr::Literal(value) => {
				self.indent(into);
				match value {
					LoxValue::Nil => into.push_str("jay_op_null()"),
					LoxValue::String(chars) => write!(into, "jay_op_string(\"{}\")", chars)?,
					LoxValue::Number(value) => write!(into, "jay_op_number({value})")?,
					LoxValue::Bool(value) => write!(into, "jay_op_boolean({value})")?,
				}
				write!(into, ";\n")?;
			},
			Expr::Logical { left, operator, right } => {
				todo!();
			}
			Expr::Set { object, name, value } => {
				todo!();
			},
			Expr::Super { keyword, method, resolved } => {
				todo!();
			},
			Expr::This { keyword, resolved } => {
				todo!();
			}
			Expr::Unary { operator, right } => {
				let op = match operator.typ {
					Bang => "jay_op_not",
					Minus => "jay_op_negate",
					_ => unreachable!()
				};

				self.indent(into);
				self.compile_expr(right, into)?;
				write!(into, "{op}();\n")?;
			},
			Expr::Variable { name, identity } => {
				self.indent(into);
				write!(into, "jay_push(")?;
				self.compile_var(*identity, into)?;
				write!(into, ");\n")?;
			},
			Expr::Assign { name, value, identity } => {
				self.compile_expr(value, into)?;
				self.indent(into);
				write!(into, "jay_push(")?;
				self.compile_var(*identity, into)?;
				write!(into, " = jay_pop());\n")?;
			},
		}

		Ok(())
	}

	fn compile_var(&mut self, var: VarRef, into: &mut String) -> fmt::Result {
		match self.lox.get_var_type(var) {
			crate::VarType::Local => {
				write!(into, "locals.at[{}]", self.lox.get_var_mut(var).index)
			},
			crate::VarType::Parameter => {
				write!(into, "args[{}]", self.lox.get_var_mut(var).index)
			},
			crate::VarType::Captured => todo!(),
			crate::VarType::Global => {
				write!(into, "globals[{}]", self.lox.get_var_mut(var).index)
			}
		}
	}

	// NOTE: Provide 'into' if the function should be stored in the scope
	// (So set to None when generated methods)
	fn compile_function(&mut self, fun: &Function, mangled_name: &String) -> fmt::Result {
		let mut def = String::new();

		// Track 'has_locals_frame' down the call stack
		let enclosing_locals = self.has_locals_frame;
		self.has_locals_frame = (fun.local_count > 0);

		// Reset indent for top-level functions
		let enclosing_indent = self.current_indent;
		self.current_indent = 0;

		// Add the mangled name to the function definition list
		writeln!(self.prelude, "jay_value {mangled_name}(jay_value *arguments, jay_closure *closure);")?;

		// Start writing the function definition
		writeln!(def, "jay_value\n{}(jay_value *args, jay_closure *closure) {{", mangled_name)?;
		self.push_indent();

		// Create the 'locals' struct.
		if self.has_locals_frame {
			writeln!(def,r#"	struct {{
		size_t count;
		jay_value at[{0}];
	}} locals;
	locals.count = {0};
	jay_push_frame(&locals);"#, fun.local_count)?;
		}

		// TODO: Determine which functions create a new closure, and which ones simply
		// pass down their parent closure.
		writeln!(def, "\tjay_instance *scope = NULL;\n")?;

		// Generate the code inside the function.
		self.compile_stmts(&fun.body, &mut def)?;

		// Finally, put a default return value.
		if self.has_locals_frame {
			writeln!(def, "\tjay_pop_frame();")?;
		}
		writeln!(def, "\treturn jay_null();")?;

		// End the function.
		self.pop_indent();
		writeln!(def, "}}")?;

		self.function_defs.push(def);

		self.current_indent = enclosing_indent;

		// Restore locals
		self.has_locals_frame = enclosing_locals;
		
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
			Stmt::Class { name, methods, superclass } => {
				todo!()
			},
			Stmt::Expression(expr) => {
				self.compile_expr(expr, into)?;
				// After the expression is done, pop the unused value.
				self.indent(into);
				into.push_str("jay_pop();\n");
			},
			Stmt::Function(fun) => {
				let mangled_name = fun.name.lexeme.clone();
				self.compile_function(fun, &mangled_name)?;

				// In the outer scope, we need to have a reference to this function
				// in our closure. And, that function's closure is our 'scope.' So,
				// create a new value.
				//
				// Because we create this value in the compiled code at the same place
				// that the function is defined in the AST, it should have the same
				// ordering properties (e.g. not being visible beforehand).
			
				self.indent(into);
				// Essentially, generate an assignment with the var.
				self.compile_var(fun.identity, into)?;
				writeln!(into, " = jay_fun_from({}, {}, scope);",
					mangled_name, fun.param_count)?;
			},
			Stmt::If { condition, then_branch, else_branch } => {
				self.compile_expr(condition, into)?;
				self.indent(into);
				// Note: We have to use braces due to the fact that expressions can be multi-line.
				into.push_str("if(jay_pop_condition()) {\n");

				self.push_indent();
				self.compile_stmt(then_branch, into)?;
				self.pop_indent();

				self.indent(into);
				into.push_str("}\n");
				if let Some(else_branch) = else_branch {
					// Note: This should be OK because we should always add a '\n' after statements anyways...
					self.indent(into);
					into.push_str("else {\n");

					self.push_indent();
					self.compile_stmt(&else_branch, into)?;
					self.pop_indent();

					self.indent(into);
					into.push_str("}\n");
				}
			},
			Stmt::Print(expr) => {
				self.compile_expr(expr, into)?;
				self.indent(into);
				into.push_str("jay_op_print();\n");
			},
			Stmt::Return { keyword, value } => {
				match value {
					Some(value) => { self.compile_expr(value, into)?; },
					None => {
						self.indent(into); 
						into.push_str("jay_op_null();\n"); 
					}
				};

				// Pop the locals frame if we have it.
				if self.has_locals_frame {
					self.indent(into);
					into.push_str("jay_pop_frame();\n");
				}

				self.indent(into);
				into.push_str("return jay_pop();\n");
			},
			Stmt::Var { name, initializer, identity } => {
				// The only real role this statement plays, given that the variables are
				// all generally "initialized" already, is explicitly initializing them
				// to jay_null().
				//
				// Note that this could be perhaps 'optimized' by simply getting rid of it...
				// But there's likely little/no value to that.

				match initializer {
					Some(initializer) => { self.compile_expr(initializer, into)?; },
					None => {
						self.indent(into);
						into.push_str("jay_op_null();\n");
					}
				}

				self.indent(into);
				self.compile_var(*identity, into)?;
				write!(into, " = jay_pop();\n")?;
			},
			Stmt::While { condition, body } => {
				self.indent(into);
				into.push_str("for(;;) {\n");

				self.push_indent();
				self.compile_expr(condition, into)?;
				self.indent(into);
				into.push_str("if(!jay_pop_condition()) { break; }\n");

				self.compile_stmt(body, into)?;

				self.pop_indent();
				self.indent(into);
				into.push_str("}\n");
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

	pub fn compile(&mut self, stmts: &Vec<Stmt>, globals_count: u32) -> fmt::Result {
		// Setup 'this' and 'super' names so that they always work
		self.add_name_str("this");
		self.add_name_str("super");
		// Also 'init' for convenience.
		self.add_name_str("init");

		// Write the first part of the prelude
		writeln!(self.prelude, "/*** This C file created by jaylox https://github.com/HoneyPony/jaylox ***/")?;
		writeln!(self.prelude, "#include \"jaylib/jaylib.h\"\n")?;

		// Write the globals array to the prelude
		writeln!(self.prelude, "static jay_value globals[{globals_count}];")?;

		let mut main_fn = String::new();

		// TODO: Consider creating an efficient string writing system
		// In particular, each function being compiled will need its own string...
		self.push_indent();

		// Setup the JAY_THIS for use in jaylib.h
		writeln!(main_fn, "\tJAY_THIS = NAME_this;\n")?;

		// The most important responsibility of main() is initializing the stack
		writeln!(main_fn, "\tjay_stack_ptr = jay_stack;\n")?;
		writeln!(main_fn, "\tjay_frames_ptr = 0;\n")?;

		// Note: Built-in functions will also be set up in main, but this requires
		// parser support..?

		self.indent(&mut main_fn);
		// Create the scope for the main fn
		writeln!(main_fn, "jay_closure *scope = NULL;")?;
		
		// Compile the actual top-level code (any normal statements will go
		// into main; other things will go into their own functions)
		self.compile_stmts(stmts, &mut main_fn)?;
		self.pop_indent();

		// First: prelude, containing all function declarations
		print!("{}", self.prelude);

		println!("\n/* --- NAME Definitions --- */\n");

		// Second, NAME_ definitions
		// Note that 0 is the TOMBSTONE so we cannot use it for a NAME
		let mut name_value: usize = 1;
		for name in &self.name_set {
			println!("#define NAME_{name} ((size_t){name_value})");
			name_value += 1;
		}
		println!("\n/* --- Function Definitions --- */\n");

		// Third, function definitions
		for fun in &self.function_defs {
			print!("{}", fun);
		}

		println!("\n/* --- main() --- */\n");

		// Last, main function definition (could go earlier too)
		print!("int\nmain(void) {{\n{}}}\n", main_fn);

		Ok(())
	}
}