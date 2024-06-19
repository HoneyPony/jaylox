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
}

impl<'a> Compiler<'a> {
	pub fn new(lox: &'a mut Lox) -> Self {
		return Compiler {
			lox,
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

	fn add_name_str(&mut self, name: &str) {
		if self.name_set.contains(name) { return; }
		self.name_set.insert(name.to_string());
	}

	fn binary_op(&mut self, into: &mut String, left: &Expr, op: &Token, right: &Expr) -> fmt::Result {
		let fun = match op.typ {
			Minus => "jay_sub",
			Plus => "jay_add",
			Slash => "jay_div",
			Star => "jay_mul",
			BangEqual => "jay_neq",
			EqualEqual => "jay_eq",
			Greater => "jay_gt",
			GreaterEqual => "jay_ge",
			Less => "jay_lt",
			LessEqual => "jay_le",
			_ => unreachable!()
		};

		write!(into, "{fun}(")?;
		self.compile_expr(left, into)?;
		into.write_str(", ")?;
		self.compile_expr(right, into)?;
		into.write_str(")")?;

		Ok(())
	}

	fn compile_expr(&mut self, expr: &Expr, into: &mut String) -> fmt::Result {
		match expr {
			Expr::Binary { left, operator, right } => {
				self.binary_op(into, left, operator, right)?;
			},
			Expr::Call { callee, paren, arguments } => {
				write!(into, "jay_call(")?;
				self.compile_expr(callee, into)?;
				// To do the call, we have to look up both the function and the
				// closure inside the jay_value.
				//
				// In the case of simple functions, especially those without
				// any needed closure, we could optimize this to not need to 
				// go through another function just to call a function. But for
				// now, this is how it is...
				write!(into, ", {}, ", arguments.len())?;
				if arguments.is_empty() {
					// If we have no arguments, just pass a NULL.
					write!(into, "NULL")?;
				}
				else {
					write!(into, " (jay_value[]){{")?;
					let mut comma = false;
					for arg in arguments {
						if comma {
							into.push_str(", ");
						}
						self.compile_expr(arg, into)?;
						comma = true;
					}
					write!(into, "}}")?;
				}
				// That's the end. The closure will be looked up inside the instance.
				write!(into, ")")?;
			},
			Expr::Get { object, name } => {
				into.push_str("jay_get(");
				self.compile_expr(object, into)?;
				self.add_name(name);
				write!(into, ", NAME_{})", name.lexeme)?;
			},
			Expr::Grouping(inner) => {
				// Note: I don't know if it's really necessary to do parens here, but whatever..
				into.push('(');
				self.compile_expr(inner, into)?;
				into.push(')');
			},
			Expr::Literal(value) => {
				match value {
					LoxValue::Nil => into.push_str("jay_null()"),
					LoxValue::String(chars) => write!(into, "jay_string(\"{}\")", chars)?,
					LoxValue::Number(value) => write!(into, "jay_number({value})")?,
					LoxValue::Bool(value) => write!(into, "jay_boolean({value})")?,
					_ => panic!("Don't know how to compile a literal of this kind.")
				}
			},
			Expr::Logical { left, operator, right } => {
				// Note: C logical operators are short-circuiting. So, if we use them,
				// we can implement Lox's short-circuiting operations without too much hassle.
				let op = match operator.typ {
					Or => "||",
					And => "&&",
					_ => unreachable!()
				};

				write!(into, "(jay_truthy(")?;
				self.compile_expr(left, into)?;
				write!(into, ") {op} (jay_truthy(")?;
				self.compile_expr(right, into)?;
				write!(into, "))")?;
			}
			Expr::Set { object, name, value } => {
				write!(into, "jay_set(")?;
				self.compile_expr(object, into)?;
				self.add_name(name);
				write!(into, ", NAME_{}, ", name.lexeme)?;
				self.compile_expr(value, into)?;
				write!(into, ")")?;
			},
			Expr::Super { keyword, method, resolved } => {
				self.add_name(method);
				write!(into, "jay_get_super(scope, NAME_{})", method.lexeme)?;
			},
			Expr::This { keyword, resolved } => {
				write!(into, "jay_lookup(scope, NAME_this)")?;
			}
			Expr::Unary { operator, right } => {
				let op = match operator.typ {
					Bang => "jay_not",
					Minus => "jay_negate",
					_ => unreachable!()
				};

				write!(into, "{op}(")?;
				self.compile_expr(right, into)?;
			},
			Expr::Variable { name, identity } => {
				self.compile_var(*identity, into);
			},
			Expr::Assign { name, value, identity } => {
				write!(into, "(")?;
				self.compile_var(*identity, into);
				write!(into, " = ")?;
				self.compile_expr(value, into)?;
				write!(into, ")")?;
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

		// Reset indent for top-level functions
		let enclosing_indent = self.current_indent;
		self.current_indent = 0;

		// Add the mangled name to the function definition list
		writeln!(self.prelude, "jay_value {mangled_name}(jay_value *arguments, jay_instance *closure);")?;

		// Start writing the function definition
		writeln!(def, "jay_value\n{}(jay_value *args, jay_instance *closure) {{", mangled_name)?;
		self.push_indent();

		// Create the 'locals' struct.
		writeln!(def,r#"	struct {{
		size_t count;
		jay_value at[{0}];
	}} locals;
	locals.count = {0};
	jay_push_frame(&locals);"#, fun.local_count)?;

		// TODO: Determine which functions create a new closure, and which ones simply
		// pass down their parent closure.
		writeln!(def, "\tjay_instance *scope = NULL;\n")?;

		// Generate the code inside the function.
		self.compile_stmts(&fun.body, &mut def)?;

		// Finally, put a default return value.
		writeln!(def, "\tjay_pop_frame();")?;
		writeln!(def, "\treturn jay_null();")?;

		// End the function.
		self.pop_indent();
		writeln!(def, "}}")?;

		self.function_defs.push(def);

		self.current_indent = enclosing_indent;
		
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
				self.indent(into);
				self.compile_expr(expr, into)?;
				into.push_str(";\n");
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
				self.indent(into);
				into.push_str("if(jay_truthy(");
				// TODO: Do we need a prelude for calls?
				self.compile_expr(condition, into)?;
				into.push_str("))\n");
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
			Stmt::Var { name, initializer, identity } => {
				// The only real role this statement plays, given that the variables are
				// all generally "initialized" already, is explicitly initializing them
				// to jay_null().
				//
				// Note that this could be perhaps 'optimized' by simply getting rid of it...
				// But there's likely little/no value to that.

				self.indent(into);
				self.compile_var(*identity, into)?;
				write!(into, " = ")?;
				
				match initializer {
					Some(initializer) => { self.compile_expr(initializer, into)?; },
					None => { into.push_str("jay_null()"); }
				}
				into.push_str(";\n");
			},
			Stmt::While { condition, body } => {
				self.indent(into);
				into.push_str("while(jay_truthy(");
				self.compile_expr(condition, into)?;
				into.push_str("))\n");
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

		self.indent(&mut main_fn);
		// Setup the JAY_THIS for use in jaylib.h
		writeln!(main_fn, "JAY_THIS = NAME_this;")?;

		// Note: Built-in functions will also be set up in main, but this requires
		// parser support..?

		self.indent(&mut main_fn);
		// Create the scope for the main fn
		writeln!(main_fn, "jay_instance *scope = NULL;")?;
		
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