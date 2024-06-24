use core::fmt;
use std::collections::HashMap;
use std::{collections::HashSet, fmt::Write};

use crate::stmt::{Class, Function};
use crate::VarRef;
use crate::{expr::Expr, scanner::Token, stmt::Stmt, Lox};
use crate::scanner::{LoxValue, TokenType};
use crate::scanner::TokenType::*;


pub struct Compiler<'a, Writer: std::io::Write> {
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

	/// Tracks whether we currently have a 'captures' frame.
	has_captures_frame: bool,

	/// Keeps track of how many hops each captured variable is from the current function.
	/// Each function that creates a dynamic scope / closure will increase the hops
	/// of all captured variables by 1 (and add its own); otherwise, the hops stay the same,
	/// which should reduce pointer chasing.
	/// 
	/// (We could implement it in a way with even less pointer chasing, but this is a good
	/// starting point)
	captured_depths: HashMap<VarRef, u32>,

	/// The set of strings that we've already used as function names.
	mangled_set: HashSet<String>,

	writer: Writer,
}

impl<'a, Writer: std::io::Write> Compiler<'a, Writer> {
	pub fn new(lox: &'a mut Lox, writer: Writer) -> Self {
		return Compiler {
			lox,
			prelude: String::new(),
			function_defs: Vec::new(),
			name_set: HashSet::new(),
			current_indent: 0,

			// The main function has no locals frame or captures frame.
			has_locals_frame: false,
			has_captures_frame: false,

			captured_depths: HashMap::new(),

			mangled_set: HashSet::new(),

			writer,
		}
	}

	fn mangle(&mut self, starting_point: String) -> String {
		// "Fast" path: If the name is unused, just use it (and track so)
		if !self.mangled_set.contains(&starting_point) {
			self.mangled_set.insert(starting_point.clone());
			return starting_point;
		}

		let mut suffix: u64 = 0;
		loop {
			let candidate: String = format!("{}_{}", starting_point, suffix);
			if !self.mangled_set.contains(&candidate) {
				self.mangled_set.insert(candidate.clone());
				return candidate;
			}

			// There is no way we get through all values of u64 without colliding...
			suffix += 1;
		}
	}

	fn push_indent(&mut self) {
		self.current_indent += 1;
	}

	fn pop_indent(&mut self) {
		self.current_indent -= 1;
	}

	fn indent(&self, into: &mut String) {
		for _ in 0..self.current_indent {
			into.push('\t');
		}
	}

	fn add_name(&mut self, name: &Token) {
		if self.name_set.contains(&name.lexeme) { return; }
		self.name_set.insert(name.lexeme.clone());
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
			Expr::Call { callee, arguments, .. } => {
				// Push all args, push the callee, then do jay_op_call.
				// Note: It is very significant that we always push all the
				// arguments, no matter what kind of call/invoke we do. They
				// all need the arguments pushed onto the stack as such.
				for arg in arguments {
					self.compile_expr(arg, into)?;
				}

				match callee.as_ref() {
					// If the callee is a Get, then instead of making a new bound
					// method, do an invoke
					Expr::Get { object, name } => {
						self.compile_expr(object, into)?;

						self.add_name(name);
						self.indent(into);
						write!(into, "jay_op_invoke(NAME_{}, {});\n",
							name.lexeme, arguments.len())?;
					}
					// For superclass calls, use invoke_super
					Expr::Super { method, identity, this_identity, .. } => {
						self.add_name(method);
						self.indent(into);
						write!(into, "jay_op_invoke_super(")?;
						self.compile_var(*this_identity, into)?;
						write!(into, ", NAME_{}, ", method.lexeme)?;
						self.compile_var(*identity, into)?;
						writeln!(into, ", {});", arguments.len())?;
					},
					// For regular calls, just compile the inner expression,
					// and then do a normal call.
					_ => {
						self.compile_expr(callee, into)?;

						self.indent(into);
						write!(into, "jay_op_call({});\n", arguments.len())?;
					}
				}
				

				
			},
			Expr::Get { object, name } => {
				self.compile_expr(object, into)?;

				self.add_name(name);
				self.indent(into);
				write!(into, "jay_op_get(NAME_{});\n", name.lexeme)?;
			},
			Expr::Grouping(inner) => {
				self.compile_expr(inner, into)?;
			},
			Expr::Literal(value) => {
				self.indent(into);
				match value {
					LoxValue::Nil => into.push_str("jay_push(jay_box_nil())"),
					LoxValue::String(ptr) => {
						// String constants are looked up inside a global array, so
						// that we only have to initialize them once.
						write!(into, "jay_push(global_string_constants[{}])", ptr.to_number())?;
					},
					LoxValue::Number(value) => write!(into, "jay_op_number({value})")?,
					LoxValue::Bool(value) => write!(into, "jay_op_bool({value})")?,
				}
				write!(into, ";\n")?;
			},
			Expr::Logical { left, operator, right } => {
				// We must implement the short-circuiting semantic. This is actually
				// somewhat straightforward, as if we, for example, "push" the left
				// expression and then find that we need to short circuit, we simply
				// skip doing anything with the right operand. Otherwise, we pop,
				// and then simply push the right operand.

				// To understand this:
				// For and, if !truthy(left), we want to return left. But, entering the
				// if statement means we rreturn right. So for and, we do 
				// if(truthy(left)) { evaluate(right) }
				// 
				// Which is somewhat different from how it was implemented in the
				// interpreter.
				let invert = 
				match operator.typ {
					TokenType::And => ' ',
					TokenType::Or  => '!',
					_ => unreachable!()
				};

				// First, generate the left expression.
				self.compile_expr(left, into)?;
				// Check if the left expression should short-circuit.
				self.indent(into);
				write!(into, "if({}jay_truthy(jay_top())) {{\n", invert)?;

				self.push_indent();

				// If it's not short-circuiting, pop it, then evaluate the right 
				// expression (and leave it on top of the stack).
				self.indent(into);
				write!(into, "jay_pop();\n")?;

				self.compile_expr(right, into)?;
				self.pop_indent();
				
				self.indent(into);
				write!(into, "}}\n")?;

				// Done!
			}
			Expr::Set { object, name, value } => {
				self.compile_expr(value, into)?;
				self.compile_expr(object, into)?;
				
				self.add_name(name);
				self.indent(into);
				writeln!(into, "jay_op_set(NAME_{});", name.lexeme)?
			},
			Expr::Super { method, identity, this_identity, .. } => {
				// Super is a little unique in that it is one of the only
				// operators that explicitly takes a non-stack argument (because
				// the superclass is always a variable, which is necessarily
				// already reachable by GC)
				// Similarly, 'this' is always a variable. So super actually
				// takes two non-stack arguments.

				// TODO: Is "already reachable" enough when talking about
				// a compacting collector? We need to reach every reference
				// to something... will have to think about it while designing
				// the garbage collector...

				self.add_name(method);
				self.indent(into);
				write!(into, "jay_op_get_super(")?;
				self.compile_var(*this_identity, into)?;
				write!(into, ", NAME_{}, ", method.lexeme)?;
				self.compile_var(*identity, into)?;
				writeln!(into, ");")?;
			},
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
			// Variables and This are essentially equivalent. Actually... I guess we could
			// scrap Expr::This, and then just generate Expr::Variable in its place...
			Expr::Variable { identity, .. } | Expr::This { identity, .. } => {
				self.indent(into);
				write!(into, "jay_push(")?;
				self.compile_var(*identity, into)?;
				write!(into, ");\n")?;
			},
			Expr::Assign { value, identity, .. } => {
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
			// CapturedParameters are made to be handled the same, based on
			// some special code generated by compile_function()
			crate::VarType::Captured | crate::VarType::CapturedParameter(_) => {
				// Here, based on the depth, we add some number of "parent" traversals
				// to the current "closure".
				//
				// If this function created a new scope than:
				//   "closure" == depth of 1.
				//   "scope" == depth of 0.
				//   "closure->parent" == depth of 2.
				// otherwise,
				//   "closure" == "scope" == depth of 0.
				//   "closure->parent" == depth of 1.
				//
				// Note that we could always do scope = 0, scope->parent = 1,
				// and so forth, but this doesn't let us make use of the fact
				// that scope->parent == closure in some cases.
				// 
				// This is due to the fact that we distinguish between the current
				// scope and the closure passed in.
				let depth = *self.captured_depths.get(&var)
					.expect("Internal compiler error: Tried to access invalid captured variable info.");
				let index = self.lox.get_var_mut(var).index;

				if depth == 0 && self.has_captures_frame {
					write!(into, "scope->values[{index}]")?;
				}
				else {
					// The hops amount is 'depth' if our scope == closure. This is because
					// hopping to closure would usually hop us up one, but because we didn't
					// create a scope, it instead hopped us up 0.
					//
					// If captures does hop us up 1, then to get to depth only requires depth - 1
					// more steps.
					let hops = if self.has_captures_frame { depth - 1 } else { depth };
					write!(into, "closure->")?;
					// Walk up correct number of parents.
					for _ in 0..hops {
						write!(into, "parent->")?;
					}
					// Finally, access the values array.
					write!(into, "values[{index}]")?;
				}

				Ok(())
			},
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
		self.has_locals_frame = fun.local_count > 0;

		let enclosing_captures = self.has_captures_frame;
		self.has_captures_frame = fun.captured.len() > 0;

		// Reset indent for top-level functions
		let enclosing_indent = self.current_indent;
		self.current_indent = 0;

		// Add the mangled name to the function definition list
		writeln!(self.prelude, "jay_value {mangled_name}(jay_value *arguments, jay_closure *closure);")?;

		// Start writing the function definition
		writeln!(def, "jay_value\n{}(jay_value *args, jay_closure *closure) {{", mangled_name)?;
		self.push_indent();

		// There are two separate but related 'scope' variables.
		// First is 'scope', which is passed down the call chain so that lower-down
		// functions can use the closure.
		//
		// Second is 'gc_scope', which is part of the 'stackframe' stack. This
		// scope can simply be left as NULL if we don't need to hang on to any
		// other scope in the garbage collector.
		let mut gc_scope = "NULL";

		// If we have any captured vars, then we need to increase the depth of
		// all the current ones, and add our own.
		if fun.captured.len() > 0 {
			for (_, v) in &mut self.captured_depths {
				*v += 1;
			}

			// The "scope" value for this function will be a new scope,
			// instead of being the same as the parent scope.
			// We allocate as many variables as are captured from this function.
			writeln!(def, "\tjay_closure *scope = jay_new_scope(closure, {});\n", fun.captured.len())?;

			// Add the new variables. Note that "captured" here means a function
			// LOWER DOWN the chain captured them. Our own function sort of
			// treats them as normal variables.
			for v in &fun.captured {
				let None = self.captured_depths.insert(*v, 0) else {
					// Safety check that everything is working as expected
					panic!("Captured variables should only come from one function");
				};

				// For each captured variable, if it was a parameter, then create code
				// mapping the parameter value into the closure value.
				match self.lox.get_var_type(*v) {
					crate::VarType::CapturedParameter(param_idx) => {
						// TODO: Consider adding get_var() / get_var_idx()
						writeln!(def, "\tscope->values[{}] = args[{}];\n", self.lox.get_var_mut(*v).index, param_idx)?;
					},
					_ => { /* No special treatment needed */ }
				}
			}

			// Captured funs need to store a reference to their scope in case
			// it's the only one.
			gc_scope = "scope";
		}
		else {
			writeln!(def, "\tjay_closure *scope = closure;\n")?;
		}

		// Create the 'locals' struct.
		if self.has_locals_frame {
			writeln!(def,r#"	struct {{
		size_t count;
		jay_closure *gc_scope;
		jay_value at[{0}];
	}} locals;
	locals.count = {0};
	locals.gc_scope = {1};
	jay_push_frame(&locals);"#, fun.local_count, gc_scope)?;
		}

		

		// Generate the code inside the function.
		self.compile_stmts(&fun.body, &mut def)?;

		// Finally, put a default return value.
		if self.has_locals_frame {
			writeln!(def, "\tjay_pop_frame();")?;
		}
		if fun.is_initializer() {
			// For initializers, we can directly figure out the 'this' value here,
			// so we might as well directly return it.
			// this = param_count - 1, i.e. the last index in the param array.
			// Note that it's impossible for 'this' to become a closure param.
			// But, if we WANTED to support that case, we could simply modify
			// the parser to always add 'return this;' to the end of a function.
			writeln!(def, "\treturn args[{}];", fun.param_count - 1)?;
		}
		else {
			writeln!(def, "\treturn jay_box_nil();")?;
		}

		// End the function.
		self.pop_indent();
		writeln!(def, "}}")?;

		self.function_defs.push(def);

		self.current_indent = enclosing_indent;

		// Restore locals and captures and initializer
		self.has_locals_frame = enclosing_locals;
		self.has_captures_frame = enclosing_captures;
		
		Ok(())
	}

	fn compile_class_dispatcher(&mut self, class: &Class, mangled_name: &String) -> fmt::Result {
		let mut def = String::new();

		// Add the dispatcher forward-declaration
		// TODO: Consider having multiple blocks of forward-declares for organization / clarity
		writeln!(self.prelude, "jay_method* {mangled_name}(jay_class *class, size_t name);")?;
		
		writeln!(def, "jay_method*\n{mangled_name}(jay_class *class, size_t name) {{")?;

		// First, generate a switch-case for each name, looking up the associated function. But,
		// we actually do this in reverse, iterating through 0..n, then adding the associated
		// name to the switch-case.
		//
		// TODO: As an optimization, simply leave out any names that aren't referenced elsewhere.
		// There's no actual reason to do add_name here except for convenience.

		writeln!(def, "\tswitch(name) {{")?;

		for (idx, method) in class.methods.iter().enumerate() {
			self.add_name(&method.name);
			writeln!(def, "\t\tcase NAME_{}: return &class->methods[{}];", method.name.lexeme, idx)?;
		}

		writeln!(def, "\t}}\n")?;

		// If the switch-case didn't look it up, then look it up in the superclass.
		writeln!(def, "\tif(class->superclass) {{")?;
		writeln!(def, "\t\treturn class->superclass->dispatcher(class->superclass, name);")?;
		writeln!(def, "\t}}\n")?;

		// If we still didn't find it, return NULL.
		// In this case, we are probably inside a get expression, and then we will
		// look for a field. That will then return an error--and so we don't want
		// to return an error here.

		writeln!(def, "\treturn NULL;")?;

		writeln!(def, "}}")?;

		self.function_defs.push(def);

		Ok(())
	}

	fn compile_class(&mut self, class: &Class, mangled_name: &String) -> fmt::Result {
		// To create the class-defining function, we need the dispatcher.
		let dispatcher_mangled = self.mangle(
			format!("jdisp_{}", class.name.lexeme));
		self.compile_class_dispatcher(class, &dispatcher_mangled)?;

		let mut def = String::new();

		// Add the mangled name to the function definition list
		writeln!(self.prelude, "jay_value {mangled_name}(jay_value superclass, jay_closure *closure);")?;

		// Reset indent for top-level functions
		let enclosing_indent = self.current_indent;
		self.current_indent = 0;

		// Start writing the function definition
		writeln!(def, "jay_value\n{}(jay_value superclass, jay_closure *closure) {{", mangled_name)?;

		// First step: Allocate the actual class object
		writeln!(def, "\tjay_class *class = jay_gc_alloc(sizeof(*class) + (sizeof(jay_method) * {}), JAY_GC_CLASS);",
			class.methods.len())?;

		// Fill out the method table
		// TODO: If we want to be able to have init in a jay_bound_method, I geuss
		// it will also need to be part of the method table..?
		for (idx, method) in class.methods.iter().enumerate() {
			let method_mangled_name = self.mangle(
				format!("jm_{}_{}", class.name.lexeme, method.name.lexeme));
			
			// The C function should be the same, due to the 'this' variable being
			// automatically added at the end.
			self.compile_function(method, &method_mangled_name)?;

			// Methods do not store the closure, because the have a pointer back
			// to the class.
			writeln!(def, "\tclass->methods[{idx}] = jay_method_from(class, {method_mangled_name}, {});",
				method.param_count)?;
		}

		// The dispatcher is simply initialized to be the dispatcher function we
		// created. This function is dynamic in terms of superclass, and so
		// it is fine (if slower than it could be).
		//
		// TODO: Note that one possible optimization would be to figure out cases
		// where a class cannot have a superclass, and implement a faster dispatcher
		// for it. But, the extra checks for doing that might make it slower than
		// just always checking for the superclass...
		writeln!(def, "\tclass->dispatcher = &{dispatcher_mangled};")?;

		// The class tracks the closure for all methods.
		writeln!(def, "\tclass->closure = closure;")?;

		writeln!(def, "\tclass->methods_count = {};\n", class.methods.len())?;

		// Fill in the superclass
		// If it is nil, then the superclass is NULL, otherwise, it must be
		// a jay_class
		writeln!(def, "\tif(JAY_IS_NIL(superclass)) {{")?;
		writeln!(def, "\t\tclass->superclass = NULL;")?;
		writeln!(def, "\t}}\n\telse if(JAY_IS_CLASS(superclass)) {{")?;
		writeln!(def, "\t\tclass->superclass = JAY_AS_CLASS(superclass);")?;
		writeln!(def, "\t}}\n\telse {{")?;
		writeln!(def, "\t\toops(\"superclass must be class\");")?;
		writeln!(def, "\t}}")?;

		writeln!(def, "\treturn jay_box_class(class);\n")?;

		writeln!(def, "}}")?;

		self.current_indent = enclosing_indent;

		self.function_defs.push(def);

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
			Stmt::Class(class) => {
				let mangled_name = self.mangle(format!("jclass_{}", class.name.lexeme));

				self.compile_class(class, &mangled_name)?;

				self.indent(into);
				self.compile_var(class.identity, into)?;
				// The class is a function that takes a superclass and a scope,
				// and creates a class accordingly.
				// As such, the associated local variable is initialized by
				// calling this function.
				write!(into, " = {mangled_name}(/* superclass = */ ")?;

				// Compile the superclass variable/null
				if let Some(superclass) = class.superclass {
					self.compile_var(superclass, into)?;
				}
				else {
					write!(into, "jay_box_nil()")?;
				}

				writeln!(into, ", scope);")?;
			},
			Stmt::Expression(expr) => {
				self.compile_expr(expr, into)?;
				// After the expression is done, pop the unused value.
				self.indent(into);
				into.push_str("jay_pop();\n");
			},
			Stmt::Function(fun) => {
				let mangled_name = self.mangle(format!("jf_{}", fun.name.lexeme));
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
				self.compile_var(fun.identity.expect("non-methods must have an identity"), into)?;
				writeln!(into, " = jay_fun_from({}, {}, scope);",
					mangled_name, fun.param_count)?;
			},
			// TODO: Do we even need to have a var_name field on ExternFunction..?
			Stmt::ExternFunction { c_name, arity, identity, .. } => {
				// Like a function, except we don't need to compile anything -- just
				// insert the known name into the current scope.
				self.compile_var(*identity, into)?;
				writeln!(into, " = jay_fun_from({c_name}, {arity}, scope);")?;
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
			Stmt::Return { value, .. } => {
				match value {
					Some(value) => { self.compile_expr(value, into)?; },
					None => {
						self.indent(into); 
						into.push_str("jay_push(jay_box_nil());\n"); 
					}
				};

				// Pop the locals frame if we have it.
				if self.has_locals_frame {
					self.indent(into);
					into.push_str("jay_pop_frame();\n");
				}

				self.indent(into);
				// OPT: Return seems especially possible to optimize to always
				// just return an expression, if that expression is a single-fun-call.
				// I don't think there would be any way to lose a root reference
				// in that process.
				into.push_str("return jay_pop();\n");
			},
			Stmt::Var { initializer, identity, .. } => {
				// The only real role this statement plays, given that the variables are
				// all generally "initialized" already, is explicitly initializing them
				// to jay_box_nil().
				//
				// Note that this could be perhaps 'optimized' by simply getting rid of it...
				// But there's likely little/no value to that.

				match initializer {
					Some(initializer) => { self.compile_expr(initializer, into)?; },
					None => {
						self.indent(into);
						into.push_str("jay_push(jay_box_nil());\n");
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

	fn compile_to_buffers(&mut self, stmts: &Vec<Stmt>, globals_count: u32) -> Result<String, fmt::Error> {
		// Write the first part of the prelude
		writeln!(self.prelude, "/*** This C file created by jaylox https://github.com/HoneyPony/jaylox ***/")?;
		writeln!(self.prelude, "#include \"jaylib/jaylib.h\"\n")?;

		// Write the globals array to the prelude (and the string constants array)
		writeln!(self.prelude, "static jay_value globals[{globals_count}];")?;
		writeln!(self.prelude, "static jay_value global_string_constants[{}];\n", self.lox.string_constants.len())?;

		// Create the global-visit function
		writeln!(self.prelude, "static\nvoid\njay_gc_visit_globals(void) {{")?;
		writeln!(self.prelude, "\tfor(size_t i = 0; i < {globals_count}; ++i) {{")?;
		writeln!(self.prelude, "\t\tjay_gc_visit(&globals[i]);")?;
		writeln!(self.prelude, "\t}}")?;
		writeln!(self.prelude, "}}\n")?;

		let mut main_fn = String::new();

		// TODO: Consider creating an efficient string writing system
		// In particular, each function being compiled will need its own string...
		self.push_indent();

		// The most important responsibility of main() is initializing the gc and stack
		writeln!(main_fn, "\tjay_gc_init(16 * 1024 * 1024); // 16 megabytes")?;
		writeln!(main_fn, "\tjay_stack_ptr = jay_stack;")?;
		writeln!(main_fn, "\tjay_frames_ptr = 0;\n")?;

		// We have to set up all the string constants in main for the rest of the
		// code to use.
		for (idx, constant) in self.lox.string_constants.iter().enumerate() {
			writeln!(main_fn, "\tglobal_string_constants[{idx}] = jay_box_string(jay_string_from_literal(\"{constant}\"));")?;
		}

		// Note: Built-in functions will also be set up in main, but this requires
		// parser support..?

		self.indent(&mut main_fn);
		// Create the scope for the main fn
		writeln!(main_fn, "jay_closure *scope = NULL;")?;

		// Compile the actual top-level code (any normal statements will go
		// into main; other things will go into their own functions)
		self.compile_stmts(stmts, &mut main_fn)?;
		self.pop_indent();

		Ok(main_fn)
	}

	pub fn compile(&mut self, stmts: &Vec<Stmt>, globals_count: u32) -> std::io::Result<()> {
		// We use write! a lot and so get fmt::Result, but most of the compilation
		// process should not fail (unless we OOM or something). By contrast, it
		// is very possible that, while writing out to the self.writer, there is
		// an error (e.g. read-only file target).
		//
		// So, wrap all of the compiling-to-buffers in its own function, and assume
		// that it almost certainly won't error. Then, the error from writing to
		// self.writer can be returned to the caller.
		let main_fn = match self.compile_to_buffers(stmts, globals_count) {
			Ok(main_fn) => main_fn,
			Err(err) => {
				panic!("Compiler error while writing to buffers: {}", err);
			}
		};

		// First: prelude, containing all function declarations
		write!(self.writer, "{}", self.prelude)?;

		writeln!(self.writer, "\n/* --- NAME Definitions --- */\n")?;

		// Second, NAME_ definitions
		// Note that 0 is the TOMBSTONE so we cannot use it for a NAME
		let mut name_value: usize = 1;
		for name in &self.name_set {
			writeln!(self.writer, "#define NAME_{name} ((size_t){name_value})")?;
			name_value += 1;
		}
		writeln!(self.writer, "\n/* --- Function Definitions --- */\n")?;

		// Third, function definitions
		for fun in &self.function_defs {
			write!(self.writer, "{}", fun)?;
		}

		writeln!(self.writer, "\n/* --- main() --- */\n")?;

		// Last, main function definition (could go earlier too)
		write!(self.writer, "int\nmain(void) {{\n{}}}\n", main_fn)?;

		Ok(())
	}
}