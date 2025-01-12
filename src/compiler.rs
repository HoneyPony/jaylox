use core::fmt;
use std::collections::HashMap;
use std::{collections::HashSet, fmt::Write};

use crate::stmt::{Class, Function};
use crate::{CodegenOptions, VarRef};
use crate::{expr::Expr, scanner::Token, stmt::Stmt, Lox};
use crate::scanner::{LoxValue, TokenType};
use crate::scanner::TokenType::*;

macro_rules! inf_write {
	($into:expr, $($arg:tt)*) => {
		match write!($into, $($arg)*) {
			Ok(_) => {},
			Err(_) => {
				panic!("compiler: 'infallible' write to buffer failed");
			}
		}
	}
}

macro_rules! inf_writeln {
	($into:expr, $($arg:tt)*) => {
		match writeln!($into, $($arg)*) {
			Ok(_) => {},
			Err(_) => {
				panic!("compiler: 'infallible' write to buffer failed");
			}
		}
	}
}

enum Val {
	OnStack,
	DoubleConst(String),
	BoolConst(String),
	Literal(LoxValue),
	Variable(VarRef),
	This(VarRef),
}

impl Val {
	pub fn is_on_stack(&self) -> bool {
		match self {
			Val::OnStack => true,
			_ => false,
		}
	}
}

pub struct Compiler<'a, Writer: std::io::Write> {
	pub lox: &'a mut Lox,

	/// Contains the #include and all function forward-declarations.
	prelude: String,

	/// Contains all the function definitions.
	function_defs: Vec<String>,

	/// Contains every string used for names. This is for the fast hashing system.
	name_set: HashSet<String>,

	/// Contains every string that was used in a Set operation. That way, any
	/// name not used in a Set operation can be placed in an entirely different
	/// range of names, so that a fast-fail for whether a name is a field name
	/// is simply name < JAY_MAX_FIELD.
	name_set_setted: HashSet<String>,

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

	opt: CodegenOptions,

	tmp_idx: u32,
}

#[derive(Clone, Copy)]
enum Ty {
	Number,
	Bool,
	Instance,
}


impl<'a, Writer: std::io::Write> Compiler<'a, Writer> {
	pub fn new(lox: &'a mut Lox, writer: Writer, opt: CodegenOptions) -> Self {
		return Compiler {
			lox,
			prelude: String::new(),
			function_defs: Vec::new(),
			name_set: HashSet::new(),
			name_set_setted: HashSet::new(),
			current_indent: 0,

			// The main function has no locals frame or captures frame.
			has_locals_frame: false,
			has_captures_frame: false,

			captured_depths: HashMap::new(),

			mangled_set: HashSet::new(),

			writer,

			opt,

			tmp_idx: 0,
		}
	}

	fn tmp_name(&mut self) -> String {
		let name = format!("tmp{}", self.tmp_idx);
		self.tmp_idx += 1;
		name
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

	fn add_name(&mut self, name: &Token, setted: bool) {
		if setted {
			if self.name_set_setted.contains(&name.lexeme) { return; }
			self.name_set_setted.insert(name.lexeme.clone());
		}

		if self.name_set.contains(&name.lexeme) { return; }
		self.name_set.insert(name.lexeme.clone());
	}

	fn stackify(&mut self, val: &Val, into: &mut String) {
		match val {
			Val::OnStack => { 
				// If it's already on the stack, we don't have to do anything.
			},
			_ => {
				self.indent(into);
				inf_write!(into, "jay_push(");
				// Note: stackidx does not matter because we're not on the stack.
				self.compile_val(val, 0, into);
				inf_writeln!(into, ");");
			}
		}
	}

	// Addition is the most complicated binary operation to compile, and requires
	// its own function.
	fn add(&mut self, into: &mut String, left: &Expr, right: &Expr) -> Val {
		let left = self.compile_expr(left, into);
		let right = self.compile_expr(right, into);

		match (&left, &right) {
			// If both values are on the stack, we cannot help but use the most
			// generic version of the operation.
			(Val::OnStack, Val::OnStack) => {
				self.indent(into);
				inf_writeln!(into, "jay_stack_ptr[-2] = jay_add(jay_stack_ptr[-2], jay_stack_ptr[-1]);");
				self.indent(into);
				inf_writeln!(into, "jay_stack_ptr -= 1;");

				return Val::OnStack
			},
			_ => { }
		}

		// Here we know that the values CANNOT both be on the stack, as otherwise
		// we would have hit the match arm. So, we can always use stackidx = 1,
		// and there will be no out-of-order stack shenanigans happening.
		//
		// If one of the values is numerical, we can fence the other one then
		// generate a straight-up numerical add. Otherwise, we generate a generic
		// jay_add as we don't specially handle strings at the moment.

		let is_num = if self.is_val_numerical(&left) {
			self.num_fence_for("jay_fence_number", &right, 1, into);
			true
		}
		else if self.is_val_numerical(&right) {
			self.num_fence_for("jay_fence_number", &left, 1, into);
			true
		}
		else {
			// If we don't know that one of the values is numerical, then we should not
			// fence either of them (because they might be strings). Instead, disable
			// any numerical-specific code generation ('return false' here) and use
			// the general jay_add call.
			false
		};

		let has_stack_value = left.is_on_stack() || right.is_on_stack();

		// If we're not doing a numerical add, then simply generate a jay_add
		// and push to stack.
		if !is_num {
			self.indent(into);
			// If we have a stack value, overwrite it with the jay_add, otherwise
			// jay_push the new value on to the stack
			if has_stack_value {
				inf_write!(into, "jay_stack_ptr[-1] = ");
			}
			else {
				inf_write!(into, "jay_push(");
			}
			inf_write!(into, "jay_add(");
			self.compile_val(&left, 1, into);
			inf_write!(into, ", ");
			self.compile_val(&right, 1, into);
			if has_stack_value {
				inf_writeln!(into, ");");
			}
			else {
				// Close the jay_add and the jay_push
				inf_writeln!(into, "));");
			}

			return Val::OnStack;
		}

		// Finally, we have a numerical operation. Generate a new double temporary,
		// and return a Val::DoubleConst.

		let name = self.tmp_name();
		self.indent(into);
		inf_write!(into, "const double {name} = ");
		self.compile_val_as(&left, 1, Ty::Number, into);
		inf_write!(into, " + ");
		self.compile_val_as(&right, 1, Ty::Number, into);
		inf_writeln!(into, ";");

		Val::DoubleConst(name)
	}

	fn binary_stackop(&mut self, into: &mut String, left: &Expr, op: &Token, right: &Expr) -> Val {
		let fun = match op.typ {
			Plus => return self.add(into, left, right),
			BangEqual => "jay_op_neq",
			EqualEqual => "jay_op_eq",
			_ => unreachable!()
		};

		self.compile_expr_tostack(left, into);
		self.compile_expr_tostack(right, into);

		self.indent(into);
		inf_write!(into, "{fun}();\n");

		Val::OnStack
	}

	fn compile_val(&mut self, val: &Val, stackidx: u32, into: &mut String) {
		match val {
			Val::OnStack => {
				inf_write!(into, "jay_stack_ptr[-{stackidx}]");
			},
			Val::DoubleConst(what) => {
				inf_write!(into, "jay_box_number({what})");
			},
			Val::BoolConst(what) => {
				inf_write!(into, "jay_box_bool({what})");
			},
			Val::Literal(lit) => {
				self.compile_literal_inline(into, lit);
			},
			Val::Variable(ptr) => {
				self.compile_var(*ptr, into);
			},
			Val::This(ptr) => {
				self.compile_var(*ptr, into);
			}
		}
	}

	fn compile_val_as(&mut self, val: &Val, stackidx: u32, as_ty: Ty, into: &mut String) {
		match (as_ty, val) {
			(Ty::Number, Val::DoubleConst(n)) => {
				inf_write!(into, "{n}");
			},
			(Ty::Bool, Val::BoolConst(n)) => {
				inf_write!(into, "{n}");
			},
			(Ty::Number, Val::Literal(LoxValue::Number(n))) => {
				inf_write!(into, "{n}");
			},
			(Ty::Bool, Val::Literal(LoxValue::Bool(n))) => {
				inf_write!(into, "{n}");
			},

			(Ty::Instance, Val::This(_)) => {
				inf_write!(into, "this");
			}

			_ => {
				let ty = match as_ty {
					Ty::Number => "NUMBER",
					Ty::Bool => "BOOL",
					Ty::Instance => "INSTANCE",
				};
				inf_write!(into, "JAY_AS_{ty}(");
				self.compile_val(val, stackidx, into);
				inf_write!(into, ")");
			}
		}
	}

	fn compile_val_as_truthy(&mut self, val: &Val, stackidx: u32, into: &mut String) {
		match val {
			Val::BoolConst(what) => {
				inf_write!(into, "{what}");
			},
			Val::Literal(LoxValue::Bool(false) | LoxValue::Nil) => {
				inf_write!(into, "false");
			},
			Val::Literal(_) => {
				inf_write!(into, "true");
			},
			_ => {
				inf_write!(into, "jay_truthy(");
				self.compile_val(val, stackidx, into);
				inf_write!(into, ")");
			}
		}
	}

	fn fence(&mut self, fn_name: &str, val: &Val, stackidx: u32, into: &mut String) {
		self.indent(into);
		inf_write!(into, "{fn_name}(");
		self.compile_val(val, stackidx, into);
		inf_writeln!(into, ");");
	}

	fn num_fence_for(&mut self, fn_name: &str, val: &Val, stackidx: u32, into: &mut String) {
		// TODO: Consider getting rid of stackidx value?
		if !self.is_val_numerical(val) {
			self.fence(fn_name, val, stackidx, into);
		}
	}

	fn is_val_numerical(&mut self, val: &Val) -> bool {
		match val {
			Val::DoubleConst(_) => true,
			Val::Literal(LoxValue::Number(_)) => true,

			// For now, we don't have any type info about variables.
			Val::Variable(_) => false,

			// Nothing else can be considered definitively numerical.
			_ => false,
		}
	}

	fn stack_idx(val: &Val, prev: u32) -> u32 {
		return prev + match val {
			Val::OnStack => 1,
			_ => 0
		}
	}

	fn binary_fenceop_num_inputs(&mut self, into: &mut String, left: &Expr, op: &Token, right: &Expr) -> Val {
		let (op, ty) = match op.typ {
			Minus => ("-", Ty::Number),
			Slash => ("/",  Ty::Number),
			Star => ("*",  Ty::Number),
			
			Greater => (">", Ty::Bool),
			GreaterEqual => (">=", Ty::Bool),
			Less => ("<", Ty::Bool),
			LessEqual => ("<=", Ty::Bool),

			_ => unreachable!()
		};

		let out_ty_ctype = match ty {
			Ty::Number => "double",
			Ty::Bool => "bool",
			_ => unreachable!()
		};

		let left = self.compile_expr(left, into);
		let right = self.compile_expr(right, into);

		let right_stack_idx = Self::stack_idx(&right, 0);
		let left_stack_idx = Self::stack_idx(&left, right_stack_idx);

		self.num_fence_for("jay_fence_number", &left, left_stack_idx, into);
		self.num_fence_for("jay_fence_number", &right, right_stack_idx, into);

		let tmp_name = self.tmp_name();

		// Now that the types are checked, we can just directly generate the
		// operation.
		self.indent(into);
		inf_write!(into, "const {out_ty_ctype} {tmp_name} = ");
		self.compile_val_as(&left, left_stack_idx, Ty::Number, into);
		inf_write!(into, " {op} ");
		self.compile_val_as(&right, right_stack_idx, Ty::Number, into);
		inf_writeln!(into, ";");

		// Finally, we have to explicitly pop as many of the values as were on 
		// the stack.
		if left_stack_idx > 0 {
			self.indent(into);
			inf_writeln!(into, "jay_stack_ptr -= {left_stack_idx};");
		}

		match ty {
			Ty::Number => Val::DoubleConst(tmp_name),
			Ty::Bool => Val::BoolConst(tmp_name),
			_ => unreachable!()
		}
	}

	fn binary_op(&mut self, into: &mut String, left: &Expr, op: &Token, right: &Expr) -> Val {
		match op.typ {
			Plus | BangEqual | EqualEqual => {
				self.binary_stackop(into, left, op, right)
			}

			Minus | Slash | Star |
			Greater | GreaterEqual | Less | LessEqual => {
				self.binary_fenceop_num_inputs(into, left, op, right)
			}

			_ => unreachable!()
		}
	}

	fn compile_literal_inline(&mut self, into: &mut String, lit: &LoxValue) {
		match lit {
			LoxValue::Nil => into.push_str("jay_box_nil()"),
			LoxValue::String(ptr) => {
				// String constants are looked up inside a global array, so
				// that we only have to initialize them once.
				inf_write!(into, "global_string_constants[{}]", ptr.to_number());
			},
			LoxValue::Number(value) => inf_write!(into, "jay_box_number({value})"),
			LoxValue::Bool(value) => inf_write!(into, "jay_box_bool({value})"),
		}
	}

	fn compile_assign(&mut self, into: &mut String, value: &Expr, identity: &VarRef) -> Val {
		let val = self.compile_expr(value, into);
		self.indent(into);

		self.compile_var(*identity, into);
		inf_write!(into, " = ");
		self.compile_val(&val, 1, into);
		inf_write!(into, ";\n");

		if let Val::OnStack = val {
			self.indent(into);
			inf_writeln!(into, "jay_stack_ptr -= 1;");
		}

		// Return a value with the variable. This results in the assignment
		// also being able to propagate, etc.
		Val::Variable(*identity)
	}

	fn compile_expr_tostack(&mut self, expr: &Expr, into: &mut String) -> Val {
		let val = self.compile_expr(expr, into);
		self.stackify(&val, into);
		val
	}

	fn compile_expr(&mut self, expr: &Expr, into: &mut String) -> Val {
		match expr {
			Expr::Binary { left, operator, right } => {
				self.binary_op(into, left, operator, right)
			},
			Expr::Call { callee, arguments, .. } => {
				// Push all args, push the callee, then do jay_op_call.
				// Note: It is very significant that we always push all the
				// arguments, no matter what kind of call/invoke we do. They
				// all need the arguments pushed onto the stack as such.
				for arg in arguments {
					self.compile_expr_tostack(arg, into);
				}

				match callee.as_ref() {
					// If the callee is a Get, then instead of making a new bound
					// method, do an invoke
					Expr::Get { object, name } => {
						let val = self.compile_expr(object, into);
						// All values must be pushed on to the stack. Some, however,
						// may be directly used as an instance.
						self.stackify(&val, into);

						// All vals but 'this' must be fenced.
						match val {
							Val::This(_) => { },
							_ => {
								self.indent(into);
								inf_write!(into, "jay_fence_invoke(");
								self.compile_val(&val, 1, into);
								inf_writeln!(into, ");");
							}
						}

						self.add_name(name, false);
						self.indent(into);
						inf_write!(into, "jay_op_invoke(NAME_{}, {}, ",
							name.lexeme, arguments.len());

						self.compile_val_as(&val, 1, Ty::Instance, into);
						inf_writeln!(into, ");");
					}
					// For superclass calls, use invoke_super
					Expr::Super { method, class_identity, ..} => {
						self.add_name(method, false);
						self.indent(into);
						inf_write!(into, "jay_op_invoke_super(this, NAME_{}, ", method.lexeme);
						self.compile_var(*class_identity, into);
						inf_writeln!(into, ", {});", arguments.len());
					},
					// For regular calls, just compile the inner expression,
					// and then do a normal call.
					_ => {
						self.compile_expr_tostack(callee, into);

						self.indent(into);
						inf_write!(into, "jay_op_call({});\n", arguments.len());
					}
				}
				
				Val::OnStack
			},
			Expr::Grouping(inner) => {
				self.compile_expr(inner, into)
			},
			Expr::Literal(value) => {
				//self.indent(into);
				//inf_write!(into, "jay_push(");
				//self.compile_literal_inline(into, value);
				//inf_write!(into, ");\n");
				Val::Literal(value.clone())
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
				self.compile_expr_tostack(left, into);
				// Check if the left expression should short-circuit.
				self.indent(into);
				inf_write!(into, "if({}jay_truthy(jay_stack_ptr[-1])) {{\n", invert);

				self.push_indent();

				// If it's not short-circuiting, pop it, then evaluate the right 
				// expression (and leave it on top of the stack).
				self.indent(into);
				inf_write!(into, "jay_pop();\n");

				self.compile_expr_tostack(right, into);
				self.pop_indent();
				
				self.indent(into);
				inf_write!(into, "}}\n");

				// Done!
				Val::OnStack
			},
			Expr::Get { object, name } => {

				match object.as_ref() {
					Expr::This { .. } => {
						// For 'this' objects, we still need to end up with
						// 1 thing on the stack. Because we aren't pushing
						// the inner 'this' object, we can just increment
						// the stack pointer directly.
						//
						// TODO: If we ever add bounds checking, we will need
						// to do it here.
						self.indent(into);
						inf_writeln!(into, "jay_stack_ptr += 1;");
					},
					_ => {
						// Generate a fence for non-This objects. Also,
						// don't compile the inner expr for This objects.
						self.compile_expr_tostack(object, into);

						self.indent(into);
						inf_writeln!(into, "jay_fence_get(jay_stack_ptr[-1]);");
					}
				};

				self.add_name(name, false);
				self.indent(into);

				inf_write!(into, "jay_stack_ptr[-1] = ");

				match object.as_ref() {
					Expr::This { .. } => {
						inf_write!(into, "jay_get_instance(this, NAME_{});\n", name.lexeme);
					},
					_ => {
						inf_write!(into, "jay_get_instance(JAY_AS_INSTANCE(jay_stack_ptr[-1]), NAME_{});\n", name.lexeme);
					}
				}

				Val::OnStack
			},
			Expr::Set { object, name, value } => {
				self.compile_expr_tostack(value, into);

				match object.as_ref() {
					Expr::This { .. } => {
						// Make one extra spot on the stack ptr for the following
						// arithmetic.
						self.indent(into);
						inf_writeln!(into, "jay_stack_ptr += 1;");
					},
					_ => {
						// Generate a fence for non-This objects. Also,
						// don't compile the inner expr for This objects.
						self.compile_expr_tostack(object, into);

						self.indent(into);
						inf_writeln!(into, "jay_fence_set(jay_stack_ptr[-1]);");
					}
				}

				self.add_name(name, true);
				self.indent(into);

				// Write to -2 because the set expression takes 2 args.
				inf_write!(into, "jay_stack_ptr[-2] = ");

				match object.as_ref() {
					Expr::This { .. } => {
						// We know that 'this' is always an instance.
						// TODO: Maybe generate a 'const pointer' 'this' that
						// we can just reference directly..?
						inf_write!(into, "jay_set_instance(this, NAME_{}, jay_stack_ptr[-2]);\n", name.lexeme);
					},
					_ => {
						inf_write!(into, "jay_set_instance(JAY_AS_INSTANCE(jay_stack_ptr[-1]), NAME_{}, jay_stack_ptr[-2]);\n", name.lexeme);
					}
				}

				// Decrement stack pointer.
				self.indent(into);
				inf_write!(into, "jay_stack_ptr -= 1;");

				Val::OnStack
			},
			Expr::Super { method, class_identity, .. } => {
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

				self.add_name(method, false);
				self.indent(into);
				inf_write!(into, "jay_op_get_super(this, NAME_{}, ", method.lexeme);
				self.compile_var(*class_identity, into);
				inf_writeln!(into, ");");

				Val::OnStack
			},
			Expr::Unary { operator, right } => {
				match operator.typ {
					Bang => {
						let val = self.compile_expr(right, into);
						let name = self.tmp_name();

						self.indent(into);
						inf_write!(into, "const bool {name} = !");
						// If it's a stack object, it's index 1
						// An interesting thing about bool semantics is that
						// any value can be compiled as a bool... so we can't
						// necessarily optimize using fences, etc.
						//
						// (That said, if we somehow knew a Variable was a
						// Bool, then we could use that info.)
						self.compile_val_as_truthy(&val, 1, into);
						inf_writeln!(into, ";");
						
						// If it's a stack object, pop the value from the stack.
						if let Val::OnStack = val {
							self.indent(into);
							inf_writeln!(into, "jay_stack_ptr -= 1;");
						}

						Val::BoolConst(name)
					},
					Minus => {
						let val = self.compile_expr(right, into);
						let name = self.tmp_name();
						self.num_fence_for("jay_fence_number", &val, 1, into);

						self.indent(into);
						// Note the negation sign
						inf_write!(into, "const double {name} = -");

						self.compile_val_as(&val, 1, Ty::Number, into);
						inf_writeln!(into, ";");

						if let Val::OnStack = val {
							self.indent(into);
							inf_writeln!(into, "jay_stack_ptr -= 1;");
						}

						Val::DoubleConst(name)
					},
					_ => unreachable!(),
				}
			},
			Expr::Variable { identity, .. } => {
				Val::Variable(*identity)
			},
			Expr::This { identity, .. } => {
				Val::This(*identity)
			}
			Expr::Assign { value, identity, .. } => {
				self.compile_assign(into, value, identity)
			},
		}
	}

	fn compile_var(&mut self, var: VarRef, into: &mut String) {
		match self.lox.get_var_type(var) {
			crate::VarType::Local => {
				inf_write!(into, "locals.at[{}]", self.lox.get_var_mut(var).index)
			},
			crate::VarType::Parameter => {
				inf_write!(into, "args[{}]", self.lox.get_var_mut(var).index)
			},
			crate::VarType::Global => {
				inf_write!(into, "globals[{}]", self.lox.get_var_mut(var).index)
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
					inf_write!(into, "scope->values[{index}]");
				}
				else {
					// The hops amount is 'depth' if our scope == closure. This is because
					// hopping to closure would usually hop us up one, but because we didn't
					// create a scope, it instead hopped us up 0.
					//
					// If captures does hop us up 1, then to get to depth only requires depth - 1
					// more steps.
					let hops = if self.has_captures_frame { depth - 1 } else { depth };
					inf_write!(into, "closure->");
					// Walk up correct number of parents.
					for _ in 0..hops {
						inf_write!(into, "parent->");
					}
					// Finally, access the values array.
					inf_write!(into, "values[{index}]");
				}

				
			},
			
		}
	}

	fn make_if(&mut self, cond: Val, invert: bool, into: &mut String) {
		self.indent(into);
		let invert = if invert { "!" } else { "" };
		match &cond {
			Val::BoolConst(name) => {
				inf_writeln!(into, "if({invert}{name}) {{");
			},
			Val::Literal(LoxValue::Bool(false) | LoxValue::Nil) => {
				inf_writeln!(into, "if({invert}false) {{");
			},
			Val::Literal(_) => {
				inf_writeln!(into, "if({invert}true) {{");
			}
			Val::Variable(_) | Val::DoubleConst(_) => {
				inf_write!(into, "if({invert}jay_truthy(");
				// Note: We aren't using the stack, so this is fine.
				self.compile_val(&cond, 0, into);
				inf_writeln!(into, ")) {{");
			}
			_ => {
				// Note: We have to use braces due to the fact that expressions can be multi-line.
				// Anything that we can't directly stick into the if must
				// be stackified then popped from the stack.
				self.stackify(&cond, into);
				inf_writeln!(into, "if({invert}jay_pop_condition()) {{");
			}
		}
	}

	fn compile_locals_frame(&mut self, count: u32, scope: &str, into: &mut String) {
		inf_writeln!(into, r#"	struct {{
		size_t count;
		jay_closure *gc_scope;
		jay_value at[{0}];
	}} locals;
	locals.count = {0};
	locals.gc_scope = {1};"#, count, scope);
	
		// In order to avoid giving the GC any bogus data, initialize all the
		// ".at" values to nil.

		for i in 0..count {
			inf_writeln!(into, "\tlocals.at[{i}] = jay_box_nil();");
		}

		// Finally, push the frame so the GC can see it.
		inf_writeln!(into, "\tjay_push_frame(&locals);");
	}

	fn compile_function(&mut self, fun: &Function, mangled_name: &String, is_method: bool) {
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
		inf_writeln!(self.prelude, "jay_value {mangled_name}(jay_value *arguments, jay_closure *closure);");

		// Start writing the function definition
		inf_writeln!(def, "jay_value\n{}(jay_value *args, jay_closure *closure) {{", mangled_name);
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
			inf_writeln!(def, "\tjay_closure *scope = jay_new_scope(closure, {});\n", fun.captured.len());

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
						inf_writeln!(def, "\tscope->values[{}] = args[{}];\n", self.lox.get_var_mut(*v).index, param_idx);
					},
					_ => { /* No special treatment needed */ }
				}
			}

			// Captured funs need to store a reference to their scope in case
			// it's the only one.
			gc_scope = "scope";
		}
		else {
			inf_writeln!(def, "\tjay_closure *scope = closure;\n");
		}

		// Create the 'locals' struct.
		if self.has_locals_frame {
			self.compile_locals_frame(fun.local_count, gc_scope, &mut def);
		}

		if let Some(this_var) = fun.this {
			// Create a "this" variable for faster get/set operations
			inf_write!(def, "\tjay_instance *const this = JAY_AS_INSTANCE(");
			self.compile_var(this_var, &mut def);
			inf_writeln!(def, ");");
		}
		
		// Generate the code inside the function.
		self.compile_stmts(&fun.body, &mut def);

		// Finally, put a default return value.
		if self.has_locals_frame {
			inf_writeln!(def, "\tjay_pop_frame();");
		}
		if fun.is_initializer() {
			// For initializers, we can directly figure out the 'this' value here,
			// so we might as well directly return it.
			// this = param_count - 1, i.e. the last index in the param array.
			// Note that it's impossible for 'this' to become a closure param.
			// But, if we WANTED to support that case, we could simply modify
			// the parser to always add 'return this;' to the end of a function.
			inf_writeln!(def, "\treturn args[{}];", fun.param_count - 1);
		}
		else {
			inf_writeln!(def, "\treturn jay_box_nil();");
		}

		// End the function.
		self.pop_indent();
		inf_writeln!(def, "}}");

		self.function_defs.push(def);

		self.current_indent = enclosing_indent;

		// Restore locals and captures and initializer
		self.has_locals_frame = enclosing_locals;
		self.has_captures_frame = enclosing_captures;
	}

	fn compile_class_dispatcher(&mut self, class: &Class, mangled_name: &String) {
		let mut def = String::new();

		// Add the dispatcher forward-declaration
		// TODO: Consider having multiple blocks of forward-declares for organization / clarity
		inf_writeln!(self.prelude, "jay_method* {mangled_name}(jay_class *class, jay_name name);");
		
		inf_writeln!(def, "jay_method*\n{mangled_name}(jay_class *class, jay_name name) {{");

		// First, generate a switch-case for each name, looking up the associated function. But,
		// we actually do this in reverse, iterating through 0..n, then adding the associated
		// name to the switch-case.
		//
		// TODO: As an optimization, simply leave out any names that aren't referenced elsewhere.
		// There's no actual reason to do add_name here except for convenience.

		inf_writeln!(def, "\tswitch(name) {{");

		for (idx, method) in class.methods.iter().enumerate() {
			self.add_name(&method.name, false);
			inf_writeln!(def, "\t\tcase NAME_{}: return &class->methods[{}];", method.name.lexeme, idx);
		}

		inf_writeln!(def, "\t}}\n");

		// If the switch-case didn't look it up, then look it up in the superclass.
		inf_writeln!(def, "\tif(class->superclass) {{");
		inf_writeln!(def, "\t\treturn class->superclass->dispatcher(class->superclass, name);");
		inf_writeln!(def, "\t}}\n");

		// If we still didn't find it, return NULL.
		// In this case, we are probably inside a get expression, and then we will
		// look for a field. That will then return an error--and so we don't want
		// to return an error here.

		inf_writeln!(def, "\treturn NULL;");

		inf_writeln!(def, "}}");

		self.function_defs.push(def);
	}

	fn compile_class(&mut self, class: &Class, mangled_name: &String) {
		// To create the class-defining function, we need the dispatcher.
		let dispatcher_mangled = self.mangle(
			format!("jdisp_{}", class.name.lexeme));
		self.compile_class_dispatcher(class, &dispatcher_mangled);

		let mut def = String::new();

		// Add the mangled name to the function definition list
		inf_writeln!(self.prelude, "jay_value {mangled_name}(jay_value superclass, jay_closure *closure);");

		// Reset indent for top-level functions
		let enclosing_indent = self.current_indent;
		self.current_indent = 0;

		// Start writing the function definition
		inf_writeln!(def, "jay_value\n{}(jay_value superclass, jay_closure *closure) {{", mangled_name);

		// Save values for GC
		inf_writeln!(def, "\tjay_harbor(closure);\n\tjay_push(superclass);");

		// First step: Allocate the actual class object
		inf_writeln!(def, "\tjay_class *class = jay_gc_alloc(sizeof(*class) + (sizeof(jay_method) * {}), JAY_GC_CLASS);",
			class.methods.len());

		// Fill out the method table
		// TODO: If we want to be able to have init in a jay_bound_method, I geuss
		// it will also need to be part of the method table..?
		for (idx, method) in class.methods.iter().enumerate() {
			let method_mangled_name = self.mangle(
				format!("jm_{}_{}", class.name.lexeme, method.name.lexeme));
			
			// The C function should be the same, due to the 'this' variable being
			// automatically added at the end.
			// Methods are compiled separately from functions so they get a this.
			self.compile_function(method, &method_mangled_name, true);

			// Methods do not store the closure, because the have a pointer back
			// to the class.
			inf_writeln!(def, "\tclass->methods[{idx}] = jay_method_from(class, {method_mangled_name}, {});",
				method.param_count);
		}

		// The dispatcher is simply initialized to be the dispatcher function we
		// created. This function is dynamic in terms of superclass, and so
		// it is fine (if slower than it could be).
		//
		// TODO: Note that one possible optimization would be to figure out cases
		// where a class cannot have a superclass, and implement a faster dispatcher
		// for it. But, the extra checks for doing that might make it slower than
		// just always checking for the superclass...
		inf_writeln!(def, "\tclass->dispatcher = &{dispatcher_mangled};");

		// The class tracks the closure for all methods.
		inf_writeln!(def, "\tclass->closure = jay_unharbor();");

		inf_writeln!(def, "\tclass->methods_count = {};\n", class.methods.len());

		// Fill in the superclass
		// If it is nil, then the superclass is NULL, otherwise, it must be
		// a jay_class
		inf_writeln!(def, "\tsuperclass = jay_pop();");
		inf_writeln!(def, "\tif(JAY_IS_NIL(superclass)) {{");
		inf_writeln!(def, "\t\tclass->superclass = NULL;");
		inf_writeln!(def, "\t}}\n\telse if(JAY_IS_CLASS(superclass)) {{");
		inf_writeln!(def, "\t\tclass->superclass = JAY_AS_CLASS(superclass);");
		inf_writeln!(def, "\t}}\n\telse {{");
		inf_writeln!(def, "\t\toops(\"superclass must be class\");");
		inf_writeln!(def, "\t}}");

		inf_writeln!(def, "\treturn jay_box_class(class);\n");

		inf_writeln!(def, "}}");

		self.current_indent = enclosing_indent;

		self.function_defs.push(def);
	}

	fn compile_stmt(&mut self, stmt: &Stmt, into: &mut String) {
		match stmt {
			Stmt::Block(stmts) => {
				self.indent(into);
				into.push_str("{\n");
				self.push_indent();
				self.compile_stmts(stmts, into);
				self.pop_indent();
				self.indent(into);
				into.push_str("}\n");
			},
			Stmt::Class(class) => {
				let mangled_name = self.mangle(format!("jclass_{}", class.name.lexeme));

				self.compile_class(class, &mangled_name);

				self.indent(into);
				self.compile_var(class.identity, into);
				// The class is a function that takes a superclass and a scope,
				// and creates a class accordingly.
				// As such, the associated local variable is initialized by
				// calling this function.
				inf_write!(into, " = {mangled_name}(/* superclass = */ ");

				// Compile the superclass variable/null
				if let Some(superclass) = class.superclass {
					self.compile_var(superclass, into);
				}
				else {
					inf_write!(into, "jay_box_nil()");
				}

				inf_writeln!(into, ", scope);");
			},
			Stmt::Expression(expr) => {
				let val = self.compile_expr(expr, into);
				match val {
					Val::OnStack => {
						// After the expression is done, pop the unused value.
						// Any expression that does not needlessly push a value
						// will not return Val::OnStack.
						self.indent(into);
						into.push_str("jay_stack_ptr -= 1;\n");
					},
					_ => { }
				}
			},
			Stmt::Function(fun) => {
				let mangled_name = self.mangle(format!("jf_{}", fun.name.lexeme));
				// Functions are not methods and do not have 'this'.
				self.compile_function(fun, &mangled_name, false);

				// In the outer scope, we need to have a reference to this function
				// in our closure. And, that function's closure is our 'scope.' So,
				// create a new value.
				//
				// Because we create this value in the compiled code at the same place
				// that the function is defined in the AST, it should have the same
				// ordering properties (e.g. not being visible beforehand).
			
				self.indent(into);
				// Essentially, generate an assignment with the var.
				self.compile_var(fun.identity.expect("non-methods must have an identity"), into);
				inf_writeln!(into, " = jay_fun_from({}, {}, scope);",
					mangled_name, fun.param_count);
			},
			// TODO: Do we even need to have a var_name field on ExternFunction..?
			Stmt::ExternFunction { c_name, arity, identity, .. } => {
				// Like a function, except we don't need to compile anything -- just
				// insert the known name into the current scope.
				self.indent(into);
				self.compile_var(*identity, into);
				inf_writeln!(into, " = jay_fun_from({c_name}, {arity}, scope);");
			},
			Stmt::If { condition, then_branch, else_branch } => {
				let cond = self.compile_expr(condition, into);
				self.make_if(cond, false, into);

				self.push_indent();
				self.compile_stmt(then_branch, into);
				self.pop_indent();

				self.indent(into);
				into.push_str("}\n");
				if let Some(else_branch) = else_branch {
					// Note: This should be OK because we should always add a '\n' after statements anyways...
					self.indent(into);
					into.push_str("else {\n");

					self.push_indent();
					self.compile_stmt(&else_branch, into);
					self.pop_indent();

					self.indent(into);
					into.push_str("}\n");
				}
			},
			Stmt::Print(expr) => {
				self.compile_expr_tostack(expr, into);
				self.indent(into);
				into.push_str("jay_op_print();\n");
			},
			Stmt::Return { value, .. } => {
				let val = match value {
					Some(value) => { self.compile_expr(value, into) },
					None => {
						Val::Literal(LoxValue::Nil)
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
				match &val {
					Val::OnStack => {
						into.push_str("return jay_pop();\n");
					},
					_ => {
						inf_write!(into, "return ");
						self.compile_val(&val, 0, into);
						inf_writeln!(into, ";");
					}
				}
			},
			Stmt::Var { initializer, identity, .. } => {
				// The only real role this statement plays, given that the variables are
				// all generally "initialized" already, is explicitly initializing them
				// to jay_box_nil().
				//
				// Note that this could be perhaps 'optimized' by simply getting rid of it...
				// But there's likely little/no value to that.
				//
				// TODO: Actually, we should initialize all variables to nil for the GC's
				// sake...

				match initializer {
					Some(initializer) => {
						// If we have an initializer, re-use the compile_assign
						// logic to compile it "inline".
						self.compile_assign(into, initializer, identity);
					},
					None => {
						// Otherwise, just compile the variable = jay_box_nil().
						self.indent(into);
						self.compile_var(*identity, into);
						inf_write!(into, " = jay_box_nil();\n");
					}
				}
			},
			Stmt::While { condition, body } => {
				self.indent(into);
				into.push_str("for(;;) {\n");

				self.push_indent();

				let condition = self.compile_expr(condition, into);

				self.make_if(condition, true, into);
				self.indent(into);
				inf_writeln!(into, "\tbreak;");
				self.indent(into);
				inf_writeln!(into, "}}");

				self.compile_stmt(body, into);

				self.pop_indent();
				self.indent(into);
				into.push_str("}\n");
			},
		}
	}

	fn compile_stmts(&mut self, stmts: &Vec<Stmt>, into: &mut String) {
		for stmt in stmts {
			self.compile_stmt(stmt, into);
		}
	}

	fn compile_to_buffers(&mut self, stmts: &Vec<Stmt>, globals_count: u32, globals_locals_count: u32) -> Result<String, fmt::Error> {
		inf_writeln!(self.prelude, "/* --- jaylib configuration --- */\n"); 
		if self.opt.gc_stress_test {
			inf_writeln!(self.prelude, "#define JAY_GC_STRESS_TEST");
		}
		if self.opt.nan_boxing {
			inf_writeln!(self.prelude, "#define JAY_NAN_BOXING");
		}
		if self.opt.assume_correct {
			inf_writeln!(self.prelude, "#define JAY_ASSUME_CORRECT");
		}
		inf_writeln!(self.prelude, "#include \"jaylib/jaylib.h\"\n");

		// Write the globals array to the prelude (and the string constants array)
		let string_constants_count = self.lox.string_constants.len();
		if globals_count > 0 { 
			inf_writeln!(self.prelude, "static jay_value globals[{globals_count}];");
		}
		if string_constants_count > 0 {
			inf_writeln!(self.prelude, "static jay_value global_string_constants[{}];\n", string_constants_count);
		}
		// Create the global-visit function
		inf_writeln!(self.prelude, "static\nvoid\njay_gc_visit_globals(void) {{");
		if globals_count > 0 {
			inf_writeln!(self.prelude, "\tfor(size_t i = 0; i < {globals_count}; ++i) {{");
			inf_writeln!(self.prelude, "\t\tjay_gc_visit(&globals[i]);");
			inf_writeln!(self.prelude, "\t}}");
		}
		// For now, we also have to copy the string constants over, although this should change
		// later (make them immortal)
		if string_constants_count > 0 {
			inf_writeln!(self.prelude, "\tfor(size_t i = 0; i < {string_constants_count}; ++i) {{", );
			inf_writeln!(self.prelude, "\t\tjay_gc_visit(&global_string_constants[i]);");
			inf_writeln!(self.prelude, "\t}}");
		}
		inf_writeln!(self.prelude, "}}\n");

		let mut main_fn = String::new();

		// TODO: Consider creating an efficient string writing system
		// In particular, each function being compiled will need its own string...
		self.push_indent();

		// The most important responsibility of main() is initializing the gc and stack
		if self.opt.gc_stress_test {
			inf_writeln!(main_fn, "\tjay_gc_init(512); // STRESS_TEST: small initial heap");
		}
		else {
			inf_writeln!(main_fn, "\tjay_gc_init(16 * 1024 * 1024); // 16 megabytes");
		}
		inf_writeln!(main_fn, "\tjay_stack_ptr = jay_stack;");
		inf_writeln!(main_fn, "\tjay_frames_ptr = 0;\n");

		// We have to set up all the string constants in main for the rest of the
		// code to use.
		for (idx, constant) in self.lox.string_constants.iter().enumerate() {
			inf_writeln!(main_fn, "\tglobal_string_constants[{idx}] = jay_box_string(jay_string_from_literal(\"{constant}\"));");
		}

		// Note: Built-in functions will also be set up in main, but this requires
		// parser support..?

		self.indent(&mut main_fn);
		// Create the scope for the main fn
		inf_writeln!(main_fn, "jay_closure *scope = NULL;");

		// Create the locals frame for the main fn
		self.compile_locals_frame(globals_locals_count, "NULL", &mut main_fn);

		// Compile the actual top-level code (any normal statements will go
		// into main; other things will go into their own functions)
		self.compile_stmts(stmts, &mut main_fn);
		self.pop_indent();

		Ok(main_fn)
	}

	pub fn compile(&mut self, stmts: &Vec<Stmt>, globals_count: u32, globals_locals_count: u32) -> std::io::Result<()> {
		// We use inf_write! a lot and so get fmt::Result, but most of the compilation
		// process should not fail (unless we OOM or something). By contrast, it
		// is very possible that, while writing out to the self.writer, there is
		// an error (e.g. read-only file target).
		//
		// So, wrap all of the compiling-to-buffers in its own function, and assume
		// that it almost certainly won't error. Then, the error from writing to
		// self.writer can be returned to the caller.
		let main_fn = match self.compile_to_buffers(stmts, globals_count, globals_locals_count) {
			Ok(main_fn) => main_fn,
			Err(err) => {
				panic!("Compiler error while writing to buffers: {}", err);
			}
		};

		writeln!(self.writer, "/*** This C file created by jaylox https://github.com/HoneyPony/jaylox ***/")?;

		// First, NAME_ definitions

		writeln!(self.writer, "\n/* --- NAME Definitions --- */\n")?;

		// Note that 0 is the TOMBSTONE so we cannot use it for a NAME
		let mut name_value: usize = 1;
		// We start by creating all names for things that might be fields (i.e.
		// things that have been set.) Then we generate JAY_MAX_FIELD based
		// on that set. Finally we generate the rest of the names.
		for name in &self.name_set_setted {
			writeln!(self.writer, "#define NAME_{name} ((jay_name){name_value})")?;
			name_value += 1;

			self.name_set.remove(name);
		}

		// Note: We will use < for the field comparison.
		let jay_max_field = name_value;

		// We must always define NAME_init for the op_call implementation, even if
		// that name is not actually used.
		self.name_set.insert(String::from("init"));

		for name in &self.name_set {
			writeln!(self.writer, "#define NAME_{name} ((jay_name){name_value})")?;
			name_value += 1;
		}

		writeln!(self.writer, "#define JAY_MAX_FIELD {jay_max_field}")?;

		// First: prelude, containing all function declarations
		write!(self.writer, "{}", self.prelude)?;

		
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