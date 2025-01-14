mod scanner;
mod expr;
mod stmt;
mod parser;
//mod resolver;
mod compiler;
mod run_libtcc;

use std::collections::HashMap;
use std::{env, fs::File};
use std::process::{exit, Command, Stdio};
use std::io;

use compiler::Compiler;
//use resolver::Resolver;
use scanner::{Scanner, Token, TokenType};
use parser::Parser;

#[derive(Clone, Copy, PartialEq)]
pub enum VarType {
	Local,
	Parameter,
	Captured,
	// A function parameter that becomes captured by a closure. Requires somewhat
	// special treatment. Tracks its original idx in u32.
	CapturedParameter(u32),
	Global,
	/// A special kind of variable that is only used for full conformance mode. The
	/// idea is that, when Lox would expect a runtime error due to an undefined variable,
	/// we usually catch that at compile time--but instead we can simply generate a
	/// C code to directly generate a runtime error if that code path is ever executed.
	/// 
	/// This means that in conformance mode, something like test/variable/unreached_undefined.lox
	/// will actually pass instead of giving a compile error, as the generated code will
	/// simply have an unreached "oops" call.
	Undefined(&'static str),
}

pub struct Variable {
	index: u32,
	typ: VarType,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct VarRef(usize);

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct StrConstRef(usize);

impl StrConstRef {
	pub fn to_number(self) -> usize {
		return self.0;
	}
}

pub struct Lox {
	had_error: bool,

	full_conformance: bool,

	variables: Vec<Variable>,

	pub string_constants: Vec<String>,
	string_constant_map: HashMap<String, StrConstRef>,
}

enum CompileOutput {
	Executable { path: String },
	CFile { path: String },
	StandardOut,
	LibTcc,
}

#[derive(Clone)]
pub struct CodegenOptions {
	gc_stress_test: bool,
	nan_boxing: bool,
	assume_correct: bool,
	backtrace: bool,

	/// Option that prevents optimizations which would turn runtime errors into compile errors, namely
	/// constant folding.
	/// This option is not actually very useful outside of running the tester, but that's OK.
	full_conformance: bool,

	/// Determines whether classes, functions, etc store their names. Will also provide names for fields
	/// in the case of a field access error.
	enable_names: bool,

	/// If true, the compiler will generate '#include "jaylib/jaylib.h"' instead of just dumping the
	/// built-in jaylib (compiled into the Rust executable) into the file.
	extern_jaylib: bool,
}

struct CompileOptions {
	input_path: String,
	output: CompileOutput,
	optimization: String,

	codegen: CodegenOptions,
}

impl Lox {

	fn new() -> Self {
		Lox {
			had_error: false,

			full_conformance: false,

			variables: vec![],

			string_constants: vec![],
			string_constant_map: HashMap::new(),
		}
	}

	fn put_string_constant(&mut self, value: String) -> StrConstRef {
		if let Some(ptr) = self.string_constant_map.get(&value) {
			return *ptr;
		}

		self.string_constants.push(value.clone());
		let ptr = StrConstRef(self.string_constants.len() - 1);
		self.string_constant_map.insert(value, ptr);

		return ptr;
	}

	fn get_string_constant(&self, ptr: StrConstRef) -> &str {
		// Safety: we only ever hand out valid StrConstRefs. 
		unsafe { self.string_constants.get_unchecked(ptr.0) }
	}

	fn new_var(&mut self) -> VarRef {
		self.variables.push(Variable { index: 0, typ: VarType::Local });
		return VarRef(self.variables.len() - 1);
	}

	fn get_var_mut(&mut self, ptr: VarRef) -> &mut Variable {
		// Safety: we only ever hand out valid VarRefs.
		unsafe { self.variables.get_unchecked_mut(ptr.0) }
	}

	fn get_var_type(&self, ptr: VarRef) -> VarType {
		unsafe { self.variables.get_unchecked(ptr.0).typ }
	}

	fn report(&mut self, line: i32, where_: &str, message: &str) {
		eprintln!("[line {line}] Error{where_}: {message}");
		self.had_error = true;
	}

	// For errors where we aren't tracking any location info.
	fn error_general(&mut self, message: &str) {
		eprintln!("Error: {}", message);
		self.had_error = true;
	}

	fn error(&mut self, line: i32, message: &str) {
		self.report(line, "", message);
	}

	fn error_token(&mut self, token: &Token, message: &str) {
		if token.typ == TokenType::Eof {
			self.report(token.line, " at end", message);
		}
		else {
			self.report(token.line, &format!(" at '{0}'", token.lexeme), message);
		}
	}

	fn compile(&mut self, options: CompileOptions) -> std::io::Result<()> {
		self.full_conformance = options.codegen.full_conformance;
		let code = std::fs::read_to_string(options.input_path)?;

		let tokens = {
			let mut scanner = Scanner::new(code, self);
			scanner.scan_tokens()
		};

		let (program, globals_count, globals_locals_count) = {
			let mut parser = Parser::new(tokens, self);
			parser.parse()
		};

		// Don't resolve if we had an error
		if self.had_error { return Ok(()); }

		// TODO: Consider using Box<dyn Write> to make this code simpler, and
		// possibly speed up the compilation of the project a lot.. it's not clear
		// exactly how much Rust is going to be monomorphizing in Compiler, given
		// that only one function actually needs the Writer...

		match options.output {
			CompileOutput::Executable { path } => {
				let mut child = Command::new("gcc")
					.stdin(Stdio::piped())
					.arg("-x")
					.arg("c")
					.arg("-o")
					.arg(path)
					.arg(options.optimization)
					.arg("-")
					.spawn()?;

				let stdin = child.stdin.take()
					.expect("Could not spawn 'gcc'.");

				Compiler::new(self, stdin, options.codegen.clone())
					.compile(&program, globals_count, globals_locals_count)?;

				let ecode = child.wait()?;
				if !ecode.success() {
					eprintln!("jaylox: C compiler did not succeed.");
				}

				Ok(())
			},
			CompileOutput::CFile { path } => {
				let out_file = File::create(path)?;
				Compiler::new(self, out_file, options.codegen.clone())
				.compile(&program, globals_count, globals_locals_count)
			},
			CompileOutput::StandardOut => {
				Compiler::new(self, std::io::stdout(), options.codegen.clone())
					.compile(&program, globals_count, globals_locals_count)
			},
			CompileOutput::LibTcc => {
				let mut out_string = Vec::<u8>::new();
				Compiler::new(self, &mut out_string, options.codegen.clone())
					.compile(&program, globals_count, globals_locals_count)?;

				// Add NUL terminator so we can use the safer method.
				out_string.push(0);

				// Convert to C string.
				let out_string = std::ffi::CString::from_vec_with_nul(out_string)
					.expect("should have generated valid C string");

				run_libtcc::run_libtcc(&out_string);

				Ok(())
			},
		}
	}
}

fn main() -> io::Result<()> {
	let mut args: Vec<String> = env::args().collect();
	let mut lox = Lox::new();

	if args.len() < 2 {
		println!("Usage: jaylox [flags] script");
		exit(64);
	}

	// Possible flags:
	//    -o  : writes to a specific executable file
	//    -oc : writes to a specific C file
	//    -os : writes to standard output
	//    -O1 : compile with -O1
	//    -O2 : compile with -O2
	//    -O3 : compile with -O3

	// Input is always the last option (for now)
	let input_path = args.pop().unwrap();
	
	let mut default_output_path = input_path.clone();
	if default_output_path.ends_with(".lox") {
		for _ in 0..4 { default_output_path.pop(); }
	}

	let mut output_path_noslash = &default_output_path[..];

	// For the default output path, output in the working directory -- so
	// try to remove any leading slashes
	while let Some(slash) = output_path_noslash.find('/') {
		output_path_noslash = &output_path_noslash[slash + 1..];
	}

	let mut options = CompileOptions {
		input_path,
		output: CompileOutput::Executable { path: output_path_noslash.to_string() },
		optimization: "-O1".into(),

		codegen: CodegenOptions {
			gc_stress_test: false,
			nan_boxing: false,
			assume_correct: false,
			backtrace: false,
			full_conformance: false,
			enable_names: false,
			extern_jaylib: false,
		},
	};

	// Process remaining arguments
	let mut eat_cfile = false;
	let mut eat_exefile = false;
	for arg in &args[1..] {
		if eat_cfile {
			options.output = CompileOutput::CFile { path: arg.clone() };
			eat_cfile = false;
		}
		else if eat_exefile {
			options.output = CompileOutput::Executable { path: arg.clone() };
			eat_exefile = false;
		}
		else {
			match arg.as_str() {
				"-o" => { eat_exefile = true; },
				"-oc" => { eat_cfile = true; },
				"-os" => { options.output = CompileOutput::StandardOut; },
				"-run" => { options.output = CompileOutput::LibTcc; }
				"-gcstress" => {
					options.codegen.gc_stress_test = true;
				}
				"-nanbox" => {
					options.codegen.nan_boxing = true;
				},
				"-assumecorrect" => {
					options.codegen.assume_correct = true;
				}
				"-backtrace" => {
					options.codegen.backtrace = true;
				}
				"-conformance" => {
					options.codegen.full_conformance = true;
				}
				"-enablenames" => {
					options.codegen.enable_names = true;
				}
				"-externjaylib" => {
					options.codegen.extern_jaylib = true;
				}
				"-O1" | "-O2" | "-O3" => {
					options.optimization = arg.clone();
				},
				_ => {
					println!("Error: Unrecognized flag {}", arg);
					exit(64);
				}
			};
		}
	}

	lox.compile(options)?;
	if lox.had_error {
		exit(65);
	}

	exit(0);
}
