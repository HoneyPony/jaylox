// This file is generated by build.rs
include!(concat!(env!("OUT_DIR"), "/stmt.gen.rs"));

use std::collections::HashSet;

use crate::expr::ExprErr;

pub type StmtRes = Result<Stmt, ExprErr>;

// Function info: lots of stuff, so carry it around Rc'd.
pub struct Function {
	pub name: Token,

	// Functions have an identity; methods do not.
	pub identity: Option<VarRef>,
	pub vars: HashSet<VarRef>,
	pub param_count: u32,
	pub local_count: u32,

	pub captured: Vec<VarRef>,

	pub body: Vec<Stmt>,
}

impl Function {
	pub fn empty_init() -> Function {
		return Function {
			name: Token::new(TokenType::Identifier, "init".into(), LoxValue::Nil, 0),

			identity: None, // This is a method which doesn't have identity
			vars: HashSet::new(),
			param_count: 1, // Any method needs at least 1 param, for 'this'
			local_count: 0,

			captured: vec![],

			body: vec![],
		};
	}

	pub fn is_method(&self) -> bool {
		// Hack: For now, whether or not we have an identity corresponds to whether
		// we are a method. Methods don't have identity.
		return self.identity.is_none()
	}

	pub fn is_initializer(&self) -> bool {
		// Methods named 'init' are initializers.
		return self.is_method() && self.name.lexeme == "init";
	}
}

pub struct Class {
	pub name: Token,
	pub methods: Vec<Function>,
	pub superclass: Option<VarRef>,
	pub identity: VarRef,
}