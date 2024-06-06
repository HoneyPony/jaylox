use crate::Lox;

pub struct Resolver<'a> {
	lox: &'a mut Lox
}

impl<'a> Resolver<'a> {
	pub fn new(lox: &'a mut Lox) -> Self {
		Resolver { lox }
	}

	
}