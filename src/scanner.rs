use std::collections::HashMap;

use crate::{expr::Expr, Lox, StrConstRef};

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum TokenType {
	LeftParen, RightParen, LeftBrace, RightBrace,
	Comma, Dot, Minus, Plus, Semicolon, Slash, Star,

	Bang, BangEqual,
	Equal, EqualEqual,
	Greater, GreaterEqual,
	Less, LessEqual,

	Identifier, StringTok, Number,

	And, Class, Else, False, Fun, For, If, Nil, Or,
	Print, Return, Super, This, True, Var, While,

	Eof
}

#[derive(Clone, PartialEq)]
pub enum LoxValue {
	Nil,
	String(StrConstRef),
	Number(f64),
	Bool(bool),
}

impl LoxValue {
	// Note: This MUST match the C implementation, otherwise constant folding
	// can change the program behavior.
	pub fn is_truthy(&self) -> bool {
		match self {
			Self::Nil | Self::Bool(false) => false,
			_ => true
		}
	}
}

impl Into<Expr> for LoxValue {
	fn into(self) -> Expr {
		return Expr::literal(self);
	}
}

#[derive(Clone)]
pub struct Token {
	pub typ: TokenType,
	pub lexeme: String,
	pub literal: LoxValue,
	pub line: i32
}

impl Token {
	pub fn new(typ: TokenType, lexeme: String, literal: LoxValue, line: i32) -> Self {
		Token { typ, lexeme, literal, line }
	}
}

pub struct Scanner<'a> {
	source: String,
	chars: Vec<char>,
	start: i32,
	current: i32,
	line: i32,

	lox: &'a mut Lox,

	keyword_hash: HashMap<&'static str, TokenType>,
}

fn is_digit(c: char) -> bool {
	return c >= '0' && c <= '9';
}

fn is_alpha(c: char) -> bool {
	return (c >= 'a' && c <= 'z') ||
		(c >= 'A' && c <= 'Z') ||
		c == '_';
}

fn is_alnum(c: char) -> bool {
	return is_digit(c) || is_alpha(c);
}

impl<'a> Scanner<'a> {
	pub fn new(source: String, lox: &'a mut Lox) -> Scanner {
		let chars = source.chars().collect();

		let keyword_hash: HashMap<&'static str, TokenType> = HashMap::from([
			("and",    TokenType::And),
			("class",  TokenType::Class),
			("else",   TokenType::Else),
			("false",  TokenType::False),
			("for",    TokenType::For),
			("fun",    TokenType::Fun),
			("if",     TokenType::If),
			("nil",    TokenType::Nil),
			("or",     TokenType::Or),
			("print",  TokenType::Print),
			("return", TokenType::Return),
			("super",  TokenType::Super),
			("this",   TokenType::This),
			("true",   TokenType::True),
			("var",    TokenType::Var),
			("while",  TokenType::While),
		]);

		Scanner {
			source,
			chars,
			start: 0,
			current: 0,
			line: 1,

			lox,

			keyword_hash,
		}
	}

	fn is_at_end(&self) -> bool {
		return self.current >= self.chars.len() as i32
	}

	fn advance(&mut self) -> char {
		let result: char = self.chars[self.current as usize];
		self.current += 1;
		result
	}

	fn peek(&mut self) -> char {
		if self.is_at_end() { return '\0'; }
		return self.chars[self.current as usize];
	}

	fn peek_next(&mut self) -> char {
		if (self.current + 1) as usize >= self.chars.len() { return '\0' }
		return self.chars[(self.current + 1) as usize];
	}

	fn match_char(&mut self, c: char) -> bool {
		if self.peek() == c {
			self.advance();
			return true;
		}

		false
	}

	fn add_token_lookahead(&mut self, tokens: &mut Vec<Token>, next_char: char, type_both: TokenType, type_first: TokenType) {
		if self.match_char(next_char) {
			self.add_token(tokens, type_both);
		}
		else {
			self.add_token(tokens, type_first);
		}
	}

	fn add_token(&mut self, tokens: &mut Vec<Token>, typ: TokenType) {
		self.add_token_lit(tokens, typ, LoxValue::Nil);
	}

	fn add_token_lit(&mut self, tokens: &mut Vec<Token>, typ: TokenType, lit: LoxValue) {
		let text = &self.chars[self.start as usize..self.current as usize];
		tokens.push(Token::new(typ, String::from_iter(text), lit, self.line));
	}

	fn string(&mut self, tokens: &mut Vec<Token>) {
		while self.peek() != '"' && !self.is_at_end() {
			if self.peek() == '\n' { self.line += 1; }
			self.advance();
		}

		if self.is_at_end() {
			self.lox.error(self.line, "Unterminated string.");
			return;
		}

		// Consume closing '"'.
		self.advance();

		// Trim surrounding quotes.
		let trimmed = &self.chars[(self.start + 1) as usize..(self.current - 1) as usize];
		let trimmed = String::from_iter(trimmed);

		let string = self.lox.put_string_constant(trimmed);

		self.add_token_lit(tokens, TokenType::StringTok, LoxValue::String(string));
	}

	fn number(&mut self, tokens: &mut Vec<Token>) {
		while is_digit(self.peek()) { self.advance(); }

		if self.peek() == '.' && is_digit(self.peek_next()) {
			self.advance(); /* Consume decimal */

			while is_digit(self.peek()) { self.advance(); }
		}

		let text = &self.chars[self.start as usize..self.current as usize];
		let text = String::from_iter(text);

		/* This should never fail because we're only lexing numbers. */
		let value: f64 = text.parse().unwrap();

		self.add_token_lit(tokens, TokenType::Number, LoxValue::Number(value));
	}

	fn identifier(&mut self, tokens: &mut Vec<Token>) {
		while is_alnum(self.peek()) { self.advance(); }

		let text = &self.chars[self.start as usize..self.current as usize];
		let text = String::from_iter(text);

		if let Some(typ) = self.keyword_hash.get(&text as &str) {
			self.add_token(tokens, *typ);
			return;
		}

		self.add_token(tokens, TokenType::Identifier);
	}

	fn scan_token(&mut self, tokens: &mut Vec<Token>) {
		let c = self.advance();
		match c {
			'(' => { self.add_token(tokens, TokenType::LeftParen); }
			')' => { self.add_token(tokens, TokenType::RightParen); }
			'{' => { self.add_token(tokens, TokenType::LeftBrace); }
			'}' => { self.add_token(tokens, TokenType::RightBrace); }
			',' => { self.add_token(tokens, TokenType::Comma); }
			'.' => { self.add_token(tokens, TokenType::Dot); }
			'-' => { self.add_token(tokens, TokenType::Minus); }
			'+' => { self.add_token(tokens, TokenType::Plus); }
			';' => { self.add_token(tokens, TokenType::Semicolon); }
			'*' => { self.add_token(tokens, TokenType::Star); }

			'!' => {
				self.add_token_lookahead(tokens, '=', 
					TokenType::BangEqual, 
					TokenType::Bang);
			}

			'=' => {
				self.add_token_lookahead(tokens, '=', 
					TokenType::EqualEqual, 
					TokenType::Equal);
			}

			'<' => {
				self.add_token_lookahead(tokens, '=', 
					TokenType::LessEqual, 
					TokenType::Less);
			}

			'>' => {
				self.add_token_lookahead(tokens, '=', 
					TokenType::GreaterEqual, 
					TokenType::Greater);
			}

			'/' => {
				if self.match_char('/') {
					while self.peek() != '\n' && !self.is_at_end() { self.advance(); }
				}
				else {
					self.add_token(tokens, TokenType::Slash)
				}
			}

			'\t' | '\r' | ' ' => { /* Ignore whitespace */ }

			'\n' => {
				/* Ignore whitespace, but increment line number */
				self.line += 1;
			}

			'"' => { self.string(tokens); }

			_ => {
				if is_digit(c) {
					self.number(tokens);
				}
				else if is_alpha(c) {
					self.identifier(tokens);
				}
				else {
					self.lox.error(self.line, &format!("Unexpected character."));
				}
			}
		}
	}

	pub fn scan_tokens(&mut self) -> Vec<Token> {
		let mut tokens = vec![];

		while !self.is_at_end() {
			self.start = self.current;
			self.scan_token(&mut tokens);
		}

		tokens.push(Token::new(TokenType::Eof, "".to_string(), LoxValue::Nil, self.line));

		return tokens;
	}
}