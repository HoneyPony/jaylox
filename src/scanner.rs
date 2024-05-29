use std::str::Chars;

use crate::Lox;

#[derive(PartialEq, Eq, Debug)]
pub enum TokenType {
	LeftParen, RightParen, LeftBrace, RightBrace,
	Comma, Dot, Minus, Plus, Semicolon, Slash, Star,

	Bang, BangEqual,
	Equal, EqualEqual,
	Greater, GreaterEqual,
	Less, LessEqual,

	Idenitifer, String, Number,

	And, Class, Else, False, Fun, For, If, Nil, Or,
	Print, Return, Super, This, True, Var, While,

	Eof
}

pub enum TokenLiteral {
	None,
	String(String),
	Number(f64)
}

impl ToString for TokenLiteral {
	fn to_string(&self) -> String {
		match self {
			TokenLiteral::None => "None".to_string(),
			TokenLiteral::String(what) => what.clone(),
			TokenLiteral::Number(num) => num.to_string(),
		}
	}
}

pub struct Token {
	typ: TokenType,
	lexeme: String,
	literal: TokenLiteral,
	line: i32
}

impl Token {
	pub fn new(typ: TokenType, lexeme: String, literal: TokenLiteral, line: i32) -> Self {
		Token { typ, lexeme, literal, line }
	}
}

impl ToString for Token {
	fn to_string(&self) -> String {
		return format!("{0:?} {1} {2}", self.typ, self.lexeme, self.literal.to_string());
	}
}

pub struct Scanner<'a> {
	source: String,
	chars: Vec<char>,
	start: i32,
	current: i32,
	line: i32,

	lox: &'a mut Lox
}

impl<'a> Scanner<'a> {
	pub fn new(source: String, lox: &'a mut Lox) -> Scanner {
		let chars = source.chars().collect();

		Scanner {
			source,
			chars,
			start: 0,
			current: 0,
			line: 1,

			lox
		}
	}

	fn is_at_end(&self) -> bool {
		return self.current >= self.source.len() as i32
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
		self.add_token_lit(tokens, typ, TokenLiteral::None);
	}

	fn add_token_lit(&mut self, tokens: &mut Vec<Token>, typ: TokenType, lit: TokenLiteral) {
		let text = &self.chars[self.start as usize..self.current as usize];
		tokens.push(Token::new(typ, String::from_iter(text), lit, self.line));
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

			_ => {
				self.lox.error(self.line, &format!("Unexpected character '{}'", c));
			}
		}
	}

	pub fn scan_tokens(&mut self) -> Vec<Token> {
		let mut tokens = vec![];

		while(!self.is_at_end()) {
			self.start = self.current;
			self.scan_token(&mut tokens);
		}

		tokens.push(Token::new(TokenType::Eof, "".to_string(), TokenLiteral::None, self.line));

		return tokens;
	}
}