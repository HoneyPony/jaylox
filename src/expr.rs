// This file is generated by build.rs
include!(concat!(env!("OUT_DIR"), "/expr.gen.rs"));

pub struct ExprErr;

pub type ExprRes = Result<Expr, ExprErr>;

#[allow(unused)]
fn paren_begin(marker: &str) {
	print!("({}", marker);
}

#[allow(unused)]
fn paren_middle(expr: &Expr) {
	print!(" ");
	ast_print(expr);
}

#[allow(unused)]
fn paren_end(expr: &Expr) {
	print!(" ");
	ast_print(expr);
	print!(")");
}

#[allow(unused)]
pub fn ast_print(expr: &Expr) {
	match expr {
		Expr::Binary { left, operator, right } => {
			paren_begin(&operator.lexeme);
			paren_middle(left);
			paren_end(right);
		},
		Expr::Logical { left, operator, right } => {
			paren_begin(&operator.lexeme);
			paren_middle(left);
			paren_end(right);
		}
		Expr::Grouping(expr) => {
			paren_begin("group");
			paren_end(expr);
		},
		Expr::Literal(lit) => {
			print!("{}", lit.to_string());
		}
		Expr::Unary { operator, right } => {
			paren_begin(&operator.lexeme);
			paren_end(right);
		},
		Expr::Variable { name, resolved }=> {
			print!("{}", name.lexeme);
		},
		Expr::Assign { name, value, resolved } => {
			paren_begin("assign");
			print!(" {}", name.lexeme);
			paren_end(value);
		},
		Expr::Call { callee, paren, arguments } => {
			print!("(");
			ast_print(callee);
			for arg in arguments {
				paren_middle(arg);
			}
			print!(")");
		},
		Expr::Get { object, name } => {
			paren_begin("get");
			ast_print(object);
			print!(" {})", name.lexeme);
		},
		Expr::Set { object, name, value } => {
			paren_begin("set");
			ast_print(object);
			print!(" {}", name.lexeme);
			paren_end(value);
		},
	}
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ast_print() {
		let minus = Token::new(TokenType::Minus, "-".into(), LoxValue::Nil, 1);
		let star = Token::new(TokenType::Star, "*".into(), LoxValue::Nil, 1);

        let expr = Expr::binary(
			Expr::unary(minus, Expr::literal(LoxValue::Number(123.0))),
			star,
			Expr::grouping(Expr::literal(LoxValue::Number(45.67)))
		);

		ast_print(&expr);
    }
}