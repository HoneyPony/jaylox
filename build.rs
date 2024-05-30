use std::{env, fs::File, io::{self, Write}, path::Path};

struct ExprArg {
	name: String,
	ty: String
}

struct ExprTy {
	tyname: String,
	args: Vec<ExprArg>
}

fn ty(tyname: &str, args: Vec<ExprArg>) -> ExprTy {
	let tyname = tyname.to_string();
	return ExprTy { tyname, args };
}

fn arg(name: &str, ty: &str) -> ExprArg {
	let name = name.to_string();
	let mut ty = ty.to_string();
	if ty.eq("Expr") {
		ty = "Box<Expr>".to_string();
	}
	return ExprArg { name, ty };
}

fn generate_ty_enum(file: &mut File, ty: &ExprTy) -> io::Result<()> {
	if ty.args.len() > 1 {
		writeln!(file, "\t{0} {{", ty.tyname)?;
		for arg in &ty.args {
			writeln!(file, "\t\t{0}: {1},", arg.name, arg.ty)?;
		}
		writeln!(file, "\t}},")?;
	}
	else {
		writeln!(file, "\t{0}({1}),", ty.tyname, ty.args[0].ty)?;
	}

	Ok(())
}

fn generate_ty_impl(file: &mut File, ty: &ExprTy) -> io::Result<()> {
	write!(file, "\tpub fn {0}(", ty.tyname.to_ascii_lowercase())?;

	let mut comma = false;
	for arg in &ty.args {
		if comma { write!(file, ", ")?; }
		if arg.ty == "Box<Expr>" {
			write!(file, "{0}: {1}", arg.name, "Expr")?;
		}
		else {
			write!(file, "{0}: {1}", arg.name, arg.ty)?;
		}
		comma = true;
	}

	writeln!(file, ") -> Expr {{")?;

	if ty.args.len() > 1 {
		for arg in &ty.args {
			if arg.ty == "Box<Expr>" {
				writeln!(file, "\t\tlet {0} = Box::new({0});", arg.name)?;
			}
		}
	}
	else {
		if ty.args[0].ty == "Box<Expr>" {
			writeln!(file, "\t\tlet {0} = Box::new({0});", ty.args[0].name)?;
		}
	}

	write!(file, "\t\tExpr::{0} ", ty.tyname)?;

	if ty.args.len() > 1 {
		write!(file, "{{ ")?;
		for arg in &ty.args {
			write!(file, "{0}, ", arg.name)?;
		}
		writeln!(file, "}}")?;
	}
	else {
		writeln!(file, "({0})", ty.args[0].name)?;
	}

	writeln!(file, "\t}}")?;

	Ok(())
}

fn generate_expr_file(file: &mut File) -> io::Result<()> {
	let tys: Vec<ExprTy> = vec![
		ty("Binary", vec![arg("left", "Expr"), arg("operator", "Token"), arg("right", "Expr")]),
		ty("Grouping", vec![arg("expression", "Expr")]),
		ty("Literal", vec![arg("value", "TokenLiteral")]),
		ty("Unary", vec![arg("operator", "Token"), arg("right", "Expr")])
	];

	writeln!(file, "use crate::scanner::*;")?;
	writeln!(file, "")?;

	writeln!(file, "pub enum Expr {{")?;
	for ty in &tys {
		generate_ty_enum(file, ty)?;
	}
	writeln!(file, "}}")?;

	writeln!(file, "")?;

	writeln!(file, "impl Expr {{")?;
	for ty in &tys {
		generate_ty_impl(file, ty)?;
	}
	writeln!(file, "}}")?;

	Ok(())
}

fn main() {
	// Only rerun when we update build.rs
	println!("cargo::rerun-if-changed=build.rs");

	let out_dir = env::var("OUT_DIR").unwrap();
    let dest_path = Path::new(&out_dir).join("expr.gen.rs");
    let mut output_file = File::create(dest_path).unwrap();

	generate_expr_file(&mut output_file).unwrap();

	// Note: This setup is based in part off of
	// https://github.com/condekind/tokers/blob/main/src/world.rs
}