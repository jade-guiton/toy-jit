use std::rc::Rc;

use inlinable_string::InlinableString;

#[derive(Clone)]
pub struct Pos {
	pub file: Rc<str>,
	pub line: u32,
	pub off: u32,
}

impl std::fmt::Debug for Pos {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		f.write_fmt(format_args!("{}:{}:{}", self.file, self.line, self.off))
	}
}

pub enum Node {
	Fn {
		name: InlinableString,
		args: Vec<(InlinableString, Node)>,
		ret: Option<Box<Node>>,
		body: Vec<NodePos>,
	},
	IntType,
	Ret(Option<Box<NodePos>>),
	Call {
		func: Box<NodePos>,
		args: Vec<NodePos>,
	},
	Plus(Box<NodePos>, Box<NodePos>),
	Id(InlinableString),
	IntLit(i64),
	Sym(InlinableString),
	// Only used in lexer:
	Eof,
}

pub struct NodePos(pub Node, pub Pos);

pub struct CompilationError {
	msg: String,
	pos: Option<Pos>,
}

impl CompilationError {
	pub fn nowhere(msg: String) -> Self {
		CompilationError { msg, pos: None }
	}
	pub fn at(pos: Pos, msg: String) -> Self {
		CompilationError { msg, pos: Some(pos) }
	}
}

impl std::fmt::Display for CompilationError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		if let Some(pos) = &self.pos {
			f.write_fmt(format_args!("at {:?}: {}", pos, self.msg))
		} else {
			f.write_str(&self.msg)
		}
	}
}

#[macro_export]
macro_rules! format_err {
	($pos:expr, $($arg:tt)*) => {{
		Err(CompilationError::at($pos.clone(), format!($($arg)*)))
	}};
}
#[macro_export]
macro_rules! format_err_nowhere {
	($($arg:tt)*) => {{
		Err(CompilationError::nowhere(format!($($arg)*)))
	}};
}

pub type CompilerResult<T> = Result<T, CompilationError>;

pub fn at<T, E: std::error::Error>(res: Result<T, E>, pos: Pos) -> CompilerResult<T> {
	res.map_err(|err| CompilationError { msg: err.to_string(), pos: Some(pos) })
}
