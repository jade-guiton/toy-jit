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

#[derive(Clone, Copy, Debug)]
pub enum BinCompOp {
	LT, GT,
	LE, GE,
	EQ, NE,
}

impl BinCompOp {
	pub fn opposite(self) -> BinCompOp {
		match self {
			BinCompOp::LT => BinCompOp::GE,
			BinCompOp::GT => BinCompOp::LE,
			BinCompOp::LE => BinCompOp::GT,
			BinCompOp::GE => BinCompOp::LT,
			BinCompOp::EQ => BinCompOp::NE,
			BinCompOp::NE => BinCompOp::EQ,
		}
	}
}

#[derive(Clone, Copy)]
pub enum BinArithOp {
	Plus,
	Minus,
}

pub struct FnDecl {
	pub name: InlinableString,
	pub args: Vec<(InlinableString, Node)>,
	pub ret: Option<Box<Node>>,
	pub body: Block,
}

pub enum Node {
	Fn(FnDecl),
	IntType, BoolType,
	If {
		if_br: Vec<(NodePos, Block)>,
		else_br: Option<Block>,
	},
	While(Box<NodePos>, Block),
	Let {
		name: InlinableString,
		ty: Option<Box<Node>>,
		expr: Option<Box<NodePos>>,
	},
	Set(Box<NodePos>, Box<NodePos>),
	ExprStat(Box<NodePos>),
	Ret(Option<Box<NodePos>>),
	Call {
		func: Box<NodePos>,
		args: Vec<NodePos>,
	},
	BinComp(BinCompOp, Box<NodePos>, Box<NodePos>),
	BinArith(BinArithOp, Box<NodePos>, Box<NodePos>),
	Id(InlinableString),
	IntLit(i64),
	Sym(InlinableString),
	// Only used in lexer:
	Eof,
}

pub struct NodePos(pub Node, pub Pos);

pub type Block = Vec<NodePos>;

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
macro_rules! format_err_raw {
	($pos:expr, $($arg:tt)*) => {{
		crate::ast::CompilationError::at($pos.clone(), format!($($arg)*))
	}};
}
#[macro_export]
macro_rules! format_err {
	($pos:expr, $($arg:tt)*) => {{
		Err(crate::format_err_raw!($pos, $($arg)*))
	}};
}
#[macro_export]
macro_rules! format_err_nowhere_raw {
	($($arg:tt)*) => {{
		crate::ast::CompilationError::nowhere(format!($($arg)*))
	}};
}
#[macro_export]
macro_rules! format_err_nowhere {
	($($arg:tt)*) => {{
		Err(crate::format_err_nowhere_raw!($($arg)*))
	}};
}

pub type CompilerResult<T> = Result<T, CompilationError>;

pub fn at<T, E: std::error::Error>(res: Result<T, E>, pos: Pos) -> CompilerResult<T> {
	res.map_err(|err| CompilationError { msg: err.to_string(), pos: Some(pos) })
}
