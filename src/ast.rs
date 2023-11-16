use std::rc::Rc;

use inlinable_string::InlinableString;

#[derive(Clone)]
pub struct Pos {
	pub file: Rc<str>,
	pub line: u32,
	pub col: u32,
}

impl std::fmt::Debug for Pos {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		f.write_fmt(format_args!("{}:{}:{}", self.file, self.line, self.col))
	}
}

pub enum Node {
	Fn {
		name: InlinableString,
		args: Vec<(InlinableString, Node)>,
		ret: Option<Box<NodePos>>,
		body: Vec<NodePos>,
	},
	Ret(Option<Box<NodePos>>),
	Call {
		func: Box<NodePos>,
		args: Vec<NodePos>,
	},
	Plus(Box<NodePos>, Box<NodePos>),
	Id(InlinableString),
	Int(i64),
	Sym(InlinableString),
	// Only used in lexer:
	Eof,
}

pub struct NodePos(pub Node, pub Pos);
