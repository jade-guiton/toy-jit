use inlinable_string::InlinableString;
use num_enum::TryFromPrimitive;
use unicode_xid::UnicodeXID;

use crate::{ast::{CompilationError, CompilerResult, Node, Pos, NodePos, Block, BinCompOp, BinArithOp, FnDecl}, format_err};

pub struct Parser<'a> {
	rest: &'a str,
	next_pos: Pos,
	next_char: Option<char>,
	next_token_pos: Pos,
	next_token: Node,
}

impl Parser<'_> {
	fn advance_char(&mut self) {
		if let Some(c) = self.next_char {
			if c == '\n' {
				self.next_pos.line += 1;
				self.next_pos.off = 1;
			} else {
				self.next_pos.off += c.len_utf8() as u32;
			}
			self.rest = &self.rest[c.len_utf8()..];
		}
		if let Some(c) = self.rest.chars().next() {
			self.next_char = Some(c);
		} else {
			self.next_char = None;
		}
	}
	
	fn skip_space(&mut self) {
		while self.next_char.unwrap_or('\0').is_ascii_whitespace() {
			self.advance_char();
		}
	}
}

const KEYWORDS: [&'static str; 6] = [
	"int", "bool",
	"fn", "ret",
	"if", "else",
];

impl Parser<'_> {
	fn advance_token(&mut self) -> CompilerResult<NodePos> {
		let new_token_pos = self.next_pos.clone();
		let new_token = match self.next_char {
			Some(c) => {
				if c.is_xid_start() {
					let s = self.rest;
					loop {
						self.advance_char();
						if !self.next_char.unwrap_or('\0').is_xid_continue() {
							break;
						}
					}
					let len = self.rest.as_ptr() as usize - s.as_ptr() as usize;
					let slice = &s[..len];
					let id = InlinableString::from(slice);
					if KEYWORDS.contains(&slice) {
						Node::Sym(id)
					} else {
						Node::Id(id)
					}
				} else if c.is_ascii_digit() {
					let s = self.rest;
					loop {
						self.advance_char();
						if !self.next_char.unwrap_or('\0').is_ascii_digit() {
							break;
						}
					}
					let len = self.rest.as_ptr() as usize - s.as_ptr() as usize;
					let slice = &s[..len];
					Node::IntLit(
						slice.parse::<i64>().map_err(|_|
							CompilationError::at(new_token_pos.clone(), "integer literal too large".into())
						)?
					)
				} else if "(){};,+-".contains(c) {
					let slice = &self.rest[..1];
					self.advance_char();
					Node::Sym(InlinableString::from(slice))
				} else if "<>".contains(c) {
					let s = self.rest;
					let mut slice = &s[..1];
					self.advance_char();
					if self.next_char.unwrap_or('\0') == '=' {
						slice = &s[..2];
						self.advance_char();
					}
					Node::Sym(InlinableString::from(slice))
				} else {
					return format_err!(new_token_pos, "unexpected character");
				}
			}
			None => Node::Eof,
		};
		self.skip_space();
		Ok(NodePos(
			std::mem::replace(&mut self.next_token, new_token),
			std::mem::replace(&mut self.next_token_pos, new_token_pos),
		))
	}
	
	fn expect_sym(&mut self, sym: &str) -> CompilerResult<Pos> {
		if let Node::Sym(sym2) = &self.next_token {
			if sym == sym2 {
				return Ok(self.advance_token()?.1);
			}
		}
		format_err!(self.next_token_pos, "expected '{}'", sym)
	}
	fn check_sym(&mut self, sym: &str) -> CompilerResult<bool> {
		if let Node::Sym(sym2) = &self.next_token {
			if sym == sym2 {
				self.advance_token()?;
				return Ok(true);
			}
		}
		Ok(false)
	}
	
	fn expect_id(&mut self) -> CompilerResult<(InlinableString, Pos)> {
		if matches!(self.next_token, Node::Id(_)) {
			let NodePos(node, pos) = self.advance_token()?;
			let id = if let Node::Id(id) = node { id } else { unreachable!() };
			Ok((id, pos))
		} else {
			format_err!(self.next_token_pos, "expected identifier")
		}
	}
	
	fn parse_ty(&mut self) -> CompilerResult<Node> {
		if self.check_sym("int")? {
			Ok(Node::IntType)
		} else if self.check_sym("bool")? {
			Ok(Node::BoolType)
		} else {
			format_err!(self.next_token_pos, "expected type")
		}
	}
	
	fn parse_prim_expr(&mut self) -> CompilerResult<NodePos> {
		if matches!(self.next_token, Node::Id(_) | Node::IntLit(_)) {
			self.advance_token()
		} else if self.check_sym("(")? {
			let expr = self.parse_expr()?;
			self.expect_sym(")")?;
			Ok(expr)
		} else {
			format_err!(self.next_token_pos, "expected expression")
		}
	}
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, TryFromPrimitive)]
#[repr(u32)]
enum OpPrec {
	Comp, Plus, Call,
}

impl Parser<'_> {
	fn parse_expr_prec(&mut self, min_prec: OpPrec) -> CompilerResult<NodePos> {
		let mut cur = self.parse_prim_expr()?;
		loop {
			let op_pos = self.next_token_pos.clone();
			if min_prec <= OpPrec::Comp && self.check_sym("<=")? {
				let rhs = self.parse_expr_prec(OpPrec::try_from(OpPrec::Comp as u32 + 1).unwrap())?;
				cur = NodePos(
					Node::BinComp(BinCompOp::LE, Box::new(cur), Box::new(rhs)),
					op_pos,
				);
			} else if min_prec <= OpPrec::Plus && self.check_sym("+")? {
				let rhs = self.parse_expr_prec(OpPrec::try_from(OpPrec::Plus as u32 + 1).unwrap())?;
				cur = NodePos(
					Node::BinArith(BinArithOp::Plus, Box::new(cur), Box::new(rhs)),
					op_pos,
				);
			} else if min_prec <= OpPrec::Plus && self.check_sym("-")? {
				let rhs = self.parse_expr_prec(OpPrec::try_from(OpPrec::Plus as u32 + 1).unwrap())?;
				cur = NodePos(
					Node::BinArith(BinArithOp::Minus, Box::new(cur), Box::new(rhs)),
					op_pos,
				);
			} else if min_prec <= OpPrec::Call && self.check_sym("(")? {
				let mut args = vec![];
				while !self.check_sym(")")? {
					args.push(self.parse_expr()?);
					if !self.check_sym(",")? {
						self.expect_sym(")")?;
						break;
					}
				}
				cur = NodePos(
					Node::Call { func: Box::new(cur), args },
					op_pos,
				);
			} else {
				break;
			}
		}
		Ok(cur)
	}
	
	fn parse_expr(&mut self) -> CompilerResult<NodePos> {
		self.parse_expr_prec(OpPrec::try_from(0).unwrap())
	}
	
	fn parse_instr(&mut self) -> CompilerResult<NodePos> {
		let pos = self.next_token_pos.clone();
		if self.check_sym("ret")? {
			let mut exp = None;
			if !self.check_sym(";")? {
				exp = Some(Box::new(self.parse_expr()?));
				self.expect_sym(";")?;
			}
			Ok(NodePos(Node::Ret(exp), pos))
		} else if self.check_sym("if")? {
			let mut if_br = vec![];
			let mut else_br = None;
			loop {
				let cond = self.parse_expr()?;
				self.expect_sym("{")?;
				let body = self.parse_block()?;
				if_br.push((cond, body));
				if self.check_sym("else")? {
					if self.check_sym("if")? {
						continue;
					}
					self.expect_sym("{")?;
					let body = self.parse_block()?;
					else_br = Some(body);
					break;
				} else {
					break;
				}
			}
			Ok(NodePos(Node::If { if_br, else_br }, pos))
		} else {
			let expr = self.parse_expr()?;
			self.expect_sym(";")?;
			Ok(NodePos(Node::ExprStat(Box::new(expr)), pos))
		}
	}

	fn parse_block(&mut self) -> CompilerResult<Block> {
		let mut body = vec![];
		while !self.check_sym("}")? {
			body.push(self.parse_instr()?);
		}
		Ok(body)
	}
	
	fn parse_fn(&mut self) -> CompilerResult<NodePos> {
		let fn_pos = self.expect_sym("fn")?;
		let (name, _) = self.expect_id()?;
		self.expect_sym("(")?;
		let mut args = vec![];
		while !self.check_sym(")")? {
			let (arg_name, _) = self.expect_id()?;
			let arg_ty = self.parse_ty()?;
			args.push((arg_name, arg_ty));
			if !self.check_sym(",")? {
				self.expect_sym(")")?;
				break;
			}
		}
		let mut ret = None;
		if !self.check_sym("{")? {
			ret = Some(self.parse_ty()?);
			self.expect_sym("{")?;
		}
		let body = self.parse_block()?;
		Ok(NodePos(
			Node::Fn(FnDecl { name, args, ret: ret.map(Box::new), body }),
			fn_pos,
		))
	}
	
	pub fn parse_program(file: String, slice: &str) -> CompilerResult<Vec<NodePos>> {
		let initial_pos = Pos {
			file: file.into(),
			line: 1, off: 1
		};
		let mut p = Parser {
			rest: slice,
			next_pos: initial_pos.clone(),
			next_char: None,
			next_token_pos: initial_pos,
			next_token: Node::Eof,
		};
		p.advance_char(); // prime next_char
		p.skip_space();
		p.advance_token()?; // prime next_token
		
		let mut fns = vec![];
		while !matches!(p.next_token, Node::Eof) {
			fns.push(p.parse_fn()?);
		}
		
		Ok(fns)
	}
}

