use ahash::{HashMap, HashMapExt};
use inlinable_string::InlinableString;

use crate::{
	backend::Backend,
	asm::Label,
	ast::{Node, NodePos, at, CompilerResult, BinCompOp, FnDecl, Pos},
	mmap::ExecBox,
	gdb::SymFile,
	format_err_nowhere, format_err, let_variant, format_err_nowhere_raw, format_err_raw,
};

#[derive(PartialEq, Eq, Clone)]
pub enum Type {
	Int,
	Bool,
	Fn {
		args: Box<[Type]>,
		ret: Option<Box<Type>>,
	},
}

impl std::fmt::Display for Type {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			Type::Bool => f.write_str("bool"),
			Type::Int => f.write_str("int"),
			Type::Fn { args, ret } => {
				f.write_str("fn(")?;
				for (i, arg) in args.iter().enumerate() {
					if i != 0 {
						f.write_str(", ")?;
					}
					f.write_fmt(format_args!("{}", arg))?;
				}
				if let Some(ret) = ret {
					f.write_fmt(format_args!(") -> {}", ret))
				} else {
					f.write_str(")")
				}
			},
		}
	}
}

#[derive(Clone)]
enum GlobalLocation {
	Const(i64),
	Label(Label),
}

#[derive(Clone)]
enum Location {
	Global(GlobalLocation),
	Arg(u16),
	Local(u32),
	Temp,
	LazyComp(BinCompOp),
}

#[derive(Clone)]
struct Value {
	loc: Location,
	ty: Type,
}

enum Block {
	Global {
		globals: HashMap<InlinableString, (GlobalLocation, Type)>,
	},
	Function {
		args: HashMap<InlinableString, (u16, Type)>,
		ret_ty: Option<Type>,
	},
	Block {
		locals: HashMap<InlinableString, (u32, Type)>,
		next_off: u32,
		extra_off: u32,
	}
}

impl Block {
	fn find(&self, key: &str) -> Option<Value> {
		match self {
			Block::Global { globals } => {
				globals.get(key).cloned().map(|(loc, ty)| Value { loc: Location::Global(loc), ty })
			},
			Block::Function { args, .. } => {
				args.get(key).cloned().map(|(off, ty)| Value { loc: Location::Arg(off), ty })
			},
			Block::Block { locals, .. } => {
				locals.get(key).cloned().map(|(off, ty)| Value { loc: Location::Local(off), ty })
			},
		}
	}
}

pub struct CompilerOutput {
	_mem: ExecBox,
	pub entry: extern "sysv64" fn() -> i32,
	pub symbols: Box<[u8]>
}

pub struct Compiler {
	back: Backend,
	sym_file: SymFile,
	blocks: Vec<Block>,
}

impl Compiler {
	pub fn new() -> Self {
		Compiler {
			back: Backend::new(),
			sym_file: SymFile::new(),
			blocks: vec![Block::Global { globals: HashMap::new() }],
		}
	}
	
	pub fn finalize(mut self) -> CompilerResult<CompilerOutput> {
		assert!(self.blocks.len() == 1);
		let_variant!(globals, Block::Global { globals } = &self.blocks[0]);
		let (main_loc, main_ty) = globals.get("main")
			.ok_or_else(|| format_err_nowhere_raw!("no main function defined"))?;
		match main_ty {
			Type::Fn { args, ret } => {
				if args.len() != 0 || ret != &Some(Box::new(Type::Int)) {
					format_err_nowhere!("invalid signature for 'main' (should be fn() -> int)")?;
				}
			},
			_ => format_err_nowhere!("'main' is not a function")?,
		}
		let main_lbl = if let GlobalLocation::Label(lbl) = main_loc { lbl } else { unreachable!() };
		
		let entry_lbl = self.back.new_label();
		let entry_addr = self.back.get_cur_addr();
		self.back.gen_c_entry(entry_lbl, *main_lbl);
		
		self.sym_file.new_file("<entry>");
		self.sym_file.new_fn("<entry>", entry_addr, self.back.get_cur_addr());
		
		let entry_off = self.back.get_label_offset(entry_lbl);
		
		let mem = self.back.finalize();
		
		let entry: extern "sysv64" fn() -> i32 = unsafe {
			std::mem::transmute(mem.get_off(entry_off))
		};
		
		Ok(CompilerOutput { _mem: mem, entry, symbols: self.sym_file.into_bytes() })
	}
	
	fn find_binding(&self, name: &str) -> Option<Value> {
		self.blocks.iter().rev().find_map(|b| b.find(name))
	}
	
	fn resolve_ty(&self, node: &Node) -> Type {
		match node {
			Node::IntType => Type::Int,
			Node::BoolType => Type::Bool,
			_ => unreachable!(),
		}
	}
	
	fn push_value(&mut self, val: &Value) {
		match val.loc {
			Location::Global(GlobalLocation::Const(cst)) => self.back.push_imm(cst),
			Location::Global(GlobalLocation::Label(lbl)) => self.back.push_label(lbl),
			Location::Local(off) => self.back.push_local(off),
			Location::Arg(off) => self.back.push_arg(off),
			Location::Temp => {},
			Location::LazyComp(op) => self.back.bin_comp_push(op),
		}
	}
	
	fn compile_expr(&mut self, np: &NodePos) -> CompilerResult<Option<Value>> {
		let NodePos(node, pos) = np;
		match node {
			Node::IntLit(val) => {
				Ok(Some(Value { loc: Location::Global(GlobalLocation::Const(*val)), ty: Type::Int }))
			},
			Node::Id(id) => {
				if let Some(val) = self.find_binding(id) {
					Ok(Some(val))
				} else {
					format_err!(pos, "unknown identifier: '{}'", id)
				}
			},
			Node::BinComp(op, lhs, rhs) => {
				let lhs_val = self.compile_expr_no_void(lhs)?;
				let rhs_val = self.compile_expr_no_void(rhs)?;
				match (&lhs_val.ty, &rhs_val.ty) {
					(Type::Int, Type::Int) => {
						self.push_value(&lhs_val);
						self.push_value(&rhs_val);
						Ok(Some(Value { loc: Location::LazyComp(*op), ty: Type::Bool }))
					},
					(lhs_ty, rhs_ty) => format_err!(pos, "cannot add {} to {}", lhs_ty, rhs_ty),
				}
			},
			Node::BinArith(op, lhs, rhs) => {
				let lhs_val = self.compile_expr_no_void(lhs)?;
				let rhs_val = self.compile_expr_no_void(rhs)?;
				match (&lhs_val.ty, &rhs_val.ty) {
					(Type::Int, Type::Int) => {
						self.push_value(&lhs_val);
						self.push_value(&rhs_val);
						self.back.bin_arith(*op);
						Ok(Some(Value { loc: Location::Temp, ty: Type::Int }))
					},
					(lhs_ty, rhs_ty) => format_err!(pos, "cannot add {} to {}", lhs_ty, rhs_ty),
				}
			},
			Node::Call { func, args } => {
				let fn_val = self.compile_expr_no_void(func)?;
				match fn_val.ty {
					Type::Fn { args: args_ty, ret: ret_ty } => {
						if args.len() != args_ty.len() {
							format_err!(pos, "expected {} arguments, got {}", args_ty.len(), args.len())?;
						}
						let ret_slots = if ret_ty.is_some() { 1 } else { 0 };
						self.back.precall(ret_slots);
						for (arg, arg_ty) in args.iter().zip(args_ty.iter()) {
							let val = self.compile_expr_no_void(arg)?;
							if val.ty != *arg_ty {
								format_err!(arg.1, "expected {}, got {}", arg_ty, val.ty)?;
							}
							self.push_value(&val);
						}
						match fn_val.loc {
							Location::Global(GlobalLocation::Label(fn_lbl)) => {
								self.back.call(fn_lbl, at(args.len().try_into(), pos.clone())?, ret_slots);
							},
							_ => format_err!(pos, "functions can only be called directly for now")?,
						}
						Ok(ret_ty.map(|ty| Value { loc: Location::Temp, ty: *ty }))
					},
					_ => format_err!(pos, "cannot call {}", fn_val.ty),
				}
			},
			_ => unreachable!(),
		}
	}
	
	fn compile_expr_no_void(&mut self, np: &NodePos) -> CompilerResult<Value> {
		if let Some(val) = self.compile_expr(np)? {
			Ok(val)
		} else {
			format_err!(np.1, "cannot use void expression here")
		}
	}
	
	fn compile_branch(&mut self, cond: &NodePos, to: Label, invert: bool) -> CompilerResult<()> {
		// When &&/|| are implemented, shortcut evaluation should be implemented here
		let cond_val = self.compile_expr_no_void(cond)?;
		if cond_val.ty != Type::Bool {
			format_err!(cond.1, "condition must evaluate to bool")?;
		}
		if let Location::LazyComp(op) = cond_val.loc {
			self.back.bin_comp_jmp(op, invert, to);
		} else {
			self.push_value(&cond_val);
			self.back.zero_comp_jmp(invert, to);
		}
		Ok(())
	}
	
	fn compile_stat(&mut self, np: &NodePos) -> CompilerResult<()> {
		let NodePos(node, pos) = np;
		match node {
			Node::If { if_br, else_br } => {
				let end = self.back.new_label();
				for (cond, block) in if_br {
					let next = self.back.new_label();
					self.compile_branch(cond, next, true)?;
					self.compile_block(block)?;
					self.back.jmp(end);
					self.back.set_label(next);
				}
				if let Some(block) = else_br {
					self.compile_block(block)?;
				}
				self.back.set_label(end);
				Ok(())
			},
			Node::While(cond, block) => {
				let start = self.back.new_label();
				let end = self.back.new_label();
				self.back.set_label(start);
				self.compile_branch(cond, end, true)?;
				self.compile_block(block)?;
				self.back.jmp(start);
				self.back.set_label(end);
				Ok(())
			},
			Node::Ret(exp) => {
				let val = exp.as_ref().map(|exp| self.compile_expr(&exp)).transpose()?.flatten();
				let ret_ty = self.blocks.iter().rev().find_map(|b|
					if let Block::Function { ret_ty, .. } = b { Some(ret_ty) } else { None })
					.expect("not in function");
				if val.as_ref().map(|v| v.ty.clone()) != *ret_ty {
					format_err!(pos, "incompatible return type")?;
				}
				if let Some(val) = val {
					self.push_value(&val);
					self.back.pop_ret(0);
				}
				self.back.ret();
				Ok(())
			},
			Node::ExprStat(exp) => {
				let val = self.compile_expr(&exp)?;
				if val.is_some() {
					self.back.ignore();
				}
				Ok(())
			},
			Node::Let { name, ty, expr } => {
				let mut ty = ty.as_ref().map(|n| self.resolve_ty(&n));
				if let Some(expr) = expr {
					let val = self.compile_expr_no_void(&expr)?;
					if let Some(exp_ty) = &ty {
						if &val.ty != exp_ty {
							format_err!(pos, "cannot assign {} value to {} binding", val.ty, exp_ty)?;
						}
					} else {
						ty = Some(val.ty.clone());
					}
					self.push_value(&val);
				}
				let ty = ty.ok_or(format_err_raw!(pos, "no type can be inferred for binding '{}'", name))?;
				
				let_variant!((locals, next_off, extra_off),
					Block::Block { locals, next_off, extra_off, .. } = self.blocks.last_mut().unwrap());
				let off = *next_off;
				locals.insert(name.clone(), (off, ty));
				
				let slots = 1; // todo: types with different sizes
				*next_off += slots;
				*extra_off += slots;
				if expr.is_some() {
					self.back.new_local(slots);
				} else {
					self.back.new_undefined_local(slots);
				}
				
				Ok(())
			},
			Node::Set(pat, expr) => {
				let val = self.compile_expr_no_void(&expr)?;
				match &pat.0 {
					Node::Id(id) => {
						if let Some(var) = self.find_binding(id) {
							if var.ty != val.ty {
								format_err!(pos, "cannot assign {} value to {} binding", val.ty, var.ty)?;
							}
							self.push_value(&val);
							match var.loc {
								Location::Arg(off) => self.back.pop_arg(off),
								Location::Local(off) => self.back.pop_local(off),
								_ => format_err!(pos, "binding '{}' cannot be reassigned", id)?,
							}
						} else {
							format_err!(pos, "unknown identifier: '{}'", id)?
						}
					},
					_ => unreachable!(),
				}
				Ok(())
			},
			_ => unreachable!(),
		}
	}
	
	pub fn declare_fn(&mut self, fn_decl: &FnDecl, pos: &Pos) -> CompilerResult<()> {
		let mut args_ty: Vec<Type> = vec![];
		for (_, arg_ty) in fn_decl.args.iter() {
			let arg_ty = self.resolve_ty(arg_ty);
			args_ty.push(arg_ty.clone());
		}
		let args_ty = args_ty.into_boxed_slice();
		let ret_ty = fn_decl.ret.as_ref().map(|n| self.resolve_ty(&n));
		
		let fn_ty = Type::Fn { args: args_ty.clone(), ret: ret_ty.clone().map(Box::new) };
		let fn_loc = GlobalLocation::Label(self.back.new_label());
		let_variant!(globals, Block::Global { globals } = &mut self.blocks[0]);
		if globals.insert(fn_decl.name.clone(), (fn_loc, fn_ty)).is_some() {
			format_err!(pos, "a global named '{}' is already defined", fn_decl.name)?;
		}
		
		Ok(())
	}
	
	fn free_block_locals(&mut self) {
		let_variant!((locals, next_off, extra_off),
			Block::Block { locals, next_off, extra_off, .. } = self.blocks.last_mut().unwrap());
		if *extra_off > 0 {
			self.back.drop_locals(*extra_off);
			locals.clear();
			*next_off -= *extra_off;
			*extra_off = 0;
		}
	}
	
	fn compile_block(&mut self, block: &[NodePos]) -> CompilerResult<()> {
		self.blocks.push(Block::Block {
			locals: HashMap::new(),
			next_off: self.blocks.iter().rev().find_map(|b| {
				if let Block::Block { next_off, ..} = b { Some(*next_off) } else { None }
			}).unwrap_or(0),
			extra_off: 0,
		});
		for stat in block {
			self.sym_file.map_line(stat.1.line, self.back.get_cur_addr());
			self.compile_stat(stat)?;
		}
		self.free_block_locals();
		self.blocks.pop().unwrap();
		Ok(())
	}
	
	pub fn compile_fn(&mut self, fn_decl: &FnDecl, pos: &Pos) -> CompilerResult<()> {
		let_variant!(globals, Block::Global { globals } = &self.blocks[0]);
		let (fn_loc, fn_ty) = globals.get(&fn_decl.name)
			.expect("function not declared before compilation")
			.clone();
		let_variant!((args_ty, ret_ty), Type::Fn { args: args_ty, ret: ret_ty } = fn_ty);
		let_variant!(fn_lbl, GlobalLocation::Label(fn_lbl) = fn_loc);
		
		let mut args = HashMap::new();
		for (i, (arg_name, _)) in fn_decl.args.iter().enumerate() {
			let arg_off = at(i.try_into(), pos.clone())?;
			args.insert(arg_name.clone(), (arg_off, args_ty[i].clone()));
		}
		let ret_slots = if ret_ty.is_some() { 1 } else { 0 };
		let fn_block = Block::Function { args, ret_ty: ret_ty.map(|b| (*b).clone()) };
		
		let start_addr = self.back.get_cur_addr();
		self.sym_file.map_line(pos.line, start_addr);
		
		self.back.begin_fn(fn_lbl, at(fn_decl.args.len().try_into(), pos.clone())?, ret_slots);
		if fn_decl.name == "main" {
			self.back.add_breakpoint();
		}
		self.blocks.push(fn_block);
		let block_cnt = self.blocks.len();
		self.compile_block(&fn_decl.body)?;
		assert!(self.blocks.len() == block_cnt, "blocks were not pushed or popped properly");
		self.blocks.pop();
		self.back.ret(); // Just in case
		self.back.end_fn();
		
		let end_addr = self.back.get_cur_addr();
		self.sym_file.new_fn(&fn_decl.name, start_addr, end_addr);
		
		Ok(())
	}
	
	pub fn declare(&mut self, np: &NodePos) -> CompilerResult<()> {
		let NodePos(node, pos) = np;
		match node {
			Node::Fn(fn_decl) => self.declare_fn(fn_decl, pos).map(|_| ()),
			_ => unreachable!(),
		}
	}
	
	pub fn define(&mut self, np: &NodePos) -> CompilerResult<()> {
		let NodePos(node, pos) = np;
		match node {
			Node::Fn(fn_decl) => self.compile_fn(fn_decl, pos),
			_ => unreachable!(),
		}
	}
	
	pub fn compile_program(file_name: &str, ast: &[NodePos]) -> CompilerResult<CompilerOutput> {
		let mut comp = Compiler::new();
		for decl in ast {
			comp.declare(decl)?;
		}
		comp.sym_file.new_file(file_name);
		for decl in ast {
			comp.define(decl)?;
		}
		comp.sym_file.end_file();
		comp.finalize()
	}
}