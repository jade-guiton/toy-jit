use std::{rc::Rc, cell::RefCell};

use ahash::{HashMap, HashMapExt};
use inlinable_string::InlinableString;

use crate::{
	backend::Backend,
	asm::Label,
	ast::{Node, NodePos, CompilationError, at, CompilerResult, BinCompOp, FnDecl, Pos},
	mmap::ExecBox,
	format_err_nowhere, format_err
};

#[derive(Clone, Debug)]
enum Location {
	Const(i64),
	Label(Label),
	Local(u32),
	Arg(u16),
	Temp,
	LazyComp(BinCompOp),
}

impl Location {
	fn is_concrete(&self) -> bool {
		match self {
			Location::Temp | Location::LazyComp(_) => false,
			_ => true,
		}
	}
	fn is_global(&self) -> bool {
		match self {
			Location::Const(_) | Location::Label(_) => true,
			_ => false,
		}
	}
}

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
struct Value(Location, Type);

struct Scope {
	parent: Option<Rc<RefCell<Scope>>>,
	bindings: HashMap<InlinableString, Value>,
	allow_shadowing: bool,
}

impl Scope {
	fn new(parent: Option<Rc<RefCell<Scope>>>, allow_shadowing: bool) -> Rc<RefCell<Self>> {
		Rc::new(RefCell::new(Scope {
			parent,
			bindings: HashMap::new(),
			allow_shadowing,
		}))
	}
	
	fn find(&self, key: &str) -> Option<Value> {
		if let Some(val) = self.bindings.get(key) {
			Some(val.clone())
		} else if let Some(parent) = &self.parent {
			parent.borrow().find(key)
		} else {
			None
		}
	}
	
	fn put(&mut self, key: &str, val: Value, pos: &Pos) -> CompilerResult<()> {
		assert!(val.0.is_concrete(), "binding at non-concrete location: {:?}", val.0);
		if self.parent.is_none() {
			assert!(val.0.is_global(), "global binding at non-constant location: {:?}", val.0);
		}
		if !self.allow_shadowing {
			if self.bindings.contains_key(key) {
				return format_err!(pos, "binding '{}' has multiple definitions", key);
			}
		}
		self.bindings.insert(key.into(), val);
		Ok(())
	}
}

pub struct CompilerOutput {
	_mem: ExecBox,
	pub entry: extern "sysv64" fn() -> i32
}

pub struct Compiler {
	back: Backend,
	globals: Rc<RefCell<Scope>>,
}

impl Compiler {
	pub fn new() -> Self {
		Compiler {
			back: Backend::new(),
			globals: Scope::new(None, false),
		}
	}
	
	pub fn finalize(mut self) -> CompilerResult<CompilerOutput> {
		let globals = self.globals.borrow();
		let main_val = globals.find("main")
			.ok_or_else(|| CompilationError::nowhere("no main function defined".into()))?;
		match &main_val.1 {
			Type::Fn { args, ret } => {
				if args.len() != 0 || *ret != Some(Box::new(Type::Int)) {
					format_err_nowhere!("invalid signature for 'main' (should be fn() -> int)")?;
				}
			},
			_ => format_err_nowhere!("'main' is not a function")?,
		}
		let main_lbl = match main_val.0 {
			Location::Label(lbl) => lbl,
			_ => format_err_nowhere!("'main' must be a static function")?,
		};
		
		let entry_lbl = self.back.new_label();
		self.back.gen_c_entry(entry_lbl, main_lbl);
		let entry_off = self.back.get_label_offset(entry_lbl);
		
		let mem = self.back.finalize();
		
		let entry: extern "sysv64" fn() -> i32 = unsafe {
			std::mem::transmute(mem.get_off(entry_off))
		};
		
		Ok(CompilerOutput { _mem: mem, entry })
	}
	
	fn resolve_ty(&self, node: &Node) -> Type {
		match node {
			Node::IntType => Type::Int,
			Node::BoolType => Type::Bool,
			_ => unreachable!(),
		}
	}
	
	fn push_value(&mut self, val: &Value) {
		match val.0 {
			Location::Const(cst) => self.back.push_imm(cst),
			Location::Local(off) => self.back.push_local(off),
			Location::Arg(off) => self.back.push_arg(off),
			Location::Temp => {},
			Location::Label(lbl) => self.back.push_label(lbl),
			Location::LazyComp(op) => self.back.bin_comp_push(op),
		}
	}
	
	fn compile_expr(&mut self, np: &NodePos, sc: &Scope) -> CompilerResult<Option<Value>> {
		let NodePos(node, pos) = np;
		match node {
			Node::IntLit(val) => {
				Ok(Some(Value(Location::Const(*val), Type::Int)))
			},
			Node::Id(id) => {
				match sc.find(id) {
					Some(val) => Ok(Some(val.clone())),
					None => format_err!(pos, "unknown identifier: '{}'", id),
				}
			},
			Node::BinComp(op, lhs, rhs) => {
				let lhs_val = self.compile_expr_no_void(lhs, sc)?;
				let rhs_val = self.compile_expr_no_void(rhs, sc)?;
				match (&lhs_val.1, &rhs_val.1) {
					(Type::Int, Type::Int) => {
						self.push_value(&lhs_val);
						self.push_value(&rhs_val);
						Ok(Some(Value(Location::LazyComp(*op), Type::Bool)))
					},
					(lhs_ty, rhs_ty) => format_err!(pos, "cannot add {} to {}", lhs_ty, rhs_ty),
				}
			},
			Node::BinArith(op, lhs, rhs) => {
				let lhs_val = self.compile_expr_no_void(lhs, sc)?;
				let rhs_val = self.compile_expr_no_void(rhs, sc)?;
				match (&lhs_val.1, &rhs_val.1) {
					(Type::Int, Type::Int) => {
						self.push_value(&lhs_val);
						self.push_value(&rhs_val);
						self.back.bin_arith(*op);
						Ok(Some(Value(Location::Temp, Type::Int)))
					},
					(lhs_ty, rhs_ty) => format_err!(pos, "cannot add {} to {}", lhs_ty, rhs_ty),
				}
			},
			Node::Call { func, args } => {
				let fn_val = self.compile_expr_no_void(func, sc)?;
				match fn_val.1 {
					Type::Fn { args: args_ty, ret: ret_ty } => {
						if args.len() != args_ty.len() {
							format_err!(pos, "expected {} arguments, got {}", args_ty.len(), args.len())?;
						}
						let ret_slots = if ret_ty.is_some() { 1 } else { 0 };
						self.back.precall(ret_slots);
						for (arg, arg_ty) in args.iter().zip(args_ty.iter()) {
							let val = self.compile_expr_no_void(arg, sc)?;
							if val.1 != *arg_ty {
								format_err!(arg.1, "expected {}, got {}", arg_ty, val.1)?;
							}
							self.push_value(&val);
						}
						match fn_val.0 {
							Location::Label(fn_lbl) => {
								self.back.call(fn_lbl, at(args.len().try_into(), pos.clone())?, ret_slots);
							},
							_ => format_err!(pos, "functions can only be called directly for now")?,
						}
						Ok(ret_ty.map(|ty| Value(Location::Temp, *ty)))
					},
					_ => format_err!(pos, "cannot call {}", fn_val.1),
				}
			},
			_ => unreachable!(),
		}
	}
	
	fn compile_expr_no_void(&mut self, np: &NodePos, sc: &Scope) -> CompilerResult<Value> {
		if let Some(val) = self.compile_expr(np, sc)? {
			Ok(val)
		} else {
			format_err!(np.1, "cannot use void expression here")
		}
	}
	
	fn compile_branch(&mut self, cond: &NodePos, sc: &Scope, to: Label, invert: bool) -> CompilerResult<()> {
		// When &&/|| are implemented, shortcut evaluation should be implemented here
		let cond_val = self.compile_expr_no_void(cond, sc)?;
		if cond_val.1 != Type::Bool {
			format_err!(cond.1, "condition must evaluate to bool")?;
		}
		if let Location::LazyComp(op) = cond_val.0 {
			self.back.bin_comp_jmp(op, invert, to);
		} else {
			self.push_value(&cond_val);
			self.back.zero_comp_jmp(invert, to);
		}
		Ok(())
	}
	
	fn compile_stat(&mut self, np: &NodePos, sc: &mut Scope, ret_ty: &Option<Type>) -> CompilerResult<()> {
		let NodePos(node, pos) = np;
		match node {
			Node::If { if_br, else_br } => {
				let end = self.back.new_label();
				for (cond, block) in if_br {
					let next = self.back.new_label();
					self.compile_branch(cond, sc, next, true)?;
					for instr in block {
						self.compile_stat(instr, sc, ret_ty)?;
					}
					self.back.jmp(end);
					self.back.set_label(next);
				}
				if let Some(block) = else_br {
					for instr in block {
						self.compile_stat(instr, sc, ret_ty)?;
					}
				}
				self.back.set_label(end);
				Ok(())
			},
			Node::Ret(exp) => {
				let val = exp.as_ref().map(|exp| self.compile_expr(&exp, sc)).transpose()?.flatten();
				if val.as_ref().map(|v| &v.1) != ret_ty.as_ref() {
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
				let val = self.compile_expr(&exp, sc)?;
				if val.is_some() {
					self.back.ignore();
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
		let fn_lbl = self.back.new_label();
		let fn_loc = Location::Label(fn_lbl);
		let fn_val = Value(fn_loc, fn_ty);
		self.globals.borrow_mut().put(&fn_decl.name, fn_val.clone(), pos)?;
		
		Ok(())
	}
	
	pub fn compile_fn(&mut self, fn_decl: &FnDecl, pos: &Pos) -> CompilerResult<()> {
		let fn_val = self.globals.borrow().find(&fn_decl.name).expect("function not declared before compilation");
		let (args_ty, ret_ty, fn_lbl) =
			if let Value(Location::Label(lbl), Type::Fn { args, ret }) = fn_val {
				(args, ret.map(|b| *b.clone()), lbl)
			} else { unreachable!() };
		
		let arg_scope = Scope::new(Some(self.globals.clone()), false);
		for (i, (arg_name, _)) in fn_decl.args.iter().enumerate() {
			let arg_loc = Location::Arg(at(i.try_into(), pos.clone())?);
			arg_scope.borrow_mut().put(arg_name, Value(arg_loc, args_ty[i].clone()), pos)?;
		}
		let fn_scope = Scope::new(Some(arg_scope), true);
		
		let ret_slots = if ret_ty.is_some() { 1 } else { 0 };
		self.back.begin_fn(fn_lbl, at(fn_decl.args.len().try_into(), pos.clone())?, ret_slots);
		for stat in &fn_decl.body {
			self.compile_stat(stat, &mut fn_scope.borrow_mut(), &ret_ty)?;
		}
		self.back.ret(); // Just in case
		self.back.end_fn();
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
	
	pub fn compile_program(ast: &[NodePos]) -> CompilerResult<CompilerOutput> {
		let mut comp = Compiler::new();
		for decl in ast {
			comp.declare(decl)?;
		}
		for decl in ast {
			comp.define(decl)?;
		}
		comp.finalize()
	}
}