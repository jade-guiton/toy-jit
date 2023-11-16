use core::panic;
use std::{rc::Rc, cell::RefCell};

use ahash::{HashMap, HashMapExt};
use inlinable_string::InlinableString;

use crate::{backend::Backend, asm::Label, ast::{Node, NodePos, Pos}, mmap::ExecBox};

#[derive(Clone)]
enum Location {
	Const(i64),
	Label(Label),
	Local(u32),
	Arg(u16),
	Temp,
}

#[derive(PartialEq, Eq, Clone)]
enum Type {
	Int,
	Fn {
		args: Box<[Type]>,
		ret: Option<Box<Type>>,
	},
}

impl std::fmt::Display for Type {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
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
}

impl Scope {
	fn new(parent: Option<Rc<RefCell<Scope>>>) -> Rc<RefCell<Self>> {
		Rc::new(RefCell::new(Scope {
			parent,
			bindings: HashMap::new(),
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
	
	fn put(&mut self, key: &str, val: Value) {
		if self.parent.is_none() && self.bindings.contains_key(key) {
			panic!("globals are not allowed to shadow each other");
		}
		self.bindings.insert(key.into(), val);
	}
}

pub struct CompilationError {
	msg: String,
	pos: Option<Pos>,
}

impl CompilationError {
	fn nowhere(msg: String) -> Self {
		CompilationError { msg, pos: None }
	}
}

impl std::fmt::Debug for CompilationError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		if let Some(pos) = &self.pos {
			f.write_fmt(format_args!("compilation error at {:?}: {}", pos, self.msg))
		} else {
			f.write_fmt(format_args!("compilation error: {}", self.msg))
		}
	}
}

macro_rules! format_err {
	($pos:expr, $($arg:tt)*) => {{
		Err(CompilationError {
			msg: format!($($arg)*),
			pos: Some($pos.clone()),
		})
	}};
}
macro_rules! format_err_nowhere {
	($($arg:tt)*) => {{
		Err(CompilationError {
			msg: format!($($arg)*),
			pos: None,
		})
	}};
}

type CompilerResult<T> = Result<T, CompilationError>;

fn at<T, E: std::error::Error>(res: Result<T, E>, pos: Pos) -> CompilerResult<T> {
	res.map_err(|err| CompilationError { msg: err.to_string(), pos: Some(pos) })
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
			globals: Scope::new(None)
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
		
		let entry_lbl = self.back.get_global("_start");
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
			Node::Sym(sym) if sym == "int" => {
				Type::Int
			},
			_ => unreachable!(),
		}
	}
	
	fn push_value(&mut self, val: &Value) {
		match val.0 {
			Location::Const(cst) => self.back.push_imm(cst),
			Location::Local(off) => self.back.push_local(off),
			Location::Arg(off) => self.back.push_arg(off),
			Location::Temp => {},
			Location::Label(_) => todo!(),
		}
	}
	
	fn compile_expr(&mut self, np: &NodePos, sc: &Scope) -> CompilerResult<Option<Value>> {
		let NodePos(node, pos) = np;
		match node {
			Node::Int(val) => {
				Ok(Some(Value(Location::Const(*val), Type::Int)))
			},
			Node::Id(id) => {
				match sc.find(id) {
					Some(val) => Ok(Some(val.clone())),
					None => format_err!(pos, "unknown identifier: '{}'", id),
				}
			},
			Node::Plus(lhs, rhs) => {
				let lhs_val = self.compile_expr_no_void(lhs, sc)?;
				let rhs_val = self.compile_expr_no_void(rhs, sc)?;
				match (&lhs_val.1, &rhs_val.1) {
					(Type::Int, Type::Int) => {
						self.push_value(&lhs_val);
						self.push_value(&rhs_val);
						self.back.add();
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
	
	fn compile_stat(&mut self, np: &NodePos, sc: &mut Scope, ret_ty: &Option<Type>) -> CompilerResult<()> {
		let NodePos(node, pos) = np;
		match node {
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
			_ => unreachable!(),
		}
	}
	
	pub fn compile_fn(&mut self, np: &NodePos) -> CompilerResult<()> {
		let NodePos(node, pos) = np;
		match node {
			Node::Fn { name, args, ret, body } => {
				let mut args_ty: Vec<Type> = vec![];
				let scope = Scope::new(Some(self.globals.clone()));
				for (i, (arg_name, arg_ty)) in args.iter().enumerate() {
					let arg_ty = self.resolve_ty(arg_ty);
					args_ty.push(arg_ty.clone());
					let arg_loc = Location::Arg(at(i.try_into(), pos.clone())?);
					scope.borrow_mut().put(arg_name, Value(arg_loc, arg_ty));
				}
				let ret_ty = ret.as_ref().map(|n| self.resolve_ty(&n.0));
				
				let fn_ty = Type::Fn { args: args_ty.into_boxed_slice(), ret: ret_ty.clone().map(Box::new) };
				let fn_lbl = self.back.get_global(name);
				let fn_loc = Location::Label(fn_lbl);
				self.globals.borrow_mut().put(name, Value(fn_loc, fn_ty));
				
				let ret_slots = if ret_ty.is_some() { 1 } else { 0 };
				self.back.begin_fn(fn_lbl, at(args.len().try_into(), pos.clone())?, ret_slots);
				for stat in body {
					self.compile_stat(stat, &mut scope.borrow_mut(), &ret_ty)?;
				}
				self.back.end_fn();
				Ok(())
			}
			_ => unreachable!(),
		}
	}
}