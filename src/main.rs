use ast::{Pos, NodePos, Node};
use compiler::Compiler;
use inlinable_string::InlinableString;

mod mmap;
mod asm;
mod backend;
mod compiler;
mod ast;

extern fn nop(_: nix::libc::c_int) {}

fn main() {
	// ignore intentional breakpoint if not in debugger
	unsafe { nix::sys::signal::signal(nix::sys::signal::SIGTRAP, nix::sys::signal::SigHandler::Handler(nop)) }
		.expect("signal()");
	
	/*
	let mut back = Backend::new();
	
	let add_gbl = back.get_global("add");
	let main_gbl = back.get_global("main");
	let entry_gbl = back.get_global("_start");
	
	back.begin_fn(add_gbl, 2, 1);
	back.push_arg(0);
	back.push_arg(1);
	back.add();
	back.pop_ret(0);
	back.ret();
	back.end_fn();
	
	back.begin_fn(main_gbl, 0, 1);
	back.precall(1);
	back.push_imm(23);
	back.push_imm(45);
	back.call(add_gbl, 2, 1);
	back.pop_ret(0);
	back.ret();
	back.end_fn();
	
	back.gen_c_entry(entry_gbl, main_gbl);
	
	let entry_idx = back.get_label_offset(entry_gbl);
	let code = back.finalize();
	let entry: extern "sysv64" fn() -> i32 = unsafe {
		std::mem::transmute(code.get_off(entry_idx))
	};
	let res = entry();
	*/
	
	let mut comp = Compiler::new();
	let def_pos = Pos { line: 0, col: 0, file: "<test>".to_owned().into() };
	
	comp.compile_fn(&NodePos(Node::Fn {
		name: "add".into(),
		args: vec![
			(InlinableString::from("a"), Node::Sym(InlinableString::from("int"))),
			(InlinableString::from("b"), Node::Sym(InlinableString::from("int"))),
		],
		ret: Some(Box::new(NodePos(Node::Sym(InlinableString::from("int")), def_pos.clone()))),
		body: vec![
			NodePos(Node::Ret(Some(Box::new(
				NodePos(Node::Plus(
					Box::new(NodePos(Node::Id(InlinableString::from("a")), def_pos.clone())),
					Box::new(NodePos(Node::Id(InlinableString::from("b")), def_pos.clone())),
				), def_pos.clone())
			))), def_pos.clone())
		]
	}, def_pos.clone())).unwrap();
	
	comp.compile_fn(&NodePos(Node::Fn {
		name: "main".into(),
		args: vec![],
		ret: Some(Box::new(NodePos(Node::Sym(InlinableString::from("int")), def_pos.clone()))),
		body: vec![
			NodePos(Node::Ret(Some(Box::new(
				NodePos(Node::Call {
					func: Box::new(NodePos(Node::Id(InlinableString::from("add")), def_pos.clone())),
					args: vec![
						NodePos(Node::Int(23), def_pos.clone()),
						NodePos(Node::Int(45), def_pos.clone()),
					],
				}, def_pos.clone())
			))), def_pos.clone())
		]
	}, def_pos.clone())).unwrap();
	
	let code = comp.finalize().unwrap();
	
	let res = (code.entry)();
	
	std::process::exit(res);
}
