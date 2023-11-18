use compiler::Compiler;
use parser::Parser;

mod mmap;
mod asm;
mod backend;
mod compiler;
mod ast;
mod parser;

extern fn nop(_: nix::libc::c_int) {}

fn main() {
	// ignore intentional breakpoint if not in debugger
	unsafe { nix::sys::signal::signal(nix::sys::signal::SIGTRAP, nix::sys::signal::SigHandler::Handler(nop)) }
		.expect("signal()");
	
	let mut args = std::env::args();
	if args.len() != 2 {
		eprintln!("expected 1 command line argument: file path");
		std::process::exit(1);
	}
	
	args.next();
	let path = args.next().unwrap();
	
	let file = match std::fs::read_to_string(&path) {
		Ok(file) => file,
		Err(err) => {
			eprintln!("error reading file: {:?}", err);
			std::process::exit(1);
		}
	};
	
	let ast = match Parser::parse_program(path, &file) {
		Ok(ast) => ast,
		Err(err) => {
			eprintln!("syntax error: {}", err);
			std::process::exit(1);
		}
	};
	
	let code = match Compiler::compile_program(&ast) {
		Ok(code) => code,
		Err(err) => {
			eprintln!("compilation error: {}", err);
			std::process::exit(1);
		}
	};
	
	let res = (code.entry)();
	
	std::process::exit(res);
}
