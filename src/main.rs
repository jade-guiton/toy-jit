use backend_x64::Backend;

mod mmap;
mod asm_x64;
mod backend_x64;

extern fn nop(_: nix::libc::c_int) {}

fn main() {
	// ignore intentional breakpoint if not in debugger
	unsafe { nix::sys::signal::signal(nix::sys::signal::SIGTRAP, nix::sys::signal::SigHandler::Handler(nop)) }
		.expect("signal()");
	
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
		std::mem::transmute(code.get_idx(entry_idx))
	};
	let res = entry();
	std::process::exit(res);
}
