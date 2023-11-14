use asm::{Assembler, Reg, Arg};

mod asm;
mod mmap;

fn main() {
	let mut asm = Assembler::new();
	
	let entry_lbl = asm.new_lbl();
	asm.set_lbl(entry_lbl);
	
	asm.op_mov(Arg::Reg(Reg::RAX), Arg::ImmU(42));
	asm.op_ret(Arg::Nil, Arg::Nil);
	asm.dbg();
	
	let code = asm.finish();
	let entry: extern "sysv64" fn() -> i32 = unsafe {
		std::mem::transmute(code.get_label(entry_lbl))
	};
	let res = entry();
	std::process::exit(res);
}
