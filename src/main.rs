use asm::{Assembler, Reg, Arg};

mod asm;
mod mmap;

fn main() {
	let mut asm = Assembler::new();
	
	let add_lbl = asm.new_lbl();
	let entry_lbl = asm.new_lbl();
	
	
	asm.set_lbl(add_lbl);
	// prologue
	asm.op_push(Arg::Reg(Reg::RBP), Arg::Nil);
	asm.op_mov(Arg::Reg(Reg::RBP), Arg::Reg(Reg::RSP));
	// ret a + b;
	asm.op_push(Arg::IndReg(Reg::RBP, 24), Arg::Nil);
	asm.op_push(Arg::IndReg(Reg::RBP, 16), Arg::Nil);
	asm.op_pop(Arg::Reg(Reg::RAX), Arg::Nil);
	asm.op_add(Arg::IndReg(Reg::RSP, 0), Arg::Reg(Reg::RAX));
	asm.op_pop(Arg::IndReg(Reg::RBP, 32), Arg::Nil);
	// epilogue
	asm.op_pop(Arg::Reg(Reg::RBP), Arg::Nil);
	asm.op_ret(Arg::Imm(16), Arg::Nil);
	
	
	asm.set_lbl(entry_lbl);
	//asm.op_int3(Arg::Nil, Arg::Nil);
	// prologue
	asm.op_push(Arg::Reg(Reg::RBP), Arg::Nil);
	// precall
	asm.op_sub(Arg::Reg(Reg::RSP), Arg::Imm(8));
	// (23, 45)
	asm.op_push(Arg::Imm(23), Arg::Nil);
	asm.op_push(Arg::Imm(45), Arg::Nil);
	// call add
	asm.op_call(Arg::Lbl(add_lbl), Arg::Nil);
	// ret (SysV64 convention)
	asm.op_pop(Arg::Reg(Reg::RAX), Arg::Nil);
	asm.op_pop(Arg::Reg(Reg::RBP), Arg::Nil);
	asm.op_ret(Arg::Nil, Arg::Nil);
	
	
	asm.dbg();
	
	let entry_idx = asm.get_label_idx(entry_lbl);
	let code = asm.finish();
	let entry: extern "sysv64" fn() -> i32 = unsafe {
		std::mem::transmute(code.get_idx(entry_idx))
	};
	let res = entry();
	std::process::exit(res);
}
