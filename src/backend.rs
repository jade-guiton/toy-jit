use crate::{asm::{Assembler, Reg, Arg, Label}, mmap::ExecBox, ast::{BinArithOp, BinCompOp}};

struct FnState {
	ret_slots: u16,
	arg_slots: u16,
	local_slots: u32,
	temp_slots: u32,
}

pub struct Backend {
	asm: Assembler,
	cur_fn: Option<FnState>,
}

impl Backend {
	pub fn new() -> Backend {
		Backend { asm: Assembler::new(), cur_fn: None }
	}
	
	pub fn finalize(self) -> ExecBox {
		self.asm.finalize()
	}
	
	pub fn new_label(&mut self) -> Label {
		self.asm.new_label()
	}
	pub fn set_label(&mut self, lbl: Label) {
		self.asm.set_label(lbl);
	}
	pub fn get_label_offset(&mut self, lbl: Label) -> usize {
		self.asm.get_label_idx(lbl)
	}
	
	pub fn get_cur_addr(&self) -> usize {
		self.asm.get_cur_addr()
	}
	
	pub fn begin_fn(&mut self, lbl: Label, arg_slots: u16, ret_slots: u16) {
		assert!(self.cur_fn.is_none(), "current function not finished");
		self.asm.set_label(lbl);
		// prologue
		self.asm.op_push(Arg::Reg(Reg::RBP), Arg::Nil);
		self.asm.op_mov(Arg::Reg(Reg::RBP), Arg::Reg(Reg::RSP));
		self.cur_fn = Some(FnState {
			ret_slots, arg_slots,
			local_slots: 0, temp_slots: 0
		});
	}
	pub fn ret(&mut self) {
		let fn_state = self.cur_fn.as_mut().expect("not inside function");
		assert!(fn_state.temp_slots == 0, "returning with temporaries on stack is probably a bug");
		if fn_state.local_slots > 0 {
			self.asm.op_add(Arg::Reg(Reg::RSP), Arg::Imm(fn_state.local_slots as i64 * 8));
		}
		self.asm.op_pop(Arg::Reg(Reg::RBP), Arg::Nil);
		if fn_state.arg_slots > 0 {
			self.asm.op_ret(Arg::Imm(fn_state.arg_slots as i64 * 8), Arg::Nil);
		} else {
			self.asm.op_ret(Arg::Nil, Arg::Nil);
		}
	}
	pub fn end_fn(&mut self) {
		self.cur_fn.take().expect("not inside function");
	}
	
	pub fn add_breakpoint(&mut self) {
		self.asm.op_int3(Arg::Nil, Arg::Nil);
	}
	
	pub fn gen_c_entry(&mut self, lbl: Label, main_fn: Label) {
		assert!(self.cur_fn.is_none(), "current function not finished");
		
		self.asm.set_label(lbl);
		// Only RBP is used among "callee-saved" registers
		self.asm.op_push(Arg::Reg(Reg::RBP), Arg::Nil);
		self.asm.op_mov(Arg::Reg(Reg::RBP), Arg::Reg(Reg::RSP));
		
		self.asm.op_sub(Arg::Reg(Reg::RSP), Arg::Imm(8)); // 1 return slot
		self.asm.op_call(Arg::Lbl(main_fn), Arg::Nil);
		self.asm.op_pop(Arg::Reg(Reg::RAX), Arg::Nil); // place return value in RAX
		
		self.asm.op_pop(Arg::Reg(Reg::RBP), Arg::Nil);
		self.asm.op_ret(Arg::Nil, Arg::Nil);
	}
	
	pub fn alloc_locals(&mut self, slots: u32) {
		let fn_state = self.cur_fn.as_mut().expect("not inside function");
		assert!(fn_state.temp_slots == 0, "can't allocate locals with temporaries on stack");
		self.asm.op_sub(Arg::Reg(Reg::RSP), Arg::Imm(slots as i64 * 8));
		fn_state.local_slots += slots;
	}
	pub fn drop_locals(&mut self, slots: u32) {
		let fn_state = self.cur_fn.as_mut().expect("not inside function");
		assert!(fn_state.temp_slots == 0, "can't drop locals with temporaries on stack");
		assert!(slots <= fn_state.local_slots, "not enough local slots to drop");
		self.asm.op_add(Arg::Reg(Reg::RSP), Arg::Imm(slots as i64 * 8));
		fn_state.local_slots -= slots;
	}
	
	pub fn push_arg(&mut self, slot_idx: u16) {
		let fn_state = self.cur_fn.as_mut().expect("not inside function");
		assert!(slot_idx < fn_state.arg_slots, "invalid arg slot index");
		// the offset of 2 is to skip the return address + saved RBP
		self.asm.op_push(Arg::IndReg(Reg::RBP, 8 * ((fn_state.arg_slots-1 - slot_idx) as i32 + 2)), Arg::Nil);
		fn_state.temp_slots += 1;
	}
	pub fn push_local(&mut self, slot_idx: u32) {
		let fn_state = self.cur_fn.as_mut().expect("not inside function");
		assert!(slot_idx < fn_state.local_slots, "invalid local slot index");
		self.asm.op_push(Arg::IndReg(Reg::RBP, -8 * (slot_idx as i32)), Arg::Nil);
		fn_state.temp_slots += 1;
	}
	pub fn push_imm(&mut self, val: i64) {
		let fn_state = self.cur_fn.as_mut().expect("not inside function");
		self.asm.op_push(Arg::Imm(val), Arg::Nil);
		fn_state.temp_slots += 1;
	}
	pub fn push_label(&mut self, lbl: Label) {
		let fn_state = self.cur_fn.as_mut().expect("not inside function");
		self.asm.op_push(Arg::Lbl(lbl), Arg::Nil);
		fn_state.temp_slots += 1;
	}
	
	pub fn pop_arg(&mut self, slot_idx: u16) {
		let fn_state = self.cur_fn.as_mut().expect("not inside function");
		assert!(slot_idx < fn_state.arg_slots, "invalid arg slot index");
		assert!(fn_state.temp_slots > 0, "no temporaries to pop");
		self.asm.op_pop(Arg::IndReg(Reg::RBP, 8 * ((fn_state.arg_slots-1 - slot_idx) as i32 + 2)), Arg::Nil);
		fn_state.temp_slots -= 1;
	}
	pub fn pop_local(&mut self, slot_idx: u32) {
		let fn_state = self.cur_fn.as_mut().expect("not inside function");
		assert!(slot_idx < fn_state.local_slots, "invalid local slot index");
		assert!(fn_state.temp_slots > 0, "no temporaries to pop");
		self.asm.op_pop(Arg::IndReg(Reg::RBP, -8 * (slot_idx as i32)), Arg::Nil);
		fn_state.temp_slots -= 1;
	}
	pub fn pop_ret(&mut self, slot_idx: u16) {
		let fn_state = self.cur_fn.as_mut().expect("not inside function");
		assert!(slot_idx < fn_state.ret_slots, "invalid ret slot index");
		assert!(fn_state.temp_slots > 0, "no temporaries to pop");
		self.asm.op_pop(Arg::IndReg(Reg::RBP, 8 * (2 + (fn_state.arg_slots + fn_state.ret_slots-1 - slot_idx) as i32)), Arg::Nil);
		fn_state.temp_slots -= 1;
	}
	pub fn ignore(&mut self) {
		let fn_state = self.cur_fn.as_mut().expect("not inside function");
		if fn_state.temp_slots > 0 {
			self.asm.op_add(Arg::Reg(Reg::RSP), Arg::Imm(fn_state.temp_slots as i64 * 8));
			fn_state.temp_slots = 0;
		}
	}
	
	pub fn bin_comp_push(&mut self, op: BinCompOp) {
		let fn_state = self.cur_fn.as_mut().expect("not inside function");
		assert!(fn_state.temp_slots >= 2, "not enough temporaries to compare");
		self.asm.op_pop(Arg::Reg(Reg::RCX), Arg::Nil);
		self.asm.op_cmp(Arg::IndReg(Reg::RSP, 0), Arg::Reg(Reg::RCX));
		match op {
			BinCompOp::LT => self.asm.op_setl(Arg::Reg(Reg::RAX), Arg::Nil),
			BinCompOp::GT => self.asm.op_setg(Arg::Reg(Reg::RAX), Arg::Nil),
			BinCompOp::LE => self.asm.op_setle(Arg::Reg(Reg::RAX), Arg::Nil),
			BinCompOp::GE => self.asm.op_setge(Arg::Reg(Reg::RAX), Arg::Nil),
			BinCompOp::EQ => self.asm.op_sete(Arg::Reg(Reg::RAX), Arg::Nil),
			BinCompOp::NE => self.asm.op_setne(Arg::Reg(Reg::RAX), Arg::Nil),
		}
		self.asm.op_movzx(Arg::Reg(Reg::RAX), Arg::Reg(Reg::RAX));
		self.asm.op_mov(Arg::IndReg(Reg::RSP, 0), Arg::Reg(Reg::RAX));
		fn_state.temp_slots -= 1;
	}
	
	pub fn bin_comp_jmp(&mut self, mut op: BinCompOp, invert: bool, to: Label) {
		let fn_state = self.cur_fn.as_mut().expect("not inside function");
		assert!(fn_state.temp_slots >= 2, "not enough temporaries to compare");
		self.asm.op_pop(Arg::Reg(Reg::RCX), Arg::Nil);
		self.asm.op_pop(Arg::Reg(Reg::RAX), Arg::Nil);
		self.asm.op_cmp(Arg::Reg(Reg::RAX), Arg::Reg(Reg::RCX));
		if invert {
			op = op.opposite();
		}
		match op {
			BinCompOp::LT => self.asm.op_jl(Arg::Lbl(to), Arg::Nil),
			BinCompOp::GT => self.asm.op_jg(Arg::Lbl(to), Arg::Nil),
			BinCompOp::LE => self.asm.op_jle(Arg::Lbl(to), Arg::Nil),
			BinCompOp::GE => self.asm.op_jge(Arg::Lbl(to), Arg::Nil),
			BinCompOp::EQ => self.asm.op_je(Arg::Lbl(to), Arg::Nil),
			BinCompOp::NE => self.asm.op_jne(Arg::Lbl(to), Arg::Nil),
		}
		fn_state.temp_slots -= 2;
	}
	
	pub fn zero_comp_jmp(&mut self, invert: bool, to: Label) {
		let fn_state = self.cur_fn.as_mut().expect("not inside function");
		assert!(fn_state.temp_slots >= 1, "no temporary to compare");
		self.asm.op_pop(Arg::Reg(Reg::RAX), Arg::Nil);
		self.asm.op_test(Arg::Reg(Reg::RAX), Arg::Reg(Reg::RAX));
		if invert {
			self.asm.op_jnz(Arg::Lbl(to), Arg::Nil);
		} else {
			self.asm.op_jz(Arg::Lbl(to), Arg::Nil);
		}
		fn_state.temp_slots -= 1;
	}
	
	pub fn jmp(&mut self, to: Label) {
		let _fn_state = self.cur_fn.as_mut().expect("not inside function");
		self.asm.op_jmp(Arg::Lbl(to), Arg::Nil);
	}
	
	pub fn bin_arith(&mut self, op: BinArithOp) {
		let fn_state = self.cur_fn.as_mut().expect("not inside function");
		assert!(fn_state.temp_slots >= 2, "not enough temporaries to add");
		self.asm.op_pop(Arg::Reg(Reg::RAX), Arg::Nil);
		match op {
			BinArithOp::Plus => self.asm.op_add(Arg::IndReg(Reg::RSP, 0), Arg::Reg(Reg::RAX)),
			BinArithOp::Minus => self.asm.op_sub(Arg::IndReg(Reg::RSP, 0), Arg::Reg(Reg::RAX)),
		}
		fn_state.temp_slots -= 1;
	}
	
	pub fn precall(&mut self, ret_slots: u16) {
		let fn_state = self.cur_fn.as_mut().expect("not inside function");
		self.asm.op_sub(Arg::Reg(Reg::RSP), Arg::Imm(ret_slots as i64 * 8));
		fn_state.temp_slots += ret_slots as u32;
	}
	pub fn call(&mut self, fn_lbl: Label, arg_slots: u16, ret_slots: u16) {
		let fn_state = self.cur_fn.as_mut().expect("not inside function");
		assert!(fn_state.temp_slots >= arg_slots as u32 + ret_slots as u32, "not enough slots on stack for call");
		self.asm.op_call(Arg::Lbl(fn_lbl), Arg::Nil);
		fn_state.temp_slots -= arg_slots as u32;
	}
}
