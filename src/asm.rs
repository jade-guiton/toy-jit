use crate::mmap::{MmapVec, ExecBox};

pub type Label = usize;

enum LabelState {
	Forward(Vec<usize>),
	Defined(usize),
}
pub struct Assembler {
	buf: MmapVec,
	labels: Vec<LabelState>,
}

#[allow(dead_code)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Reg {
	RAX = 0, RCX, RDX, RBX, RSP, RBP, RSI, RDI
}

#[allow(dead_code)]
#[derive(Debug)]
pub enum Arg {
	Nil,
	Imm(i64),
	Reg(Reg),
	IndReg(Reg, i32),
	Lbl(Label),
}

impl Assembler {
	pub fn new() -> Self {
		Self { buf: MmapVec::new(), labels: vec![] }
	}
	
	pub fn get_label_idx(&self, lbl: Label) -> usize {
		if let LabelState::Defined(idx) = self.labels[lbl] {
			idx
		} else {
			panic!("trying to locate undefined assembler label");
		}
	}
	
	pub fn get_cur_addr(&self) -> usize {
		self.buf.get_end_addr()
	}
	
	pub fn finalize(self) -> ExecBox {
		for lbl_st in &self.labels {
			if let LabelState::Forward(refs) = lbl_st {
				assert!(refs.len() == 0, "assembler label is referenced but not defined");
			}
		}
		self.buf.make_exec()
	}
	
	pub fn new_label(&mut self) -> Label {
		let lbl = self.labels.len() as Label;
		self.labels.push(LabelState::Forward(vec![]));
		lbl
	}
	
	pub fn write_rel32(&mut self, at: usize, to: usize) {
		let from = at + 4;
		let diff = (to as isize) - (from as isize);
		let diff: i32 = diff.try_into().expect("relative address too large to encode");
		let bytes = diff.to_le_bytes();
		for i in 0..4 {
			self.buf[at + i] = bytes[i];
		}
	}
	
	fn push_label_rel32(&mut self, lbl: Label) {
		let at = self.buf.len();
		self.buf.push_i32(0); // placeholder
		if let LabelState::Forward(refs) = &mut self.labels[lbl] {
			refs.push(at);
		} else if let LabelState::Defined(to) = &self.labels[lbl] {
			self.write_rel32(at, *to);
		}
	}
	
	pub fn set_label(&mut self, lbl: Label) {
		let to = self.buf.len();
		if let LabelState::Forward(refs) = &self.labels[lbl] {
			for at in refs.clone() {
				self.write_rel32(at, to);
			}
		} else {
			panic!("assembly label defined twice");
		}
		self.labels[lbl] = LabelState::Defined(to);
	}
}

const REX_W: u8 = 0x48;

fn modrm(mode: u8, reg: u8, rm: u8) -> u8 {
	(mode << 6) | (reg << 3) | rm
}
fn sib(scale: u8, idx: u8, base: u8) -> u8 {
	(scale << 6) | (idx << 3) | base
}

include!(concat!(env!("OUT_DIR"), "/ops_x64.rs"));
