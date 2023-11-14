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
			panic!("trying to get index of undefined assembler label");
		}
	}
	
	pub fn finish(self) -> ExecBox {
		for lbl_st in &self.labels {
			if let LabelState::Forward(refs) = lbl_st {
				assert!(refs.len() == 0, "assembler label is referenced but not defined");
			}
		}
		self.buf.make_exec()
	}
	
	pub fn dbg(&self) {
		println!("Assembler.buf: [");
		for line in 0..self.buf.len()/8 + 1 {
			print!("  ");
			for col in 0..8 {
				let i = line*8 + col;
				if i >= self.buf.len() { break; }
				print!("{:02x} ", self.buf[i]);
			}
			println!();
		}
		println!("]");
	}
	
	pub fn new_lbl(&mut self) -> Label {
		let lbl = self.labels.len() as Label;
		self.labels.push(LabelState::Forward(vec![]));
		lbl
	}
	
	fn ref_lbl(&mut self, lbl: Label) {
		let at = self.buf.len();
		self.buf.push_i32(0); // placeholder
		match &mut self.labels[lbl] {
			LabelState::Forward(refs) => refs.push(at),
			LabelState::Defined(to) => self.buf.write_rel32(at, *to),
		}
	}
	
	pub fn set_lbl(&mut self, lbl: Label) {
		let to = self.buf.len();
		match &self.labels[lbl] {
			LabelState::Forward(refs) => {
				for at in refs {
					self.buf.write_rel32(*at, to)
				}
			},
			LabelState::Defined(_) => panic!("assembly label defined twice"),
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

include!(concat!(env!("OUT_DIR"), "/ops.rs"));
