use std::{ptr::null, sync::Mutex};

#[repr(u32)]
enum JitActions {
	NoAction = 0,
	RegisterFn,
	_UnregisterFn,
}

#[repr(C)]
struct JitCodeEntry {
	next_entry: *const JitCodeEntry,
	prev_entry: *const JitCodeEntry,
	symfile_addr: *const u8,
	symfile_size: u64,
}

#[repr(C)]
struct JitDescriptor {
	version: u32,
	action_flag: JitActions,
	relevant_entry: *const JitCodeEntry,
	first_entry: *const JitCodeEntry,
}

#[no_mangle]
#[inline(never)]
extern fn __jit_debug_register_code() {
	// GDB adds a breakpoint in here
}

#[no_mangle]
static mut __jit_debug_descriptor: JitDescriptor = JitDescriptor {
	version: 1,
	action_flag: JitActions::NoAction,
	relevant_entry: null(),
	first_entry: null(),
};
static DESCRIPTOR_MUTEX: Mutex<()> = Mutex::new(());

#[repr(u8)]
enum SymInstr {
	NewFile = 1,
	NewFn,
	MapLines,
}

pub struct SymFile {
	bytes: Vec<u8>,
	lines: Vec<(u32, usize)>,
}

impl SymFile {
	pub fn new() -> SymFile {
		SymFile { bytes: vec![b'r',b'j',b'i',b't'], lines: vec![] }
	}
	pub fn into_bytes(self) -> Box<[u8]> {
		self.bytes.into()
	}
	fn add_instr(&mut self, i: SymInstr) {
		self.bytes.push(i as u8)
	}
	fn add_u16(&mut self, n: u16) {
		self.bytes.extend(n.to_le_bytes());
	}
	fn add_u32(&mut self, n: u32) {
		self.bytes.extend(n.to_le_bytes());
	}
	fn add_u64(&mut self, n: u64) {
		self.bytes.extend(n.to_le_bytes());
	}
	fn add_str(&mut self, s: &str) {
		self.add_u16(s.len().try_into().unwrap());
		self.bytes.extend(s.as_bytes())
	}
	pub fn new_file(&mut self, file_name: &str) {
		self.add_instr(SymInstr::NewFile);
		self.add_str(file_name);
	}
	pub fn new_fn(&mut self, name: &str, start: usize, end: usize) {
		self.add_instr(SymInstr::NewFn);
		self.add_str(&format!("jit!{}", name));
		self.add_u64(start as u64);
		self.add_u64(end as u64);
	}
	pub fn map_line(&mut self, line: u32, addr: usize) {
		self.lines.push((line, addr))
	}
	pub fn end_file(&mut self) {
		self.add_instr(SymInstr::MapLines);
		self.add_u16(self.lines.len().try_into().unwrap());
		let lines: Vec<(u32, usize)> = self.lines.drain(..).collect();
		for (line, addr) in lines {
			self.add_u32(line);
			self.add_u64(addr as u64);
		}
	}
}

pub fn register_symbols(symbols: &[u8]) {
	let _lock = DESCRIPTOR_MUTEX.lock();
	let entry = Box::leak(Box::new(JitCodeEntry {
		next_entry: unsafe { __jit_debug_descriptor.first_entry },
		prev_entry: null(),
		symfile_addr: symbols.as_ptr(),
		symfile_size: symbols.len() as u64,
	}));
	unsafe {
		__jit_debug_descriptor.first_entry = entry;
		__jit_debug_descriptor.relevant_entry = entry;
		__jit_debug_descriptor.action_flag = JitActions::RegisterFn;
	}
	__jit_debug_register_code();
}
