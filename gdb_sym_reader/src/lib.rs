use api::{
	GdbReader, make_reader_funcs,
	ReaderFuncs, Status, FrameId, SymbolCallbacks, UnwindCallbacks,
	SymTab
};
use c_api::LineMapping;

mod c_api;
mod api;

struct JitReader {}

struct SymFileReader<'a> {
	rest: &'a [u8],
}
impl<'a> SymFileReader<'a> {
	fn new(mem: &'a [u8]) -> Self {
		SymFileReader { rest: mem }
	}
	fn check_header(&mut self) -> bool {
		if self.rest.starts_with(b"rjit") {
			self.rest = &self.rest[4..];
			true
		} else {
			false
		}
	}
	fn done(&self) -> bool {
		self.rest.is_empty()
	}
	fn read_byte(&mut self) -> Option<u8> {
		if self.rest.len() < 1 {
			None
		} else {
			let res = self.rest[0];
			self.rest = &self.rest[1..];
			Some(res)
		}
	}
	fn read_u16(&mut self) -> Option<u16> {
		if self.rest.len() < 2 {
			None
		} else {
			let res = u16::from_le_bytes(self.rest[0..2].try_into().unwrap());
			self.rest = &self.rest[2..];
			Some(res)
		}
	}
	fn read_u32(&mut self) -> Option<u32> {
		if self.rest.len() < 4 {
			None
		} else {
			let res = u32::from_le_bytes(self.rest[0..4].try_into().unwrap());
			self.rest = &self.rest[4..];
			Some(res)
		}
	}
	fn read_u64(&mut self) -> Option<u64> {
		if self.rest.len() < 8 {
			None
		} else {
			let res = u64::from_le_bytes(self.rest[0..8].try_into().unwrap());
			self.rest = &self.rest[8..];
			Some(res)
		}
	}
	fn read_str(&mut self) -> Option<Box<str>> {
		let len = self.read_u16()? as usize;
		if self.rest.len() < len {
			None
		} else {
			let res = std::str::from_utf8(&self.rest[..len]).ok()?;
			self.rest = &self.rest[len..];
			Some(res.into())
		}
	}
}

fn parse(r: &mut SymFileReader, cb: SymbolCallbacks) -> Option<()> {
	let obj = cb.object_open();
	let mut symtab: Option<SymTab> = None;
	while !r.done() {
		let instr = r.read_byte()?;
		match instr {
			1 => { // NewFile
				let file_name = r.read_str()?;
				symtab.take();
				symtab = Some(obj.symtab_open(Some(&file_name)));
			},
			2 => { // NewFn
				let fn_name = r.read_str()?;
				let start = r.read_u64()?;
				let end = r.read_u64()?;
				symtab.as_mut()?.block_open(None, start, end, Some(&fn_name));
			},
			3 => { // MapLines
				let map_size = r.read_u16()?;
				let mut map = vec![];
				for _ in 0..map_size {
					let line = r.read_u32()?;
					let addr = r.read_u64()?;
					map.push(LineMapping {
						line: line as i32,
						pc: addr,
					});
				}
				symtab.as_mut()?.line_mapping_add(map.into_boxed_slice());
			},
			_ => return None,
		}
	}
	Some(())
}

impl GdbReader for JitReader {
	fn read(&mut self, cb: SymbolCallbacks, mem: &[u8]) -> Status {
		let mut r = SymFileReader::new(mem);
		if r.check_header() {
			if parse(&mut r, cb).is_some() {
				println!("JIT symbols successfully parsed.");
				Status::Success
			} else {
				println!("JIT symbols file could not be parsed!");
				Status::Fail
			}
		} else {
			Status::Fail
		}
	}

	fn unwind(&mut self, _cb: UnwindCallbacks) -> Status {
		Status::Fail
	}

	fn get_frame_id(&mut self, _cb: UnwindCallbacks) -> FrameId {
		todo!()
	}
}

#[no_mangle]
extern fn gdb_init_reader() -> *mut ReaderFuncs {
	make_reader_funcs(JitReader {})
}
