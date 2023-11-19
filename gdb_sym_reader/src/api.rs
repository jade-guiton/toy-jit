use std::{ffi::{c_long, CString, c_int}, ptr::{null, null_mut}};

use crate::c_api;

pub use c_api::{Status, FrameId, ReaderFuncs, CoreAddr, LineMapping};

pub struct Object {
	ptr: *mut c_api::Object,
	cb: *mut c_api::SymbolCallbacks,
}
impl Drop for Object {
	fn drop(&mut self) {
		unsafe {
			((*self.cb).object_close)(self.cb, self.ptr);
		}
	}
}

pub struct SymTab {
	ptr: *mut c_api::SymTab,
	cb: *mut c_api::SymbolCallbacks,
}
impl Drop for SymTab {
	fn drop(&mut self) {
		unsafe {
			((*self.cb).symtab_close)(self.cb, self.ptr);
		}
	}
}

pub struct Block {
	ptr: *mut c_api::Block,
}

pub struct SymbolCallbacks {
	cb: *mut c_api::SymbolCallbacks,
}

impl SymbolCallbacks {
	pub fn object_open(&self) -> Object {
		unsafe {
			Object {
				ptr: ((*self.cb).object_open)(self.cb),
				cb: self.cb,
			}
		}
	}
	pub fn target_read(&self, target_mem: CoreAddr, buf: &mut [u8]) -> Status {
		unsafe {
			((*self.cb).target_read)(
				target_mem,
				buf.as_mut_ptr().cast(),
				buf.len().try_into().unwrap()
			)
		}
	}
}

impl Object {
	pub fn symtab_open(&self, name: Option<&str>) -> SymTab {
		let name = name.map(|s| CString::new(s).unwrap().into_raw()).unwrap_or(null_mut());
		unsafe {
			SymTab {
				ptr: ((*self.cb).symtab_open)(self.cb, self.ptr, name),
				cb: self.cb,
			}
		}
	}
}

impl SymTab {
	pub fn block_open(
		&self,
		parent: Option<&mut Block>,
		begin: CoreAddr,
		end: CoreAddr,
		name: Option<&str>
	) -> Block {
		let name = name.map(|s| CString::new(s).unwrap().into_raw()).unwrap_or(null_mut());
		unsafe {
			Block {
				ptr: ((*self.cb).block_open)(
					self.cb, self.ptr,
					parent.map(|b| b.ptr).unwrap_or(null_mut()),
					begin, end,
					name
				),
			}
		}
	}
	pub fn line_mapping_add(
		&self,
		lines: Box<[LineMapping]>,
	) {
		let line_cnt: i32 = lines.len().try_into().unwrap();
		let lines = Box::into_raw(lines).cast();
		unsafe {
			((*self.cb).line_mapping_add)(
				self.cb,
				self.ptr,
				line_cnt,
				lines
			);
		}
	}
}

pub struct RegValue {
	ptr: *mut c_api::RegValue,
}
impl Drop for RegValue {
	fn drop(&mut self) {
		unsafe {
			((*self.ptr).free)(self.ptr);
		}
	}
}

pub struct UnwindCallbacks {
	cb: *mut c_api::UnwindCallbacks,
}

impl UnwindCallbacks {
	pub fn reg_get(&self, regnum: i32) -> RegValue {
		unsafe {
			RegValue {
				ptr: ((*self.cb).reg_get)(self.cb, regnum as c_int)
			}
		}
	}
	pub fn reg_set(&self, regnum: i32, val: RegValue) {
		unsafe {
			((*self.cb).reg_set)(self.cb, regnum as c_int, val.ptr)
		}
	}
	pub fn target_read(&self, target_mem: CoreAddr, buf: &mut [u8]) -> Status {
		unsafe {
			((*self.cb).target_read)(
				target_mem,
				buf.as_mut_ptr().cast(),
				buf.len().try_into().unwrap()
			)
		}
	}
}

pub trait GdbReader {
	fn read(&mut self, cb: SymbolCallbacks, mem: &[u8]) -> Status;
	fn unwind(&mut self, cb: UnwindCallbacks) -> Status;
	fn get_frame_id(&mut self, cb: UnwindCallbacks) -> FrameId;
}

unsafe extern fn read<R: GdbReader>(
	self_: *mut ReaderFuncs,
	cb: *mut c_api::SymbolCallbacks,
	mem: *const u8, mem_sz: c_long
) -> Status {
	let reader = &mut *(*self_).priv_data.cast::<R>();
	let cb = SymbolCallbacks { cb };
	reader.read(cb, std::slice::from_raw_parts(mem, mem_sz as usize))
}

unsafe extern fn unwind<R: GdbReader>(
	self_: *mut ReaderFuncs,
	cb: *mut c_api::UnwindCallbacks,
) -> Status {
	let reader = &mut *(*self_).priv_data.cast::<R>();
	let cb = UnwindCallbacks { cb };
	reader.unwind(cb)
}

unsafe extern fn get_frame_id<R: GdbReader>(
	self_: *mut ReaderFuncs,
	cb: *mut c_api::UnwindCallbacks,
) -> FrameId {
	let reader = &mut *(*self_).priv_data.cast::<R>();
	let cb = UnwindCallbacks { cb };
	reader.get_frame_id(cb)
}

unsafe extern fn destroy<R: GdbReader>(self_: *mut ReaderFuncs) {
	drop(Box::from_raw((*self_).priv_data.cast::<R>()));
	drop(Box::from_raw(self_));
}

pub fn make_reader_funcs<R: GdbReader>(reader: R) -> *mut ReaderFuncs {
	Box::leak(Box::new(ReaderFuncs {
		reader_version: 1,
		priv_data: Box::into_raw(Box::new(reader)).cast(),
		read: read::<R>,
		unwind: unwind::<R>,
		get_frame_id: get_frame_id::<R>,
		destroy: destroy::<R>,
	}))
}
