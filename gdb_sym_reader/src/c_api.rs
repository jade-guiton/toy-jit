use std::ffi::{c_void, c_long, c_int, c_ulong, c_char};

// Make sure your plugin's license really *is* GPL compatible, obviously.
#[no_mangle]
extern fn plugin_is_GPL_compatible() -> c_int { 0 }

pub type CoreAddr = c_ulong;

#[repr(C)]
pub enum Status {
	Fail = 0,
	Success = 1,
}

pub struct Opaque {
	_unused: [u8; 0],
}
pub type Object = Opaque;
pub type SymTab = Opaque;
pub type Block = Opaque;

#[repr(C)]
#[derive(Debug)]
pub struct LineMapping {
	pub line: c_int,
	pub pc: CoreAddr,
}

pub type TargetRead = unsafe extern fn(
	target_mem: CoreAddr,
	gdb_buf: *mut c_void,
	len: c_int,
) -> Status;

#[repr(C)]
pub struct SymbolCallbacks {
	pub object_open: unsafe extern fn(cb: *mut SymbolCallbacks) -> *mut Object,
	pub symtab_open: unsafe extern fn(
		cb: *mut SymbolCallbacks,
		obj: *mut Object,
		file_name: *const c_char,
	) -> *mut SymTab,
	pub block_open: unsafe extern fn(
		cb: *mut SymbolCallbacks,
		symtab: *mut SymTab,
		parent: *const Block,
		begin: CoreAddr,
		end: CoreAddr,
		name: *const c_char,
	) -> *mut Block,
	pub symtab_close: unsafe extern fn(cb: *mut SymbolCallbacks, symtab: *mut SymTab),
	pub object_close: unsafe extern fn(cb: *mut SymbolCallbacks, obj: *mut Object),
	pub line_mapping_add: unsafe extern fn(
		cb: *mut SymbolCallbacks,
		symtab: *mut SymTab,
		nlines: c_int,
		lines: *const LineMapping,
	),
	pub target_read: TargetRead,
	_priv_data: *mut Opaque,
}

#[repr(C)]
pub struct RegValue {
	pub size: c_int,
	pub defined: c_int,
	pub free: unsafe extern fn(*mut RegValue),
	pub value: [u8],
}

#[repr(C)]
pub struct FrameId {
	pub code_address: CoreAddr,
	pub stack_address: CoreAddr,
}

#[repr(C)]
pub struct UnwindCallbacks {
	pub reg_get: unsafe extern fn(
		cb: *mut UnwindCallbacks,
		regnum: c_int,
	) -> *mut RegValue,
	pub reg_set: unsafe extern fn(
		cb: *mut UnwindCallbacks,
		regnum: c_int,
		val: *const RegValue,
	),
	pub target_read: TargetRead,
	_priv_data: *mut Opaque,
}

#[repr(C)]
pub struct ReaderFuncs {
	pub reader_version: c_int,
	pub priv_data: *mut c_void,
	pub read: unsafe extern fn(
		self_: *mut ReaderFuncs,
		cb: *mut SymbolCallbacks,
		mem: *const u8, mem_sz: c_long
	) -> Status,
	pub unwind: unsafe extern fn(
		self_: *mut ReaderFuncs,
		cb: *mut UnwindCallbacks,
	) -> Status,
	pub get_frame_id: unsafe extern fn(
		self_: *mut ReaderFuncs,
		cb: *mut UnwindCallbacks,
	) -> FrameId,
	pub destroy: unsafe extern fn(
		self_: *mut ReaderFuncs,
	),
}
