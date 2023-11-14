use std::fs::File;
use std::ops::DerefMut;
use std::ptr::null_mut;

use nix::sys::mman::*;
use nix::unistd::sysconf;

pub struct MmapVec {
	ptr: *mut nix::libc::c_void,
	page_size: usize,
	cap: usize,
	len: usize,
}

pub struct ExecBox {
	ptr: *const nix::libc::c_void,
	len: usize,
}

impl MmapVec {
	pub fn new() -> Self {
		let page_size: usize =
			sysconf(nix::unistd::SysconfVar::PAGE_SIZE)
				.unwrap().unwrap().try_into().unwrap();
		let cap = page_size;
		let ptr = unsafe {
			mmap::<File>(None,
				cap.try_into().unwrap(),
				ProtFlags::PROT_READ | ProtFlags::PROT_WRITE,
				MapFlags::MAP_ANONYMOUS | MapFlags::MAP_PRIVATE,
				None, 0
			)
		}.expect("mmap failed");
		MmapVec { ptr, page_size, cap, len: 0 }
	}
	
	pub fn make_exec(mut self) -> ExecBox {
		let ptr = self.ptr;
		self.ptr = null_mut();
		unsafe { mprotect(ptr, self.len, ProtFlags::PROT_READ | ProtFlags::PROT_EXEC) }
			.expect("mprotect failed");
		ExecBox { ptr, len: self.len }
	}
	
	pub fn len(&self) -> usize {
		self.len
	}
	
	fn grow(&mut self) {
		let new_size = self.cap + self.page_size;
		let new_ptr = unsafe { mremap(self.ptr, self.cap, new_size, MRemapFlags::MREMAP_MAYMOVE, None) }.unwrap();
		self.ptr = new_ptr;
		self.cap = new_size;
	}
	
	fn push_bytes<const N: usize>(&mut self, b: [u8; N]) {
		if self.len + N > self.cap {
			self.grow();
		}
		let mut pos = self.len;
		self.len += N;
		let buf = self.deref_mut();
		for i in 0..N {
			buf[pos] = b[i];
			pos += 1;
		}
	}
	pub fn push_u8(&mut self, x: u8) { self.push_bytes([x]); }
	pub fn push_i8(&mut self, x: i8) { self.push_bytes(x.to_le_bytes()); }
	pub fn push_u16(&mut self, x: u16) { self.push_bytes(x.to_le_bytes()); }
	pub fn push_u32(&mut self, x: u32) { self.push_bytes(x.to_le_bytes()); }
	pub fn push_i32(&mut self, x: i32) { self.push_bytes(x.to_le_bytes()); }
	pub fn push_i64(&mut self, x: i64) { self.push_bytes(x.to_le_bytes()); }
	
	pub fn write_rel32(&mut self, at: usize, to: usize) {
		let from = at + 4;
		let diff = (to as isize) - (from as isize);
		let diff: i32 = diff.try_into().expect("relative address too large to encode");
		let bytes = diff.to_le_bytes();
		for i in 0..4 {
			self[at + i] = bytes[i];
		}
	}
}

impl ExecBox {
	pub fn get_idx(&self, idx: usize) -> *const nix::libc::c_void {
		assert!(idx <= self.len);
		unsafe { self.ptr.offset(idx as isize) }
	}
}

impl std::ops::Deref for MmapVec {
	type Target = [u8];
	fn deref(&self) -> &[u8] {
		unsafe { std::slice::from_raw_parts(self.ptr as *const u8, self.len) }
	}
}
impl std::ops::DerefMut for MmapVec {
	fn deref_mut(&mut self) -> &mut [u8] {
		unsafe { std::slice::from_raw_parts_mut(self.ptr as *mut u8, self.len) }
	}
}

impl std::ops::Drop for MmapVec {
	fn drop(&mut self) {
		if !self.ptr.is_null() {
			unsafe { munmap(self.ptr, self.len) }.expect("munmap failed");
		}
	}
}
impl std::ops::Drop for ExecBox {
	fn drop(&mut self) {
		if !self.ptr.is_null() {
			unsafe { munmap(self.ptr.cast_mut(), self.len) }.expect("munmap failed");
		}
	}
}
