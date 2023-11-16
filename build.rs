use std::{
	env,
	fs::{self, File},
	path::Path,
	collections::HashMap,
	io::Write
};
use nom::{
  IResult,
  character::complete::{char, one_of},
  combinator::{opt, map_res, map, map_opt, all_consuming, value},
	bytes::complete::{take_while1, take_while_m_n, tag, is_a},
	multi::{count, many0},
	sequence::{terminated, preceded}, branch::alt,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Arg {
	Nil,
	ImmI(u8), ImmU(u8),
	Rel32,
	Reg64, RegOrMem64,
}

#[derive(Clone, Debug)]
enum OpExt {
	PlusRd,
	Reg(u8),
}

#[derive(Clone, Debug)]
struct Variant {
	args: [Arg; 2],
	rex_w: bool,
	opcode: u8,
	op_ext: Option<OpExt>,
}

fn octal_digit(input: &str) -> IResult<&str, u8> {
	map_opt(one_of("01234567"), |d| d.to_digit(8).map(|x| x as u8))(input)
}

fn bit_cnt(input: &str) -> IResult<&str, u8> {
	map_opt(map_res(
		take_while_m_n(1, 2, |c| char::is_ascii_digit(&c)),
		|s| u8::from_str_radix(s, 10)),
		|n| if n.count_ones() == 1 && n >= 8 && n <= 64 { Some(n) } else { None }
	)(input)
}

fn parse_arg(input: &str) -> IResult<&str, Arg> {
	return map(opt(alt((
		map(preceded(char('u'), bit_cnt), |n| Arg::ImmU(n)),
		map(preceded(char('i'), bit_cnt), |n| Arg::ImmI(n)),
		value(Arg::Rel32, tag("rel32")),
		value(Arg::Reg64, tag("r64")),
		value(Arg::RegOrMem64, tag("r/m64")),
	))), |o| o.unwrap_or(Arg::Nil))(input);
}

fn parse_variant(input: &str) -> IResult<&str, (Box<str>, Variant)> {
	let (input, mnem): (&str, Box<str>) = map(
		take_while1(|c| char::is_ascii_alphanumeric(&c)), |s: &str| s.into()
	)(input)?;
	let (input, _) = char('\t')(input)?;
	let (input, args) = count(terminated(parse_arg, char('\t')), 2)(input)?;
	let (input, rex_w) = map(opt(char('W')), |o| o.is_some())(input)?;
	let (input, _) = char('\t')(input)?;
	let (input, opcode) = map_res(
		take_while_m_n(2, 2, |c: char| c.is_digit(16)),
		|s: &str| u8::from_str_radix(s, 16))(input)?;
	let (input, op_ext) = opt(alt((
		map(tag("+rd"), |_| OpExt::PlusRd),
		map(preceded(char('/'), octal_digit), |reg| OpExt::Reg(reg)),
	)))(input)?;
	let (input, _) = char('\n')(input)?;
	Ok((input, (mnem, Variant {
		args: [args[0], args[1]],
		rex_w,
		opcode,
		op_ext,
	})))
}

fn main() {
	println!("cargo:rerun-if-changed=build.rs");
	println!("cargo:rerun-if-changed=src/ops_x64.txt");
	
	let in_path = Path::new("./src/ops_x64.txt");
	let ops_txt = fs::read(in_path).expect("could not read ops_x64.txt");
	let ops_txt = std::str::from_utf8(&ops_txt).expect("ops_x64.txt is not valid utf-8");
	
	let (_, variants) = all_consuming(many0(alt((
		map(parse_variant, Some),
		map(terminated(is_a(" \t"), char('\n')), |_| None)
	))))(ops_txt).unwrap();
	
	let mut mnems: HashMap<Box<str>, Vec<Variant>> = HashMap::new();
	for opt in variants {
		if let Some((mnem, var)) = opt {
			if let Some(vars) = mnems.get_mut(&mnem) {
				vars.push(var);
			} else {
				mnems.insert(mnem, vec![var]);
			}
		}
	}
	
	let out_dir = env::var_os("OUT_DIR").unwrap();
	let out_path = Path::new(&out_dir).join("ops_x64.rs");
	let mut out = File::create(out_path).expect("could not create ops.rs");
	
	write!(out, "#[allow(dead_code, unreachable_patterns)]\n").unwrap();
	write!(out, "impl Assembler {{\n").unwrap();
	for (mnem, vars) in mnems {
		write!(out, "  pub fn op_{}(&mut self, a1: Arg, a2: Arg) {{\n", mnem).unwrap();
		write!(out, "    match (a1, a2) {{\n").unwrap();
		for var in vars {
			let has_rm = var.args[0] == Arg::RegOrMem64 || var.args[1] == Arg::RegOrMem64;
			let rep = if has_rm { 2 } else { 1 };
			for i in 0..rep {
				write!(out, "      (").unwrap();
				let mut guard = String::new();
				let mut imm: Option<Arg> = None;
				for j in 0..2 {
					if j != 0 { write!(out, ", ").unwrap(); }
					match var.args[j] {
						Arg::Nil => write!(out, "Arg::Nil").unwrap(),
						Arg::ImmI(size) => {
							write!(out, "Arg::Imm(imm)").unwrap();
							imm = Some(Arg::ImmI(size));
							if size < 64 {
								guard = format!("if imm >= i{}::MIN as i64 && imm <= i{}::MAX as i64 ", size, size);
							}
						},
						Arg::ImmU(size) => {
							write!(out, "Arg::Imm(imm)").unwrap();
							imm = Some(Arg::ImmU(size));
							if size < 64 {
								guard = format!("if imm >= 0 && imm <= u{}::MAX as i64 ", size);
							}
						},
						Arg::Reg64 => write!(out, "Arg::Reg(reg)").unwrap(),
						Arg::RegOrMem64 => {
							if i == 0 {
								write!(out, "Arg::IndReg(rm, off)").unwrap();
							} else {
								write!(out, "Arg::Reg(rm)").unwrap();
							}
						},
						Arg::Rel32 => {
							write!(out, "Arg::Lbl(lbl)").unwrap();
							imm = Some(Arg::Rel32);
						},
					}
				}
				write!(out, ") {}=> {{\n", guard).unwrap();
				
				if var.rex_w {
					write!(out, "        self.buf.push_u8(REX_W);\n").unwrap();
				}
				match var.op_ext {
					None | Some(OpExt::Reg(_)) => {
						write!(out, "        self.buf.push_u8({:#02x});\n", var.opcode).unwrap();
					},
					Some(OpExt::PlusRd) => {
						write!(out, "        self.buf.push_u8({:#02x} + reg as u8);\n", var.opcode).unwrap();
					},
				}
				if let Some(OpExt::Reg(reg)) = var.op_ext {
					write!(out, "        let reg: u8 = {};\n", reg).unwrap();
				}
				
				if has_rm {
					if i == 1 { // rm is a register
						write!(out, "        let mode: u8 = 0b11;\n").unwrap();
					} else {
						write!(out, "        let no_off = off == 0 && rm != Reg::RBP;\n").unwrap();
						write!(out, "        let sm_off = off >= -128 && off < 128;\n").unwrap();
						write!(out, "        let mode: u8 = if no_off {{ 0b00 }} else if sm_off {{ 0b01 }} else {{ 0b10 }};\n").unwrap();
					}
					write!(out, "        self.buf.push_u8(modrm(mode, reg as u8, rm as u8));\n").unwrap();
					if i != 1 {
						write!(out, "        if mode == 0b01 {{\n").unwrap();
						write!(out, "          self.buf.push_i8(off as i8);\n").unwrap();
						write!(out, "        }} else if mode == 0b10 {{\n").unwrap();
						write!(out, "          self.buf.push_i32(off);\n").unwrap();
						write!(out, "        }}\n").unwrap();
						write!(out, "        if rm == Reg::RSP {{\n").unwrap();
						write!(out, "          self.buf.push_u8(sib(0, Reg::RSP as u8, rm as u8));\n").unwrap();
						write!(out, "        }}\n").unwrap();
					}
				}
				
				match imm {
					Some(Arg::ImmU(size)) => write!(out, "        self.buf.push_u{}(imm as u{});\n", size, size).unwrap(),
					Some(Arg::ImmI(size)) => write!(out, "        self.buf.push_i{}(imm as i{});\n", size, size).unwrap(),
					Some(Arg::Rel32) => write!(out, "        self.ref_label(lbl);\n").unwrap(),
					None => {},
					_ => unreachable!(),
				}
				
				write!(out, "      }}\n").unwrap();
			}
		}
		write!(out, "      (a1,a2) => panic!(\"unknown '{}' variant (a1 = {{:?}}, a2 = {{:?}})\", a1, a2),\n",
			mnem).unwrap();
		write!(out, "    }}\n").unwrap();
		write!(out, "  }}\n").unwrap();
	}
	write!(out, "}}\n").unwrap();
}