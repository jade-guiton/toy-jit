#[macro_export]
macro_rules! let_variant {
	( $out:tt, $case:pat = $exp:expr ) => {
		let $out = match $exp {
			$case => $out,
			_ => unreachable!(),
		};
	}
}