use std::io::Read;

pub fn input() -> String {
	let mut s = String::new();
	let stdin = std::io::stdin();
	let mut stdin = stdin.lock();
	stdin.read_to_string(&mut s).unwrap();
	s
}
