use regex::Regex;

fn main() {
	let input = io::input();

	let first = part1(&input);
	let second = part2(&input);

	println!("{first} {second}");
}

fn part1(input: &str) -> i64 {
	fn parse_digit(c: u8) -> Option<i64> {
		if c.is_ascii_digit() {
			Some((c - b'0') as i64)
		} else {
			None
		}
	}

	fn per_line(s: &str) -> i64 {
		let first = s.bytes().filter_map(parse_digit).next().unwrap();
		let second = s.bytes().rev().filter_map(parse_digit).next().unwrap();
		first * 10 + second
	}

	input.lines().map(per_line).sum()
}

fn part2(input: &str) -> i64 {
	let regex_str = r"1|2|3|4|5|6|7|8|9|0|one|two|three|four|five|six|seven|eight|nine|zero";
	let rregex_str = regex_str.chars().rev().collect::<String>();
	let forwards_regex = Regex::new(regex_str).unwrap();
	let backwards_regex = Regex::new(&rregex_str).unwrap();

	fn parse_digit(s: &str) -> i64 {
		match s {
			"1" | "one" | "eno" => 1,
			"2" | "two" | "owt" => 2,
			"3" | "three" | "eerht" => 3,
			"4" | "four" | "ruof" => 4,
			"5" | "five" | "evif" => 5,
			"6" | "six" | "xis" => 6,
			"7" | "seven" | "neves" => 7,
			"8" | "eight" | "thgie" => 8,
			"9" | "nine" | "enin" => 9,
			"0" | "zero" | "orez" => 0,
			_ => panic!("{s}"),
		}
	}

	input
		.lines()
		.map(|s: &str| -> i64 {
			let rs = s.chars().rev().collect::<String>();
			let first = forwards_regex.find(s).unwrap().as_str();
			let second = backwards_regex.find(&rs).unwrap().as_str();
			parse_digit(first) * 10 + parse_digit(second)
		})
		.sum()
}

#[cfg(test)]
mod test {
	use super::*;

	#[test]
	fn example1() {
		let input = "\
1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet";
		let res = part1(input);
		assert_eq!(res, 142);
	}

	#[test]
	fn example2() {
		let input = "\
two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen";
		let res = part2(input);
		assert_eq!(res, 281);
	}

	#[test]
	fn example3() {
		let input = "eightwo";
		let res = part2(input);
		assert_eq!(res, 82);
	}
}
