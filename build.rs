use std::fs::File;
use std::io::Write;
use std::path::Path;
use std::process::Command;
use std::{env, str};

fn command_output(cmd: &mut Command) -> String {
	str::from_utf8(&cmd.output().unwrap().stdout)
		.unwrap()
		.trim_right()
		.to_string()
}

fn command_ok(cmd: &mut Command) -> bool {
	cmd.status().ok().map_or(false, |s| s.success())
}

fn ghc(builder: &str, arg: &str) -> String {
	command_output(Command::new(builder).args(&["exec", "--", "ghc", arg]))
}

fn main() {
	let builder = if command_ok(Command::new("stack").arg("--version")) {
		"stack"
	} else {
		"cabal"
	};
	let ghc_version = ghc(builder, "--numeric-version");
	let ghc_libdir = ghc(builder, "--print-libdir");
	println!("cargo:rustc-link-search=native=htest");
	println!("cargo:rustc-link-search=native={}/rts", ghc_libdir);

	let out_dir = env::var("OUT_DIR").unwrap();
	let dest_path = Path::new(&out_dir).join("hs_rts.rs");
	let mut f = File::create(&dest_path).unwrap();
	let ghc_rts_rs = format!("#[link(name = \"HSrts-ghc{}\")]\nextern {{}}\n", ghc_version);
	f.write_all(ghc_rts_rs.as_bytes()).unwrap();
}
