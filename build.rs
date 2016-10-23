use std::fs::read_dir;
use std::path::Path;
use std::process::Command;
use std::io;
use std::str;

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
	let x = link_ghc_libs();

	match x {
		Err(e) => panic!("Unable to link ghc_libs: {}", e),
		Ok(_)  => println!("cargo:rustc-link-search=native=htest"),
	}

}

fn link_ghc_libs() -> io::Result<()> {

	let builder = if command_ok(Command::new("stack").arg("--version")) {
		"stack"
	} else {
		"cabal"
	};

	let ghc_libdir = ghc(builder, "--print-libdir");
	let lib_path = Path::new(&ghc_libdir);

	for entry in try!(read_dir(lib_path)) {
		let entry = try!(entry);

		if try!(entry.metadata()).is_dir() {
			for item in try!(read_dir(entry.path())) {
				match (entry.path().to_str(), try!(item).file_name().to_str()) {
					(Some(e),Some(i)) => {
						if i.starts_with("lib") && i.ends_with(".so") {
							println!("cargo:rustc-link-search=native={}", e);
							let temp = i.split_at(3).1;
							let trimmed = temp.split_at(temp.len() - 3).0;
							println!("cargo:rustc-link-lib=dylib={}", trimmed);
						}
					},
					_ => panic!("Unable to link ghc libs"),
				}
			}
		}
	}

	Ok(())
}
