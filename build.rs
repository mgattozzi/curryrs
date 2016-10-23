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
	// Traverse the directory to link all of the libs in ghc
	// then tell cargo where to get htest for linking
	match link_ghc_libs() {
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

	// Go to the libdir for ghc then traverse all the entries
	for entry in try!(read_dir(Path::new(&ghc(builder, "--print-libdir"))) {
		let entry = try!(entry);

		// For each directory in the libdir check it for .so files and
		// link them.
		if try!(entry.metadata()).is_dir() {
			for item in try!(read_dir(entry.path())) {
				match (entry.path().to_str(), try!(item).file_name().to_str()) {
					// This directory has lib files link them
					(Some(e),Some(i)) => {
						if i.starts_with("lib") && i.ends_with(".so") {
							println!("cargo:rustc-link-search=native={}", e);
							// Get rid of lib from the file name
							let temp = i.split_at(3).1;
							// Get rid of the .so from the file name
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
