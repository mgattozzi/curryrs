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

// Each os has a diferent extesion for the Dynamic Libraries. This compiles for
// the correct ones.
#[cfg(not(any(target_os = "macos", target_os = "windows")))]
const DYLIB_EXTENSION: &'static str = ".so";

#[cfg(target_os = "macos")]
const DYLIB_EXTENSION: &'static str = ".dylib";

#[cfg(target_os = "windows")]
const DYLIB_EXTENSION: &'static str = ".dll";

// This allows the user to choose which version of the Runtime System they want
// to use. By default it is non threaded.
#[cfg(not(any(feature = "threaded", feature = "threaded_l", feature = "threaded_debug")))]
const RTS: &'static str = "libHSrts-g";

#[cfg(feature = "threaded")]
const RTS: &'static str = "libHSrts_thr-";

#[cfg(feature = "threaded_l")]
const RTS: &'static str = "libHSrts_thr_l-";

#[cfg(feature = "threaded_debug")]
const RTS: &'static str = "libHSrts_thr_debug-";

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
	for entry in try!(read_dir(Path::new(&ghc(builder, "--print-libdir")))) {
		let entry = try!(entry);

		// For each directory in the libdir check it for .so files and
		// link them.
		if try!(entry.metadata()).is_dir() {
			for item in try!(read_dir(entry.path())) {
				match (entry.path().to_str(), try!(item).file_name().to_str()) {
					// This directory has lib files link them
					(Some(e),Some(i)) => {
						if i.starts_with("lib") && i.ends_with(DYLIB_EXTENSION) {

							// This filtering of items gets us the bare minimum of libraries
							// we need in order to get the Haskell Runtime linked into the
							// library. By default it's the non-threaded version that is
							// chosen
							if  i.starts_with(RTS) ||
								i.starts_with("libHSghc-") && !i.starts_with("libHSghc-boot-") ||
								i.starts_with("libHSbase") ||
								i.starts_with("libHSinteger-gmp") {

								println!("cargo:rustc-link-search=native={}", e);
								// Get rid of lib from the file name
								let temp = i.split_at(3).1;
								// Get rid of the .so from the file name
								let trimmed = temp.split_at(temp.len() - DYLIB_EXTENSION.len()).0;
								println!("cargo:rustc-link-lib=dylib={}", trimmed);
							}
						}
					},
					_ => panic!("Unable to link ghc libs"),
				}
			}
		}
	}

	Ok(())
}
