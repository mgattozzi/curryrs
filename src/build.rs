//! Functions for linking to a Haskell package.
//!

use std::path::Path;
use std::process;
use std::process::Command;
use std::process::Stdio;
use std::env;

/// Link to a Haskell package. The package must have a custom Setup.hs
pub fn link_package(name: &str, path: &Path) {
	let builddir = Path::new(&env::var("OUT_DIR").unwrap()).join(format!("HS{}-dist", name));
	let mut cmd = Command::new("cabal");
	cmd.arg("build")
	   .arg("--builddir")
	   .arg(builddir.to_str().unwrap())
	   .current_dir(&path)
	   .stdout(Stdio::inherit());

	#[cfg(feature = "threaded")]
	cmd.arg("--ghc-option=-threaded");
	#[cfg(feature = "eventlog")]
	cmd.arg("--ghc-option=-eventlog");
	#[cfg(feature = "debug")]
	cmd.arg("--ghc-option=-debug");

	call_command(&mut cmd, "failed to build haskell package");
}

#[allow(dead_code)]
fn main() {
	link_package("htest", Path::new("htest"));
}

fn call_command(cmd: &mut Command, failmsg: &str) -> process::Output {
	let output = cmd.stderr(Stdio::inherit()).output().expect(failmsg);
	if output.status.success() {
		return output;
	} else {
		panic!(String::from(failmsg));
	}
}
