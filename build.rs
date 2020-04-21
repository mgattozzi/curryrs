use std::{env, error::Error, path::PathBuf, process::Command, str};
use walkdir::WalkDir;

fn command_output(cmd: &mut Command) -> String {
  str::from_utf8(&cmd.output().unwrap().stdout)
    .unwrap()
    .trim()
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
const DYLIB_EXTENSION: &'static str = ".a";

#[cfg(target_os = "windows")]
const DYLIB_EXTENSION: &'static str = ".dll";

fn main() {
  // Traverse the directory to link all of the libs in ghc
  // then tell cargo where to get htest for linking
  match link_ghc_libs() {
    Err(e) => panic!("Unable to link ghc_libs: {}", e),
    Ok(_) => {
      println!("cargo:rustc-link-search=native=htest/dist-newstyle/build/x86_64-osx/ghc-8.8.3/htest-0.1.0.0/build/");
      println!("cargo:rustc-link-lib=static=HShtest-0.1.0.0-inplace");

      let bindings = bindgen::Builder::default()
        // The input header we would like to generate
        // bindings for.
        .header({
          let mut dir = PathBuf::from(ghc("cabal", "--print-libdir"));
          dir.push("include");
          dir.push("HsFFI.h");
          dir.as_os_str().to_owned().to_string_lossy()
        })
        // Tell cargo to invalidate the built crate whenever any of the
        // included header files changed.
        .parse_callbacks(Box::new(bindgen::CargoCallbacks))
        // Finish the builder and generate the bindings.
        .generate()
        // Unwrap the Result and panic on failure.
        .expect("Unable to generate bindings");
      // Write the bindings to the $OUT_DIR/bindings.rs file.
      let out_path = PathBuf::from(env::var("OUT_DIR").unwrap());
      bindings
        .write_to_file(out_path.join("bindings.rs"))
        .expect("Couldn't write bindings!");
    }
  }
}

fn link_ghc_libs() -> Result<(), Box<dyn Error>> {
  let builder = if command_ok(Command::new("stack").arg("--version")) {
    "stack"
  } else {
    "cabal"
  };

  // Go to the libdir for ghc then traverse all the entries
  for entry in WalkDir::new(&ghc(builder, "--print-libdir"))
    .min_depth(1)
    .into_iter()
  {
    let entry = entry?;
    match (entry.path().to_str(), entry.file_name().to_str()) {
      (Some(e), Some(file_name)) => {
        // This filters out every file shipped with GHC that isn't
        // a static archive file and only one of every type (RTS and
        // C FFI have a few different versions)
        if entry.path().extension().map(|y| y != "a").unwrap_or(true)
          || entry.path().is_dir()
          || e.ends_with("_p.a")
          || e.ends_with("_thr.a")
          || e.ends_with("_thr_l.a")
          || e.ends_with("_thr_debug.a")
          || e.ends_with("_l.a")
          || e.ends_with("_debug.a")
          || e.contains("_debug.a")
        {
          continue;
        }

        // Get the path without the file in the name
        let lib_path = {
          let mut path = entry.path().to_owned();
          path.pop();
          path
        }
        .to_str()
        .unwrap()
        .to_owned();

        println!("cargo:rustc-link-search=native={}", lib_path);
        // Get rid of lib from the file name
        let temp = file_name.split_at(3).1;
        // Get rid of the .so from the file name
        let trimmed = temp.split_at(temp.len() - DYLIB_EXTENSION.len()).0;
        println!("cargo:rustc-link-lib=static={}", trimmed);
      }
      _ => panic!("Unable to link GHC libs at all"),
    }
  }

  // Mac specific linking bugs cause it really is developer hostile and
  // doesn't care about you and working!
  #[cfg(target_os = "macos")]
  println!("cargo:rustc-link-search=native=/usr/lib");
  #[cfg(target_os = "macos")]
  println!("cargo:rustc-link-lib=dylib=iconv");

  Ok(())
}
