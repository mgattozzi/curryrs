fn main() {
    println!("cargo:rustc-link-search=native=hs2rs");
    println!("cargo:rustc-link-lib=dylib=inter");
    println!("cargo:rustc-link-lib=dylib=hs");
}
