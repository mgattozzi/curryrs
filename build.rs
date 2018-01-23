extern crate cabal_rs;
use cabal_rs::Cabal;

fn main() {
    Cabal::src("htest").build().unwrap();
}
