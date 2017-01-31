extern crate curryrs;
use curryrs::types::I32;

extern {
    pub fn init();
    pub fn fin();
    pub fn triple(x: I32) -> I32;
}

fn main() {
    unsafe { init(); }
    println!("Tripled value: {}", unsafe{triple(50)});
    unsafe {fin();}
}
